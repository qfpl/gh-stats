{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GhStats.DbTest where

import           Control.Monad              (void, (<=<), zipWithM_)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)
import           Data.List                  (nub)
import qualified Data.Map as M
import           Data.Maybe                 (fromJust)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (UTCTime), fromGregorian,
                                             secondsToDiffTime)
import           Database.SQLite.Simple     (Connection)
import qualified GitHub                     as GH

import           Hedgehog                   (GenT, MonadGen, Property,
                                             PropertyT, failure, forAll,
                                             property, tripping, (===))
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           GhStats.Db                 (initDb, insertPop, insertReferrers,
                                             insertRepoStats, selectPop,
                                             selectReferrersForRepoStats,
                                             selectRepoStats, toDbReferrers)
import           GhStats.Db.Types           (Count (Count), DbRepoStats (DbRepoStats, _dbRepoStatsId),
                                             HasTable (tableName), Id (Id),
                                             Pop (Pop, popId),
                                             Position (Position),
                                             Uniques (Uniques))
import           GhStats.Types              (AsSQLiteResponse, Forks (..),
                                             HasConnection, RepoStats,
                                             Stars (..), runGhStatsM)

import           GhStats.Test               (GhStatsPropReaderT,
                                             GhStatsPropertyT,
                                             runGhStatsPropertyT)

type TestConstraints e r m = (
    MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  , MonadReader r m
  , HasConnection r
  , MonadGen m
  )

testDb ::
  Connection
  -> TestTree
testDb conn = testGroup "GhStats.Db" . fmap ($ conn) $ [
    testRepoStatsRoundTrip
  , testReferrerRoundTrip
  , testPathRoundTrip
  , testReferrersRoundTrip
  ]

testRepoStatsRoundTrip ::
  Connection
  -> TestTree
testRepoStatsRoundTrip conn =
  ghStatsProp "DbRepoStats round trip" conn $ do
    drs <- forAllT genDbRepoStats
    drsId <- insertRepoStats drs
    let
      drsWithId =
        drs {_dbRepoStatsId = Just drsId}
    maybe failure (=== drsWithId) =<< selectRepoStats drsId

testReferrerRoundTrip ::
  Connection
  -> TestTree
testReferrerRoundTrip =
  popRoundTrip (Proxy :: Proxy GH.Referrer)

testPathRoundTrip ::
  Connection
  -> TestTree
testPathRoundTrip =
  popRoundTrip (Proxy :: Proxy GH.PopularPath)

testReferrersRoundTrip ::
  Connection
  -> TestTree
testReferrersRoundTrip conn =
  ghStatsProp "referrers round trip" conn $ do
    drs <- forAllT genDbRepoStats
    refs <- forAllT genReferrers
    drsId <- insertRepoStats drs
    let
      dbRefsExpected = toDbReferrers drsId refs
    insertReferrers drsId refs
    dbRefsActual <- selectReferrersForRepoStats drsId
    dbReferrersEqual dbRefsExpected dbRefsActual

dbReferrersEqual ::
  [Pop GH.Referrer]
  -> [Pop GH.Referrer]
  -> GhStatsPropertyT ()
dbReferrersEqual =
  zipWithM_ (\e a -> e === (a {popId = Nothing}))

ghStatsProp ::
  TestName
  -> Connection
  -> GhStatsPropertyT ()
  -> TestTree
ghStatsProp propName conn =
  testProperty propName . property . runGhStatsPropertyT conn

popRoundTrip ::
  forall a.
  HasTable a
  => Proxy a
  -> Connection
  -> TestTree
popRoundTrip _ conn =
  let
    testName = show (tableName @a) <> " round trip"
  in
    ghStatsProp testName conn $ do
      drs <- forAllT genDbRepoStats
      pop <- forAllT genPop
      drsId <- insertRepoStats drs
      popId' <- insertPop @a pop
      let
        popWithId =
          pop {popId = Just popId'}
      maybe failure (=== popWithId) =<< selectPop popId'

genDbRepoStats ::
   MonadGen m
   => m DbRepoStats
genDbRepoStats =
  DbRepoStats
  Nothing
  <$> genName
  <*> genUTCTime
  <*> genStars
  <*> genForks

genStars ::
  MonadGen m
  => m Stars
genStars =
  Stars <$> Gen.int (Range.linear 0 1000000)

genForks ::
  MonadGen m
  => m Forks
genForks =
  Forks <$> Gen.int (Range.linear 0 1000000)

genId ::
  MonadGen m
  => m (Id a)
genId =
  Id <$> Gen.int64 Range.linearBounded

genName ::
  MonadGen m
  => m (GH.Name a)
genName =
  -- TODO: should probably change this to `unicode` once hedgehog is updated
  -- (https://github.com/hedgehogqa/haskell-hedgehog/pull/303).
  GH.mkName (undefined :: GH.Name a) <$> Gen.text (Range.linear 1 30) Gen.ascii

genUTCTime
  :: MonadGen n
  => n UTCTime
genUTCTime =
  let
    gYear = Gen.int (Range.linearFrom 1900 1970 2500)
    gMonth = Gen.int (Range.linear 1 12)
    -- fromGregorian automatically trims to valid dates, so 2001-02-31 becomes 2001-02-28
    gDay = Gen.int (Range.linear 1 31)
    hToS = (* 3600)
    gSeconds = Gen.int (Range.linearFrom (hToS 12) 0 86400)
    gUTCTimeDay = fromGregorian . fromIntegral <$> gYear <*> gMonth <*> gDay
    gDiffTime = secondsToDiffTime . fromIntegral <$> gSeconds
  in
    UTCTime <$> gUTCTimeDay <*> gDiffTime

genReferrer ::
  MonadGen m
  => m GH.Referrer
genReferrer =
  let
    genIntCount = Gen.int (Range.linear 0 1000000)
  in
    GH.Referrer <$> genName <*> genIntCount <*> genIntCount

-- List of referrers with unique names
genReferrers ::
  MonadGen m
  => m [GH.Referrer]
genReferrers =
  M.elems <$> Gen.map (Range.constant 0 15) ((\r -> (GH.referrer r, r)) <$> genReferrer)

genPop ::
  MonadGen m
  => m (Pop a)
genPop =
  Pop
  Nothing
  <$> (Position <$> Gen.int (Range.linear 1 10))
  <*> genName
  <*> genCount
  <*> genUniques
  <*> genId

genCount ::
  MonadGen m
  => m (Count a)
genCount =
  Count <$> Gen.int (Range.linear 0 1000000)

genUniques ::
  MonadGen m
  => m (Uniques a)
genUniques =
  Uniques <$> Gen.int (Range.linear 0 1000000)

-- genRepoStats ::
--   MonadGen m
--   => m RepoStats
-- genRepoStats =
--   RepoStats
--   <$> genName
--   <*> genUTCTime
--   <*> genStars
--   <*> genForks
--   <*> genReferrers
--   <*> genPaths
--   <*> genViews
--   <*> genClones
