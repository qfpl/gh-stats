{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GhStats.DbTest where

import           Control.Monad              (void, zipWithM_, (<=<))
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader)
import           Data.Foldable              (traverse_)
import           Data.List                  (nub)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (UTCTime), fromGregorian,
                                             secondsToDiffTime)
import           Database.SQLite.Simple     (Connection, execute_)
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
                                             DbView,
                                             HasTable (tableName, tableNameQ),
                                             Id (Id), Pop (Pop, popId),
                                             Position (Position),
                                             Uniques (Uniques))
import           GhStats.Types              (AsSQLiteResponse, Forks (..),
                                             HasConnection, RepoStats,
                                             Stars (..), runGhStatsM)

import           GhStats.Test               (GhStatsPropReaderT,
                                             GhStatsPropertyT,
                                             runGhStatsPropertyT)

testDb ::
  Connection
  -> TestTree
testDb conn =
  testGroup "GhStats.Db" . fmap mkProp $ [
    ("DbRepoStats round trip", testRepoStatsRoundTrip)
  , ("Referrer round trip", testReferrerRoundTrip)
  , ("Path round trip", testPathRoundTrip)
  , ("Referrers round trip", testReferrersRoundTrip)
  ]
  where
    mkProp (name, prop) =
      testProperty name . property $ resetDb conn >> prop conn

testRepoStatsRoundTrip ::
  Connection
  -> PropertyT IO ()
testRepoStatsRoundTrip conn =
  runGhStatsPropertyT conn $ do
    resetDb conn
    drs <- forAllT genDbRepoStats
    drsId <- insertRepoStats drs
    let
      drsWithId =
        drs {_dbRepoStatsId = Just drsId}
    maybe failure (=== drsWithId) =<< selectRepoStats drsId

testReferrerRoundTrip ::
  Connection
  -> PropertyT IO ()
testReferrerRoundTrip =
  popRoundTrip (Proxy :: Proxy GH.Referrer)

testPathRoundTrip ::
  Connection
  -> PropertyT IO ()
testPathRoundTrip =
  popRoundTrip (Proxy :: Proxy GH.PopularPath)

testReferrersRoundTrip ::
  Connection
  -> PropertyT IO ()
testReferrersRoundTrip conn = runGhStatsPropertyT conn $ do
  drs <- forAllT genDbRepoStats
  refs <- forAllT genReferrers
  drsId <- insertRepoStats drs
  let
    dbRefsExpected = toDbReferrers drsId refs
  insertReferrers drsId refs
  dbRefsActual <- selectReferrersForRepoStats drsId
  dbReferrersEqual dbRefsExpected dbRefsActual


-- testViewsRoundTrip ::
--   Connection
--   -> TestTree
-- testViewsRoundTrip conn =
--   ghStatsProp "views round trip" conn $ do
--     drs <- forAllT genDbRepoStats
--     drsId <- insertRepoStats drs
--     dbView <- forAllT genDbView

resetDb ::
  MonadIO m
  => Connection
  -> m ()
resetDb conn =
  liftIO $ traverse_ (execute_ conn . ("DELETE FROM " <>)) [
      tableNameQ @DbRepoStats
    , tableNameQ @GH.Referrer
    , tableNameQ @GH.PopularPath
    , tableNameQ @DbView
    ]

dbReferrersEqual ::
  [Pop GH.Referrer]
  -> [Pop GH.Referrer]
  -> GhStatsPropertyT ()
dbReferrersEqual =
  zipWithM_ (\e a -> e === (a {popId = Nothing}))

popRoundTrip ::
  forall a.
  HasTable a
  => Proxy a
  -> Connection
  -> PropertyT IO ()
popRoundTrip _ conn = runGhStatsPropertyT conn $ do
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
  <*> genCount
  <*> genUniques
  <*> genCount
  <*> genUniques

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

-- genDbView ::
--   MonadGen m
--   => m DbView
-- genDbView =
--   DbView Nothing
--   <$> genUTCTime
--   <*> genCount
--   <*> genUniques
--   <*>

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
