{-# LANGUAGE ConstraintKinds #-}

module GhStats.DbTest where

import           Control.Monad              (void, (<=<))
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)
import           Data.Maybe                 (fromJust)
import           Data.Time                  (UTCTime (UTCTime), fromGregorian,
                                             secondsToDiffTime)
import           Database.SQLite.Simple     (Connection)
import qualified GitHub                     as GH

import           Hedgehog                   (MonadGen, failure, forAll,
                                             property, tripping, (===))
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           GhStats.Db                 (DbReferrer (DbReferrer, dbRefId), DbRepoStats (DbRepoStats, _dbRepoStatsId),
                                             Id (Id), initDb, insertReferrer,
                                             insertRepoStats, selectReferrer,
                                             selectRepoStats)
import           GhStats.Types              (AsSQLiteResponse, Forks (..),
                                             HasConnection, Stars (..),
                                             runGhStatsM)

import           GhStats.Test               (GhStatsPropertyT,
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
  ]

testRepoStatsRoundTrip ::
  Connection
  -> TestTree
testRepoStatsRoundTrip conn =
  testProperty "select . insert $ dbRepoStats" . property . runGhStatsPropertyT conn $ do
    drs <- forAllT genDbRepoStats
    drsId <- insertRepoStats drs
    let
      drsWithId =
        drs {_dbRepoStatsId = Just drsId}
    maybe failure (=== drsWithId) =<< selectRepoStats drsId

testReferrerRoundTrip ::
  Connection
  -> TestTree
testReferrerRoundTrip conn =
  testProperty "select . insert $ dbReferrer" . property . runGhStatsPropertyT conn $ do
    drs <- forAllT genDbRepoStats
    dr <- forAllT genDbReferrer
    drsId <- insertRepoStats drs
    drId <- insertReferrer dr
    let
      drWithId =
        dr {dbRefId = Just drId}
    maybe failure (=== drWithId) =<< selectReferrer drId

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

genDbReferrer ::
  MonadGen m
  => m DbReferrer
genDbReferrer =
  DbReferrer
  Nothing
  <$> Gen.int (Range.linear 1 10)
  <*> genName
  <*> Gen.int (Range.linear 0 1000000)
  <*> Gen.int (Range.linear 0 1000000)
  <*> genId
