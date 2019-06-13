{-# LANGUAGE ConstraintKinds #-}

module GhStats.DbTest where

import           Control.Monad              (void)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader)
import           Data.Time                  (UTCTime)
import           Database.SQLite.Simple     (Connection)
import qualified GitHub                     as GH

import           Hedgehog                   (MonadGen, forAll, property)
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import           GhStats.Db                 (DbRepoStats (DbRepoStats), Id (Id),
                                             initDb, insertRepoStats)
import           GhStats.Types              (AsSQLiteResponse, Forks (..),
                                             HasConnection, Stars (..))

import           GhStats.Test               (runGhStatsTestM)

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
testDb conn = testGroup "GhStats.Db" [
    testRepoStatsRoundTrip conn
  ]

testRepoStatsRoundTrip ::
  Connection
  -> TestTree
testRepoStatsRoundTrip conn =
  testProperty "select . insert" . property . runGhStatsTestM conn $ do
    drs <- forAllT genDbRepoStats
    void $ insertRepoStats drs
    -- selectRepoStats drsId

genDbRepoStats ::
   MonadGen m
   => m DbRepoStats
genDbRepoStats =
  DbRepoStats
  <$> genId
  <*> genName
  <*> genUTCTime
  <*> genStars
  <*> genForks

genStars ::
  MonadGen m
  => m Stars
genStars =
  Stars <$> (Gen.int (Range.linear 0 1000000))

genForks ::
  MonadGen m
  => m Forks
genForks =
  Forks <$> (Gen.int (Range.linear 0 1000000))

genId ::
  MonadGen m
  => m (Id a)
genId =
  Id <$> (Gen.int64 Range.linearBounded)

genName ::
  MonadGen m
  => m (GH.Name a)
genName =
  N <$> Gen.text

genTimestamp
  :: MonadGen n
  => n UTCTime
genTimestamp =
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
    fmap (utcToLocalTime utc) . UTCTime <$> gUTCTimeDay <*> gDiffTime
