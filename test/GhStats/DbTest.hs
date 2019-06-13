module GhStats.DbTest where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError)

import Hedgehog (MonadGen, forAll)
import Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range
import Test.Tasty (TestTree, testGroup)

import GhStats.Types (HasConnection, AsSQLiteResponse)
import GhStats.Db (initDb, Id (Id), DbRepoStats (DbRepoStats))

type TestConstraints e r m = (
    MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  , MonadReader r m
  , HasConnection r
  , MonadGen m
  )

testDb ::
  TestConstraints e r m
  => m TestTree
testDb = testGroup "GhStats.Db" [
    testRepoStatsRoundTrip
  ]

testRepoStatsRoundTrip ::
  TestConstraints e r m
  => m TestTree
testRepoStatsRoundTrip = testProperty "select . insert" . property $ do
  drs <- forAllT genDbRepoStats
  insertRepoStats 


genDbRepoStats ::
   MonadGen m
   => m DbRepoStats
genDbRepoStats =
  DbRepoStats
  <$> genId
  <*> genName

genId ::
  MonadGen m
  => Gen (Id a)
genId =
  Id <$> (Gen.int64 (Range.linear ))

genName ::
  MonadGen m
  => m (Name a)
genName =
  N <$> Gen.text
