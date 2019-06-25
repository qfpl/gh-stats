{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GhStats.DbTest where

import           Control.Exception                  (throw)
import           Control.Monad                      (join, void, zipWithM_,
                                                     (<=<))
import           Control.Monad.Except               (ExceptT, MonadError,
                                                     runExceptT)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Morph                (hoist)
import           Control.Monad.Reader               (MonadReader, ReaderT,
                                                     runReaderT)
import           Data.Foldable                      (traverse_)
import           Data.List                          (nub)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust)
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Text                          (Text)
import           Data.Time                          (UTCTime (UTCTime),
                                                     fromGregorian,
                                                     secondsToDiffTime)
import qualified Data.Vector                        as V
import           Database.SQLite.Simple             (Connection, execute_)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse (SQLConstraintError, SQLOtherError))
import qualified GitHub                             as GH

import           Hedgehog                           (Gen, GenT, MonadGen,
                                                     MonadTest, Property,
                                                     PropertyT, annotateShow,
                                                     evalEither, evalM, failure,
                                                     forAll, property, success,
                                                     tripping, (===))
import qualified Hedgehog.Gen                       as Gen
import           Hedgehog.Internal.Property         (forAllT)
import qualified Hedgehog.Range                     as Range
import           Test.Tasty                         (TestName, TestTree,
                                                     testGroup)
import           Test.Tasty.Hedgehog                (testProperty)

import           GhStats.Db                         (initDb, insertPop,
                                                     insertPops,
                                                     insertReferrers,
                                                     insertRepoStats, insertVC,
                                                     insertViews, selectPop,
                                                     selectPopsForRepoStats,
                                                     selectRepoStats, selectVC,
                                                     selectViewsForRepoId,
                                                     toDbPath, toDbPops,
                                                     toDbReferrer)
import           GhStats.Db.Types                   (DbRepoStats (DbRepoStats, _dbRepoStatsId, _dbRepoStatsName),
                                                     HasTable (tableName, tableNameQ),
                                                     Id (Id),
                                                     Pop (Pop, popId, popRepoId),
                                                     Position (Position),
                                                     VC (VC, _vcId, _vcRepoId, _vcRepoName))
import           GhStats.Types                      (AsSQLiteResponse,
                                                     Count (Count),
                                                     Error (SQLiteError),
                                                     Forks (..),
                                                     GhStatsM (GhStatsM),
                                                     HasConnection, RepoStats,
                                                     Stars (..),
                                                     Uniques (Uniques),
                                                     runGhStatsM)

import           GhStats.Gens
import           GhStats.Test                       (GhStatsPropReaderT,
                                                     GhStatsPropertyT,
                                                     runGhStatsPropertyT)

testDb ::
  Connection
  -> TestTree
testDb conn =
  testGroup "GhStats.Db" . fmap mkProp $ [
    ("DbRepoStats round trip", testRepoStatsRoundTrip)
  , ("Referrer round trip", testReferrerRoundTrip)
  , ("Referrers round trip", testReferrersRoundTrip)
  , ("Path round trip", testPathRoundTrip)
  , ("Paths round trip", testPathsRoundTrip)
  , ("No repo for referrer fails", testNonExistentRepo)
  , ("View round trip", testViewRoundTrip)
  , ("Clone round trip", testClonesRoundTrip)
  , ("repo_name consistency check", testRepoNameTrigger)
  , ("insertViews is idempotent", testInsertViewsIdempotency)
  , ("insertViews only inserts new", testInsertViewsOnlyInsertsNew)
  ]
  where
    mkProp (name, prop) =
      testProperty name . property $ prop conn

testRepoStatsRoundTrip ::
  Connection
  -> PropertyT IO ()
testRepoStatsRoundTrip conn = do
    drs <- forAll genDbRepoStats
    resetDb conn
    drsId <- (evalEither =<<) . hoozit conn $ insertRepoStats drs
    selectedRepoStats <- (evalEither =<<) . hoozit conn $ selectRepoStats drsId
    let
      drsWithId =
        drs {_dbRepoStatsId = Just drsId}
    maybe failure (=== drsWithId) selectedRepoStats

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
testReferrersRoundTrip =
  testPopsRoundTrip genReferrers toDbReferrer

testPathsRoundTrip ::
  Connection
  -> PropertyT IO ()
testPathsRoundTrip =
  testPopsRoundTrip genPaths toDbPath

testPopsRoundTrip ::
  ( Show a
  , HasTable a
  )
  => Gen [a]
  -> (Id DbRepoStats -> Position a -> a -> Pop a)
  -> Connection
  -> PropertyT IO ()
testPopsRoundTrip genA convert conn = do
  drs <- forAll genDbRepoStats
  as <- forAll genA
  resetDb conn
  drsId <- (evalEither =<<) . hoozit conn $ insertRepoStats drs
  let
    dbPopsExpected = toDbPops (convert drsId) as
  (evalEither =<<) . hoozit conn $ insertPops (convert drsId) as
  dbPopsActual <- (evalEither =<<) . hoozit conn $ selectPopsForRepoStats drsId
  dbPopsEqual dbPopsExpected dbPopsActual

testNonExistentRepo ::
  Connection
  -> PropertyT IO ()
testNonExistentRepo conn = do
  ref <- forAllT genPop
  resetDb conn
  eRefId <- hoozit conn $ insertPop ref
  checkResult eRefId
  where
    checkResult ::
      Either Error (Id GH.Referrer)
      -> PropertyT IO ()
    checkResult =
      either assertIsConstraintError (const err)
    err = error "Expected failure due to foreign key constraint"
    assertIsConstraintError e = case e of
      SQLiteError (SQLConstraintError _ _) -> success
      e'                                   -> throw e'

testRepoNameTrigger ::
  Connection
  -> PropertyT IO ()
testRepoNameTrigger conn = do
  rs <- forAll genDbRepoStats
  viewsNoRsId <- forAll genVC
  resetDb conn
  drsId <- evalEither <=< hoozit conn $ insertRepoStats rs
  let views = viewsNoRsId {_vcRepoId = drsId}
  evId <- hoozit conn $ insertVC @GH.Views views
  either checkIsOtherError bad evId
  where
    bad =
      error "Expected failure due to trigger on insert --- repo name not found in repos"
    checkIsOtherError e = case e of
      SQLiteError soe@(SQLOtherError _) -> success
      e'                                -> throw e'

testViewRoundTrip ::
  Connection
  -> PropertyT IO ()
testViewRoundTrip =
  testVCRoundTrip (Proxy :: Proxy GH.Views)

testClonesRoundTrip ::
  Connection
  -> PropertyT IO ()
testClonesRoundTrip =
  testVCRoundTrip (Proxy :: Proxy GH.Clones)

testVCRoundTrip ::
  forall a.
  HasTable a
  => Proxy a
  -> Connection
  -> PropertyT IO ()
testVCRoundTrip _ conn = do
  drs <- forAllT genDbRepoStats
  vcBadId <- forAllT genVC
  resetDb conn
  drsId <- (evalEither =<<) . hoozit conn $ insertRepoStats drs
  let vc = vcBadId {_vcRepoId = drsId, _vcRepoName = _dbRepoStatsName drs}
  vcId <- (evalEither =<<) . hoozit conn $ insertVC @a vc
  let vcExpected = vc {_vcId = Just vcId}
  vcSelected <- (evalEither =<<) . hoozit conn $ selectVC vcId
  Just vcExpected === vcSelected

testInsertViewsIdempotency ::
  Connection
  -> PropertyT IO ()
testInsertViewsIdempotency conn = do
  drs <- forAll genDbRepoStats
  vs <- forAll genGhViews
  resetDb conn

  drsId <- evalEither <=< hoozit conn $ insertRepoStats drs
  let ins = evalEither <=< hoozit conn $ insertViews drsId (_dbRepoStatsName drs) vs
  sequence_ [ins,ins]
  dbViews <- evalEither <=< hoozit conn $ selectViewsForRepoId drsId
  length dbViews === length (GH.views vs)

testInsertViewsOnlyInsertsNew ::
  Connection
  -> PropertyT IO ()
testInsertViewsOnlyInsertsNew conn = do
  drs <- forAll genDbRepoStats
  vs <- forAll genGhViews
  n <- forAll $ Gen.int (Range.constant 0 5)
  resetDb conn

  drsId <- evalEither <=< hoozit conn $ insertRepoStats drs
  let
    ins vs' = evalEither <=< hoozit conn $ insertViews drsId (_dbRepoStatsName drs) vs'
    tcvs1 = V.take n $ GH.views vs
    vs1 = vs {GH.views = tcvs1}
  sequence_ [ins vs1, ins vs]
  dbViews <- evalEither <=< hoozit conn $ selectViewsForRepoId drsId
  length dbViews === length (GH.views vs)

resetDb ::
  MonadIO m
  => Connection
  -> m ()
resetDb conn =
  liftIO $ traverse_ (execute_ conn . ("DELETE FROM " <>)) [
      tableNameQ @GH.Views
    , tableNameQ @GH.Clones
    , tableNameQ @GH.Referrer
    , tableNameQ @GH.PopularPath
    , tableNameQ @DbRepoStats
    ]

dbPopsEqual ::
  ( Applicative m
  , MonadTest m
  )
  => [Pop a]
  -> [Pop a]
  -> m ()
dbPopsEqual =
  zipWithM_ (\e a -> e === (a {popId = Nothing}))

popRoundTrip ::
  forall a.
  HasTable a
  => Proxy a
  -> Connection
  -> PropertyT IO ()
popRoundTrip _ conn =  do
  drs <- forAllT genDbRepoStats
  popBadRepoId <- forAllT genPop
  resetDb conn
  drsId <- (evalEither =<<) . hoozit conn $ insertRepoStats drs
  let pop = popBadRepoId {popRepoId = drsId}
  popId' <- (evalEither =<<) . hoozit conn $ insertPop @a pop
  mPopSelected <- (evalEither =<<) . hoozit conn $ selectPop popId'
  let popWithId = pop {popId = Just popId'}
  maybe failure (=== popWithId) mPopSelected

hoozit ::
  Connection
  -> GhStatsM a
  -> PropertyT IO (Either Error a)
hoozit conn (GhStatsM m)=
  liftIO . runExceptT $ runReaderT m conn
