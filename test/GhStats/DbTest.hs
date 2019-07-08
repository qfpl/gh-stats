{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module GhStats.DbTest where

import           Control.Exception                  (throw)
import           Control.Lens
    (Getter, Lens', mapped, to, (%~), (&), (+~), (.~), (?~), (^.), (^?), (<&>),
    _Wrapped, (#), Traversal', (^..))
import           Control.Lens.Extras                (is)
import           Control.Monad
    (join, void, zipWithM_, (<=<))
import           Control.Monad.Except
    (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Morph                (hoist)
import           Control.Monad.Reader
    (MonadReader, ReaderT, runReaderT)
import           Data.Foldable                      (traverse_)
import           Data.List                          (nub)
import qualified Data.Map                           as M
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy                         (Proxy (Proxy))
import           Data.Text                          (Text)
import           Data.Time
    (UTCTime (UTCTime), fromGregorian, secondsToDiffTime)
import           Data.Validation                    (Validation, validation, _Success, _Failure)
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Database.SQLite.Simple             (Connection, execute_)
import           Database.SQLite.SimpleErrors.Types
    (SQLiteResponse (SQLConstraintError, SQLOtherError))
import qualified GitHub                             as GH
import           GitHub.Lens
    (clones, trafficCount, trafficCountTimestamp, trafficCountUniques, views)

import           Hedgehog
    (Gen, GenT, MonadGen, MonadTest, Property, PropertyT, annotateShow, assert,
    evalEither, evalM, failure, forAll, property, success, tripping, (===))
import qualified Hedgehog.Gen               as Gen
import           Hedgehog.Internal.Property (forAllT)
import qualified Hedgehog.Range             as Range
import           Test.Tasty                 (TestName, TestTree, testGroup)
import           Test.Tasty.Hedgehog        (testProperty)

import GhStats.Db
    (dropEarliestAndLatest, initDb, insertClones, insertPop, insertPops,
    insertReferrers, insertRepoStats, insertRepoStatsRun, insertRepoStatsTree,
    insertRepoStatsesValidation, insertVC, insertViews,
    selectClonesForRepoId, selectPop, selectPopsForRepoStats, selectRepoStats,
    selectVC, selectVCsForRepoId, selectViewsForRepoId, toDbPath, toDbPops,
    toDbReferrer)
import GhStats.Db.Types
    (DbRepoStats (DbRepoStats, _dbRepoStatsName),
    HasTable (tableName, tableNameQ), Id (Id), Pop (Pop, popId, popRepoId),
    Position (Position), RepoStatsRun,
    VC (VC, _vcCount, _vcId, _vcRepoId, _vcRepoName, _vcTimestamp, _vcUniques),
    dbRepoStatsId, dbRepoStatsName, dbRepoStatsRunId)
import GhStats.Types
    (ValResult, AsSQLiteResponse, CVD, Count (Count), Error (SQLiteError), Forks (..),
    GhStatsM (GhStatsM), HasConnection, RepoStats, Stars (..),
    Uniques (Uniques), cvdExistingCount, cvdExistingUniques, cvdNewCount,
    cvdNewUniques, repoStatsName, repoStatsViews, runGhStatsM,
    _ConflictingVCData)

import GhStats.Gens
import GhStats.Test (GhStatsPropReaderT, GhStatsPropertyT, runGhStatsPropertyT)

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
  , ("insertClones is idempotent", testInsertClonesIdempotency)
  , ("insertViews only inserts new", testInsertViewsOnlyInsertsNew)
  , ("insertClones only inserts new", testInsertClonesOnlyInsertsNew)
  , ("insertViews returns list of conflicts", testInsertViewsConflicts)
  , ("insertClones returns list of conflicts", testInsertClonesConflicts)
  , ("insertRepoStatsesValidation collects all errors", testInsertTreeValidation)
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
    (runId, drsId) <- (evalEither =<<) $ insertRunAndStats conn drs
    selectedRepoStats <- (evalEither =<<) . hoozit conn $ selectRepoStats drsId
    let
      drs' = drs
           & dbRepoStatsId ?~ drsId
           & dbRepoStatsRunId .~ runId
    maybe failure (=== drs') selectedRepoStats

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
  (_, drsId) <- (evalEither =<<) $ insertRunAndStats conn drs
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
  evalM $ checkResult eRefId
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
  drs <- forAll genDbRepoStats
  viewsNoRsId <- forAll genVC
  resetDb conn
  (_, drsId) <- evalEither =<< insertRunAndStats conn drs
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
  forall (a :: *).
  HasTable a
  => Proxy a
  -> Connection
  -> PropertyT IO ()
testVCRoundTrip _ conn = do
  drs <- forAllT genDbRepoStats
  vcBadId <- forAllT genVC
  resetDb conn
  (_, drsId) <- evalEither =<< insertRunAndStats conn drs
  let vc = vcBadId {_vcRepoId = drsId, _vcRepoName = _dbRepoStatsName drs}
  vcId <- evalEither <=< hoozit conn $ insertVC @a vc
  let vcExpected = vc {_vcId = Just vcId}
  vcSelected <- evalEither <=< hoozit conn $ selectVC vcId
  Just vcExpected === vcSelected

type InsertVCFn a = Id DbRepoStats -> GH.Name GH.Repo -> a -> GhStatsM ()
type SelectVCFn a = Id DbRepoStats -> GhStatsM [VC a]

testInsertViewsIdempotency ::
  Connection
  -> PropertyT IO ()
testInsertViewsIdempotency =
  testInsertVCsIdempotency genGhViews insertViews selectViewsForRepoId views

testInsertClonesIdempotency ::
  Connection
  -> PropertyT IO ()
testInsertClonesIdempotency =
  testInsertVCsIdempotency genGhClones insertClones selectClonesForRepoId clones

testInsertVCsIdempotency ::
  ( HasTable a
  , Show a
  )
  => Gen a
  -> InsertVCFn a
  -> SelectVCFn a
  -> Getter a (Vector (GH.TrafficCount b))
  -> Connection
  -> PropertyT IO ()
testInsertVCsIdempotency gen ins sel gv conn = do
  drs <- forAll genDbRepoStats
  vcs <- forAll gen
  resetDb conn

  (_, drsId) <- evalEither =<< insertRunAndStats conn drs
  let insAction = evalEither <=< hoozit conn $ ins drsId (_dbRepoStatsName drs) vcs
  sequence_ [insAction, insAction]
  dbVCs <- evalEither <=< hoozit conn $ sel drsId
  (vcs ^. gv.to V.toList & dropEarliestAndLatest) === (vcToTrafficCount <$> dbVCs)

testInsertViewsOnlyInsertsNew ::
  Connection
  -> PropertyT IO ()
testInsertViewsOnlyInsertsNew =
  testInsertVCsOnlyInsertsNew genGhViews insertViews selectViewsForRepoId views

testInsertClonesOnlyInsertsNew ::
  Connection
  -> PropertyT IO ()
testInsertClonesOnlyInsertsNew =
  testInsertVCsOnlyInsertsNew genGhClones insertClones selectClonesForRepoId clones

testInsertVCsOnlyInsertsNew ::
  ( HasTable a
  , Show a
  )
  => Gen a
  -> InsertVCFn a
  -> SelectVCFn a
  -> Lens' a (Vector (GH.TrafficCount b))
  -> Connection
  -> PropertyT IO ()
testInsertVCsOnlyInsertsNew gen ins sel gv conn = do
  drs <- forAll genDbRepoStats
  vcs <- forAll gen
  n <- forAll $ Gen.int (Range.constant 0 5)
  resetDb conn

  (_, drsId) <- (evalEither =<<) $ insertRunAndStats conn drs
  let
    insertAction vcs' = evalEither <=< hoozit conn $ ins drsId (_dbRepoStatsName drs) vcs'
    tcvs1 = V.take n $ vcs ^. gv
    vcs1 = gv .~ tcvs1 $ vcs
  sequence_ [insertAction vcs1, insertAction vcs]
  dbViews <- evalEither <=< hoozit conn $ sel drsId
  (vcs ^. gv.to V.toList & dropEarliestAndLatest) === (vcToTrafficCount <$> dbViews)

testInsertViewsConflicts ::
  Connection
  -> PropertyT IO ()
testInsertViewsConflicts =
  testInsertVCsConflicts genGhViews insertViews selectViewsForRepoId views

testInsertClonesConflicts ::
  Connection
  -> PropertyT IO ()
testInsertClonesConflicts =
  testInsertVCsConflicts genGhClones insertClones selectClonesForRepoId clones

testInsertVCsConflicts ::
  forall a b.
  ( HasTable a
  , Show a
  )
  => Gen a
  -> InsertVCFn a
  -> SelectVCFn a
  -> Lens' a (Vector (GH.TrafficCount b))
  -> Connection
  -> PropertyT IO ()
testInsertVCsConflicts gen ins sel ltcs conn = do
  drs1 <- forAll genDbRepoStats
  drs2' <- forAll genDbRepoStats
  vc <- forAll gen
  resetDb conn

  let
    repoName = drs1 ^. dbRepoStatsName
    drs2 = dbRepoStatsName .~ repoName $ drs2'
    moddedVCs = makeConflicting ltcs vc
    ensureFailed = either pure $ error "Expected insertion of conflicting view data to fail"

  (_, drsId1) <- (evalEither =<<) $ insertRunAndStats conn drs1
  (_, drsId2) <- (evalEither =<<) $ insertRunAndStats conn drs2
  evalEither <=< hoozit conn $ ins drsId1 repoName vc
  insModError <- evalM . ensureFailed <=< hoozit conn $ ins drsId2 repoName moddedVCs

  evalM $ checkConflicts insModError

  dbViews1 <- evalEither <=< hoozit conn $ sel drsId1
  (vc ^. ltcs.to V.toList & dropEarliestAndLatest) === (vcToTrafficCount <$> dbViews1)

  dbViews2 <- evalEither <=< hoozit conn $ sel drsId2
  assert $ null dbViews2

testInsertTreeValidation ::
  Connection
  -> PropertyT IO ()
testInsertTreeValidation conn = do
  rss <- (fmap . fmap) (_Success #) $ forAll genRepoStatses
  resetDb conn

  let
    ghViews ::
      Traversal' [ValResult Error RepoStats] (Vector (GH.TrafficCount GH.View))
    ghViews =
      traverse._Success.repoStatsViews.views

    conflicting =
      makeConflicting ghViews rss

    expectedErrorCount =
      rss ^.. ghViews & length

  rsrId <- evalEither <=< hoozit conn $ insertRepoStatsRun
  liftIO $ insertRepoStatsesValidation conn rsrId rss
  vcs <- liftIO . fmap sequenceA $ insertRepoStatsesValidation conn rsrId conflicting
  evalM $ validation (traverse_ checkConflicts) (const $ error "Expecting failure") vcs
  (vcs ^? _Failure <&> length) === Just expectedErrorCount

makeConflicting ::
  Traversal' a (Vector (GH.TrafficCount b))
  -> a
  -> a
makeConflicting ltcs vc =
  vc & ltcs.mapped.trafficCount +~ 1 & ltcs.mapped.trafficCountUniques +~ 2

checkConflicts ::
  Error
  -> PropertyT IO ()
checkConflicts e = do
  let
    checkConflict cvd = do
      cvd ^. cvdNewCount === (cvd ^. cvdExistingCount & _Wrapped %~ (+1))
      cvd ^. cvdNewUniques === (cvd ^. cvdExistingUniques & _Wrapped %~ (+2))
  cvds <- e ^? _ConflictingVCData & maybe (error "Expecting conflicting VC data") pure
  traverse_ checkConflict cvds

vcToTrafficCount ::
  VC a
  -> GH.TrafficCount b
vcToTrafficCount VC{_vcTimestamp, _vcCount, _vcUniques} =
  GH.TrafficCount {
    GH.trafficCountTimestamp = _vcTimestamp
  , GH.trafficCount = _vcCount ^. _Wrapped
  , GH.trafficCountUniques = _vcUniques ^. _Wrapped
  }

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
  forall (a :: *).
  HasTable a
  => Proxy a
  -> Connection
  -> PropertyT IO ()
popRoundTrip _ conn =  do
  drs <- forAll genDbRepoStats
  popBadRepoId <- forAll genPop
  resetDb conn
  (_, drsId) <- (evalEither =<<) $ insertRunAndStats conn drs
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

insertRunAndStats ::
  Connection
  -> DbRepoStats
  -> PropertyT IO (Either Error (Id RepoStatsRun, Id DbRepoStats))
insertRunAndStats conn rs = do
  rsrId <- evalEither =<< hoozit conn insertRepoStatsRun
  let mRsId = hoozit conn . insertRepoStats $ dbRepoStatsRunId .~ rsrId $ rs
  (fmap . fmap) (rsrId,) mRsId
