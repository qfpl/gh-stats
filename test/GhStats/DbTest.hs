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
import           Database.SQLite.Simple             (Connection, execute_)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse (SQLConstraintError))
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
                                                     selectPop,
                                                     selectPopsForRepoStats,
                                                     selectRepoStats, selectVC,
                                                     toDbPath, toDbPops,
                                                     toDbReferrer)
import           GhStats.Db.Types                   (Count (Count), DbRepoStats (DbRepoStats, _dbRepoStatsId),
                                                     HasTable (tableName, tableNameQ),
                                                     Id (Id),
                                                     Pop (Pop, popId, popRepoId),
                                                     Position (Position),
                                                     Uniques (Uniques),
                                                     VC (VC, _vcId, _vcRepoId))
import           GhStats.Types                      (AsSQLiteResponse,
                                                     Error (SQLiteError),
                                                     Forks (..),
                                                     GhStatsM (GhStatsM),
                                                     HasConnection, RepoStats,
                                                     Stars (..), runGhStatsM)

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
  liftIO $ checkResult eRefId
  where
    checkResult ::
      Either Error (Id GH.Referrer)
      -> IO ()
    checkResult =
      either assertIsConstraintError (const err)
    err = error "Expected failure due to foreign key constraint"
    assertIsConstraintError :: Error -> IO ()
    assertIsConstraintError e = case e of
      SQLiteError (SQLConstraintError _ _) -> pure ()
      e'                                   -> throw e'

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
  let vc = vcBadId {_vcRepoId = drsId}
  vcId <- (evalEither =<<) . hoozit conn $ insertVC @a vc
  let vcExpected = vc {_vcId = Just vcId}
  vcSelected <- (evalEither =<<) . hoozit conn $ selectVC vcId
  Just vcExpected === vcSelected

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

genDbRepoStats ::
   MonadGen m
   => m DbRepoStats
genDbRepoStats =
  DbRepoStats
  Nothing
  <$> genName
  <*> genUTCTime
  <*> (Stars <$> genIntCount)
  <*> (Forks <$> genIntCount)
  <*> genCount
  <*> genUniques
  <*> genCount
  <*> genUniques

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
  GH.Referrer <$> genName <*> genIntCount <*> genIntCount

-- List of referrers with unique names
genReferrers ::
  MonadGen m
  => m [GH.Referrer]
genReferrers =
  M.elems <$> Gen.map (Range.constant 0 15) ((\r -> (GH.referrer r, r)) <$> genReferrer)

genPath ::
  MonadGen m
  => m GH.PopularPath
genPath =
  let
    genText = Gen.text (Range.linear 1 100) Gen.ascii
  in
    GH.PopularPath <$> genText <*> genText <*> genIntCount <*> genIntCount

genPaths ::
  MonadGen m
  => m [GH.PopularPath]
genPaths =
  genUniqueList genPath GH.popularPath

genUniqueList ::
  ( MonadGen m
  , Ord b
  )
  => m a
  -> (a -> b)
  -> m [a]
genUniqueList gen f =
  fmap M.elems . Gen.map (Range.constant 0 15) . fmap (\a -> (f a, a)) $ gen

genIntCount ::
  MonadGen m
  => m Int
genIntCount =
  Gen.int (Range.linear 0 1000000)

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

genVC ::
  MonadGen m
  => m (VC a)
genVC =
  VC Nothing
  <$> genUTCTime
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
