{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GhStats.Db
  ( initDb

  -- * Inserts
  , addToDb
  , insertClones
  , insertPop
  , insertPops
  , insertReferrers
  , insertRepoStats
  , insertRepoStatsRun
  , insertRepoStatsTree
  , insertVC
  , insertViews

  -- * Queries
  , selectClonesForRepoId
  , selectViewsForRepoId
  , selectClones
  , selectLatestRepoStats
  , selectPop
  , selectPopsForRepoStats
  , selectRepoStats
  , selectVC
  , selectVCsForRepoId
  , selectViews

  -- * Helpers
  , DbConstraints
  , toDbPath
  , toDbPops
  , toDbReferrer
  , runDb
  ) where

import           Control.Lens                 (view, (^.))
import           Control.Lens.Indexed
    (FunctorWithIndex, TraversableWithIndex, imap)
import           Control.Monad                (void, (<=<))
import           Control.Monad.Error.Lens     (throwing)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, ask)
import           Data.Bool                    (bool)
import           Data.Foldable                (Foldable, toList, traverse_)
import           Data.List.NonEmpty           (NonEmpty)
import qualified Data.List.NonEmpty           as NE
import qualified Data.Map.Merge.Strict        as M
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as M
import           Data.Proxy                   (Proxy (Proxy))
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import qualified Data.Validation              as V
import           Database.SQLite.Simple
    (Connection, FromRow, Only (Only), Query (Query), ToRow, execute,
    executeMany, execute_, lastInsertRowId, query, query_)
import           Database.SQLite.SimpleErrors (runDBAction)
import qualified GitHub                       as GH

import GhStats.Db.Types
import GhStats.Types
    (AsError (_ConflictingVCData, _TooManyResults),
    AsSQLiteResponse (_SQLiteResponse), CVD (CVD), Count (Count),
    HasConnection (connection), RepoStats (..), Uniques (Uniques),
    repoStatsClones, repoStatsName, repoStatsPopularPaths,
    repoStatsPopularReferrers, repoStatsViews)

type DbConstraints e r m =
  ( MonadReader r m
  , HasConnection r
  , MonadIO m
  , AsSQLiteResponse e
  , MonadError e m
  )

initDb ::
  DbConstraints e r m
  => m ()
initDb =
  let
    enableForeignKeys = "PRAGMA foreign_keys = ON;"
    toQ = Query . T.intercalate "\n"
    qRepoStatsRuns = toQ [
        "CREATE TABLE IF NOT EXISTS " <> tableName @RepoStatsRun
      , "( id INTEGER PRIMARY KEY"
      , ", timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP"
      , ")"
      ]
    qRepoStats = toQ [
        "CREATE TABLE IF NOT EXISTS ", tableName @RepoStats, " "
      , "( id INTEGER PRIMARY KEY"
      , ", name TEXT NOT NULL"
      , ", timestamp TEXT NOT NULL"
      , ", stars INTEGER NOT NULL"
      , ", forks INTEGER NOT NULL"
      , ", views INTEGER NOT NULL"  -- views over the last 14 days
      , ", unique_views INTEGER NOT NULL" -- unique views over the last 14 days
      , ", clones INTEGER NOT NULL" -- clones over the last 14 days
      , ", unique_clones INTEGER NOT NULL" -- unique_clones over the last 14 days
      , ", repo_stats_run INTEGER NOT NULL"
      , ", FOREIGN KEY(repo_stats_run) REFERENCES " <> tableName @RepoStatsRun <> "(id)"
      , ")"
      ]
    qPop tn = toQ [
        "CREATE TABLE IF NOT EXISTS ", tn, " "
      , "( id INTEGER PRIMARY KEY"
      , ", position INTEGER NOT NULL"
      , ", name TEXT NOT NULL"
      , ", count INTEGER NOT NULL"
      , ", uniques INTEGER NOT NULL"
      , ", repo_id INTEGER NOT NULL"
      , ", FOREIGN KEY(repo_id) REFERENCES " <> tableName @RepoStats <> "(id)"
      , ", UNIQUE (position, repo_id)"
      , ", UNIQUE (name, repo_id)"
      , ")"
      ]
    qVC tn = toQ [
        "CREATE TABLE IF NOT EXISTS ", tn, " "
      , "( id INTEGER PRIMARY KEY"
      , ", timestamp TEXT NOT NULL"
      , ", count INTEGER NOT NULL"
      , ", uniques INTEGER NTO NULL"
      , ", repo_id INTEGER NOT NULL"
      , ", repo_name TEXT NOT NULL"
      , ", FOREIGN KEY(repo_id) REFERENCES " <> tableName @RepoStats <> "(id)"
      , ", UNIQUE (timestamp, repo_id)"
      , ", UNIQUE (timestamp, repo_name)"
      , ")"
      ]
    qRepoNameTrigger tn = toQ [
        "CREATE TRIGGER IF NOT EXISTS " <> tn <> "_repo_name"
      , "AFTER INSERT ON " <> tn
      , "BEGIN"
      , "  SELECT RAISE(ROLLBACK, 'repo name not valid')"
      , "  FROM " <> tn
      , "  WHERE NEW.repo_name <> ("
      , "    SELECT name FROM " <> tableName @RepoStats
      , "    WHERE id = NEW.repo_id"
      , "  );"
      , "END"
      ]
    qReferrer = qPop $ tableName @GH.Referrer
    qPaths = qPop $ tableName @GH.PopularPath
    qViews = qVC $ tableName @GH.Views
    qClones = qVC $ tableName @GH.Clones
    qViewsRepoNameCheck = qRepoNameTrigger $ tableName @GH.Views
    qClonesRepoNameCheck = qRepoNameTrigger $ tableName @GH.Clones
  in
    withConnIO $ \conn ->
      traverse_ (execute_ conn) [
        enableForeignKeys
      , qRepoStatsRuns
      , qRepoStats
      , qReferrer
      , qPaths
      , qViews
      , qClones
      , qViewsRepoNameCheck
      , qClonesRepoNameCheck
      ]

addToDb ::
  ( DbConstraints e r m
  , Traversable t
  , AsError e
  )
  => t RepoStats
  -> m ()
addToDb =
  void . traverse insertRepoStatsTree

insertRepoStatsTree ::
  ( DbConstraints e r m
  , AsError e
  )
  => RepoStats
  -> m (Id DbRepoStats)
insertRepoStatsTree rs = do
  let
    repoName = rs ^. repoStatsName
  rsrId <- insertRepoStatsRun
  rsId <- insertRepoStats $ toDbRepoStats rsrId rs
  insertReferrers rsId $ rs ^. repoStatsPopularReferrers
  insertPaths rsId $ rs ^. repoStatsPopularPaths
  insertViews rsId repoName (rs ^. repoStatsViews)
  insertClones rsId repoName (rs ^. repoStatsClones)
  pure rsId


insertRepoStatsRun ::
  DbConstraints e r m
  => m (Id RepoStatsRun)
insertRepoStatsRun =
  let
    q = "INSERT INTO " <> tableNameQ @RepoStatsRun <> " DEFAULT VALUES"
  in
    withConnIO $ \conn -> do
      execute_ conn q
      fmap Id $ lastInsertRowId conn

type VCMap a = Map (GH.Name GH.Repo, UTCTime) (Count a, Uniques a)

insertViews ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbRepoStats
  -> GH.Name GH.Repo
  -> GH.Views
  -> m ()
insertViews rsId rsName =
  maybe (pure ()) ins . NE.nonEmpty . toList . GH.views
  where
    ins = insertVCs (Proxy :: Proxy GH.Views) rsId rsName

insertClones ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbRepoStats
  -> GH.Name GH.Repo
  -> GH.Clones
  -> m ()
insertClones rsId rsName =
  maybe (pure ()) ins . NE.nonEmpty . toList . GH.clones
  where
    ins = insertVCs (Proxy :: Proxy GH.Clones) rsId rsName

insertVCs ::
  forall a te e r m.
  ( DbConstraints e r m
  , AsError e
  , HasTable a
  )
  => Proxy a
  -> Id DbRepoStats
  -> GH.Name GH.Repo
  -> NonEmpty (GH.TrafficCount te)
  -> m ()
insertVCs p rsId repoName tcvs = do
  let
    dateRange = findDateRange tcvs
    toViewMapElem GH.TrafficCount{GH.trafficCountTimestamp, GH.trafficCount, GH.trafficCountUniques} =
      ((repoName, trafficCountTimestamp), (Count trafficCount, Uniques trafficCountUniques))
    new = M.fromList . NE.toList $ toViewMapElem <$> tcvs
    alwaysSkip = M.filterAMissing $ \_ _ -> pure False
    alwaysInsert = M.filterAMissing $ \_ _ -> pure True
    liftToInsertMap = either (throwing _ConflictingVCData) pure . V.toEither
    toInsertList = fmap (\((n,t),(c,u)) -> VC Nothing t c u rsId n) . M.toList
  existing <- selectVCsBetweenDates @a dateRange repoName
  insertMap <- liftToInsertMap $ M.mergeA alwaysSkip alwaysInsert checkOverlap existing new
  withConnIO $ \conn ->
    executeMany conn (insertVCQ p) . toInsertList $ insertMap

selectViewsForRepoId ::
  forall e r m.
  DbConstraints e r m
  => Id DbRepoStats
  -> m [VC GH.Views]
selectViewsForRepoId =
  selectVCsForRepoId @GH.Views

selectClonesForRepoId ::
  forall e r m.
  DbConstraints e r m
  => Id DbRepoStats
  -> m [VC GH.Clones]
selectClonesForRepoId =
  selectVCsForRepoId @GH.Clones

selectVCsForRepoId ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => Id DbRepoStats
  -> m [VC a]
selectVCsForRepoId drsId =
  let
    q =  selectVCFieldsQ
      <> "FROM " <> tableNameQ @a <> " "
      <> "WHERE repo_id = ?"
  in
    withConnIO $ \conn ->
      query conn q (Only drsId)

checkOverlap ::
  M.WhenMatched (V.Validation [CVD])
                (GH.Name GH.Repo, UTCTime)
                (Count a, Uniques a)
                (Count a, Uniques a)
                (Count a, Uniques a)
checkOverlap =
  M.zipWithMaybeAMatched $ \(n,t) (c1,u1) (c2,u2) ->
    bool (V.Failure [CVD n t c1 u1 c2 u2])
         (pure Nothing)
         (c1 == c2 && u1 == u2)

selectVCsBetweenDates ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => (UTCTime, UTCTime)
  -> GH.Name GH.Repo
  -> m (VCMap a)
selectVCsBetweenDates (start, end) repoName =
  let
    q =  "SELECT repo_name, timestamp, count, uniques "
      <> "FROM " <> tableNameQ @a <> " "
      <> "WHERE repo_name = ? "
      <> "AND timestamp BETWEEN ? AND ?"
    mkName = GH.mkName (Proxy :: Proxy GH.Repo)
    toViewMapElem (n,t,c,u) = ((mkName n, t), (c,u))
  in
    withConnIO $ \conn ->
       M.fromList . (toViewMapElem <$>) <$> query conn q (GH.untagName repoName, start, end)

findDateRange ::
  NonEmpty (GH.TrafficCount a)
  -> (UTCTime, UTCTime)
findDateRange views =
  let
    sorted = NE.sort . (GH.trafficCountTimestamp <$>) $ views
  in
    (NE.head sorted, NE.last sorted)

insertRepoStats ::
  DbConstraints e r m
  => DbRepoStats
  -> m (Id DbRepoStats)
insertRepoStats =
  let
    tnq = tableNameQ @RepoStats
  in
    insert $
      "INSERT INTO " <> tnq <> " "
      <> "(name, timestamp, stars, forks, views, unique_views, clones, unique_clones, repo_stats_run) "
      <> "VALUES (?,?,?,?,?,?,?,?,?)"

unwrapId ::
  Id (f a)
  -> Id a
unwrapId (Id i) =
  Id i

wrapId ::
  Id a
  -> Id (f a)
wrapId (Id i) =
  Id i

selectRepoStatsQ ::
  Query
selectRepoStatsQ =
  "SELECT id, name, timestamp, stars, forks, views, unique_views, clones, unique_clones, repo_stats_run "

selectRepoStats ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbRepoStats
  -> m (Maybe DbRepoStats)
selectRepoStats i =
  let
    tnq = tableNameQ @RepoStats
    q = selectRepoStatsQ <> "FROM " <> tnq <> " WHERE id = ?"
  in
    selectById q i

selectLatestRepoStats ::
  ( DbConstraints e r m
  , AsError e
  )
  => m [DbRepoStats]
selectLatestRepoStats =
  let
    tnq = tableNameQ @RepoStats
    latestRepoRunIdQ =
      "SELECT id FROM " <> tableNameQ @RepoStatsRun <> "ORDER BY timestamp DESC LIMIT 1"
    q = selectRepoStatsQ <> "FROM " <> tnq <> " WHERE repo_stats_run = (" <> latestRepoRunIdQ <> ")"
  in
    withConnIO $ \conn -> query_ conn q

insertReferrers ::
  ( DbConstraints e r m
  , Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => Id DbRepoStats
  -> t GH.Referrer
  -> m ()
insertReferrers =
  insertPops . toDbReferrer

toDbReferrer ::
  Id DbRepoStats
  -> Position GH.Referrer
  -> GH.Referrer
  -> Pop GH.Referrer
toDbReferrer rsId i GH.Referrer{GH.referrer, GH.referrerCount, GH.referrerUniques} =
  Pop Nothing i referrer (Count referrerCount) (Uniques referrerUniques) rsId

insertPaths ::
  ( DbConstraints e r m
  , Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => Id DbRepoStats
  -> t GH.PopularPath
  -> m ()
insertPaths =
  insertPops . toDbPath

toDbPath ::
  Id DbRepoStats
  -> Position GH.PopularPath
  -> GH.PopularPath
  -> Pop GH.PopularPath
toDbPath rsId i GH.PopularPath{GH.popularPath, GH.popularPathCount, GH.popularPathUniques} =
  Pop Nothing i (GH.mkName (Proxy :: Proxy GH.PopularPath) popularPath) (Count popularPathCount)
      (Uniques popularPathUniques) rsId

toDbPops ::
  ( Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => (Position a -> a -> Pop a)
  -> t a
  -> [Pop a]
toDbPops toDbPop =
  imap (toDbPop . Position . succ) . toList

insertPops ::
  forall a e r m t.
  ( DbConstraints e r m
  , Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  , HasTable a
  )
  => (Position a -> a -> Pop a)
  -> t a
  -> m ()
insertPops toDbPop pops =
  withConnIO $ \conn ->
    executeMany conn (insertPopQ $ tableNameQ @a) . toDbPops toDbPop $ pops

selectPopsForRepoStats ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => Id DbRepoStats
  -> m [Pop a]
selectPopsForRepoStats i =
  withConnIO $ \conn ->
    query conn (selectPopQ @a <> " WHERE repo_id = ? ORDER BY position") (Only i)

selectViews ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id GH.Views
  -> m (Maybe (VC GH.Views))
selectViews =
  selectVC

selectClones ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id GH.Clones
  -> m (Maybe (VC GH.Clones))
selectClones =
  selectVC

insertPopQ ::
  Query
  -> Query
insertPopQ tn =
  "INSERT INTO " <> tn <> " (position, name, count, uniques, repo_id) VALUES (?,?,?,?,?)"

selectPopByIdQ ::
  forall a.
  HasTable a
  => Query
selectPopByIdQ =
  selectPopQ @a <> " WHERE id = ?"

selectPopQ ::
  forall a.
  HasTable a
  => Query
selectPopQ =
  "SELECT id, position, name, count, uniques, repo_id FROM " <> tableNameQ @a

insertPop ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => Pop a
  -> m (Id a)
insertPop =
  fmap unwrapId . insert (insertPopQ $ tableNameQ @a)

selectPop ::
  forall e r m a.
  ( DbConstraints e r m
  , AsError e
  , HasTable a
  )
  => Id a
  -> m (Maybe (Pop a))
selectPop idee =
  selectById (selectPopByIdQ @a) $ wrapId idee

insertVC ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => VC a
  -> m (Id a)
insertVC =
  fmap unwrapId . insert (insertVCQ (Proxy :: Proxy a))

insertVCQ ::
  forall a.
  HasTable a
  => Proxy a
  -> Query
insertVCQ _ =
     "INSERT INTO " <> tableNameQ @a <> " (timestamp, count, uniques, repo_id, repo_name) "
  <> "VALUES (?,?,?,?,?)"

selectVC ::
  forall e r m a.
  ( DbConstraints e r m
  , AsError e
  , HasTable a
  )
  => Id a
  -> m (Maybe (VC a))
selectVC idee =
  let
    q =  selectVCFieldsQ
      <> "FROM " <> tableNameQ @a <> " "
      <> "WHERE id = ?"
  in
    selectById q $ wrapId idee

selectVCFieldsQ ::
  Query
selectVCFieldsQ =
  "SELECT id, timestamp, count, uniques, repo_id, repo_name "

selectById ::
  forall e r m a.
  ( DbConstraints e r m
  , HasTable a
  , FromRow a
  , AsError e
  )
  => Query
  -> Id a
  -> m (Maybe a)
selectById q idee =
  withConn $ \conn -> do
    rs <- runDb $ query conn q (Only idee)
    case rs of
      [r] -> pure $ Just r
      (_:_:_) -> throwing _TooManyResults $
        "Found multiple records in `" <> tableName @a <> "` with id " <> (T.pack . show $ idee)
      _ -> pure Nothing

toDbRepoStats ::
  Id RepoStatsRun
  -> RepoStats
  -> DbRepoStats
toDbRepoStats rsrId RepoStats{ _repoStatsName, _repoStatsTimestamp, _repoStatsStars, _repoStatsForks
                       , _repoStatsViews, _repoStatsClones} =
  DbRepoStats
    Nothing
    _repoStatsName
    _repoStatsTimestamp
    _repoStatsStars
    _repoStatsForks
    (Count . GH.viewsCount $ _repoStatsViews)
    (Uniques . GH.viewsUniques $ _repoStatsViews)
    (Count . GH.clonesCount $ _repoStatsClones)
    (Uniques . GH.clonesUniques $ _repoStatsClones)
    rsrId

insert ::
  ( DbConstraints e r m
  , ToRow a
  )
  => Query
  -> a
  -> m (Id a)
insert q a =
  withConnIO $ \conn -> do
    execute conn q a
    fmap Id $ lastInsertRowId conn

withConn ::
  DbConstraints e r m
  => (Connection -> m a)
  -> m a
withConn f =
  (f . view connection) =<< ask

withConnIO ::
  DbConstraints e r m
  => (Connection -> IO a)
  -> m a
withConnIO f =
  (runDb . f . view connection) =<< ask

runDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => IO a
  -> m a
runDb =
  either (throwing _SQLiteResponse) pure <=< liftIO . runDBAction
