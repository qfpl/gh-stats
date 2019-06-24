{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module GhStats.Db where

import           Control.Lens                 (view, (^.))
import           Control.Lens.Indexed         (FunctorWithIndex,
                                               TraversableWithIndex, imap)
import           Control.Monad                (void, (<=<))
import           Control.Monad.Error.Lens     (throwing)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, ask)
import           Data.Foldable                (Foldable, toList)
import           Data.Proxy                   (Proxy (Proxy))
import qualified Data.Text                    as T
import           Database.SQLite.Simple       (Connection, FromRow, Only (Only),
                                               Query, ToRow, execute,
                                               executeMany, execute_,
                                               lastInsertRowId, query)
import           Database.SQLite.SimpleErrors (runDBAction)
import qualified GitHub                       as GH

import           GhStats.Db.Types
import           GhStats.Types                (AsError (_TooManyResults), AsSQLiteResponse (_SQLiteResponse),
                                               HasConnection (connection),
                                               RepoStats (..),
                                               repoStatsPopularPaths,
                                               repoStatsPopularReferrers)

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
    qRepos = mconcat [
        "CREATE TABLE IF NOT EXISTS ", tableNameQ @RepoStats, " "
      , "( id INTEGER PRIMARY KEY"
      , ", name TEXT NOT NULL"
      , ", timestamp TEXT NOT NULL"
      , ", stars INTEGER NOT NULL"
      , ", forks INTEGER NOT NULL"
      , ", views INTEGER NOT NULL"  -- views over the last 14 days
      , ", unique_views INTEGER NOT NULL" -- unique views over the last 14 days
      , ", clones INTEGER NOT NULL" -- clones over the last 14 days
      , ", unique_clones INTEGER NOT NULL" -- unique_clones over the last 14 days
      , ")"
      ]
    qPop tn = mconcat [
        "CREATE TABLE IF NOT EXISTS ", tn, " "
      , "( id INTEGER PRIMARY KEY"
      , ", position INTEGER NOT NULL"
      , ", name TEXT NOT NULL"
      , ", count INTEGER NOT NULL"
      , ", uniques INTEGER NOT NULL"
      , ", repo_id INTEGER NOT NULL"
      , ", FOREIGN KEY(repo_id) REFERENCES " <> tableNameQ @RepoStats <> "(id)"
      , ", UNIQUE (position, repo_id)"
      , ", UNIQUE (name, repo_id)"
      , ")"
      ]
    qVC tn = mconcat [
        "CREATE TABLE IF NOT EXISTS ", tn, " "
      , "( id INTEGER PRIMARY KEY"
      , ", timestamp TEXT NOT NULL"
      , ", count INTEGER NOT NULL"
      , ", uniques INTEGER NTO NULL"
      , ", repo_id INTEGER NOT NULL"
      , ", FOREIGN KEY(repo_id) REFERENCES " <> tableNameQ @RepoStats <> "(id)"
      , ")"
      ]
    qReferrer = qPop $ tableNameQ @GH.Referrer
    qPaths = qPop $ tableNameQ @GH.PopularPath
    qViews = qVC $ tableNameQ @GH.Views
    qClones = qVC $ tableNameQ @GH.Clones
  in
    withConn $ \conn -> liftIO . void $
      traverse (execute_ conn) [enableForeignKeys, qRepos, qReferrer, qPaths, qViews, qClones]

addToDb ::
  ( DbConstraints e r m
  , Traversable t
  )
  => t RepoStats
  -> m ()
addToDb =
  void . traverse insertRepoStatsTree

insertRepoStatsTree ::
  DbConstraints e r m
  => RepoStats
  -> m (Id DbRepoStats)
insertRepoStatsTree rs = do
  rsId <- insertRepoStats $ toDbRepoStats rs
  insertReferrers rsId $ rs ^. repoStatsPopularReferrers
  insertPaths rsId $ rs ^. repoStatsPopularPaths
  pure rsId

insertRepoStats ::
  DbConstraints e r m
  => DbRepoStats
  -> m (Id DbRepoStats)
insertRepoStats =
  let
    tnq = tableNameQ @RepoStats
  in
    insert $
      "INSERT INTO " <> tnq <> " (name, timestamp, stars, forks, views, unique_views, clones, unique_clones) VALUES (?,?,?,?,?,?,?,?)"

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

selectRepoStats ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbRepoStats
  -> m (Maybe DbRepoStats)
selectRepoStats i =
  let
    tnq = tableNameQ @RepoStats
    q = "SELECT id, name, timestamp, stars, forks, views, unique_views, clones, unique_clones "
      <> "FROM " <> tnq <> " WHERE id = ?"
  in
    selectById q i

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
  withConn $ \conn ->
    liftIO . executeMany conn (insertPopQ $ tableNameQ @a) . toDbPops toDbPop $ pops

selectPopsForRepoStats ::
  forall a e r m.
  ( DbConstraints e r m
  , HasTable a
  )
  => Id DbRepoStats
  -> m [Pop a]
selectPopsForRepoStats i =
  withConn $ \conn ->
    runDb $ query conn (selectPopQ @a <> " WHERE repo_id = ? ORDER BY position") (Only i)

insertViews ::
  DbConstraints e r m
  => VC GH.Views
  -> m (Id GH.Views)
insertViews =
  insertVC

selectViews ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id GH.Views
  -> m (Maybe (VC GH.Views))
selectViews =
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
  let
    q =  "INSERT INTO " <> tableNameQ @a <> " (timestamp, count, uniques, repo_id) "
      <> "VALUES (?,?,?,?)"
  in
    fmap unwrapId . insert q

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
    q =  "SELECT id, timestamp, count, uniques, repo_id "
      <> "FROM " <> tableNameQ @a <> " "
      <> "WHERE id = ?"
  in
    selectById q $ wrapId idee

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
  RepoStats
  -> DbRepoStats
toDbRepoStats RepoStats{ _repoStatsName, _repoStatsTimestamp, _repoStatsStars, _repoStatsForks
                       , _repoStatsViews, _repoStatsClones} =
  DbRepoStats Nothing _repoStatsName _repoStatsTimestamp _repoStatsStars _repoStatsForks
              (Count . GH.viewsCount $ _repoStatsViews) (Uniques . GH.viewsUniques $ _repoStatsViews)
              (Count . GH.clonesCount $ _repoStatsClones) (Uniques . GH.clonesUniques $ _repoStatsClones)

insert ::
  ( DbConstraints e r m
  , ToRow a
  )
  => Query
  -> a
  -> m (Id a)
insert q a =
  withConn $ \conn -> do
    runDb $ execute conn q a
    runDb . fmap Id $ lastInsertRowId conn

withConn ::
  DbConstraints e r m
  => (Connection -> m a)
  -> m a
withConn f =
  (f . view connection) =<< ask

runDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => IO a
  -> m a
runDb =
  either (throwing _SQLiteResponse) pure <=< liftIO . runDBAction
