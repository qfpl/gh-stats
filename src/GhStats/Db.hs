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
import qualified Data.Text                    as T
import           Database.SQLite.Simple       (Connection, FromRow, Only (Only),
                                               Query, ToRow, execute,
                                               executeMany, execute_,
                                               lastInsertRowId, query)
import           Database.SQLite.SimpleErrors (runDBAction)
import           GitHub.Data.Traffic          (PopularPath, Referrer (Referrer, referrer, referrerCount, referrerUniques))

import           GhStats.Db.Types
import           GhStats.Types                (AsError (_TooManyResults), AsSQLiteResponse (_SQLiteResponse),
                                               HasConnection (connection),
                                               RepoStats (..),
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
    qRepos = mconcat [
        "CREATE TABLE IF NOT EXISTS ", tableNameQ @RepoStats, " "
      , "( id INTEGER PRIMARY KEY"
      , ", name TEXT NOT NULL"
      , ", timestamp TEXT NOT NULL"
      , ", stars INTEGER NOT NULL"
      , ", forks INTEGER NOT NULL"
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
      , ", FOREIGN KEY(repo_id) REFERENCES repo(id)"
      , ", UNIQUE (position, repo_id)"
      , ", UNIQUE (name, repo_id)"
      , ")"
      ]
    qReferrer = qPop $ tableNameQ @Referrer
    qPaths = qPop $ tableNameQ @PopularPath
  in
    withConn $ \conn -> liftIO $ do
      execute_ conn qRepos
      execute_ conn qReferrer
      execute_ conn qPaths


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
  -- insertPaths rsId $ repoStats ^. repoStatsPopularPaths
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
      "INSERT INTO " <> tnq <> " (name, timestamp, stars, forks) VALUES (?,?,?,?)"

fromPopId ::
  Id (Pop a)
  -> Id a
fromPopId (Id i) =
  Id i

toPopId ::
  Id a
  -> Id (Pop a)
toPopId (Id i) =
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
    q = "SELECT id, name, timestamp, stars, forks FROM " <> tnq <> " WHERE id = ?"
  in
    selectById q i

insertReferrers ::
  ( DbConstraints e r m
  , Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => Id DbRepoStats
  -> t Referrer
  -> m ()
insertReferrers rsId refs =
  withConn $ \conn ->
    liftIO . executeMany conn (insertPopQ $ tableNameQ @Referrer) . toDbReferrers rsId $ refs

toDbReferrers ::
  ( Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => Id DbRepoStats
  -> t Referrer
  -> [Pop Referrer]
toDbReferrers rsId =
  let
    toDbReferrer i Referrer{referrer, referrerCount, referrerUniques} =
      Pop Nothing i referrer (Count referrerCount) (Uniques referrerUniques) rsId
  in
    imap (toDbReferrer . Position) . toList

selectReferrersForRepoStats ::
  DbConstraints e r m
  => Id DbRepoStats
  -> m [Pop Referrer]
selectReferrersForRepoStats i =
  withConn $ \conn ->
    runDb $ query conn (selectPopQ @Referrer <> " WHERE repo_id = ? ORDER BY position") (Only i)

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
  fmap fromPopId . insert (insertPopQ $ tableNameQ @a)

selectPop ::
  forall e r m a.
  ( DbConstraints e r m
  , AsError e
  , HasTable a
  )
  => Id a
  -> m (Maybe (Pop a))
selectPop idee =
  selectById (selectPopByIdQ @a) $ toPopId idee

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
toDbRepoStats RepoStats{_repoStatsName, _repoStatsTimestamp, _repoStatsStars, _repoStatsForks} =
  DbRepoStats Nothing _repoStatsName _repoStatsTimestamp _repoStatsStars _repoStatsForks

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
