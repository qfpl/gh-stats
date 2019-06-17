{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module GhStats.Db where

import           Control.Lens                     (view, (^.))
import           Control.Lens.Indexed             (FunctorWithIndex,
                                                   TraversableWithIndex, imap)
import           Control.Monad                    (void, (<=<))
import           Control.Monad.Error.Lens         (throwing)
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, ask)
import           Data.Foldable                    (Foldable, toList)
import           Data.Int                         (Int64)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time.Clock                  (UTCTime)
import           Database.SQLite.Simple           (Connection, Only (Only),
                                                   Query (Query), ToRow (toRow),
                                                   execute, executeMany,
                                                   execute_, field,
                                                   lastInsertRowId, query)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow   (FromRow (fromRow))
import           Database.SQLite.Simple.ToField   (ToField)
import           Database.SQLite.SimpleErrors     (runDBAction)
import           GitHub                           (Name, Repo, mkName,
                                                   untagName)
import           GitHub.Data.Traffic              (PopularPath, Referrer (Referrer, referrer, referrerCount, referrerUniques))

import           GhStats.Types                    (AsError (_TooManyResults), AsSQLiteResponse (_SQLiteResponse),
                                                   Forks,
                                                   HasConnection (connection),
                                                   RepoStats (..), Stars,
                                                   repoStatsPopularReferrers)

newtype Id a = Id Int64
  deriving (Eq, Show, FromField, ToField)

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
        "CREATE TABLE IF NOT EXISTS ", tableNameQ @DbRepoStats, " "
      , "( id INTEGER PRIMARY KEY"
      , ", name TEXT NOT NULL"
      , ", timestamp TEXT NOT NULL"
      , ", stars INTEGER NOT NULL"
      , ", forks INTEGER NOT NULL"
      , ")"
      ]
    qReferrers = mconcat [
        "CREATE TABLE IF NOT EXISTS ", tableNameQ @DbReferrer, " "
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
  in
    withConn $ \conn -> liftIO $ do
      execute_ conn qRepos
      execute_ conn qReferrers


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
insertRepoStats drs =
  let
    q = "INSERT INTO repos (name, timestamp, stars, forks) VALUES (?,?,?,?)"
  in
    withConn $ \conn -> do
      runDb $ execute conn q drs
      runDb . fmap Id $ lastInsertRowId conn

selectRepoStats ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbRepoStats
  -> m (Maybe DbRepoStats)
selectRepoStats idee =
  let
    tnq = tableNameQ @DbRepoStats
    q = "SELECT id, name, timestamp, stars, forks FROM " <> tnq <> " WHERE id = ?"
  in
    selectById q idee

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
  let
    toDbReferrer i Referrer{referrer, referrerCount, referrerUniques} =
      DbReferrer (Just (Id 0)) i referrer referrerCount referrerUniques rsId
  in
    withConn $ \conn ->
      liftIO . executeMany conn insertReferrerQ . imap toDbReferrer . toList $ refs

insertReferrerQ ::
  Query
insertReferrerQ =
  "INSERT INTO referrers (position, name, count, uniques, repo_id) VALUES (?,?,?,?,?)"

insertReferrer ::
  DbConstraints e r m
  => DbReferrer
  -> m (Id DbReferrer)
insertReferrer dbRef =
  withConn $ \conn -> do
    runDb $ execute conn insertReferrerQ dbRef
    runDb . fmap Id $ lastInsertRowId conn

selectReferrer ::
  ( DbConstraints e r m
  , AsError e
  )
  => Id DbReferrer
  -> m (Maybe DbReferrer)
selectReferrer idee =
  let
    tnq = tableNameQ @DbReferrer
    q = "SELECT id, position, name, count, uniques, repo_id FROM " <> tnq <> " WHERE id = ?"
  in
    selectById q idee

class HasTable r where
  tableName :: Text
  tableNameQ :: Query
  tableNameQ = Query $ tableName @r

instance HasTable DbRepoStats where
  tableName = "repos"

instance HasTable DbReferrer where
  tableName = "referrers"

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

data DbRepoStats =
  DbRepoStats {
    _dbRepoStatsId        :: !(Maybe (Id DbRepoStats))
  , _dbRepoStatsName      :: !(Name Repo)
  , _dbRepoStatsTimestamp :: !UTCTime
  , _dbRepoStatsStars     :: !Stars
  , _dbRepoStatsForks     :: !Forks
  }
  deriving (Eq, Show)

instance ToRow DbRepoStats where
  toRow DbRepoStats {..} =
    toRow (untagName _dbRepoStatsName, _dbRepoStatsTimestamp, _dbRepoStatsStars, _dbRepoStatsForks)

instance FromRow DbRepoStats where
  fromRow =
    let
      nameField = mkName (Proxy :: Proxy Repo) <$> field
    in
      DbRepoStats <$> field <*> nameField <*> field <*> field <*> field

toDbRepoStats ::
  RepoStats
  -> DbRepoStats
toDbRepoStats RepoStats{_repoStatsName, _repoStatsTimestamp, _repoStatsStars, _repoStatsForks} =
  DbRepoStats Nothing _repoStatsName _repoStatsTimestamp _repoStatsStars _repoStatsForks

data DbReferrer =
  DbReferrer {
    dbRefId       :: !(Maybe (Id DbReferrer))
  , dbRefPosition :: !Int
  , dbRefName     :: !(Name Referrer)
  , dbRefCount    :: !Int
  , dbRefUniques  :: !Int
  , dbRefRepoId   :: !(Id DbRepoStats)
  }
  deriving (Eq, Show)

instance ToRow DbReferrer where
  toRow DbReferrer{..} =
    toRow (dbRefPosition, untagName dbRefName, dbRefCount, dbRefUniques, dbRefRepoId)

instance FromRow DbReferrer where
  fromRow =
    let
      nameField = mkName (Proxy :: Proxy Referrer) <$> field
    in
      DbReferrer <$> field <*> field <*> nameField <*> field <*> field <*> field

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
