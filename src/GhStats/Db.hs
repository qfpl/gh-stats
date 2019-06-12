{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module GhStats.Db where

import           Control.Lens                     (view, (^.))
import           Control.Lens.Indexed             (FunctorWithIndex,
                                                   TraversableWithIndex, imap)
import           Control.Monad                    (join, void, (<=<))
import           Control.Monad.Error.Lens         (throwing)
import           Control.Monad.Except             (MonadError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Reader             (MonadReader, reader)
import           Data.Foldable                    (Foldable, toList)
import           Data.Int                         (Int64)
import           Data.String                      (fromString)
import           Data.Time.Clock                  (UTCTime)
import           Database.SQLite.Simple           (Connection, ToRow (toRow),
                                                   execute, executeMany,
                                                   execute_, lastInsertRowId)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           Database.SQLite.SimpleErrors     (runDBAction)
import           GitHub                           (Name, Repo, untagName)
import           GitHub.Data.Traffic              (PopularPath, Referrer (Referrer, referrer, referrerCount, referrerUniques))

import           GhStats.Types                    (AsSQLiteResponse (_SQLiteResponse),
                                                   Forks,
                                                   HasConnection (connection),
                                                   RepoStats, Stars,
                                                   repoStatsPopularPaths,
                                                   repoStatsPopularReferrers,
                                                   repoStatsTimestamp)

newtype Id a = Id Int64
  deriving (Eq, Show, FromField, ToField)

initDb ::
  ( MonadReader r m
  , HasConnection r
  , MonadIO m
  , AsSQLiteResponse e
  , MonadError e m
  )
  => m ()
initDb =
  let
    qRepos = fromString $ concat [
        "CREATE TABLE IF NOT EXISTS repos"
      , "( id INTEGER PRIMARY KEY"
      , ", name TEXT"
      , ", timestamp TEXT"
      , ", stars INTEGER"
      , ", forks INTEGER"
      , ")"
      ]
    qReferrers = fromString $ concat [
        "CREATE TABLE IF NOT EXISTS referrers"
      , "( id INTEGER PRIMARY KEY"
      , ", position INTEGER"
      , ", name TEXT"
      , ", count INTEGER"
      , ", uniques INTEGER"
      , ", FOREIGN KEY(repo_id) REFERENCES repo(id)"
      , ")"
      ]
  in
    withConn $ \conn -> do
      execute_ conn qRepos
      execute_ conn qReferrers


addToDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadReader r m
  , Traversable t
  , HasConnection r
  , MonadIO m
  )
  => t RepoStats
  -> m ()
addToDb =
  void . traverse insertRepoStats

insertRepoStats ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadReader r m
  , HasConnection r
  , MonadIO m
  )
  => RepoStats
  -> m (Id DbRepoStats)
insertRepoStats repoStats =
  let
    q = "INSERT INTO repos (name, timestamp, stars, forks) VALUES (?,?,?,?)"
  in
    withConnM $ \conn -> do
      runDb $ execute conn q repoStats
      rsId <- runDb . fmap Id $ lastInsertRowId conn
      insertReferrers rsId $ repoStats ^. repoStatsPopularReferrers
      -- insertPaths rsId $ repoStats ^. repoStatsPopularPaths
      pure rsId


insertReferrers ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadReader r m
  , HasConnection r
  , MonadIO m
  , Foldable t
  , FunctorWithIndex Int t
  , TraversableWithIndex Int t
  )
  => Id DbRepoStats
  -> t Referrer
  -> m ()
insertReferrers rsId refs = do
  let
    q =  "INSERT INTO referrers (position, name, count, uniques, repo_id) VALUES (?,?,?,?,?)"
    toDbReferrer i Referrer{referrer, referrerCount, referrerUniques} =
      DbReferrer (Id 0) i referrer referrerCount referrerUniques rsId
  conn <- reader (view connection)
  liftIO . executeMany conn q . imap toDbReferrer . toList $ refs

data DbRepoStats =
  DbRepoStats {
    dbRepoStatsId        :: !(Id DbReferrer)
  , dbRepoStatsName      :: !(Name Repo)
  , dbRepoStatsTimestamp :: !UTCTime
  , dbRepoStatsStars     :: !Stars
  , dbRepoStatsForks     :: !Forks
  }

data DbReferrer =
  DbReferrer {
    dbRefId       :: !(Id DbReferrer)
  , dbRefPosition :: !Int
  , dbRefName     :: !(Name Referrer)
  , dbRefCount    :: !Int
  , dbRefUniques  :: !Int
  , dbRefRepoId   :: !(Id DbRepoStats)
  }

instance ToRow DbReferrer where
  toRow DbReferrer{..} =
    toRow (dbRefPosition, untagName dbRefName, dbRefCount, dbRefUniques, dbRefRepoId)

withConn ::
  ( MonadReader r m
  , HasConnection r
  , MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => (Connection -> IO a)
  -> m a
withConn f =
    join $ reader (runDb . f . view connection)

withConnM ::
  ( MonadReader r m
  , HasConnection r
  , MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => (Connection -> m a)
  -> m a
withConnM f =
    join $ reader (f . view connection)

runDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => IO a
  -> m a
runDb =
  either (throwing _SQLiteResponse) pure <=< liftIO . runDBAction
