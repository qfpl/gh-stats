{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module GhStats.Db where

import           Control.Lens                 (view, (^.))
import           Control.Lens.Indexed         (FunctorWithIndex,
                                               TraversableWithIndex, imap)
import           Control.Monad                (join, void, (<=<))
import           Control.Monad.Error.Lens     (throwing)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Reader         (MonadReader, reader)
-- import           Data.Vector            (Vector)
import           Data.Foldable                (Foldable, toList)
import           Data.Time.Clock              (UTCTime)
import           Database.SQLite.Simple       (Connection, ToRow (toRow),
                                               execute, executeMany, execute_)
import           Database.SQLite.Simple.QQ    (sql)
import           Database.SQLite.SimpleErrors (runDBAction)
import           GitHub                       (Name, untagName)
import           GitHub.Data.Traffic          (PopularPath, Referrer (Referrer, referrer, referrerCount, referrerUniques))

import           GhStats.Types                (AsSQLiteResponse (_SQLiteResponse),
                                               HasConnection (connection),
                                               RepoStats,
                                               repoStatsPopularReferrers,
                                               repoStatsTimestamp)

initDb ::
  ( MonadReader r m
  , HasConnection r
  , MonadIO m
  , AsSQLiteResponse e
  , MonadError e m
  )
  => m ()
initDb = do
  let
    q = [sql| CREATE TABLE IF NOT EXISTS repos
              ( id INTEGER PRIMARY KEY
              , name TEXT
              , timestamp TEXT
              , stars INTEGER
              , forks INTEGER
              )
            |]
  withConn $ \conn -> execute_ conn q

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
  -> m ()
insertRepoStats repoStats = do
  let
    q = "INSERT INTO repos (name, timestamp, stars, forks) VALUES (?,?,?,?)"
    t = repoStats ^. repoStatsTimestamp
  insertReferrers t $ repoStats ^. repoStatsPopularReferrers
  withConn $ \conn ->
    execute conn q repoStats


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
  => UTCTime
  -> t Referrer
  -> m ()
insertReferrers t refs = do
  let
    q =  "INSERT INTO referrers (timestamp, position, name, count, uniques) VALUES (?,?,?,?,?)"
    toDbReferrer i Referrer{referrer, referrerCount, referrerUniques} =
      DbReferrer t i referrer referrerCount referrerUniques
  conn <- reader (view connection)
  liftIO . executeMany conn q . imap toDbReferrer . toList $ refs

data DbReferrer =
  DbReferrer {
    dbRefTimestamp :: !UTCTime
  , dbRefPosition  :: !Int
  , dbRefName      :: !(Name Referrer)
  , dbRefCount     :: !Int
  , dbRefUniques   :: !Int
  }

instance ToRow DbReferrer where
  toRow DbReferrer{..} =
    toRow (dbRefTimestamp, dbRefPosition, untagName dbRefName, dbRefCount, dbRefUniques)

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
  let
    handleErrors =
      either (throwing _SQLiteResponse) pure
  in
    join $ reader (handleErrors <=< liftIO . runDBAction . f . view connection)
