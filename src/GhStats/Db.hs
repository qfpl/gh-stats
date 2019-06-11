{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GhStats.Db where

import           Control.Lens                   (view)
import           Control.Lens.Indexed           (FunctorWithIndex,
                                                 TraversableWithIndex, imap)
import           Control.Monad                  (void)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader           (MonadReader, reader)
-- import           Data.Vector            (Vector)
import           Data.Foldable                  (Foldable, toList)
import           Data.Time.Clock                (UTCTime)
import           Database.SQLite.Simple         (Connection,
                                                 SQLData (SQLInteger),
                                                 ToRow (toRow), executeMany,
                                                 open)
import           Database.SQLite.Simple.ToField (toField)
import           GitHub                         (Name, untagName)
import           GitHub.Data.Traffic            (PopularPath, Referrer (Referrer, referrer, referrerCount, referrerUniques))

import           GhStats.Types                  (AsSQLiteResponse,
                                                 HasConnection (connection),
                                                 RepoStats)

addToDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadReader r m
  , Traversable t
  , HasConnection r
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
  )
  => RepoStats
  -> m ()
insertRepoStats repoStats = do
  let
    q = "INSERT INTO repos (name, timestamp, stars, forks) VALUES (?,?,?,?)"
  conn <- reader (view connection)

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
    toDbReferrer t i Referrer{referrer, referrerCount, referrerUniques} =
      DbReferrer t i referrer referrerCount referrerUniques
  conn <- reader (view connection)
  liftIO . executeMany conn q . imap (toDbReferrer t) . toList $ refs

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
