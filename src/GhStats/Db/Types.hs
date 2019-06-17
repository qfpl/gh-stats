{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module GhStats.Db.Types where

import           Data.Int                         (Int64)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)
import           Database.SQLite.Simple           (Query (Query), ToRow (toRow),
                                                   field)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow   (FromRow (fromRow))
import           Database.SQLite.Simple.ToField   (ToField)
import           GitHub                           (Name, PopularPath, Referrer,
                                                   Repo, mkName, untagName)

import           GhStats.Types                    (Forks, Stars, RepoStats)

newtype Id a = Id Int64
  deriving (Eq, Show, FromField, ToField)

newtype Position a = Position Int
  deriving (Eq, Show, FromField, ToField)

newtype Count a = Count Int
  deriving (Eq, Show, FromField, ToField)

newtype Uniques a = Uniques Int
  deriving (Eq, Show, FromField, ToField)

class HasTable r where
  tableName :: Text
  tableNameQ :: Query
  tableNameQ = Query $ tableName @r

instance HasTable DbRepoStats where
  tableName = "repos"

instance HasTable RepoStats where
  tableName = "repos"

instance HasTable Referrer where
  tableName = "referrers"

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

data Pop a =
  Pop {
    popId       :: !(Maybe (Id a))
  , popPosition :: !(Position a)
  , popName     :: !(Name a)
  , popCount    :: !Int
  , popUniques  :: !Int
  , popRepoId   :: !(Id DbRepoStats)
  }
  deriving (Eq, Show)

instance HasTable a => HasTable (Pop a) where
  tableName = tableName @a

instance ToRow (Pop a) where
  toRow Pop{..} =
    toRow (popPosition, untagName popName, popCount, popUniques, popRepoId)

instance FromRow (Pop a) where
  fromRow =
    let
      nameField = mkName (Proxy :: Proxy a) <$> field
    in
      Pop <$> field <*> field <*> nameField <*> field <*> field <*> field

-- TODO: Can probably have `data Popular a` and use `Popular Referrer` and `Popular PopularPath`
-- data DbPath =
--   DbPath {
--     dbPathId       :: !(Maybe (Id DbPath))
--   , dbPathPosition :: !(Position PopularPath)
--   , dbPathName     :: !(Name PopularPath)
--   , dbPathCount    :: !(Count PopularPath)
--   , dbPathUniques  :: !(Uniques PopularPath)
--   , dbPathRepoId   :: !(Id DbRepoStats)
--   }
--   deriving (Eq, Show)

instance HasTable PopularPath where
  tableName = "paths"
