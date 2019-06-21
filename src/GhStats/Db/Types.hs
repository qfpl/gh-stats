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
import           GhStats.Types                    (Forks, RepoStats, Stars)
import qualified GitHub                           as GH

newtype Id a = Id Int64
  deriving (Eq, Show, FromField, ToField)

newtype Position a = Position Int
  deriving (Eq, Show, FromField, ToField)

newtype Count a = Count Int
  deriving (Eq, Show, FromField, ToField)

newtype Uniques a = Uniques Int
  deriving (Eq, Show, FromField, ToField)

newtype Views = Views Int
  deriving (Eq, Show, FromField, ToField)

newtype Clones = Clones Int
  deriving (Eq, Show, FromField, ToField)

class HasTable r where
  tableName :: Text
  tableNameQ :: Query
  tableNameQ = Query $ tableName @r

instance HasTable DbRepoStats where
  tableName = "repos"

instance HasTable RepoStats where
  tableName = "repos"

instance HasTable GH.Referrer where
  tableName = "referrers"

data DbRepoStats =
  DbRepoStats {
    _dbRepoStatsId           :: !(Maybe (Id DbRepoStats))
  , _dbRepoStatsName         :: !(GH.Name GH.Repo)
  , _dbRepoStatsTimestamp    :: !UTCTime
  , _dbRepoStatsStars        :: !Stars
  , _dbRepoStatsForks        :: !Forks
  , _dbRepoStatsViews        :: !(Count Views)
  , _dbRepoStatsUniqueViews  :: !(Uniques Views)
  , _dbRepoStatsClones       :: !(Count Clones)
  , _dbRepoStatsUniqueClones :: !(Uniques Clones)
  }
  deriving (Eq, Show)

instance ToRow DbRepoStats where
  toRow DbRepoStats {..} =
    toRow (GH.untagName _dbRepoStatsName, _dbRepoStatsTimestamp, _dbRepoStatsStars, _dbRepoStatsForks,
           _dbRepoStatsViews, _dbRepoStatsUniqueViews, _dbRepoStatsClones, _dbRepoStatsUniqueClones)

instance FromRow DbRepoStats where
  fromRow =
    let
      nameField = GH.mkName (Proxy :: Proxy GH.Repo) <$> field
    in
      DbRepoStats <$> field <*> nameField <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data Pop a =
  Pop {
    popId       :: !(Maybe (Id a))
  , popPosition :: !(Position a)
  , popName     :: !(GH.Name a)
  , popCount    :: !(Count a)
  , popUniques  :: !(Uniques a)
  , popRepoId   :: !(Id DbRepoStats)
  }
  deriving (Eq, Show)

instance HasTable a => HasTable (Pop a) where
  tableName = tableName @a

instance ToRow (Pop a) where
  toRow Pop{..} =
    toRow (popPosition, GH.untagName popName, popCount, popUniques, popRepoId)

instance FromRow (Pop a) where
  fromRow =
    let
      nameField = GH.mkName (Proxy :: Proxy a) <$> field
    in
      Pop <$> field <*> field <*> nameField <*> field <*> field <*> field

instance HasTable GH.PopularPath where
  tableName = "paths"

data DbView =
  DbView
  { _dbViewId        :: !(Maybe (Id DbView))
  , _dbViewTimestamp :: !UTCTime
  , _dbViewCount     :: !(Count DbView)
  , _dbViewUniques   :: !(Uniques DbView)
  , _dbViewRepoId    :: !(Id DbRepoStats)
  }
  deriving (Eq, Show)

instance HasTable DbView where
  tableName = "views"

instance ToRow DbView where
  toRow DbView{..} =
    toRow (_dbViewTimestamp, _dbViewCount, _dbViewUniques, _dbViewRepoId)

instance FromRow DbView where
  fromRow =
    DbView <$> field <*> field <*> field <*> field <*> field

data DbClone =
  DbClone
  { _dbCloneId        :: !(Maybe (Id DbClone))
  , _dbCloneTimestamp :: !UTCTime
  , _dbCloneCount     :: !(Count DbClone)
  , _dbCloneUniques   :: !(Uniques DbClone)
  , _dbCloneRepoId    :: !(Id DbRepoStats)
  }
  deriving (Eq, Show)

instance HasTable DbClone where
  tableName = "clones"

instance ToRow DbClone where
  toRow DbClone{..} =
    toRow (_dbCloneTimestamp, _dbCloneCount, _dbCloneUniques, _dbCloneRepoId)

instance FromRow DbClone where
  fromRow =
    DbClone <$> field <*> field <*> field <*> field <*> field