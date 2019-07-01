{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}

module GhStats.Db.Types where

import           Control.Lens                     (makeLenses)
import           Data.Int                         (Int64)
import           Data.Proxy                       (Proxy (Proxy))
import           Data.Text                        (Text)
import           Data.Time.Clock                  (UTCTime)
import           Database.SQLite.Simple           (Query (Query), ToRow (toRow),
                                                   field)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.FromRow   (FromRow (fromRow), RowParser)
import           Database.SQLite.Simple.ToField   (ToField)
import           GhStats.Types                    (Clones, Count, Forks,
                                                   RepoStats, Stars, Uniques,
                                                   Views)
import qualified GitHub                           as GH

newtype Id a = Id Int64
  deriving (Eq, Show, FromField, ToField)

newtype Position a = Position Int
  deriving (Eq, Show, FromField, ToField)

class HasTable r where
  tableName :: Text
  tableName = tableNameProxy (Proxy :: Proxy r)

  tableNameProxy :: Proxy r -> Text

  tableNameQ :: Query
  tableNameQ = Query $ tableNameProxy (Proxy :: Proxy r)

instance HasTable DbRepoStats where
  tableNameProxy _ = tableNameProxy (Proxy :: Proxy RepoStats)

instance HasTable RepoStats where
  tableNameProxy _ = "repo_stats"

instance HasTable GH.Referrer where
  tableNameProxy _ = "referrers"

data RepoStatsRun

instance HasTable RepoStatsRun where
  tableNameProxy _ = "repo_stats_runs"

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
  , _dbRepoStatsRunId        :: !(Id RepoStatsRun)
  }
  deriving (Eq, Show)

instance ToRow DbRepoStats where
  toRow DbRepoStats {..} =
    toRow (GH.untagName _dbRepoStatsName, _dbRepoStatsTimestamp, _dbRepoStatsStars, _dbRepoStatsForks,
           _dbRepoStatsViews, _dbRepoStatsUniqueViews, _dbRepoStatsClones, _dbRepoStatsUniqueClones,
           _dbRepoStatsRunId)

instance FromRow DbRepoStats where
  fromRow =
    DbRepoStats <$> field <*> nameField <*> field <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field

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
  tableNameProxy (Proxy :: Proxy (Pop a)) = tableNameProxy (Proxy :: Proxy a)

instance ToRow (Pop a) where
  toRow Pop{..} =
    toRow (popPosition, GH.untagName popName, popCount, popUniques, popRepoId)

instance FromRow (Pop a) where
  fromRow =
    Pop <$> field <*> field <*> nameField <*> field <*> field <*> field

instance HasTable GH.PopularPath where
  tableNameProxy _ = "paths"

data VC a =
  VC
  { _vcId        :: !(Maybe (Id a))
  , _vcTimestamp :: !UTCTime
  , _vcCount     :: !(Count a)
  , _vcUniques   :: !(Uniques a)
  , _vcRepoId    :: !(Id DbRepoStats)
  , _vcRepoName  :: !(GH.Name GH.Repo)
  }
  deriving (Eq, Show)

instance HasTable a => HasTable (VC a) where
  tableNameProxy (Proxy :: Proxy (VC a)) = tableNameProxy (Proxy :: Proxy a)

instance HasTable GH.Views where
  tableNameProxy _ = "views"

instance HasTable GH.Clones where
  tableNameProxy _ = "clones"

instance ToRow (VC a) where
  toRow VC{..} =
    toRow (_vcTimestamp, _vcCount, _vcUniques, _vcRepoId, GH.untagName _vcRepoName)

instance FromRow (VC a) where
  fromRow =
    VC <$> field <*> field <*> field <*> field <*> field <*> nameField

nameField ::
  RowParser (GH.Name a)
nameField = GH.mkName (Proxy :: Proxy a) <$> field

makeLenses ''VC
makeLenses ''DbRepoStats
