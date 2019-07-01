{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module GhStats.Web where

import Control.Lens           (makeClassyPrisms)
import Control.Monad.Except   (MonadError)
import Control.Monad.Reader   (MonadReader)
import Data.Proxy             (Proxy (Proxy))
import Database.SQLite.Simple (Connection)
import GHC.Generics           (Generic)
import Lucid
import Servant.API            ((:<|>), (:>), Get)
import Servant.API.Generic    ((:-), AsApi, ToServant, fromServant)
import Servant.HTML.Lucid     (HTML)
import Servant.Server         (ServerT)

import GhStats.Db    (selectRepoStats)
import GhStats.Types (AsServerError, RepoStats)

type ReposRoute =
  "repos" :> Get '[HTML] [RepoStats]

data GhStatsApi = GhStatsApi
  { _ghStatsRepos :: ReposRoute
  } deriving (Generic)

ghStatsApi ::
  Proxy GhStatsApi
ghStatsApi = Proxy

-- ghStatsServer ::
--   ( MonadReader Connection m
--   , MonadError e m
--   , AsServerError e
--   )
--   => ServerT GhStatsApi m
-- ghStatsServer =
--   reposServer

-- reposServer ::
--   ( MonadReader Connection m
--   , MonadError e m
--   , AsServantError m
--   )
--   => ServerT GhStatsApi m
-- reposServer =
