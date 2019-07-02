{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE TypeOperators       #-}

module GhStats.Web where

import Control.Monad.Except   (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (MonadReader)
import Data.Proxy             (Proxy (Proxy))
import GHC.Generics           (Generic)
import Lucid
import Servant.API            ((:>), Get)
import Servant.API.Generic
    ((:-), ToServantApi, genericApi)
import Servant.HTML.Lucid     (HTML)
import Servant.Server.Generic (AsServerT)

import GhStats.Db       (DbConstraints, selectLatestRepoStats)
import GhStats.Db.Types (DbRepoStats)
import GhStats.Types
    (AsSQLiteResponse, AsServerError, HasConnection, AsError)

type ReposRoute =
  "repos" :> Get '[HTML] [DbRepoStats]

data GhStatsApi route = GhStatsApi
  { _ghStatsRepos :: route :- ReposRoute
  } deriving (Generic)

ghStatsApi ::
  Proxy (ToServantApi GhStatsApi)
ghStatsApi = genericApi (Proxy :: Proxy GhStatsApi)

ghStatsServer ::
  ( DbConstraints e r m
  , AsError e
  )
  => GhStatsApi (AsServerT m)
ghStatsServer = GhStatsApi
  { _ghStatsRepos = reposServer
  }

reposServer ::
  ( DbConstraints e r m
  , AsError e
  )
  => m [DbRepoStats]
reposServer =
  selectLatestRepoStats
