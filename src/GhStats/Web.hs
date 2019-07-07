{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module GhStats.Web where

import Data.Proxy             (Proxy (Proxy))
import GHC.Generics           (Generic)
import Servant.API            ((:>), Get)
import Servant.API.Generic    ((:-), ToServantApi, genericApi)
import Servant.HTML.Lucid     (HTML)
import Servant.Server.Generic (AsServerT)

import GhStats.Db       (DbConstraints, selectLatestRepoStats)
import GhStats.Db.Types (DbRepoStats)
import GhStats.Types    (AsError)

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
