{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GhStats where

import           Control.Lens            (makeLenses, to)
import           Data.Semigroup          ((<>))
import           Data.Sv                 (NameEncode, (=:))
import qualified Data.Sv.Encode          as E
import           GitHub                  (Error, Name, Organization, Repo (..),
                                          untagName)
import           GitHub.Endpoints.Repos  (organizationRepos)
import           GitHub.Internal.Prelude (Vector)

data Stats =
  Stats {
    repos :: Vector RepoStats
  }
  deriving Show

data RepoStats =
  RepoStats {
    _name  :: Name Repo
  , _stars :: Int
  , _forks :: Maybe Int
  }
  deriving Show

makeLenses ''RepoStats

repoStatsEnc ::
  NameEncode RepoStats
repoStatsEnc =
     "name" =: E.encodeOf (name.to untagName) E.text
  <> "stars" =: E.encodeOf stars E.int
  <> "forks" =: E.encodeOf forks (E.int E.?>> "0")

toRepoStats ::
  Repo
  -> RepoStats
toRepoStats Repo {repoName, repoForks, repoStargazersCount} =
  RepoStats {
    _name = repoName
  , _stars = repoStargazersCount
  , _forks = repoForks
  }

getOrgStats ::
  Name Organization
  -> IO (Either Error Stats)
getOrgStats =
  (fmap . fmap) (Stats . fmap toRepoStats) . organizationRepos

