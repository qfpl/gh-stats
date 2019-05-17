{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module GhStats where

import GitHub (Error, Name)
import GitHub.Data.Repos (Repo (..))
import GitHub.Endpoints.Repos (organizationRepos)
import GitHub.Internal.Prelude (Vector)

data Stats =
  Stats {
    repos :: Vector RepoStats
  }
  deriving Show

data RepoStats =
  RepoStats {
    name :: Name Repo
  , stars :: Int
  , forks :: Maybe Int
  }
  deriving Show

toRepoStats ::
  Repo
  -> RepoStats
toRepoStats Repo {repoName, repoForks, repoStargazersCount} =
  RepoStats {name = repoName, stars = repoStargazersCount, forks = repoForks}

getStats ::
  IO (Either Error Stats)
getStats =
  (fmap . fmap) (Stats . fmap toRepoStats) $ organizationRepos "qfpl"

