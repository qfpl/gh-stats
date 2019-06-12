{-# LANGUAGE NamedFieldPuns #-}

module GhStats where

import           Control.Monad.Except           (ExceptT (ExceptT))
import           Control.Monad.IO.Class         (liftIO)
import           Data.Maybe                     (fromMaybe)
import           Data.Time.Clock                (getCurrentTime)
import           Data.Vector                    (Vector)
import           GitHub                         (Auth (OAuth), Error, Name,
                                                 Organization, Repo (..),
                                                 SimpleOwner (simpleOwnerLogin))
import           GitHub.Data.Traffic            (Period (Day))
import           GitHub.Endpoints.Repos         (organizationRepos)
import           GitHub.Endpoints.Repos.Traffic (clones', popularPaths',
                                                 popularReferrers', views')

import           GhStats.Types                  (Forks (Forks), HighLevelRepoStats (HighLevelRepoStats),
                                                 RepoStats (RepoStats),
                                                 Stars (Stars),
                                                 Token (getToken))


-- TODO: use validation to collect all failures.
getOrgStats ::
  Token
  -> Name Organization
  -> ExceptT Error IO (Vector RepoStats)
getOrgStats tok org =
  traverse (toRepoStats tok) =<< ExceptT (organizationRepos org)

getHighLevelOrgStats ::
  Token
  -> Name Organization
  -> ExceptT Error IO (Vector HighLevelRepoStats)
getHighLevelOrgStats tok org =
  traverse (toHighLevelRepoStats tok) =<< ExceptT (organizationRepos org)

toRepoStats ::
  Token
  -> Repo
  -> ExceptT Error IO RepoStats
toRepoStats tok Repo{repoName, repoOwner, repoForks, repoStargazersCount} =
  let
    auth = OAuth . getToken $ tok
    owner = simpleOwnerLogin repoOwner
  in
    RepoStats repoName
    <$> liftIO getCurrentTime
    <*> pure (Stars repoStargazersCount)
    <*> pure (Forks $ fromMaybe 0 repoForks)
    <*> ExceptT (popularReferrers' auth owner repoName)
    <*> ExceptT (popularPaths' auth owner repoName)
    <*> ExceptT (views' auth owner repoName Day)
    <*> ExceptT (clones' auth owner repoName Day)

toHighLevelRepoStats ::
  Token
  -> Repo
  -> ExceptT Error IO HighLevelRepoStats
toHighLevelRepoStats tok Repo{repoName, repoOwner, repoForks, repoStargazersCount} =
  let
    auth = OAuth . getToken $ tok
    owner = simpleOwnerLogin repoOwner
  in
    HighLevelRepoStats repoName
    <$> liftIO getCurrentTime
    <*> pure (Stars repoStargazersCount)
    <*> pure (Forks $ fromMaybe 0 repoForks)
    <*> ExceptT (views' auth owner repoName Day)
    <*> ExceptT (clones' auth owner repoName Day)
