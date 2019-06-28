{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns  #-}

module GhStats where

import           Control.Monad                  ((<=<))
import           Control.Monad.Error.Lens       (throwing)
import           Control.Monad.Except           (MonadError)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.Maybe                     (fromMaybe)
import           Data.Time.Clock                (getCurrentTime)
import           Data.Vector                    (Vector)
import qualified GitHub                         as GH
import           GitHub.Data.Traffic            (Period (Day))
import           GitHub.Endpoints.Repos         (organizationRepos)
import           GitHub.Endpoints.Repos.Traffic (clones', popularPaths',
                                                 popularReferrers', views')

import           GhStats.Types                  (AsGhError, Forks (Forks), HighLevelRepoStats (HighLevelRepoStats),
                                                 RepoStats (RepoStats),
                                                 Stars (Stars),
                                                 Token (getToken), _GhError)


type GhConstraints e m = (MonadIO m, MonadError e m, AsGhError e)

-- TODO: use validation to collect all failures.
getOrgStats ::
  GhConstraints e m
  => Token
  -> GH.Name GH.Organization
  -> m (Vector RepoStats)
getOrgStats tok =
  traverse (toRepoStats tok) <=< getReposForOrg

getReposForOrg ::
  GhConstraints e m
  => GH.Name GH.Organization
  -> m (Vector GH.Repo)
getReposForOrg org =
  handleGhReq (organizationRepos org)

getHighLevelOrgStats ::
  GhConstraints e m
  => Token
  -> GH.Name GH.Organization
  -> m (Vector HighLevelRepoStats)
getHighLevelOrgStats tok org =
  traverse (toHighLevelRepoStats tok) =<< handleGhReq (organizationRepos org)

toRepoStats ::
  GhConstraints e m
  => Token
  -> GH.Repo
  -> m RepoStats
toRepoStats tok GH.Repo{GH.repoName, GH.repoOwner, GH.repoForks, GH.repoStargazersCount} =
  let
    auth = GH.OAuth . getToken $ tok
    owner = GH.simpleOwnerLogin repoOwner
  in
    RepoStats repoName
    <$> liftIO getCurrentTime
    <*> pure (Stars repoStargazersCount)
    <*> pure (Forks $ fromMaybe 0 repoForks)
    <*> handleGhReq (popularReferrers' auth owner repoName)
    <*> handleGhReq (popularPaths' auth owner repoName)
    <*> handleGhReq (views' auth owner repoName Day)
    <*> handleGhReq (clones' auth owner repoName Day)

toHighLevelRepoStats ::
  GhConstraints e m
  => Token
  -> GH.Repo
  -> m HighLevelRepoStats
toHighLevelRepoStats tok GH.Repo{GH.repoName, GH.repoOwner, GH.repoForks, GH.repoStargazersCount} =
  let
    auth = GH.OAuth . getToken $ tok
    owner = GH.simpleOwnerLogin repoOwner
  in
    HighLevelRepoStats repoName
    <$> liftIO getCurrentTime
    <*> pure (Stars repoStargazersCount)
    <*> pure (Forks $ fromMaybe 0 repoForks)
    <*> handleGhReq (views' auth owner repoName Day)
    <*> handleGhReq (clones' auth owner repoName Day)

handleGhReq ::
  GhConstraints e m
  => IO (Either GH.Error a)
  -> m a
handleGhReq =
  either (throwing _GhError) pure <=< liftIO
