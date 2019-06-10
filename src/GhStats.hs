{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GhStats where

import           Control.Lens                   (makeLenses, to)
import           Control.Monad.Except           (ExceptT (ExceptT))
import           Control.Monad.IO.Class         (liftIO)
import           Data.ByteString                (ByteString)
import           Data.Semigroup                 ((<>))
import           Data.Sv                        (NameEncode, (=:))
import qualified Data.Sv.Encode                 as E
import           Data.Time.Clock                (UTCTime, getCurrentTime)
import           GitHub                         (Auth (OAuth), Error, Name,
                                                 Organization, Repo (..),
                                                 SimpleOwner (simpleOwnerLogin),
                                                 untagName)
import           GitHub.Data.Traffic            (Clones, Period (Day),
                                                 PopularPath, Referrer, Views)
import           GitHub.Endpoints.Repos         (organizationRepos)
import           GitHub.Endpoints.Repos.Traffic (clones', popularPaths',
                                                 popularReferrers', views')
import           GitHub.Internal.Prelude        (Vector)

data RepoStats =
  RepoStats {
    _name             :: Name Repo
  , _timestamp        :: UTCTime
  , _stars            :: Int
  , _forks            :: Maybe Int
  , _popularReferrers :: Vector Referrer
  , _popularPaths     :: Vector PopularPath
  , _views            :: Views
  , _clones           :: Clones
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
  ByteString
  -> Repo
  -> ExceptT Error IO RepoStats
toRepoStats tok Repo{repoName, repoOwner, repoForks, repoStargazersCount} =
  let
    auth = OAuth tok
    owner = simpleOwnerLogin repoOwner
  in
    RepoStats repoName
    <$> liftIO getCurrentTime
    <*> pure repoStargazersCount
    <*> pure repoForks
    <*> ExceptT (popularReferrers' auth owner repoName)
    <*> ExceptT (popularPaths' auth owner repoName)
    <*> ExceptT (views' auth owner repoName Day)
    <*> ExceptT (clones' auth owner repoName Day)

getOrgStats ::
  ByteString
  -> Name Organization
  -> ExceptT Error IO (Vector RepoStats)
getOrgStats tok org =
  traverse (toRepoStats tok) =<< ExceptT (organizationRepos org)

