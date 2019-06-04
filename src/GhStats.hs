{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GhStats where

import           Control.Lens            (makeLenses, to)
import           Data.Semigroup          ((<>))
import           Data.Sv                 (NameEncode, (=:))
import qualified Data.Sv.Encode          as E
import           Data.Time.Clock         (UTCTime)
import           GitHub                  (Error, Name, Organization, Repo (..),
                                          untagName)
import           GitHub.Data.Traffic     (Referrer)
import           GitHub.Endpoints.Repos  (organizationRepos)
import           GitHub.Internal.Prelude (Vector)

data RepoStats =
  RepoStats {
    _name             :: Name Repo
  , _timestamp        :: UTCTime
  , _stars            :: Int
  , _forks            :: Maybe Int
  , _popularReferrers :: Vector Referrer
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
  -> IO (Either Error (Vector RepoStats))
getOrgStats =
  (fmap . fmap . fmap) toRepoStats . organizationRepos

