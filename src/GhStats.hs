{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module GhStats where

import           Control.Lens                   (makeLenses, to)
import           Control.Monad.Except           (ExceptT (ExceptT))
import           Control.Monad.IO.Class         (liftIO)
import           Data.ByteString                (ByteString)
import           Data.Semigroup                 ((<>))
import           Data.String                    (IsString)
import           Data.Sv                        (NameEncode, (=:))
import qualified Data.Sv.Encode                 as E
import           Data.Time.Clock                (UTCTime, getCurrentTime)
import           Data.Vector                    (Vector)
import           GitHub                         (Auth (OAuth), Error, Name,
                                                 Organization, Repo (..),
                                                 SimpleOwner (simpleOwnerLogin),
                                                 untagName)
import           GitHub.Data.Traffic            (Clones (clonesCount, clonesUniques),
                                                 Period (Day), PopularPath,
                                                 Referrer,
                                                 Views (viewsCount, viewsUniques))
import           GitHub.Endpoints.Repos         (organizationRepos)
import           GitHub.Endpoints.Repos.Traffic (clones', popularPaths',
                                                 popularReferrers', views')

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

data HighLevelRepoStats =
  HighLevelRepoStats {
    _hlName             :: Name Repo
  , _hlTimestamp        :: UTCTime
  , _hlStars            :: Int
  , _hlForks            :: Maybe Int
  , _hlViews            :: Views
  , _hlClones           :: Clones
  }
  deriving Show

makeLenses ''HighLevelRepoStats

highLevelRepoStatsEnc ::
  NameEncode HighLevelRepoStats
highLevelRepoStatsEnc =
     "name" =: E.encodeOf (hlName.to untagName) E.text
  <> "stars" =: E.encodeOf hlStars E.int
  <> "forks" =: E.encodeOf hlForks (E.int E.?>> "0")
  <> "views" =: E.encodeOf (hlViews.to viewsCount) E.int
  <> "unique views" =: E.encodeOf (hlViews.to viewsUniques) E.int
  <> "clones" =: E.encodeOf (hlClones.to clonesCount) E.int
  <> "unique clones" =: E.encodeOf (hlClones.to clonesUniques) E.int

newtype Token = Token { getToken :: ByteString }
  deriving (Eq, Show, IsString)

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
    <*> pure repoStargazersCount
    <*> pure repoForks
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
    <*> pure repoStargazersCount
    <*> pure repoForks
    <*> ExceptT (views' auth owner repoName Day)
    <*> ExceptT (clones' auth owner repoName Day)
