{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module GhStats.Types where

import           Control.Lens                       (Lens', Prism',
                                                     makeClassyPrisms,
                                                     makeLenses, prism, to, (&),
                                                     (^.))
import           Data.ByteString                    (ByteString)
import           Data.String                        (IsString)
import           Data.Sv                            (NameEncode, (=:))
import qualified Data.Sv.Encode                     as E
import           Data.Text                          (Text)
import           Data.Time.Clock                    (UTCTime)
import           Data.Vector                        (Vector)
import           Database.SQLite.Simple             (Connection, ToRow (toRow))
import           Database.SQLite.Simple.FromField   (FromField)
import           Database.SQLite.Simple.ToField     (ToField)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import qualified GitHub                             as GH
import           Network.HTTP.Client                (HttpException)

class HasConnection s where
  connection :: Lens' s Connection

data Error =
  SQLiteError SQLiteResponse
  | GithubError GH.Error
  deriving (Show)

makeClassyPrisms ''Error
makeClassyPrisms ''SQLiteResponse

-- Writing the class by hand because of naming clashes
class AsGhError e where
  _GhError :: Prism' e GH.Error
  _GhHttpError :: Prism' e HttpException
  _GhParseError :: Prism' e Text
  _GhJsonError :: Prism' e Text
  _GhUserError :: Prism' e Text

  _GhHttpError = _GhError . _GhHttpError
  _GhParseError = _GhError . _GhParseError
  _GhJsonError = _GhError . _GhJsonError
  _GhUserError = _GhError . _GhUserError

instance AsGhError GH.Error where
  _GhError =
    prism id Right

  _GhHttpError =
    prism GH.HTTPError (\case; GH.HTTPError e -> Right e; e -> Left e)

  _GhParseError =
    prism GH.ParseError (\case; GH.ParseError t -> Right t; e -> Left e)

  _GhJsonError =
    prism GH.JsonError (\case; GH.JsonError t -> Right t; e -> Left e)

  _GhUserError =
    prism GH.UserError (\case; GH.UserError t -> Right t; e -> Left e)

instance AsGhError Error where
  _GhError = _GithubError

instance AsSQLiteResponse Error where
  _SQLiteResponse = _SQLiteError

newtype Stars = Stars Int
  deriving (Eq, Show, FromField, ToField)

newtype Forks = Forks Int
  deriving (Eq, Show, FromField, ToField)

data RepoStats =
  RepoStats {
    _repoStatsName             :: GH.Name GH.Repo
  , _repoStatsTimestamp        :: UTCTime
  , _repoStatsStars            :: Stars
  , _repoStatsForks            :: Forks
  , _repoStatsPopularReferrers :: Vector GH.Referrer
  , _repoStatsPopularPaths     :: Vector GH.PopularPath
  , _repoStatsViews            :: GH.Views
  , _repoStatsClones           :: GH.Clones
  }
  deriving Show

makeLenses ''RepoStats

instance ToRow RepoStats where
  toRow rs = toRow (
      rs ^. repoStatsName &  GH.untagName
    , rs ^. repoStatsTimestamp
    , rs ^. repoStatsStars
    , rs ^. repoStatsForks
    )

data HighLevelRepoStats =
  HighLevelRepoStats {
    _hlName      :: !(GH.Name GH.Repo)
  , _hlTimestamp :: !UTCTime
  , _hlStars     :: !Int
  , _hlForks     :: !Int
  , _hlViews     :: !GH.Views
  , _hlClones    :: !GH.Clones
  }
  deriving Show

makeLenses ''HighLevelRepoStats

highLevelRepoStatsEnc ::
  NameEncode HighLevelRepoStats
highLevelRepoStatsEnc =
     "name" =: E.encodeOf (hlName.to GH.untagName) E.text
  <> "stars" =: E.encodeOf hlStars E.int
  <> "forks" =: E.encodeOf hlForks E.int
  <> "views" =: E.encodeOf (hlViews.to GH.viewsCount) E.int
  <> "unique views" =: E.encodeOf (hlViews.to GH.viewsUniques) E.int
  <> "clones" =: E.encodeOf (hlClones.to GH.clonesCount) E.int
  <> "unique clones" =: E.encodeOf (hlClones.to GH.clonesUniques) E.int

newtype Token = Token { getToken :: ByteString }
  deriving (Eq, Show, IsString)
