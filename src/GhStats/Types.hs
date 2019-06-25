{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module GhStats.Types where

import           Control.Exception                  (Exception, throw)
import           Control.Lens                       (Lens', Prism', lens,
                                                     makeClassyPrisms,
                                                     makeLenses, makeWrapped,
                                                     prism, to, (&), (^.),
                                                     _Wrapped)
import           Control.Monad.Except               (ExceptT, MonadError,
                                                     runExceptT)
import           Control.Monad.IO.Class             (MonadIO)
import           Control.Monad.Reader               (MonadReader, ReaderT,
                                                     runReaderT)
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

newtype GhStatsM a =
  GhStatsM (ReaderT Connection (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO, MonadError Error)

runGhStatsM ::
  Connection
  -> GhStatsM a
  -> IO a
runGhStatsM conn (GhStatsM m) =
  fmap (either throw id) . runExceptT . runReaderT m $ conn

newtype Count a = Count Int
  deriving (Eq, Show, FromField, ToField)

newtype Uniques a = Uniques Int
  deriving (Eq, Show, FromField, ToField)

newtype Views = Views Int
  deriving (Eq, Show, FromField, ToField)

newtype Clones = Clones Int
  deriving (Eq, Show, FromField, ToField)

class HasConnection s where
  connection :: Lens' s Connection

instance HasConnection Connection where
  connection = lens id (const id)

data Error =
  SQLiteError SQLiteResponse
  | GithubError GH.Error
  | TooManyResults Text
  | ConflictingViewData [CVD]
  deriving (Show)

data CVD =
  CVD {
    _cvdName            :: GH.Name GH.Repo
  , _cvdTimestamp       :: UTCTime
  , _cvdExistingCount   :: Count GH.Views
  , _cvdExistingUniques :: Uniques GH.Views
  , _cvdNewCount        :: Count GH.Views
  , _cvdNewUniques      :: Uniques GH.Views
  }
  deriving (Show)

makeLenses ''CVD

makeClassyPrisms ''Error
makeClassyPrisms ''SQLiteResponse

instance Exception Error

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

makeWrapped ''Stars

newtype Forks = Forks Int
  deriving (Eq, Show, FromField, ToField)

makeWrapped ''Forks

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
  , _hlStars     :: !Stars
  , _hlForks     :: !Forks
  , _hlViews     :: !GH.Views
  , _hlClones    :: !GH.Clones
  }
  deriving Show

makeLenses ''HighLevelRepoStats

highLevelRepoStatsEnc ::
  NameEncode HighLevelRepoStats
highLevelRepoStatsEnc =
     "name" =: E.encodeOf (hlName.to GH.untagName) E.text
  <> "stars" =: E.encodeOf (hlStars._Wrapped) E.int
  <> "forks" =: E.encodeOf (hlForks._Wrapped) E.int
  <> "views" =: E.encodeOf (hlViews.to GH.viewsCount) E.int
  <> "unique views" =: E.encodeOf (hlViews.to GH.viewsUniques) E.int
  <> "clones" =: E.encodeOf (hlClones.to GH.clonesCount) E.int
  <> "unique clones" =: E.encodeOf (hlClones.to GH.clonesUniques) E.int

newtype Token = Token { getToken :: ByteString }
  deriving (Eq, Show, IsString)

makeWrapped ''Count
makeWrapped ''Uniques