{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module GhStats.Types where

import           Control.Exception                  (Exception, throw)
import           Control.Lens
    (Getter, Lens', Prism', lens, makeClassyPrisms, makeLenses, makeWrapped,
    prism, to, ( # ), (&), (^.), _Wrapped)
import           Control.Monad.Except
    (ExceptT (ExceptT), MonadError, runExceptT)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader
    (MonadReader, ReaderT, asks, runReaderT)
import           Data.Aeson
    (ToJSON (toJSON), object, (.=))
import           Data.Bifunctor                     (first)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy.Char8         as BSL8
import           Data.Foldable                      (toList)
import           Data.List.NonEmpty                 (NonEmpty)
import           Data.String                        (IsString)
import           Data.Sv                            (NameEncode, (=:))
import qualified Data.Sv.Encode                     as E
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (encodeUtf8)
import           Data.Time.Clock                    (UTCTime)
import           Data.Validation
    (Validate (_Validation), Validation, validationNel)
import           Data.Vector                        (Vector)
import           Database.SQLite.Simple             (Connection, ToRow (toRow))
import           Database.SQLite.Simple.FromField   (FromField)
import           Database.SQLite.Simple.ToField     (ToField)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import qualified GitHub                             as GH
import           Network.HTTP.Client                (HttpException)
import           Servant
    (Handler (Handler), ServerError, err500, errBody)

newtype GhStatsM a =
  GhStatsM (ReaderT Connection (ExceptT Error IO) a)
  deriving (Functor, Applicative, Monad, MonadReader Connection, MonadIO, MonadError Error)

runGhStatsM ::
  Connection
  -> GhStatsM a
  -> IO a
runGhStatsM conn (GhStatsM m) =
  fmap (either throw id) . runExceptT . runReaderT m $ conn

runGhStatsMToHandler ::
  Connection
  -> GhStatsM a
  -> Handler a
runGhStatsMToHandler conn (GhStatsM m) =
  Handler . ExceptT . fmap (first toServantError) . runExceptT . runReaderT m $ conn

newtype Count a = Count Int
  deriving (Eq, Show, FromField, ToField)

newtype Uniques a = Uniques Int
  deriving (Eq, Show, FromField, ToField)

newtype Views = Views Int
  deriving (Eq, Show, FromField, ToField)

newtype Clones = Clones Int
  deriving (Eq, Show, FromField, ToField)

newtype ValResult e a =
  ValResult {getValResult :: Validation (NonEmpty e) a}
  deriving (Show, Functor, Applicative)

class HasConnection s where
  connection :: Lens' s Connection

instance HasConnection Connection where
  connection = lens id (const id)

data Error =
  SQLiteError SQLiteResponse
  | GithubError GH.Error
  | TooManyResults Text
  | ConflictingVCData Text (NonEmpty CVD)
  deriving (Show)

toServantError ::
  Error
  -> ServerError
toServantError e =
  let
    -- dbMsg = "A database error occurred."
    textToBSL = BSL8.fromStrict . encodeUtf8
    msg = case e of
      SQLiteError sqle    -> BSL8.pack $ show sqle
      GithubError ghe     -> BSL8.pack $ show ghe
      TooManyResults tmr  -> textToBSL tmr
      ConflictingVCData table cvd ->
        let
          conflicts = BSL8.intercalate "\n  " . toList . fmap (BSL8.pack . show) $ cvd
        in
          "Conflicting data in " <> textToBSL table <> ":\n  " <> conflicts
  in
    err500 {errBody = msg}

data CVD =
  forall a.
  CVD {
    _cvdName            :: GH.Name GH.Repo
  , _cvdTimestamp       :: UTCTime
  , _cvdExistingCount   :: Count a
  , _cvdExistingUniques :: Uniques a
  , _cvdNewCount        :: Count a
  , _cvdNewUniques      :: Uniques a
  }

deriving instance Show CVD

-- makeLenses silently fails creating lenses for these given the existential. We
-- can easily create the Getters though.
cvdNewCount ::
  Getter CVD (Count a)
cvdNewCount =
  to $ \CVD{_cvdNewCount = Count n} -> Count n

cvdNewUniques ::
  Getter CVD (Uniques a)
cvdNewUniques =
  to $ \CVD{_cvdNewUniques = Uniques n} -> Uniques n

cvdExistingCount ::
  Getter CVD (Count a)
cvdExistingCount =
  to $ \CVD{_cvdExistingCount = Count n} -> Count n

cvdExistingUniques ::
  Getter CVD (Uniques a)
cvdExistingUniques =
  to $ \CVD{_cvdExistingUniques = Uniques n} -> Uniques n

makeClassyPrisms ''Error
makeClassyPrisms ''SQLiteResponse

ghStatsMToValidationM ::
  ( Monad m
  , MonadReader r m
  , HasConnection r
  , AsError e
  , MonadIO m
  )
  => GhStatsM a
  -> m (ValResult e a)
ghStatsMToValidationM (GhStatsM m) = do
  conn <- asks (^. connection)
  let
    runM = runExceptT . flip runReaderT conn
    toValidation = ValResult . validationNel . first (_Error #)
  liftIO . fmap toValidation . runM $ m

ghStatsMToValidationIO ::
  ( AsError e
  )
  => Connection
  -> GhStatsM a
  -> IO (ValResult e a)
ghStatsMToValidationIO conn ghStatsM =
  runReaderT (ghStatsMToValidationM ghStatsM) conn

instance Exception Error

-- Writing by hand as ServerError has a single constructor of the same name.
class AsServerError e where
  _ServerError :: Prism' e ServerError

instance AsServerError ServerError where
  _ServerError = id

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
  deriving (Eq, Show, FromField, ToField, ToJSON)

makeWrapped ''Stars

newtype Forks = Forks Int
  deriving (Eq, Show, FromField, ToField, ToJSON)

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

instance ToJSON RepoStats where
  toJSON (RepoStats n t s f r p v c) = object
    [ "name" .= n
    , "timestamp" .= t
    , "stars" .= s
    , "forks" .= f
    , "referrers" .= r
    , "paths" .= p
    , "views" .= v
    , "clones" .= c
    ]

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
makeWrapped ''ValResult
