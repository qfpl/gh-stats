{-# LANGUAGE LambdaCase #-}

module GhStats.Main where

import           Control.Exception        (throw)
import           Control.Monad            ((<=<))
import           Control.Monad.Except     (ExceptT, runExceptT)
import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Foldable            (toList, traverse_)
import           Data.Functor.Compose     (Compose (Compose, getCompose))
import           Data.List.NonEmpty       (NonEmpty)
import           Data.Sv                  (defaultEncodeOptions, encodeNamed)
import           Data.Validation          (Validation, validationNel, validation)
import           Data.Word                (Word16)
import           Database.SQLite.Simple   (Connection, open)
import qualified GitHub                   as GH
import           Network.Wai.Handler.Warp (run)
import           Options.Applicative
    (Parser, command, eitherReader, execParser, fullDesc, header, help, helper,
    info, long, metavar, option, progDesc, short, strOption, subparser, (<**>))
import           Servant.Server.Generic   (genericServeT)
import           Text.Read                (readEither)

import GhStats       (getHighLevelOrgStats, getReposForOrg, toRepoStats)
import GhStats.Db    (initDb, insertRepoStatsRun, insertRepoStatsTree)
import GhStats.Types (Error, Token, highLevelRepoStatsEnc, runGhStatsMToHandler)
import GhStats.Web   (ghStatsServer)

go ::
  Command
  -> IO ()
go = \case
  Csv orgName token -> csv orgName token
  UpdateDb orgName dbFile token -> updateDb orgName dbFile token
  Serve dbFile port -> serveyMcServeFace dbFile port

csv ::
  GH.Name GH.Organization
  -> Token
  -> IO ()
csv orgName token =
  let
    runM :: ExceptT Error IO () -> IO ()
    runM = print <=< runExceptT

    enc = encodeNamed highLevelRepoStatsEnc defaultEncodeOptions
    dumpCsv = LBS.putStr . enc . toList
  in
    runM $ do
      es <- getHighLevelOrgStats token orgName
      liftIO $ dumpCsv es

updateDb ::
  GH.Name GH.Organization
  -> FilePath
  -> Token
  -> IO ()
updateDb orgName dbFile token = do
  conn <- open dbFile
  updateDb' conn orgName token

updateDb' ::
  Connection
  -> GH.Name GH.Organization
  -> Token
  -> IO ()
updateDb' conn orgName token =
  let
    runM :: ReaderT Connection (ExceptT Error IO) a -> IO a
    runM = either throw pure <=< runExceptT . flip runReaderT conn

    runToValidation :: ReaderT Connection (ExceptT Error IO) a -> IO (Validation (NonEmpty Error) a)
    runToValidation =
      fmap validationNel . runExceptT . flip runReaderT conn

    ins rsrId = insertRepoStatsTree rsrId <=< toRepoStats token

    printErrors es = do
      putStrLn "Encountered the following errors:"
      traverse_ ((putStr "  " >>) . print) es
  in
    do
      runM initDb
      repos <- runM $ getReposForOrg orgName
      rsrId <- runM insertRepoStatsRun
      res <- getCompose $ traverse (Compose . runToValidation . ins rsrId) repos
      validation printErrors (const $ pure ()) res

serveyMcServeFace ::
  FilePath
  -> Port
  -> IO ()
serveyMcServeFace dbFile (Port port) = do
  conn <- open $ "file:" <> dbFile <> "?mode=ro"
  let app = genericServeT (runGhStatsMToHandler conn) ghStatsServer
  run (fromIntegral port) app

main ::
  IO ()
main =
  go =<< execParser opts
  where
    opts =
      info (cmdParser <**> helper)
        ( fullDesc
       <> header "gh-stats --- record and serve github statistics for an organisation"
        )

newtype Port = Port Word16
  deriving (Eq, Show)

data Command =
  Csv (GH.Name GH.Organization) Token                 -- ^ Output the per-repository summary as a CSV to stdout
  | UpdateDb (GH.Name GH.Organization) FilePath Token -- ^ Insert the latest summary into the SQLite DB
  | Serve FilePath Port                               -- ^ Serve the latest summary as a web page
  deriving (Eq, Show)

dbParser ::
  Parser FilePath
dbParser =
  strOption
    (  long "db-path"
   <> short 'd'
   <> metavar "DB_PATH"
   <> help "Path to the SQLite database to update. Will be created if it doesn't exist."
    )

orgParser ::
  Parser (GH.Name GH.Organization)
orgParser =
  strOption
    ( long "org"
   <> short 'o'
   <> metavar "ORGANISATION"
   <> help "Github organisation to collect stats for."
    )

tokenParser ::
  Parser Token
tokenParser =
  strOption
    ( long "token"
   <> short 't'
   <> metavar "TOKEN"
   <> help "Github OAuth token."
    )

portParser :: Parser Port
portParser =
  let portHelp = help "TCP port to accept requests on"
      mods = long "port" <> short 'p' <> metavar "PORT" <> portHelp
      portReader = eitherReader (fmap Port . readEither)
   in option portReader mods

cmdParser ::
  Parser Command
cmdParser =
  let
    csvParser =
      Csv <$> orgParser <*> tokenParser
    csvDesc =
      "Output the latest summary as a CSV to stdout."

    updateParser =
      UpdateDb <$> orgParser <*> dbParser <*> tokenParser
    updateDesc =
      "Insert the latest summary into the SQLite DB, creating the DB if necessary."

    serveParser =
      Serve <$> dbParser <*> portParser
    serveDesc =
      "Serve the latest summary as a web page."
  in
    subparser
      ( command "csv" (info csvParser (progDesc csvDesc))
     <> command "update" (info updateParser (progDesc updateDesc))
     <> command "serve" (info serveParser (progDesc serveDesc))
      )
