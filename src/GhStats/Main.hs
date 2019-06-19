{-# LANGUAGE LambdaCase #-}

module GhStats.Main where

import           Control.Monad          ((<=<))
import           Control.Monad.Except   (ExceptT, runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (runReaderT)
import qualified Data.ByteString.Lazy   as LBS
import           Data.Foldable          (toList)
import           Data.Sv                (defaultEncodeOptions, encodeNamed)
import           Database.SQLite.Simple (open)
import qualified GitHub                 as GH
import           Options.Applicative    (Parser, command, execParser, fullDesc,
                                         header, help, helper, info, long,
                                         metavar, progDesc, short, strOption,
                                         subparser, (<**>))

import           GhStats                (getHighLevelOrgStats, getOrgStats)
import           GhStats.Db             (addToDb, runDb)
import           GhStats.Types          (Error, Token, highLevelRepoStatsEnc)

go ::
  Command
  -> IO ()
go = \case
  Csv orgName token -> csv orgName token
  UpdateDb orgName dbFile token -> updateDb orgName dbFile token
  Serve _orgName _dbFile _token -> error "todo: serve"

csv ::
  GH.Name GH.Organization
  -> Token
  -> IO ()
csv orgName token = print <=< runExceptT $ do
  let
    dumpCsv = LBS.putStr . encodeNamed highLevelRepoStatsEnc defaultEncodeOptions . toList
  es <- getHighLevelOrgStats token orgName
  (liftIO :: IO () -> ExceptT Error IO ()) $ dumpCsv es

updateDb ::
  GH.Name GH.Organization
  -> FilePath
  -> Token
  -> IO ()
updateDb orgName dbFile token = print <=< runExceptT $ do
  conn <- runDb $ open dbFile
  repoStats <- getOrgStats token orgName
  runReaderT (addToDb repoStats) conn :: ExceptT Error IO ()

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

data Command =
  Csv (GH.Name GH.Organization) Token                -- ^ Output the per-repository summary as a CSV to stdout
  | UpdateDb (GH.Name GH.Organization) FilePath Token -- ^ Insert the latest summary into the SQLite DB
  | Serve (GH.Name GH.Organization) FilePath Token    -- ^ Serve the latest summary as a web page
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
      Serve <$> orgParser <*> dbParser <*> tokenParser
    serveDesc =
      "Serve the latest summary as a web page."
  in
    subparser
      ( command "csv" (info csvParser (progDesc csvDesc))
     <> command "update" (info updateParser (progDesc updateDesc))
     <> command "serve" (info serveParser (progDesc serveDesc))
      )

