{-# LANGUAGE LambdaCase #-}

module GhStats.Main where

import           Control.Monad.Except (runExceptT)
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (toList)
import           Data.Sv              (defaultEncodeOptions, encodeNamed)
import           GitHub               (Name, Organization)
import           Options.Applicative  (Parser, command, execParser, fullDesc,
                                       header, help, helper, info, long,
                                       metavar, progDesc, short, strOption,
                                       subparser, (<**>))

import           GhStats              (Token, getHighLevelOrgStats, getOrgStats,
                                       highLevelRepoStatsEnc)
import           GhStats.Db           (addToDb)

go ::
  Command
  -> IO ()
go = \case
  Csv orgName token -> csv orgName token
  UpdateDb orgName dbFile token -> updateDb orgName dbFile token
  Serve _orgName _dbFile _token -> error "todo: serve"

csv ::
  Name Organization
  -> Token
  -> IO ()
csv orgName token = do
  let
    dumpCsv = LBS.putStr . encodeNamed highLevelRepoStatsEnc defaultEncodeOptions . toList
  es <- runExceptT $ getHighLevelOrgStats token orgName
  either print dumpCsv es

updateDb ::
  Name Organization
  -> FilePath
  -> Token
  -> IO ()
updateDb orgName dbFile token = printErrors . runExceptT $ do
  repoStats <- getOrgStats token orgName
  addToDb repoStats

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
  Csv (Name Organization) Token                -- ^ Output the per-repository summary as a CSV to stdout
  | UpdateDb (Name Organization) FilePath Token -- ^ Insert the latest summary into the SQLite DB
  | Serve (Name Organization) FilePath Token    -- ^ Serve the latest summary as a web page
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
  Parser (Name Organization)
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

