{-# LANGUAGE LambdaCase #-}

module GhStats.Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (toList)
import           Data.Sv              (defaultEncodeOptions, encodeNamed)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GitHub               (mkOrganizationName, Name, Organization)
import           Options.Applicative  (Parser, command, execParser, fullDesc,
                                       header, help, helper, info, long,
                                       metavar, progDesc, short, strOption,
                                       subparser, (<**>))

import           GhStats              (getOrgStats, repoStatsEnc)

go ::
  Command
  -> IO ()
go = \case
  Csv orgName -> csv orgName
  UpdateDb _orgName _dbFile -> error "todo: update"
  Serve _orgName _dbFile -> error "todo: serve"

csv ::
  Name Organization
  -> IO ()
csv orgName = do
  let
    dumpCsv = LBS.putStr . encodeNamed repoStatsEnc defaultEncodeOptions . toList
  es <- getOrgStats orgName
  either print dumpCsv es

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
  Csv (Name Organization)                 -- ^ Output the per-repository summary as a CSV to stdout
  | UpdateDb (Name Organization) FilePath -- ^ Insert the latest summary into the SQLite DB
  | Serve (Name Organization) FilePath    -- ^ Serve the latest summary as a web page
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

cmdParser ::
  Parser Command
cmdParser =
  let
    csvDesc =
      "Output the latest summary as a CSV to stdout."

    updateParser =
      UpdateDb <$> orgParser <*> dbParser
    updateDesc =
      "Insert the latest summary into the SQLite DB, creating the DB if necessary."

    serveParser =
      Serve <$> orgParser <*> dbParser
    serveDesc =
      "Serve the latest summary as a web page."
  in
    subparser
      ( command "csv" (info (Csv <$> orgParser) (progDesc csvDesc))
     <> command "update" (info updateParser (progDesc updateDesc))
     <> command "serve" (info serveParser (progDesc serveDesc))
      )

