module GhStats.Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (toList)
import           Data.Sv              (defaultEncodeOptions, encodeNamed)
import qualified Data.Text            as T
import           GitHub               (mkOrganizationName)
import           Options.Applicative  (command, subparser)
import           System.Environment   (getArgs)

import           GhStats              (getOrgStats, repoStatsEnc)

data Command =
  Csv
  | Server FilePath
  deriving (Eq, Show)

main ::
  IO ()
main = do
  let
    dumpCsv = LBS.putStr . encodeNamed repoStatsEnc defaultEncodeOptions . toList
  (org:_) <- getArgs
  es <- getOrgStats . mkOrganizationName $ T.pack org
  either print dumpCsv es
