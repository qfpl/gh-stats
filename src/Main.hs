{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable        (toList)
import           Data.Sv              (defaultEncodeOptions, encodeNamed)
import           GhStats              (getOrgStats, repoStatsEnc)

main ::
  IO ()
main = do
  let
    dumpCsv = LBS.putStr . encodeNamed repoStatsEnc defaultEncodeOptions . toList
  es <- getOrgStats "qfpl"
  either print dumpCsv es
