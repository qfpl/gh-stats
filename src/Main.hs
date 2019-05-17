{-# LANGUAGE OverloadedStrings #-}

module Main where

import GhStats (getOrgStats)

main ::
  IO ()
main = do
  es <- getOrgStats "qfpl"
  either print print es
