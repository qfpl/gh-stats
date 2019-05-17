module Main where

import GhStats (getStats)

main ::
  IO ()
main = do
  es <- getStats
  either print print es
