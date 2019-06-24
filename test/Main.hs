module Main where

import           Test.Tasty             (TestTree, defaultMain, testGroup)

import           Control.Monad          (when)
import           Database.SQLite.Simple (Connection, open)
import           System.Directory       (doesFileExist, removeFile)

import           GhStats.Db             (initDb)
import           GhStats.Types          (runGhStatsM)

import           GhStats.DbTest         (testDb)

main :: IO ()
main = do
  flip when (removeFile testDbPath) =<< doesFileExist testDbPath
  conn <- open testDbPath
  runGhStatsM conn initDb
  defaultMain $ tests conn

testDbPath :: FilePath
testDbPath = "test.sqlite"

tests :: Connection -> TestTree
tests conn = testGroup "gh-stats" [
   testDb conn
 ]
