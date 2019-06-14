module Main where

import           Test.Tasty             (TestTree, defaultMain, testGroup)

import           Database.SQLite.Simple (Connection, open)

import           GhStats.Db             (initDb)
import           GhStats.Types          (runGhStatsM)

import           GhStats.DbTest         (testDb)

main :: IO ()
main = do
  conn <- open testDbPath
  runGhStatsM conn initDb
  defaultMain $ tests conn

testDbPath :: FilePath
testDbPath = "test.sqlite"

tests :: Connection -> TestTree
tests conn = testGroup "gh-stats" [
   testDb conn
 ]
