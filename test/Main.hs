module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)

import Database.SQLite.Simple (open)

import GhStats.DbTest (testDb)

main :: IO ()
main = do
  conn <- open testDb
  defaultMain tests

testDb :: FilePath
testDb = "test.sqlite"

tests :: TestTree
tests = testGroup "gh-stats" [
   testDb
 ]
