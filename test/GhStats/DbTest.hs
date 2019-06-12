module GhStats.DbTest where

import Test.Tasty (TestTree, testGroup)

testDb :: TestTree
testDb = testGroup "GhStats.Db" [
    testRepoStatsRoundTrip
  ]
