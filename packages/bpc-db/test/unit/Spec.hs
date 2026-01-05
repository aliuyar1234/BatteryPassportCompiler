module Main (main) where

import Test.Tasty

import qualified BPC.DB.PoolSpec
import qualified BPC.DB.EventsSpec
import qualified BPC.DB.JobsSpec
import qualified BPC.DB.SnapshotsSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BPC.DB Unit Tests"
  [ BPC.DB.PoolSpec.tests
  , BPC.DB.EventsSpec.tests
  , BPC.DB.JobsSpec.tests
  , BPC.DB.SnapshotsSpec.tests
  ]
