module Main (main) where

import Test.Tasty

import qualified BPC.DB.AuthIntegrationSpec
import qualified BPC.DB.DatabaseSpec
import qualified BPC.DB.DocumentsIntegrationSpec
import qualified BPC.DB.EventsIntegrationSpec
import qualified BPC.DB.FactsIntegrationSpec
import qualified BPC.DB.JobsIntegrationSpec
import qualified BPC.DB.PassportsIntegrationSpec
import qualified BPC.DB.RulesIntegrationSpec
import qualified BPC.DB.SnapshotsIntegrationSpec
import qualified BPC.DB.TenantIsolationSpec
import qualified BPC.DB.WebhooksIntegrationSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "BPC.DB Integration Tests"
  [ BPC.DB.DatabaseSpec.tests
  , BPC.DB.EventsIntegrationSpec.tests
  , BPC.DB.TenantIsolationSpec.tests
  , BPC.DB.DocumentsIntegrationSpec.tests
  , BPC.DB.FactsIntegrationSpec.tests
  , BPC.DB.SnapshotsIntegrationSpec.tests
  , BPC.DB.PassportsIntegrationSpec.tests
  , BPC.DB.JobsIntegrationSpec.tests
  , BPC.DB.RulesIntegrationSpec.tests
  , BPC.DB.AuthIntegrationSpec.tests
  , BPC.DB.WebhooksIntegrationSpec.tests
  ]
