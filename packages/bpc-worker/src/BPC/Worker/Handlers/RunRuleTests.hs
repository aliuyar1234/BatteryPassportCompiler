{-# LANGUAGE OverloadedStrings #-}

-- | Run Rule Tests Handler
--
-- Executes rule tests requiring >= 500 passing cases for VALIDATED.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.RunRuleTests
  ( handle
  , RunRuleTestsPayload(..)
  ) where

import Control.Monad (void, forM)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, diffUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import System.Random (mkStdGen, randomR)

import BPC.Worker.Types
import qualified BPC.DB as DB
import qualified BPC.Core.Evaluator as Evaluator

-- | Minimum passing test cases for VALIDATED status.
minPassingCases :: Int
minPassingCases = 500

-- | Payload for RUN_RULE_TESTS job.
--
-- @since 0.1.0.0
data RunRuleTestsPayload = RunRuleTestsPayload
  { rtpRuleVersionId :: UUID
  , rtpTenantId :: UUID
  , rtpSeed :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON RunRuleTestsPayload where
  parseJSON = Aeson.withObject "RunRuleTestsPayload" $ \o -> RunRuleTestsPayload
    <$> o .: "rule_version_id"
    <*> o .: "tenant_id"
    <*> o .:? "seed"

instance ToJSON RunRuleTestsPayload where
  toJSON RunRuleTestsPayload{..} = object
    [ "rule_version_id" .= rtpRuleVersionId
    , "tenant_id" .= rtpTenantId
    , "seed" .= rtpSeed
    ]

-- | Test result.
--
-- @since 0.1.0.0
data TestResult = TestResult
  { trName :: Text
  , trPassed :: Bool
  , trInput :: Value
  , trExpected :: Value
  , trActual :: Value
  , trError :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TestResult where
  toJSON TestResult{..} = object
    [ "name" .= trName
    , "passed" .= trPassed
    , "input" .= trInput
    , "expected" .= trExpected
    , "actual" .= trActual
    , "error" .= trError
    ]

-- | Handle RUN_RULE_TESTS job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe RunRuleTestsPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid RUN_RULE_TESTS payload"
    Just payload -> withResource pool $ \conn -> do
      -- Get rule version
      mRuleVer <- DB.getRuleVersion conn (rtpTenantId payload) (rtpRuleVersionId payload)
      case mRuleVer of
        Nothing -> pure $ HRFailure $ HENotFound "Rule version not found"
        Just ruleVer -> do
          -- Parse test suite from rule version
          let testSuite = DB.rvTestSuite ruleVer
          let rules = DB.rvRules ruleVer

          -- Generate seed
          let seed = maybe 42 id (rtpSeed payload)

          -- Run tests
          startTime <- getCurrentTime
          results <- runTestSuite seed rules testSuite
          endTime <- getCurrentTime
          let duration = diffUTCTime endTime startTime

          -- Count results
          let passedCount = length $ filter trPassed results
          let failedCount = length results - passedCount
          let totalCount = length results

          -- Record test run
          testRunId <- UUID.nextRandom
          void $ DB.createTestRun conn (rtpTenantId payload) DB.TestRunInput
            { DB.triId = testRunId
            , DB.triRuleVersionId = rtpRuleVersionId payload
            , DB.triSeed = seed
            , DB.triPassedCount = passedCount
            , DB.triFailedCount = failedCount
            , DB.triResults = LBS.toStrict $ Aeson.encode results
            , DB.triDurationMs = round (duration * 1000)
            }

          -- Update rule version status
          let newStatus = if passedCount >= minPassingCases && failedCount == 0
                          then DB.RuleVersionValidated
                          else DB.RuleVersionDraft

          DB.updateRuleVersionStatus conn (rtpRuleVersionId payload) newStatus

          -- Emit audit event
          now <- getCurrentTime
          void $ DB.appendEvent conn (rtpTenantId payload) DB.AppendEventInput
            { DB.aeiAggregateType = "RulePackageVersion"
            , DB.aeiAggregateId = rtpRuleVersionId payload
            , DB.aeiEventType = "TESTS_RUN"
            , DB.aeiEventData = Aeson.encode $ object
                [ "rule_version_id" .= rtpRuleVersionId payload
                , "test_run_id" .= testRunId
                , "passed_count" .= passedCount
                , "failed_count" .= failedCount
                , "total_count" .= totalCount
                , "status" .= show newStatus
                , "seed" .= seed
                ]
            , DB.aeiActorId = Nothing
            }

          pure HRSuccess

-- | Run test suite.
--
-- @since 0.1.0.0
runTestSuite :: Int -> Value -> Value -> IO [TestResult]
runTestSuite seed rules testSuiteJson = do
  -- MVP: Simple test execution
  -- Real implementation would:
  -- 1. Parse test cases from JSON
  -- 2. Generate property-based test cases with seed
  -- 3. Execute each test against rules
  -- 4. Collect results
  case Aeson.fromJSON testSuiteJson of
    Aeson.Error _ -> pure []
    Aeson.Success testCases -> forM testCases $ \tc -> do
      runSingleTest rules tc

-- | Run a single test case.
--
-- @since 0.1.0.0
runSingleTest :: Value -> Value -> IO TestResult
runSingleTest rules testCase = do
  -- MVP: Placeholder test execution
  let name = getTextField testCase "name" "unnamed"
  let input = getField testCase "input"
  let expected = getField testCase "expected"

  -- Execute rule against input
  case Evaluator.evaluate rules input of
    Left err -> pure TestResult
      { trName = name
      , trPassed = False
      , trInput = input
      , trExpected = expected
      , trActual = Aeson.Null
      , trError = Just $ T.pack err
      }
    Right actual -> pure TestResult
      { trName = name
      , trPassed = actual == expected
      , trInput = input
      , trExpected = expected
      , trActual = actual
      , trError = Nothing
      }

-- | Get text field from JSON.
--
-- @since 0.1.0.0
getTextField :: Value -> Text -> Text -> Text
getTextField (Aeson.Object obj) key def =
  case Aeson.lookup key obj of
    Just (Aeson.String s) -> s
    _ -> def
getTextField _ _ def = def

-- | Get field from JSON.
--
-- @since 0.1.0.0
getField :: Value -> Text -> Value
getField (Aeson.Object obj) key =
  maybe Aeson.Null id $ Aeson.lookup key obj
getField _ _ = Aeson.Null
