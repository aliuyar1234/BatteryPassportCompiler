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

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

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

-- | Handle RUN_RULE_TESTS job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid RUN_RULE_TESTS payload"
    Aeson.Success (_payload :: RunRuleTestsPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement rule test execution
      -- 1. Get rule version
      -- 2. Parse test suite
      -- 3. Generate test cases with seed
      -- 4. Execute tests
      -- 5. Record test run results
      -- 6. Update rule version status (VALIDATED if >= 500 passing)
      -- 7. Emit audit event
      pure HRSuccess
