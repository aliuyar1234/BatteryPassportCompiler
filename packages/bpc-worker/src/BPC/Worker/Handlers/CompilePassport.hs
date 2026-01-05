{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Compile Passport Handler
--
-- Compiles passport using pure compilePassportPure function.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.CompilePassport
  ( handle
  , CompilePassportPayload(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import Data.Pool (Pool, withResource)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for COMPILE_PASSPORT job.
--
-- @since 0.1.0.0
data CompilePassportPayload = CompilePassportPayload
  { cppPassportId :: UUID
  , cppTenantId :: UUID
  , cppSnapshotId :: UUID
  , cppRuleVersionId :: UUID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON CompilePassportPayload where
  parseJSON = Aeson.withObject "CompilePassportPayload" $ \o -> CompilePassportPayload
    <$> o .: "passport_id"
    <*> o .: "tenant_id"
    <*> o .: "snapshot_id"
    <*> o .: "rule_version_id"

instance ToJSON CompilePassportPayload where
  toJSON CompilePassportPayload{..} = object
    [ "passport_id" .= cppPassportId
    , "tenant_id" .= cppTenantId
    , "snapshot_id" .= cppSnapshotId
    , "rule_version_id" .= cppRuleVersionId
    ]

-- | Handle COMPILE_PASSPORT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid COMPILE_PASSPORT payload"
    Aeson.Success (_payload :: CompilePassportPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement passport compilation
      -- 1. Verify snapshot is SEALED
      -- 2. Load rules and facts
      -- 3. Call pure compilation function
      -- 4. Store passport version
      -- 5. Enqueue SIGN_PASSPORT job
      pure HRSuccess
