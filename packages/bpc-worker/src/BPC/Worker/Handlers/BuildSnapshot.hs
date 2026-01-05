{-# LANGUAGE OverloadedStrings #-}

-- | Build Snapshot Handler
--
-- Transitions snapshots from BUILDING to READY.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.BuildSnapshot
  ( handle
  , BuildSnapshotPayload(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for BUILD_SNAPSHOT job.
--
-- @since 0.1.0.0
data BuildSnapshotPayload = BuildSnapshotPayload
  { bspSnapshotId :: UUID
  , bspTenantId :: UUID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BuildSnapshotPayload where
  parseJSON = Aeson.withObject "BuildSnapshotPayload" $ \o -> BuildSnapshotPayload
    <$> o .: "snapshot_id"
    <*> o .: "tenant_id"

instance ToJSON BuildSnapshotPayload where
  toJSON BuildSnapshotPayload{..} = object
    [ "snapshot_id" .= bspSnapshotId
    , "tenant_id" .= bspTenantId
    ]

-- | Handle BUILD_SNAPSHOT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid BUILD_SNAPSHOT payload"
    Aeson.Success (_payload :: BuildSnapshotPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement snapshot building
      -- 1. Get snapshot
      -- 2. Check status is BUILDING
      -- 3. Verify all facts are present
      -- 4. Transition to READY
      pure HRSuccess
