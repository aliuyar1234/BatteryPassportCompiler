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

import Control.Monad (void, when)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
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
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe BuildSnapshotPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid BUILD_SNAPSHOT payload"
    Just payload -> withResource pool $ \conn -> do
      -- Get snapshot
      mSnapshot <- DB.getSnapshot conn (bspTenantId payload) (bspSnapshotId payload)
      case mSnapshot of
        Nothing ->
          pure $ HRFailure $ HENotFound "Snapshot not found"
        Just snapshot -> do
          -- Check status
          case DB.snapshotStatus snapshot of
            DB.SnapshotBuilding -> do
              -- Transition to READY
              DB.updateSnapshotStatus conn (bspSnapshotId payload) DB.SnapshotReady

              -- Emit audit event
              now <- getCurrentTime
              void $ DB.appendEvent conn (bspTenantId payload) DB.AppendEventInput
                { DB.aeiAggregateType = "Snapshot"
                , DB.aeiAggregateId = bspSnapshotId payload
                , DB.aeiEventType = "SNAPSHOT_READY"
                , DB.aeiEventData = Aeson.encode $ object
                    [ "snapshot_id" .= bspSnapshotId payload
                    ]
                , DB.aeiActorId = Nothing
                }

              pure HRSuccess

            DB.SnapshotReady ->
              pure $ HRFailure $ HEPrecondition "Snapshot already READY"

            DB.SnapshotSealed ->
              pure $ HRFailure $ HEPrecondition "Snapshot already SEALED"
