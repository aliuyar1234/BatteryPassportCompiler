{-# LANGUAGE OverloadedStrings #-}

-- | Retention Handler
--
-- Deletes expired data per configured retention periods.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.Retention
  ( -- * Configuration
    RetentionConfig(..)
  , defaultRetentionConfig
    -- * Operations
  , runRetention
  , deleteEventsOlderThan
  , deletePassportVersionsOlderThan
  , deleteDocumentVersionsOlderThan
  ) where

import Control.Monad (void, when)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime, nominalDay)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection, Only(..), execute, query)
import GHC.Generics (Generic)

import qualified BPC.DB as DB

-- | Retention configuration.
data RetentionConfig = RetentionConfig
  { rcEventsRetentionYears :: Int
  -- ^ Event retention period (default: 15 years per SSOT)
  , rcPassportVersionsRetentionYears :: Int
  -- ^ Passport version retention (default: 15 years)
  , rcDocumentVersionsRetentionYears :: Int
  -- ^ Document version retention (default: 10 years)
  , rcIdempotencyRetentionHours :: Int
  -- ^ Idempotency key retention (default: 24 hours)
  }
  deriving stock (Show, Eq, Generic)

-- | Default retention configuration per SSOT.
defaultRetentionConfig :: RetentionConfig
defaultRetentionConfig = RetentionConfig
  { rcEventsRetentionYears = 15
  , rcPassportVersionsRetentionYears = 15
  , rcDocumentVersionsRetentionYears = 10
  , rcIdempotencyRetentionHours = 24
  }

-- | Run retention process.
--
-- Deletes expired data and emits audit event with results.
--
-- @since 0.1.0.0
runRetention :: RetentionConfig -> Pool Connection -> IO ()
runRetention config pool = withResource pool $ \conn -> do
  now <- getCurrentTime

  -- Calculate cutoff dates
  let eventsCutoff = addYears (- rcEventsRetentionYears config) now
  let passportCutoff = addYears (- rcPassportVersionsRetentionYears config) now
  let documentCutoff = addYears (- rcDocumentVersionsRetentionYears config) now

  -- Delete expired data
  eventsDeleted <- deleteEventsOlderThan conn eventsCutoff
  passportsDeleted <- deletePassportVersionsOlderThan conn passportCutoff
  documentsDeleted <- deleteDocumentVersionsOlderThan conn documentCutoff

  -- Cleanup idempotency keys
  idempotencyDeleted <- cleanupIdempotencyKeys conn

  -- Emit audit event
  emitRetentionEvent conn now eventsDeleted passportsDeleted documentsDeleted idempotencyDeleted

  putStrLn $ "Retention completed: "
    ++ show eventsDeleted ++ " events, "
    ++ show passportsDeleted ++ " passport versions, "
    ++ show documentsDeleted ++ " document versions, "
    ++ show idempotencyDeleted ++ " idempotency keys deleted"

-- | Delete events older than cutoff.
--
-- @since 0.1.0.0
deleteEventsOlderThan :: Connection -> UTCTime -> IO Int
deleteEventsOlderThan conn cutoff = do
  rowCount <- execute conn
    "DELETE FROM audit_events WHERE created_at < ?"
    (Only cutoff)
  pure $ fromIntegral rowCount

-- | Delete passport versions older than cutoff.
--
-- @since 0.1.0.0
deletePassportVersionsOlderThan :: Connection -> UTCTime -> IO Int
deletePassportVersionsOlderThan conn cutoff = do
  rowCount <- execute conn
    "DELETE FROM passport_versions WHERE created_at < ?"
    (Only cutoff)
  pure $ fromIntegral rowCount

-- | Delete document versions older than cutoff.
--
-- @since 0.1.0.0
deleteDocumentVersionsOlderThan :: Connection -> UTCTime -> IO Int
deleteDocumentVersionsOlderThan conn cutoff = do
  rowCount <- execute conn
    "DELETE FROM document_versions WHERE created_at < ?"
    (Only cutoff)
  pure $ fromIntegral rowCount

-- | Cleanup expired idempotency keys.
--
-- @since 0.1.0.0
cleanupIdempotencyKeys :: Connection -> IO Int
cleanupIdempotencyKeys conn = do
  rowCount <- execute conn
    "DELETE FROM idempotency_keys WHERE expires_at < NOW()"
    ()
  pure $ fromIntegral rowCount

-- | Emit retention applied event.
--
-- @since 0.1.0.0
emitRetentionEvent :: Connection -> UTCTime -> Int -> Int -> Int -> Int -> IO ()
emitRetentionEvent conn timestamp eventsDeleted passportsDeleted documentsDeleted idempotencyDeleted = do
  -- Use system tenant for retention events
  let systemTenantId = read "00000000-0000-0000-0000-000000000000" :: UUID
  eventId <- UUID.nextRandom

  void $ execute conn
    "INSERT INTO audit_events (id, tenant_id, aggregate_type, aggregate_id, event_type, event_data, created_at) \
    \VALUES (?, ?, 'System', ?, 'RETENTION_APPLIED', ?, ?)"
    ( eventId
    , systemTenantId
    , eventId  -- Self-referential for system events
    , Aeson.encode $ object
        [ "events_deleted" .= eventsDeleted
        , "passport_versions_deleted" .= passportsDeleted
        , "document_versions_deleted" .= documentsDeleted
        , "idempotency_keys_deleted" .= idempotencyDeleted
        , "executed_at" .= timestamp
        ]
    , timestamp
    )

-- | Add years to a UTCTime.
addYears :: Int -> UTCTime -> UTCTime
addYears years t = addUTCTime (fromIntegral years * 365 * nominalDay) t
