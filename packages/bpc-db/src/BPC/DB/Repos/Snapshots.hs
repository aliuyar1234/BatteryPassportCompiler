{-# LANGUAGE OverloadedStrings #-}

-- | Snapshot Repository
--
-- Create snapshots, add fact items, and seal with BPC-SNAPSHOT-1 deterministic hash.
-- Sealed snapshots are immutable.
--
-- == BPC-SNAPSHOT-1 Hash Calculation
--
-- Facts are sorted by (fact_type, fact_key, payload_hash) for determinism.
-- Hash = SHA256(version || tenant_id || sorted_fact_hashes)
--
-- @since 0.1.0.0
module BPC.DB.Repos.Snapshots
  ( -- * Types
    SnapshotId
  , SnapshotStatus(..)
  , Snapshot(..)
  , SnapshotItem(..)
    -- * Operations
  , createSnapshot
  , getSnapshot
  , listSnapshots
  , addItem
  , removeItem
  , getItems
  , sealSnapshot
  , getSnapshotByHash
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Error (SealError(..), SnapshotError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Snapshot ID (UUID)
type SnapshotId = UUID

-- | Snapshot status enum.
data SnapshotStatus
  = SnapBuilding
  | SnapReady
  | SnapSealed
  deriving stock (Show, Eq, Generic)

statusToText :: SnapshotStatus -> Text
statusToText = \case
  SnapBuilding -> "BUILDING"
  SnapReady    -> "READY"
  SnapSealed   -> "SEALED"

textToStatus :: Text -> Maybe SnapshotStatus
textToStatus = \case
  "BUILDING" -> Just SnapBuilding
  "READY"    -> Just SnapReady
  "SEALED"   -> Just SnapSealed
  _          -> Nothing

-- | Snapshot record.
data Snapshot = Snapshot
  { snapId :: SnapshotId
  , snapTenantId :: TenantId
  , snapStatus :: SnapshotStatus
  , snapHash :: Maybe Text
  , snapDescription :: Maybe Text
  , snapCreatedAt :: UTCTime
  , snapSealedAt :: Maybe UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Snapshot item (fact reference).
data SnapshotItem = SnapshotItem
  { siSnapshotId :: SnapshotId
  , siFactId :: UUID
  , siFactType :: Text
  , siFactKey :: Text
  , siPayloadHash :: Text
  , siAddedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Calculate SHA-256 hash of bytes.
sha256Hex :: BS.ByteString -> Text
sha256Hex content =
  let digest = hashWith SHA256 content
      hexBytes = convert digest :: BS.ByteString
  in TE.decodeUtf8 $ BS.concatMap toHex hexBytes
  where
    toHex :: Word8 -> BS.ByteString
    toHex w =
      let (hi, lo) = w `divMod` 16
      in BS.pack [hexChar hi, hexChar lo]

    hexChar :: Word8 -> Word8
    hexChar n
      | n < 10    = 48 + n
      | otherwise = 87 + n

-- | Create a new snapshot with status BUILDING.
--
-- @since 0.1.0.0
createSnapshot
  :: Connection
  -> TenantId
  -> Maybe Text
  -> IO SnapshotId
createSnapshot conn tenantId description = do
  [Only snapId] <- PG.query conn
    "INSERT INTO snapshots (tenant_id, status, description) \
    \VALUES (?, 'BUILDING', ?) RETURNING id"
    (tenantId, description)
  pure snapId

-- | Get a snapshot by ID.
getSnapshot
  :: Connection
  -> TenantId
  -> SnapshotId
  -> IO (Maybe Snapshot)
getSnapshot conn tenantId snapId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, status, hash, description, created_at, sealed_at \
    \FROM snapshots WHERE tenant_id = ? AND id = ?"
    (tenantId, snapId)
  pure $ case rows of
    [] -> Nothing
    ((sid, tid, status, hash, desc, cat, sat) : _) ->
      Just Snapshot
        { snapId = sid
        , snapTenantId = tid
        , snapStatus = maybe SnapBuilding id $ textToStatus status
        , snapHash = hash
        , snapDescription = desc
        , snapCreatedAt = cat
        , snapSealedAt = sat
        }

-- | List snapshots with cursor pagination.
--
-- Fetches snapshots ordered by (created_at DESC, id DESC).
-- If cursor is provided (timestamp, id), returns snapshots created before that point.
--
-- @since 0.1.0.0
listSnapshots
  :: Connection
  -> TenantId
  -> Int
  -- ^ Limit (number of items to fetch)
  -> Maybe (UTCTime, UUID)
  -- ^ Optional cursor (timestamp, id)
  -> IO [Snapshot]
listSnapshots conn tenantId limit cursor = do
  rows <- case cursor of
    Nothing ->
      PG.query conn
        "SELECT id, tenant_id, status, hash, description, created_at, sealed_at \
        \FROM snapshots \
        \WHERE tenant_id = ? \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, limit)
    Just (timestamp, cursorId) ->
      PG.query conn
        "SELECT id, tenant_id, status, hash, description, created_at, sealed_at \
        \FROM snapshots \
        \WHERE tenant_id = ? AND (created_at, id) < (?, ?) \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, timestamp, cursorId, limit)

  pure $ map toSnapshot rows
  where
    toSnapshot (sid, tid, status, hash, desc, cat, sat) =
      Snapshot
        { snapId = sid
        , snapTenantId = tid
        , snapStatus = maybe SnapBuilding id $ textToStatus status
        , snapHash = hash
        , snapDescription = desc
        , snapCreatedAt = cat
        , snapSealedAt = sat
        }

-- | Add a fact to a snapshot.
--
-- Returns SNAPSHOT_SEALED if the snapshot is already sealed.
--
-- @since 0.1.0.0
addItem
  :: Connection
  -> TenantId
  -> SnapshotId
  -> UUID
  -> IO (Either SnapshotError ())
addItem conn tenantId snapId factId = do
  -- Check snapshot status
  mSnap <- getSnapshot conn tenantId snapId
  case mSnap of
    Nothing -> pure $ Left $ SNAPSHOT_NOT_FOUND snapId
    Just snap
      | snapStatus snap == SnapSealed -> pure $ Left $ SNAPSHOT_SEALED snapId
      | otherwise -> do
          _ <- PG.execute conn
            "INSERT INTO snapshot_items (snapshot_id, fact_id) VALUES (?, ?) \
            \ON CONFLICT DO NOTHING"
            (snapId, factId)
          pure $ Right ()

-- | Remove a fact from a snapshot.
--
-- Returns SNAPSHOT_SEALED if the snapshot is already sealed.
removeItem
  :: Connection
  -> TenantId
  -> SnapshotId
  -> UUID
  -> IO (Either SnapshotError ())
removeItem conn tenantId snapId factId = do
  mSnap <- getSnapshot conn tenantId snapId
  case mSnap of
    Nothing -> pure $ Left $ SNAPSHOT_NOT_FOUND snapId
    Just snap
      | snapStatus snap == SnapSealed -> pure $ Left $ SNAPSHOT_SEALED snapId
      | otherwise -> do
          _ <- PG.execute conn
            "DELETE FROM snapshot_items WHERE snapshot_id = ? AND fact_id = ?"
            (snapId, factId)
          pure $ Right ()

-- | Get all items in a snapshot.
getItems
  :: Connection
  -> TenantId
  -> SnapshotId
  -> IO [SnapshotItem]
getItems conn tenantId snapId = do
  rows <- PG.query conn
    "SELECT si.snapshot_id, si.fact_id, f.fact_type, f.fact_key, \
    \       f.payload_hash, si.added_at \
    \FROM snapshot_items si \
    \INNER JOIN facts f ON f.id = si.fact_id \
    \WHERE f.tenant_id = ? AND si.snapshot_id = ? \
    \ORDER BY f.fact_type, f.fact_key, f.payload_hash"
    (tenantId, snapId)
  pure $ map toItem rows
  where
    toItem (sid, fid, ftype, fkey, phash, added) = SnapshotItem
      { siSnapshotId = sid
      , siFactId = fid
      , siFactType = ftype
      , siFactKey = fkey
      , siPayloadHash = phash
      , siAddedAt = added
      }

-- | Seal a snapshot with BPC-SNAPSHOT-1 hash.
--
-- Calculates deterministic hash from sorted fact hashes.
-- Returns SealSnapshotEmpty if no items.
--
-- @since 0.1.0.0
sealSnapshot
  :: Connection
  -> TenantId
  -> SnapshotId
  -> IO (Either SealError Text)
sealSnapshot conn tenantId snapId = do
  mSnap <- getSnapshot conn tenantId snapId
  case mSnap of
    Nothing -> pure $ Left $ SealSnapshotNotFound snapId
    Just snap
      | snapStatus snap == SnapSealed -> pure $ Left $ SealSnapshotAlreadySealed snapId
      | snapStatus snap /= SnapBuilding -> pure $ Left $ SealSnapshotNotBuilding snapId
      | otherwise -> do
          items <- getItems conn tenantId snapId
          if null items
            then pure $ Left $ SealSnapshotEmpty snapId
            else do
              let hash = calculateSnapshotHash tenantId items
              _ <- PG.execute conn
                "UPDATE snapshots SET status = 'SEALED', hash = ?, sealed_at = NOW() \
                \WHERE tenant_id = ? AND id = ?"
                (hash, tenantId, snapId)
              pure $ Right hash

-- | Calculate BPC-SNAPSHOT-1 hash.
--
-- Format: SHA256("BPC-SNAPSHOT-1" || "|" || tenant_id || "|" || sorted_hashes)
-- Facts sorted by (fact_type, fact_key, payload_hash)
calculateSnapshotHash :: TenantId -> [SnapshotItem] -> Text
calculateSnapshotHash tenantId items =
  let -- Sort by (fact_type, fact_key, payload_hash)
      sorted = sortOn (\i -> (siFactType i, siFactKey i, siPayloadHash i)) items
      -- Concatenate payload hashes
      hashes = map siPayloadHash sorted
      hashesConcat = T.intercalate "," hashes
      -- Build input
      input = T.intercalate "|"
        [ "BPC-SNAPSHOT-1"
        , UUID.toText tenantId
        , hashesConcat
        ]
  in sha256Hex $ TE.encodeUtf8 input

-- | Get a snapshot by its hash.
getSnapshotByHash
  :: Connection
  -> TenantId
  -> Text
  -> IO (Maybe Snapshot)
getSnapshotByHash conn tenantId hash = do
  rows <- PG.query conn
    "SELECT id, tenant_id, status, hash, description, created_at, sealed_at \
    \FROM snapshots WHERE tenant_id = ? AND hash = ?"
    (tenantId, hash)
  pure $ case rows of
    [] -> Nothing
    ((sid, tid, status, h, desc, cat, sat) : _) ->
      Just Snapshot
        { snapId = sid
        , snapTenantId = tid
        , snapStatus = maybe SnapBuilding id $ textToStatus status
        , snapHash = h
        , snapDescription = desc
        , snapCreatedAt = cat
        , snapSealedAt = sat
        }
