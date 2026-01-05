{-# LANGUAGE OverloadedStrings #-}

-- | Fact Repository
--
-- Store facts with canonical JSON encoding and SHA-256 payload hash.
-- Facts are immutable and uniquely identified by (tenant_id, fact_type, fact_key).
--
-- @since 0.1.0.0
module BPC.DB.Repos.Facts
  ( -- * Types
    FactId
  , FactInput(..)
  , Fact(..)
    -- * Operations
  , insertFact
  , getFact
  , getFactById
  , getFactsByPrefix
  , getFactsByType
  , getFactsBySnapshot
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import Data.Word (Word8)

import BPC.DB.Error (FactError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Fact ID (UUID)
type FactId = UUID

-- | Input for inserting a new fact.
data FactInput = FactInput
  { fiFactType :: Text
  -- ^ Fact type (e.g., "Battery", "Manufacturer")
  , fiFactKey :: Text
  -- ^ Fact key (e.g., "battery:SKU-123")
  , fiPayload :: Value
  -- ^ Fact payload (JSON object)
  , fiSourceVersionId :: Maybe UUID
  -- ^ Optional source document version ID
  }
  deriving stock (Show, Eq, Generic)

-- | Stored fact record.
data Fact = Fact
  { factId :: FactId
  , factTenantId :: TenantId
  , factType :: Text
  , factKey :: Text
  , factPayload :: Value
  , factPayloadCanonical :: BS.ByteString
  , factPayloadHash :: Text
  , factSourceVersionId :: Maybe UUID
  , factCreatedAt :: UTCTime
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

-- | Canonicalize JSON value to bytes.
--
-- Uses Aeson's compact encoding (sorted keys, no whitespace).
canonicalEncode :: Value -> BS.ByteString
canonicalEncode = LBS.toStrict . Aeson.encode

-- | Insert a new fact.
--
-- Calculates canonical payload and hash. Returns FACT_DUPLICATE_HASH
-- if the same (fact_type, fact_key, payload_hash) exists.
--
-- @since 0.1.0.0
insertFact
  :: Connection
  -> TenantId
  -> FactInput
  -> IO (Either FactError FactId)
insertFact conn tenantId input = do
  -- Canonicalize payload
  let payloadCanonical = canonicalEncode (fiPayload input)
  let payloadHash = sha256Hex payloadCanonical

  result <- (Right <$> PG.query conn
    "INSERT INTO facts \
    \(tenant_id, fact_type, fact_key, payload, payload_canonical, \
    \ payload_hash, source_version_id) \
    \VALUES (?, ?, ?, ?, ?, ?, ?) \
    \RETURNING id"
    ( tenantId
    , fiFactType input
    , fiFactKey input
    , fiPayload input
    , PG.Binary payloadCanonical
    , payloadHash
    , fiSourceVersionId input
    )) `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ FACT_NOT_FOUND (error "No ID returned")
    Right (Only fid : _) -> pure $ Right fid
  where
    handleSqlError :: SqlError -> IO (Either FactError [Only UUID])
    handleSqlError _ = pure $ Left $ FACT_DUPLICATE_HASH "Constraint violation"

-- | Get a fact by type and key.
--
-- Returns the most recent fact matching (fact_type, fact_key).
--
-- @since 0.1.0.0
getFact
  :: Connection
  -> TenantId
  -> Text
  -> Text
  -> IO (Maybe Fact)
getFact conn tenantId factType factKey = do
  rows <- PG.query conn
    "SELECT id, tenant_id, fact_type, fact_key, payload, payload_canonical, \
    \       payload_hash, source_version_id, created_at \
    \FROM facts \
    \WHERE tenant_id = ? AND fact_type = ? AND fact_key = ? \
    \ORDER BY created_at DESC LIMIT 1"
    (tenantId, factType, factKey)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToFact row

-- | Get a fact by ID.
getFactById
  :: Connection
  -> TenantId
  -> FactId
  -> IO (Maybe Fact)
getFactById conn tenantId factId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, fact_type, fact_key, payload, payload_canonical, \
    \       payload_hash, source_version_id, created_at \
    \FROM facts WHERE tenant_id = ? AND id = ?"
    (tenantId, factId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToFact row

-- | Get facts by type and key prefix.
--
-- Matches facts where fact_key starts with the given prefix.
--
-- @since 0.1.0.0
getFactsByPrefix
  :: Connection
  -> TenantId
  -> Text
  -> Text
  -> IO [Fact]
getFactsByPrefix conn tenantId factType keyPrefix = do
  rows <- PG.query conn
    "SELECT id, tenant_id, fact_type, fact_key, payload, payload_canonical, \
    \       payload_hash, source_version_id, created_at \
    \FROM facts \
    \WHERE tenant_id = ? AND fact_type = ? AND fact_key LIKE ? \
    \ORDER BY fact_key ASC"
    (tenantId, factType, keyPrefix <> "%")
  pure $ map rowToFact rows

-- | Get all facts of a given type.
getFactsByType
  :: Connection
  -> TenantId
  -> Text
  -> IO [Fact]
getFactsByType conn tenantId factType = do
  rows <- PG.query conn
    "SELECT id, tenant_id, fact_type, fact_key, payload, payload_canonical, \
    \       payload_hash, source_version_id, created_at \
    \FROM facts \
    \WHERE tenant_id = ? AND fact_type = ? \
    \ORDER BY fact_key ASC"
    (tenantId, factType)
  pure $ map rowToFact rows

-- | Get all facts in a snapshot.
getFactsBySnapshot
  :: Connection
  -> TenantId
  -> UUID
  -> IO [Fact]
getFactsBySnapshot conn tenantId snapshotId = do
  rows <- PG.query conn
    "SELECT f.id, f.tenant_id, f.fact_type, f.fact_key, f.payload, \
    \       f.payload_canonical, f.payload_hash, f.source_version_id, f.created_at \
    \FROM facts f \
    \INNER JOIN snapshot_items si ON si.fact_id = f.id \
    \WHERE f.tenant_id = ? AND si.snapshot_id = ? \
    \ORDER BY f.fact_type, f.fact_key, f.payload_hash"
    (tenantId, snapshotId)
  pure $ map rowToFact rows

-- | Convert database row to Fact.
rowToFact
  :: (UUID, UUID, Text, Text, Value, PG.Binary BS.ByteString, Text, Maybe UUID, UTCTime)
  -> Fact
rowToFact (fid, tid, ftype, fkey, payload, PG.Binary canonical, phash, srcId, cat) = Fact
  { factId = fid
  , factTenantId = tid
  , factType = ftype
  , factKey = fkey
  , factPayload = payload
  , factPayloadCanonical = canonical
  , factPayloadHash = phash
  , factSourceVersionId = srcId
  , factCreatedAt = cat
  }
