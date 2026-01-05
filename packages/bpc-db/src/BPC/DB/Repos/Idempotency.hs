{-# LANGUAGE OverloadedStrings #-}

-- | Idempotency Repository
--
-- HTTP idempotency key storage per BPC-IDEMP-1.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Idempotency
  ( -- * Types
    IdempotencyEntry(..)
  , IdempotencyConflict(..)
    -- * Operations
  , lookupIdempotencyKey
  , storeIdempotencyKey
  , cleanupIdempotencyKeys
  ) where

import Control.Exception (Exception)
import Control.Monad (void)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)

-- | Idempotency entry.
data IdempotencyEntry = IdempotencyEntry
  { ieId :: UUID
  , ieTenantId :: UUID
  , ieKeyHash :: Text
  , ieRequestHash :: Text
  , ieResponseStatus :: Int
  , ieResponseBody :: BS.ByteString
  , ieResponseHeaders :: Value
  , ieCreatedAt :: UTCTime
  , ieExpiresAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromRow IdempotencyEntry where
  fromRow = IdempotencyEntry <$> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field

-- | Idempotency conflict (request hash mismatch).
data IdempotencyConflict = IdempotencyConflict
  { icKeyHash :: Text
  , icStoredRequestHash :: Text
  , icNewRequestHash :: Text
  }
  deriving stock (Show, Eq, Generic)

instance Exception IdempotencyConflict

type TenantId = UUID

-- | Look up an idempotency key.
--
-- Returns the stored entry if found and not expired.
--
-- @since 0.1.0.0
lookupIdempotencyKey :: Connection -> TenantId -> Text -> IO (Maybe IdempotencyEntry)
lookupIdempotencyKey conn tenantId keyHash = do
  results <- query conn
    "SELECT id, tenant_id, key_hash, request_hash, response_status, \
    \response_body, response_headers, created_at, expires_at \
    \FROM idempotency_keys \
    \WHERE tenant_id = ? AND key_hash = ? AND expires_at > NOW()"
    (tenantId, keyHash)
  pure $ case results of
    [entry] -> Just entry
    _ -> Nothing

-- | Store an idempotency key with response.
--
-- TTL is set to 24 hours from now via database default.
--
-- @since 0.1.0.0
storeIdempotencyKey
  :: Connection
  -> TenantId
  -> Text  -- ^ Key hash
  -> Text  -- ^ Request hash
  -> Int   -- ^ Response status
  -> BS.ByteString  -- ^ Response body
  -> Value  -- ^ Response headers
  -> IO ()
storeIdempotencyKey conn tenantId keyHash requestHash status body headers = do
  void $ execute conn
    "INSERT INTO idempotency_keys \
    \(tenant_id, key_hash, request_hash, response_status, response_body, response_headers) \
    \VALUES (?, ?, ?, ?, ?, ?) \
    \ON CONFLICT (tenant_id, key_hash) DO NOTHING"
    (tenantId, keyHash, requestHash, status, body, headers)

-- | Clean up expired idempotency keys.
--
-- Returns the number of keys deleted.
--
-- @since 0.1.0.0
cleanupIdempotencyKeys :: Connection -> IO Int
cleanupIdempotencyKeys conn = do
  rowCount <- execute conn
    "DELETE FROM idempotency_keys WHERE expires_at < NOW()"
    ()
  pure $ fromIntegral rowCount
