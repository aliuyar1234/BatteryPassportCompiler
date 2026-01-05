{-# LANGUAGE OverloadedStrings #-}

-- | Rate Limit Repository
--
-- Token bucket rate limiting per BPC-RL-1.
--
-- @since 0.1.0.0
module BPC.DB.Repos.RateLimits
  ( -- * Types
    RateLimitBucket(..)
  , RateLimitExceeded(..)
    -- * Operations
  , consumeToken
  , computeRetryAfter
  , getBucket
  , createBucket
    -- * Configuration
  , defaultCapacity
  , defaultRefillPerSecond
  ) where

import Control.Exception (Exception)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)

-- | Default bucket capacity.
defaultCapacity :: Double
defaultCapacity = 100

-- | Default refill rate (tokens per second).
defaultRefillPerSecond :: Double
defaultRefillPerSecond = 10

-- | Rate limit bucket.
data RateLimitBucket = RateLimitBucket
  { rlbId :: UUID
  , rlbTenantId :: UUID
  , rlbKeyHash :: Text
  , rlbTokens :: Double
  , rlbCapacity :: Double
  , rlbRefillPerSecond :: Double
  , rlbLastRefillAt :: UTCTime
  , rlbCreatedAt :: UTCTime
  , rlbUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromRow RateLimitBucket where
  fromRow = RateLimitBucket <$> field <*> field <*> field <*> field
    <*> field <*> field <*> field <*> field <*> field

-- | Rate limit exceeded exception.
data RateLimitExceeded = RateLimitExceeded
  { rleRetryAfterSeconds :: Int
  }
  deriving stock (Show, Eq, Generic)

instance Exception RateLimitExceeded

type TenantId = UUID

-- | Consume a token from the bucket.
--
-- Uses atomic SQL with FOR UPDATE to prevent race conditions.
-- Returns Right () if token consumed, Left RateLimitExceeded otherwise.
--
-- @since 0.1.0.0
consumeToken :: Connection -> TenantId -> Text -> IO (Either RateLimitExceeded ())
consumeToken conn tenantId keyHash = do
  now <- getCurrentTime

  -- Use FOR UPDATE to lock the row atomically
  results <- query conn
    "UPDATE rate_limit_buckets \
    \SET tokens = GREATEST(0, LEAST(capacity, tokens + \
    \  EXTRACT(EPOCH FROM (NOW() - last_refill_at)) * refill_per_second) - 1), \
    \  last_refill_at = NOW(), \
    \  updated_at = NOW() \
    \WHERE tenant_id = ? AND key_hash = ? \
    \RETURNING tokens >= 0"
    (tenantId, keyHash)

  case results of
    [Only True] -> pure $ Right ()
    [Only False] -> do
      retryAfter <- computeRetryAfter conn tenantId keyHash
      pure $ Left $ RateLimitExceeded retryAfter
    [] -> do
      -- Bucket doesn't exist, create it
      void $ createBucket conn tenantId keyHash
      -- Consume the first token
      consumeToken conn tenantId keyHash

-- | Compute retry-after seconds.
--
-- Returns the number of seconds until a token will be available.
--
-- @since 0.1.0.0
computeRetryAfter :: Connection -> TenantId -> Text -> IO Int
computeRetryAfter conn tenantId keyHash = do
  results <- query conn
    "SELECT tokens, refill_per_second FROM rate_limit_buckets \
    \WHERE tenant_id = ? AND key_hash = ?"
    (tenantId, keyHash)
  case results of
    [(tokens :: Double, refillRate :: Double)] ->
      if tokens >= 1
        then pure 0
        else pure $ ceiling $ (1 - tokens) / refillRate
    [] -> pure 0  -- No bucket means no limit

-- | Get a rate limit bucket.
--
-- @since 0.1.0.0
getBucket :: Connection -> TenantId -> Text -> IO (Maybe RateLimitBucket)
getBucket conn tenantId keyHash = do
  results <- query conn
    "SELECT id, tenant_id, key_hash, tokens, capacity, refill_per_second, \
    \last_refill_at, created_at, updated_at \
    \FROM rate_limit_buckets WHERE tenant_id = ? AND key_hash = ?"
    (tenantId, keyHash)
  pure $ case results of
    [b] -> Just b
    _ -> Nothing

-- | Create a new rate limit bucket.
--
-- @since 0.1.0.0
createBucket :: Connection -> TenantId -> Text -> IO UUID
createBucket conn tenantId keyHash = do
  [Only bucketId] <- query conn
    "INSERT INTO rate_limit_buckets (tenant_id, key_hash, tokens, capacity, refill_per_second) \
    \VALUES (?, ?, ?, ?, ?) \
    \ON CONFLICT (tenant_id, key_hash) DO UPDATE SET updated_at = NOW() \
    \RETURNING id"
    (tenantId, keyHash, defaultCapacity, defaultCapacity, defaultRefillPerSecond)
  pure bucketId
