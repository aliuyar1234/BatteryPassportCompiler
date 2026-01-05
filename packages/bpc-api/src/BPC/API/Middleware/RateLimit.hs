{-# LANGUAGE OverloadedStrings #-}

-- | Rate Limit Middleware
--
-- Token bucket rate limiting per BPC-RL-1.
--
-- @since 0.1.0.0
module BPC.API.Middleware.RateLimit
  ( -- * Types
    RateLimitConfig(..)
    -- * Middleware
  , rateLimitMiddleware
  , checkRateLimit
    -- * Utilities
  , hashApiKey
  ) where

import Control.Exception (try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (SHA256(..), hash, Digest)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection)
import Network.HTTP.Types (status429, hContentType, Header)
import Network.Wai (Application, Middleware, Request, Response, responseLBS)
import qualified Network.Wai as Wai
import GHC.Generics (Generic)

import BPC.API.App (AppM, Env(..), withPool)
import BPC.API.Error (AppError(..))
import BPC.API.Types (AuthContext(..))

-- | Rate limit configuration.
data RateLimitConfig = RateLimitConfig
  { rlcEnabled :: Bool
  -- ^ Enable rate limiting
  , rlcDefaultCapacity :: Double
  -- ^ Default bucket capacity
  , rlcDefaultRefillPerSecond :: Double
  -- ^ Default refill rate
  , rlcFailOpen :: Bool
  -- ^ Allow requests on DB errors (for availability)
  }
  deriving stock (Show, Eq, Generic)

-- | Default rate limit configuration.
defaultRateLimitConfig :: RateLimitConfig
defaultRateLimitConfig = RateLimitConfig
  { rlcEnabled = True
  , rlcDefaultCapacity = 100
  , rlcDefaultRefillPerSecond = 10
  , rlcFailOpen = True
  }

-- | Rate limit middleware.
--
-- Applies token bucket rate limiting per API key.
--
-- @since 0.1.0.0
rateLimitMiddleware :: RateLimitConfig -> Pool Connection -> Middleware
rateLimitMiddleware config pool app req respond = do
  -- Stubbed out - rate limiting repository not yet implemented
  -- TODO: Implement when BPC.DB.Repos.RateLimits is available
  app req respond

-- | Check rate limit in AppM monad.
--
-- @since 0.1.0.0
checkRateLimit :: AuthContext -> Text -> AppM ()
checkRateLimit ctx apiKey = do
  -- Stubbed out - rate limiting repository not yet implemented
  -- TODO: Implement when BPC.DB.Repos.RateLimits is available
  pure ()

-- | Extract API key from request.
--
-- @since 0.1.0.0
extractApiKey :: Request -> Maybe Text
extractApiKey req =
  let headers = Wai.requestHeaders req
      authHeader = lookup (CI.mk "Authorization") headers
  in case authHeader of
       Just h | "Bearer " `BS.isPrefixOf` h ->
         Just $ TE.decodeUtf8 $ BS.drop 7 h
       _ -> Nothing

-- | Hash API key for storage.
--
-- @since 0.1.0.0
hashApiKey :: Text -> Text
hashApiKey key =
  let bs = TE.encodeUtf8 key
      digest :: Digest SHA256
      digest = hash bs
  in T.pack $ show digest

-- | Rate limit response (429 Too Many Requests).
--
-- @since 0.1.0.0
rateLimitResponse :: Int -> Response
rateLimitResponse retryAfter =
  responseLBS
    status429
    [ (hContentType, "application/json")
    , (CI.mk "Retry-After", BS8.pack $ show retryAfter)
    ]
    "{\"error\":{\"code\":\"RATE_LIMITED\",\"message\":\"Too many requests\"}}"
