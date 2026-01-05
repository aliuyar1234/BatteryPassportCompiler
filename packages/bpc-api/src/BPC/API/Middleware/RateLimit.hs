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
import qualified BPC.DB.Repos.RateLimits as RL

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
  if not (rlcEnabled config)
    then app req respond
    else do
      -- Extract API key from request
      let mApiKey = extractApiKey req
      case mApiKey of
        Nothing -> app req respond  -- No API key, skip rate limiting
        Just apiKey -> do
          let keyHash = hashApiKey apiKey
          -- Try to consume a token
          result <- try $ withResource pool $ \conn ->
            RL.consumeToken conn tenantId keyHash
          case result of
            Left (ex :: IOError) ->
              if rlcFailOpen config
                then app req respond  -- Fail open
                else respond $ rateLimitResponse 60  -- Fail closed with 60s retry
            Right (Left rl) ->
              respond $ rateLimitResponse (RL.rleRetryAfterSeconds rl)
            Right (Right ()) ->
              app req respond
  where
    -- MVP: Use nil UUID for tenant
    tenantId = read "00000000-0000-0000-0000-000000000000"

-- | Check rate limit in AppM monad.
--
-- @since 0.1.0.0
checkRateLimit :: AuthContext -> Text -> AppM ()
checkRateLimit ctx apiKey = do
  let keyHash = hashApiKey apiKey
  result <- withPool $ \conn -> RL.consumeToken conn (acTenantId ctx) keyHash
  case result of
    Left rl -> throwError RateLimited
    Right () -> pure ()

-- | Extract API key from request.
--
-- @since 0.1.0.0
extractApiKey :: Request -> Maybe Text
extractApiKey req =
  let headers = Wai.requestHeaders req
      authHeader = lookup "Authorization" headers
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
    , ("Retry-After", BS8.pack $ show retryAfter)
    ]
    "{\"error\":{\"code\":\"RATE_LIMITED\",\"message\":\"Too many requests\"}}"
