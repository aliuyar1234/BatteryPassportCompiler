{-# LANGUAGE OverloadedStrings #-}

-- | Correlation ID Middleware
--
-- Tracks requests across logs with X-Correlation-Id header.
-- Generates UUID if not provided in request.
--
-- @since 0.1.0.0
module BPC.API.Middleware.CorrelationId
  ( -- * Middleware
    correlationMiddleware
    -- * Types
  , CorrelationId(..)
    -- * Extraction
  , getCorrelationId
  , generateCorrelationId
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Middleware, Request, requestHeaders, mapResponseHeaders)

-- | Correlation ID newtype.
newtype CorrelationId = CorrelationId { unCorrelationId :: UUID }
  deriving stock (Show, Eq)
  deriving newtype (Ord)

-- | Header name for correlation ID.
correlationIdHeader :: ByteString
correlationIdHeader = "X-Correlation-Id"

-- | Correlation ID middleware.
--
-- 1. Extracts X-Correlation-Id from request header
-- 2. Generates new UUID if not present
-- 3. Adds X-Correlation-Id to response header
--
-- @since 0.1.0.0
correlationMiddleware :: Middleware
correlationMiddleware app req respond = do
  -- Get or generate correlation ID
  correlationId <- getOrGenerateCorrelationId req

  -- Add to response headers
  let addHeader :: ResponseHeaders -> ResponseHeaders
      addHeader headers =
        (correlationIdHeader, TE.encodeUtf8 $ UUID.toText (unCorrelationId correlationId))
        : headers

  -- Pass through with modified response
  app req $ \response ->
    respond $ mapResponseHeaders addHeader response

-- | Get correlation ID from request or generate new one.
getOrGenerateCorrelationId :: Request -> IO CorrelationId
getOrGenerateCorrelationId req =
  case getCorrelationId req of
    Just cid -> pure cid
    Nothing -> generateCorrelationId

-- | Extract correlation ID from request header.
getCorrelationId :: Request -> Maybe CorrelationId
getCorrelationId req = do
  headerValue <- lookup correlationIdHeader (requestHeaders req)
  uuid <- UUID.fromString (BS8.unpack headerValue)
  pure $ CorrelationId uuid

-- | Generate new correlation ID.
generateCorrelationId :: IO CorrelationId
generateCorrelationId = CorrelationId <$> UUID4.nextRandom
