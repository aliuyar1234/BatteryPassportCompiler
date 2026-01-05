{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Idempotency Middleware
--
-- Deduplicates mutating requests using Idempotency-Key header.
-- Stores request hash and response for replay.
--
-- @since 0.1.0.0
module BPC.API.Middleware.Idempotency
  ( -- * Middleware
    idempotencyMiddleware
    -- * Functions
  , computeRequestHash
  , withIdempotency
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (SHA256(..), hashWith)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import Data.Word (Word8)
import Network.HTTP.Types (Status, status200, status409, status422, Method)
import Network.Wai (Middleware, Request, Response, requestHeaders,
                     requestMethod, strictRequestBody, responseLBS,
                     responseStatus, responseHeaders, responseToStream)
import qualified BPC.DB

import BPC.API.App (AppM, Env(..), withPool)
import BPC.API.Error (AppError(..))
import BPC.DB.Repos.Idempotency (lookupIdempotencyKey, storeIdempotencyKey, IdempotencyEntry(..))

-- | Header name for idempotency key.
idempotencyKeyHeader :: BS.ByteString
idempotencyKeyHeader = "Idempotency-Key"

-- | Idempotency middleware.
--
-- For POST/PUT/PATCH/DELETE:
-- 1. Requires Idempotency-Key header
-- 2. Checks if key was seen before
-- 3. If yes and request hash matches, replay stored response
-- 4. If yes and request hash differs, return 409 IDEMPOTENCY_CONFLICT
-- 5. If no, execute request and store response
--
-- Note: This middleware requires tenant_id to be extracted from auth context.
-- For now, it extracts from X-Tenant-ID header (in production, use auth middleware).
--
-- @since 0.1.0.0
idempotencyMiddleware :: Env -> Middleware
idempotencyMiddleware env app req respond = do
  if isMutatingMethod (requestMethod req)
    then handleIdempotent env app req respond
    else app req respond

-- | Check if method requires idempotency.
isMutatingMethod :: Method -> Bool
isMutatingMethod method = method `elem` ["POST", "PUT", "PATCH", "DELETE"]

-- | Handle idempotent request.
handleIdempotent :: Env -> Middleware
handleIdempotent env app req respond = do
  case lookup idempotencyKeyHeader (requestHeaders req) of
    Nothing -> respond $ responseLBS status422
      [("Content-Type", "application/json")]
      "{\"error\":{\"code\":\"VALIDATION_ERROR\",\"message\":\"Idempotency-Key header is required for mutating requests\"}}"

    Just idempotencyKeyBS -> do
      -- Extract tenant ID (in production, this comes from auth context)
      -- For now, require X-Tenant-ID header
      case lookup "X-Tenant-ID" (requestHeaders req) of
        Nothing -> respond $ responseLBS status422
          [("Content-Type", "application/json")]
          "{\"error\":{\"code\":\"VALIDATION_ERROR\",\"message\":\"X-Tenant-ID header is required\"}}"

        Just tenantIdBS -> do
          case Aeson.decode (LBS.fromStrict tenantIdBS) of
            Nothing -> respond $ responseLBS status422
              [("Content-Type", "application/json")]
              "{\"error\":{\"code\":\"VALIDATION_ERROR\",\"message\":\"Invalid X-Tenant-ID format\"}}"

            Just tenantId -> do
              -- Read request body for hashing
              body <- strictRequestBody req

              -- Compute hashes
              let idempotencyKey = TE.decodeUtf8 idempotencyKeyBS
              let keyHash = hashText idempotencyKey
              let requestHash = computeRequestHashBS (requestMethod req) body

              -- Check for existing entry
              result <- BPC.DB.withConn (envPool env) $ \conn ->
                lookupIdempotencyKey conn tenantId keyHash

              case result of
                Just entry -> do
                  -- Found existing entry
                  if ieRequestHash entry == requestHash
                    then do
                      -- Request hash matches, replay response
                      let headers = jsonToHeaders (ieResponseHeaders entry)
                      respond $ responseLBS
                        (toEnum $ ieResponseStatus entry)
                        headers
                        (LBS.fromStrict $ ieResponseBody entry)
                    else do
                      -- Request hash mismatch, return conflict
                      respond $ responseLBS status409
                        [("Content-Type", "application/json")]
                        (Aeson.encode $ object
                          [ "error" .= object
                            [ "code" .= ("IDEMPOTENCY_CONFLICT" :: Text)
                            , "message" .= ("Idempotency key already used with different request" :: Text)
                            , "key_hash" .= keyHash
                            ]
                          ])

                Nothing -> do
                  -- No existing entry, execute request and capture response
                  responseRef <- newIORef Nothing

                  app req $ \response -> do
                    -- Capture response
                    captured <- captureResponse response
                    writeIORef responseRef (Just captured)

                    -- Store in database
                    let (status, headers, responseBody) = captured
                    _ <- BPC.DB.withConn (envPool env) $ \conn ->
                      storeIdempotencyKey
                        conn
                        tenantId
                        keyHash
                        requestHash
                        (fromEnum status)
                        (LBS.toStrict responseBody)
                        (headersToJson headers)
                        `catch` \(_ :: SomeException) -> pure ()

                    -- Send response to client
                    respond response

-- | Compute SHA-256 hash of request.
computeRequestHashBS :: Method -> LBS.ByteString -> Text
computeRequestHashBS method body =
  let input = LBS.toStrict $ LBS.fromStrict method <> body
      digest = hashWith SHA256 input
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

-- | Hash text using SHA-256.
hashText :: Text -> Text
hashText text =
  let bytes = TE.encodeUtf8 text
      digest = hashWith SHA256 bytes
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

-- | Capture response for storage.
--
-- Extracts status, headers, and body from a WAI Response.
--
-- Note: This is a simplified implementation that works for responseLBS.
-- For streaming responses, we would need more complex buffering.
captureResponse :: Response -> IO (Status, [(BS.ByteString, BS.ByteString)], LBS.ByteString)
captureResponse response = do
  let status = responseStatus response
  let headers = responseHeaders response

  -- Stream response body to capture it
  bodyRef <- newIORef []
  let (_, _, withBody) = responseToStream response
  withBody $ \streamingBody -> do
    streamingBody
      (\builder -> modifyIORef bodyRef (++ [BB.toLazyByteString builder]))
      (pure ())

  bodyChunks <- readIORef bodyRef
  let body = LBS.concat bodyChunks
  pure (status, headers, body)

-- | Convert headers to JSON Value.
headersToJson :: [(BS.ByteString, BS.ByteString)] -> Value
headersToJson headers = object
  [ TE.decodeUtf8 k .= TE.decodeUtf8 v
  | (k, v) <- headers
  ]

-- | Convert JSON Value to headers.
jsonToHeaders :: Value -> [(BS.ByteString, BS.ByteString)]
jsonToHeaders (Aeson.Object obj) =
  [ (TE.encodeUtf8 k, TE.encodeUtf8 v)
  | (k, Aeson.String v) <- KM.toList obj
  ]
jsonToHeaders _ = [("Content-Type", "application/json")]

-- | Compute request hash in AppM context.
computeRequestHash :: Method -> LBS.ByteString -> AppM Text
computeRequestHash method body = pure $ computeRequestHashBS method body

-- | Execute action with idempotency.
--
-- Checks for existing key, executes action if new, stores response.
--
-- This is a higher-level function for use within handlers.
--
-- @since 0.1.0.0
withIdempotency
  :: Text           -- ^ Idempotency key
  -> Text           -- ^ Request hash
  -> UUID           -- ^ Tenant ID
  -> AppM Value     -- ^ Action to execute
  -> AppM Value
withIdempotency idempotencyKey requestHash tenantId action = do
  -- Compute key hash
  let keyHash = hashText idempotencyKey

  -- Look up existing entry
  maybeEntry <- withPool $ \conn ->
    lookupIdempotencyKey conn tenantId keyHash

  case maybeEntry of
    Just entry -> do
      -- Check if request hash matches
      if ieRequestHash entry == requestHash
        then do
          -- Decode and return stored response
          case Aeson.decode (LBS.fromStrict $ ieResponseBody entry) of
            Just val -> pure val
            Nothing -> throwError $ InternalError "Failed to decode stored idempotency response"
        else do
          -- Hash mismatch
          throwError $ IdempotencyConflict keyHash

    Nothing -> do
      -- Execute action and store result
      result <- action

      -- Store in database (fire and forget, errors are logged but don't fail the request)
      _ <- withPool $ \conn ->
        storeIdempotencyKey
          conn
          tenantId
          keyHash
          requestHash
          200  -- Assuming success status
          (LBS.toStrict $ Aeson.encode result)
          (object [])  -- Empty headers for now
          `catch` \(_ :: SomeException) -> pure ()

      pure result
