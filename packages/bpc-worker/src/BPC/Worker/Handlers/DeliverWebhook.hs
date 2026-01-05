{-# LANGUAGE OverloadedStrings #-}

-- | Deliver Webhook Handler
--
-- Delivers webhook events with HMAC signature.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.DeliverWebhook
  ( handle
  , DeliverWebhookPayload(..)
  , computeHmacSignature
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (void)
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.ByteArray (convert)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for DELIVER_WEBHOOK job.
--
-- @since 0.1.0.0
data DeliverWebhookPayload = DeliverWebhookPayload
  { dwpDeliveryId :: UUID
  , dwpTenantId :: UUID
  , dwpEndpointId :: UUID
  , dwpEventType :: Text
  , dwpEventData :: Value
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DeliverWebhookPayload where
  parseJSON = Aeson.withObject "DeliverWebhookPayload" $ \o -> DeliverWebhookPayload
    <$> o .: "delivery_id"
    <*> o .: "tenant_id"
    <*> o .: "endpoint_id"
    <*> o .: "event_type"
    <*> o .: "event_data"

instance ToJSON DeliverWebhookPayload where
  toJSON DeliverWebhookPayload{..} = object
    [ "delivery_id" .= dwpDeliveryId
    , "tenant_id" .= dwpTenantId
    , "endpoint_id" .= dwpEndpointId
    , "event_type" .= dwpEventType
    , "event_data" .= dwpEventData
    ]

-- | Handle DELIVER_WEBHOOK job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe DeliverWebhookPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid DELIVER_WEBHOOK payload"
    Just payload -> withResource pool $ \conn -> do
      -- Get endpoint
      mEndpoint <- DB.getEndpoint conn (dwpTenantId payload) (dwpEndpointId payload)
      case mEndpoint of
        Nothing -> pure $ HRFailure $ HENotFound "Webhook endpoint not found"
        Just endpoint -> do
          -- Check endpoint is active
          if not (DB.weIsActive endpoint)
            then pure $ HRFailure $ HENonRetryable "Webhook endpoint is deactivated"
            else do
              -- Build request body
              let body = Aeson.encode $ object
                    [ "event_type" .= dwpEventType payload
                    , "delivery_id" .= dwpDeliveryId payload
                    , "data" .= dwpEventData payload
                    , "timestamp" .= show =<< Just <$> getCurrentTime
                    ]

              -- Compute HMAC signature
              let secret = TE.encodeUtf8 $ DB.weSecret endpoint
              let signature = computeHmacSignature secret (LBS.toStrict body)

              -- Deliver webhook
              result <- deliverWebhook config (DB.weUrl endpoint) body signature

              case result of
                Left err -> do
                  -- Update delivery status
                  void $ DB.updateDeliveryStatus conn (dwpDeliveryId payload)
                    DB.DeliveryFailed (Just $ T.pack $ show err)
                  -- Retry on network errors
                  pure $ HRFailure $ HERetryable $ "Webhook delivery failed: " <> T.pack (show err)

                Right statusCode' -> do
                  if statusCode' >= 200 && statusCode' < 300
                    then do
                      -- Success
                      void $ DB.updateDeliveryStatus conn (dwpDeliveryId payload)
                        DB.DeliveryDelivered Nothing
                      pure HRSuccess
                    else do
                      -- HTTP error
                      let errMsg = "HTTP " <> T.pack (show statusCode')
                      void $ DB.updateDeliveryStatus conn (dwpDeliveryId payload)
                        DB.DeliveryFailed (Just errMsg)
                      if statusCode' >= 500
                        then pure $ HRFailure $ HERetryable errMsg  -- Retry server errors
                        else pure $ HRFailure $ HENonRetryable errMsg  -- Don't retry client errors

-- | Compute HMAC-SHA256 signature.
--
-- Returns hex-encoded signature prefixed with "sha256=".
--
-- @since 0.1.0.0
computeHmacSignature :: BS.ByteString -> BS.ByteString -> Text
computeHmacSignature secret body =
  let mac :: HMAC SHA256
      mac = hmac secret body
      digest = hmacGetDigest mac
      hexDigest = BS8.unpack $ convert digest
  in "sha256=" <> T.pack (concatMap toHex hexDigest)
  where
    toHex c = let (hi, lo) = (fromEnum c `divMod` 16)
              in [hexDigit hi, hexDigit lo]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'a' + n - 10)

-- | Deliver webhook HTTP request.
--
-- @since 0.1.0.0
deliverWebhook :: WorkerConfig -> Text -> LBS.ByteString -> Text -> IO (Either SomeException Int)
deliverWebhook config url body signature = try $ do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro (wcWebhookTimeoutSeconds config * 1000000)
    }

  initReq <- parseRequest (T.unpack url)
  let req = initReq
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("X-BPC-Signature", TE.encodeUtf8 signature)
            , ("User-Agent", "BPC-Webhook/1.0")
            ]
        }

  response <- httpLbs req manager
  pure $ statusCode $ responseStatus response
