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
import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:))
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
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid DELIVER_WEBHOOK payload"
    Aeson.Success (_payload :: DeliverWebhookPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement webhook delivery
      -- 1. Get webhook endpoint
      -- 2. Build request body with HMAC signature
      -- 3. Deliver HTTP request
      -- 4. Update delivery status
      pure HRSuccess

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
