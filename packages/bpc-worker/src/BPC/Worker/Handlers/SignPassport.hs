{-# LANGUAGE OverloadedStrings #-}

-- | Sign Passport Handler
--
-- Signs receipt hash with ED25519.
--
-- @since 0.1.0.0
module BPC.Worker.Handlers.SignPassport
  ( handle
  , SignPassportPayload(..)
  ) where

import Control.Monad (void)
import Crypto.Error (CryptoFailable(..))
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, Signature, publicKey, secretKey, sign, toPublic)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), decode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LBS
import Data.ByteArray (convert)
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)

import BPC.Worker.Types
import qualified BPC.DB as DB

-- | Payload for SIGN_PASSPORT job.
--
-- @since 0.1.0.0
data SignPassportPayload = SignPassportPayload
  { sppPassportVersionId :: UUID
  , sppTenantId :: UUID
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SignPassportPayload where
  parseJSON = Aeson.withObject "SignPassportPayload" $ \o -> SignPassportPayload
    <$> o .: "passport_version_id"
    <*> o .: "tenant_id"

instance ToJSON SignPassportPayload where
  toJSON SignPassportPayload{..} = object
    [ "passport_version_id" .= sppPassportVersionId
    , "tenant_id" .= sppTenantId
    ]

-- | Handle SIGN_PASSPORT job.
--
-- @since 0.1.0.0
handle :: WorkerConfig -> Pool Connection -> DB.Job -> IO HandlerResult
handle config pool job = do
  -- Decode payload
  let mPayload = decode (LBS.fromStrict $ DB.jobPayload job) :: Maybe SignPassportPayload
  case mPayload of
    Nothing -> pure $ HRFailure $ HEValidation "Invalid SIGN_PASSPORT payload"
    Just payload -> do
      -- Load signing key
      case wcSigningKeyBase64 config of
        Nothing -> pure $ HRFailure $ HENonRetryable "Signing key not configured"
        Just keyBase64 -> do
          case decodeSigningKey keyBase64 of
            Left err -> pure $ HRFailure $ HENonRetryable $ "Invalid signing key: " <> err
            Right (privKey, pubKey) -> withResource pool $ \conn -> do
              -- Get passport version
              mVersion <- DB.getPassportVersion conn (sppTenantId payload) (sppPassportVersionId payload)
              case mVersion of
                Nothing -> pure $ HRFailure $ HENotFound "Passport version not found"
                Just version -> do
                  -- Check status is COMPILING
                  case DB.pvStatus version of
                    DB.PassportVersionCompiling -> do
                      -- Sign receipt hash
                      let receiptHash = DB.pvReceiptHash version
                      let hashBytes = TE.encodeUtf8 receiptHash
                      let signature = sign privKey pubKey hashBytes

                      -- Store signature
                      let sigBase64 = B64.encode (convert signature :: BS.ByteString)
                      let pubKeyBase64 = B64.encode (convert pubKey :: BS.ByteString)

                      DB.signPassportVersion conn (sppPassportVersionId payload)
                        (TE.decodeUtf8 sigBase64)
                        (TE.decodeUtf8 pubKeyBase64)

                      -- Update status to SIGNED
                      DB.updatePassportVersionStatus conn (sppPassportVersionId payload) DB.PassportVersionSigned

                      -- Enqueue GENERATE_QR job
                      qrJobId <- UUID.nextRandom
                      void $ DB.enqueue conn (sppTenantId payload) DB.JobInput
                        { DB.jiId = qrJobId
                        , DB.jiType = "GENERATE_QR"
                        , DB.jiPayload = LBS.toStrict $ Aeson.encode $ object
                            [ "passport_version_id" .= sppPassportVersionId payload
                            , "tenant_id" .= sppTenantId payload
                            ]
                        }

                      -- Emit audit event
                      now <- getCurrentTime
                      void $ DB.appendEvent conn (sppTenantId payload) DB.AppendEventInput
                        { DB.aeiAggregateType = "PassportVersion"
                        , DB.aeiAggregateId = sppPassportVersionId payload
                        , DB.aeiEventType = "PASSPORT_SIGNED"
                        , DB.aeiEventData = Aeson.encode $ object
                            [ "passport_version_id" .= sppPassportVersionId payload
                            ]
                        , DB.aeiActorId = Nothing
                        }

                      pure HRSuccess

                    _ -> pure $ HRFailure $ HEPrecondition "Passport version not in COMPILING status"

-- | Decode ED25519 signing key from base64.
--
-- @since 0.1.0.0
decodeSigningKey :: Text -> Either Text (SecretKey, PublicKey)
decodeSigningKey base64Text = do
  let base64Bs = TE.encodeUtf8 base64Text
  case B64.decode base64Bs of
    Left err -> Left $ T.pack err
    Right keyBytes ->
      case secretKey keyBytes of
        CryptoFailed err -> Left $ T.pack $ show err
        CryptoPassed sk -> Right (sk, toPublic sk)
