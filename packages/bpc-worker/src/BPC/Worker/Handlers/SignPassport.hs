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

import Crypto.Error (CryptoFailable(..))
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey, publicKey, secretKey, toPublic)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as B64
import Data.Pool (Pool, withResource)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
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
handle _config pool job = do
  -- Decode payload
  case Aeson.fromJSON (DB.jobPayload job) of
    Aeson.Error _err -> pure $ HRFailure $ HEValidation "Invalid SIGN_PASSPORT payload"
    Aeson.Success (_payload :: SignPassportPayload) -> withResource pool $ \_conn -> do
      -- TODO: Implement passport signing
      -- 1. Load ED25519 signing key
      -- 2. Get passport version
      -- 3. Check status is COMPILING
      -- 4. Sign receipt hash
      -- 5. Store signature and public key
      -- 6. Update status to SIGNED
      -- 7. Enqueue GENERATE_QR job
      -- 8. Emit audit event
      pure HRSuccess

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
