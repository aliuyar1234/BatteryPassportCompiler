{-# LANGUAGE OverloadedStrings #-}

-- | Signing Key Provider Abstraction
--
-- Provides a pluggable interface for signing passport receipts.
-- Supports both development (environment-based) and production (HSM) modes.
--
-- @since 0.1.0.0
module BPC.Core.Signing
  ( -- * Key Provider Interface
    SigningKeyProvider(..)
  , SigningResult(..)
  , SigningError(..)
    -- * Provider Implementations
  , envKeyProvider
  , hsmKeyProvider
    -- * Utilities
  , verifySignature
  ) where

import Crypto.Error (CryptoFailable(..))
import Crypto.PubKey.Ed25519
  ( PublicKey, SecretKey, Signature
  , publicKey, secretKey, sign, signature, toPublic, verify
  )
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Result of a signing operation.
data SigningResult = SigningResult
  { srSignature :: !Text    -- ^ Base64-encoded ED25519 signature
  , srPublicKey :: !Text    -- ^ Base64-encoded public key for verification
  }
  deriving stock (Show, Eq)

-- | Signing errors.
data SigningError
  = SigningKeyNotConfigured
  -- ^ No signing key available
  | SigningKeyInvalid Text
  -- ^ Key format or content invalid
  | SigningHSMError Text
  -- ^ HSM communication error
  | SigningHSMUnavailable
  -- ^ HSM not reachable
  deriving stock (Show, Eq)

-- | Abstract interface for signing key providers.
--
-- Different implementations can use environment variables (dev),
-- Hardware Security Modules (prod), or cloud KMS services.
--
-- @since 0.1.0.0
data SigningKeyProvider = SigningKeyProvider
  { skpName :: Text
  -- ^ Provider name for logging
  , skpSign :: BS.ByteString -> IO (Either SigningError SigningResult)
  -- ^ Sign arbitrary bytes
  , skpGetPublicKey :: IO (Either SigningError Text)
  -- ^ Get the public key (base64)
  }

-- | Create an environment-based key provider.
--
-- Loads ED25519 private key from base64-encoded text (from env var).
-- Suitable for development and testing only.
--
-- WARNING: This provider stores the private key in memory.
-- For production, use hsmKeyProvider.
--
-- @since 0.1.0.0
envKeyProvider :: Maybe Text -> SigningKeyProvider
envKeyProvider mKeyBase64 = SigningKeyProvider
  { skpName = "env"
  , skpSign = \payload -> case mKeyBase64 of
      Nothing -> pure $ Left SigningKeyNotConfigured
      Just keyBase64 -> case decodeKey keyBase64 of
        Left err -> pure $ Left $ SigningKeyInvalid err
        Right (sk, pk) -> do
          let signature = sign sk pk payload
          let sigBase64 = TE.decodeUtf8 $ B64.encode $ convert signature
          let pubBase64 = TE.decodeUtf8 $ B64.encode $ convert pk
          pure $ Right SigningResult
            { srSignature = sigBase64
            , srPublicKey = pubBase64
            }
  , skpGetPublicKey = case mKeyBase64 of
      Nothing -> pure $ Left SigningKeyNotConfigured
      Just keyBase64 -> case decodeKey keyBase64 of
        Left err -> pure $ Left $ SigningKeyInvalid err
        Right (_, pk) -> pure $ Right $ TE.decodeUtf8 $ B64.encode $ convert pk
  }
  where
    decodeKey :: Text -> Either Text (SecretKey, PublicKey)
    decodeKey base64Text = do
      let base64Bs = TE.encodeUtf8 base64Text
      case B64.decode base64Bs of
        Left err -> Left $ T.pack err
        Right keyBytes ->
          case secretKey keyBytes of
            CryptoFailed err -> Left $ T.pack $ show err
            CryptoPassed sk -> Right (sk, toPublic sk)

-- | Create an HSM-based key provider.
--
-- Connects to a PKCS#11 compatible HSM for signing operations.
-- The private key never leaves the HSM.
--
-- @since 0.1.0.0
hsmKeyProvider
  :: Text    -- ^ HSM module path (e.g., /usr/lib/softhsm/libsofthsm2.so)
  -> Text    -- ^ HSM slot PIN
  -> Text    -- ^ Key label in HSM
  -> SigningKeyProvider
hsmKeyProvider _modulePath _pin _keyLabel = SigningKeyProvider
  { skpName = "hsm"
  , skpSign = \_payload -> do
      -- TODO: Implement PKCS#11 signing
      -- For now, return error indicating HSM not implemented
      pure $ Left $ SigningHSMError "HSM integration not yet implemented - use BPC_SIGNING_MODE=env for development"
  , skpGetPublicKey = do
      -- TODO: Implement PKCS#11 public key extraction
      pure $ Left $ SigningHSMError "HSM integration not yet implemented"
  }

-- | Verify an ED25519 signature.
--
-- @since 0.1.0.0
verifySignature
  :: Text           -- ^ Base64-encoded public key
  -> Text           -- ^ Base64-encoded signature
  -> BS.ByteString  -- ^ Original data that was signed
  -> Either Text Bool
verifySignature pubKeyBase64 sigBase64 originalData = do
  -- Decode public key
  pubKeyBytes <- case B64.decode $ TE.encodeUtf8 pubKeyBase64 of
    Left err -> Left $ "Invalid public key encoding: " <> T.pack err
    Right bs -> Right bs

  pk <- case publicKey pubKeyBytes of
    CryptoFailed err -> Left $ "Invalid public key: " <> T.pack (show err)
    CryptoPassed k -> Right k

  -- Decode signature
  sigBytes <- case B64.decode $ TE.encodeUtf8 sigBase64 of
    Left err -> Left $ "Invalid signature encoding: " <> T.pack err
    Right bs -> Right bs

  -- Signature must be exactly 64 bytes for ED25519
  if BS.length sigBytes /= 64
    then Left "Signature must be 64 bytes"
    else do
      -- Convert bytes to Signature using memory-unsafe but correct approach
      let sigResult = signature sigBytes
      case sigResult of
        CryptoFailed err -> Left $ "Invalid signature format: " <> T.pack (show err)
        CryptoPassed sig -> Right $ verify pk originalData sig
