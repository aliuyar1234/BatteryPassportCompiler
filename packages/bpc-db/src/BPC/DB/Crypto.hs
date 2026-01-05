{-# LANGUAGE OverloadedStrings #-}

-- | Cryptographic Utilities
--
-- Application-level encryption for sensitive data like webhook secrets.
-- Uses AES-256-CTR with HMAC-SHA256 for authenticated encryption.
--
-- @since 0.1.0.0
module BPC.DB.Crypto
  ( -- * Encryption/Decryption
    encryptSecret
  , decryptSecret
    -- * Key Derivation
  , deriveKey
    -- * Types
  , CryptoError(..)
  ) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit, ctrCombine, makeIV, IV)
import Crypto.Error (CryptoFailable(..))
import Crypto.Hash (SHA256(..), hashWith)
import Crypto.KDF.PBKDF2 (Parameters(..), fastPBKDF2_SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random (getRandomBytes)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Crypto errors.
data CryptoError
  = CryptoKeyError Text
  | CryptoDecryptError Text
  | CryptoAuthError Text
  deriving stock (Show, Eq)

-- | Fixed salt for key derivation (application-specific).
derivationSalt :: BS.ByteString
derivationSalt = "bpc-webhook-encryption-key-v1"

-- | Derive a 256-bit key from the master key using PBKDF2.
--
-- @since 0.1.0.0
deriveKey :: Text -> BS.ByteString
deriveKey masterKey =
  let params = Parameters
        { iterCounts = 100000
        , outputLength = 32  -- 256 bits
        }
  in fastPBKDF2_SHA256 params (TE.encodeUtf8 masterKey) derivationSalt

-- | Compute HMAC-SHA256 authentication tag.
computeHmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
computeHmac key msg =
  let tag = hmac key msg :: HMAC SHA256
  in convert tag

-- | Encrypt a secret using AES-256-CTR with HMAC-SHA256.
--
-- Output format (base64): iv (16 bytes) || ciphertext || hmac (32 bytes)
--
-- @since 0.1.0.0
encryptSecret :: Text -> Text -> IO (Either CryptoError Text)
encryptSecret masterKey plaintext = do
  -- Generate random 16-byte IV
  ivBytes <- getRandomBytes 16 :: IO BS.ByteString

  let key = deriveKey masterKey
  let plaintextBytes = TE.encodeUtf8 plaintext

  case cipherInit key :: CryptoFailable AES256 of
    CryptoFailed err -> pure $ Left $ CryptoKeyError $ T.pack $ show err
    CryptoPassed cipher ->
      case makeIV ivBytes :: Maybe (IV AES256) of
        Nothing -> pure $ Left $ CryptoKeyError "Failed to create IV"
        Just iv -> do
          let ciphertext = ctrCombine cipher iv plaintextBytes
          let authData = ivBytes <> ciphertext
          let tag = computeHmac key authData
          let combined = ivBytes <> ciphertext <> tag
          pure $ Right $ TE.decodeUtf8 $ B64.encode combined

-- | Decrypt a secret encrypted with encryptSecret.
--
-- Verifies HMAC before decryption to detect tampering.
--
-- @since 0.1.0.0
decryptSecret :: Text -> Text -> Either CryptoError Text
decryptSecret masterKey encryptedText = do
  -- Decode base64
  combined <- case B64.decode $ TE.encodeUtf8 encryptedText of
    Left err -> Left $ CryptoDecryptError $ T.pack err
    Right bs -> Right bs

  -- Extract components: iv (16) || ciphertext (n) || hmac (32)
  -- Minimum length: 16 + 1 + 32 = 49
  if BS.length combined < 49
    then Left $ CryptoDecryptError "Encrypted data too short"
    else do
      let ivBytes = BS.take 16 combined
      let rest = BS.drop 16 combined
      let tag = BS.drop (BS.length rest - 32) rest
      let ciphertext = BS.take (BS.length rest - 32) rest

      let key = deriveKey masterKey

      -- Verify HMAC
      let authData = ivBytes <> ciphertext
      let expectedTag = computeHmac key authData
      if tag /= expectedTag
        then Left $ CryptoAuthError "Authentication failed - data may be tampered"
        else case cipherInit key :: CryptoFailable AES256 of
          CryptoFailed err -> Left $ CryptoKeyError $ T.pack $ show err
          CryptoPassed cipher ->
            case makeIV ivBytes :: Maybe (IV AES256) of
              Nothing -> Left $ CryptoKeyError "Failed to create IV"
              Just iv -> do
                let plaintext = ctrCombine cipher iv ciphertext
                Right $ TE.decodeUtf8 plaintext
