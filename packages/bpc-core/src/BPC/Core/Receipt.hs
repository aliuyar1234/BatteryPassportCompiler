{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | BPC.Core.Receipt - Receipt Generation & Signing (BPC-RECEIPT-1)
--
-- Generates machine-verifiable receipts containing all hashes and metadata.
-- Supports ED25519 signature generation and verification.
module BPC.Core.Receipt
  ( -- * Receipt Types
    ReceiptInput (..)
  , ReceiptUnsigned (..)
  , ReceiptSigned (..)

    -- * Receipt Building
  , buildReceiptUnsigned
  , hashReceiptUnsigned

    -- * Signature Types
  , Ed25519PrivateKey (..)
  , Ed25519PublicKey (..)
  , Signature (..)

    -- * Signing & Verification
  , signReceiptHash
  , verifySignature
  , derivePublicKey
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import GHC.Generics (Generic)

import BPC.Core.CanonicalJson (canonicalEncode)
import BPC.Core.Hash (sha256Hex)

-- Cryptographic imports (using cryptonite)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Crypto.Error (CryptoFailable(..))
import Data.ByteArray (convert)

-- | Input for building a receipt.
data ReceiptInput = ReceiptInput
  { riPassportVersionId :: !UUID
  , riTenantId :: !UUID
  , riSnapshotId :: !UUID
  , riSnapshotHash :: !Text
  , riRulesId :: !UUID
  , riRulesHash :: !Text
  , riPayloadHash :: !Text
  , riProofHash :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Unsigned receipt ready for signing.
data ReceiptUnsigned = ReceiptUnsigned
  { ruVersion :: !Text           -- ^ Always "BPC-RECEIPT-1"
  , ruPassportVersionId :: !UUID
  , ruTenantId :: !UUID
  , ruSnapshotId :: !UUID
  , ruSnapshotHash :: !Text
  , ruRulesId :: !UUID
  , ruRulesHash :: !Text
  , ruPayloadHash :: !Text
  , ruProofHash :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReceiptUnsigned where
  toJSON ReceiptUnsigned{..} = object
    [ "version" .= ruVersion
    , "passport_version_id" .= UUID.toText ruPassportVersionId
    , "tenant_id" .= UUID.toText ruTenantId
    , "snapshot_id" .= UUID.toText ruSnapshotId
    , "snapshot_hash" .= ruSnapshotHash
    , "rules_id" .= UUID.toText ruRulesId
    , "rules_hash" .= ruRulesHash
    , "payload_hash" .= ruPayloadHash
    , "proof_hash" .= ruProofHash
    ]

instance FromJSON ReceiptUnsigned

-- | Signed receipt with ED25519 signature.
data ReceiptSigned = ReceiptSigned
  { rsReceipt :: !ReceiptUnsigned
  , rsReceiptHash :: !Text
  , rsSignature :: !Text         -- ^ Hex-encoded ED25519 signature
  , rsSignerKeyId :: !UUID       -- ^ ID of signing key
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReceiptSigned where
  toJSON ReceiptSigned{..} = object
    [ "receipt" .= rsReceipt
    , "receipt_hash" .= rsReceiptHash
    , "signature" .= rsSignature
    , "signer_key_id" .= UUID.toText rsSignerKeyId
    ]

instance FromJSON ReceiptSigned

-- | ED25519 private key (32 bytes).
newtype Ed25519PrivateKey = Ed25519PrivateKey { unPrivateKey :: ByteString }
  deriving stock (Eq, Show)

-- | ED25519 public key (32 bytes).
newtype Ed25519PublicKey = Ed25519PublicKey { unPublicKey :: ByteString }
  deriving stock (Eq, Show)

-- | ED25519 signature (64 bytes).
newtype Signature = Signature { unSignature :: ByteString }
  deriving stock (Eq, Show)

-- | Build an unsigned receipt from input.
buildReceiptUnsigned :: ReceiptInput -> ReceiptUnsigned
buildReceiptUnsigned ReceiptInput{..} = ReceiptUnsigned
  { ruVersion = "BPC-RECEIPT-1"
  , ruPassportVersionId = riPassportVersionId
  , ruTenantId = riTenantId
  , ruSnapshotId = riSnapshotId
  , ruSnapshotHash = riSnapshotHash
  , ruRulesId = riRulesId
  , ruRulesHash = riRulesHash
  , ruPayloadHash = riPayloadHash
  , ruProofHash = riProofHash
  }

-- | Compute SHA-256 hash of unsigned receipt.
hashReceiptUnsigned :: ReceiptUnsigned -> Text
hashReceiptUnsigned receipt =
  case canonicalEncode (Aeson.toJSON receipt) of
    Right bs -> sha256Hex bs
    Left _ -> ""  -- Should not happen with valid input

-- | Sign a receipt hash with ED25519 private key.
signReceiptHash :: Ed25519PrivateKey -> Text -> Either Text Signature
signReceiptHash (Ed25519PrivateKey privKeyBytes) hashHex = do
  -- Parse private key
  secretKey <- case Ed25519.secretKey privKeyBytes of
    CryptoPassed sk -> Right sk
    CryptoFailed _ -> Left "Invalid private key"

  -- Parse hash bytes (from hex)
  hashBytes <- case B16.decode (TE.encodeUtf8 hashHex) of
    Right bs -> Right bs
    Left _ -> Left "Invalid hash hex"

  -- Derive public key and sign
  let publicKey = Ed25519.toPublic secretKey
  let sig = Ed25519.sign secretKey publicKey hashBytes

  Right $ Signature $ convert sig
  where
    -- Helper to get raw bytes from signature

-- | Verify an ED25519 signature.
verifySignature :: Ed25519PublicKey -> Text -> Signature -> Bool
verifySignature (Ed25519PublicKey pubKeyBytes) hashHex (Signature sigBytes) =
  case (Ed25519.publicKey pubKeyBytes, Ed25519.signature sigBytes, parseHashHex hashHex) of
    (CryptoPassed pk, CryptoPassed sig, Just hashBytes) ->
      Ed25519.verify pk hashBytes sig
    _ -> False
  where
    parseHashHex hex = case B16.decode (TE.encodeUtf8 hex) of
      Right bs -> Just bs
      Left _ -> Nothing

-- | Derive public key from private key.
derivePublicKey :: Ed25519PrivateKey -> Either Text Ed25519PublicKey
derivePublicKey (Ed25519PrivateKey privKeyBytes) =
  case Ed25519.secretKey privKeyBytes of
    CryptoPassed sk ->
      let pk = Ed25519.toPublic sk
      in Right $ Ed25519PublicKey $ convert pk
    CryptoFailed _ -> Left "Invalid private key"
