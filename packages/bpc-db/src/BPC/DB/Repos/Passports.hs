{-# LANGUAGE OverloadedStrings #-}

-- | Passport Repository
--
-- Manage passports and versions with activate/revoke lifecycle.
-- Activation is transactional - old version becomes SUPERSEDED, new becomes ACTIVE.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Passports
  ( -- * Types
    PassportId
  , PassportVersionId
  , PassportStatus(..)
  , Passport(..)
  , PassportVersion(..)
  , PassportVersionInput(..)
    -- * Passport Operations
  , createPassport
  , getPassport
  , listPassports
  , getPassportByProduct
    -- * Version Operations
  , insertVersion
  , getPassportVersion
  , getVersionsByPassport
  , getActiveVersion
    -- * Lifecycle Operations
  , activate
  , revoke
  ) where

import Control.Exception (catch, bracket)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import GHC.Generics (Generic)

import BPC.DB.Error (ActivateError(..), PassportError(..), RevokeError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Passport ID (UUID)
type PassportId = UUID

-- | Passport Version ID (UUID)
type PassportVersionId = UUID

-- | Passport version status.
data PassportStatus
  = PassCompiling
  | PassSigned
  | PassActive
  | PassSuperseded
  | PassRevoked
  deriving stock (Show, Eq, Generic)

statusToText :: PassportStatus -> Text
statusToText = \case
  PassCompiling  -> "COMPILING"
  PassSigned     -> "SIGNED"
  PassActive     -> "ACTIVE"
  PassSuperseded -> "SUPERSEDED"
  PassRevoked    -> "REVOKED"

textToStatus :: Text -> Maybe PassportStatus
textToStatus = \case
  "COMPILING"  -> Just PassCompiling
  "SIGNED"     -> Just PassSigned
  "ACTIVE"     -> Just PassActive
  "SUPERSEDED" -> Just PassSuperseded
  "REVOKED"    -> Just PassRevoked
  _            -> Nothing

-- | Passport record.
data Passport = Passport
  { passId :: PassportId
  , passTenantId :: TenantId
  , passBatteryProductId :: UUID
  , passCreatedAt :: UTCTime
  , passUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating a passport version.
data PassportVersionInput = PassportVersionInput
  { pviSnapshotId :: UUID
  -- ^ Sealed snapshot ID
  , pviRulesVersionId :: UUID
  -- ^ Published rules version ID
  , pviPayloadCanonical :: BS.ByteString
  -- ^ Canonical payload bytes
  , pviPayloadHash :: Text
  -- ^ SHA-256 hash of payload
  , pviProofCanonical :: BS.ByteString
  -- ^ Canonical proof bytes
  , pviProofHash :: Text
  -- ^ SHA-256 hash of proof
  , pviReceiptCanonical :: BS.ByteString
  -- ^ Canonical receipt bytes
  , pviReceiptHash :: Text
  -- ^ SHA-256 hash of receipt
  , pviQrPayload :: Text
  -- ^ QR code payload string
  }
  deriving stock (Show, Eq, Generic)

-- | Passport version record.
data PassportVersion = PassportVersion
  { pvId :: PassportVersionId
  , pvPassportId :: PassportId
  , pvTenantId :: TenantId
  , pvVersionNumber :: Int
  , pvSnapshotId :: UUID
  , pvRulesVersionId :: UUID
  , pvPayloadHash :: Text
  , pvProofHash :: Text
  , pvReceiptHash :: Text
  , pvQrPayload :: Text
  , pvStatus :: PassportStatus
  , pvSignature :: Maybe Text
  , pvSignedAt :: Maybe UTCTime
  , pvActivatedAt :: Maybe UTCTime
  , pvRevokedAt :: Maybe UTCTime
  , pvCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Create a new passport for a battery product.
--
-- @since 0.1.0.0
createPassport
  :: Connection
  -> TenantId
  -> UUID
  -> IO (Either PassportError PassportId)
createPassport conn tenantId batteryProductId = do
  result <- (Right <$> PG.query conn
    "INSERT INTO passports (tenant_id, battery_product_id) \
    \VALUES (?, ?) RETURNING id"
    (tenantId, batteryProductId))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ PASSPORT_NOT_FOUND (error "No ID returned")
    Right (Only pid : _) -> pure $ Right pid
  where
    handleSqlError :: SqlError -> IO (Either PassportError [Only UUID])
    handleSqlError _ = pure $ Left $ PASSPORT_NOT_FOUND (error "Constraint violation")

-- | Get a passport by ID.
getPassport
  :: Connection
  -> TenantId
  -> PassportId
  -> IO (Maybe Passport)
getPassport conn tenantId passId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, battery_product_id, created_at, updated_at \
    \FROM passports WHERE tenant_id = ? AND id = ?"
    (tenantId, passId)
  pure $ case rows of
    [] -> Nothing
    ((pid, tid, bpid, cat, uat) : _) ->
      Just Passport
        { passId = pid
        , passTenantId = tid
        , passBatteryProductId = bpid
        , passCreatedAt = cat
        , passUpdatedAt = uat
        }

-- | List passports with cursor pagination.
--
-- Fetches passports ordered by (created_at DESC, id DESC).
-- If cursor is provided (timestamp, id), returns passports created before that point.
--
-- @since 0.1.0.0
listPassports
  :: Connection
  -> TenantId
  -> Int
  -- ^ Limit (number of items to fetch)
  -> Maybe (UTCTime, UUID)
  -- ^ Optional cursor (timestamp, id)
  -> IO [Passport]
listPassports conn tenantId limit cursor = do
  rows <- case cursor of
    Nothing ->
      PG.query conn
        "SELECT id, tenant_id, battery_product_id, created_at, updated_at \
        \FROM passports \
        \WHERE tenant_id = ? \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, limit)
    Just (timestamp, cursorId) ->
      PG.query conn
        "SELECT id, tenant_id, battery_product_id, created_at, updated_at \
        \FROM passports \
        \WHERE tenant_id = ? AND (created_at, id) < (?, ?) \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, timestamp, cursorId, limit)

  pure $ map toPassport rows
  where
    toPassport (pid, tid, bpid, cat, uat) =
      Passport
        { passId = pid
        , passTenantId = tid
        , passBatteryProductId = bpid
        , passCreatedAt = cat
        , passUpdatedAt = uat
        }

-- | Get a passport by battery product ID.
getPassportByProduct
  :: Connection
  -> TenantId
  -> UUID
  -> IO (Maybe Passport)
getPassportByProduct conn tenantId productId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, battery_product_id, created_at, updated_at \
    \FROM passports WHERE tenant_id = ? AND battery_product_id = ?"
    (tenantId, productId)
  pure $ case rows of
    [] -> Nothing
    ((pid, tid, bpid, cat, uat) : _) ->
      Just Passport
        { passId = pid
        , passTenantId = tid
        , passBatteryProductId = bpid
        , passCreatedAt = cat
        , passUpdatedAt = uat
        }

-- | Insert a new passport version.
--
-- Creates a version with status COMPILING.
--
-- @since 0.1.0.0
insertVersion
  :: Connection
  -> TenantId
  -> PassportId
  -> PassportVersionInput
  -> IO (Either PassportError PassportVersionId)
insertVersion conn tenantId passId input = do
  -- Get next version number
  versionNum <- getNextVersionNumber conn tenantId passId

  result <- (Right <$> PG.query conn
    "INSERT INTO passport_versions \
    \(tenant_id, passport_id, version_number, snapshot_id, rules_version_id, \
    \ payload_canonical, payload_hash, proof_canonical, proof_hash, \
    \ receipt_canonical, receipt_hash, qr_payload, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, 'COMPILING') \
    \RETURNING id"
    ( tenantId
    , passId
    , versionNum
    , pviSnapshotId input
    , pviRulesVersionId input
    , PG.Binary $ pviPayloadCanonical input
    , pviPayloadHash input
    , PG.Binary $ pviProofCanonical input
    , pviProofHash input
    , PG.Binary $ pviReceiptCanonical input
    , pviReceiptHash input
    , pviQrPayload input
    )) `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ PASSPORT_VERSION_NOT_FOUND (error "No ID returned")
    Right (Only vid : _) -> pure $ Right vid
  where
    handleSqlError :: SqlError -> IO (Either PassportError [Only UUID])
    handleSqlError _ = pure $ Left $ PASSPORT_NOT_FOUND passId

-- | Get next version number for a passport.
getNextVersionNumber :: Connection -> TenantId -> PassportId -> IO Int
getNextVersionNumber conn tenantId passId = do
  [Only maxVer] <- PG.query conn
    "SELECT COALESCE(MAX(version_number), 0) FROM passport_versions \
    \WHERE tenant_id = ? AND passport_id = ?"
    (tenantId, passId)
  pure $ maxVer + 1

-- | Get a passport version by ID.
getPassportVersion
  :: Connection
  -> TenantId
  -> PassportVersionId
  -> IO (Maybe PassportVersion)
getPassportVersion conn tenantId versionId = do
  rows <- PG.query conn
    "SELECT id, passport_id, tenant_id, version_number, snapshot_id, \
    \       rules_version_id, payload_hash, proof_hash, receipt_hash, \
    \       qr_payload, status, signature, signed_at, activated_at, \
    \       revoked_at, created_at \
    \FROM passport_versions WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToVersion row

-- | Get all versions for a passport.
getVersionsByPassport
  :: Connection
  -> TenantId
  -> PassportId
  -> IO [PassportVersion]
getVersionsByPassport conn tenantId passId = do
  rows <- PG.query conn
    "SELECT id, passport_id, tenant_id, version_number, snapshot_id, \
    \       rules_version_id, payload_hash, proof_hash, receipt_hash, \
    \       qr_payload, status, signature, signed_at, activated_at, \
    \       revoked_at, created_at \
    \FROM passport_versions \
    \WHERE tenant_id = ? AND passport_id = ? \
    \ORDER BY version_number DESC"
    (tenantId, passId)
  pure $ map rowToVersion rows

-- | Get the active version for a passport.
getActiveVersion
  :: Connection
  -> TenantId
  -> PassportId
  -> IO (Maybe PassportVersion)
getActiveVersion conn tenantId passId = do
  rows <- PG.query conn
    "SELECT id, passport_id, tenant_id, version_number, snapshot_id, \
    \       rules_version_id, payload_hash, proof_hash, receipt_hash, \
    \       qr_payload, status, signature, signed_at, activated_at, \
    \       revoked_at, created_at \
    \FROM passport_versions \
    \WHERE tenant_id = ? AND passport_id = ? AND status = 'ACTIVE'"
    (tenantId, passId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToVersion row

-- | Activate a passport version.
--
-- Transactionally supersedes any current ACTIVE version and activates this one.
--
-- @since 0.1.0.0
activate
  :: Connection
  -> TenantId
  -> PassportVersionId
  -> IO (Either ActivateError ())
activate conn tenantId versionId = do
  mVersion <- getPassportVersion conn tenantId versionId
  case mVersion of
    Nothing -> pure $ Left $ ActivateVersionNotFound versionId
    Just ver
      | pvStatus ver == PassActive -> pure $ Left $ ActivateAlreadyActive versionId
      | pvStatus ver == PassRevoked -> pure $ Left $ ActivateAlreadyRevoked versionId
      | otherwise -> do
          -- Transaction: supersede old, activate new
          PG.withTransaction conn $ do
            -- Supersede any current ACTIVE version
            _ <- PG.execute conn
              "UPDATE passport_versions SET status = 'SUPERSEDED' \
              \WHERE tenant_id = ? AND passport_id = ? AND status = 'ACTIVE'"
              (tenantId, pvPassportId ver)
            -- Activate this version
            _ <- PG.execute conn
              "UPDATE passport_versions SET status = 'ACTIVE', activated_at = NOW() \
              \WHERE tenant_id = ? AND id = ?"
              (tenantId, versionId)
            pure ()
          pure $ Right ()

-- | Revoke a passport version.
--
-- Only ACTIVE versions can be revoked.
--
-- @since 0.1.0.0
revoke
  :: Connection
  -> TenantId
  -> PassportVersionId
  -> IO (Either RevokeError ())
revoke conn tenantId versionId = do
  mVersion <- getPassportVersion conn tenantId versionId
  case mVersion of
    Nothing -> pure $ Left $ RevokeVersionNotFound versionId
    Just ver
      | pvStatus ver == PassRevoked -> pure $ Left $ RevokeAlreadyRevoked versionId
      | pvStatus ver /= PassActive -> pure $ Left $ RevokeNotActive versionId
      | otherwise -> do
          _ <- PG.execute conn
            "UPDATE passport_versions SET status = 'REVOKED', revoked_at = NOW() \
            \WHERE tenant_id = ? AND id = ?"
            (tenantId, versionId)
          pure $ Right ()

-- | Convert database row to PassportVersion.
rowToVersion
  :: (UUID, UUID, UUID, Int, UUID, UUID, Text, Text, Text, Text, Text,
      Maybe Text, Maybe UTCTime, Maybe UTCTime, Maybe UTCTime, UTCTime)
  -> PassportVersion
rowToVersion (vid, pid, tid, vnum, snapId, rulesId, phash, prhash, rhash,
              qr, status, sig, sigAt, actAt, revAt, cat) = PassportVersion
  { pvId = vid
  , pvPassportId = pid
  , pvTenantId = tid
  , pvVersionNumber = vnum
  , pvSnapshotId = snapId
  , pvRulesVersionId = rulesId
  , pvPayloadHash = phash
  , pvProofHash = prhash
  , pvReceiptHash = rhash
  , pvQrPayload = qr
  , pvStatus = maybe PassCompiling id $ textToStatus status
  , pvSignature = sig
  , pvSignedAt = sigAt
  , pvActivatedAt = actAt
  , pvRevokedAt = revAt
  , pvCreatedAt = cat
  }
