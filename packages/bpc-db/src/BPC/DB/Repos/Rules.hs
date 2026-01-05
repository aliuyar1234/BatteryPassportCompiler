{-# LANGUAGE OverloadedStrings #-}

-- | Rules Repository
--
-- Manage rule packages and versions with publish validation.
-- Publishing requires >= 500 passing test cases.
--
-- @since 0.1.0.0
module BPC.DB.Repos.Rules
  ( -- * Types
    RulePackageId
  , RuleVersionId
  , RulePackageStatus(..)
  , RulePackage(..)
  , RuleVersion(..)
  , PackageInput(..)
  , VersionInput(..)
  , TestRunInput(..)
  , TestRun(..)
    -- * Package Operations
  , createPackage
  , getPackage
  , getPackageByName
  , listPackages
    -- * Version Operations
  , createVersion
  , getRuleVersion
  , getVersionsByPackage
  , getPublishedVersion
    -- * Publish Operations
  , publishVersion
  , recordTestRun
  , getTestRuns
    -- * Constants
  , minTestCases
  ) where

import Control.Exception (catch)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Database.PostgreSQL.Simple (Connection, Only(..), SqlError)
import qualified Database.PostgreSQL.Simple as PG
import Data.Word (Word8)
import GHC.Generics (Generic)

import BPC.DB.Error (PublishError(..), RuleError(..))
import BPC.DB.Repos.Events (TenantId)

-- | Rule Package ID (UUID)
type RulePackageId = UUID

-- | Rule Version ID (UUID)
type RuleVersionId = UUID

-- | Minimum test cases required for publishing.
minTestCases :: Int
minTestCases = 500

-- | Rule package status.
data RulePackageStatus
  = RuleDraft
  | RuleValidated
  | RulePublished
  | RuleDeprecated
  | RuleRetired
  deriving stock (Show, Eq, Generic)

statusToText :: RulePackageStatus -> Text
statusToText = \case
  RuleDraft      -> "DRAFT"
  RuleValidated  -> "VALIDATED"
  RulePublished  -> "PUBLISHED"
  RuleDeprecated -> "DEPRECATED"
  RuleRetired    -> "RETIRED"

textToStatus :: Text -> Maybe RulePackageStatus
textToStatus = \case
  "DRAFT"      -> Just RuleDraft
  "VALIDATED"  -> Just RuleValidated
  "PUBLISHED"  -> Just RulePublished
  "DEPRECATED" -> Just RuleDeprecated
  "RETIRED"    -> Just RuleRetired
  _            -> Nothing

-- | Rule package record.
data RulePackage = RulePackage
  { rpId :: RulePackageId
  , rpTenantId :: TenantId
  , rpName :: Text
  , rpDescription :: Maybe Text
  , rpCreatedAt :: UTCTime
  , rpUpdatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Rule version record.
data RuleVersion = RuleVersion
  { rvId :: RuleVersionId
  , rvPackageId :: RulePackageId
  , rvTenantId :: TenantId
  , rvVersionNumber :: Int
  , rvDslSource :: Text
  , rvDslHash :: Text
  , rvTestsHash :: Maybe Text
  , rvStatus :: RulePackageStatus
  , rvPublishedAt :: Maybe UTCTime
  , rvCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Test run record.
data TestRun = TestRun
  { trId :: UUID
  , trVersionId :: RuleVersionId
  , trTenantId :: TenantId
  , trTotalCases :: Int
  , trPassedCases :: Int
  , trFailedCases :: Int
  , trRunAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating a rule package.
data PackageInput = PackageInput
  { piName :: Text
  -- ^ Package name (unique per tenant)
  , piDescription :: Maybe Text
  -- ^ Optional description
  }
  deriving stock (Show, Eq, Generic)

-- | Input for creating a rule version.
data VersionInput = VersionInput
  { viDslSource :: Text
  -- ^ DSL source code
  , viTestsSource :: Maybe Text
  -- ^ Test cases source
  }
  deriving stock (Show, Eq, Generic)

-- | Input for recording a test run.
data TestRunInput = TestRunInput
  { triTotalCases :: Int
  -- ^ Total test cases
  , triPassedCases :: Int
  -- ^ Passed test cases
  , triFailedCases :: Int
  -- ^ Failed test cases
  }
  deriving stock (Show, Eq, Generic)

-- | Calculate SHA-256 hash of text.
sha256Text :: Text -> Text
sha256Text content =
  let bytes = TE.encodeUtf8 content
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

-- | Create a new rule package.
--
-- @since 0.1.0.0
createPackage
  :: Connection
  -> TenantId
  -> PackageInput
  -> IO (Either RuleError RulePackageId)
createPackage conn tenantId input = do
  result <- (Right <$> PG.query conn
    "INSERT INTO rule_packages (tenant_id, name, description) \
    \VALUES (?, ?, ?) RETURNING id"
    (tenantId, piName input, piDescription input))
    `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ RULE_PACKAGE_NOT_FOUND (error "No ID returned")
    Right (Only pid : _) -> pure $ Right pid
  where
    handleSqlError :: SqlError -> IO (Either RuleError [Only UUID])
    handleSqlError _ = pure $ Left $ RULE_PARSE_ERROR "Constraint violation"

-- | Get a rule package by ID.
getPackage
  :: Connection
  -> TenantId
  -> RulePackageId
  -> IO (Maybe RulePackage)
getPackage conn tenantId pkgId = do
  rows <- PG.query conn
    "SELECT id, tenant_id, name, description, created_at, updated_at \
    \FROM rule_packages WHERE tenant_id = ? AND id = ?"
    (tenantId, pkgId)
  pure $ case rows of
    [] -> Nothing
    ((pid, tid, name, desc, cat, uat) : _) ->
      Just RulePackage
        { rpId = pid
        , rpTenantId = tid
        , rpName = name
        , rpDescription = desc
        , rpCreatedAt = cat
        , rpUpdatedAt = uat
        }

-- | Get a rule package by name.
getPackageByName
  :: Connection
  -> TenantId
  -> Text
  -> IO (Maybe RulePackage)
getPackageByName conn tenantId name = do
  rows <- PG.query conn
    "SELECT id, tenant_id, name, description, created_at, updated_at \
    \FROM rule_packages WHERE tenant_id = ? AND name = ?"
    (tenantId, name)
  pure $ case rows of
    [] -> Nothing
    ((pid, tid, n, desc, cat, uat) : _) ->
      Just RulePackage
        { rpId = pid
        , rpTenantId = tid
        , rpName = n
        , rpDescription = desc
        , rpCreatedAt = cat
        , rpUpdatedAt = uat
        }

-- | List rule packages with cursor pagination.
--
-- Fetches packages ordered by (created_at DESC, id DESC).
-- If cursor is provided (timestamp, id), returns packages created before that point.
--
-- @since 0.1.0.0
listPackages
  :: Connection
  -> TenantId
  -> Int
  -- ^ Limit (number of items to fetch)
  -> Maybe (UTCTime, UUID)
  -- ^ Optional cursor (timestamp, id)
  -> IO [RulePackage]
listPackages conn tenantId limit cursor = do
  rows <- case cursor of
    Nothing ->
      PG.query conn
        "SELECT id, tenant_id, name, description, created_at, updated_at \
        \FROM rule_packages \
        \WHERE tenant_id = ? \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, limit)
    Just (timestamp, cursorId) ->
      PG.query conn
        "SELECT id, tenant_id, name, description, created_at, updated_at \
        \FROM rule_packages \
        \WHERE tenant_id = ? AND (created_at, id) < (?, ?) \
        \ORDER BY created_at DESC, id DESC \
        \LIMIT ?"
        (tenantId, timestamp, cursorId, limit)

  pure $ map rowToPackage rows
  where
    rowToPackage (pid, tid, name, desc, cat, uat) = RulePackage
      { rpId = pid
      , rpTenantId = tid
      , rpName = name
      , rpDescription = desc
      , rpCreatedAt = cat
      , rpUpdatedAt = uat
      }

-- | Create a new rule version.
--
-- Calculates DSL hash and tests hash.
--
-- @since 0.1.0.0
createVersion
  :: Connection
  -> TenantId
  -> RulePackageId
  -> VersionInput
  -> IO (Either RuleError RuleVersionId)
createVersion conn tenantId pkgId input = do
  -- Get next version number
  versionNum <- getNextVersionNumber conn tenantId pkgId

  -- Calculate hashes
  let dslHash = sha256Text $ viDslSource input
  let testsHash = sha256Text <$> viTestsSource input

  result <- (Right <$> PG.query conn
    "INSERT INTO rule_package_versions \
    \(tenant_id, package_id, version_number, dsl_source, dsl_sha256, \
    \ tests_source, tests_sha256, status) \
    \VALUES (?, ?, ?, ?, ?, ?, ?, 'DRAFT') \
    \RETURNING id"
    ( tenantId
    , pkgId
    , versionNum
    , viDslSource input
    , dslHash
    , viTestsSource input
    , testsHash
    )) `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ RULE_VERSION_NOT_FOUND (error "No ID returned")
    Right (Only vid : _) -> pure $ Right vid
  where
    handleSqlError :: SqlError -> IO (Either RuleError [Only UUID])
    handleSqlError _ = pure $ Left $ RULE_PACKAGE_NOT_FOUND pkgId

-- | Get next version number for a package.
getNextVersionNumber :: Connection -> TenantId -> RulePackageId -> IO Int
getNextVersionNumber conn tenantId pkgId = do
  [Only maxVer] <- PG.query conn
    "SELECT COALESCE(MAX(version_number), 0) FROM rule_package_versions \
    \WHERE tenant_id = ? AND package_id = ?"
    (tenantId, pkgId)
  pure $ maxVer + 1

-- | Get a rule version by ID.
getRuleVersion
  :: Connection
  -> TenantId
  -> RuleVersionId
  -> IO (Maybe RuleVersion)
getRuleVersion conn tenantId versionId = do
  rows <- PG.query conn
    "SELECT id, package_id, tenant_id, version_number, dsl_source, \
    \       dsl_sha256, tests_sha256, status, published_at, created_at \
    \FROM rule_package_versions WHERE tenant_id = ? AND id = ?"
    (tenantId, versionId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToVersion row

-- | Get all versions for a package.
getVersionsByPackage
  :: Connection
  -> TenantId
  -> RulePackageId
  -> IO [RuleVersion]
getVersionsByPackage conn tenantId pkgId = do
  rows <- PG.query conn
    "SELECT id, package_id, tenant_id, version_number, dsl_source, \
    \       dsl_sha256, tests_sha256, status, published_at, created_at \
    \FROM rule_package_versions \
    \WHERE tenant_id = ? AND package_id = ? \
    \ORDER BY version_number DESC"
    (tenantId, pkgId)
  pure $ map rowToVersion rows

-- | Get the latest published version for a package.
getPublishedVersion
  :: Connection
  -> TenantId
  -> RulePackageId
  -> IO (Maybe RuleVersion)
getPublishedVersion conn tenantId pkgId = do
  rows <- PG.query conn
    "SELECT id, package_id, tenant_id, version_number, dsl_source, \
    \       dsl_sha256, tests_sha256, status, published_at, created_at \
    \FROM rule_package_versions \
    \WHERE tenant_id = ? AND package_id = ? AND status = 'PUBLISHED' \
    \ORDER BY version_number DESC LIMIT 1"
    (tenantId, pkgId)
  pure $ case rows of
    [] -> Nothing
    (row : _) -> Just $ rowToVersion row

-- | Publish a rule version.
--
-- Requires >= 500 passing test cases. Returns PublishInsufficientTests
-- if test requirements not met.
--
-- @since 0.1.0.0
publishVersion
  :: Connection
  -> TenantId
  -> RuleVersionId
  -> IO (Either PublishError ())
publishVersion conn tenantId versionId = do
  mVersion <- getRuleVersion conn tenantId versionId
  case mVersion of
    Nothing -> pure $ Left $ PublishVersionNotFound versionId
    Just ver
      | rvStatus ver == RulePublished -> pure $ Left $ PublishAlreadyPublished versionId
      | otherwise -> do
          -- Check test runs
          runs <- getTestRuns conn tenantId versionId
          let totalPassed = sum $ map trPassedCases runs
          let anyFailed = any (\r -> trFailedCases r > 0) runs
          case runs of
            [] -> pure $ Left $ PublishTestsNotRun versionId
            _
              | anyFailed -> pure $ Left $ PublishTestsFailed versionId
              | totalPassed < minTestCases ->
                  pure $ Left $ PublishInsufficientTests versionId totalPassed minTestCases
              | otherwise -> do
                  _ <- PG.execute conn
                    "UPDATE rule_package_versions SET status = 'PUBLISHED', published_at = NOW() \
                    \WHERE tenant_id = ? AND id = ?"
                    (tenantId, versionId)
                  pure $ Right ()

-- | Record a test run for a rule version.
--
-- @since 0.1.0.0
recordTestRun
  :: Connection
  -> TenantId
  -> RuleVersionId
  -> TestRunInput
  -> IO (Either RuleError UUID)
recordTestRun conn tenantId versionId input = do
  result <- (Right <$> PG.query conn
    "INSERT INTO rule_test_runs \
    \(tenant_id, version_id, total_cases, passed_cases, failed_cases) \
    \VALUES (?, ?, ?, ?, ?) \
    \RETURNING id"
    ( tenantId
    , versionId
    , triTotalCases input
    , triPassedCases input
    , triFailedCases input
    )) `catch` handleSqlError

  case result of
    Left err -> pure $ Left err
    Right [] -> pure $ Left $ RULE_VERSION_NOT_FOUND versionId
    Right (Only runId : _) -> pure $ Right runId
  where
    handleSqlError :: SqlError -> IO (Either RuleError [Only UUID])
    handleSqlError _ = pure $ Left $ RULE_VERSION_NOT_FOUND versionId

-- | Get all test runs for a rule version.
getTestRuns
  :: Connection
  -> TenantId
  -> RuleVersionId
  -> IO [TestRun]
getTestRuns conn tenantId versionId = do
  rows <- PG.query conn
    "SELECT id, version_id, tenant_id, total_cases, passed_cases, \
    \       failed_cases, run_at \
    \FROM rule_test_runs \
    \WHERE tenant_id = ? AND version_id = ? \
    \ORDER BY run_at DESC"
    (tenantId, versionId)
  pure $ map rowToTestRun rows
  where
    rowToTestRun (rid, vid, tid, total, passed, failed, runAt) = TestRun
      { trId = rid
      , trVersionId = vid
      , trTenantId = tid
      , trTotalCases = total
      , trPassedCases = passed
      , trFailedCases = failed
      , trRunAt = runAt
      }

-- | Convert database row to RuleVersion.
rowToVersion
  :: (UUID, UUID, UUID, Int, Text, Text, Maybe Text, Text, Maybe UTCTime, UTCTime)
  -> RuleVersion
rowToVersion (vid, pkgId, tid, vnum, dsl, dslHash, testsHash, status, pubAt, cat) =
  RuleVersion
    { rvId = vid
    , rvPackageId = pkgId
    , rvTenantId = tid
    , rvVersionNumber = vnum
    , rvDslSource = dsl
    , rvDslHash = dslHash
    , rvTestsHash = testsHash
    , rvStatus = maybe RuleDraft id $ textToStatus status
    , rvPublishedAt = pubAt
    , rvCreatedAt = cat
    }
