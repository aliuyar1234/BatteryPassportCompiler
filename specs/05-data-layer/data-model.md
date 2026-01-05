# Data Model: Data Layer

**Feature**: 05-data-layer
**Date**: 2025-12-28
**SSOT Reference**: Section 6 (DDL), 7.4 (Event Hash Chain)

## Overview

The data layer maps Haskell types to PostgreSQL tables. All tables are defined in SSOT Section 6.2.

## Repository Types

### Auth Types

```haskell
data Actor = Actor
  { actorId      :: ActorId
  , tenantId     :: TenantId
  , actorType    :: ActorType
  , displayName  :: Text
  , email        :: Maybe Text
  , isActive     :: Bool
  , createdAt    :: UTCTime
  }

data ActorType = USER | API_CLIENT | SERVICE
  deriving (Eq, Show)

data ApiKey = ApiKey
  { apiKeyId  :: ApiKeyId
  , tenantId  :: TenantId
  , actorId   :: ActorId
  , keyPrefix :: Text
  , keyHash   :: ByteString
  , label     :: Maybe Text
  , createdAt :: UTCTime
  , revokedAt :: Maybe UTCTime
  }

data Role = Role
  { roleId   :: RoleId
  , tenantId :: TenantId
  , name     :: Text
  }

newtype Permission = Permission { unPermission :: Text }
  deriving (Eq, Ord, Show, IsString)
```

### Event Store Types

```haskell
data Event = Event
  { eventId          :: EventId
  , tenantId         :: TenantId
  , aggregateType    :: Text
  , aggregateId      :: UUID
  , aggregateVersion :: Int64
  , eventType        :: Text
  , occurredAt       :: UTCTime
  , actorId          :: Maybe ActorId
  , payload          :: Value
  , payloadCanonical :: ByteString
  , payloadHash      :: ByteString
  , prevEventHash    :: Maybe ByteString
  , eventHash        :: ByteString
  }

data AppendEventInput = AppendEventInput
  { aeiTenantId        :: TenantId
  , aeiAggregateType   :: Text
  , aeiAggregateId     :: UUID
  , aeiAggregateVersion :: Int64
  , aeiEventType       :: Text
  , aeiActorId         :: Maybe ActorId
  , aeiPayload         :: Value
  }
```

### Document Types

```haskell
data Document = Document
  { documentId  :: DocumentId
  , tenantId    :: TenantId
  , kind        :: DocumentKind
  , externalRef :: Maybe Text
  , createdAt   :: UTCTime
  , createdBy   :: Maybe ActorId
  }

data DocumentKind = BOM | PCF | DUE_DILIGENCE | OTHER
  deriving (Eq, Show)

data DocumentVersion = DocumentVersion
  { documentVersionId :: DocumentVersionId
  , tenantId          :: TenantId
  , documentId        :: DocumentId
  , version           :: Int
  , status            :: DocumentStatus
  , mimeType          :: Text
  , sha256            :: ByteString
  , content           :: ByteString
  , uploadedAt        :: UTCTime
  }

data DocumentStatus = UPLOADED | PARSED | VALIDATED | REJECTED
  deriving (Eq, Show)
```

### Fact Types

```haskell
data Fact = Fact
  { factId                   :: FactId
  , tenantId                 :: TenantId
  , factType                 :: Text
  , factKey                  :: Text
  , schemaVersion            :: Int
  , sourceDocumentVersionId  :: DocumentVersionId
  , payload                  :: Value
  , payloadCanonical         :: ByteString
  , payloadHash              :: ByteString
  , createdAt                :: UTCTime
  }
```

### Snapshot Types

```haskell
data Snapshot = Snapshot
  { snapshotId        :: SnapshotId
  , tenantId          :: TenantId
  , status            :: SnapshotStatus
  , label             :: Maybe Text
  , createdAt         :: UTCTime
  , sealedAt          :: Maybe UTCTime
  , createdBy         :: Maybe ActorId
  , snapshotCanonical :: Maybe ByteString
  , snapshotHash      :: Maybe ByteString
  }

data SnapshotStatus = BUILDING | READY | SEALED
  deriving (Eq, Show)

data SnapshotItem = SnapshotItem
  { siSnapshotId :: SnapshotId
  , siTenantId   :: TenantId
  , siFactId     :: FactId
  }
```

### Passport Types

```haskell
data Passport = Passport
  { passportId               :: PassportId
  , tenantId                 :: TenantId
  , batteryProductId         :: BatteryProductId
  , currentPassportVersionId :: Maybe PassportVersionId
  , createdAt                :: UTCTime
  }

data PassportVersion = PassportVersion
  { passportVersionId       :: PassportVersionId
  , tenantId                :: TenantId
  , passportId              :: PassportId
  , status                  :: PassportStatus
  , createdAt               :: UTCTime
  , activatedAt             :: Maybe UTCTime
  , supersededAt            :: Maybe UTCTime
  , revokedAt               :: Maybe UTCTime
  , snapshotId              :: SnapshotId
  , rulePackageVersionId    :: RulePackageVersionId
  , compilerBuildId         :: Text
  , payloadCanonical        :: ByteString
  , payloadHash             :: ByteString
  , proofCanonical          :: ByteString
  , proofRootHash           :: ByteString
  , receiptCanonical        :: ByteString
  , receiptHash             :: ByteString
  , signatureAlg            :: Text
  , signature               :: Maybe ByteString
  , signingKeyId            :: Maybe Text
  , qrPng                   :: Maybe ByteString
  , qrPayload               :: Maybe Text
  }

data PassportStatus = COMPILING | SIGNED | ACTIVE | SUPERSEDED | REVOKED
  deriving (Eq, Show)
```

### Job Types

```haskell
data Job = Job
  { jobId          :: JobId
  , tenantId       :: TenantId
  , jobType        :: JobType
  , status         :: JobStatus
  , priority       :: Int
  , attempts       :: Int
  , maxAttempts    :: Int
  , idempotencyKey :: Text
  , payload        :: Value
  , scheduledAt    :: UTCTime
  , startedAt      :: Maybe UTCTime
  , finishedAt     :: Maybe UTCTime
  , leaseOwner     :: Maybe Text
  , leaseExpiresAt :: Maybe UTCTime
  , lastError      :: Maybe Value
  }

data JobType
  = INGEST_DOCUMENT
  | PARSE_FACTS
  | BUILD_SNAPSHOT
  | COMPILE_PASSPORT
  | RUN_RULE_TESTS
  | SIGN_PASSPORT
  | GENERATE_QR
  | EXPORT_PASSPORT
  | DELIVER_WEBHOOK
  deriving (Eq, Show)

data JobStatus = QUEUED | RUNNING | SUCCEEDED | FAILED | CANCELLED | DEAD_LETTER
  deriving (Eq, Show)
```

### Rule Types

```haskell
data RulePackage = RulePackage
  { rulePackageId :: RulePackageId
  , tenantId      :: TenantId
  , name          :: Text
  , description   :: Maybe Text
  , createdAt     :: UTCTime
  , createdBy     :: Maybe ActorId
  }

data RulePackageVersion = RulePackageVersion
  { rulePackageVersionId :: RulePackageVersionId
  , tenantId             :: TenantId
  , rulePackageId        :: RulePackageId
  , version              :: Int
  , status               :: RulePkgStatus
  , createdAt            :: UTCTime
  , publishedAt          :: Maybe UTCTime
  , compilerMinBuild     :: Text
  , dslSource            :: ByteString
  , dslSha256            :: ByteString
  , testsSource          :: ByteString
  , testsSha256          :: ByteString
  }

data RulePkgStatus = DRAFT | VALIDATED | PUBLISHED | DEPRECATED | RETIRED
  deriving (Eq, Show)
```

## Error Types

```haskell
-- Database errors
data DBError
  = DBNotFound
  | DBUniqueViolation Text
  | DBForeignKeyViolation Text
  | DBConnectionError Text
  | DBTimeout
  deriving (Eq, Show)

-- Domain errors
data EventError = EVENT_VERSION_CONFLICT
data SealError = SNAPSHOT_NOT_READY | SNAPSHOT_ALREADY_SEALED
data PublishError = RULE_TESTS_NOT_PASSED | ALREADY_PUBLISHED
data ActivateError = VERSION_NOT_SIGNED | ALREADY_ACTIVE
data RevokeError = ALREADY_REVOKED
```

## Pagination Types

```haskell
data ListOptions = ListOptions
  { loLimit  :: Int           -- 1-200, default 50
  , loCursor :: Maybe Cursor  -- Opaque cursor
  }

data Cursor = Cursor
  { cursorTime :: UTCTime
  , cursorId   :: UUID
  }

data Page a = Page
  { pageItems      :: [a]
  , pageNextCursor :: Maybe Cursor
  }
```
