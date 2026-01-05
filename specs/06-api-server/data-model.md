# Data Model: API Server

**Feature**: 06-api-server
**Date**: 2025-12-28
**SSOT Reference**: Section 10 (API)

## Request/Response Types

### Authentication

```haskell
-- Authentication context (from middleware)
data AuthContext = AuthContext
  { acTenantId    :: TenantId
  , acActorId     :: ActorId
  , acPermissions :: Set Permission
  }

-- API error codes (from SSOT 2.4)
data AppError
  = UNAUTHORIZED
  | API_KEY_REVOKED
  | FORBIDDEN Permission
  | NOT_FOUND
  | VALIDATION_ERROR Text
  | IDEMPOTENCY_CONFLICT
  | SNAPSHOT_SEALED
  | SNAPSHOT_NOT_READY
  | RULE_PKG_NOT_PUBLISHED
  | RATE_LIMITED Int  -- retry_after
  | INTERNAL_ERROR
  deriving (Eq, Show)
```

### Pagination

```haskell
-- Query params
data PaginationParams = PaginationParams
  { ppLimit  :: Int        -- 1-200, default 50
  , ppCursor :: Maybe Text -- Opaque base64 cursor
  }

-- Response wrapper
data CursorPage a = CursorPage
  { cpItems      :: [a]
  , cpNextCursor :: Maybe Text
  }

instance ToJSON a => ToJSON (CursorPage a) where
  toJSON cp = object
    [ "items" .= cpItems cp
    , "next_cursor" .= cpNextCursor cp
    ]
```

### Error Envelope

```haskell
data ErrorEnvelope = ErrorEnvelope
  { eeCode          :: Text
  , eeMessage       :: Text
  , eeCorrelationId :: UUID
  , eeDetails       :: Maybe Value
  }

instance ToJSON ErrorEnvelope where
  toJSON ee = object
    [ "error" .= object
        [ "code" .= eeCode ee
        , "message" .= eeMessage ee
        , "correlation_id" .= eeCorrelationId ee
        , "details" .= eeDetails ee
        ]
    ]
```

### Document Endpoints

```haskell
-- POST /v1/documents
data CreateDocumentRequest = CreateDocumentRequest
  { cdrKind        :: DocumentKind
  , cdrExternalRef :: Maybe Text
  }

data CreateDocumentResponse = CreateDocumentResponse
  { cdrDocumentId :: DocumentId
  }

-- POST /v1/documents/{id}/versions
data UploadVersionRequest = UploadVersionRequest
  { uvrContent  :: Text  -- Base64 encoded
  , uvrMimeType :: Text
  }

data UploadVersionResponse = UploadVersionResponse
  { uvrDocumentVersionId :: DocumentVersionId
  , uvrSha256            :: Text
  }

-- GET /v1/documents
data DocumentSummary = DocumentSummary
  { dsDocumentId  :: DocumentId
  , dsKind        :: DocumentKind
  , dsExternalRef :: Maybe Text
  , dsCreatedAt   :: UTCTime
  }
```

### Snapshot Endpoints

```haskell
-- POST /v1/snapshots
data CreateSnapshotRequest = CreateSnapshotRequest
  { csrLabel :: Maybe Text
  }

data CreateSnapshotResponse = CreateSnapshotResponse
  { csrSnapshotId :: SnapshotId
  , csrStatus     :: SnapshotStatus
  }

-- POST /v1/snapshots/{id}/items
data AddItemRequest = AddItemRequest
  { airFactId :: FactId
  }

-- POST /v1/snapshots/{id}/seal
data SealSnapshotResponse = SealSnapshotResponse
  { ssrSnapshotId   :: SnapshotId
  , ssrSnapshotHash :: Text
  , ssrSealedAt     :: UTCTime
  }
```

### Passport Endpoints

```haskell
-- POST /v1/passports
data CreatePassportRequest = CreatePassportRequest
  { cprBatteryProductId :: BatteryProductId
  }

-- POST /v1/passports/{id}/compile
data CompileRequest = CompileRequest
  { crSnapshotId           :: SnapshotId
  , crRulePackageVersionId :: RulePackageVersionId
  }

data CompileResponse = CompileResponse
  { crPassportVersionId :: PassportVersionId
  , crJobId             :: JobId
  , crStatus            :: Text  -- "ACCEPTED"
  }

-- GET /v1/passport-versions/{id}
data PassportVersionResponse = PassportVersionResponse
  { pvrPassportVersionId :: PassportVersionId
  , pvrPassportId        :: PassportId
  , pvrStatus            :: PassportStatus
  , pvrCreatedAt         :: UTCTime
  , pvrPayloadHash       :: Text
  , pvrProofRootHash     :: Text
  , pvrReceiptHash       :: Text
  }

-- POST /v1/passport-versions/{id}/replay
data ReplayResponse = ReplayResponse
  { rrVerification :: Text  -- "passed" | "failed"
  , rrDetails      :: Maybe Value
  }
```

### Health Endpoints

```haskell
-- GET /v1/health/live
data LivenessResponse = LivenessResponse
  { lrStatus :: Text  -- "OK"
  }

-- GET /v1/health/ready
data ReadinessResponse = ReadinessResponse
  { rrStatus :: Text  -- "OK" | "DEGRADED"
  , rrChecks :: Map Text CheckResult
  }

data CheckResult = CheckResult
  { crStatus  :: Text  -- "OK" | "FAILED"
  , crMessage :: Maybe Text
  }
```

### Audit Endpoints

```haskell
-- GET /v1/audit/events
data AuditEventSummary = AuditEventSummary
  { aesEventId        :: EventId
  , aesAggregateType  :: Text
  , aesAggregateId    :: UUID
  , aesEventType      :: Text
  , aesOccurredAt     :: UTCTime
  , aesActorId        :: Maybe ActorId
  }
```

### Webhook Endpoints

```haskell
-- POST /v1/webhook-endpoints
data CreateWebhookEndpointRequest = CreateWebhookEndpointRequest
  { cwerUrl    :: Text
  , cwerSecret :: Text  -- Base64 encoded
  }

-- POST /v1/webhook-subscriptions
data CreateWebhookSubscriptionRequest = CreateWebhookSubscriptionRequest
  { cwsrEndpointId :: WebhookEndpointId
  , cwsrEventType  :: Text  -- e.g., "passport.version.active"
  }
```

### GraphQL

```haskell
-- POST /v1/graphql
data GraphQLRequest = GraphQLRequest
  { grQuery     :: Text
  , grVariables :: Maybe Value
  }

data GraphQLResponse = GraphQLResponse
  { grData   :: Maybe Value
  , grErrors :: Maybe [GraphQLError]
  }

data GraphQLError = GraphQLError
  { geMessage   :: Text
  , geLocations :: Maybe [GraphQLLocation]
  , gePath      :: Maybe [Text]
  }
```
