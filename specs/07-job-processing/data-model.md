# Data Model: Job Processing (Worker)

**Feature**: 07-job-processing
**Date**: 2025-12-28
**SSOT Reference**: Sections 2.2, 3.5.4, 6.2 (jobs table), 14.3-14.5

## Overview

The worker processes jobs from the `jobs` table. Each job type has specific input payloads and output artifacts. This document defines all data structures, handler types, configuration, and error types used by the worker.

---

## Job Core Types

### Job (Database Row)

```haskell
data Job = Job
  { jJobId           :: JobId             -- Primary key (UUID)
  , jTenantId        :: TenantId          -- Multi-tenancy scope
  , jType            :: JobType           -- Job type enum (see below)
  , jStatus          :: JobStatus         -- QUEUED | RUNNING | SUCCEEDED | FAILED | CANCELLED | DEAD_LETTER
  , jPayload         :: Value             -- JSON payload (job-type specific)
  , jAttempts        :: Int               -- Current attempt count (starts at 0)
  , jMaxAttempts     :: Int               -- Max attempts before DEAD_LETTER (default 5)
  , jPriority        :: Int               -- Lower = higher priority (0-3)
  , jScheduledAt     :: UTCTime           -- When job should run
  , jStartedAt       :: Maybe UTCTime     -- When job started (RUNNING)
  , jCompletedAt     :: Maybe UTCTime     -- When job completed
  , jLeaseOwner      :: Maybe Text        -- Worker ID holding lease
  , jLeaseExpiresAt  :: Maybe UTCTime     -- When lease expires
  , jLastError       :: Maybe Value       -- Last error JSON (if failed)
  , jCreatedAt       :: UTCTime           -- When job was enqueued
  }
```

### JobType Enum (SSOT 2.2)

```haskell
data JobType
  = INGEST_DOCUMENT      -- Store document content (future)
  | PARSE_FACTS          -- Parse DocumentVersion → Facts
  | BUILD_SNAPSHOT       -- Snapshot BUILDING → READY
  | COMPILE_PASSPORT     -- Snapshot + Rules → PassportVersion
  | RUN_RULE_TESTS       -- Run rule tests (500+ cases required)
  | SIGN_PASSPORT        -- Sign receipt with ED25519
  | GENERATE_QR          -- Generate QR PNG + payload
  | EXPORT_PASSPORT      -- Export passport to file (future)
  | DELIVER_WEBHOOK      -- Send webhook with HMAC signature
  deriving (Eq, Show, Read, Enum, Bounded)
```

### JobStatus Enum (SSOT 2.2)

```haskell
data JobStatus
  = QUEUED        -- Waiting to be processed
  | RUNNING       -- Currently being processed (lease active)
  | SUCCEEDED     -- Completed successfully
  | FAILED        -- Permanent failure (non-retryable error)
  | CANCELLED     -- Manually cancelled
  | DEAD_LETTER   -- Exceeded max attempts
  deriving (Eq, Show, Read, Enum, Bounded)
```

---

## Job Payload Structures

Each job type has a specific JSON payload structure in the `payload` column.

### PARSE_FACTS Payload

```haskell
data ParseFactsPayload = ParseFactsPayload
  { pfpDocumentVersionId :: DocumentVersionId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "document_version_id": "550e8400-e29b-41d4-a716-446655440000"
-- }
```

**Output**: Facts inserted into `facts` table with:
- `fact_type` (e.g., "Battery", "PCF")
- `fact_key` (e.g., "battery:SKU-123")
- `payload` (JSONB)
- `payload_canonical` (bytea)
- `payload_hash` (bytea SHA-256)

**Status Transitions**:
- DocumentVersion: UPLOADED → VALIDATED (success) or REJECTED (failure)

---

### BUILD_SNAPSHOT Payload

```haskell
data BuildSnapshotPayload = BuildSnapshotPayload
  { bspSnapshotId :: SnapshotId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "snapshot_id": "123e4567-e89b-12d3-a456-426614174000"
-- }
```

**Output**: Snapshot status updated to READY (no artifacts created)

**Status Transitions**:
- Snapshot: BUILDING → READY

---

### COMPILE_PASSPORT Payload

```haskell
data CompilePassportPayload = CompilePassportPayload
  { cppPassportId           :: PassportId
  , cppSnapshotId           :: SnapshotId
  , cppRulePackageVersionId :: RulePackageVersionId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "passport_id": "passport-uuid",
--   "snapshot_id": "snapshot-uuid",
--   "rule_package_version_id": "rules-uuid"
--   }
```

**Output**: PassportVersion created with:
- `payload_canonical` (bytea)
- `payload_hash` (bytea)
- `proof_canonical` (bytea)
- `proof_root_hash` (bytea)
- `receipt_canonical` (bytea unsigned)
- `receipt_hash` (bytea)
- `compiler_build_id` (text)
- `issued_at` (timestamptz)
- `status` = COMPILING

**Follow-up**: SIGN_PASSPORT job auto-enqueued

**Preconditions**:
- Snapshot status MUST be SEALED
- RulePackageVersion status MUST be PUBLISHED

---

### SIGN_PASSPORT Payload

```haskell
data SignPassportPayload = SignPassportPayload
  { sppPassportVersionId :: PassportVersionId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "passport_version_id": "pv-uuid"
-- }
```

**Output**: PassportVersion updated with:
- `signature` (bytea, 64 bytes ED25519)
- `signing_key_id` (text)
- `signing_key_public` (bytea, 32 bytes public key)
- `status` = SIGNED

**Follow-up**: GENERATE_QR job auto-enqueued

**Preconditions**:
- PassportVersion status MUST be COMPILING
- ENV `BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64` MUST be set

---

### GENERATE_QR Payload

```haskell
data GenerateQRPayload = GenerateQRPayload
  { gqpPassportVersionId :: PassportVersionId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "passport_version_id": "pv-uuid"
-- }
```

**Output**: PassportVersion updated with:
- `qr_payload` (text, BPC-QR-1 format)
- `qr_png` (bytea, PNG image)
- `status` remains SIGNED (no transition)

**QR Payload Format** (BPC-QR-1, SSOT 7.8):
```
BPC1|pv=<uuid>|ph=<base32>|pr=<base32>|rh=<base32>
```

**Preconditions**:
- PassportVersion status MUST be SIGNED

---

### RUN_RULE_TESTS Payload

```haskell
data RunRuleTestsPayload = RunRuleTestsPayload
  { rrtpRulePackageVersionId :: RulePackageVersionId
  , rrtpSeed                 :: Int64  -- Random seed for test execution
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "rule_package_version_id": "rpv-uuid",
--   "seed": 42
-- }
```

**Output**: Test run result inserted into `rule_tests_runs` table:
- `total_cases` (int)
- `passed_count` (int)
- `failed_count` (int)
- `status` ("PASSED" | "FAILED")
- `first_failure` (jsonb, optional)

**Status Transitions**:
- RulePackageVersion: DRAFT → VALIDATED (if tests PASSED AND passed_count >= 500)

**Preconditions**:
- RulePackageVersion MUST have `tests_source` (not null)

---

### DELIVER_WEBHOOK Payload

```haskell
data DeliverWebhookPayload = DeliverWebhookPayload
  { dwpWebhookDeliveryId :: WebhookDeliveryId
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "webhook_delivery_id": "delivery-uuid"
-- }
```

**Output**: `webhook_deliveries` row updated:
- `status` = DELIVERED (success) or FAILED (error)
- `http_status` (int, if received)
- `response_headers` (jsonb, if received)
- `response_body` (bytea, if received)
- `delivered_at` (timestamptz, if success)
- `error_message` (text, if failure)

**HTTP Request** (SSOT 7.10):
- Method: POST
- Headers:
  - `Content-Type: application/json`
  - `X-BPC-Signature: sha256=<hex_hmac>`
  - `X-BPC-Event-Type: <event_type>`
  - `X-BPC-Delivery-Id: <delivery_id>`
- Body: JSON event payload
- Timeout: 30 seconds

**Preconditions**:
- `webhook_deliveries` row MUST exist
- `webhook_endpoints` row MUST exist (for URL and secret)
- `events` row MUST exist (for payload)

---

## Worker Configuration

### WorkerConfig Type

```haskell
data WorkerConfig = WorkerConfig
  { wcWorkerId         :: Text     -- Unique worker ID (e.g., "worker-1")
  , wcPollIntervalMs   :: Int      -- DB poll interval in milliseconds (default 1000)
  , wcLeaseSeconds     :: Int      -- Lease TTL in seconds (default 60)
  , wcLeaseRenewSecs   :: Int      -- Lease renewal interval in seconds (default 30)
  , wcBuildId          :: Text     -- Compiler build ID (from ENV)
  , wcSigningKeyId     :: Text     -- Signing key identifier (default "dev-key-1")
  }
  deriving (Eq, Show)
```

### Configuration Loading (from ENV)

```haskell
loadWorkerConfig :: IO WorkerConfig
loadWorkerConfig = WorkerConfig
  <$> requireEnv "BPC_WORKER_ID"
  <*> envInt "BPC_JOBS_POLL_INTERVAL_MS" 1000
  <*> envInt "BPC_JOBS_LEASE_SECONDS" 60
  <*> envInt "BPC_JOBS_LEASE_RENEW_SECONDS" 30
  <*> requireEnv "BPC_COMPILER_BUILD_ID"
  <*> envWithDefault "BPC_SIGNING_KEY_ID" "dev-key-1"

-- ENV Variables (SSOT 4.6):
-- BPC_WORKER_ID                             -- REQUIRED (unique per worker instance)
-- BPC_JOBS_POLL_INTERVAL_MS                 -- Default: 1000
-- BPC_JOBS_LEASE_SECONDS                    -- Default: 60
-- BPC_JOBS_LEASE_RENEW_SECONDS              -- Default: 30
-- BPC_COMPILER_BUILD_ID                     -- REQUIRED (git commit hash)
-- BPC_SIGNING_KEY_ID                        -- Default: "dev-key-1"
-- BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64    -- REQUIRED in prod
```

---

## Handler Error Types

### HandlerError Sum Type

```haskell
data HandlerError
  -- Document/Fact errors
  = DocumentNotFound DocumentVersionId
  | DocumentRejected Text  -- Parse error details
  | UnsupportedDocumentKind DocumentKind

  -- Snapshot errors
  | SnapshotNotFound SnapshotId
  | SnapshotNotReady SnapshotId SnapshotStatus SnapshotStatus  -- expected, actual
  | SnapshotNotSealed SnapshotId
  | InvalidSnapshotState SnapshotStatus

  -- Rule errors
  | RulePackageNotFound RulePackageVersionId
  | RulesNotPublished RulePackageVersionId RulePackageStatus RulePackageStatus  -- expected, actual
  | TestParseFailed Text  -- Parse error

  -- Compilation errors
  | CompileError CompileError  -- From bpc-core
  | PayloadTooLarge Int Int    -- actual, limit
  | ProofTooLarge Int Int
  | ReceiptTooLarge Int Int

  -- Signing errors
  | SigningKeyMissing
  | InvalidSigningKey Text  -- Base64 decode error
  | InvalidPassportStatus PassportVersionId PassportStatus PassportStatus  -- expected, actual

  -- QR errors
  | QREncodeFailed Text  -- QR payload that failed

  -- Webhook errors (retryable)
  | WebhookDeliveryFailed Text  -- HTTP error details
  | WebhookTimeout
  | WebhookNon2xx Int  -- HTTP status code

  -- Job errors
  | UnsupportedJobType JobType
  | JobPayloadInvalid Text  -- JSON parse error

  deriving (Eq, Show)
```

### Retryable Classification

```haskell
isRetryable :: HandlerError -> Bool
isRetryable = \case
  -- Network/transient errors → retry
  WebhookDeliveryFailed _ -> True
  WebhookTimeout -> True
  WebhookNon2xx code -> code >= 500  -- Retry 5xx, not 4xx

  -- All other errors are permanent → FAILED
  _ -> False
```

---

## Pipeline Flow Diagram

```text
┌──────────────────────────────────────────────────────────────────┐
│                       Job Processing Pipeline                     │
└──────────────────────────────────────────────────────────────────┘

Upload DocumentVersion (via API)
  ↓
  ├─ Auto-enqueue: PARSE_FACTS
  │
  ↓
PARSE_FACTS Job (Worker)
  ├─ Parse BOM/PCF/DD document
  ├─ Insert Facts (canonical bytes + hash)
  └─ Update DocumentVersion → VALIDATED
  │
  ↓
Create Snapshot BUILDING (via API)
  ├─ Add Facts to snapshot_items
  │
  ↓
BUILD_SNAPSHOT Job (Worker, optional)
  └─ Update Snapshot → READY
  │
  ↓
Seal Snapshot (via API)
  ├─ Compute snapshot_canonical + snapshot_hash
  └─ Update Snapshot → SEALED
  │
  ↓
Trigger Compile (via API)
  ├─ Auto-enqueue: COMPILE_PASSPORT
  │
  ↓
COMPILE_PASSPORT Job (Worker)
  ├─ Load SEALED Snapshot + PUBLISHED Rules
  ├─ Call compilePassportPure (deterministic!)
  ├─ Insert PassportVersion (payload/proof/receipt)
  ├─ Status → COMPILING
  └─ Auto-enqueue: SIGN_PASSPORT
  │
  ↓
SIGN_PASSPORT Job (Worker)
  ├─ Load ED25519 private key from ENV
  ├─ Sign receipt hash
  ├─ Update PassportVersion (signature + public key)
  ├─ Status → SIGNED
  └─ Auto-enqueue: GENERATE_QR
  │
  ↓
GENERATE_QR Job (Worker)
  ├─ Build QR payload (BPC-QR-1 format)
  ├─ Generate PNG image
  ├─ Update PassportVersion (qr_payload + qr_png)
  └─ Status remains SIGNED
  │
  ↓
Activate (via API)
  ├─ Supersede previous ACTIVE version
  └─ Update PassportVersion → ACTIVE

┌───────────────────────────────────────────────────────────────┐
│                     Side Pipelines                             │
└───────────────────────────────────────────────────────────────┘

Publish RulePackageVersion (via API)
  ├─ Auto-enqueue: RUN_RULE_TESTS
  │
  ↓
RUN_RULE_TESTS Job (Worker)
  ├─ Run all tests with seed
  ├─ Insert rule_tests_runs
  └─ If PASSED + >= 500 cases: Status → VALIDATED

Event occurs (via API)
  ├─ Auto-enqueue: DELIVER_WEBHOOK (for each subscription)
  │
  ↓
DELIVER_WEBHOOK Job (Worker)
  ├─ Build JSON payload
  ├─ Compute HMAC-SHA256 signature
  ├─ POST to endpoint with X-BPC-Signature header
  └─ Update webhook_deliveries (DELIVERED or FAILED)
```

---

## Database Schema (Jobs Table)

Full DDL from SSOT 6.2:

```sql
CREATE TABLE jobs (
  job_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL REFERENCES tenants(tenant_id) ON DELETE CASCADE,
  type job_type NOT NULL,  -- ENUM: PARSE_FACTS, COMPILE_PASSPORT, etc.
  status job_status NOT NULL DEFAULT 'QUEUED',  -- ENUM: QUEUED, RUNNING, etc.
  payload jsonb NOT NULL,
  attempts int NOT NULL DEFAULT 0,
  max_attempts int NOT NULL DEFAULT 5,
  priority int NOT NULL DEFAULT 2 CHECK (priority >= 0 AND priority <= 3),
  scheduled_at timestamptz NOT NULL DEFAULT now(),
  started_at timestamptz NULL,
  completed_at timestamptz NULL,
  lease_owner text NULL,
  lease_expires_at timestamptz NULL,
  last_error jsonb NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);

CREATE INDEX jobs_by_tenant ON jobs(tenant_id);
CREATE INDEX jobs_queue_idx ON jobs(tenant_id, status, scheduled_at, priority)
  WHERE status = 'QUEUED';  -- Optimizes acquireLease query
CREATE INDEX jobs_running_idx ON jobs(tenant_id, status, lease_expires_at)
  WHERE status = 'RUNNING';  -- Monitors active jobs
```

---

## Repository Functions (bpc-db)

### Enqueue Job

```haskell
enqueue :: Connection -> TenantId -> EnqueueInput -> IO JobId

data EnqueueInput = EnqueueInput
  { eiType            :: JobType
  , eiPayload         :: Value
  , eiPriority        :: Maybe Int          -- Default: 2
  , eiMaxAttempts     :: Maybe Int          -- Default: 5
  , eiScheduledAt     :: Maybe UTCTime      -- Default: now()
  , eiIdempotencyKey  :: Maybe Text         -- Optional dedup key
  }
```

### Acquire Lease

```haskell
acquireLease :: Connection -> Text -> IO (Maybe Job)
-- Worker ID → Maybe Job
-- Uses: FOR UPDATE SKIP LOCKED
```

### Renew Lease

```haskell
renewLease :: Connection -> TenantId -> JobId -> Text -> IO ()
-- Extends lease_expires_at for RUNNING job
```

### Complete Job

```haskell
completeJob :: Connection -> TenantId -> JobId -> IO ()
-- Sets status = SUCCEEDED, completed_at = now()
```

### Fail Job

```haskell
failJob :: Connection -> TenantId -> JobId -> Value -> IO ()
-- Sets status = FAILED, last_error = error JSON
```

### Retry Job

```haskell
retryJob :: Connection -> TenantId -> JobId -> UTCTime -> Value -> IO ()
-- Sets status = QUEUED, scheduled_at = future, attempts += 1, last_error
```

### Dead Letter Job

```haskell
deadLetterJob :: Connection -> TenantId -> JobId -> Value -> IO ()
-- Sets status = DEAD_LETTER, last_error = error JSON
```

---

## Metrics & Observability

### Job Metrics (Prometheus format)

```text
# Total jobs enqueued
bpc_jobs_enqueued_total{tenant_id, job_type} counter

# Jobs currently RUNNING
bpc_jobs_running{tenant_id, job_type} gauge

# Job completion by status
bpc_jobs_completed_total{tenant_id, job_type, status} counter

# Job processing duration (seconds)
bpc_job_duration_seconds{tenant_id, job_type} histogram

# Lease renewals
bpc_lease_renewals_total{worker_id} counter

# Dead letter queue size
bpc_jobs_dead_letter{tenant_id} gauge
```

### Structured Logs (JSON format)

```json
{
  "timestamp": "2025-12-28T12:34:56Z",
  "level": "info",
  "worker_id": "worker-1",
  "job_id": "550e8400-e29b-41d4-a716-446655440000",
  "job_type": "COMPILE_PASSPORT",
  "tenant_id": "tenant-uuid",
  "status": "SUCCEEDED",
  "duration_ms": 1234,
  "attempts": 1,
  "message": "Job completed successfully"
}
```

---

## References

- **SSOT 2.2**: job_type, job_status enums
- **SSOT 3.5.4**: bpc-worker exports (handlers, types)
- **SSOT 4.6**: ENV schema (worker config)
- **SSOT 6.2**: jobs table DDL
- **SSOT 7.8**: BPC-QR-1 format
- **SSOT 7.10**: Webhook delivery (HMAC signature)
- **SSOT 14.3-14.5**: Worker tasks (P2-T3, P3-T2)

---

---

## Future Job Types (SSOT Enum, Not Current Scope)

The following job types are defined in the SSOT `job_type` enum but handlers are not implemented in the current MVP scope. Payload structures are provisional.

### INGEST_DOCUMENT (Future)

**Purpose**: Automatic document ingestion with file parsing/OCR.

```haskell
-- PROVISIONAL - Subject to change
data IngestDocumentPayload = IngestDocumentPayload
  { idpDocumentId :: DocumentId      -- Parent document
  , idpFileUrl    :: Text            -- Source file location (S3/local)
  , idpMimeType   :: Text            -- MIME type for parser selection
  , idpOptions    :: Maybe Value     -- Parser-specific options
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Example JSON:
-- {
--   "document_id": "doc-uuid",
--   "file_url": "s3://bucket/file.pdf",
--   "mime_type": "application/pdf"
-- }
```

**Output**: DocumentVersion created → auto-triggers PARSE_FACTS

**Blocking Issues**:
- OCR provider selection not finalized
- File storage architecture not decided
- Document format specifications pending

---

### EXPORT_PASSPORT (Future)

**Purpose**: Export signed passport to external formats (PDF, JSON-LD, XML).

```haskell
-- PROVISIONAL - Subject to change
data ExportPassportPayload = ExportPassportPayload
  { eppPassportVersionId :: PassportVersionId
  , eppFormat            :: ExportFormat       -- PDF | JSON_LD | XML
  , eppTemplate          :: Maybe Text         -- Template ID for PDF
  , eppDestination       :: Maybe Text         -- Output path/URL (optional)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data ExportFormat = PDF | JSON_LD | XML
  deriving (Eq, Show, Read, Enum, Bounded)

-- Example JSON:
-- {
--   "passport_version_id": "pv-uuid",
--   "format": "PDF",
--   "template": "eu-battery-passport-v1"
-- }
```

**Output**: Export file stored, record in `passport_exports` table

**Blocking Issues**:
- PDF template design not finalized
- JSON-LD context schema pending EU specification
- Storage/delivery mechanism not decided

---

## Next Steps

Use this data model as reference during implementation. All handler payloads must conform to these structures. All database operations must use the defined repository functions with tenant_id filtering.

Future job handlers should be implemented when:
1. Business requirements are documented
2. External dependencies (OCR, templates) are selected
3. Migration for additional tables is created
