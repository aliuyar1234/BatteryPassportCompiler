# Research: Job Processing (Worker)

**Feature**: 07-job-processing
**Date**: 2025-12-28
**Status**: Complete
**SSOT References**: Sections 3.5.4, 4.6, 14.3-14.5

## Research Summary

The worker handles async job processing with database-backed leasing for distributed execution. This research document covers key architectural decisions around polling vs queue triggers, lease-based concurrency control, retry strategies, and handler patterns. All decisions are grounded in SSOT requirements and constitution principles.

---

## 1. Polling vs Message Queue

**Decision**: Hybrid approach with DB polling + optional RabbitMQ trigger

**Rationale**:
- **DB Polling (Always Available)**: Simple, requires no additional infrastructure, always works even if queue is down
- **RabbitMQ Trigger (Optional)**: Provides instant notification for lower latency, reduces polling overhead
- **Configuration**: `BPC_QUEUE_ENABLED=true` enables queue trigger; falls back to polling if queue unavailable

**DB Polling Pattern** (SSOT 4.6):
```sql
-- acquireLease query with FOR UPDATE SKIP LOCKED
SELECT job_id, tenant_id, type, status, payload, attempts, max_attempts, priority, scheduled_at
FROM jobs
WHERE status = 'QUEUED'
  AND scheduled_at <= now()
  AND (lease_expires_at IS NULL OR lease_expires_at < now())
ORDER BY priority ASC, scheduled_at ASC
FOR UPDATE SKIP LOCKED
LIMIT 1;

-- Update lease on acquisition
UPDATE jobs
SET status = 'RUNNING',
    lease_owner = :worker_id,
    lease_expires_at = now() + interval ':lease_seconds seconds',
    started_at = now()
WHERE job_id = :job_id;
```

**Why SKIP LOCKED**: Critical for horizontal scaling
- Multiple workers can poll simultaneously without blocking
- Each worker gets a different job (or none if queue empty)
- No deadlocks, no coordination overhead
- Postgres-native concurrency control

**Polling Interval**: 1000ms (configurable via `BPC_JOBS_POLL_INTERVAL_MS`)
- Fast enough for responsive job processing
- Low enough overhead for continuous operation

**RabbitMQ Integration** (optional):
- Exchange: `bpc` (configurable)
- Routing key: `jobs.trigger` (configurable)
- Message: `{"job_id": "<uuid>"}` (minimal payload, job data in DB)
- On receive: immediately poll DB for new jobs (don't trust message content)

**Failure Handling**:
- Queue unavailable → Log warning, continue with polling
- Message malformed → Ignore, continue polling
- Health check (`/v1/health/ready`): Can optionally require queue (`BPC_HEALTH_REQUIRE_QUEUE`)

---

## 2. Job Leasing & Concurrency Control

**Decision**: Database-backed leasing with expiry and renewal

**Why Leasing**:
- Prevents double-processing (critical for idempotency)
- Handles worker crashes gracefully (lease expires, job becomes available)
- Enables horizontal scaling without coordination
- Simplicity: no ZooKeeper, no etcd, just Postgres

**Lease Flow**:
1. **Acquire**: Worker selects job with `FOR UPDATE SKIP LOCKED`, updates lease_owner and lease_expires_at
2. **Renew**: Background thread extends lease every 30s (configurable via `BPC_JOBS_LEASE_RENEW_SECONDS`)
3. **Process**: Handler executes job logic
4. **Complete**: Worker updates status to SUCCEEDED/FAILED, clears lease
5. **Timeout**: If lease expires before completion, another worker can acquire

**Lease Parameters** (SSOT 4.6):
- **Lease TTL**: 60 seconds (`BPC_JOBS_LEASE_SECONDS`)
- **Renewal Interval**: 30 seconds (`BPC_JOBS_LEASE_RENEW_SECONDS`)
- **Safety Margin**: TTL is 2x renewal interval to handle network delays

**Renewal Implementation**:
```haskell
leaseRenewalLoop :: WorkerConfig -> Pool Connection -> Job -> IO ()
leaseRenewalLoop config pool job = forever $ do
  threadDelay (wcLeaseRenewSecs config * 1_000_000)  -- 30s
  withConn pool $ \conn ->
    -- Extend lease if job still RUNNING
    execute conn
      "UPDATE jobs SET lease_expires_at = now() + interval ':lease_seconds seconds' \
      \ WHERE job_id = :job_id AND status = 'RUNNING'"
      (wcLeaseSeconds config, jJobId job)
```

**Worker Crash Handling**:
- Lease expires after 60s
- Job becomes visible in `acquireLease` query again (lease_expires_at < now())
- Another worker picks up the job
- Idempotency ensures no side effects from duplicate execution

**Multi-Tenant Safety**:
- All queries filter by `tenant_id`
- Lease owner is worker-scoped, not tenant-scoped (one worker can handle multiple tenants)

---

## 3. Retry Strategy & Backoff

**Decision**: Exponential backoff with max 5 attempts

**Formula**: `delay = min(1024, 2^attempts)` seconds

**Attempts Limit**: 5 (MUST per user requirement, overrides SSOT default of 10)
- Attempt 1 (original): immediate
- Attempt 2: retry after 2s
- Attempt 3: retry after 4s
- Attempt 4: retry after 8s
- Attempt 5: retry after 16s
- After 5: DEAD_LETTER status

**Backoff Table**:
| Attempt | Delay (seconds) | Cumulative Time |
|---------|-----------------|-----------------|
| 1       | 0               | 0s              |
| 2       | 2               | 2s              |
| 3       | 4               | 6s              |
| 4       | 8               | 14s             |
| 5       | 16              | 30s             |
| 6+      | DEAD_LETTER     | -               |

**Retryable vs Non-Retryable Errors**:

**Retryable** (transient):
- `DB_UNAVAILABLE` (Postgres down, connection timeout)
- `QUEUE_UNAVAILABLE` (RabbitMQ down)
- `HttpException` (webhook delivery network errors)
- `WebhookTimeout` (30s timeout)
- `WebhookNon2xx` (5xx server errors)

**Non-Retryable** (permanent):
- `SNAPSHOT_NOT_READY` (snapshot not SEALED)
- `RULE_PKG_NOT_PUBLISHED` (rules not PUBLISHED)
- `VALIDATION_ERROR` (malformed job payload)
- `RULE_TYPE_ERROR` (DSL type error)
- `PAYLOAD_TOO_LARGE` (deterministic size violation)
- `SIGNING_KEY_MISSING` (configuration error)

**Implementation**:
```haskell
handleJobResult :: Connection -> Job -> Either SomeException () -> IO ()
handleJobResult conn job (Left err)
  | jAttempts job >= 5 =
      -- DEAD_LETTER after 5 attempts
      updateJobStatus conn (jTenantId job) (jJobId job) DEAD_LETTER (toJSON err)

  | isRetryable err = do
      -- Schedule retry with exponential backoff
      let backoff = min 1024 (2 ^ jAttempts job)
      now <- getCurrentTime
      let scheduledAt = addUTCTime (fromIntegral backoff) now
      execute conn
        "UPDATE jobs SET status = 'QUEUED', \
        \                 scheduled_at = :scheduled_at, \
        \                 attempts = attempts + 1, \
        \                 last_error = :error, \
        \                 lease_owner = NULL, \
        \                 lease_expires_at = NULL \
        \ WHERE job_id = :job_id"
        (scheduledAt, toJSON err, jJobId job)

  | otherwise =
      -- Permanent failure
      updateJobStatus conn (jTenantId job) (jJobId job) FAILED (toJSON err)
```

**Dead Letter Queue Inspection**:
- Jobs with status DEAD_LETTER remain in DB for manual inspection
- `last_error` contains JSON-encoded exception details
- Admins can manually retry via SQL: `UPDATE jobs SET status = 'QUEUED', attempts = 0`

---

## 4. Handler Isolation & Error Boundaries

**Decision**: Each job type has dedicated handler module with isolated exception handling

**Benefits**:
- **Clear Responsibilities**: Each handler owns one job type
- **Type Safety**: Handler signatures enforce correct payload structure
- **Error Isolation**: Handler exceptions don't crash worker loop
- **Testing**: Handlers unit-testable independently
- **Maintainability**: Changes to one handler don't affect others

**Handler Signature**:
```haskell
type Handler = Pool Connection -> Job -> IO ()

-- Example
parseFacts :: Handler
parseFacts pool job = do
  -- 1. Parse payload
  let docVersionId = parseDocVersionId $ jPayload job ^. key "document_version_id"

  -- 2. Load inputs
  docVersion <- withConn pool $ \conn -> getDocumentVersion conn ...

  -- 3. Execute business logic
  facts <- parseBOM (dvContent docVersion)

  -- 4. Store results
  forM_ facts $ \fact -> withConn pool $ \conn -> insertFact conn ...

  -- 5. Emit audit event
  withConn pool $ \conn -> appendEvent conn ...
```

**Exception Handling**:
```haskell
processJob :: WorkerConfig -> Pool Connection -> Job -> IO ()
processJob config pool job = case jType job of
  PARSE_FACTS      -> Handlers.parseFacts pool job
  COMPILE_PASSPORT -> Handlers.compilePassport config pool job
  -- ... (exceptions bubble up to runWorker)

runWorker :: WorkerConfig -> Pool Connection -> IO ()
runWorker config pool = forever $ do
  mJob <- withConn pool $ \conn -> acquireLease conn (wcWorkerId config)

  case mJob of
    Just job -> do
      renewThread <- async $ leaseRenewalLoop config pool job
      result <- try $ processJob config pool job  -- Catch all exceptions
      cancel renewThread
      withConn pool $ \conn -> handleJobResult conn job result
```

**Error Boundary**: `try :: IO a -> IO (Either SomeException a)` catches all exceptions
- Prevents single job failure from crashing worker
- Ensures lease renewal thread is cancelled
- Allows retry logic to run

---

## 5. QR Code Generation

**Decision**: Use `qrcode` Haskell library for PNG generation

**QR Content**: BPC-QR-1 format (SSOT 7.8)
```
BPC1|pv=<uuid>|ph=<base32>|pr=<base32>|rh=<base32>
```

**Example**:
```
BPC1|pv=550e8400-e29b-41d4-a716-446655440000|ph=ABCDEF123456|pr=GHIJKL789012|rh=MNOPQR345678
```

**Fields**:
- `BPC1`: Format version
- `pv`: PassportVersion UUID
- `ph`: Payload hash (Base32, no padding)
- `pr`: Proof root hash (Base32, no padding)
- `rh`: Receipt hash (Base32, no padding)

**Error Correction**: Level M (15% recovery) - balanced size/reliability
- Level L (7%): Too fragile for physical labels
- Level Q (25%): Too large
- Level H (30%): Unnecessarily large
- **Level M (15%)**: SSOT default, good compromise

**Image Parameters**:
- **Module size**: 10 pixels (determines overall size)
- **Border**: 0 pixels (quiet zone handled by QR library)
- **Output**: PNG encoded via `JuicyPixels`
- **Storage**: Binary blob in `passport_versions.qr_png`

**Scanability Requirements**:
- Minimum print size: 2cm x 2cm (at 300 DPI)
- Maximum payload length: ~200 characters (fits in smallest QR version)
- Black/white contrast for industrial scanners

**Implementation**:
```haskell
import qualified Codec.QRCode as QR
import qualified Codec.Picture.Png as PNG

generateQR :: Pool Connection -> Job -> IO ()
generateQR pool job = do
  -- ... load passport version

  -- Build QR payload
  let qrPayload = "BPC1|pv=" <> toText pvId
                  <> "|ph=" <> base32NoPad (pvPayloadHash pv)
                  <> "|pr=" <> base32NoPad (pvProofRootHash pv)
                  <> "|rh=" <> base32NoPad (pvReceiptHash pv)

  -- Generate QR code
  qrCode <- case QR.encodeText qrPayload QR.M of
    Nothing -> throwIO $ QREncodeFailed qrPayload
    Just qr -> pure qr

  -- Render to PNG
  let qrImage = QR.toImage 10 0 qrCode
      qrPng = PNG.encodePng qrImage

  -- Store
  withConn pool $ \conn ->
    updatePassportVersionQR conn (jTenantId job) pvId qrPng qrPayload
```

---

## 6. Webhook Delivery

**Decision**: HMAC-SHA256 signature in `X-BPC-Signature` header

**Header Format**: `X-BPC-Signature: sha256=<hex>`

**Signature Computation**:
```haskell
import Crypto.Hash.SHA256 (hmac)
import qualified Data.ByteString.Base16 as B16

let body = encode $ object [...]  -- JSON event payload
let signature = hmac (weSecret endpoint) body
let signatureHex = B16.encode signature
```

**HTTP Request Structure**:
```http
POST /webhooks/bpc HTTP/1.1
Host: customer-endpoint.example.com
Content-Type: application/json
X-BPC-Signature: sha256=5f3d8b7a2e1c4f9b6a8d3e7f1c2b5a4d9e8f7c6b5a4d3e2f1c0b9a8d7e6f5c4b
X-BPC-Event-Type: PASSPORT_SIGNED
X-BPC-Delivery-Id: 123e4567-e89b-12d3-a456-426614174000

{
  "event_id": "evt_abc123",
  "event_type": "PASSPORT_SIGNED",
  "aggregate_type": "PassportVersion",
  "aggregate_id": "550e8400-e29b-41d4-a716-446655440000",
  "occurred_at": "2025-12-28T12:34:56Z",
  "payload": {
    "passport_version_id": "550e8400-e29b-41d4-a716-446655440000",
    "key_id": "dev-key-1"
  }
}
```

**Recipient Verification** (customer implementation):
```python
import hmac
import hashlib

def verify_signature(secret, body_bytes, signature_header):
    expected = hmac.new(secret.encode(), body_bytes, hashlib.sha256).hexdigest()
    provided = signature_header.removeprefix("sha256=")
    return hmac.compare_digest(expected, provided)
```

**Retry Strategy**: Same exponential backoff as other jobs
- Network errors → retry
- Timeouts (30s) → retry
- Non-2xx responses → retry
- Max 5 attempts → DEAD_LETTER

**Timeout**: 30 seconds per delivery
- Prevents webhook from blocking worker indefinitely
- Configurable per endpoint (future enhancement)

**Delivery Status Tracking**:
```sql
CREATE TABLE webhook_deliveries (
  webhook_delivery_id uuid PRIMARY KEY,
  tenant_id uuid NOT NULL,
  webhook_endpoint_id uuid NOT NULL,
  event_id uuid NOT NULL,
  status text NOT NULL, -- PENDING | DELIVERED | FAILED
  http_status int NULL,
  response_headers jsonb NULL,
  response_body bytea NULL,
  delivered_at timestamptz NULL,
  error_message text NULL,
  created_at timestamptz NOT NULL DEFAULT now()
);
```

**Security Considerations**:
- Secret stored per webhook endpoint (not per delivery)
- Secret rotation requires updating `webhook_endpoints` table
- HTTPS required for production endpoints (enforced via validation)

---

## 7. Lease Timeout Handling

**Scenario**: Worker crashes mid-job, lease expires

**Expected Behavior**:
1. Worker A acquires job, starts processing
2. Worker A crashes (no graceful shutdown)
3. Lease expires after 60s (`lease_expires_at < now()`)
4. Job becomes visible in `acquireLease` query again
5. Worker B acquires job, starts processing
6. Worker B completes job successfully

**Idempotency Requirements**:
- **PARSE_FACTS**: `UNIQUE(tenant_id, fact_type, fact_key, schema_version, payload_hash)` prevents duplicate facts
- **COMPILE_PASSPORT**: `UNIQUE(tenant_id, passport_id, snapshot_id, rules_id, build_id, issued_at)` prevents duplicate versions
- **SIGN_PASSPORT**: Signature is deterministic for same receipt hash
- **GENERATE_QR**: QR payload is deterministic, re-generation safe
- **DELIVER_WEBHOOK**: Idempotency enforced by recipient (X-BPC-Delivery-Id header)

**Database Constraints Ensure Safety**:
- Unique constraints prevent duplicate inserts
- Foreign key constraints prevent orphaned records
- Check constraints enforce valid status transitions

---

## 8. Priority Handling

**Decision**: Priority field in jobs table (lower number = higher priority)

**Priority Levels**:
- **0**: Critical (system maintenance jobs)
- **1**: High (compilation, signing)
- **2**: Normal (QR generation, parsing)
- **3**: Low (webhooks, exports)

**Ordering** (SSOT 4.6):
```sql
ORDER BY priority ASC, scheduled_at ASC
```

**Why Separate Field**:
- Explicit priority control (vs implicit based on job type)
- Future: allow API to set priority
- Future: tenant-specific priority overrides

---

## Open Questions

**None** - SSOT provides complete specifications. All decisions documented above are final.

---

## References

- **SSOT 3.5.4**: bpc-worker exports (Runner, Dispatch, Handlers)
- **SSOT 4.6**: ENV schema (polling interval, lease seconds, max attempts)
- **SSOT 7.8**: BPC-QR-1 format specification
- **SSOT 14.3-14.5**: Tasks P1-P3 (worker loop, handlers, retry)
- **Constitution V**: Layered Architecture (bpc-worker imports only core+db)

---

## Next Steps

Proceed to implementation following `plan.md` phases. All architectural decisions documented and approved.
