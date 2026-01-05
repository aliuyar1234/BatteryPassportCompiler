# Quickstart: Job Processing (Worker)

**Feature**: 07-job-processing
**Date**: 2025-12-28
**Package**: bpc-worker
**Prerequisites**: 05-data-layer complete (jobs table exists)

## Table of Contents

1. [Running the Worker](#running-the-worker)
2. [Multiple Workers](#multiple-workers-horizontal-scaling)
3. [Job Pipeline Example](#job-pipeline-example)
4. [Monitoring Jobs](#monitoring-jobs)
5. [Retry Failed Jobs](#retry-failed-jobs)
6. [Testing](#testing)
7. [Troubleshooting](#troubleshooting)

---

## Running the Worker

### Prerequisites

Ensure dependencies are running:

```bash
# Start Postgres and RabbitMQ
docker compose up -d postgres rabbitmq

# Run migrations (if not already done)
./scripts/migrate.sh
```

### Environment Variables

Set required environment variables:

```bash
# Worker identification
export BPC_WORKER_ID=worker-1                 # Unique ID for this worker

# Database connection
export BPC_DB_HOST=localhost
export BPC_DB_PORT=5432
export BPC_DB_USER=bpc
export BPC_DB_PASSWORD=bpc
export BPC_DB_NAME=bpc

# Job processing configuration
export BPC_JOBS_POLL_INTERVAL_MS=1000         # Poll every 1 second
export BPC_JOBS_LEASE_SECONDS=60              # Lease TTL (60s)
export BPC_JOBS_LEASE_RENEW_SECONDS=30        # Renew every 30s

# Compilation & signing
export BPC_COMPILER_BUILD_ID=$(git rev-parse HEAD)
export BPC_SIGNING_KEY_ID=dev-key-1

# ED25519 signing key (REQUIRED for SIGN_PASSPORT jobs)
# Generate with: openssl genpkey -algorithm ed25519 | base64 -w0
export BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64=<your-base64-encoded-key>

# Optional: RabbitMQ trigger (for faster job pickup)
export BPC_QUEUE_ENABLED=false                # Use DB polling only
# export BPC_QUEUE_URL=amqp://guest:guest@localhost:5672/
# export BPC_QUEUE_EXCHANGE=bpc
# export BPC_QUEUE_ROUTING_KEY=jobs.trigger
```

### Start Worker

```bash
# Build
cabal build bpc-worker:exe:bpc-worker

# Run
cabal run bpc-worker:exe:bpc-worker
```

### Expected Output

```text
[2025-12-28T12:00:00Z] INFO  Worker started: worker-id=worker-1
[2025-12-28T12:00:00Z] INFO  Config: poll_interval=1000ms, lease_ttl=60s, lease_renew=30s
[2025-12-28T12:00:00Z] INFO  Polling for jobs...
[2025-12-28T12:00:01Z] INFO  No jobs in queue, sleeping 1000ms
[2025-12-28T12:00:02Z] INFO  No jobs in queue, sleeping 1000ms
...
[2025-12-28T12:00:10Z] INFO  Acquired job: job_id=abc-123, type=PARSE_FACTS
[2025-12-28T12:00:10Z] INFO  Starting lease renewal thread
[2025-12-28T12:00:12Z] INFO  Job completed: job_id=abc-123, status=SUCCEEDED, duration=2134ms
[2025-12-28T12:00:12Z] INFO  Polling for jobs...
```

---

## Multiple Workers (Horizontal Scaling)

Run multiple workers to process jobs in parallel. `FOR UPDATE SKIP LOCKED` ensures no job is processed twice.

### Terminal 1

```bash
export BPC_WORKER_ID=worker-1
export BPC_DB_HOST=localhost
# ... other ENV vars
cabal run bpc-worker:exe:bpc-worker
```

### Terminal 2

```bash
export BPC_WORKER_ID=worker-2
export BPC_DB_HOST=localhost
# ... other ENV vars
cabal run bpc-worker:exe:bpc-worker
```

### Terminal 3

```bash
export BPC_WORKER_ID=worker-3
export BPC_DB_HOST=localhost
# ... other ENV vars
cabal run bpc-worker:exe:bpc-worker
```

### Verify Concurrency

Enqueue 100 jobs and watch all workers process them concurrently:

```sql
-- Enqueue 100 test jobs
INSERT INTO jobs (job_id, tenant_id, type, status, payload, priority)
SELECT
  gen_random_uuid(),
  'tenant-uuid',
  'PARSE_FACTS',
  'QUEUED',
  jsonb_build_object('document_version_id', gen_random_uuid()::text),
  2
FROM generate_series(1, 100);

-- Watch jobs being processed
SELECT lease_owner, status, COUNT(*)
FROM jobs
WHERE tenant_id = 'tenant-uuid'
GROUP BY lease_owner, status
ORDER BY lease_owner, status;
```

Expected output (while processing):

```text
lease_owner | status   | count
------------+----------+-------
worker-1    | RUNNING  |   2
worker-2    | RUNNING  |   3
worker-3    | RUNNING  |   2
(null)      | QUEUED   |  93
```

After completion:

```text
lease_owner | status    | count
------------+-----------+-------
worker-1    | SUCCEEDED |  34
worker-2    | SUCCEEDED |  33
worker-3    | SUCCEEDED |  33
```

---

## Job Pipeline Example

### Full Pipeline: Upload → Active

This example walks through the complete passport compilation pipeline.

#### Step 1: Upload Document (via API)

```bash
curl -X POST http://localhost:8080/v1/documents/{document_id}/versions \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "mime_type": "application/json",
    "content": "<base64-encoded-BOM-content>"
  }'

# Response:
# {
#   "document_version_id": "dv-123",
#   "status": "UPLOADED"
# }
```

**Auto-enqueued**: `PARSE_FACTS` job

#### Step 2: Worker Processes PARSE_FACTS

Worker picks up job and creates Facts:

```text
[INFO] Acquired job: job_id=job-abc, type=PARSE_FACTS
[INFO] Parsing document: document_version_id=dv-123, kind=BOM
[INFO] Extracted 5 facts
[INFO] Job completed: status=SUCCEEDED
```

**Result**: DocumentVersion status → `VALIDATED`, Facts inserted

#### Step 3: Create Snapshot (via API)

```bash
# Create snapshot
curl -X POST http://localhost:8080/v1/snapshots \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "label": "Battery Snapshot 2025-Q1"
  }'

# Response: { "snapshot_id": "snap-456", "status": "BUILDING" }

# Add facts to snapshot
curl -X POST http://localhost:8080/v1/snapshots/snap-456/items \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type": application/json" \
  -d '{
    "fact_ids": ["fact-1", "fact-2", "fact-3", "fact-4", "fact-5"]
  }'

# Seal snapshot (computes snapshot_hash)
curl -X POST http://localhost:8080/v1/snapshots/snap-456/seal \
  -H "Authorization: Bearer $API_KEY"

# Response: { "status": "SEALED", "snapshot_hash": "abc123..." }
```

#### Step 4: Trigger Compilation (via API)

```bash
curl -X POST http://localhost:8080/v1/passports/{passport_id}/compile \
  -H "Authorization: Bearer $API_KEY" \
  -H "Content-Type: application/json" \
  -d '{
    "snapshot_id": "snap-456",
    "rule_package_version_id": "rpv-789"
  }'

# Response:
# {
#   "job_id": "job-compile-123",
#   "status": "QUEUED"
# }
```

**Auto-enqueued**: `COMPILE_PASSPORT` job

#### Step 5: Worker Processes COMPILE_PASSPORT

```text
[INFO] Acquired job: job_id=job-compile-123, type=COMPILE_PASSPORT
[INFO] Loading snapshot: snapshot_id=snap-456, status=SEALED
[INFO] Loading rules: rpv_id=rpv-789, status=PUBLISHED
[INFO] Calling compilePassportPure (deterministic)
[INFO] Compilation succeeded: payload_size=12345, proof_size=67890, receipt_size=1234
[INFO] PassportVersion created: pv_id=pv-abc123, status=COMPILING
[INFO] Enqueued SIGN_PASSPORT job: job_id=job-sign-456
[INFO] Job completed: status=SUCCEEDED
```

**Auto-enqueued**: `SIGN_PASSPORT` job

#### Step 6: Worker Processes SIGN_PASSPORT

```text
[INFO] Acquired job: job_id=job-sign-456, type=SIGN_PASSPORT
[INFO] Loading PassportVersion: pv_id=pv-abc123, status=COMPILING
[INFO] Signing receipt hash with ED25519
[INFO] Signature computed: key_id=dev-key-1
[INFO] PassportVersion updated: status=SIGNED
[INFO] Enqueued GENERATE_QR job: job_id=job-qr-789
[INFO] Job completed: status=SUCCEEDED
```

**Auto-enqueued**: `GENERATE_QR` job

#### Step 7: Worker Processes GENERATE_QR

```text
[INFO] Acquired job: job_id=job-qr-789, type=GENERATE_QR
[INFO] Building QR payload: format=BPC-QR-1
[INFO] QR payload: BPC1|pv=pv-abc123|ph=ABCDEF...|pr=GHIJKL...|rh=MNOPQR...
[INFO] Generating QR PNG: size=200x200px, error_correction=M
[INFO] QR generated: png_size=4567 bytes
[INFO] Job completed: status=SUCCEEDED
```

**Result**: PassportVersion has `qr_png` and `qr_payload`, status remains `SIGNED`

#### Step 8: Activate (via API)

```bash
curl -X POST http://localhost:8080/v1/passport-versions/pv-abc123/activate \
  -H "Authorization: Bearer $API_KEY"

# Response:
# {
#   "passport_version_id": "pv-abc123",
#   "status": "ACTIVE"
# }
```

**Done!** Passport is now ACTIVE and ready for use.

---

## Monitoring Jobs

### Check Job Status (SQL)

```sql
-- Recent jobs
SELECT job_id, type, status, attempts, scheduled_at, lease_owner, created_at
FROM jobs
WHERE tenant_id = 'tenant-uuid'
ORDER BY created_at DESC
LIMIT 20;

-- Jobs by status
SELECT status, COUNT(*)
FROM jobs
WHERE tenant_id = 'tenant-uuid'
GROUP BY status;

-- Currently running jobs
SELECT job_id, type, lease_owner, lease_expires_at, started_at
FROM jobs
WHERE status = 'RUNNING'
AND tenant_id = 'tenant-uuid'
ORDER BY started_at;

-- Dead letter queue
SELECT job_id, type, attempts, last_error
FROM jobs
WHERE status = 'DEAD_LETTER'
AND tenant_id = 'tenant-uuid'
ORDER BY created_at DESC;

-- Failed jobs (non-retryable errors)
SELECT job_id, type, last_error->>'error_code' AS error_code, last_error->>'message' AS message
FROM jobs
WHERE status = 'FAILED'
AND tenant_id = 'tenant-uuid'
ORDER BY created_at DESC;
```

### Job Metrics (Prometheus)

If metrics are enabled (`BPC_METRICS_ENABLED=true`), query metrics endpoint:

```bash
curl http://localhost:8080/v1/metrics | grep bpc_jobs

# Example output:
# bpc_jobs_enqueued_total{tenant_id="tenant-uuid",job_type="PARSE_FACTS"} 150
# bpc_jobs_enqueued_total{tenant_id="tenant-uuid",job_type="COMPILE_PASSPORT"} 45
# bpc_jobs_running{tenant_id="tenant-uuid",job_type="PARSE_FACTS"} 2
# bpc_jobs_completed_total{tenant_id="tenant-uuid",job_type="PARSE_FACTS",status="SUCCEEDED"} 148
# bpc_jobs_completed_total{tenant_id="tenant-uuid",job_type="COMPILE_PASSPORT",status="SUCCEEDED"} 43
# bpc_job_duration_seconds_bucket{tenant_id="tenant-uuid",job_type="COMPILE_PASSPORT",le="1.0"} 20
# bpc_job_duration_seconds_bucket{tenant_id="tenant-uuid",job_type="COMPILE_PASSPORT",le="5.0"} 40
# bpc_jobs_dead_letter{tenant_id="tenant-uuid"} 2
```

### Worker Logs

Worker logs are structured JSON (if `BPC_LOG_JSON=true`):

```bash
# Follow worker logs
tail -f /var/log/bpc-worker.log | jq .

# Filter by job type
tail -f /var/log/bpc-worker.log | jq 'select(.job_type == "COMPILE_PASSPORT")'

# Filter by status
tail -f /var/log/bpc-worker.log | jq 'select(.status == "FAILED")'
```

---

## Retry Failed Jobs

### Retry Single Job

```sql
-- Find failed job
SELECT job_id, type, attempts, last_error
FROM jobs
WHERE status = 'DEAD_LETTER'
AND tenant_id = 'tenant-uuid'
AND job_id = 'job-abc-123';

-- Retry job (reset attempts and status)
UPDATE jobs
SET status = 'QUEUED',
    attempts = 0,
    scheduled_at = now(),
    lease_owner = NULL,
    lease_expires_at = NULL,
    last_error = NULL
WHERE job_id = 'job-abc-123';

-- Worker will pick it up on next poll
```

### Retry All Dead Letter Jobs

```sql
-- WARNING: Only do this if you've fixed the root cause!

UPDATE jobs
SET status = 'QUEUED',
    attempts = 0,
    scheduled_at = now(),
    lease_owner = NULL,
    lease_expires_at = NULL
WHERE status = 'DEAD_LETTER'
AND tenant_id = 'tenant-uuid';

-- Returns: UPDATE 5 (number of jobs retried)
```

### Retry Specific Job Type

```sql
-- Retry all failed WEBHOOK jobs
UPDATE jobs
SET status = 'QUEUED',
    attempts = 0,
    scheduled_at = now(),
    lease_owner = NULL,
    lease_expires_at = NULL
WHERE status = 'DEAD_LETTER'
AND type = 'DELIVER_WEBHOOK'
AND tenant_id = 'tenant-uuid';
```

---

## Testing

### Unit Tests

```bash
# Run all unit tests
cabal test bpc-worker

# Run specific test suite
cabal test bpc-worker:test:unit

# Run with coverage
cabal test bpc-worker --enable-coverage
```

### Integration Tests

Integration tests require a real Postgres database:

```bash
# Start test database
docker compose -f docker-compose.test.yml up -d postgres

# Set test environment
export BPC_DB_HOST=localhost
export BPC_DB_PORT=55432
export BPC_DB_USER=bpc
export BPC_DB_PASSWORD=bpc
export BPC_DB_NAME=bpc_test

# Run migrations
DATABASE_URL=postgres://bpc:bpc@localhost:55432/bpc_test?sslmode=disable \
  dbmate up

# Run integration tests
cabal test bpc-worker:test:integration

# Cleanup
docker compose -f docker-compose.test.yml down -v
```

### E2E Pipeline Test

```bash
# Full pipeline: upload → active
cabal test bpc-worker:test:integration --test-option="--pattern E2E"

# Expected output:
#   ✓ E2E happy path (upload → parse → compile → sign → qr → activate)
#   ✓ E2E with invalid snapshot (SNAPSHOT_NOT_SEALED error)
#   ✓ E2E with invalid rules (RULE_PKG_NOT_PUBLISHED error)
```

### Concurrency Test

```bash
# 10 workers process 100 jobs without duplicates
cabal test bpc-worker:test:integration --test-option="--pattern Concurrency"

# Expected output:
#   ✓ 10 workers process 100 jobs (no duplicates)
#   ✓ Lease expiry allows re-acquisition
#   ✓ Retry with exponential backoff
#   ✓ 5 failed attempts → DEAD_LETTER
```

---

## Troubleshooting

### Worker Not Picking Up Jobs

**Symptom**: Jobs stuck in QUEUED status

**Diagnosis**:

```sql
-- Check if jobs are visible to worker
SELECT job_id, type, status, scheduled_at, lease_expires_at
FROM jobs
WHERE status = 'QUEUED'
AND scheduled_at <= now()
AND (lease_expires_at IS NULL OR lease_expires_at < now())
AND tenant_id = 'tenant-uuid'
ORDER BY priority, scheduled_at
LIMIT 5;
```

**Possible Causes**:
1. Worker not running: `ps aux | grep bpc-worker`
2. Worker polling wrong database: Check `BPC_DB_*` ENV vars
3. Jobs scheduled in future: `scheduled_at > now()`
4. Worker crashed: Check logs for errors

---

### Job Stuck in RUNNING

**Symptom**: Job status is RUNNING but worker is down

**Diagnosis**:

```sql
-- Check for stuck jobs
SELECT job_id, type, lease_owner, lease_expires_at, started_at
FROM jobs
WHERE status = 'RUNNING'
AND lease_expires_at < now()  -- Lease expired
AND tenant_id = 'tenant-uuid';
```

**Solution**: Wait for lease to expire (60s), then another worker will pick it up.

**Manual Recovery** (if urgent):

```sql
-- Manually reset job to QUEUED
UPDATE jobs
SET status = 'QUEUED',
    lease_owner = NULL,
    lease_expires_at = NULL,
    scheduled_at = now()
WHERE job_id = 'stuck-job-uuid';
```

---

### Job Failing Repeatedly

**Symptom**: Job has high `attempts` count but keeps failing

**Diagnosis**:

```sql
-- Check last error
SELECT job_id, type, attempts, last_error
FROM jobs
WHERE status = 'QUEUED'
AND attempts > 0
AND tenant_id = 'tenant-uuid'
ORDER BY attempts DESC;
```

**Possible Causes**:
1. **Transient error** (DB, network): Wait for retry with backoff
2. **Configuration error**: Check ENV vars (`BPC_SIGNING_KEY`, etc.)
3. **Data error**: Check preconditions (snapshot SEALED, rules PUBLISHED)
4. **Bug in handler**: Check worker logs for stack traces

**Solution**: Fix root cause, then retry:

```sql
UPDATE jobs
SET status = 'QUEUED',
    attempts = 0,
    scheduled_at = now()
WHERE job_id = 'failing-job-uuid';
```

---

### Dead Letter Queue Growing

**Symptom**: Many jobs in DEAD_LETTER status

**Diagnosis**:

```sql
-- Group dead letter jobs by error type
SELECT
  last_error->>'error_code' AS error_code,
  COUNT(*) AS count
FROM jobs
WHERE status = 'DEAD_LETTER'
AND tenant_id = 'tenant-uuid'
GROUP BY error_code
ORDER BY count DESC;
```

**Common Errors**:
- `SNAPSHOT_NOT_SEALED`: Snapshot not sealed before compile
- `RULE_PKG_NOT_PUBLISHED`: Rules not published
- `SIGNING_KEY_MISSING`: ED25519 key not configured
- `PAYLOAD_TOO_LARGE`: Compilation output exceeds size limit

**Solution**: Fix root cause (seal snapshots, publish rules, configure keys, optimize rules), then retry dead letter queue.

---

### Webhook Delivery Failing

**Symptom**: DELIVER_WEBHOOK jobs stuck in retry loop

**Diagnosis**:

```sql
-- Check webhook delivery errors
SELECT
  webhook_deliveries.webhook_delivery_id,
  webhook_endpoints.url,
  webhook_deliveries.http_status,
  webhook_deliveries.error_message
FROM webhook_deliveries
JOIN webhook_endpoints USING (webhook_endpoint_id)
WHERE webhook_deliveries.status = 'FAILED'
AND webhook_deliveries.tenant_id = 'tenant-uuid'
ORDER BY webhook_deliveries.created_at DESC;
```

**Possible Causes**:
1. **Endpoint down**: Timeout or connection refused
2. **Invalid URL**: DNS resolution failed
3. **Non-2xx response**: Endpoint rejecting requests
4. **HMAC mismatch**: Endpoint signature verification failed

**Solution**:
1. Fix endpoint (bring back online, fix URL)
2. Test endpoint manually: `curl -X POST <url> -d '{"test": true}'`
3. Retry webhooks: `UPDATE jobs SET status = 'QUEUED', attempts = 0 WHERE type = 'DELIVER_WEBHOOK' AND status = 'DEAD_LETTER';`

---

## Next Steps

After job processing is working:

1. **08-advanced-features**: Rate limiting, policies, retention
2. **Observability**: Set up Grafana dashboards for job metrics
3. **Alerting**: Configure alerts for dead letter queue size
4. **Horizontal Scaling**: Deploy multiple worker instances in production
5. **Queue Optimization**: Enable RabbitMQ trigger for faster job pickup

---

## Quick Reference

### Environment Variables

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `BPC_WORKER_ID` | ✅ Yes | - | Unique worker identifier |
| `BPC_DB_HOST` | ✅ Yes | - | Postgres host |
| `BPC_DB_PORT` | No | 5432 | Postgres port |
| `BPC_DB_USER` | No | postgres | Postgres user |
| `BPC_DB_PASSWORD` | No | postgres | Postgres password |
| `BPC_DB_NAME` | No | bpc | Database name |
| `BPC_JOBS_POLL_INTERVAL_MS` | No | 1000 | Poll interval (ms) |
| `BPC_JOBS_LEASE_SECONDS` | No | 60 | Lease TTL (seconds) |
| `BPC_JOBS_LEASE_RENEW_SECONDS` | No | 30 | Lease renewal interval (seconds) |
| `BPC_COMPILER_BUILD_ID` | ✅ Yes | - | Git commit hash |
| `BPC_SIGNING_KEY_ID` | No | dev-key-1 | Signing key identifier |
| `BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64` | ✅ Yes (prod) | - | ED25519 private key (Base64) |
| `BPC_QUEUE_ENABLED` | No | false | Enable RabbitMQ trigger |
| `BPC_QUEUE_URL` | No | - | RabbitMQ connection URL |

### SQL Queries

```sql
-- Count jobs by status
SELECT status, COUNT(*) FROM jobs WHERE tenant_id = 'tenant-uuid' GROUP BY status;

-- Recent jobs
SELECT job_id, type, status, attempts FROM jobs WHERE tenant_id = 'tenant-uuid' ORDER BY created_at DESC LIMIT 20;

-- Dead letter queue
SELECT job_id, type, last_error FROM jobs WHERE status = 'DEAD_LETTER' AND tenant_id = 'tenant-uuid';

-- Retry job
UPDATE jobs SET status = 'QUEUED', attempts = 0, scheduled_at = now() WHERE job_id = 'job-uuid';
```

### Test Commands

```bash
cabal test bpc-worker                      # All tests
cabal test bpc-worker:test:unit            # Unit tests only
cabal test bpc-worker:test:integration     # Integration tests
```
