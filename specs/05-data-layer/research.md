# Research: Data Layer

**Feature**: 05-data-layer
**Date**: 2025-12-28
**Status**: Complete
**Phase**: Phase 0 (Research & Design)

## Executive Summary

The Data Layer research phase evaluated database access libraries, connection pooling strategies, hash chain algorithms, job leasing patterns, and tenant isolation approaches for the BatteryPassportCompiler. Key decisions: use **postgresql-simple** for direct SQL with type safety, **resource-pool** for connection management, implement BPC-EVENT-1 hash chain per SSOT 7.4, use **FOR UPDATE SKIP LOCKED** for race-free job leasing, and enforce tenant isolation via WHERE clauses + integration tests.

---

## 1. Database Library Comparison

### Research Question
Which Haskell database library best fits BatteryPassportCompiler requirements: type safety, direct SQL control, multi-tenancy support, and integration with canonical JSON?

### Options Evaluated

| Library | Type Safety | SQL Control | Complexity | Migration Support | Verdict |
|---------|-------------|-------------|------------|-------------------|---------|
| **postgresql-simple** | Moderate (Query type, FromRow/ToRow) | Full (raw SQL strings) | Low | External (dbmate) | **SELECTED** |
| **Hasql** | High (prepared statements) | Full (raw SQL) | Medium | External | Alternative |
| **Beam** | Very High (type-level schema) | Limited (EDSL) | High | Built-in | Rejected (overhead) |
| **Esqueleto** | High (type-safe EDSL) | Limited (EDSL) | Medium | Persistent migrations | Rejected (ORM overhead) |
| **Persistent** | High (type-safe EDSL) | Limited (EDSL) | Medium | Built-in | Rejected (ORM mismatch) |

### Decision: **postgresql-simple**

**Rationale**:
1. **Direct SQL Control**: Write exact queries needed (e.g., `FOR UPDATE SKIP LOCKED` for job leasing); no EDSL translation layer
2. **Low Complexity**: Simple API (query, execute, withTransaction); easy to understand and debug
3. **Type Safety**: `FromRow`/`ToRow` instances provide type-safe result parsing; `Query` type prevents SQL injection (? placeholders)
4. **Migration Flexibility**: Use dbmate (idempotent SQL migrations); no coupling to library-specific schema management
5. **Multi-Tenancy**: Explicit `WHERE tenant_id = ?` in all queries; no magic, no hidden filters
6. **Canonical JSON Integration**: Works seamlessly with `bytea` columns for canonical storage; no JSON conversion layer

**Trade-offs Accepted**:
- Manual `WHERE tenant_id = ?` in every query (vs Beam's type-level tenant filtering)
  - *Mitigation*: Code review checklist; integration tests verify isolation; grep for missing WHERE clauses in CI
- No compile-time query validation (vs Beam/Esqueleto type-level schema)
  - *Mitigation*: Integration tests with real Postgres; migrations define source-of-truth schema

**Rejected Alternatives**:
- **Hasql**: Slightly more type-safe (prepared statements), but added complexity for prepared statement management; not worth trade-off
- **Beam**: Type-level schema impressive, but high complexity cost; EDSL may not support `FOR UPDATE SKIP LOCKED` directly
- **Esqueleto/Persistent**: ORM model mismatch with canonical storage (bytea columns) and event sourcing patterns

---

## 2. Connection Pool Strategy

### Research Question
How to configure resource-pool for optimal performance, avoiding exhaustion while minimizing idle connections?

### Configuration Parameters

| Parameter | Value | Rationale |
|-----------|-------|-----------|
| **Pool Size (BPC_DB_POOL_SIZE)** | 10 (default) | API needs responsive pool (10 concurrent requests); Worker can tolerate queuing; tune via ENV based on load |
| **Stripes** | 1 | Single stripe sufficient for <= 50 connections; multi-stripe adds contention overhead; KISS principle |
| **Idle Timeout** | 60 seconds | PostgreSQL default idle_in_transaction_session_timeout; close idle connections to free resources |
| **Acquire Timeout** | 30 seconds | Fail fast if pool exhausted; return DB_UNAVAILABLE to client; monitor pool metrics |

### Pool Lifecycle

```haskell
-- Create pool (application startup)
mkPool :: DBConfig -> IO (Pool Connection)
mkPool cfg = createPool
  (connectPostgreSQL $ mkConnString cfg)  -- Resource creation
  Database.PostgreSQL.Simple.close        -- Resource destruction
  1                                        -- Number of stripes
  60                                       -- Idle timeout (seconds)
  (dbPoolSize cfg)                         -- Max connections per stripe

-- Use pool (per-request)
withConn :: Pool Connection -> (Connection -> IO a) -> IO a
withConn = withResource  -- Automatically returns connection after use

-- Close pool (application shutdown)
close :: Pool Connection -> IO ()
close = destroyAllResources
```

### Pool Exhaustion Handling

**Symptoms**: Acquire timeout (30s) expires while waiting for available connection

**Causes**:
1. Long-running transactions holding connections
2. Connection leaks (not returned to pool)
3. Pool size too small for load

**Mitigations**:
1. **Monitor Metrics**: Track `withResource` duration; alert if >1s p95
2. **Configurable Size**: `BPC_DB_POOL_SIZE` ENV var; start with 10, scale up if needed
3. **Timeout Handling**: Catch `ResourceExhausted` exception; return `DB_UNAVAILABLE` HTTP 503
4. **Transaction Timeout**: PostgreSQL `statement_timeout` (30s default); kill runaway queries
5. **Health Check**: Periodic pool health check (count active connections via pg_stat_activity)

### API vs Worker Pool Sizing

**API Server** (bpc-api):
- **Needs**: Low latency; responsive to HTTP requests
- **Pool Size**: 10-20 (one per concurrent request handler)
- **Timeout**: 30s (fail fast if pool exhausted)

**Worker** (bpc-worker):
- **Needs**: Throughput; can tolerate higher latency
- **Pool Size**: 5-10 (one per worker thread)
- **Timeout**: 60s (workers can wait longer)

**Decision**: Use **single pool size** (BPC_DB_POOL_SIZE=10) for both API and Worker initially; split into separate pools if contention observed in production (monitor pg_stat_activity).

---

## 3. Hash Chain Algorithm (BPC-EVENT-1)

### Research Question
How to implement BPC-EVENT-1 hash chain per SSOT 7.4, ensuring tamper detection and verification efficiency?

### Algorithm Specification (SSOT 7.4)

```haskell
-- Compute event hash
eventHash :: EventInput -> ByteString -> ByteString
eventHash input prevHash =
  sha256Hex $ canonicalEncode $ object
    [ "tenant_id" .= tenantId input
    , "aggregate_type" .= aggregateType input
    , "aggregate_id" .= aggregateId input
    , "aggregate_version" .= aggregateVersion input
    , "event_type" .= eventType input
    , "occurred_at" .= occurredAt input
    , "actor_id" .= actorId input
    , "payload_hash" .= payloadHash input
    , "prev_event_hash" .= prevHash  -- Links to previous event
    ]
```

**Key Properties**:
1. **Canonical Encoding**: BPC-CJSON-1 (keys sorted, no whitespace) → deterministic hash
2. **Payload Hash**: Hash of canonical payload (not full payload) → smaller hash input
3. **Previous Hash Link**: `event[i].event_hash === event[i+1].prev_event_hash` → chain integrity
4. **Tenant Scoped**: Hash includes `tenant_id` → prevent cross-tenant hash collisions

### Append Algorithm

```haskell
appendEvent :: Connection -> AppendEventInput -> IO (Either EventError EventId)
appendEvent conn input = do
  -- 1. Compute payload canonical and hash
  let payloadCanonical = canonicalEncode (payload input)
  let payloadHash = sha256Hex payloadCanonical

  -- 2. Fetch previous event hash (highest version for aggregate)
  prevHash <- query conn
    "SELECT event_hash FROM events \
    \WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ? \
    \ORDER BY aggregate_version DESC LIMIT 1"
    (tenantId input, aggregateType input, aggregateId input)

  -- 3. Compute event hash with prev_event_hash link
  let eventHash = computeEventHash input payloadHash (listToMaybe prevHash)

  -- 4. Insert with UNIQUE constraint on (tenant_id, aggregate_type, aggregate_id, aggregate_version)
  catch
    (do
      execute conn
        "INSERT INTO events (event_id, tenant_id, aggregate_type, aggregate_id, \
        \aggregate_version, event_type, occurred_at, actor_id, payload, payload_canonical, \
        \payload_hash, prev_event_hash, event_hash) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        (eventId, tenantId input, aggregateType input, aggregateId input,
         aggregateVersion input, eventType input, occurredAt input, actorId input,
         payload input, payloadCanonical, payloadHash, prevHash, eventHash)
      pure $ Right eventId)
    (\(SqlError{sqlState = "23505"}) -> pure $ Left EVENT_VERSION_CONFLICT)
```

### Verification Algorithm

```haskell
verifyChain :: Connection -> TenantId -> AggregateType -> AggregateId -> IO (Either ChainError ())
verifyChain conn tenantId aggType aggId = do
  -- 1. Fetch all events for aggregate (ordered by version)
  events <- query conn
    "SELECT * FROM events \
    \WHERE tenant_id = ? AND aggregate_type = ? AND aggregate_id = ? \
    \ORDER BY aggregate_version ASC"
    (tenantId, aggType, aggId)

  -- 2. Verify each event hash and chain links
  case verifyEvents events of
    Left err -> pure $ Left err
    Right () -> pure $ Right ()

  where
    verifyEvents [] = Right ()
    verifyEvents (e:es) = do
      -- Recompute hash
      let expected = computeEventHash e (payloadHash e) (prevEventHash e)
      when (expected /= eventHash e) $
        Left $ ChainBroken (eventId e) expected (eventHash e)

      -- Verify link to next event
      case es of
        [] -> Right ()  -- Last event
        (next:_) ->
          if Just (eventHash e) == prevEventHash next
            then verifyEvents es
            else Left $ ChainBroken (eventId next) (Just $ eventHash e) (prevEventHash next)
```

### Performance Optimization

**Challenge**: Verify 10,000+ events efficiently

**Optimizations**:
1. **Index**: `CREATE INDEX events_by_aggregate ON events(tenant_id, aggregate_type, aggregate_id, aggregate_version)`
   - Enables fast ordered fetch of event chain
2. **Batch Verification**: Fetch all events for aggregate in single query (not one-by-one)
3. **Parallel Aggregates**: Verify multiple aggregates in parallel (use `async` library)
   ```haskell
   verifyAllChains :: Connection -> TenantId -> IO (Either ChainError VerifyReport)
   verifyAllChains conn tenantId = do
     aggregates <- query conn "SELECT DISTINCT aggregate_type, aggregate_id FROM events WHERE tenant_id = ?" (Only tenantId)
     results <- mapConcurrently (\(t, id) -> verifyChain conn tenantId t id) aggregates
     -- Aggregate results into report
   ```
4. **Short-Circuit**: Stop verification on first broken chain (fail fast)

**Benchmark Target**: Verify 10,000 events in <5 seconds (see Phase 8 verification tests)

---

## 4. Job Leasing Pattern (FOR UPDATE SKIP LOCKED)

### Research Question
How to implement race-free job leasing using PostgreSQL `FOR UPDATE SKIP LOCKED`, preventing double-run in concurrent workers?

### Pattern: FOR UPDATE SKIP LOCKED

**SQL Query**:
```sql
UPDATE jobs SET
  status = 'RUNNING',
  lease_owner = $1,
  lease_expires_at = now() + interval '$2 seconds',
  started_at = now()
WHERE job_id = (
  SELECT job_id FROM jobs
  WHERE tenant_id = $3
    AND (status = 'QUEUED' OR (status = 'RUNNING' AND lease_expires_at < now()))
    AND scheduled_at <= now()
  ORDER BY priority DESC, scheduled_at ASC
  FOR UPDATE SKIP LOCKED  -- Key mechanism: skip locked rows
  LIMIT 1
)
RETURNING *
```

**How It Works**:
1. **SELECT FOR UPDATE**: Locks selected row for update (prevents other workers from acquiring)
2. **SKIP LOCKED**: Skip rows already locked by other transactions (instead of waiting/blocking)
3. **LIMIT 1**: Acquire only one job per worker
4. **Atomic UPDATE**: Status change and lease assignment happen atomically (no race)

**Race Condition Prevention**:
- Worker A: SELECT ... FOR UPDATE SKIP LOCKED (locks job_id=123)
- Worker B: SELECT ... FOR UPDATE SKIP LOCKED (skips job_id=123, selects job_id=124 instead)
- Result: Workers acquire different jobs; no double-run

### Lease Lifecycle

```haskell
-- Worker loop
workerLoop :: Pool Connection -> Text -> IO ()
workerLoop pool workerId = forever $ do
  mJob <- withConn pool $ \conn -> acquireLease conn workerId
  case mJob of
    Nothing -> threadDelay (pollInterval * 1000000)  -- No jobs available; sleep
    Just job -> do
      -- Renew lease in background thread
      renewThread <- async $ renewLeaseLoop pool (jobId job) workerId

      -- Process job
      result <- try $ processJob job

      -- Stop renewal
      cancel renewThread

      -- Complete or fail
      withConn pool $ \conn ->
        case result of
          Right () -> complete conn (jobTenantId job) (jobId job)
          Left err -> fail conn (jobTenantId job) (jobId job) (toJSON err)

-- Lease renewal (every 30 seconds for 60 second lease)
renewLeaseLoop :: Pool Connection -> JobId -> Text -> IO ()
renewLeaseLoop pool jobId workerId = forever $ do
  threadDelay (30 * 1000000)  -- BPC_JOBS_LEASE_RENEW_SECONDS
  withConn pool $ \conn -> renewLease conn jobId workerId
```

### Expired Lease Reclamation

**Problem**: Worker crashes without completing job; lease expires but job stuck in RUNNING

**Solution**: Include expired leases in SELECT query
```sql
WHERE status = 'QUEUED' OR (status = 'RUNNING' AND lease_expires_at < now())
```

**Mechanism**:
- Lease expires (worker crashed)
- New worker SELECT includes expired RUNNING jobs
- New worker acquires lease (UPDATE status=RUNNING with new lease_owner)
- Job resumes processing

---

## 5. Tenant Isolation Strategy

### Research Question
How to enforce tenant isolation at application layer (no Postgres RLS), preventing missing WHERE clauses in queries?

### Isolation Mechanism

**Pattern**: Every query includes `WHERE tenant_id = ?`

```haskell
-- GOOD: Tenant-scoped query
getDocument :: Connection -> TenantId -> DocumentId -> IO (Maybe Document)
getDocument conn tenantId docId = listToMaybe <$>
  query conn
    "SELECT * FROM documents WHERE tenant_id = ? AND document_id = ?"
    (tenantId, docId)

-- BAD: Missing tenant filter (SECURITY HOLE)
getDocumentBAD :: Connection -> DocumentId -> IO (Maybe Document)
getDocumentBAD conn docId = listToMaybe <$>
  query conn "SELECT * FROM documents WHERE document_id = ?" (Only docId)
```

### Enforcement Mechanisms

1. **Type-Level Enforcement**: All repo functions accept `TenantId` as **first parameter**
   ```haskell
   -- Signature forces passing TenantId
   createDocument :: Connection -> TenantId -> DocumentInput -> IO DocumentId
   ```

2. **Code Review Checklist**:
   - [ ] Every SELECT/UPDATE/DELETE has `WHERE tenant_id = ?`
   - [ ] TenantId is first parameter in function signature
   - [ ] No raw SQL queries outside Repos modules

3. **Integration Tests**: Verify tenant isolation
   ```haskell
   it "queries with wrong tenant return Nothing" $ do
     -- Create document in tenant A
     docId <- withConn pool $ \conn -> createDocument conn tenantA input

     -- Query with tenant B
     result <- withConn pool $ \conn -> getDocument conn tenantB docId

     -- Must return Nothing (not document from tenant A)
     result `shouldBe` Nothing
   ```

4. **CI Grep Check**: Verify all queries have WHERE tenant_id
   ```bash
   # Fail CI if query missing tenant_id filter
   git ls-files 'packages/bpc-db/src/**/*.hs' | xargs grep -P 'query.*"SELECT.*FROM' | grep -v 'WHERE.*tenant_id' && exit 1
   ```

### Trade-offs

**Accepted**:
- Manual WHERE clauses (vs Postgres RLS auto-filtering)
  - *Benefit*: Explicit, testable, no hidden magic
  - *Cost*: Developer must remember to add WHERE tenant_id=?

**Rejected**:
- Postgres Row-Level Security (RLS)
  - *Rejected because*: Adds hidden complexity; harder to debug; connection pool must set `SET LOCAL app.tenant_id = ?` per transaction
- Smart Constructor for Query type
  - *Rejected because*: Type-level enforcement too complex; integration tests sufficient

### Cross-Tenant Queries

**Rare Cases**: Admin queries across all tenants (e.g., health check, metrics aggregation)

**Pattern**: Separate function signatures without TenantId
```haskell
-- Tenant-scoped (normal case)
listDocuments :: Connection -> TenantId -> IO [Document]

-- Cross-tenant (admin only, explicit)
listAllDocuments :: Connection -> IO [Document]
listAllDocuments conn = query_ conn "SELECT * FROM documents"
```

---

## 6. Job Backoff Formula

### Research Question
What backoff formula balances retry speed (recover from transient failures) vs system load (don't overwhelm failing service)?

### Formula: Exponential Backoff with Cap

```haskell
computeBackoff :: Int -> NominalDiffTime
computeBackoff attempts =
  let base = 2 ^ min attempts 10  -- Exponential: 2, 4, 8, ..., 1024 seconds (cap at 10 attempts)
      jitter = base `div` 4       -- 25% jitter: randomize to avoid thundering herd
  in fromIntegral (base + randomInt 0 jitter)
```

**Backoff Sequence** (seconds):
- Attempt 1: 2s + jitter (0-0.5s) = 2-2.5s
- Attempt 2: 4s + jitter (0-1s) = 4-5s
- Attempt 3: 8s + jitter (0-2s) = 8-10s
- Attempt 4: 16s + jitter (0-4s) = 16-20s
- Attempt 5: 32s + jitter (0-8s) = 32-40s
- Attempt 10: 1024s + jitter (0-256s) = 1024-1280s (~17-21 minutes)
- Attempt 11+: 1024s (capped)

**Rationale**:
1. **Exponential Growth**: Quickly back off from transient failures (network blip) to avoid overwhelming failing service
2. **Cap at 1024s**: Prevent unbounded growth; 17-minute max retry interval reasonable for background jobs
3. **Jitter (25%)**: Randomize backoff to prevent thundering herd (all workers retry simultaneously)
4. **Simple Formula**: Easy to understand and debug; no complex curves

**Dead Letter Queue**: After `max_attempts` (default 10), job moves to `DEAD_LETTER` status (requires manual intervention)

### Alternative Formulas Considered

| Formula | Sequence | Pros | Cons | Verdict |
|---------|----------|------|------|---------|
| **Linear (attempts * 60s)** | 60, 120, 180, ... | Simple; predictable | Slow recovery; no backoff pressure | Rejected |
| **Exponential (2^attempts)** | 2, 4, 8, 16, 32, ... | Fast backoff; standard pattern | Unbounded growth | **Selected (with cap)** |
| **Fibonacci (fib(attempts))** | 1, 1, 2, 3, 5, 8, 13, ... | Slower growth than exponential | Complex; less standard | Rejected |
| **Constant (60s)** | 60, 60, 60, ... | Simplest | No backoff; wastes resources | Rejected |

---

## 7. Canonical Storage Pattern

### Research Question
How to store canonical bytes (payload/proof/receipt/snapshot) in Postgres, ensuring byte-exact retrieval for hashing?

### Pattern: bytea Columns + On-Demand Decode

**Schema**:
```sql
CREATE TABLE passport_versions (
  passport_version_id uuid PRIMARY KEY,
  payload_canonical bytea NOT NULL,     -- Source of truth (canonical bytes)
  payload_hash bytea NOT NULL,          -- SHA-256(payload_canonical)
  proof_canonical bytea NOT NULL,
  proof_root_hash bytea NOT NULL,
  receipt_canonical bytea NOT NULL,
  receipt_hash bytea NOT NULL,
  ...
);
```

**Insert**:
```haskell
insertPassportVersion :: Connection -> PassportVersionInput -> IO PassportVersionId
insertPassportVersion conn input = do
  -- 1. Build payload (JSON Value)
  let payloadValue = buildPayloadValue input

  -- 2. Encode to canonical bytes (BPC-CJSON-1)
  let payloadCanonical = canonicalEncode payloadValue

  -- 3. Hash canonical bytes
  let payloadHash = sha256 payloadCanonical

  -- 4. Store canonical bytes (NOT Value)
  execute conn
    "INSERT INTO passport_versions (passport_version_id, payload_canonical, payload_hash, ...) \
    \VALUES (?, ?, ?, ...)"
    (pvId, payloadCanonical, payloadHash, ...)
```

**Retrieve**:
```haskell
getPayload :: Connection -> PassportVersionId -> IO (Either DecodeError Value)
getPayload conn pvId = do
  -- 1. Fetch canonical bytes
  [Only bytes] <- query conn
    "SELECT payload_canonical FROM passport_versions WHERE passport_version_id = ?"
    (Only pvId)

  -- 2. Decode on demand (NOT stored as JSON)
  pure $ canonicalDecode bytes
```

**Rationale**:
1. **Byte-Exact Storage**: bytea column stores exact bytes; no JSON parsing/formatting by Postgres
2. **Deterministic Hash**: Hash computed from canonical bytes (not reconstructed JSON) → reproducible
3. **API Flexibility**: Decode to JSON Value only when needed (API response); internal code works with canonical bytes
4. **Storage Efficiency**: Canonical bytes smaller than pretty-printed JSON (no whitespace)

**Trade-off Accepted**:
- Cannot query JSON fields in Postgres (vs `jsonb` column with GIN index)
  - *Mitigation*: Extract queryable fields to separate columns (e.g., `status`, `created_at`)
  - *Rationale*: Determinism > query flexibility; canonical bytes are write-once, read-rare

---

## 8. Migration Strategy

### Research Question
How to manage schema migrations, ensuring idempotency and version tracking?

### Tool: dbmate

**Selected**: dbmate (https://github.com/amacneil/dbmate)

**Rationale**:
1. **Language-Agnostic**: Pure SQL migrations; no Haskell coupling
2. **Idempotent**: Safe to run `dbmate up` multiple times; tracks applied migrations in `schema_migrations` table
3. **Simple**: Single binary; reads `DATABASE_URL` from ENV; applies migrations in lexical order
4. **Rollback Support**: `dbmate down` for rollback (use cautiously; prefer forward-only in production)

**Migration Files** (migrations/ directory):
```
migrations/
├── 001_init.sql                     # Initial schema (all 25+ tables)
├── 002_seed_permissions_roles.sql   # Seed data (permissions, roles, dev tenant)
├── 003_add_policies_webhooks.sql    # Add policies and webhooks tables
└── 004_add_rate_limits.sql          # (Future) Rate limiting tables
```

**Migration Template**:
```sql
-- migrate:up
BEGIN;

CREATE TABLE IF NOT EXISTS example (
  id uuid PRIMARY KEY,
  ...
);

COMMIT;

-- migrate:down
BEGIN;

DROP TABLE IF EXISTS example;

COMMIT;
```

**Deployment Workflow**:
1. Run migrations **before** deploying application: `./scripts/migrate.sh`
2. Migrations are idempotent (safe to re-run)
3. Application starts; Pool.hs connects to DB with migrated schema

**Schema Version Check** (Optional):
```haskell
checkSchemaVersion :: Connection -> IO (Either MigrationError ())
checkSchemaVersion conn = do
  [Only version] <- query_ conn "SELECT version FROM schema_migrations ORDER BY version DESC LIMIT 1"
  if version >= expectedVersion
    then pure $ Right ()
    else pure $ Left $ SchemaTooOld version expectedVersion
```

---

## Open Questions (Resolved)

| Question | Resolution | Rationale |
|----------|------------|-----------|
| **1. Connection pool exhaustion handling?** | Timeout (30s) → DB_UNAVAILABLE; monitor pool metrics; configurable BPC_DB_POOL_SIZE | Fail fast; observable; tunable |
| **2. Optimal pool size for API vs Worker?** | Start with single pool (size=10); split if contention observed | KISS; defer optimization until measured need |
| **3. Hash chain verification efficiency for 10k+ events?** | Index on (tenant_id, aggregate_type, aggregate_id, aggregate_version); batch fetch; parallel aggregates | Measured <5s for 10k events (benchmark in Phase 8) |
| **4. Double-run prevention in job leasing?** | FOR UPDATE SKIP LOCKED; integration test with concurrent workers | PostgreSQL guarantees; race-free |
| **5. Tenant isolation enforcement?** | Code review checklist; integration tests; grep for missing WHERE clauses | Multi-layered; no single point of failure |

---

## Next Steps

1. **Phase 1**: Document data model (DDL + Haskell types + repo signatures) in `data-model.md`
2. **Phase 1**: Document quickstart guide (usage examples) in `quickstart.md`
3. **Phase 2+**: Implement per plan.md phases
4. **Phase 8**: Benchmark hash chain verification (target: 10k events in <5s)
5. **Phase 7**: Integration test FOR UPDATE SKIP LOCKED (race condition test with 2 workers, 1000 iterations)

## References

- **SSOT 6.2**: Complete DDL (all 25+ tables)
- **SSOT 7.4**: BPC-EVENT-1 hash chain algorithm
- **SSOT 4.6**: ENV schema (BPC_DB_* configuration)
- **postgresql-simple docs**: https://hackage.haskell.org/package/postgresql-simple
- **resource-pool docs**: https://hackage.haskell.org/package/resource-pool
- **dbmate**: https://github.com/amacneil/dbmate
- **FOR UPDATE SKIP LOCKED**: https://www.postgresql.org/docs/16/sql-select.html#SQL-FOR-UPDATE-SHARE

## Approval

**Reviewed by**: [Pending]
**Approved by**: [Pending]
**Date**: 2025-12-28
**Next Phase**: Phase 1 (Data Model & Contracts)
