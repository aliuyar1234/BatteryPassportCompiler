# Implementation Plan: Job Processing (Worker)

**Branch**: `07-job-processing` | **Date**: 2025-12-28 | **Spec**: [.specify/features/07-job-processing/spec.md](../../.specify/features/07-job-processing/spec.md)
**Input**: Feature specification from `.specify/features/07-job-processing/spec.md`
**Phase**: P2-P3 | **Package**: bpc-worker | **Status**: Planning

## Summary

Implement the worker process that handles asynchronous job processing using database polling with optional RabbitMQ integration. The worker uses `FOR UPDATE SKIP LOCKED` for distributed job leasing, ensuring no job is processed twice. Implements exponential backoff retry logic with a maximum of 5 attempts before moving jobs to DEAD_LETTER status. Covers all job types: PARSE_FACTS, BUILD_SNAPSHOT, COMPILE_PASSPORT, SIGN_PASSPORT, GENERATE_QR, RUN_RULE_TESTS, and DELIVER_WEBHOOK.

**Technical Approach**:
- **Worker Loop**: Continuous polling with configurable interval (default 1000ms)
- **Job Leasing**: Database-backed using `FOR UPDATE SKIP LOCKED` with 60s TTL, renewal every 30s
- **Concurrency**: Horizontal scaling via multiple workers without coordination overhead
- **Retry Strategy**: Exponential backoff `2^attempts` seconds, capped at 1024s
- **Max Attempts**: 5 attempts (MUST requirement from user), then DEAD_LETTER
- **Handler Isolation**: Each job type has dedicated handler module

## Technical Context

**Language/Version**: Haskell GHC 9.6.4
**Primary Dependencies**:
  - postgresql-simple (DB polling & leasing)
  - amqp (optional RabbitMQ trigger)
  - async (lease renewal threads)
  - qrcode (QR PNG generation)
  - http-client (webhook delivery)

**Storage**: PostgreSQL via bpc-db repositories
**Testing**: tasty-hunit, tasty-quickcheck, docker-compose for integration
**Target Platform**: Linux server (Docker)
**Performance Goals**: Process 100+ jobs/minute per worker instance
**Constraints**:
  - Lease TTL 60s (`BPC_JOBS_LEASE_SECONDS`)
  - Lease renewal every 30s (`BPC_JOBS_LEASE_RENEW_SECONDS`)
  - Max attempts 5 (MUST per user requirement)
  - Deterministic compilation via `compilePassportPure`

**Scale/Scope**:
  - ~1500 LOC across all handlers
  - 9 job types
  - 7 primary handlers
  - Support for horizontal scaling (10+ workers)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. Determinism** | ✅ USES | COMPILE_PASSPORT uses pure `compilePassportPure` function; same inputs → identical artifacts |
| **II. Canonical Storage** | ✅ ENFORCED | All artifacts stored as canonical bytes; handlers never reconstruct from JSON |
| **III. Immutability** | ✅ ENFORCED | PassportVersion, Facts, SEALED Snapshots are immutable; status transitions only |
| **IV. Audit Trail** | ✅ ENFORCED | Every handler action appends to Event Store with hash chain |
| **V. Layered Architecture** | ✅ ENFORCED | bpc-worker imports only bpc-core + bpc-db; no API dependency |
| **VI. Type-Safe Rules** | ✅ USES | RUN_RULE_TESTS validates typed rules; COMPILE uses type-checked DSL |

**Quality Gates**:
- ✅ Formatting: fourmolu check mode
- ✅ Linting: hlint with project hlint.yaml
- ✅ Test Coverage: ≥ 75% (per constitution requirement)
- ✅ Integration Tests: Full pipeline coverage with docker-compose.test.yml

## Project Structure

### Documentation (this feature)

```text
specs/07-job-processing/
├── plan.md              # This file (implementation plan)
├── research.md          # Phase 0: Polling vs Queue, Leasing, Retry
├── data-model.md        # Phase 1: Job payloads, Handler types
├── quickstart.md        # Phase 1: How to run, monitor, retry
└── contracts/           # N/A (internal service, no public API)
```

### Source Code (repository root)

```text
packages/bpc-worker/
├── bpc-worker.cabal
├── src/
│   └── BPC/
│       └── Worker/
│           ├── Main.hs              # Entry point: load config, start runner
│           ├── Runner.hs            # Job loop: poll, lease, dispatch, renew
│           ├── Dispatch.hs          # Job type → handler mapping
│           ├── Types.hs             # WorkerConfig, HandlerError, Result
│           ├── Retry.hs             # Backoff computation, retry logic
│           └── Handlers/
│               ├── ParseFacts.hs        # Parse DocumentVersion → Facts
│               ├── BuildSnapshot.hs     # BUILDING → READY (future)
│               ├── CompilePassport.hs   # Snapshot+Rules → PassportVersion
│               ├── SignPassport.hs      # ED25519 signature over receipt
│               ├── GenerateQR.hs        # QR PNG + payload string
│               ├── RunRuleTests.hs      # Rule test execution (500+ cases)
│               └── DeliverWebhook.hs    # HMAC-signed webhook delivery
└── test/
    ├── Main.hs
    ├── unit/
    │   └── BPC/
    │       └── Worker/
    │           ├── RetrySpec.hs         # Backoff formula tests
    │           └── HandlersSpec.hs      # Handler unit tests
    └── integration/
        └── BPC/
            └── Worker/
                └── E2ESpec.hs           # Full pipeline with DB

migrations/
└── (No new migrations - uses existing jobs table from 05-data-layer)
```

**Structure Decision**: Single project (bpc-worker package) within the existing multi-package Cabal workspace. All handlers live under `BPC.Worker.Handlers.*` namespace. Worker loop and dispatch are in `BPC.Worker.Runner` and `BPC.Worker.Dispatch` respectively.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

None - all constitution principles are satisfied.

## Implementation Phases

### Phase 0: Lease Renewal Thread Pattern (Infrastructure)

**Goal**: Establish the pattern for background lease renewal that all handlers will use.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Runner.hs` (lease renewal logic)

**Key Patterns**:

```haskell
-- Lease renewal in background thread
leaseRenewalLoop :: WorkerConfig -> Pool Connection -> Job -> IO ()
leaseRenewalLoop config pool job = forever $ do
  threadDelay (wcLeaseRenewSecs config * 1_000_000)
  withConn pool $ \conn ->
    renewLease conn (jTenantId job) (jJobId job) (wcWorkerId config)
```

**Test Cases**:
- TC-P0-01: Lease renewal updates `lease_expires_at`
- TC-P0-02: Lease renewal stops when job completes
- TC-P0-03: Multiple workers don't interfere (SKIP LOCKED)

**Acceptance Criteria**:
- [ ] Lease renewal thread successfully extends lease every 30s
- [ ] Thread cancels cleanly when job completes
- [ ] Expired lease allows new worker to acquire job

---

### Phase 1: Worker Loop & Job Dispatch

**Goal**: Core worker loop that polls jobs, acquires leases, dispatches to handlers, and handles completion/failure.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Main.hs`
- `packages/bpc-worker/src/BPC/Worker/Runner.hs`
- `packages/bpc-worker/src/BPC/Worker/Dispatch.hs`
- `packages/bpc-worker/src/BPC/Worker/Types.hs`
- `packages/bpc-worker/src/BPC/Worker/Retry.hs`

**Key Algorithms**:

```haskell
-- Main worker loop
runWorker :: WorkerConfig -> Pool Connection -> IO ()
runWorker config pool = forever $ do
  mJob <- withConn pool $ \conn -> acquireLease conn (wcWorkerId config)

  case mJob of
    Nothing -> threadDelay (wcPollIntervalMs config * 1000)
    Just job -> do
      -- Start lease renewal thread
      renewThread <- async $ leaseRenewalLoop config pool job

      -- Process job with exception handling
      result <- try $ processJob config pool job

      -- Cancel renewal
      cancel renewThread

      -- Handle result
      withConn pool $ \conn -> handleJobResult conn job result

-- Job dispatch
processJob :: WorkerConfig -> Pool Connection -> Job -> IO ()
processJob config pool job = case jType job of
  PARSE_FACTS      -> Handlers.parseFacts pool job
  BUILD_SNAPSHOT   -> Handlers.buildSnapshot pool job
  COMPILE_PASSPORT -> Handlers.compilePassport config pool job
  SIGN_PASSPORT    -> Handlers.signPassport config pool job
  GENERATE_QR      -> Handlers.generateQR pool job
  RUN_RULE_TESTS   -> Handlers.runRuleTests pool job
  DELIVER_WEBHOOK  -> Handlers.deliverWebhook pool job
  _                -> throwIO $ UnsupportedJobType (jType job)

-- Exponential backoff: 2^attempts seconds, capped at 1024
computeBackoff :: Int -> NominalDiffTime
computeBackoff attempts = fromIntegral $ min 1024 (2 ^ attempts)

-- Max attempts = 5 (MUST per user requirement)
maxAttempts :: Int
maxAttempts = 5

-- Result handler with retry logic
handleJobResult :: Connection -> Job -> Either SomeException () -> IO ()
handleJobResult conn job result = case result of
  Right () ->
    completeJob conn (jTenantId job) (jJobId job)

  Left err
    | jAttempts job >= maxAttempts ->
        updateJobStatus conn (jTenantId job) (jJobId job) DEAD_LETTER (toJSON err)

    | isRetryable err -> do
        let backoff = computeBackoff (jAttempts job)
        let scheduledAt = addUTCTime backoff <$> getCurrentTime
        retryJob conn (jTenantId job) (jJobId job) scheduledAt (toJSON err)

    | otherwise ->
        updateJobStatus conn (jTenantId job) (jJobId job) FAILED (toJSON err)

-- Retryable errors: network, DB transient
isRetryable :: SomeException -> Bool
isRetryable ex = case fromException ex of
  Just (HttpException _) -> True
  Just (DBUnavailable _) -> True
  Just (QueueUnavailable _) -> True
  _ -> False
```

**Test Cases**:
- TC-P1-01: Worker polls at configured interval when queue empty
- TC-P1-02: Worker acquires job using FOR UPDATE SKIP LOCKED
- TC-P1-03: Multiple workers don't acquire same job
- TC-P1-04: Lease renewal extends expiry time
- TC-P1-05: Job completion cancels lease renewal thread
- TC-P1-06: Failed job with attempts < 5 → QUEUED with backoff
- TC-P1-07: Failed job with attempts = 5 → DEAD_LETTER
- TC-P1-08: Non-retryable error → FAILED immediately
- TC-P1-09: Unsupported job type throws error

**Acceptance Criteria**:
- [ ] Worker loop runs continuously without crashes
- [ ] Jobs are acquired with database lock (no double-processing)
- [ ] Lease renewal thread keeps job lease alive
- [ ] Successful jobs transition to SUCCEEDED
- [ ] Failed jobs retry with exponential backoff (max 5 attempts)
- [ ] Max attempts reached → DEAD_LETTER status
- [ ] Integration test: 10 workers process 100 jobs without duplicates

---

### Phase 2: PARSE_FACTS Handler

**Goal**: Parse uploaded DocumentVersions and extract Facts with canonical bytes and hashes.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/ParseFacts.hs`
- `packages/bpc-worker/test/unit/BPC/Worker/Handlers/ParseFactsSpec.hs`

**Algorithm**:

```haskell
module BPC.Worker.Handlers.ParseFacts where

import BPC.Core.CanonicalJson (canonicalEncode)
import BPC.Core.Hash (sha256)
import BPC.DB.Repos.Documents (getDocumentVersion, updateDocumentVersionStatus)
import BPC.DB.Repos.Facts (insertFact)

parseFacts :: Pool Connection -> Job -> IO ()
parseFacts pool job = do
  let payload = jPayload job
      docVersionId = parseDocVersionId $ payload ^. key "document_version_id"

  -- Load document version
  docVersion <- withConn pool $ \conn ->
    getDocumentVersion conn (jTenantId job) docVersionId

  -- Parse based on document kind
  facts <- case dvKind docVersion of
    BOM -> parseBOM (dvContent docVersion)
    PCF -> parsePCF (dvContent docVersion)
    DUE_DILIGENCE -> parseDD (dvContent docVersion)
    OTHER -> throwIO $ UnsupportedDocumentKind OTHER

  -- Canonicalize and hash each fact
  factsWithHashes <- forM facts $ \fact -> do
    canonical <- either throwIO pure $ canonicalEncode (pfPayload fact)
    let hash = sha256 canonical
    pure $ fact { pfPayloadCanonical = canonical, pfPayloadHash = hash }

  -- Insert facts
  forM_ factsWithHashes $ \fact ->
    withConn pool $ \conn ->
      insertFact conn (jTenantId job) InsertFactInput
        { ifiFactType = pfFactType fact
        , ifiFactKey = pfFactKey fact
        , ifiSchemaVersion = pfSchemaVersion fact
        , ifiSourceDocVersionId = docVersionId
        , ifiPayload = pfPayload fact
        , ifiPayloadCanonical = pfPayloadCanonical fact
        , ifiPayloadHash = pfPayloadHash fact
        }

  -- Update document status to VALIDATED
  withConn pool $ \conn ->
    updateDocumentVersionStatus conn (jTenantId job) docVersionId VALIDATED

  -- Emit audit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "DocumentVersion"
      , aeiAggregateId = docVersionId
      , aeiEventType = "FACTS_PARSED"
      , aeiActorId = Nothing  -- System action
      , aeiPayload = object ["fact_count" .= length factsWithHashes]
      }

-- BOM parser (simplified)
parseBOM :: ByteString -> IO [ParsedFact]
parseBOM content = do
  bomData <- either throwIO pure $ eitherDecodeStrict content
  pure $ bomData ^.. key "components" . values . to parseBatteryComponent

parseBatteryComponent :: Value -> ParsedFact
parseBatteryComponent val = ParsedFact
  { pfFactType = "Battery"
  , pfFactKey = "battery:" <> (val ^. key "sku" . _String)
  , pfSchemaVersion = 1
  , pfPayload = val
  }
```

**Test Cases**:
- TC-P2-01: Valid BOM document → Facts created with canonical bytes
- TC-P2-02: Valid PCF document → Facts created
- TC-P2-03: Invalid document format → REJECTED status
- TC-P2-04: Fact canonical encoding is deterministic
- TC-P2-05: Fact hash matches SHA-256 of canonical bytes
- TC-P2-06: DocumentVersion status → VALIDATED after success
- TC-P2-07: Audit event emitted with fact count

**Acceptance Criteria**:
- [ ] BOM documents parsed correctly
- [ ] PCF documents parsed correctly
- [ ] Facts have canonical bytes and SHA-256 hash
- [ ] DocumentVersion status updated to VALIDATED
- [ ] Invalid documents rejected with error details
- [ ] Audit event logged

---

### Phase 3: BUILD_SNAPSHOT Handler (If Needed)

**Goal**: Transition snapshots from BUILDING to READY status.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/BuildSnapshot.hs`

**Note**: This handler is minimal since snapshot building is mostly done via API when adding items. The handler just validates and transitions status.

**Algorithm**:

```haskell
module BPC.Worker.Handlers.BuildSnapshot where

buildSnapshot :: Pool Connection -> Job -> IO ()
buildSnapshot pool job = do
  let snapshotId = parseSnapshotId $ jPayload job ^. key "snapshot_id"

  -- Load snapshot
  snapshot <- withConn pool $ \conn ->
    getSnapshot conn (jTenantId job) snapshotId

  -- Verify it's in BUILDING state
  unless (sStatus snapshot == BUILDING) $
    throwIO $ InvalidSnapshotState (sStatus snapshot)

  -- Update to READY
  withConn pool $ \conn ->
    updateSnapshotStatus conn (jTenantId job) snapshotId READY

  -- Emit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "DataSnapshot"
      , aeiAggregateId = snapshotId
      , aeiEventType = "SNAPSHOT_READY"
      , aeiActorId = Nothing
      , aeiPayload = object []
      }
```

**Test Cases**:
- TC-P3-01: BUILDING snapshot → READY
- TC-P3-02: Empty snapshot (no items) → still READY
- TC-P3-03: Already READY snapshot → error
- TC-P3-04: Audit event emitted

**Acceptance Criteria**:
- [ ] BUILDING snapshot transitions to READY
- [ ] Empty snapshots handled correctly
- [ ] Invalid state transitions rejected
- [ ] Audit event logged

---

### Phase 4: COMPILE_PASSPORT Handler

**Goal**: Compile passport using SEALED snapshot and PUBLISHED rules via pure `compilePassportPure` function.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/CompilePassport.hs`
- `packages/bpc-worker/test/unit/BPC/Worker/Handlers/CompilePassportSpec.hs`
- `packages/bpc-worker/test/integration/BPC/Worker/E2ESpec.hs` (includes compilation)

**Algorithm**:

```haskell
module BPC.Worker.Handlers.CompilePassport where

import BPC.Core.Eval (compilePassportPure, CompileInput(..), CompileOutput(..))
import BPC.DB.Repos.Snapshots (getSnapshot, getSnapshotItems)
import BPC.DB.Repos.Rules (getRulePackageVersion)
import BPC.DB.Repos.Passports (insertPassportVersion)
import BPC.DB.Repos.Jobs (enqueue)

compilePassport :: WorkerConfig -> Pool Connection -> Job -> IO ()
compilePassport config pool job = do
  let payload = jPayload job
      passportId = parsePassportId $ payload ^. key "passport_id"
      snapshotId = parseSnapshotId $ payload ^. key "snapshot_id"
      rulesId = parseRulesPkgVersionId $ payload ^. key "rule_package_version_id"

  -- Load snapshot (must be SEALED)
  snapshot <- withConn pool $ \conn ->
    getSnapshot conn (jTenantId job) snapshotId

  unless (sStatus snapshot == SEALED) $
    throwIO $ SnapshotNotReady snapshotId SEALED (sStatus snapshot)

  snapshotItems <- withConn pool $ \conn ->
    getSnapshotItems conn (jTenantId job) snapshotId

  -- Load rules (must be PUBLISHED)
  rules <- withConn pool $ \conn ->
    getRulePackageVersion conn (jTenantId job) rulesId

  unless (rpvStatus rules == PUBLISHED) $
    throwIO $ RulesNotPublished rulesId PUBLISHED (rpvStatus rules)

  -- Load battery product
  product <- withConn pool $ \conn ->
    getBatteryProductByPassport conn (jTenantId job) passportId

  -- Prepare compile input
  issuedAt <- getCurrentTime
  let input = CompileInput
        { ciSnapshot = snapshot
        , ciSnapshotItems = snapshotItems
        , ciRules = rules
        , ciProduct = product
        , ciIssuedAt = issuedAt
        , ciBuildId = wcBuildId config
        }

  -- PURE COMPILATION (deterministic!)
  output <- case compilePassportPure input of
    Left err -> throwIO $ CompileError err
    Right out -> pure out

  -- Verify size limits
  when (BS.length (coPayloadCanonical output) > maxPayloadBytes) $
    throwIO PayloadTooLarge

  when (BS.length (coProofCanonical output) > maxProofBytes) $
    throwIO ProofTooLarge

  when (BS.length (coReceiptCanonical output) > maxReceiptBytes) $
    throwIO ReceiptTooLarge

  -- Create passport version
  pvId <- withConn pool $ \conn ->
    insertPassportVersion conn (jTenantId job) InsertPassportVersionInput
      { ipviPassportId = passportId
      , ipviSnapshotId = snapshotId
      , ipviRulePackageVersionId = rulesId
      , ipviPayloadCanonical = coPayloadCanonical output
      , ipviPayloadHash = coPayloadHash output
      , ipviProofCanonical = coProofCanonical output
      , ipviProofRootHash = coProofRootHash output
      , ipviReceiptCanonical = coReceiptUnsigned output
      , ipviReceiptHash = coReceiptHash output
      , ipviCompilerBuildId = wcBuildId config
      , ipviIssuedAt = issuedAt
      }

  -- Emit audit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "PassportVersion"
      , aeiAggregateId = pvId
      , aeiEventType = "PASSPORT_COMPILED"
      , aeiActorId = Nothing
      , aeiPayload = object
          [ "snapshot_id" .= snapshotId
          , "rules_id" .= rulesId
          , "build_id" .= wcBuildId config
          ]
      }

  -- Enqueue SIGN_PASSPORT job
  withConn pool $ \conn ->
    enqueue conn (jTenantId job) EnqueueInput
      { eiType = SIGN_PASSPORT
      , eiPayload = object ["passport_version_id" .= pvId]
      , eiPriority = 1
      , eiMaxAttempts = Just 5
      , eiIdempotencyKey = Just $ "sign-" <> toText pvId
      }

-- Size limits from ENV
maxPayloadBytes, maxProofBytes, maxReceiptBytes :: Int
maxPayloadBytes = 131_072   -- BPC_PAYLOAD_MAX_BYTES
maxProofBytes = 262_144     -- BPC_PROOF_MAX_BYTES
maxReceiptBytes = 16_384    -- BPC_RECEIPT_MAX_BYTES
```

**Test Cases**:
- TC-P4-01: Valid sealed snapshot + published rules → PassportVersion COMPILING
- TC-P4-02: Snapshot not SEALED → SNAPSHOT_NOT_READY error
- TC-P4-03: Rules not PUBLISHED → RULE_PKG_NOT_PUBLISHED error
- TC-P4-04: Compilation produces deterministic output (golden test)
- TC-P4-05: Payload exceeds max size → PAYLOAD_TOO_LARGE error
- TC-P4-06: Proof exceeds max size → PROOF_TOO_LARGE error
- TC-P4-07: Receipt exceeds max size → RECEIPT_TOO_LARGE error
- TC-P4-08: SIGN_PASSPORT job enqueued after success
- TC-P4-09: Audit event logged

**Acceptance Criteria**:
- [ ] Only SEALED snapshots and PUBLISHED rules accepted
- [ ] `compilePassportPure` called with correct inputs
- [ ] Output artifacts stored as canonical bytes
- [ ] Size limits enforced
- [ ] PassportVersion created with COMPILING status
- [ ] SIGN_PASSPORT job enqueued
- [ ] Determinism verified via golden tests
- [ ] Audit event logged

---

### Phase 5: SIGN_PASSPORT Handler

**Goal**: Sign the receipt hash using ED25519 private key.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/SignPassport.hs`

**Algorithm**:

```haskell
module BPC.Worker.Handlers.SignPassport where

import Crypto.Sign.Ed25519 (sign, SecretKey, toPublicKey)
import qualified Data.ByteString.Base64 as B64

signPassport :: WorkerConfig -> Pool Connection -> Job -> IO ()
signPassport config pool job = do
  let pvId = parsePassportVersionId $ jPayload job ^. key "passport_version_id"

  -- Load passport version
  pv <- withConn pool $ \conn ->
    getPassportVersion conn (jTenantId job) pvId

  -- Verify status is COMPILING
  unless (pvStatus pv == COMPILING) $
    throwIO $ InvalidPassportStatus pvId COMPILING (pvStatus pv)

  -- Load signing key from ENV
  privateKeyB64 <- lookupEnv "BPC_SIGNING_ED25519_PRIVATE_KEY_BASE64" >>= \case
    Nothing -> throwIO SigningKeyMissing
    Just k -> pure k

  privateKey <- case B64.decode (encodeUtf8 privateKeyB64) of
    Left err -> throwIO $ InvalidSigningKey err
    Right bs -> pure $ SecretKey bs

  let keyId = wcSigningKeyId config
      publicKey = toPublicKey privateKey

  -- Sign receipt hash (BPC-SIGN-1: ED25519 over receipt hash)
  let receiptHash = pvReceiptHash pv
      signature = sign privateKey receiptHash

  -- Update passport version with signature
  withConn pool $ \conn ->
    updatePassportVersionSignature conn (jTenantId job) pvId UpdateSignatureInput
      { usiSignature = signature
      , usiSigningKeyId = keyId
      , usiSigningKeyPublic = publicKey
      }

  -- Update status to SIGNED
  withConn pool $ \conn ->
    updatePassportVersionStatus conn (jTenantId job) pvId SIGNED

  -- Emit audit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "PassportVersion"
      , aeiAggregateId = pvId
      , aeiEventType = "PASSPORT_SIGNED"
      , aeiActorId = Nothing
      , aeiPayload = object ["key_id" .= keyId]
      }

  -- Enqueue GENERATE_QR job
  withConn pool $ \conn ->
    enqueue conn (jTenantId job) EnqueueInput
      { eiType = GENERATE_QR
      , eiPayload = object ["passport_version_id" .= pvId]
      , eiPriority = 2  -- Lower priority than signing
      , eiMaxAttempts = Just 5
      , eiIdempotencyKey = Just $ "qr-" <> toText pvId
      }
```

**Test Cases**:
- TC-P5-01: Valid COMPILING passport → SIGNED with signature
- TC-P5-02: Signature verifies with public key
- TC-P5-03: Signing key missing → SIGNING_KEY_MISSING error
- TC-P5-04: Invalid key format → error
- TC-P5-05: GENERATE_QR job enqueued
- TC-P5-06: Status transitions COMPILING → SIGNED
- TC-P5-07: Audit event logged

**Acceptance Criteria**:
- [ ] ED25519 signature computed over receipt hash
- [ ] Signature stored in database
- [ ] Public key stored for verification
- [ ] Status updated to SIGNED
- [ ] GENERATE_QR job enqueued
- [ ] Signature verifiable (integration test)
- [ ] Audit event logged

---

### Phase 6: GENERATE_QR Handler

**Goal**: Generate QR code PNG and payload string in BPC-QR-1 format.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/GenerateQR.hs`

**Algorithm**:

```haskell
module BPC.Worker.Handlers.GenerateQR where

import BPC.Core.QR (buildQrPayload, QrInput(..))
import BPC.Core.Hash (base32NoPad)
import qualified Codec.Picture.Png as PNG
import qualified Codec.QRCode as QR

generateQR :: Pool Connection -> Job -> IO ()
generateQR pool job = do
  let pvId = parsePassportVersionId $ jPayload job ^. key "passport_version_id"

  -- Load passport version
  pv <- withConn pool $ \conn ->
    getPassportVersion conn (jTenantId job) pvId

  -- Verify status is SIGNED
  unless (pvStatus pv == SIGNED) $
    throwIO $ InvalidPassportStatus pvId SIGNED (pvStatus pv)

  -- Build QR payload string (BPC-QR-1 format)
  -- Format: BPC1|pv=<uuid>|ph=<base32>|pr=<base32>|rh=<base32>
  let qrPayload = buildQrPayload QrInput
        { qiPassportVersionId = pvId
        , qiPayloadHash = base32NoPad (pvPayloadHash pv)
        , qiProofRootHash = base32NoPad (pvProofRootHash pv)
        , qiReceiptHash = base32NoPad (pvReceiptHash pv)
        }

  -- Generate QR code
  qrCode <- case QR.encodeText qrPayload QR.M of
    Nothing -> throwIO $ QREncodeFailed qrPayload
    Just qr -> pure qr

  -- Render to PNG (200x200 pixels)
  let qrImage = QR.toImage 10 0 qrCode  -- 10px per module, 0px border
      qrPng = PNG.encodePng qrImage

  -- Update passport version with QR data
  withConn pool $ \conn ->
    updatePassportVersionQR conn (jTenantId job) pvId UpdateQRInput
      { uqiQrPayload = qrPayload
      , uqiQrPng = qrPng
      }

  -- Emit audit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "PassportVersion"
      , aeiAggregateId = pvId
      , aeiEventType = "QR_GENERATED"
      , aeiActorId = Nothing
      , aeiPayload = object ["qr_payload" .= qrPayload]
      }
```

**Test Cases**:
- TC-P6-01: Valid SIGNED passport → QR PNG generated
- TC-P6-02: QR payload matches BPC-QR-1 format
- TC-P6-03: QR code scannable and contains correct data
- TC-P6-04: Base32 encoding has no padding
- TC-P6-05: Status remains SIGNED (no transition)
- TC-P6-06: Audit event logged

**Acceptance Criteria**:
- [ ] QR payload string in BPC-QR-1 format
- [ ] PNG image generated and stored
- [ ] QR code scannable (integration test with decoder)
- [ ] Base32 hashes have no padding
- [ ] Status remains SIGNED
- [ ] Audit event logged

---

### Phase 7: RUN_RULE_TESTS Handler (P2 Priority)

**Goal**: Execute rule tests and require ≥500 passing test cases before allowing PUBLISH.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/RunRuleTests.hs`

**Algorithm**:

```haskell
module BPC.Worker.Handlers.RunRuleTests where

import BPC.Core.Rules.Tests (parseTestSuite, runTests, TestResult(..))

runRuleTests :: Pool Connection -> Job -> IO ()
runRuleTests pool job = do
  let rpvId = parseRulesPkgVersionId $ jPayload job ^. key "rule_package_version_id"
      seed = jPayload job ^. key "seed" . _Integer

  -- Load rule package version
  rpv <- withConn pool $ \conn ->
    getRulePackageVersion conn (jTenantId job) rpvId

  -- Parse test suite
  testSuite <- case parseTestSuite (rpvTestsSource rpv) of
    Left err -> throwIO $ TestParseFailed err
    Right ts -> pure ts

  -- Run all tests with seed
  results <- runTests testSuite seed

  let (passed, failed) = partition (\r -> trStatus r == Passed) results
      totalCases = length results
      passedCount = length passed
      failedCount = length failed

  -- Determine status: PASSED if all passed AND >= 500 cases
  let runStatus = if failedCount == 0 && passedCount >= 500
                  then "PASSED"
                  else "FAILED"

  let firstFailure = listToMaybe failed >>= \r -> Just $ object
        [ "test_name" .= trTestName r
        , "error" .= trError r
        ]

  -- Store test run result
  runId <- withConn pool $ \conn ->
    insertTestRun conn (jTenantId job) InsertTestRunInput
      { itriRulePackageVersionId = rpvId
      , itriSeed = seed
      , itriTotalCases = totalCases
      , itriPassedCount = passedCount
      , itriFailedCount = failedCount
      , itriStatus = runStatus
      , itriFirstFailure = firstFailure
      }

  -- Update rule package version status if passed
  when (runStatus == "PASSED") $
    withConn pool $ \conn ->
      updateRulePackageVersionStatus conn (jTenantId job) rpvId VALIDATED

  -- Emit audit event
  withConn pool $ \conn ->
    appendEvent conn (jTenantId job) AppendEventInput
      { aeiAggregateType = "RulePackageVersion"
      , aeiAggregateId = rpvId
      , aeiEventType = "TESTS_RUN"
      , aeiActorId = Nothing
      , aeiPayload = object
          [ "run_id" .= runId
          , "status" .= runStatus
          , "total_cases" .= totalCases
          , "passed" .= passedCount
          ]
      }
```

**Test Cases**:
- TC-P7-01: All tests pass + ≥500 cases → VALIDATED
- TC-P7-02: Tests pass but <500 cases → DRAFT (not validated)
- TC-P7-03: Tests fail → DRAFT with failure details
- TC-P7-04: Test results stored in rule_tests_runs table
- TC-P7-05: Audit event logged

**Acceptance Criteria**:
- [ ] Test suite parsed correctly
- [ ] All tests executed with provided seed
- [ ] ≥500 passing cases required for VALIDATED
- [ ] Failure details stored for inspection
- [ ] RulePackageVersion status updated on success
- [ ] Audit event logged

---

### Phase 8: DELIVER_WEBHOOK Handler (P3 Priority)

**Goal**: Deliver webhook events to configured endpoints with HMAC signature.

**Files Created**:
- `packages/bpc-worker/src/BPC/Worker/Handlers/DeliverWebhook.hs`

**Algorithm**:

```haskell
module BPC.Worker.Handlers.DeliverWebhook where

import Crypto.Hash.SHA256 (hmac)
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Base16 as B16

deliverWebhook :: Pool Connection -> Job -> IO ()
deliverWebhook pool job = do
  let deliveryId = parseWebhookDeliveryId $ jPayload job ^. key "webhook_delivery_id"

  -- Load delivery
  delivery <- withConn pool $ \conn ->
    getWebhookDelivery conn (jTenantId job) deliveryId

  -- Load endpoint
  endpoint <- withConn pool $ \conn ->
    getWebhookEndpoint conn (jTenantId job) (wdEndpointId delivery)

  -- Load event
  event <- withConn pool $ \conn ->
    getEvent conn (jTenantId job) (wdEventId delivery)

  -- Build request body
  let body = encode $ object
        [ "event_id" .= eEventId event
        , "event_type" .= eEventType event
        , "aggregate_type" .= eAggregateType event
        , "aggregate_id" .= eAggregateId event
        , "occurred_at" .= eOccurredAt event
        , "payload" .= ePayload event
        ]

  -- Compute HMAC-SHA256 signature
  let signature = hmac (weSecret endpoint) body
      signatureHex = B16.encode signature

  -- Send HTTP request
  manager <- HTTP.newManager HTTP.defaultManagerSettings
  request <- HTTP.parseRequest (weUrl endpoint)

  let request' = request
        { HTTP.method = "POST"
        , HTTP.requestHeaders =
            [ ("Content-Type", "application/json")
            , ("X-BPC-Signature", "sha256=" <> signatureHex)
            , ("X-BPC-Event-Type", encodeUtf8 $ eEventType event)
            , ("X-BPC-Delivery-Id", encodeUtf8 $ toText deliveryId)
            ]
        , HTTP.requestBody = HTTP.RequestBodyLBS body
        }

  -- Execute with timeout
  result <- timeout (30 * 1_000_000) $ try $ HTTP.httpLbs request' manager

  case result of
    Nothing -> do
      -- Timeout
      withConn pool $ \conn ->
        updateWebhookDeliveryFailed conn (jTenantId job) deliveryId "timeout"
      throwIO WebhookTimeout

    Just (Left (err :: HTTP.HttpException)) -> do
      -- HTTP error
      withConn pool $ \conn ->
        updateWebhookDeliveryFailed conn (jTenantId job) deliveryId (show err)
      throwIO $ WebhookDeliveryFailed (show err)

    Just (Right response) -> do
      let status = HTTP.responseStatus response

      if HTTP.statusIsSuccessful status then do
        -- Success
        withConn pool $ \conn ->
          updateWebhookDeliverySuccess conn (jTenantId job) deliveryId UpdateDeliveryInput
            { udiHttpStatus = HTTP.statusCode status
            , udiResponseHeaders = HTTP.responseHeaders response
            , udiResponseBody = toStrict $ HTTP.responseBody response
            }
      else do
        -- Non-2xx status
        withConn pool $ \conn ->
          updateWebhookDeliveryFailed conn (jTenantId job) deliveryId $
            "HTTP " <> show (HTTP.statusCode status)
        throwIO $ WebhookNon2xx (HTTP.statusCode status)
```

**Test Cases**:
- TC-P8-01: Successful delivery → DELIVERED status
- TC-P8-02: HMAC signature verifiable by recipient
- TC-P8-03: HTTP error → retry with backoff
- TC-P8-04: Timeout → retry
- TC-P8-05: Non-2xx status → retry
- TC-P8-06: Max attempts → DEAD_LETTER
- TC-P8-07: Response stored for successful deliveries

**Acceptance Criteria**:
- [ ] HMAC-SHA256 signature in X-BPC-Signature header
- [ ] Signature format: `sha256=<hex>`
- [ ] Timeout set to 30 seconds
- [ ] Successful delivery → status DELIVERED
- [ ] Failed delivery → retry with exponential backoff
- [ ] Max 5 attempts before DEAD_LETTER
- [ ] Response details stored

---

### Phase 9: Integration Tests

**Goal**: End-to-end tests covering full pipeline and multi-worker scenarios.

**Files Created**:
- `packages/bpc-worker/test/integration/BPC/Worker/E2ESpec.hs`
- `packages/bpc-worker/test/integration/BPC/Worker/ConcurrencySpec.hs`

**Test Scenarios**:

```haskell
-- E2E happy path
testE2EHappyPath :: SpecWith TestEnv
testE2EHappyPath = it "processes full pipeline: upload → active" $ \env -> do
  -- 1. Upload document
  docVersionId <- uploadDocument env bomContent

  -- 2. PARSE_FACTS job should be auto-enqueued
  job1 <- waitForJobType env PARSE_FACTS
  job1Status <- getJobStatus env (jJobId job1)
  job1Status `shouldBe` SUCCEEDED

  -- 3. Create and seal snapshot
  snapshotId <- createSnapshot env
  addFactsToSnapshot env snapshotId
  sealSnapshot env snapshotId

  -- 4. Trigger compilation
  passportId <- createPassport env
  compileJobId <- triggerCompile env passportId snapshotId rulesId

  -- 5. Wait for COMPILE_PASSPORT
  waitForJobSuccess env compileJobId

  -- 6. SIGN_PASSPORT should auto-enqueue
  signJob <- waitForJobType env SIGN_PASSPORT
  waitForJobSuccess env (jJobId signJob)

  -- 7. GENERATE_QR should auto-enqueue
  qrJob <- waitForJobType env GENERATE_QR
  waitForJobSuccess env (jJobId qrJob)

  -- 8. Verify passport version is SIGNED with QR
  pv <- getPassportVersion env (pvId signJob)
  pvStatus pv `shouldBe` SIGNED
  pvQrPng pv `shouldSatisfy` isJust
  pvQrPayload pv `shouldSatisfy` isJust

-- Concurrency test
testConcurrency :: SpecWith TestEnv
testConcurrency = it "10 workers process 100 jobs without duplicates" $ \env -> do
  -- Enqueue 100 jobs
  jobIds <- replicateM 100 $ enqueueTestJob env

  -- Start 10 workers concurrently
  workers <- replicateM 10 $ async $ runWorkerOnce env

  -- Wait for all workers to finish
  mapM_ wait workers

  -- Verify all jobs completed exactly once
  jobStatuses <- mapM (getJobStatus env) jobIds
  all (== SUCCEEDED) jobStatuses `shouldBe` True

  -- Verify no duplicate processing
  executionCounts <- countExecutions env jobIds
  all (== 1) executionCounts `shouldBe` True

-- Retry test
testRetryWithBackoff :: SpecWith TestEnv
testRetryWithBackoff = it "retries failed jobs with exponential backoff" $ \env -> do
  -- Enqueue job that will fail 3 times then succeed
  jobId <- enqueueFlakyJob env (failTimes = 3)

  -- Run worker
  replicateM_ 4 $ runWorkerOnce env

  -- Verify retry attempts
  job <- getJob env jobId
  jAttempts job `shouldBe` 4
  jStatus job `shouldBe` SUCCEEDED

  -- Verify backoff delays
  executionTimes <- getExecutionTimes env jobId
  let delays = zipWith diffUTCTime (tail executionTimes) executionTimes

  -- First retry: ~2s, second: ~4s, third: ~8s
  delays !! 0 `shouldSatisfy` (\d -> d >= 2 && d <= 3)
  delays !! 1 `shouldSatisfy` (\d -> d >= 4 && d <= 5)
  delays !! 2 `shouldSatisfy` (\d -> d >= 8 && d <= 10)

-- Dead letter test
testDeadLetter :: SpecWith TestEnv
testDeadLetter = it "moves to DEAD_LETTER after 5 failed attempts" $ \env -> do
  -- Enqueue job that always fails
  jobId <- enqueueFailingJob env

  -- Run worker 5 times
  replicateM_ 5 $ runWorkerOnce env

  -- Verify DEAD_LETTER status
  job <- getJob env jobId
  jAttempts job `shouldBe` 5
  jStatus job `shouldBe` DEAD_LETTER
  jLastError job `shouldSatisfy` isJust
```

**Acceptance Criteria**:
- [ ] Full pipeline test passes (upload → active)
- [ ] 10 workers process 100 jobs without duplicates
- [ ] Retry backoff follows exponential pattern
- [ ] 5 failed attempts → DEAD_LETTER
- [ ] Lease expiry allows job re-acquisition
- [ ] Integration tests use docker-compose.test.yml

---

## Verification Checklist

### Worker Loop & Dispatch
- [ ] Worker polls at configured interval (BPC_JOBS_POLL_INTERVAL_MS)
- [ ] `FOR UPDATE SKIP LOCKED` prevents double-processing
- [ ] Lease renewal thread keeps job alive during processing
- [ ] Lease renewal cancels when job completes
- [ ] Failed jobs retry with exponential backoff (2^attempts seconds)
- [ ] Max 5 attempts before DEAD_LETTER (MUST requirement)
- [ ] Non-retryable errors → FAILED immediately
- [ ] Unsupported job types → error

### PARSE_FACTS
- [ ] BOM documents parsed correctly
- [ ] PCF documents parsed correctly
- [ ] Facts have canonical bytes and SHA-256 hash
- [ ] DocumentVersion status → VALIDATED
- [ ] Invalid documents → REJECTED with error
- [ ] Audit event emitted

### COMPILE_PASSPORT
- [ ] Only SEALED snapshots accepted
- [ ] Only PUBLISHED rules accepted
- [ ] Uses pure `compilePassportPure` function
- [ ] Output is deterministic (golden test)
- [ ] Payload/Proof/Receipt size limits enforced
- [ ] PassportVersion created with COMPILING status
- [ ] SIGN_PASSPORT job auto-enqueued
- [ ] Audit event emitted

### SIGN_PASSPORT
- [ ] ED25519 signature over receipt hash
- [ ] Signature stored with public key
- [ ] Status transitions COMPILING → SIGNED
- [ ] GENERATE_QR job auto-enqueued
- [ ] Signature verifiable
- [ ] Audit event emitted

### GENERATE_QR
- [ ] QR payload in BPC-QR-1 format
- [ ] PNG image generated and stored
- [ ] QR code scannable
- [ ] Base32 hashes have no padding
- [ ] Status remains SIGNED
- [ ] Audit event emitted

### RUN_RULE_TESTS
- [ ] Test suite parsed correctly
- [ ] All tests executed
- [ ] ≥500 passing cases required for VALIDATED
- [ ] Failure details stored
- [ ] Status updated on success
- [ ] Audit event emitted

### DELIVER_WEBHOOK
- [ ] HMAC-SHA256 signature in X-BPC-Signature header
- [ ] Timeout set to 30 seconds
- [ ] Successful delivery → DELIVERED
- [ ] Failed delivery → retry with backoff
- [ ] Max 5 attempts → DEAD_LETTER
- [ ] Response details stored

### Integration Tests
- [ ] Full pipeline test passes
- [ ] Multi-worker concurrency test passes (no duplicates)
- [ ] Retry backoff verified
- [ ] Dead letter handling verified
- [ ] Lease expiry allows re-acquisition

### Code Quality
- [ ] Test coverage ≥ 75% (constitution requirement)
- [ ] fourmolu formatting passes
- [ ] hlint passes with project config
- [ ] All integration tests pass with docker-compose.test.yml
- [ ] No bpc-api imports in bpc-worker (layered architecture)

---

## Future Job Handlers (SSOT Enum, Not Current Scope)

These job types are defined in the SSOT enum but handlers are not implemented in the current MVP scope.

### INGEST_DOCUMENT

**Purpose**: Automatic document ingestion with OCR/parsing for various file formats.

**Status**: Enum defined, handler TBD

**Prerequisites**:
- Document format standardization (PDF, XML, JSON schemas)
- OCR integration decision (Tesseract vs cloud service)
- File storage strategy (S3, local, DB blob)

**Potential Payload**:
```haskell
data IngestDocumentPayload = IngestDocumentPayload
  { idpDocumentId :: DocumentId
  , idpFileUrl    :: Text        -- S3 or local path
  , idpMimeType   :: Text        -- application/pdf, etc.
  }
```

**Expected Output**:
- DocumentVersion created with parsed content
- Auto-enqueue PARSE_FACTS if format recognized

---

### EXPORT_PASSPORT

**Purpose**: Export passport artifacts to external formats for distribution.

**Status**: Enum defined, handler TBD

**Prerequisites**:
- Format requirements from stakeholders (PDF, JSON-LD, XML)
- Template system for PDF generation
- JSON-LD context for Linked Data compliance

**Potential Payload**:
```haskell
data ExportPassportPayload = ExportPassportPayload
  { eppPassportVersionId :: PassportVersionId
  , eppFormat            :: ExportFormat  -- PDF | JSON_LD | XML
  , eppDestination       :: Maybe Text    -- Optional output path/URL
  }
```

**Expected Output**:
- Exported file stored (S3, DB blob, or returned)
- Export record in `passport_exports` table (future migration)

---

**Note**: These handlers will be implemented when business requirements are finalized. The enum values exist to ensure forward compatibility with the job queue schema.
