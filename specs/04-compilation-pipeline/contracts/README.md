# Compilation Pipeline Contracts

**Feature**: 04-compilation-pipeline
**Package**: bpc-core (pure library)
**Date**: 2025-12-28

## Why No HTTP/API Contracts

This feature implements the **pure compilation pipeline** in the `bpc-core` package. It is a library package with no HTTP endpoints or external API contracts.

**Key Points**:
- `bpc-core` is **IO-free** (constitutional principle V: Layered Architecture)
- No HTTP server (that's in `bpc-api` - feature 06)
- No database access (that's in `bpc-db` - feature 05)
- No job queue (that's in `bpc-worker` - feature 07)

## Internal Function Contracts

Instead of HTTP contracts, this package defines **function contracts** (type signatures):

### Main Entry Point

```haskell
-- | Pure compilation function
compilePassportPure :: CompileInput -> Either CompileError CompileOutput

-- Contract:
-- - MUST be deterministic (same input → same output, byte-identical)
-- - NO IO operations
-- - NO side effects
-- - Errors returned as Left, never thrown
```

### Proof Verification

```haskell
-- | Verify proof integrity
verifyProof :: ByteString -> Either ProofError ()

-- Contract:
-- - Accept canonical JSON proof bytes
-- - Verify all node hashes match BPC-PROOF-HASH-1
-- - Return () on success, ProofError on failure
-- - NO IO operations
```

### Receipt Signing

```haskell
-- | Sign receipt hash
signReceiptHash :: Ed25519PrivateKey -> Text -> Signature

-- Contract:
-- - Accept private key and receipt hash (hex)
-- - Return 64-byte ED25519 signature
-- - Deterministic (same key + hash → same signature)
-- - NO IO operations
```

### QR Payload

```haskell
-- | Build QR payload string
buildQrPayload :: QrInput -> Text

-- Contract:
-- - Format: BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
-- - Deterministic output
-- - Result fits in QR code Version 5 (~212 chars)
-- - NO IO operations
```

## Versioned Format Contracts

This feature defines and implements several versioned binary formats:

### BPC-PROOF-1 (Proof Schema)

**Contract**: Canonical JSON adhering to schema in SSOT 9.1

**Required Fields**:
- `proof_version`: "BPC-PROOF-1"
- `root_hash`: SHA-256 hex of root node
- `nodes`: Array of ProofNode objects
- `field_index`: Map from field_path to node_id

**Invariants**:
- All node hashes MUST be valid (BPC-PROOF-HASH-1)
- `root_hash` MUST match computed root
- `field_index` MUST reference valid node IDs

### BPC-RECEIPT-1 (Receipt Schema)

**Contract**: Canonical JSON adhering to schema in SSOT 9.3

**Required Fields**:
- `receipt_version`: "BPC-RECEIPT-1"
- `issued_at`: ISO 8601 timestamp
- `tenant_id`, `passport_version_id`, `passport_id`, `battery_product_id`: UUIDs
- `snapshot_id`, `snapshot_hash`: Snapshot reference
- `rule_package_version_id`, `dsl_sha256`, `tests_sha256`: Rule reference
- `compiler_build_id`: Build identifier
- `payload_hash`, `proof_root_hash`: Output hashes
- `signature_alg`: "ED25519"
- `signing_key_id`: Key identifier
- `signature`: Base64-encoded signature (64 bytes)

**Invariants**:
- Signature MUST verify with corresponding public key
- `receipt_hash = sha256(canonical(receipt without signature))`

### BPC-QR-1 (QR Payload Format)

**Contract**: Text string in specific format

**Format**: `BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>`

**Components**:
- `BPC1`: Format identifier
- `pv`: passport_version_id (UUID)
- `ph`: payload_hash (32 bytes, Base32 no padding)
- `pr`: proof_root_hash (32 bytes, Base32 no padding)
- `rh`: receipt_hash (32 bytes, Base32 no padding)

**Invariants**:
- Total length ≤ 220 characters
- All characters in QR alphanumeric set
- Base32 encoding uses RFC 4648 alphabet

### BPC-SIGN-1 (Signature Format)

**Contract**: ED25519 signature over receipt hash

**Algorithm**:
```
signature = ED25519.sign(privateKey, hexToBytes(receipt_hash))
```

**Properties**:
- Signature: 64 bytes
- Deterministic (same key + hash → same signature)
- Verifiable: `ED25519.verify(publicKey, hashBytes, signature)`

## Size Limits

Hard limits enforced by `compilePassportPure`:

| Artifact | Max Size | Error |
|----------|----------|-------|
| Payload | 131,072 bytes (128 KB) | PayloadTooLarge |
| Proof | 262,144 bytes (256 KB) | ProofTooLarge |
| Receipt | 16,384 bytes (16 KB) | ReceiptTooLarge |

## Testing Contracts

### Property Tests

```haskell
-- Determinism (THE critical property)
forall input. compilePassportPure input === compilePassportPure input

-- Proof verification roundtrip
forall input. case compilePassportPure input of
  Right output -> verifyProof (coProof output) === Right ()

-- Signature roundtrip
forall key hash. verifySignature (derivePublicKey key) hash (signReceiptHash key hash) === True

-- QR determinism
forall input. buildQrPayload input === buildQrPayload input
```

### Golden Tests

Byte-exact comparison against fixtures:
- `minimal.json`: Simplest valid compilation
- `medium.json`: Multiple fields with dependencies
- `edge.json`: Edge cases

## External Usage (from other packages)

### From bpc-worker (07-job-processing)

```haskell
-- In COMPILE_PASSPORT job handler
import BPC.Core.Compile (compilePassportPure)

handleCompileJob :: Job -> WorkerM ()
handleCompileJob job = do
  -- Load inputs from DB
  snapshot <- loadSealedSnapshot (jobSnapshotId job)
  rules <- loadPublishedRules (jobRulesId job)
  product <- loadBatteryProduct (jobProductId job)

  -- Call pure function
  let input = CompileInput { ... }
  case compilePassportPure input of
    Left err -> markJobFailed err
    Right output -> do
      -- Store outputs in DB
      storePassportVersion output
      markJobSucceeded
```

### From bpc-api (06-api-server)

```haskell
-- In /v1/passport-versions/:id/replay endpoint
import BPC.Core.Compile (compilePassportPure)
import BPC.Core.Receipt (verifySignature)

handleReplay :: PassportVersionId -> HandlerM ()
handleReplay pvId = do
  -- Load stored version
  stored <- loadPassportVersion pvId

  -- Replay compilation
  let replayOutput = compilePassportPure (buildInput stored)

  -- Verify hashes match
  unless (coPayloadHash replayOutput == storedPayloadHash stored) $
    throwError ReplayMismatch

  -- Verify signature
  unless (verifySignature pubKey receiptHash signature) $
    throwError SignatureInvalid

  pure ()
```

## See Also

- **HTTP Contracts**: Feature 06-api-server (`specs/06-api-server/contracts/`)
- **Job Contracts**: Feature 07-job-processing (`specs/07-job-processing/contracts/`)
- **Database Contracts**: Feature 05-data-layer (`specs/05-data-layer/data-model.md`)
