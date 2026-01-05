# Implementation Plan: Compilation Pipeline

**Branch**: `04-compilation-pipeline` | **Date**: 2025-12-28 | **Spec**: [.specify/features/04-compilation-pipeline/spec.md](../../../.specify/features/04-compilation-pipeline/spec.md)
**Input**: Feature specification from `.specify/features/04-compilation-pipeline/spec.md`
**Phase**: P1 | **Package**: bpc-core | **Status**: Planning

## Summary

Implement the pure compilation pipeline that transforms Snapshot + Rules into Passport artifacts (Payload, Proof, Receipt, QR). This is the heart of the system - the `compilePassportPure` function that MUST be deterministic (identical inputs produce byte-identical outputs). The compilation pipeline includes:

- **compilePassportPure**: Main orchestration function (pure, no IO)
- **Proof Builder**: Generates BPC-PROOF-1 derivation tree capturing all computation steps
- **Receipt Builder**: Generates BPC-RECEIPT-1 machine-verifiable receipt with all hashes
- **Signature**: ED25519 signing and verification for receipts
- **QR Payload**: Compact BPC-QR-1 format for physical battery labels
- **Size Enforcement**: Validates payload ≤131KB, proof ≤256KB, receipt ≤16KB

## Technical Context

**Language/Version**: Haskell GHC 9.6.4
**Package**: bpc-core (pure, IO-free)
**Primary Dependencies**:
  - aeson (JSON encoding/decoding)
  - bytestring (canonical bytes)
  - cryptonite (SHA-256 hashing)
  - ed25519 (signature operations)
  - containers (ordered Map for determinism)

**Testing**:
  - tasty-quickcheck (property tests for determinism)
  - tasty-golden (byte-exact regression tests)
  - tasty-hunit (unit tests)

**Performance Goals**:
  - Compile < 100ms for typical passport (20-50 fields)
  - Proof verification < 10ms

**Constraints**:
  - NO IO operations anywhere
  - Pure functions only
  - Deterministic output (byte-identical for same inputs)
  - No timestamps/random values generated
  - Ordered data structures only (no HashMap)

**Scale/Scope**:
  - ~1200 LOC across 5 modules
  - 3+ golden test fixtures
  - 1000+ property test samples

## Constitution Check

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. Determinism** | **CRITICAL** | Main purpose - byte-identical outputs. Property tests verify. |
| **II. Canonical Storage** | **CRITICAL** | All outputs use canonical bytes (BPC-CJSON-1). No pretty-printing. |
| **III. Immutability** | **N/A** | Pure functions - no mutation possible |
| **IV. Audit Trail** | **PREPARED** | Receipt enables replay verification (BPC-REPLAY-1) |
| **V. Layered Architecture** | **ENFORCED** | bpc-core is IO-free, cannot import DB/HTTP/Queue |
| **VI. Type-Safe Rules** | **USES** | Evaluates typed AST from rule engine (03-rule-engine) |

**GATES PASSED**: All constitutional principles satisfied. No violations to justify.

## Project Structure

### Documentation (this feature)

```text
specs/04-compilation-pipeline/
├── plan.md              # This file
├── research.md          # Design decisions & rationale
├── data-model.md        # Haskell types & schemas
├── quickstart.md        # Usage examples
└── contracts/           # N/A for pure library
    └── README.md        # Explains why no contracts
```

### Source Code (bpc-core package)

```text
packages/bpc-core/src/BPC/Core/
├── Eval.hs              # Expression evaluator with proof emission
├── Compile.hs           # compilePassportPure orchestration
├── Proof.hs             # Proof tree builder + verifier (BPC-PROOF-1)
├── Receipt.hs           # Receipt builder + hash + signature (BPC-RECEIPT-1, BPC-SIGN-1)
├── QR.hs                # QR payload string builder (BPC-QR-1)
└── Passport/
    └── Schema.hs        # BPC-PASSPORT-1 payload schema

packages/bpc-core/test/
├── Test/BPC/Core/
│   ├── CompileSpec.hs   # Property tests (determinism!)
│   ├── ProofSpec.hs     # Proof verification tests
│   ├── ReceiptSpec.hs   # Signature roundtrip tests
│   └── QRSpec.hs        # QR format tests
└── golden/              # Golden test fixtures
    ├── minimal.json     # Simplest valid compilation
    ├── medium.json      # Multiple fields with dependencies
    └── edge.json        # Edge cases (empty fields, max values)
```

**Structure Decision**: Single package (bpc-core) as pure library. No web/mobile structure needed.

## Implementation Phases

### Phase 0: Prerequisites (Dependencies on 02 & 03)

**Required**: Features 02-core-primitives and 03-rule-engine must be complete.

**Verify**:
- [x] Canonical JSON encoding available (BPC.Core.Canonical)
- [x] SHA-256 hashing available (BPC.Core.Hash)
- [x] Typed expression AST available (BPC.Rules.AST)
- [x] Expression evaluator available (BPC.Rules.Eval)
- [x] Topological sort for fields available (BPC.Rules.Graph)

### Phase 1: Compile Orchestration (BPC-COMPILE-1)

**Goal**: Implement main `compilePassportPure` function that orchestrates the entire pipeline.

**Module**: `BPC.Core.Compile`

**Type Signatures**:
```haskell
module BPC.Core.Compile where

-- | Main compilation function (MUST be pure)
compilePassportPure :: CompileInput -> Either CompileError CompileOutput

-- | Complete input for compilation
data CompileInput = CompileInput
  { ciSnapshot    :: SealedSnapshot    -- SEALED snapshot with facts
  , ciRules       :: TypedRulePackage  -- Typechecked rules (PUBLISHED)
  , ciProduct     :: BatteryProduct    -- Battery product metadata
  , ciIssuedAt    :: UTCTime           -- Timestamp (passed in, not generated)
  , ciBuildId     :: Text              -- Compiler build ID
  }

-- | Complete compilation output (all canonical bytes)
data CompileOutput = CompileOutput
  { coPayload         :: ByteString  -- Canonical JSON payload
  , coPayloadHash     :: Text        -- SHA-256 hex of payload
  , coProof           :: ByteString  -- Canonical JSON proof
  , coProofRootHash   :: Text        -- SHA-256 hex of proof root
  , coReceiptUnsigned :: ByteString  -- Canonical JSON receipt (no signature)
  , coReceiptHash     :: Text        -- SHA-256 hex of receipt
  }

-- | Compilation errors
data CompileError
  = SnapshotNotSealed
  | RulesNotPublished
  | CycleDetected [FieldPath]
  | EvalError EvalError
  | PayloadTooLarge Int
  | ProofTooLarge Int
  | ReceiptTooLarge Int
  deriving (Eq, Show)
```

**Algorithm**:
```haskell
compilePassportPure :: CompileInput -> Either CompileError CompileOutput
compilePassportPure input = do
  -- 1. Verify inputs
  unless (isSealed $ ciSnapshot input) $ Left SnapshotNotSealed
  unless (isPublished $ ciRules input) $ Left RulesNotPublished

  -- 2. Topologically sort fields (BPC-GRAPH-1)
  sortedFields <- first CycleDetected $ topoSortFields (ruleFields $ ciRules input)

  -- 3. Evaluate fields in order, building proof tree
  (values, proofNodes) <- evalFieldsWithProof (ciSnapshot input) sortedFields

  -- 4. Build payload from evaluated values
  payload <- buildPayload (ciProduct input) values
  payloadCanonical <- canonicalEncode payload
  checkSize "payload" maxPayloadSize payloadCanonical PayloadTooLarge

  -- 5. Build proof tree from nodes
  proof <- buildProof proofNodes
  proofCanonical <- canonicalEncode proof
  checkSize "proof" maxProofSize proofCanonical ProofTooLarge

  -- 6. Build receipt (unsigned)
  receipt <- buildReceiptUnsigned ReceiptInput
    { riIssuedAt = ciIssuedAt input
    , riTenantId = getTenantId input
    , riPassportVersionId = genPassportVersionId input
    , riPayloadHash = sha256Hex payloadCanonical
    , riProofRootHash = computeRootHash proofNodes
    , ...
    }
  receiptCanonical <- canonicalEncode receipt
  checkSize "receipt" maxReceiptSize receiptCanonical ReceiptTooLarge

  pure CompileOutput
    { coPayload = payloadCanonical
    , coPayloadHash = sha256Hex payloadCanonical
    , coProof = proofCanonical
    , coProofRootHash = computeRootHash proofNodes
    , coReceiptUnsigned = receiptCanonical
    , coReceiptHash = sha256Hex receiptCanonical
    }
```

**Tests**:
- Unit: Happy path with minimal input
- Property: `prop_compile_deterministic` (THE critical test)
- Golden: Byte-exact comparison for minimal.json

**Acceptance**: User Story 1 scenarios pass

### Phase 2: Proof Builder (BPC-PROOF-1)

**Goal**: Build derivation tree that captures every computation step for audit replay.

**Module**: `BPC.Core.Proof`

**Type Signatures**:
```haskell
module BPC.Core.Proof where

-- | Proof tree structure
data Proof = Proof
  { proofVersion :: Text           -- "BPC-PROOF-1"
  , rootHash     :: Text           -- SHA-256 hex
  , nodes        :: [ProofNode]    -- All nodes
  , fieldIndex   :: Map Text Int   -- field_path -> node_id
  }

-- | Single proof node
data ProofNode = ProofNode
  { pnId       :: Int       -- Unique node ID
  , pnType     :: NodeType  -- Node type
  , pnHash     :: Text      -- SHA-256 of canonical({id,type,data,children_hashes})
  , pnData     :: Value     -- Node-specific data (JSON)
  , pnChildren :: [Int]     -- Child node IDs
  }

-- | Node types (SSOT 9.1)
data NodeType
  = CONST           -- Constant value
  | FACT_GET        -- Fact lookup (getFact)
  | FIELD_REF       -- Field reference
  | OP              -- Operation (+, -, *, /, etc.)
  | ASSERT          -- Assertion/RequireSome
  | COMPLIANCE_EMIT -- emitCompliance call
  deriving (Eq, Show)

-- | Build proof from evaluation trace
buildProof :: [ProofNode] -> Proof

-- | Compute node hash (BPC-PROOF-HASH-1)
computeNodeHash :: ProofNode -> [ProofNode] -> Text
computeNodeHash node allNodes = sha256Hex $ canonicalEncode $ object
  [ "id" .= pnId node
  , "type" .= show (pnType node)
  , "data" .= pnData node
  , "children" .= [pnHash (allNodes !! i) | i <- pnChildren node]
  ]

-- | Verify proof integrity (all hashes match)
verifyProof :: ByteString -> Either ProofError ()
```

**Algorithm**:
```haskell
buildProof :: [ProofNode] -> Proof
buildProof nodes = Proof
  { proofVersion = "BPC-PROOF-1"
  , rootHash = computeRootHash nodes
  , nodes = nodesWithHashes
  , fieldIndex = buildFieldIndex nodes
  }
  where
    -- Compute hashes bottom-up
    nodesWithHashes = map (\n -> n { pnHash = computeNodeHash n nodes }) nodes

verifyProof :: ByteString -> Either ProofError ()
verifyProof proofBytes = do
  proof <- eitherDecode proofBytes
  unless (proofVersion proof == "BPC-PROOF-1") $ Left InvalidVersion
  forM_ (nodes proof) $ \node -> do
    let expectedHash = computeNodeHash node (nodes proof)
    unless (pnHash node == expectedHash) $ Left (HashMismatch (pnId node))
  pure ()
```

**Tests**:
- Unit: Build proof from simple node list
- Property: `prop_proof_verify` (all generated proofs verify)
- Property: `prop_proof_hash_deterministic` (node hash is deterministic)
- Golden: Proof structure for medium.json

**Acceptance**: User Story 2 scenarios pass

### Phase 3: Receipt Builder (BPC-RECEIPT-1)

**Goal**: Build machine-verifiable receipt containing all hashes and metadata for replay.

**Module**: `BPC.Core.Receipt`

**Type Signatures**:
```haskell
module BPC.Core.Receipt where

-- | Receipt input (everything needed to build receipt)
data ReceiptInput = ReceiptInput
  { riIssuedAt             :: UTCTime
  , riTenantId             :: TenantId
  , riPassportVersionId    :: PassportVersionId
  , riPassportId           :: PassportId
  , riBatteryProductId     :: BatteryProductId
  , riSnapshotId           :: SnapshotId
  , riSnapshotHash         :: Text
  , riRulePackageVersionId :: RulePackageVersionId
  , riDslSha256            :: Text
  , riTestsSha256          :: Text
  , riCompilerBuildId      :: Text
  , riPayloadHash          :: Text
  , riProofRootHash        :: Text
  , riSigningKeyId         :: Text
  }

-- | Unsigned receipt (ready for signing)
data ReceiptUnsigned = ReceiptUnsigned
  { ruVersion             :: Text  -- "BPC-RECEIPT-1"
  , ruIssuedAt            :: UTCTime
  , ruTenantId            :: TenantId
  , ruPassportVersionId   :: PassportVersionId
  , ruPassportId          :: PassportId
  , ruBatteryProductId    :: BatteryProductId
  , ruSnapshotId          :: SnapshotId
  , ruSnapshotHash        :: Text
  , ruRulePackageVersionId :: RulePackageVersionId
  , ruDslSha256           :: Text
  , ruTestsSha256         :: Text
  , ruCompilerBuildId     :: Text
  , ruPayloadHash         :: Text
  , ruProofRootHash       :: Text
  , ruSignatureAlg        :: Text  -- "ED25519"
  , ruSigningKeyId        :: Text
  }

-- | Build unsigned receipt
buildReceiptUnsigned :: ReceiptInput -> ReceiptUnsigned

-- | Hash unsigned receipt (BPC-SIGN-1)
hashReceiptUnsigned :: ReceiptUnsigned -> Text
hashReceiptUnsigned = sha256Hex . canonicalEncode
```

**Algorithm**:
```haskell
buildReceiptUnsigned :: ReceiptInput -> ReceiptUnsigned
buildReceiptUnsigned input = ReceiptUnsigned
  { ruVersion = "BPC-RECEIPT-1"
  , ruIssuedAt = riIssuedAt input
  , ruTenantId = riTenantId input
  , ruPassportVersionId = riPassportVersionId input
  , ruPassportId = riPassportId input
  , ruBatteryProductId = riBatteryProductId input
  , ruSnapshotId = riSnapshotId input
  , ruSnapshotHash = riSnapshotHash input
  , ruRulePackageVersionId = riRulePackageVersionId input
  , ruDslSha256 = riDslSha256 input
  , ruTestsSha256 = riTestsSha256 input
  , ruCompilerBuildId = riCompilerBuildId input
  , ruPayloadHash = riPayloadHash input
  , ruProofRootHash = riProofRootHash input
  , ruSignatureAlg = "ED25519"
  , ruSigningKeyId = riSigningKeyId input
  }
```

**Tests**:
- Unit: Build receipt from input
- Property: `prop_receipt_hash_deterministic` (hash is deterministic)
- Golden: Receipt for minimal.json

**Acceptance**: User Story 3 scenarios pass

### Phase 4: Signature (BPC-SIGN-1)

**Goal**: Sign receipt hash with ED25519 and verify signatures.

**Module**: `BPC.Core.Receipt` (extended)

**Type Signatures**:
```haskell
-- | ED25519 private key
newtype Ed25519PrivateKey = Ed25519PrivateKey ByteString

-- | ED25519 public key
newtype Ed25519PublicKey = Ed25519PublicKey ByteString

-- | Signature
newtype Signature = Signature ByteString

-- | Sign receipt hash
signReceiptHash :: Ed25519PrivateKey -> Text -> Signature

-- | Verify signature
verifySignature :: Ed25519PublicKey -> Text -> Signature -> Bool

-- | Derive public key from private key
derivePublicKey :: Ed25519PrivateKey -> Ed25519PublicKey
```

**Algorithm**:
```haskell
signReceiptHash :: Ed25519PrivateKey -> Text -> Signature
signReceiptHash (Ed25519PrivateKey privKey) hashHex =
  let hashBytes = hexToBytes hashHex
      sig = ED25519.sign privKey hashBytes
  in Signature sig

verifySignature :: Ed25519PublicKey -> Text -> Signature -> Bool
verifySignature (Ed25519PublicKey pubKey) hashHex (Signature sig) =
  let hashBytes = hexToBytes hashHex
  in ED25519.verify pubKey hashBytes sig
```

**Tests**:
- Unit: Sign and verify roundtrip
- Property: `prop_signature_roundtrip` (sign → verify always succeeds)
- Unit: Verify with wrong key fails
- Unit: Verify with wrong hash fails

**Acceptance**: User Story 4 scenarios pass

### Phase 5: QR Payload (BPC-QR-1)

**Goal**: Generate compact QR payload string for physical battery labels.

**Module**: `BPC.Core.QR`

**Type Signatures**:
```haskell
module BPC.Core.QR where

-- | QR payload input
data QrInput = QrInput
  { qiPassportVersionId :: PassportVersionId
  , qiPayloadHash       :: Text
  , qiProofRootHash     :: Text
  , qiReceiptHash       :: Text
  }

-- | Build QR payload (BPC-QR-1 format)
buildQrPayload :: QrInput -> Text
```

**Algorithm**:
```haskell
-- Format: BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
buildQrPayload :: QrInput -> Text
buildQrPayload input =
  "BPC1"
  <> "|pv=" <> uuidToText (qiPassportVersionId input)
  <> "|ph=" <> base32NoPad (hexToBytes $ qiPayloadHash input)
  <> "|pr=" <> base32NoPad (hexToBytes $ qiProofRootHash input)
  <> "|rh=" <> base32NoPad (hexToBytes $ qiReceiptHash input)

-- Base32 encoding (RFC 4648, no padding)
base32NoPad :: ByteString -> Text
base32NoPad = T.dropWhileEnd (== '=') . encodeBase32
```

**Format Analysis**:
```
BPC1|pv=550e8400-e29b-41d4-a716-446655440000|ph=MFZWIZLSN5UGKZLDMFZWIZLSN5UGKZLDMFZWIZLSN5UGKZLD|pr=NJSWC3DPN5XGKZLSNFXGGK3KNJSWC3DPN5XGKZLSNFXGGK3K|rh=GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ
Total: ~208 characters (fits in QR alphanumeric mode)
```

**Tests**:
- Unit: Format matches BPC-QR-1
- Property: `prop_qr_deterministic` (same input → same output)
- Unit: Base32 decode roundtrip
- Golden: QR payload for minimal.json

**Acceptance**: User Story 5 scenarios pass

### Phase 6: Size Limit Enforcement

**Goal**: Enforce hard limits on artifact sizes to prevent unbounded growth.

**Module**: `BPC.Core.Compile` (extended)

**Type Signatures**:
```haskell
-- | Maximum artifact sizes (SSOT 7.6)
maxPayloadSize :: Int
maxPayloadSize = 131072  -- 128 KB

maxProofSize :: Int
maxProofSize = 262144  -- 256 KB

maxReceiptSize :: Int
maxReceiptSize = 16384  -- 16 KB

-- | Check size limit
checkSize :: Text -> Int -> ByteString -> (Int -> CompileError) -> Either CompileError ()
checkSize name limit bs mkError
  | BS.length bs > limit = Left $ mkError (BS.length bs)
  | otherwise = Right ()
```

**Algorithm**:
```haskell
-- In compilePassportPure:
checkSize "payload" maxPayloadSize payloadCanonical PayloadTooLarge
checkSize "proof" maxProofSize proofCanonical ProofTooLarge
checkSize "receipt" maxReceiptSize receiptCanonical ReceiptTooLarge
```

**Tests**:
- Unit: Payload exceeds limit → PayloadTooLarge
- Unit: Proof exceeds limit → ProofTooLarge
- Unit: Receipt exceeds limit → ReceiptTooLarge
- Unit: All under limits → success

**Acceptance**: User Story 6 scenarios pass

### Phase 7: Golden Tests (Determinism Verification)

**Goal**: Byte-exact regression tests to ensure determinism.

**Fixtures**: `packages/bpc-core/test/golden/`

**Fixture Structure**:
```json
{
  "name": "minimal",
  "input": {
    "snapshot": { ... },
    "rules": { ... },
    "product": { ... },
    "issued_at": "2025-01-01T00:00:00Z",
    "build_id": "build-123"
  },
  "expected": {
    "payload_canonical": "<hex>",
    "payload_hash": "43258cff...",
    "proof_canonical": "<hex>",
    "proof_root_hash": "92a3d4e5...",
    "receipt_canonical": "<hex>",
    "receipt_hash": "7f3a2b1c..."
  }
}
```

**Golden Tests**:
1. **minimal.json**: Simplest valid compilation (1 field, 1 fact)
2. **medium.json**: Multiple fields with dependencies (5-10 fields)
3. **edge.json**: Edge cases (empty optional fields, max values, unit conversions)

**Test Code**:
```haskell
goldenTest :: FilePath -> TestTree
goldenTest fixturePath = testCase (takeBaseName fixturePath) $ do
  fixture <- loadFixture fixturePath
  let result = compilePassportPure (fixtureInput fixture)
  case result of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right output -> do
      assertEqual "payload hash" (fixturePayloadHash fixture) (coPayloadHash output)
      assertEqual "payload bytes" (fixturePayloadCanonical fixture) (coPayload output)
      assertEqual "proof root hash" (fixtureProofRootHash fixture) (coProofRootHash output)
      assertEqual "proof bytes" (fixtureProofCanonical fixture) (coProof output)
      assertEqual "receipt hash" (fixtureReceiptHash fixture) (coReceiptHash output)
      assertEqual "receipt bytes" (fixtureReceiptCanonical fixture) (coReceiptUnsigned output)
```

**Acceptance**: User Story 7 scenarios pass

## Size Limits (SSOT 7.6)

| Artifact | Max Bytes | Hex | Error Code |
|----------|-----------|-----|------------|
| Payload | 131,072 | 0x20000 | PAYLOAD_TOO_LARGE |
| Proof | 262,144 | 0x40000 | PROOF_TOO_LARGE |
| Receipt | 16,384 | 0x4000 | RECEIPT_TOO_LARGE |

**Rationale**:
- Payload: Typical passport ~10-20KB; 128KB allows plenty of headroom
- Proof: Complex derivation trees; 256KB allows deep nesting
- Receipt: Minimal metadata only; 16KB is generous

## Property Tests Required

```haskell
-- THE determinism test (NON-NEGOTIABLE)
prop_compile_deterministic :: CompileInput -> Property
prop_compile_deterministic input =
  compilePassportPure input === compilePassportPure input

-- Proof verification roundtrip
prop_proof_verify :: CompileInput -> Property
prop_proof_verify input = case compilePassportPure input of
  Left _ -> discard
  Right output -> verifyProof (coProof output) === Right ()

-- Signature roundtrip
prop_signature_roundtrip :: Ed25519PrivateKey -> Text -> Property
prop_signature_roundtrip privKey hash =
  let pubKey = derivePublicKey privKey
      sig = signReceiptHash privKey hash
  in verifySignature pubKey hash sig === True

-- QR payload deterministic
prop_qr_deterministic :: QrInput -> Property
prop_qr_deterministic input =
  buildQrPayload input === buildQrPayload input

-- Canonical encoding deterministic
prop_canonical_deterministic :: Value -> Property
prop_canonical_deterministic v =
  canonicalEncode v === canonicalEncode v
```

## Verification Checklist

Phase 1:
- [ ] `compilePassportPure` is pure (no IO in type signature)
- [ ] Same input produces byte-identical output (property test)
- [ ] Topological sort handles cycles correctly
- [ ] Evaluation errors propagate correctly

Phase 2:
- [ ] Proof nodes have correct hashes (BPC-PROOF-HASH-1)
- [ ] `verifyProof` accepts all generated proofs
- [ ] Field index maps field paths to node IDs
- [ ] Proof format matches BPC-PROOF-1 exactly

Phase 3:
- [ ] Receipt contains all required fields (BPC-RECEIPT-1)
- [ ] Receipt hash is deterministic
- [ ] Unsigned receipt omits signature field

Phase 4:
- [ ] Signature roundtrip (sign → verify) always succeeds
- [ ] Wrong key/hash verification fails
- [ ] ED25519 algorithm matches SSOT 7.7

Phase 5:
- [ ] QR payload matches BPC-QR-1 format exactly
- [ ] Base32 encoding is correct (RFC 4648, no padding)
- [ ] QR payload fits in ~208 characters

Phase 6:
- [ ] Size limits are enforced correctly
- [ ] Error messages include actual size

Phase 7:
- [ ] Golden tests pass (byte-exact)
- [ ] All 3+ fixtures are byte-identical on re-run

Overall:
- [ ] NO IO imports in bpc-core
- [ ] Code coverage >= 90%
- [ ] Property tests pass with 1000 samples
- [ ] All acceptance scenarios pass
- [ ] Documentation is complete

## Complexity Tracking

**No violations** - All constitution principles are satisfied without exceptions.

## Dependencies

**Requires Complete**:
- 02-core-primitives (canonical JSON, SHA-256 hashing)
- 03-rule-engine (typed AST, expression evaluator, topological sort)

**Enables**:
- 05-data-layer (stores compiled artifacts in DB)
- 07-job-processing (COMPILE_PASSPORT job uses this)
- 08-advanced-features (replay verification)

## Risk Analysis

| Risk | Mitigation |
|------|------------|
| Non-determinism due to HashMap iteration | Use ordered Map everywhere |
| Non-determinism due to floating point | Forbid floats; use Dec/Qty only |
| Non-determinism due to timestamps | Pass timestamp as input, never generate |
| Proof verification too slow | Cache node hashes; verify incrementally |
| Size limits too restrictive | Monitor real-world usage; adjust if needed |
| ED25519 library incompatibility | Pin exact version; test roundtrips |

## Success Criteria (from spec.md)

- **SC-001**: Property test `prop_compile_deterministic` passes with 1,000 samples
- **SC-002**: All 3+ golden fixtures are byte-exact
- **SC-003**: `verifyProof` accepts all generated proofs
- **SC-004**: Signature roundtrip (sign → verify) always succeeds
- **SC-005**: Code coverage for BPC.Core.Eval, Proof, Receipt >= 90%
- **SC-006**: No IO imports in bpc-core (enforced by GHC)
