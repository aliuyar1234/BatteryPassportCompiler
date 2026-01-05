# Research: Compilation Pipeline

**Feature**: 04-compilation-pipeline
**Date**: 2025-12-28
**Status**: Complete
**SSOT Reference**: Sections 7.6 (Compile), 9.1-9.3 (Proof/Receipt)

## Research Summary

The compilation pipeline is the core deterministic transformation that converts Facts + Rules into a signed Battery Passport. All design decisions are constrained by the **NON-NEGOTIABLE determinism requirement**: identical inputs MUST produce byte-identical outputs. This research document captures the design rationale for key decisions.

---

## 1. Determinism Strategy

### Decision: Eliminate ALL sources of non-determinism

**Problem**: Typical compilers have many sources of non-determinism:
- Timestamp generation
- Random number generation
- HashMap iteration order
- Floating-point arithmetic
- Thread scheduling
- File system ordering

**Solution**: Comprehensive elimination strategy

| Source | Mitigation |
|--------|------------|
| Timestamps | Passed in as `CompileInput.ciIssuedAt` (never generated) |
| Random values | None used anywhere |
| HashMap iteration | Use `Data.Map` (ordered by keys) everywhere |
| Floating point | **Forbidden** - only `Dec` (fixed-point) and `Qty` allowed |
| Thread variation | Pure function - no threads, no IO |
| File system | Pure function - no file access |

**Verification Strategy**:
```haskell
-- Property test with 1000+ samples
prop_compile_deterministic :: CompileInput -> Property
prop_compile_deterministic input =
  compilePassportPure input === compilePassportPure input
```

**Why This Matters**:
- Audit replay REQUIRES byte-identical recomputation
- Hashes in receipt must match exactly for verification
- Proof tree must be reproducible for forensic analysis

**Alternative Rejected**: "Close enough" hashing (e.g., ignoring timestamp fields) - rejected because it violates SSOT requirement that receipt contains `issued_at`.

---

## 2. Proof Tree Design

### Decision: DAG with per-node hashes, not Merkle tree

**Problem**: How to structure the derivation tree to capture all computation steps?

**Options Considered**:

| Option | Pros | Cons | Decision |
|--------|------|------|----------|
| **Merkle tree** | Well-known structure, efficient verification | Requires deduplication; complex for DAG | **Rejected** |
| **DAG with node hashes** | Simple; matches SSOT 9.1; allows shared subexpressions | Slightly larger | **Selected** |
| **Flat trace log** | Simplest to implement | No structural information; large | **Rejected** |

**Selected Approach**: DAG with per-node hashes

**Rationale**:
1. **SSOT Compliance**: BPC-PROOF-1 schema explicitly defines node-based structure
2. **Simplicity**: No need for deduplication logic
3. **Verifiability**: Each node hash depends on children hashes (bottom-up verification)
4. **Flexibility**: Allows multiple fields to reference same fact (shared FACT_GET node)

**Hash Computation** (BPC-PROOF-HASH-1):
```haskell
-- Node hash includes its own data AND children hashes
computeNodeHash :: ProofNode -> [ProofNode] -> Text
computeNodeHash node allNodes = sha256Hex $ canonicalEncode $ object
  [ "id" .= pnId node
  , "type" .= show (pnType node)
  , "data" .= pnData node
  , "children" .= [pnHash (allNodes !! i) | i <- pnChildren node]
  ]
```

**Why Bottom-Up**:
- Leaf nodes (CONST, FACT_GET) have no children → hash depends only on data
- Parent nodes hash includes children hashes → changing any child invalidates parent
- Root hash summarizes entire tree → single value for quick verification

**Example Proof Tree**:
```
Field: battery.capacity_kwh = requireSome(getFact("Battery", "bat:123"), "E001", "missing")

Nodes:
0: CONST("Battery")              hash=abc...
1: CONST("bat:123")              hash=def...
2: FACT_GET(data={...})          hash=ghi... children=[0,1]
3: CONST("E001")                 hash=jkl...
4: CONST("missing")              hash=mno...
5: ASSERT(requireSome)           hash=pqr... children=[2,3,4]
6: FIELD_REF("battery.capacity_kwh") hash=stu... children=[5]

root_hash = stu...
field_index = {"battery.capacity_kwh": 6}
```

---

## 3. Evaluation with Proof Emission

### Decision: Interleave evaluation with proof node creation (Writer monad)

**Problem**: How to capture evaluation trace without polluting core evaluator?

**Options Considered**:

| Option | Pros | Cons | Decision |
|--------|------|------|----------|
| **Writer monad** | Clean separation; composable | Slight overhead | **Selected** |
| **Manual list passing** | Explicit control | Verbose; error-prone | **Rejected** |
| **Separate proof builder pass** | Evaluator stays pure | Duplicate logic; fragile | **Rejected** |

**Selected Approach**: Writer monad with proof node emission

**Pattern**:
```haskell
type EvalM a = WriterT [ProofNode] (Either EvalError) a

-- Example: evaluate constant
evalWithProof :: Expr t -> EvalContext -> EvalM (Value t)
evalWithProof (ELitInt n) _ = do
  nodeId <- freshNodeId
  tell [ProofNode
    { pnId = nodeId
    , pnType = CONST
    , pnHash = ""  -- Computed later
    , pnData = toJSON n
    , pnChildren = []
    }]
  pure (VInt n)

-- Example: evaluate function application
evalWithProof (EApp BGetFact [factType, factKey]) ctx = do
  -- Evaluate arguments (emits their nodes)
  factTypeVal <- evalWithProof factType ctx
  factKeyVal <- evalWithProof factKey ctx

  -- Lookup fact
  result <- lift $ lookupFact factTypeVal factKeyVal

  -- Emit FACT_GET node
  nodeId <- freshNodeId
  tell [ProofNode
    { pnId = nodeId
    , pnType = FACT_GET
    , pnHash = ""
    , pnData = toJSON result
    , pnChildren = [getNodeId factTypeVal, getNodeId factKeyVal]
    }]

  pure result
```

**Rationale**:
- Writer monad automatically collects nodes during evaluation
- Each expression evaluation emits exactly the nodes needed
- Children IDs are captured naturally from argument evaluation
- Order is deterministic (topological sort of fields)

**Hash Computation Timing**:
- During evaluation: nodes have `pnHash = ""` (placeholder)
- After evaluation: compute hashes bottom-up using `computeNodeHash`
- Why deferred: children hashes must exist before parent hash can be computed

---

## 4. Receipt Schema

### Decision: Follow BPC-RECEIPT-1 exactly (SSOT 9.3)

**Problem**: What fields are needed for replay verification?

**SSOT Mandate**: All fields in BPC-RECEIPT-1 are **required**.

**Field Categories**:

| Category | Fields | Purpose |
|----------|--------|---------|
| **Versioning** | `receipt_version` | Format version ("BPC-RECEIPT-1") |
| **Identity** | `tenant_id`, `passport_version_id`, `passport_id`, `battery_product_id` | Uniquely identifies this passport |
| **Inputs** | `snapshot_id`, `snapshot_hash`, `rule_package_version_id`, `dsl_sha256`, `tests_sha256` | Identifies input artifacts |
| **Compilation** | `issued_at`, `compiler_build_id` | When and how compiled |
| **Outputs** | `payload_hash`, `proof_root_hash` | Hashes of generated artifacts |
| **Signature** | `signature_alg`, `signing_key_id`, `signature` | Cryptographic proof |

**Unsigned Receipt**:
- For hashing purposes, receipt is first built WITHOUT `signature` field
- `receipt_hash = sha256(canonical(receipt_unsigned))`
- Then signature is added: `signature = ED25519.sign(privKey, receipt_hash)`

**Why Unsigned First**:
- Signature depends on receipt hash
- Receipt hash depends on receipt bytes
- Circular dependency if signature field included
- Solution: hash unsigned receipt, then add signature

**Replay Verification** (SSOT 9.6):
```text
replay(passport_version_id):
  1. Load passport_version from DB
  2. Load snapshot, rules, product (from receipt fields)
  3. Recompile: compilePassportPure(snapshot, rules, product, issued_at, build_id)
  4. Assert: recompiled hashes == stored hashes
  5. Verify signature: ED25519.verify(pubKey, receipt_hash, signature)
```

**Why Every Field Matters**:
- `snapshot_hash`: Ensures facts haven't changed
- `dsl_sha256`, `tests_sha256`: Ensures rules haven't changed
- `compiler_build_id`: Detects compiler bugs
- `issued_at`: Included in payload (battery manufactured date)

---

## 5. ED25519 Signing

### Decision: Use `ed25519` Haskell package

**Problem**: Which signature algorithm and library?

**Algorithm Selection**: ED25519 (SSOT 7.7 mandate)

**Why ED25519** (vs RSA, ECDSA):
- Fast: Sign/verify in microseconds
- Small: 64-byte signatures, 32-byte keys
- Simple: No parameter choices (unlike RSA key size, ECDSA curve)
- Secure: 128-bit security level
- Deterministic: No random nonce needed (unlike ECDSA)

**Library Selection**: `ed25519` package

**Options Considered**:

| Library | Pros | Cons | Decision |
|---------|------|------|----------|
| **ed25519** | Pure Haskell; no FFI; well-tested | Slightly slower than C | **Selected** |
| **cryptonite** | Full crypto suite | Heavier dependency | **Backup** |
| **libsodium-bindings** | Fastest (C library) | FFI dependency; build complexity | **Rejected** |

**Key Management**:
```haskell
-- Private key: 32 bytes (base64 in ENV)
newtype Ed25519PrivateKey = Ed25519PrivateKey ByteString

-- Public key: 32 bytes (derived from private)
newtype Ed25519PublicKey = Ed25519PublicKey ByteString

-- Derive public from private
derivePublicKey :: Ed25519PrivateKey -> Ed25519PublicKey

-- Signature: 64 bytes
newtype Signature = Signature ByteString
```

**Signing Process**:
```haskell
-- 1. Hash unsigned receipt
receiptHash = sha256Hex (canonicalEncode receiptUnsigned)

-- 2. Convert hash to bytes
hashBytes = hexToBytes receiptHash  -- 32 bytes

-- 3. Sign
signature = ED25519.sign privateKey hashBytes  -- 64 bytes

-- 4. Base64 encode signature for storage
signatureBase64 = encodeBase64 signature
```

**Verification Process**:
```haskell
-- 1. Load signature and receipt hash
signature = decodeBase64 signatureBase64
receiptHash = loadFromDB()
hashBytes = hexToBytes receiptHash

-- 2. Verify
isValid = ED25519.verify publicKey hashBytes signature
```

**Key Rotation**:
- Receipt includes `signing_key_id` field
- Allows multiple keys to be active
- Old signatures remain valid (key ID lookup)

---

## 6. QR Payload Compression

### Decision: Base32 encoding for hashes (32 bytes → 52 chars)

**Problem**: Fit passport reference into scannable QR code.

**Constraints**:
- QR code alphanumeric mode (0-9, A-Z, space, $%*+-./:) is most efficient
- Standard Base64 uses `+/=` which forces QR binary mode (less efficient)
- Need to encode: passport_version_id (UUID) + 3 hashes (32 bytes each)

**Options Considered**:

| Encoding | Hash Size | Total Size | QR Mode | Decision |
|----------|-----------|------------|---------|----------|
| **Hex** | 64 chars | ~260 chars | Alphanumeric | Too long |
| **Base64** | 43 chars | ~180 chars | **Binary** (due to +/=) | Wrong mode |
| **Base32** | 52 chars | ~208 chars | **Alphanumeric** | **Selected** |

**Selected Approach**: Base32 (RFC 4648, no padding)

**Format** (BPC-QR-1):
```
BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
```

**Size Analysis**:
```
"BPC1"                              =   4 chars
"|pv=" + UUID                       =  40 chars (36 + 4)
"|ph=" + base32(32 bytes)           =  56 chars (52 + 4)
"|pr=" + base32(32 bytes)           =  56 chars (52 + 4)
"|rh=" + base32(32 bytes)           =  56 chars (52 + 4)
                                    ----------------
Total                               = 212 chars
```

**QR Code Capacity** (alphanumeric mode, error correction M):
- Version 5 (37×37): 256 chars
- Version 6 (41×41): 304 chars

**Result**: Fits comfortably in Version 5 QR code.

**Base32 Encoding**:
```haskell
-- RFC 4648 alphabet (A-Z, 2-7)
base32Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

-- Encode without padding
base32NoPad :: ByteString -> Text
base32NoPad = T.dropWhileEnd (== '=') . encodeBase32
```

**Why No Padding**:
- Padding `=` is not in QR alphanumeric set
- Not needed for decoding (length is fixed: 32 bytes)
- Saves 4 characters

**Decode Roundtrip**:
```haskell
-- Verify: decode(encode(hash)) == hash
prop_base32_roundtrip :: ByteString -> Property
prop_base32_roundtrip bs =
  decodeBase32 (base32NoPad bs) === Right bs
```

---

## 7. Size Limit Rationale

### Decision: Payload 131,072 / Proof 262,144 / Receipt 16,384 bytes

**Problem**: Without limits, unbounded artifacts could exhaust storage/memory.

**Approach**: Empirical limits with headroom

| Artifact | Typical Size | Max Size | Headroom Factor | Rationale |
|----------|--------------|----------|-----------------|-----------|
| **Payload** | 10-20 KB | **128 KB** | 6-12x | Typical passport has 20-50 fields; 128KB allows complex cases |
| **Proof** | 50-100 KB | **256 KB** | 2-5x | Deep derivation trees for complex rules; 256KB allows nesting |
| **Receipt** | 2-4 KB | **16 KB** | 4-8x | Minimal metadata; 16KB is very generous |

**Payload Analysis**:
- Typical field: ~200 bytes (name + type + value + metadata)
- 50 fields × 200 bytes = 10 KB
- 100 fields × 200 bytes = 20 KB
- Limit: 128 KB allows 600+ fields (far beyond realistic)

**Proof Analysis**:
- Typical node: ~300 bytes (id + type + hash + data + children)
- 10 nodes per field (average evaluation depth)
- 50 fields × 10 nodes × 300 bytes = 150 KB
- Limit: 256 KB allows 850+ nodes

**Receipt Analysis**:
- Fixed fields: ~2 KB (UUIDs, hashes, metadata)
- Limit: 16 KB allows 8x expansion (future fields)

**Error Handling**:
```haskell
checkSize :: Text -> Int -> ByteString -> (Int -> CompileError) -> Either CompileError ()
checkSize name limit bs mkError
  | BS.length bs > limit = Left $ mkError (BS.length bs)
  | otherwise = Right ()

-- Error includes actual size for debugging
data CompileError
  = PayloadTooLarge Int  -- Actual size
  | ProofTooLarge Int
  | ReceiptTooLarge Int
```

**Monitoring Strategy**:
- Log artifact sizes in production
- Alert if approaching limits (> 80%)
- Adjust limits if needed (requires SSOT update)

---

## 8. Canonical JSON Determinism

### Decision: Use BPC-CJSON-1 from 02-core-primitives

**Problem**: JSON serialization can vary (key order, whitespace, number format).

**Solution**: Canonical JSON (BPC-CJSON-1)

**Rules**:
1. Object keys sorted by UTF-8 byte order
2. No whitespace (compact format)
3. Numbers: integers only (no floats, no exponents)
4. Strings: minimal escaping (only required escapes)

**Example**:
```json
Input:  {"b": 2, "a": 1}
Canonical: {"a":1,"b":2}

Input:  {"value": 3.14}
Error: CanonicalNumberNotAllowed (floats forbidden)
```

**Why UTF-8 Byte Order**:
- Locale-independent
- Deterministic across platforms
- Matches SSOT 7.1 specification

**Implementation** (from 02-core-primitives):
```haskell
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalEncode = ...

-- Property test
prop_canonical_deterministic :: Value -> Property
prop_canonical_deterministic v =
  canonicalEncode v === canonicalEncode v
```

---

## 9. Topological Sort for Fields

### Decision: Use Kahn's algorithm with lexical tie-breaking

**Problem**: Fields can reference other fields; must evaluate in dependency order.

**Example**:
```text
field a = 1;
field b = a + 2;
field c = b * 3;
```

**Dependency Graph**: a → b → c

**Algorithm**: Kahn's algorithm (SSOT 7.5)

```text
topoSort(fields):
  1. Build adjacency list from field references
  2. Compute in-degrees
  3. Start with zero in-degree nodes
  4. Process in lexical order (determinism!)
  5. Remove edges, add newly zero in-degree nodes
  6. If graph not empty after processing → CYCLE
```

**Why Lexical Tie-Breaking**:
- Multiple fields may have zero in-degree simultaneously
- Lexical order ensures deterministic ordering
- Example: `field a` and `field x` both zero in-degree → process `a` first

**Cycle Detection**:
```haskell
data CompileError
  = CycleDetected [FieldPath]  -- Fields involved in cycle

-- Example cycle
field a = b + 1;
field b = a + 2;
-- Error: CycleDetected ["a", "b"]
```

**Why Not DFS**: DFS visit order depends on implementation details (harder to make deterministic).

---

## Open Questions

**None** - SSOT fully specifies all formats and algorithms.

---

## Next Steps

Proceed to implementation following plan.md phases:
1. Phase 1: Compile orchestration
2. Phase 2: Proof builder
3. Phase 3: Receipt builder
4. Phase 4: Signature
5. Phase 5: QR payload
6. Phase 6: Size limits
7. Phase 7: Golden tests

---

## References

- **SSOT 7.6**: BPC-COMPILE-1 algorithm
- **SSOT 9.1**: BPC-PROOF-1 schema
- **SSOT 9.2**: BPC-PROOF-HASH-1 node hashing
- **SSOT 9.3**: BPC-RECEIPT-1 schema
- **SSOT 7.7**: BPC-SIGN-1 signature
- **SSOT 7.8**: BPC-QR-1 QR payload format
- **RFC 4648**: Base32 encoding
