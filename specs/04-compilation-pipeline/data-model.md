# Data Model: Compilation Pipeline

**Feature**: 04-compilation-pipeline
**Date**: 2025-12-28
**SSOT Reference**: Sections 7.6 (Compile), 9.1-9.3 (Proof/Receipt)

## Overview

The compilation pipeline transforms inputs (Snapshot, Rules) into outputs (Payload, Proof, Receipt). All types are defined to ensure determinism and type safety.

---

## Input Types

### CompileInput

```haskell
-- | Complete input for compilation
data CompileInput = CompileInput
  { ciSnapshot    :: SealedSnapshot    -- SEALED snapshot with facts
  , ciRules       :: TypedRulePackage  -- Typechecked rules (PUBLISHED)
  , ciProduct     :: BatteryProduct    -- Battery product metadata
  , ciIssuedAt    :: UTCTime           -- Timestamp (passed in, NOT generated)
  , ciBuildId     :: Text              -- Compiler build ID
  }
  deriving (Eq, Show)
```

**Invariants**:
- `ciSnapshot` MUST be SEALED (status check in compilePassportPure)
- `ciRules` MUST be PUBLISHED/DEPRECATED/RETIRED
- `ciIssuedAt` MUST be passed in (determinism requirement)
- `ciBuildId` identifies compiler version for replay

---

### SealedSnapshot

```haskell
-- | Sealed snapshot (immutable)
data SealedSnapshot = SealedSnapshot
  { ssId          :: SnapshotId
  , ssFacts       :: Map (Text, Text) Value  -- (fact_type, fact_key) -> record
  , ssCanonical   :: ByteString              -- Canonical JSON
  , ssHash        :: Text                    -- SHA-256 hex
  , ssSealedAt    :: UTCTime
  }
  deriving (Eq, Show)

newtype SnapshotId = SnapshotId UUID
  deriving (Eq, Ord, Show, ToJSON, FromJSON)
```

**Invariants**:
- `ssCanonical` MUST be canonical encoding of snapshot (BPC-SNAPSHOT-1)
- `ssHash` MUST equal `sha256Hex ssCanonical`
- `ssFacts` is ordered Map (deterministic iteration)

**Fact Structure**:
```haskell
-- Facts are JSON objects
type FactRecord = Value  -- Must be Object

-- Example fact:
{
  "fact_type": "Battery",
  "fact_key": "battery:SKU-123",
  "schema_version": 1,
  "payload": {
    "capacity_kwh": 75.5,
    "chemistry": "NMC",
    "weight_kg": 450
  }
}
```

---

### TypedRulePackage

```haskell
-- | Typechecked rules ready for evaluation
data TypedRulePackage = TypedRulePackage
  { trpId         :: RulePackageVersionId
  , trpFields     :: Map FieldPath (Expr t)  -- Typed expressions (GADTs)
  , trpDslHash    :: Text                    -- SHA-256 of DSL source
  , trpTestsHash  :: Text                    -- SHA-256 of tests source
  }
  deriving (Eq, Show)

newtype RulePackageVersionId = RulePackageVersionId UUID
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Field path (e.g. "battery.capacity_kwh")
newtype FieldPath = FieldPath Text
  deriving (Eq, Ord, Show, ToJSON, FromJSON)
```

**Invariants**:
- `trpFields` is ordered Map (deterministic iteration order)
- All expressions are typechecked (GADT guarantees)
- Expressions use typed AST from 03-rule-engine

---

### BatteryProduct

```haskell
-- | Battery product metadata
data BatteryProduct = BatteryProduct
  { bpId   :: BatteryProductId
  , bpSku  :: Text
  , bpName :: Text
  }
  deriving (Eq, Show)

newtype BatteryProductId = BatteryProductId UUID
  deriving (Eq, Ord, Show, ToJSON, FromJSON)
```

---

## Output Types

### CompileOutput

```haskell
-- | Complete compilation output (all canonical bytes)
data CompileOutput = CompileOutput
  { coPayload         :: ByteString  -- Canonical JSON payload
  , coPayloadHash     :: Text        -- SHA-256 hex
  , coProof           :: ByteString  -- Canonical JSON proof
  , coProofRootHash   :: Text        -- SHA-256 hex of root node
  , coReceiptUnsigned :: ByteString  -- Canonical JSON receipt (no signature)
  , coReceiptHash     :: Text        -- SHA-256 hex of unsigned receipt
  }
  deriving (Eq, Show)
```

**Invariants**:
- All `ByteString` fields are canonical JSON (BPC-CJSON-1)
- `coPayloadHash = sha256Hex coPayload`
- `coProofRootHash = sha256Hex <root node>`
- `coReceiptHash = sha256Hex coReceiptUnsigned`
- Size limits enforced:
  - `length coPayload <= 131072`
  - `length coProof <= 262144`
  - `length coReceiptUnsigned <= 16384`

---

### CompileError

```haskell
-- | Compilation error
data CompileError
  = SnapshotNotSealed                -- Snapshot status != SEALED
  | RulesNotPublished                -- Rules status not PUBLISHED/DEPRECATED/RETIRED
  | CycleDetected [FieldPath]        -- Circular field dependencies
  | EvalError EvalError              -- Expression evaluation failed
  | PayloadTooLarge Int              -- Payload exceeds 131,072 bytes (actual size)
  | ProofTooLarge Int                -- Proof exceeds 262,144 bytes (actual size)
  | ReceiptTooLarge Int              -- Receipt exceeds 16,384 bytes (actual size)
  deriving (Eq, Show)

-- | Expression evaluation error (from 03-rule-engine)
data EvalError
  = FactNotFound Text Text           -- (fact_type, fact_key)
  | FieldNotFound FieldPath          -- Referenced field not defined
  | TypeError Text                   -- Type mismatch
  | DivisionByZero                   -- Arithmetic error
  | RequireSomeFailed Text Text      -- (error_code, message)
  | UnitMismatch Text Text           -- (expected, actual)
  deriving (Eq, Show)
```

---

## Proof Schema (BPC-PROOF-1)

### Proof

```haskell
-- | Proof tree structure
data Proof = Proof
  { proofVersion :: Text           -- "BPC-PROOF-1"
  , rootHash     :: Text           -- SHA-256 hex of root node
  , nodes        :: [ProofNode]    -- All nodes (order preserved)
  , fieldIndex   :: Map Text Int   -- field_path -> node_id
  }
  deriving (Eq, Show)

instance ToJSON Proof where
  toJSON p = object
    [ "proof_version" .= proofVersion p
    , "root_hash" .= rootHash p
    , "nodes" .= nodes p
    , "field_index" .= fieldIndex p
    ]

instance FromJSON Proof where
  parseJSON = withObject "Proof" $ \o -> do
    ver <- o .: "proof_version"
    unless (ver == "BPC-PROOF-1") $ fail "Invalid proof_version"
    Proof <$> pure ver
          <*> o .: "root_hash"
          <*> o .: "nodes"
          <*> o .: "field_index"
```

**Invariants**:
- `proofVersion` MUST be "BPC-PROOF-1"
- `nodes` MUST NOT be empty
- `rootHash` MUST match hash of root node
- All node IDs in `fieldIndex` MUST be valid (< length nodes)

---

### ProofNode

```haskell
-- | Single proof node
data ProofNode = ProofNode
  { pnId       :: Int       -- Unique node ID (0-indexed)
  , pnType     :: NodeType  -- Node type
  , pnHash     :: Text      -- SHA-256 of canonical({id,type,data,children_hashes})
  , pnData     :: Value     -- Node-specific data (JSON)
  , pnChildren :: [Int]     -- Child node IDs
  }
  deriving (Eq, Show)

instance ToJSON ProofNode where
  toJSON n = object
    [ "id" .= pnId n
    , "type" .= pnType n
    , "hash" .= pnHash n
    , "data" .= pnData n
    , "children" .= pnChildren n
    ]

instance FromJSON ProofNode where
  parseJSON = withObject "ProofNode" $ \o ->
    ProofNode <$> o .: "id"
              <*> o .: "type"
              <*> o .: "hash"
              <*> o .: "data"
              <*> o .: "children"
```

**Invariants**:
- `pnId` is unique within proof
- `pnHash` MUST equal `computeNodeHash node allNodes` (BPC-PROOF-HASH-1)
- All IDs in `pnChildren` MUST be valid (< length nodes)
- Children IDs MUST be < parent ID (DAG property)

---

### NodeType

```haskell
-- | Node types (SSOT 9.1)
data NodeType
  = CONST           -- Constant value (literals)
  | FACT_GET        -- Fact lookup (getFact built-in)
  | FIELD_REF       -- Field reference
  | OP              -- Operation (+, -, *, /, ==, etc.)
  | ASSERT          -- Assertion (requireSome, assert)
  | COMPLIANCE_EMIT -- emitCompliance call
  deriving (Eq, Show, Read)

instance ToJSON NodeType where
  toJSON = String . pack . show

instance FromJSON NodeType where
  parseJSON = withText "NodeType" $ \t -> case t of
    "CONST" -> pure CONST
    "FACT_GET" -> pure FACT_GET
    "FIELD_REF" -> pure FIELD_REF
    "OP" -> pure OP
    "ASSERT" -> pure ASSERT
    "COMPLIANCE_EMIT" -> pure COMPLIANCE_EMIT
    _ -> fail $ "Invalid NodeType: " ++ unpack t
```

**Node Data by Type**:

| Type | pnData Contents | Example |
|------|-----------------|---------|
| `CONST` | Literal value | `{"value": 42}` |
| `FACT_GET` | Fact record or null | `{"fact_type": "Battery", "fact_key": "bat:123", "result": {...}}` |
| `FIELD_REF` | Field path | `{"field": "battery.capacity_kwh"}` |
| `OP` | Operation name + operands | `{"op": "+", "left": 10, "right": 5}` |
| `ASSERT` | Condition + error info | `{"condition": true, "error_code": "E001"}` |
| `COMPLIANCE_EMIT` | Compliance ID + status | `{"id": "C001", "status": "PASS"}` |

---

### Node Hash Computation (BPC-PROOF-HASH-1)

```haskell
-- | Compute node hash
computeNodeHash :: ProofNode -> [ProofNode] -> Text
computeNodeHash node allNodes = sha256Hex $ canonicalEncode $ object
  [ "id" .= pnId node
  , "type" .= show (pnType node)
  , "data" .= pnData node
  , "children" .= childrenHashes
  ]
  where
    childrenHashes = [pnHash (allNodes !! i) | i <- pnChildren node]
```

**Example**:
```haskell
-- Node: OP "+" with children [0, 1]
-- Child 0 hash: "abc123..."
-- Child 1 hash: "def456..."

nodeHash = sha256Hex $ canonicalEncode {
  "id": 2,
  "type": "OP",
  "data": {"op": "+", "left": 10, "right": 5},
  "children": ["abc123...", "def456..."]
}
```

---

## Receipt Schema (BPC-RECEIPT-1)

### ReceiptInput

```haskell
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
  deriving (Eq, Show)

-- | UUID newtypes
newtype TenantId = TenantId UUID deriving (Eq, Ord, Show, ToJSON, FromJSON)
newtype PassportVersionId = PassportVersionId UUID deriving (Eq, Ord, Show, ToJSON, FromJSON)
newtype PassportId = PassportId UUID deriving (Eq, Ord, Show, ToJSON, FromJSON)
```

---

### ReceiptUnsigned

```haskell
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
  deriving (Eq, Show)

instance ToJSON ReceiptUnsigned where
  toJSON r = object
    [ "receipt_version" .= ruVersion r
    , "issued_at" .= ruIssuedAt r
    , "tenant_id" .= ruTenantId r
    , "passport_version_id" .= ruPassportVersionId r
    , "passport_id" .= ruPassportId r
    , "battery_product_id" .= ruBatteryProductId r
    , "snapshot_id" .= ruSnapshotId r
    , "snapshot_hash" .= ruSnapshotHash r
    , "rule_package_version_id" .= ruRulePackageVersionId r
    , "dsl_sha256" .= ruDslSha256 r
    , "tests_sha256" .= ruTestsSha256 r
    , "compiler_build_id" .= ruCompilerBuildId r
    , "payload_hash" .= ruPayloadHash r
    , "proof_root_hash" .= ruProofRootHash r
    , "signature_alg" .= ruSignatureAlg r
    , "signing_key_id" .= ruSigningKeyId r
    ]
```

**Invariants**:
- `ruVersion` MUST be "BPC-RECEIPT-1"
- `ruSignatureAlg` MUST be "ED25519"
- All hashes MUST be 64-character hex strings (SHA-256)
- `ruIssuedAt` MUST be in ISO 8601 format

---

### ReceiptSigned

```haskell
-- | Signed receipt (with signature field)
data ReceiptSigned = ReceiptSigned
  { rsUnsigned  :: ReceiptUnsigned
  , rsSignature :: ByteString  -- 64 bytes (ED25519)
  }
  deriving (Eq, Show)

instance ToJSON ReceiptSigned where
  toJSON r = case toJSON (rsUnsigned r) of
    Object o -> Object (o <> "signature" .= encodeBase64 (rsSignature r))
    _ -> error "Impossible: ReceiptUnsigned serialization is not Object"
```

**Invariants**:
- `rsSignature` MUST be 64 bytes
- Signature MUST verify: `ED25519.verify(pubKey, receipt_hash, signature)`

---

## QR Payload (BPC-QR-1)

### QrInput

```haskell
-- | QR payload input
data QrInput = QrInput
  { qiPassportVersionId :: PassportVersionId
  , qiPayloadHash       :: Text
  , qiProofRootHash     :: Text
  , qiReceiptHash       :: Text
  }
  deriving (Eq, Show)
```

---

### QR Format

```haskell
-- | QR payload format: BPC1|pv=<uuid>|ph=<b32>|pr=<b32>|rh=<b32>
buildQrPayload :: QrInput -> Text
buildQrPayload input =
  "BPC1"
  <> "|pv=" <> uuidToText (qiPassportVersionId input)
  <> "|ph=" <> base32NoPad (hexToBytes $ qiPayloadHash input)
  <> "|pr=" <> base32NoPad (hexToBytes $ qiProofRootHash input)
  <> "|rh=" <> base32NoPad (hexToBytes $ qiReceiptHash input)
```

**Format Components**:
- `BPC1`: Format version identifier (4 chars)
- `pv=<uuid>`: Passport version ID (36 chars UUID)
- `ph=<b32>`: Payload hash (52 chars Base32)
- `pr=<b32>`: Proof root hash (52 chars Base32)
- `rh=<b32>`: Receipt hash (52 chars Base32)

**Total Length**: ~212 characters (fits QR Version 5)

---

## Signature Types (BPC-SIGN-1)

### Cryptographic Keys

```haskell
-- | ED25519 private key (32 bytes)
newtype Ed25519PrivateKey = Ed25519PrivateKey ByteString
  deriving (Eq)

-- | ED25519 public key (32 bytes)
newtype Ed25519PublicKey = Ed25519PublicKey ByteString
  deriving (Eq, Show)

-- | Signature (64 bytes)
newtype Signature = Signature ByteString
  deriving (Eq, Show)
```

**DO NOT derive Show for Ed25519PrivateKey** (security)

---

### Signature Functions

```haskell
-- | Sign receipt hash
signReceiptHash :: Ed25519PrivateKey -> Text -> Signature
signReceiptHash (Ed25519PrivateKey privKey) hashHex =
  let hashBytes = hexToBytes hashHex  -- 32 bytes
      sig = ED25519.sign privKey hashBytes  -- 64 bytes
  in Signature sig

-- | Verify signature
verifySignature :: Ed25519PublicKey -> Text -> Signature -> Bool
verifySignature (Ed25519PublicKey pubKey) hashHex (Signature sig) =
  let hashBytes = hexToBytes hashHex
  in ED25519.verify pubKey hashBytes sig

-- | Derive public key from private key
derivePublicKey :: Ed25519PrivateKey -> Ed25519PublicKey
derivePublicKey (Ed25519PrivateKey privKey) =
  Ed25519PublicKey (ED25519.toPublic privKey)
```

---

## Size Limits

```haskell
-- | Maximum artifact sizes (bytes)
maxPayloadSize :: Int
maxPayloadSize = 131072  -- 128 KB (0x20000)

maxProofSize :: Int
maxProofSize = 262144  -- 256 KB (0x40000)

maxReceiptSize :: Int
maxReceiptSize = 16384  -- 16 KB (0x4000)

-- | Check size limit
checkSize :: Text -> Int -> ByteString -> (Int -> CompileError) -> Either CompileError ()
checkSize name limit bs mkError
  | BS.length bs > limit = Left $ mkError (BS.length bs)
  | otherwise = Right ()
```

---

## Utility Types

### Hash Functions

```haskell
-- | SHA-256 hash (hex string)
sha256Hex :: ByteString -> Text
sha256Hex bs = encodeHex $ SHA256.hash bs

-- | Canonical JSON encoding
canonicalEncode :: Value -> Either CanonicalError ByteString
canonicalEncode = ...  -- From 02-core-primitives
```

### Base32 Encoding

```haskell
-- | Base32 encoding without padding (RFC 4648)
base32NoPad :: ByteString -> Text
base32NoPad = T.dropWhileEnd (== '=') . encodeBase32

-- | Base32 decoding
decodeBase32 :: Text -> Either Text ByteString
```

### UUID Utilities

```haskell
-- | UUID to text
uuidToText :: UUID -> Text
uuidToText = toText

-- | Hex to bytes
hexToBytes :: Text -> ByteString
hexToBytes = decodeHex
```

---

## JSON Schema References

All schemas are defined in SSOT.md:
- **BPC-PROOF-1**: SSOT 9.1
- **BPC-RECEIPT-1**: SSOT 9.3
- **BPC-CJSON-1**: SSOT 7.1

---

## Property Invariants

### Global Invariants

```haskell
-- Determinism
forall input. compilePassportPure input == compilePassportPure input

-- Canonical encoding determinism
forall value. canonicalEncode value == canonicalEncode value

-- Hash consistency
forall bs. sha256Hex bs == sha256Hex bs

-- Signature roundtrip
forall key hash. verifySignature (derivePublicKey key) hash (signReceiptHash key hash) == True
```

### Proof Invariants

```haskell
-- All nodes have valid hashes
forall proof. all (\node -> computeNodeHash node (nodes proof) == pnHash node) (nodes proof)

-- Root hash matches
forall proof. rootHash proof == pnHash (last $ nodes proof)

-- Field index references valid nodes
forall proof path. case lookup path (fieldIndex proof) of
  Nothing -> True
  Just nid -> nid >= 0 && nid < length (nodes proof)
```

### Receipt Invariants

```haskell
-- Receipt hash matches canonical encoding
forall receipt. sha256Hex (canonicalEncode receipt) == hashReceiptUnsigned receipt

-- Signature verifies
forall signed pubKey. verifySignature pubKey (hashReceiptUnsigned $ rsUnsigned signed) (rsSignature signed) == True
```

---

## Error Handling

All errors are values (no exceptions thrown):

```haskell
-- Compilation returns Either
compilePassportPure :: CompileInput -> Either CompileError CompileOutput

-- Proof verification returns Either
verifyProof :: ByteString -> Either ProofError ()

-- Canonical encoding returns Either
canonicalEncode :: Value -> Either CanonicalError ByteString
```

**NO EXCEPTIONS** in pure code (constitutional requirement).
