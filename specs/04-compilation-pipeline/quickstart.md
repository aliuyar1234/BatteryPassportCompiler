# Quickstart: Compilation Pipeline

**Feature**: 04-compilation-pipeline
**Date**: 2025-12-28
**Package**: bpc-core

## Prerequisites

### Feature Dependencies

- **02-core-primitives** complete (canonical JSON, hashing)
- **03-rule-engine** complete (typed AST, evaluation)

### Verify Prerequisites

```haskell
import BPC.Core.Canonical (canonicalEncode)
import BPC.Core.Hash (sha256Hex)
import BPC.Rules.AST (Expr)
import BPC.Rules.Eval (eval)
import BPC.Rules.Graph (topoSortFields)
```

---

## Quick Usage

### 1. Compile a Passport

```haskell
import BPC.Core.Compile
import BPC.Core.Proof
import BPC.Core.Receipt

-- Build input
let input = CompileInput
      { ciSnapshot = sealedSnapshot    -- From 05-data-layer
      , ciRules = typedRules            -- From 03-rule-engine
      , ciProduct = batteryProduct      -- From 05-data-layer
      , ciIssuedAt = parseTime "2025-01-01T00:00:00Z"
      , ciBuildId = "build-123"
      }

-- Compile (pure function!)
case compilePassportPure input of
  Left err -> error $ "Compilation failed: " ++ show err
  Right output -> do
    -- All outputs are canonical bytes
    putStrLn $ "Payload hash: " ++ coPayloadHash output
    putStrLn $ "Proof root: " ++ coProofRootHash output
    putStrLn $ "Receipt hash: " ++ coReceiptHash output

    -- Outputs are canonical bytes, ready for storage
    storeInDB (coPayload output) (coProof output) (coReceiptUnsigned output)
```

---

### 2. Verify Proof

```haskell
import BPC.Core.Proof

-- Verify proof integrity (all node hashes match)
case verifyProof (coProof output) of
  Left err -> error $ "Proof invalid: " ++ show err
  Right () -> putStrLn "Proof verified!"
```

---

### 3. Sign Receipt

```haskell
import BPC.Core.Receipt

-- Load private key from environment
keyBase64 <- getEnv "BPC_SIGNING_KEY"
let privateKey = Ed25519PrivateKey (decodeBase64 keyBase64)

-- Sign receipt hash
let signature = signReceiptHash privateKey (coReceiptHash output)

-- Verify signature
let publicKey = derivePublicKey privateKey
if verifySignature publicKey (coReceiptHash output) signature
  then putStrLn "Signature valid!"
  else error "Signature invalid!"
```

---

### 4. Generate QR Payload

```haskell
import BPC.Core.QR

let qrInput = QrInput
      { qiPassportVersionId = passportVersionId
      , qiPayloadHash = coPayloadHash output
      , qiProofRootHash = coProofRootHash output
      , qiReceiptHash = coReceiptHash output
      }

let qrPayload = buildQrPayload qrInput
-- "BPC1|pv=550e8400-e29b-41d4-a716-446655440000|ph=MFZW...|pr=NJSW...|rh=GEZDG..."

putStrLn $ "QR payload (" ++ show (T.length qrPayload) ++ " chars): " ++ qrPayload
```

---

## Determinism Testing

### Property Test (THE Critical Test)

```haskell
import Test.Tasty.QuickCheck

-- THE most important test - verifies determinism
prop_deterministic :: CompileInput -> Property
prop_deterministic input =
  let result1 = compilePassportPure input
      result2 = compilePassportPure input
  in result1 === result2

-- Run with QuickCheck (1000 samples)
main :: IO ()
main = quickCheckWith (stdArgs { maxSuccess = 1000 }) prop_deterministic
```

---

## Common Patterns

### Pattern 1: Handle Compilation Errors

```haskell
handleCompileError :: Either CompileError CompileOutput -> IO ()
handleCompileError result = case result of
  Left SnapshotNotSealed ->
    putStrLn "Error: Seal snapshot first"

  Left RulesNotPublished ->
    putStrLn "Error: Publish rules first"

  Left (CycleDetected fields) ->
    putStrLn $ "Error: Circular dependencies in fields: " ++ show fields

  Left (PayloadTooLarge size) ->
    putStrLn $ "Error: Payload too large (" ++ show size ++ " bytes, max 131,072)"

  Left (ProofTooLarge size) ->
    putStrLn $ "Error: Proof too large (" ++ show size ++ " bytes, max 262,144)"

  Left (ReceiptTooLarge size) ->
    putStrLn $ "Error: Receipt too large (" ++ show size ++ " bytes, max 16,384)"

  Left (EvalError e) ->
    putStrLn $ "Error: Evaluation failed: " ++ show e

  Right output ->
    putStrLn "Compilation succeeded!"
```

---

### Pattern 2: Replay Verification (Audit Trail)

```haskell
import BPC.Core.Compile
import BPC.Core.Receipt

-- Re-run compilation with stored inputs
replayPassport :: PassportVersionId -> IO ()
replayPassport pvId = do
  -- Load stored passport version
  storedVersion <- loadPassportVersion pvId

  -- Load inputs (same as original compilation)
  snapshot <- loadSnapshot (storedSnapshotId storedVersion)
  rules <- loadRules (storedRulesId storedVersion)
  product <- loadProduct (storedProductId storedVersion)

  -- Reconstruct CompileInput
  let replayInput = CompileInput
        { ciSnapshot = snapshot
        , ciRules = rules
        , ciProduct = product
        , ciIssuedAt = storedIssuedAt storedVersion
        , ciBuildId = storedBuildId storedVersion
        }

  -- Recompile
  case compilePassportPure replayInput of
    Left err -> error $ "Replay failed: " ++ show err
    Right replayOutput -> do
      -- Verify hashes match
      assert (coPayloadHash replayOutput == storedPayloadHash storedVersion)
      assert (coProofRootHash replayOutput == storedProofRootHash storedVersion)
      assert (coReceiptHash replayOutput == storedReceiptHash storedVersion)

      -- Verify signature
      let pubKey = lookupPublicKey (storedSigningKeyId storedVersion)
      assert (verifySignature pubKey (coReceiptHash replayOutput) (storedSignature storedVersion))

      putStrLn "Replay verification succeeded!"
```

---

### Pattern 3: Inspect Proof Tree

```haskell
import BPC.Core.Proof
import Data.Aeson (decode)

-- Decode proof from canonical bytes
inspectProof :: ByteString -> IO ()
inspectProof proofBytes = case decode proofBytes of
  Nothing -> error "Invalid proof JSON"
  Just proof -> do
    putStrLn $ "Proof version: " ++ proofVersion proof
    putStrLn $ "Root hash: " ++ rootHash proof
    putStrLn $ "Total nodes: " ++ show (length $ nodes proof)

    -- Show field index
    putStrLn "\nField index:"
    forM_ (Map.toList $ fieldIndex proof) $ \(fieldPath, nodeId) ->
      putStrLn $ "  " ++ fieldPath ++ " -> node " ++ show nodeId

    -- Show nodes
    putStrLn "\nNodes:"
    forM_ (nodes proof) $ \node -> do
      putStrLn $ "\nNode " ++ show (pnId node) ++ ":"
      putStrLn $ "  Type: " ++ show (pnType node)
      putStrLn $ "  Hash: " ++ take 16 (pnHash node) ++ "..."
      putStrLn $ "  Children: " ++ show (pnChildren node)
```

---

### Pattern 4: Build CompileInput from Database

```haskell
import BPC.Core.Compile
import BPC.DB.Repositories

-- Load all inputs from database and build CompileInput
buildCompileInput :: SnapshotId -> RulePackageVersionId -> BatteryProductId -> IO CompileInput
buildCompileInput snapshotId rulesId productId = do
  -- Load sealed snapshot
  snapshot <- loadSealedSnapshot snapshotId
  unless (isSealed snapshot) $ error "Snapshot not sealed"

  -- Load published rules
  rules <- loadRulePackageVersion rulesId
  unless (isPublished rules) $ error "Rules not published"

  -- Load battery product
  product <- loadBatteryProduct productId

  -- Get current time for issued_at
  issuedAt <- getCurrentTime

  -- Get compiler build ID
  buildId <- getBuildId  -- From cabal metadata or ENV

  pure CompileInput
    { ciSnapshot = snapshot
    , ciRules = rules
    , ciProduct = product
    , ciIssuedAt = issuedAt
    , ciBuildId = buildId
    }
```

---

### Pattern 5: Store CompileOutput to Database

```haskell
import BPC.Core.Compile
import BPC.DB.Repositories

-- Store compilation output in database
storePassportVersion :: PassportVersionId -> CompileOutput -> Signature -> IO ()
storePassportVersion pvId output signature = do
  let passportVersion = PassportVersion
        { pvId = pvId
        , pvStatus = COMPILING
        , pvPayloadCanonical = coPayload output
        , pvPayloadHash = coPayloadHash output
        , pvProofCanonical = coProof output
        , pvProofRootHash = coProofRootHash output
        , pvReceiptCanonical = coReceiptUnsigned output
        , pvReceiptHash = coReceiptHash output
        , pvSignatureAlg = "ED25519"
        , pvSignature = Just signature
        , pvCreatedAt = now()
        }

  insertPassportVersion passportVersion
```

---

## Complete Example: Compile → Sign → Store

```haskell
import BPC.Core.Compile
import BPC.Core.Receipt
import BPC.Core.QR
import BPC.DB.Repositories

-- Complete workflow
compileAndStore :: SnapshotId -> RulePackageVersionId -> BatteryProductId -> IO PassportVersionId
compileAndStore snapshotId rulesId productId = do
  -- 1. Build input
  input <- buildCompileInput snapshotId rulesId productId

  -- 2. Compile (pure!)
  output <- case compilePassportPure input of
    Left err -> throwIO $ CompilationError err
    Right out -> pure out

  putStrLn $ "Compilation succeeded:"
  putStrLn $ "  Payload: " ++ show (BS.length $ coPayload output) ++ " bytes"
  putStrLn $ "  Proof: " ++ show (BS.length $ coProof output) ++ " bytes"
  putStrLn $ "  Receipt: " ++ show (BS.length $ coReceiptUnsigned output) ++ " bytes"

  -- 3. Verify proof
  case verifyProof (coProof output) of
    Left err -> throwIO $ ProofVerificationError err
    Right () -> putStrLn "Proof verified!"

  -- 4. Sign receipt
  privateKey <- loadSigningKey
  let signature = signReceiptHash privateKey (coReceiptHash output)

  putStrLn "Receipt signed!"

  -- 5. Generate QR payload
  pvId <- generatePassportVersionId
  let qrInput = QrInput
        { qiPassportVersionId = pvId
        , qiPayloadHash = coPayloadHash output
        , qiProofRootHash = coProofRootHash output
        , qiReceiptHash = coReceiptHash output
        }
  let qrPayload = buildQrPayload qrInput

  putStrLn $ "QR payload generated (" ++ show (T.length qrPayload) ++ " chars)"

  -- 6. Store in database
  storePassportVersion pvId output signature

  putStrLn "Passport version stored!"

  pure pvId
```

---

## Testing

### Run Tests

```bash
# Run all compilation tests
cabal test bpc-core --test-option="--pattern=Compile"

# Run golden tests (determinism verification)
cabal test bpc-core --test-option="--pattern=golden"

# Run property tests (1000 samples)
cabal test bpc-core --test-option="--quickcheck-tests=1000"

# Run proof verification tests
cabal test bpc-core --test-option="--pattern=Proof"

# Run signature tests
cabal test bpc-core --test-option="--pattern=Receipt"
```

---

### Golden Test Example

```haskell
import Test.Tasty.Golden
import BPC.Core.Compile

goldenTest :: TestTree
goldenTest = testGroup "Golden Tests"
  [ goldenCompile "minimal" "test/golden/minimal.json"
  , goldenCompile "medium" "test/golden/medium.json"
  , goldenCompile "edge" "test/golden/edge.json"
  ]

goldenCompile :: String -> FilePath -> TestTree
goldenCompile name fixturePath = testCase name $ do
  -- Load fixture
  fixture <- loadFixture fixturePath

  -- Compile
  let result = compilePassportPure (fixtureInput fixture)

  -- Verify
  case result of
    Left err -> assertFailure $ "Compilation failed: " ++ show err
    Right output -> do
      -- Byte-exact comparison
      assertEqual "payload bytes" (fixturePayloadCanonical fixture) (coPayload output)
      assertEqual "payload hash" (fixturePayloadHash fixture) (coPayloadHash output)
      assertEqual "proof bytes" (fixtureProofCanonical fixture) (coProof output)
      assertEqual "proof root" (fixtureProofRootHash fixture) (coProofRootHash output)
      assertEqual "receipt bytes" (fixtureReceiptCanonical fixture) (coReceiptUnsigned output)
      assertEqual "receipt hash" (fixtureReceiptHash fixture) (coReceiptHash output)
```

---

### Property Test Examples

```haskell
import Test.Tasty.QuickCheck

-- Determinism (THE critical property)
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

-- QR determinism
prop_qr_deterministic :: QrInput -> Property
prop_qr_deterministic input =
  buildQrPayload input === buildQrPayload input

-- Size limits are enforced
prop_size_limits :: CompileInput -> Property
prop_size_limits input = case compilePassportPure input of
  Left (PayloadTooLarge size) -> size > maxPayloadSize
  Left (ProofTooLarge size) -> size > maxProofSize
  Left (ReceiptTooLarge size) -> size > maxReceiptSize
  Right output ->
    BS.length (coPayload output) <= maxPayloadSize .&&.
    BS.length (coProof output) <= maxProofSize .&&.
    BS.length (coReceiptUnsigned output) <= maxReceiptSize
```

---

## Debugging

### Enable Detailed Logging

```haskell
import Debug.Trace

-- Trace compilation steps
compilePassportPureDebug :: CompileInput -> Either CompileError CompileOutput
compilePassportPureDebug input =
  traceShow ("Compiling with " ++ show (length $ ruleFields $ ciRules input) ++ " fields") $
  compilePassportPure input
```

### Inspect Intermediate Values

```haskell
-- After compilation
case compilePassportPure input of
  Right output -> do
    -- Decode proof to inspect nodes
    let proof = decode (coProof output) :: Maybe Proof
    case proof of
      Just p -> do
        putStrLn $ "Nodes generated: " ++ show (length $ nodes p)
        forM_ (take 5 $ nodes p) $ \node ->
          putStrLn $ "Node " ++ show (pnId node) ++ ": " ++ show (pnType node)
      Nothing -> error "Invalid proof JSON"
```

---

## Next Steps

After compilation pipeline is complete:

1. **05-data-layer**: Store compiled artifacts in PostgreSQL
2. **07-job-processing**: COMPILE_PASSPORT job uses `compilePassportPure`
3. **08-advanced-features**: Replay verification endpoint

---

## Performance Tips

### 1. Avoid Recompiling Unnecessarily

```haskell
-- Cache compilation results by snapshot_hash + rules_hash
cacheKey = sha256Hex (snapshotHash <> rulesHash)

result <- lookupCache cacheKey
case result of
  Just cached -> pure cached
  Nothing -> do
    output <- compilePassportPure input
    insertCache cacheKey output
    pure output
```

### 2. Parallel Proof Verification

```haskell
-- Verify nodes in parallel
verifyProofParallel :: Proof -> IO (Either ProofError ())
verifyProofParallel proof = do
  results <- forConcurrently (nodes proof) $ \node -> do
    let expected = computeNodeHash node (nodes proof)
    pure (pnHash node == expected)

  if all id results
    then pure (Right ())
    else pure (Left (HashMismatch 0))
```

### 3. Lazy Decoding

```haskell
-- Don't decode entire proof if only checking hash
verifyProofHash :: ByteString -> Text -> Bool
verifyProofHash proofBytes expectedHash =
  sha256Hex proofBytes == expectedHash
```

---

## Common Pitfalls

### Pitfall 1: Non-Deterministic Timestamps

```haskell
-- WRONG: Generating timestamp inside function
compilePassport :: CompileInput -> IO CompileOutput
compilePassport input = do
  now <- getCurrentTime  -- NON-DETERMINISTIC!
  ...

-- RIGHT: Pass timestamp as input
compilePassportPure :: CompileInput -> Either CompileError CompileOutput
compilePassportPure input =
  -- Use ciIssuedAt from input
  ...
```

### Pitfall 2: HashMap Iteration

```haskell
-- WRONG: HashMap has non-deterministic iteration order
import Data.HashMap.Strict as HM

fields :: HashMap FieldPath Expr
fields = ...

-- RIGHT: Use ordered Map
import Data.Map.Strict as Map

fields :: Map FieldPath Expr
fields = ...
```

### Pitfall 3: Floating Point

```haskell
-- WRONG: Float arithmetic is non-deterministic
value :: Float
value = 3.14159

-- RIGHT: Use fixed-point Dec
value :: Dec 6
value = toDec 6 3141590
```

---

## Reference

- **SSOT.md**: Normative specification (sections 7.6, 9)
- **Plan**: [plan.md](./plan.md)
- **Research**: [research.md](./research.md)
- **Data Model**: [data-model.md](./data-model.md)
