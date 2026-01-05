{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | BPC.Core.Compile - Pure Passport Compilation
--
-- Implements deterministic compilation from Snapshot + Rules to Passport artifacts.
-- All functions are pure (IO-free) and produce byte-identical outputs for identical inputs.
module BPC.Core.Compile
  ( -- * Compilation
    compilePassportPure

    -- * Input/Output Types
  , CompileInput (..)
  , CompileOutput (..)

    -- * Error Types
  , CompileError (..)

    -- * Size Limits
  , maxPayloadSize
  , maxProofSize
  , maxReceiptSize

    -- * Validation
  , validateInput
  , checkSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.UUID (UUID)

import BPC.Core.CanonicalJson (canonicalEncode)
import BPC.Core.Hash (sha256Hex)
import BPC.Core.Proof (Proof, buildProof, ProofNode)
import BPC.Core.Receipt (ReceiptUnsigned, buildReceiptUnsigned, ReceiptInput (..))
import BPC.Core.Rules.AST (Module)
import BPC.Core.Rules.Eval (Value, evalModule, emptyContext, withFacts)
import BPC.Core.Rules.Error (EvalError, CycleError)
import BPC.Core.Rules.Graph (topoSortFields, buildGraph)

import qualified Data.Aeson as Aeson

-- | Size limit for payload (131,072 bytes = 128 KB)
maxPayloadSize :: Int
maxPayloadSize = 131072

-- | Size limit for proof (262,144 bytes = 256 KB)
maxProofSize :: Int
maxProofSize = 262144

-- | Size limit for receipt (16,384 bytes = 16 KB)
maxReceiptSize :: Int
maxReceiptSize = 16384

-- | Input for passport compilation.
data CompileInput = CompileInput
  { ciSnapshotId :: !UUID
  , ciSnapshotHash :: !Text
  , ciSnapshotStatus :: !Text          -- ^ Must be "SEALED"
  , ciRulesId :: !UUID
  , ciRulesHash :: !Text
  , ciRulesStatus :: !Text             -- ^ Must be "PUBLISHED"
  , ciRulesModule :: !Module           -- ^ Parsed and typechecked rules
  , ciFacts :: !(Map (Text, Text) Value)  -- ^ (factType, factKey) -> Value
  , ciTenantId :: !UUID
  , ciPassportVersionId :: !UUID
  }
  deriving stock (Eq, Show)

-- | Output from passport compilation.
data CompileOutput = CompileOutput
  { coPayloadCanonical :: !ByteString     -- ^ Canonical JSON of passport data
  , coPayloadHash :: !Text                -- ^ SHA-256 hex of payload
  , coProofCanonical :: !ByteString       -- ^ Canonical JSON of proof tree
  , coProofHash :: !Text                  -- ^ SHA-256 hex of proof
  , coReceiptUnsigned :: !ReceiptUnsigned -- ^ Receipt ready for signing
  , coReceiptHash :: !Text                -- ^ SHA-256 hex of unsigned receipt
  , coFieldValues :: !(Map Text Value)    -- ^ Evaluated field values
  , coProofNodes :: ![ProofNode]          -- ^ Proof nodes for derivation tree
  }
  deriving stock (Eq, Show)

-- | Errors during compilation.
data CompileError
  = SnapshotNotSealed !Text           -- ^ Snapshot status was not SEALED
  | RulesNotPublished !Text           -- ^ Rules status was not PUBLISHED
  | CycleDetected ![Text]             -- ^ Cycle in field dependencies
  | EvaluationError !EvalError        -- ^ Error during rule evaluation
  | PayloadTooLarge !Int !Int         -- ^ (actual, limit)
  | ProofTooLarge !Int !Int           -- ^ (actual, limit)
  | ReceiptTooLarge !Int !Int         -- ^ (actual, limit)
  | CanonicalEncodingFailed !Text     -- ^ Error encoding to canonical JSON
  deriving stock (Eq, Show)

-- | Validate compilation input.
validateInput :: CompileInput -> Either CompileError ()
validateInput CompileInput{..} = do
  if ciSnapshotStatus /= "SEALED"
    then Left $ SnapshotNotSealed ciSnapshotStatus
    else Right ()
  if ciRulesStatus /= "PUBLISHED"
    then Left $ RulesNotPublished ciRulesStatus
    else Right ()

-- | Check size against limit.
checkSize :: Text -> Int -> Int -> Either CompileError ()
checkSize name actual limit
  | actual > limit = case name of
      "payload" -> Left $ PayloadTooLarge actual limit
      "proof" -> Left $ ProofTooLarge actual limit
      "receipt" -> Left $ ReceiptTooLarge actual limit
      _ -> Left $ PayloadTooLarge actual limit
  | otherwise = Right ()

-- | Pure passport compilation.
--
-- Given a sealed snapshot and published rules, produces deterministic
-- compilation output including payload, proof tree, and unsigned receipt.
--
-- This function is pure and will produce byte-identical outputs
-- for identical inputs.
compilePassportPure :: CompileInput -> Either CompileError CompileOutput
compilePassportPure input@CompileInput{..} = do
  -- Step 1: Validate input
  validateInput input

  -- Step 2: Check for cycles in field dependencies
  let graph = buildGraph ciRulesModule
  case topoSortFields graph of
    Left cycleErr -> Left $ CycleDetected (cycleFields cycleErr)
    Right _ -> Right ()

  -- Step 3: Evaluate all fields with memoization
  let ctx = withFacts ciFacts emptyContext
  fieldValues <- case evalModule ctx ciRulesModule of
    Left err -> Left $ EvaluationError err
    Right vals -> Right vals

  -- Step 4: Build proof nodes during evaluation
  let proofNodes = buildProofNodes fieldValues

  -- Step 5: Build payload from evaluated values
  payloadCanonical <- buildPayload fieldValues

  -- Step 6: Check payload size
  checkSize "payload" (BS.length payloadCanonical) maxPayloadSize

  -- Step 7: Compute payload hash
  let payloadHash = sha256Hex payloadCanonical

  -- Step 8: Build proof tree
  let proof = buildProof proofNodes
  proofCanonical <- buildProofCanonical proof

  -- Step 9: Check proof size
  checkSize "proof" (BS.length proofCanonical) maxProofSize

  -- Step 10: Compute proof hash
  let proofHash = sha256Hex proofCanonical

  -- Step 11: Build unsigned receipt
  let receiptInput = ReceiptInput
        { riPassportVersionId = ciPassportVersionId
        , riTenantId = ciTenantId
        , riSnapshotId = ciSnapshotId
        , riSnapshotHash = ciSnapshotHash
        , riRulesId = ciRulesId
        , riRulesHash = ciRulesHash
        , riPayloadHash = payloadHash
        , riProofHash = proofHash
        }
  let receiptUnsigned = buildReceiptUnsigned receiptInput
  receiptCanonical <- buildReceiptCanonical receiptUnsigned

  -- Step 12: Check receipt size
  checkSize "receipt" (BS.length receiptCanonical) maxReceiptSize

  -- Step 13: Compute receipt hash
  let receiptHash = sha256Hex receiptCanonical

  Right CompileOutput
    { coPayloadCanonical = payloadCanonical
    , coPayloadHash = payloadHash
    , coProofCanonical = proofCanonical
    , coProofHash = proofHash
    , coReceiptUnsigned = receiptUnsigned
    , coReceiptHash = receiptHash
    , coFieldValues = fieldValues
    , coProofNodes = proofNodes
    }

-- | Build canonical payload JSON from field values.
buildPayload :: Map Text Value -> Either CompileError ByteString
buildPayload values = case canonicalEncode (Aeson.toJSON $ valueMapToJson values) of
  Left err -> Left $ CanonicalEncodingFailed (showErr err)
  Right bs -> Right bs
  where
    showErr e = case e of _ -> "encoding error"

-- | Convert Value map to JSON-encodable format.
valueMapToJson :: Map Text Value -> Map Text Aeson.Value
valueMapToJson = Map.map valueToJson

-- | Convert a Value to JSON.
valueToJson :: Value -> Aeson.Value
valueToJson val = case val of
  _ -> Aeson.Null  -- Placeholder, full implementation needed

-- | Build proof nodes from evaluated fields.
buildProofNodes :: Map Text Value -> [ProofNode]
buildProofNodes _ = []  -- Placeholder, implementation in Proof.hs

-- | Build canonical proof JSON.
buildProofCanonical :: Proof -> Either CompileError ByteString
buildProofCanonical proof = case canonicalEncode (Aeson.toJSON proof) of
  Left _ -> Left $ CanonicalEncodingFailed "proof encoding failed"
  Right bs -> Right bs

-- | Build canonical receipt JSON.
buildReceiptCanonical :: ReceiptUnsigned -> Either CompileError ByteString
buildReceiptCanonical receipt = case canonicalEncode (Aeson.toJSON receipt) of
  Left _ -> Left $ CanonicalEncodingFailed "receipt encoding failed"
  Right bs -> Right bs

-- Helper for cycle error fields
cycleFields :: CycleError -> [Text]
cycleFields (BPC.Core.Rules.Graph.CycleError fields) = fields
