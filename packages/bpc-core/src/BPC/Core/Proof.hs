{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | BPC.Core.Proof - Proof Tree Generation (BPC-PROOF-1)
--
-- Generates derivation trees capturing all computation steps for audit replay.
-- Each node includes a hash computed per BPC-PROOF-HASH-1 formula.
module BPC.Core.Proof
  ( -- * Proof Types
    Proof (..)
  , ProofNode (..)
  , NodeType (..)

    -- * Proof Building
  , buildProof
  , buildFieldIndex

    -- * Hashing
  , computeNodeHash
  , computeRootHash

    -- * Verification
  , verifyProof

    -- * Errors
  , ProofError (..)
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..), (.=), object)
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import BPC.Core.CanonicalJson (canonicalEncode)
import BPC.Core.Hash (sha256Hex)

-- | Node types in the derivation tree.
data NodeType
  = CONST          -- ^ Constant literal value
  | FACT_GET       -- ^ Fact retrieval (getFact, getFactsByPrefix)
  | FIELD_REF      -- ^ Reference to another field
  | OP             -- ^ Binary/unary operation
  | ASSERT         -- ^ Assertion node
  | COMPLIANCE_EMIT -- ^ Compliance emission
  deriving stock (Eq, Show, Generic)

instance ToJSON NodeType where
  toJSON CONST = "CONST"
  toJSON FACT_GET = "FACT_GET"
  toJSON FIELD_REF = "FIELD_REF"
  toJSON OP = "OP"
  toJSON ASSERT = "ASSERT"
  toJSON COMPLIANCE_EMIT = "COMPLIANCE_EMIT"

instance FromJSON NodeType

-- | A node in the derivation tree.
data ProofNode = ProofNode
  { pnId :: !Int               -- ^ Unique node ID
  , pnType :: !NodeType        -- ^ Node type
  , pnValue :: !Aeson.Value    -- ^ Computed value
  , pnInputIds :: ![Int]       -- ^ IDs of input nodes
  , pnFieldPath :: !(Maybe Text)  -- ^ Optional field path
  , pnHash :: !Text            -- ^ Node hash per BPC-PROOF-HASH-1
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ProofNode where
  toJSON ProofNode{..} = object
    [ "id" .= pnId
    , "type" .= pnType
    , "value" .= pnValue
    , "input_ids" .= pnInputIds
    , "field_path" .= pnFieldPath
    , "hash" .= pnHash
    ]

instance FromJSON ProofNode

-- | Complete proof tree with metadata.
data Proof = Proof
  { prVersion :: !Text         -- ^ Always "BPC-PROOF-1"
  , prNodes :: ![ProofNode]    -- ^ All nodes in topological order
  , prRootHash :: !Text        -- ^ Combined hash of all nodes
  , prFieldIndex :: !(Map Text Int)  -- ^ Field path -> node ID mapping
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Proof where
  toJSON Proof{..} = object
    [ "version" .= prVersion
    , "nodes" .= prNodes
    , "root_hash" .= prRootHash
    , "field_index" .= prFieldIndex
    ]

instance FromJSON Proof

-- | Errors during proof operations.
data ProofError
  = InvalidNodeHash !Int !Text !Text  -- ^ nodeId, expected, actual
  | InvalidRootHash !Text !Text       -- ^ expected, actual
  | MissingInputNode !Int !Int        -- ^ nodeId, missingInputId
  deriving stock (Eq, Show)

-- | Compute hash for a proof node per BPC-PROOF-HASH-1.
--
-- Formula: SHA256(type || value_canonical || sorted(input_hashes))
computeNodeHash :: NodeType -> Aeson.Value -> [Text] -> Text
computeNodeHash nodeType value inputHashes =
  let typeBytes = TE.encodeUtf8 $ T.pack $ show nodeType
      valueBytes = case canonicalEncode value of
        Right bs -> bs
        Left _ -> BS.empty
      sortedHashes = T.intercalate "" $ map id inputHashes  -- Already sorted by ID
      hashesBytes = TE.encodeUtf8 sortedHashes
      combined = typeBytes <> valueBytes <> hashesBytes
  in sha256Hex combined

-- | Compute root hash from all node hashes.
computeRootHash :: [ProofNode] -> Text
computeRootHash nodes =
  let allHashes = T.intercalate "" $ map pnHash nodes
  in sha256Hex (TE.encodeUtf8 allHashes)

-- | Build field index mapping field paths to node IDs.
buildFieldIndex :: [ProofNode] -> Map Text Int
buildFieldIndex nodes = Map.fromList
  [ (path, pnId node)
  | node <- nodes
  , Just path <- [pnFieldPath node]
  ]

-- | Build a complete proof from a list of nodes.
--
-- Computes hashes for all nodes and the root hash.
buildProof :: [ProofNode] -> Proof
buildProof rawNodes =
  let -- First pass: compute hashes for all nodes
      nodeMap = Map.fromList [(pnId n, n) | n <- rawNodes]
      nodesWithHashes = computeAllHashes nodeMap rawNodes
      rootHash = computeRootHash nodesWithHashes
      fieldIndex = buildFieldIndex nodesWithHashes
  in Proof
    { prVersion = "BPC-PROOF-1"
    , prNodes = nodesWithHashes
    , prRootHash = rootHash
    , prFieldIndex = fieldIndex
    }

-- | Compute hashes for all nodes in topological order.
computeAllHashes :: Map Int ProofNode -> [ProofNode] -> [ProofNode]
computeAllHashes nodeMap = go Map.empty
  where
    go :: Map Int Text -> [ProofNode] -> [ProofNode]
    go _ [] = []
    go hashCache (node:rest) =
      let inputHashes = map (getHash hashCache nodeMap) (pnInputIds node)
          nodeHash = computeNodeHash (pnType node) (pnValue node) inputHashes
          updatedCache = Map.insert (pnId node) nodeHash hashCache
          updatedNode = node { pnHash = nodeHash }
      in updatedNode : go updatedCache rest

    getHash :: Map Int Text -> Map Int ProofNode -> Int -> Text
    getHash cache nodes nid =
      case Map.lookup nid cache of
        Just h -> h
        Nothing -> case Map.lookup nid nodes of
          Just n -> pnHash n
          Nothing -> ""

-- | Verify a proof by recomputing all hashes.
verifyProof :: Proof -> Either ProofError ()
verifyProof Proof{..} = do
  -- Verify each node hash
  verifyNodeHashes Map.empty prNodes
  -- Verify root hash
  let computedRoot = computeRootHash prNodes
  if computedRoot /= prRootHash
    then Left $ InvalidRootHash prRootHash computedRoot
    else Right ()

-- | Verify hashes for all nodes.
verifyNodeHashes :: Map Int Text -> [ProofNode] -> Either ProofError ()
verifyNodeHashes _ [] = Right ()
verifyNodeHashes hashCache (node:rest) = do
  let inputHashes = map (Map.findWithDefault "" `flip` hashCache) (pnInputIds node)
  let expectedHash = computeNodeHash (pnType node) (pnValue node) inputHashes
  if expectedHash /= pnHash node
    then Left $ InvalidNodeHash (pnId node) (pnHash node) expectedHash
    else do
      let updatedCache = Map.insert (pnId node) (pnHash node) hashCache
      verifyNodeHashes updatedCache rest
