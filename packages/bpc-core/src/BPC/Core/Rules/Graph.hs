{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Graph - Dependency Graph and Topological Sort
--
-- Builds field dependency graph and sorts using Kahn's algorithm
-- as specified in SSOT 7.5.
module BPC.Core.Rules.Graph
  ( -- * Graph Building
    buildGraph
  , extractDeps

    -- * Topological Sort
  , topoSortFields
  , topoSort

    -- * Cycle Detection
  , detectCycles
  , findCycle

    -- * Types
  , DepGraph (..)
  , FieldDeps
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

import BPC.Core.Rules.AST
import BPC.Core.Rules.Error (CycleError (..))

-- | Field dependencies map: field -> set of fields it depends on.
type FieldDeps = Map Text (Set Text)

-- | Dependency graph representation.
data DepGraph = DepGraph
  { dgNodes :: !(Set Text)         -- ^ All field names
  , dgEdges :: !FieldDeps          -- ^ Dependencies (field -> deps)
  , dgReverseEdges :: !FieldDeps   -- ^ Reverse deps (field -> dependents)
  }
  deriving stock (Eq, Show)

-- | Build a dependency graph from a module.
buildGraph :: Module -> DepGraph
buildGraph (Module decls) = DepGraph
  { dgNodes = nodes
  , dgEdges = edges
  , dgReverseEdges = reverseEdges
  }
  where
    -- Extract field declarations only
    fieldDecls = [fd | DeclField fd <- decls]

    -- All field names
    nodes = Set.fromList [unFieldPath (fieldName fd) | fd <- fieldDecls]

    -- Build forward edges: field -> dependencies
    edges = Map.fromList
      [ (unFieldPath (fieldName fd), extractDeps (fieldExpr fd))
      | fd <- fieldDecls
      ]

    -- Build reverse edges: field -> dependents
    reverseEdges = Map.fromListWith Set.union
      [ (dep, Set.singleton (unFieldPath (fieldName fd)))
      | fd <- fieldDecls
      , dep <- Set.toList (extractDeps (fieldExpr fd))
      ]

-- | Extract field dependencies from an expression.
extractDeps :: UntypedExpr -> Set Text
extractDeps (ULitBool _) = Set.empty
extractDeps (ULitInt _) = Set.empty
extractDeps (ULitString _) = Set.empty
extractDeps (ULitDec _ _) = Set.empty
extractDeps (ULitQty _ _ _) = Set.empty
extractDeps (UVar _) = Set.empty  -- Local variables, not fields
extractDeps (UFieldRef (FieldPath path)) = Set.singleton path
extractDeps (UBinOp _ left right) =
  extractDeps left `Set.union` extractDeps right
extractDeps (UUnaryOp _ expr) = extractDeps expr
extractDeps (UFuncCall _ args) =
  Set.unions (map extractDeps args)
extractDeps (ULet _ value body) =
  extractDeps value `Set.union` extractDeps body
extractDeps (UIf cond thenBranch elseBranch) =
  extractDeps cond `Set.union` extractDeps thenBranch `Set.union` extractDeps elseBranch
extractDeps (UAssert cond _ _) = extractDeps cond

-- | Topologically sort fields with cycle detection.
-- Uses Kahn's algorithm as specified in SSOT 7.5.
topoSortFields :: DepGraph -> Either CycleError [Text]
topoSortFields graph = topoSort (dgNodes graph) (dgEdges graph)

-- | Kahn's algorithm for topological sorting.
topoSort :: Set Text -> FieldDeps -> Either CycleError [Text]
topoSort nodes edges = go initialSources inDegrees []
  where
    -- Calculate in-degrees
    inDegrees :: Map Text Int
    inDegrees = Map.fromListWith (+) $
      [(n, 0) | n <- Set.toList nodes] ++
      [(dep, 1) | deps <- Map.elems edges, dep <- Set.toList deps, dep `Set.member` nodes]

    -- Initial sources: nodes with in-degree 0
    initialSources :: [Text]
    initialSources = sortLexical [n | (n, deg) <- Map.toList inDegrees, deg == 0]

    -- Main Kahn's loop
    go :: [Text] -> Map Text Int -> [Text] -> Either CycleError [Text]
    go [] degrees result
      | all (== 0) (Map.elems degrees) = Right (reverse result)
      | otherwise = Left $ CycleError (findCycleNodes degrees edges)
    go (source:sources) degrees result =
      let newDegrees = foldl decrementDegree degrees dependents
          dependents = Set.toList $ Map.findWithDefault Set.empty source edges
          newSources = sortLexical [d | d <- dependents, Map.lookup d newDegrees == Just 0]
          remainingSources = sortLexical $ sources ++ newSources
      in go remainingSources newDegrees (source : result)

    -- Decrement in-degree
    decrementDegree :: Map Text Int -> Text -> Map Text Int
    decrementDegree degrees node =
      Map.adjust (subtract 1) node degrees

-- | Detect cycles in the graph.
detectCycles :: DepGraph -> Maybe CycleError
detectCycles graph = case topoSortFields graph of
  Left err -> Just err
  Right _ -> Nothing

-- | Find nodes involved in a cycle (for error reporting).
findCycleNodes :: Map Text Int -> FieldDeps -> [Text]
findCycleNodes degrees _edges =
  [n | (n, deg) <- Map.toList degrees, deg > 0]

-- | Find a specific cycle path (DFS-based).
findCycle :: DepGraph -> Maybe [Text]
findCycle graph = findCycleDFS (Set.toList $ dgNodes graph) Set.empty []
  where
    findCycleDFS :: [Text] -> Set Text -> [Text] -> Maybe [Text]
    findCycleDFS [] _ _ = Nothing
    findCycleDFS (n:ns) visited path
      | n `Set.member` visited = Just (n : takeWhile (/= n) path ++ [n])
      | otherwise =
          let deps = Set.toList $ Map.findWithDefault Set.empty n (dgEdges graph)
              newVisited = Set.insert n visited
              newPath = n : path
          in case findCycleDFS deps newVisited newPath of
              Just cycle -> Just cycle
              Nothing -> findCycleDFS ns visited path

-- | Sort a list lexically (for deterministic tie-breaking).
sortLexical :: [Text] -> [Text]
sortLexical = sortBy (comparing id)
