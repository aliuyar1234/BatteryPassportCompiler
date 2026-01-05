{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules - Rule DSL Module
--
-- This module re-exports the complete Rule DSL functionality:
-- - Parsing: Convert DSL source to AST
-- - Type Checking: Verify type safety with GADTs
-- - Dependency Graph: Build and topologically sort fields
-- - Built-ins: 16 built-in functions per SSOT 8.2
-- - Evaluation: Pure expression evaluation with memoization
--
-- == Example Usage
--
-- @
-- import BPC.Core.Rules
--
-- -- Parse DSL source
-- let source = "field battery.capacity: Dec(6) = toDec(6, 100);"
-- case parseSource source of
--   Left err -> print err
--   Right modul -> do
--     -- Type check
--     case typecheckSource modul of
--       Left err -> print err
--       Right typed -> do
--         -- Evaluate
--         let ctx = emptyContext
--         case evalModule ctx modul of
--           Left err -> print err
--           Right fields -> print fields
-- @
module BPC.Core.Rules
  ( -- * Parsing
    parseSource
  , parseSourceFile

    -- * AST Types
  , module BPC.Core.Rules.AST

    -- * Error Types
  , module BPC.Core.Rules.Error

    -- * Type Checking
  , typecheck
  , typecheckSource
  , typecheckModule
  , typecheckExpr
  , TypeEnv (..)
  , FieldEnv (..)
  , emptyTypeEnv
  , emptyFieldEnv

    -- * Dependency Graph
  , buildGraph
  , topoSortFields
  , extractDeps
  , detectCycles
  , DepGraph (..)
  , FieldDeps

    -- * Built-in Functions
  , Builtin (..)
  , BuiltinSig (..)
  , builtinSignature
  , lookupBuiltin
  , allBuiltins

    -- * Evaluation
  , evalExpr
  , evalFields
  , evalModule
  , EvalContext (..)
  , emptyContext
  , withFacts
  , withFields
  , Value (..)
  ) where

import BPC.Core.Rules.AST
import BPC.Core.Rules.Builtins
  ( Builtin (..)
  , BuiltinSig (..)
  , allBuiltins
  , builtinSignature
  , lookupBuiltin
  )
import BPC.Core.Rules.Error
import BPC.Core.Rules.Eval
  ( EvalContext (..)
  , Value (..)
  , emptyContext
  , evalExpr
  , evalFields
  , evalModule
  , withFacts
  , withFields
  )
import BPC.Core.Rules.Graph
  ( DepGraph (..)
  , FieldDeps
  , buildGraph
  , detectCycles
  , extractDeps
  , topoSortFields
  )
import BPC.Core.Rules.Parser (parseSource, parseSourceFile)
import BPC.Core.Rules.Typecheck
  ( FieldEnv (..)
  , TypeEnv (..)
  , emptyFieldEnv
  , emptyTypeEnv
  , typecheck
  , typecheckExpr
  , typecheckModule
  , typecheckSource
  )
