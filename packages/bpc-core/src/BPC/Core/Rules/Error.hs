{-# LANGUAGE OverloadedStrings #-}

-- | BPC.Core.Rules.Error - Error types for Rule Engine
module BPC.Core.Rules.Error
  ( -- * Parse Errors
    ParseError (..)

    -- * Type Errors
  , TypecheckError (..)

    -- * Cycle Errors
  , CycleError (..)

    -- * Evaluation Errors
  , EvalError (..)
  ) where

import Data.Text (Text)

-- | Errors during DSL parsing.
data ParseError = ParseError
  { parseErrorLine :: !Int
  , parseErrorColumn :: !Int
  , parseErrorMessage :: !Text
  }
  deriving stock (Eq, Show)

-- | Errors during type checking.
data TypecheckError
  = TypeMismatch !Text !Text !Text  -- expected, actual, context
  | UnitMismatch !Text !Text !Text  -- unit1, unit2, context
  | UnknownField !Text              -- field name
  | UnknownUnit !Text               -- unit name
  | ArityMismatch !Text !Int !Int   -- function, expected, actual
  | UnknownFunction !Text           -- function name
  deriving stock (Eq, Show)

-- | Errors for cycle detection in field dependencies.
data CycleError = CycleError
  { cycleFields :: ![Text]  -- Fields involved in the cycle
  }
  deriving stock (Eq, Show)

-- | Errors during rule evaluation.
data EvalError
  = DivisionByZero !Text        -- context
  | NullValue !Text !Text       -- field, errorCode
  | AssertionFailed !Text !Text -- message, errorCode
  | FactNotFound !Text !Text    -- factType, factKey
  deriving stock (Eq, Show)
