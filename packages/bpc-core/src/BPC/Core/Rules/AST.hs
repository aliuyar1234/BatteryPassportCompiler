{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | BPC.Core.Rules.AST - Abstract Syntax Tree for Rule DSL
--
-- Defines both untyped (parsed) and typed (typechecked) AST representations.
module BPC.Core.Rules.AST
  ( -- * Identifiers
    Identifier (..)
  , FieldPath (..)

    -- * Types
  , Ty (..)
  , STy (..)

    -- * Untyped AST (from parser)
  , UntypedExpr (..)
  , FieldDecl (..)
  , Declaration (..)
  , Module (..)

    -- * Typed AST (after typecheck)
  , Expr (..)
  , SomeExpr (..)
  ) where

import Data.Text (Text)

-- | A simple identifier (variable or function name).
newtype Identifier = Identifier { unIdentifier :: Text }
  deriving stock (Eq, Ord, Show)

-- | A dotted field path like "battery.capacity_kwh".
newtype FieldPath = FieldPath { unFieldPath :: Text }
  deriving stock (Eq, Ord, Show)

-- | Type universe for the DSL.
data Ty
  = TBool
  | TInt
  | TDec !Int     -- Dec(scale)
  | TQty !Text    -- Qty(unit)
  | TString
  | TDate
  | TOptional !Ty
  | TList !Ty
  | TRecord ![(Text, Ty)]
  deriving stock (Eq, Show)

-- | Singleton type for runtime type information.
data STy a where
  STyBool :: STy Bool
  STyInt :: STy Integer
  STyString :: STy Text

deriving instance Show (STy a)

-- | Untyped expression (from parser).
data UntypedExpr
  = ULitBool !Bool
  | ULitInt !Integer
  | ULitString !Text
  | ULitDec !Int !Integer  -- scale, value
  | ULitQty !Integer !Text !Text  -- value, scale, unit
  | UVar !Identifier
  | UFieldRef !FieldPath
  | UBinOp !Text !UntypedExpr !UntypedExpr
  | UUnaryOp !Text !UntypedExpr
  | UFuncCall !Identifier ![UntypedExpr]
  | ULet !Identifier !UntypedExpr !UntypedExpr
  | UIf !UntypedExpr !UntypedExpr !UntypedExpr
  | UAssert !UntypedExpr !Text !Text  -- condition, errorCode, message
  deriving stock (Eq, Show)

-- | A field declaration.
data FieldDecl = FieldDecl
  { fieldName :: !FieldPath
  , fieldType :: !Ty
  , fieldExpr :: !UntypedExpr
  }
  deriving stock (Eq, Show)

-- | A top-level declaration.
data Declaration
  = DeclField !FieldDecl
  | DeclExample !Text ![UntypedExpr]  -- name, assertions
  | DeclProperty !Text !UntypedExpr   -- name, property
  deriving stock (Eq, Show)

-- | A complete DSL module.
data Module = Module
  { moduleDecls :: ![Declaration]
  }
  deriving stock (Eq, Show)

-- | Typed expression (after typecheck).
data Expr a where
  EBool :: !Bool -> Expr Bool
  EInt :: !Integer -> Expr Integer
  EString :: !Text -> Expr Text
  -- More constructors would be added during full implementation

deriving instance Show (Expr a)

-- | Existential wrapper for typed expressions.
data SomeExpr where
  SomeExpr :: STy a -> Expr a -> SomeExpr
