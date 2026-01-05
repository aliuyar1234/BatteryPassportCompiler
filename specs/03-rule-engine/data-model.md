# Data Model: Rule Engine (DSL)

**Feature**: 03-rule-engine
**Date**: 2025-12-28
**SSOT Reference**: Section 8 (Rule-DSL BPC-RULES-1)

## Overview

The Rule Engine defines a domain-specific language for computing passport fields from facts. This document describes the AST types, type system, and built-in functions.

## Type Universe

```haskell
-- | Type universe for the DSL
data Ty
  = TBool                    -- Boolean
  | TInt                     -- Unbounded integer
  | TDec Nat                 -- Fixed-point decimal with scale
  | TQty Symbol              -- Quantity with unit
  | TText                    -- Unicode text
  | TDate                    -- Calendar date (YYYY-MM-DD)
  | TOpt Ty                  -- Optional value (Some/None)
  | TList Ty                 -- Homogeneous list
  | TMap Ty                  -- Map with Text keys
  | TRecord [(Symbol, Ty)]   -- Record with named fields
  deriving (Eq, Show)

-- | Type equality witness
data (:~:) (a :: k) (b :: k) where
  Refl :: a :~: a

-- | Singleton type for runtime type information
data STy (t :: Ty) where
  STBool   :: STy 'TBool
  STInt    :: STy 'TInt
  STDec    :: KnownNat n => Proxy n -> STy ('TDec n)
  STQty    :: KnownSymbol u => Proxy u -> STy ('TQty u)
  STText   :: STy 'TText
  STDate   :: STy 'TDate
  STOpt    :: STy t -> STy ('TOpt t)
  STList   :: STy t -> STy ('TList t)
  STMap    :: STy t -> STy ('TMap t)
  STRecord :: SRecordFields fs -> STy ('TRecord fs)
```

## Untyped AST (Parser Output)

```haskell
module BPC.Core.Rules.AST where

-- | Field path like "battery.capacity_kwh"
newtype FieldPath = FieldPath { unFieldPath :: [Text] }
  deriving (Eq, Ord, Show)

-- | Identifier (variable/function name)
newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Eq, Ord, Show)

-- | Source position for error reporting
data SourcePos = SourcePos
  { posLine :: Int
  , posColumn :: Int
  , posFile :: Maybe FilePath
  }
  deriving (Eq, Show)

-- | Literal values
data Literal
  = LBool Bool
  | LInt Integer
  | LDec Text         -- Parsed as string, validated later
  | LQty Text Text    -- value, unit
  | LText Text
  | LDate Text        -- YYYY-MM-DD format
  | LNone
  deriving (Eq, Show)

-- | Binary operators
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv
  | OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe
  | OpAnd | OpOr
  deriving (Eq, Show)

-- | Unary operators
data UnaryOp = OpNot
  deriving (Eq, Show)

-- | Untyped expression (direct from parser)
data UntypedExpr
  = ULit Literal SourcePos
  | UVar Identifier SourcePos
  | UBinOp BinOp UntypedExpr UntypedExpr SourcePos
  | UUnaryOp UnaryOp UntypedExpr SourcePos
  | UApp Identifier [UntypedExpr] SourcePos
  | ULet Identifier UntypedExpr UntypedExpr SourcePos
  | UIf UntypedExpr UntypedExpr UntypedExpr SourcePos
  | UAssert UntypedExpr Text Text UntypedExpr SourcePos
  deriving (Eq, Show)

-- | Type expression in source
data TypeExpr
  = TyPrim Text                  -- Bool, Int, Text, Date
  | TyDec Int                    -- Dec(6)
  | TyQty Text                   -- Qty(kg)
  | TyOpt TypeExpr               -- Opt(T)
  | TyList TypeExpr              -- List(T)
  | TyMap TypeExpr               -- Map(Text, T)
  | TyRecord [(Text, TypeExpr)]  -- Record(field: Type, ...)
  deriving (Eq, Show)

-- | Field declaration
data FieldDecl = FieldDecl
  { fdPath :: FieldPath
  , fdType :: TypeExpr
  , fdExpr :: UntypedExpr
  , fdPos  :: SourcePos
  }
  deriving (Eq, Show)
```

## Typed AST (GADT-Based)

```haskell
{-# LANGUAGE GADTs, DataKinds, TypeFamilies, PolyKinds #-}
module BPC.Core.Rules.AST where

-- | Typed expression (GADT)
data Expr (t :: Ty) where
  -- Literals
  ELitBool   :: Bool -> Expr 'TBool
  ELitInt    :: Integer -> Expr 'TInt
  ELitDec    :: forall n. KnownNat n => Dec n -> Expr ('TDec n)
  ELitQty    :: forall u. KnownSymbol u => Qty u -> Expr ('TQty u)
  ELitText   :: Text -> Expr 'TText
  ELitDate   :: Day -> Expr 'TDate
  ELitNone   :: STy t -> Expr ('TOpt t)
  ELitSome   :: Expr t -> Expr ('TOpt t)
  ELitList   :: [Expr t] -> Expr ('TList t)

  -- Variables
  EVar       :: Identifier -> STy t -> Expr t

  -- Operators
  EBinOp     :: SBinOp a b c -> Expr a -> Expr b -> Expr c
  EUnaryOp   :: SUnaryOp a b -> Expr a -> Expr b

  -- Control flow
  ELet       :: Identifier -> Expr a -> Expr b -> Expr b
  EIf        :: Expr 'TBool -> Expr t -> Expr t -> Expr t
  EAssert    :: Expr 'TBool -> Text -> Text -> Expr t -> Expr t

  -- Built-in function application
  EApp       :: SBuiltin args ret -> HList Expr args -> Expr ret

  -- Field reference
  EField     :: FieldPath -> STy t -> Expr t

-- | Heterogeneous list for function arguments
data HList (f :: k -> Type) (ts :: [k]) where
  HNil  :: HList f '[]
  HCons :: f t -> HList f ts -> HList f (t ': ts)

-- | Existential wrapper for unknown type
data SomeExpr where
  SomeExpr :: STy t -> Expr t -> SomeExpr
```

## Built-in Functions

```haskell
module BPC.Core.Rules.Builtins where

-- | Built-in function enumeration
data Builtin
  = BGetFact          -- (Text, Text) -> Opt(Record)
  | BGetFactsByPrefix -- (Text, Text) -> List(Record)
  | BField            -- (Text) -> any
  | BRecordGet        -- (Record, Text) -> Opt(any)
  | BIsSome           -- Opt(a) -> Bool
  | BUnwrapOr         -- (Opt(a), a) -> a
  | BRequireSome      -- (Opt(a), Text, Text) -> a
  | BToDec            -- (Int, Int|Dec) -> Dec(scale)
  | BToQty            -- (Text, Dec|Int) -> Qty(unit)
  | BConvert          -- (Text, Text, Qty(u)) -> Qty(v)
  | BSumQty           -- (Text, List(Qty(u))) -> Qty(u)
  | BSumDec           -- (Int, List(Dec(_))) -> Dec(scale)
  | BMap              -- (List(a), (a->b)) -> List(b)
  | BFilter           -- (List(a), (a->Bool)) -> List(a)
  | BFold             -- (List(a), b, (b,a->b)) -> b
  | BEmitCompliance   -- (Text, Text, Text) -> Bool
  deriving (Eq, Show, Enum, Bounded)

-- | Type signature for built-in
data BuiltinSig = BuiltinSig
  { bsArgs :: [TypeExpr]
  , bsRet  :: TypeExpr
  }

builtinSignature :: Builtin -> BuiltinSig
builtinSignature = \case
  BGetFact          -> BuiltinSig [TyPrim "Text", TyPrim "Text"] (TyOpt (TyPrim "Record"))
  BGetFactsByPrefix -> BuiltinSig [TyPrim "Text", TyPrim "Text"] (TyList (TyPrim "Record"))
  BField            -> BuiltinSig [TyPrim "Text"] (TyPrim "any")  -- Polymorphic
  BRecordGet        -> BuiltinSig [TyPrim "Record", TyPrim "Text"] (TyOpt (TyPrim "any"))
  BIsSome           -> BuiltinSig [TyOpt (TyPrim "any")] (TyPrim "Bool")
  BUnwrapOr         -> BuiltinSig [TyOpt (TyPrim "a"), TyPrim "a"] (TyPrim "a")
  BRequireSome      -> BuiltinSig [TyOpt (TyPrim "a"), TyPrim "Text", TyPrim "Text"] (TyPrim "a")
  BToDec            -> BuiltinSig [TyPrim "Int", TyPrim "any"] (TyDec 0)  -- Scale from first arg
  BToQty            -> BuiltinSig [TyPrim "Text", TyPrim "any"] (TyQty "any")  -- Unit from first arg
  BConvert          -> BuiltinSig [TyPrim "Text", TyPrim "Text", TyQty "any"] (TyQty "any")
  BSumQty           -> BuiltinSig [TyPrim "Text", TyList (TyQty "any")] (TyQty "any")
  BSumDec           -> BuiltinSig [TyPrim "Int", TyList (TyDec 0)] (TyDec 0)
  BMap              -> BuiltinSig [TyList (TyPrim "a"), TyPrim "a -> b"] (TyList (TyPrim "b"))
  BFilter           -> BuiltinSig [TyList (TyPrim "a"), TyPrim "a -> Bool"] (TyList (TyPrim "a"))
  BFold             -> BuiltinSig [TyList (TyPrim "a"), TyPrim "b", TyPrim "(b, a) -> b"] (TyPrim "b")
  BEmitCompliance   -> BuiltinSig [TyPrim "Text", TyPrim "Text", TyPrim "Text"] (TyPrim "Bool")
```

## Test Definitions

```haskell
module BPC.Core.Rules.Tests where

-- | Fact fixture for example tests
data FactFixture = FactFixture
  { ffName :: Identifier
  , ffType :: Text
  , ffKey  :: Text
  , ffValue :: Value
  }
  deriving (Eq, Show)

-- | Assertion in example test
data Assertion = Assertion
  { aField :: FieldPath
  , aOp    :: Comparator
  , aValue :: UntypedExpr
  }
  deriving (Eq, Show)

data Comparator = CmpEq | CmpNeq | CmpLt | CmpLe | CmpGt | CmpGe
  deriving (Eq, Show)

-- | Example test definition
data ExampleTest = ExampleTest
  { etName       :: Identifier
  , etFixtures   :: [FactFixture]
  , etAssertions :: [Assertion]
  , etPos        :: SourcePos
  }
  deriving (Eq, Show)

-- | Property test definition
data PropertyTest = PropertyTest
  { ptName        :: Identifier
  , ptCases       :: Int
  , ptSeed        :: Int
  , ptQuantifiers :: [(Identifier, TypeExpr)]
  , ptImplication :: (UntypedExpr, UntypedExpr)  -- precondition, property
  , ptPos         :: SourcePos
  }
  deriving (Eq, Show)
```

## Declaration Types

```haskell
-- | Top-level declaration
data Declaration
  = DeclField FieldDecl
  | DeclExample ExampleTest
  | DeclProperty PropertyTest
  deriving (Eq, Show)

-- | Parsed source file
data SourceFile = SourceFile
  { sfFields     :: [FieldDecl]
  , sfExamples   :: [ExampleTest]
  , sfProperties :: [PropertyTest]
  }
  deriving (Eq, Show)
```

## Error Types

```haskell
-- | Parse error
data ParseError = ParseError
  { peMessage  :: Text
  , pePosition :: SourcePos
  }
  deriving (Eq, Show)

-- | Type checking error
data TypecheckError
  = TypeMismatch
    { tmExpected :: TypeExpr
    , tmActual   :: TypeExpr
    , tmPos      :: SourcePos
    }
  | UnitMismatch
    { umUnit1 :: Text
    , umUnit2 :: Text
    , umPos   :: SourcePos
    }
  | UnknownIdentifier
    { uiName :: Identifier
    , uiPos  :: SourcePos
    }
  | UnknownField
    { ufPath :: FieldPath
    , ufPos  :: SourcePos
    }
  | ArityMismatch
    { amFunction :: Text
    , amExpected :: Int
    , amActual   :: Int
    , amPos      :: SourcePos
    }
  deriving (Eq, Show)

-- | Cycle detection error
data CycleError = CycleError
  { ceFields :: [FieldPath]
  }
  deriving (Eq, Show)

-- | Evaluation error
data EvalError
  = DivisionByZero SourcePos
  | RequireSomeFailed Text Text SourcePos
  | UnitConversionFailed Text Text SourcePos
  | FieldNotComputed FieldPath SourcePos
  deriving (Eq, Show)
```

## Operator Definitions

```haskell
-- | Binary operator semantics
data SBinOp (a :: Ty) (b :: Ty) (c :: Ty) where
  -- Arithmetic (same type)
  SOpAddInt :: SBinOp 'TInt 'TInt 'TInt
  SOpSubInt :: SBinOp 'TInt 'TInt 'TInt
  SOpMulInt :: SBinOp 'TInt 'TInt 'TInt
  SOpDivInt :: SBinOp 'TInt 'TInt 'TInt

  SOpAddDec :: KnownNat n => Proxy n -> SBinOp ('TDec n) ('TDec n) ('TDec n)
  SOpSubDec :: KnownNat n => Proxy n -> SBinOp ('TDec n) ('TDec n) ('TDec n)

  SOpAddQty :: KnownSymbol u => Proxy u -> SBinOp ('TQty u) ('TQty u) ('TQty u)
  SOpSubQty :: KnownSymbol u => Proxy u -> SBinOp ('TQty u) ('TQty u) ('TQty u)

  -- Qty * Dec = Qty
  SOpMulQtyDec :: (KnownSymbol u, KnownNat n) => Proxy u -> Proxy n -> SBinOp ('TQty u) ('TDec n) ('TQty u)
  SOpDivQtyDec :: (KnownSymbol u, KnownNat n) => Proxy u -> Proxy n -> SBinOp ('TQty u) ('TDec n) ('TQty u)

  -- Comparison
  SOpEq  :: SBinOp t t 'TBool
  SOpNeq :: SBinOp t t 'TBool
  SOpLt  :: SBinOp t t 'TBool
  SOpLe  :: SBinOp t t 'TBool
  SOpGt  :: SBinOp t t 'TBool
  SOpGe  :: SBinOp t t 'TBool

  -- Logical
  SOpAnd :: SBinOp 'TBool 'TBool 'TBool
  SOpOr  :: SBinOp 'TBool 'TBool 'TBool

-- | Unary operator semantics
data SUnaryOp (a :: Ty) (b :: Ty) where
  SOpNot :: SUnaryOp 'TBool 'TBool
  SOpNeg :: KnownNat n => Proxy n -> SUnaryOp ('TDec n) ('TDec n)
```
