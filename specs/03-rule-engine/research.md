# Research: Rule Engine (DSL)

**Feature**: 03-rule-engine
**Date**: 2025-12-28
**Status**: Complete

## Research Summary

The Rule Engine implements a domain-specific language for defining passport field computations. This research covers parser technology, type system design with GADTs, and evaluation strategies.

---

## 1. Parser Library Choice

**Decision**: Megaparsec (not Parsec)

**Rationale**:
- Better error messages with source positions
- More modern API
- Good performance for our DSL size
- Active maintenance

**Alternatives Considered**:
- Parsec: Older, less informative errors
- attoparsec: Optimized for binary/streaming, not DSL
- alex/happy: Overkill for our grammar size

**Implementation Pattern**:
```haskell
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Lexer with whitespace/comment handling
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
```

---

## 2. GADT-Based Type System

**Decision**: Use GADTs for type-safe AST

**Rationale**:
- Compile-time type safety (Constitution VI)
- Impossible to construct ill-typed expressions
- Type information preserved through evaluation

**Implementation Pattern**:
```haskell
{-# LANGUAGE GADTs, DataKinds, TypeFamilies, PolyKinds #-}

-- Type universe (promoted data kinds)
data Ty
  = TBool
  | TInt
  | TDec Nat
  | TQty Symbol
  | TText
  | TDate
  | TOpt Ty
  | TList Ty
  | TRecord [(Symbol, Ty)]

-- Singleton for type reification
data STy (t :: Ty) where
  STBool   :: STy 'TBool
  STInt    :: STy 'TInt
  STDec    :: KnownNat n => Proxy n -> STy ('TDec n)
  STQty    :: KnownSymbol u => Proxy u -> STy ('TQty u)
  -- ...

-- Type-indexed expression
data Expr (t :: Ty) where
  ELitBool :: Bool -> Expr 'TBool
  EIf      :: Expr 'TBool -> Expr t -> Expr t -> Expr t
  -- ...
```

**Key Insight**: GADTs allow the type parameter to vary based on constructor, enabling type-safe operations.

---

## 3. Two-Phase Compilation

**Decision**: Untyped AST → Typecheck → Typed AST

**Rationale**:
- Parser is simpler without type tracking
- Better error messages (separate parse vs type errors)
- Standard compiler pattern

**Flow**:
```
Source Text
    ↓ (parse)
Untyped AST
    ↓ (typecheck)
Typed AST (GADTs)
    ↓ (eval)
Values
```

**Existential Wrapper for Typed AST**:
```haskell
-- Result of typechecking (unknown type)
data SomeExpr where
  SomeExpr :: STy t -> Expr t -> SomeExpr

-- Typecheck returns existential
typecheck :: TypeEnv -> UntypedExpr -> Either TypecheckError SomeExpr
```

---

## 4. Built-in Function Typing

**Decision**: Type-indexed built-in definitions

**Rationale**:
- Each built-in has known argument/return types
- Type checking verifies application correctness
- Evaluation is type-directed

**Pattern**:
```haskell
-- Built-in function with type-level signature
data BuiltinFn where
  -- getFact(type, key) -> Opt(Record)
  BGetFact :: BuiltinFn

-- Type signature lookup
builtinType :: BuiltinFn -> ([SomeTy], SomeTy)
builtinType BGetFact = ([SomeTy STText, SomeTy STText], SomeTy (STOpt STRecord))

-- Evaluation
evalBuiltin :: BuiltinFn -> [SomeValue] -> Either EvalError SomeValue
evalBuiltin BGetFact [VText factType, VText factKey] = ...
```

---

## 5. Dependency Graph

**Decision**: Kahn's algorithm with lexical tie-break

**Rationale**:
- SSOT 7.5 specifies Kahn's algorithm
- Lexical tie-break ensures determinism
- Cycle detection is built-in

**Implementation**:
```haskell
topoSort :: Map FieldPath (Set FieldPath) -> Either CycleError [FieldPath]
topoSort graph = go initQueue initDegrees []
  where
    initDegrees = Map.map Set.size (transposeGraph graph)
    initQueue = sortBy lexical [n | (n, d) <- Map.toList initDegrees, d == 0]

    go [] degrees result
      | all (== 0) (Map.elems degrees) = Right (reverse result)
      | otherwise = Left $ CycleError (Map.keys $ Map.filter (> 0) degrees)
    go (n:ns) degrees result = go queue' degrees' (n:result)
      where
        neighbors = Map.findWithDefault Set.empty n graph
        degrees' = foldr (Map.adjust pred) degrees (Set.toList neighbors)
        newZeros = [m | m <- Set.toList neighbors, Map.findWithDefault 0 m degrees' == 0]
        queue' = foldr insertSorted ns newZeros
```

---

## 6. Unit System

**Decision**: Type-level units with runtime conversion

**Rationale**:
- Type-level units prevent compile-time mistakes
- Runtime conversion handles dynamic unit expressions
- Limited unit set (9 units) is manageable

**Type-Level Approach**:
```haskell
-- Type-level: Qty "kg" cannot be added to Qty "kWh"
addQty :: Qty u -> Qty u -> Qty u

-- Runtime: for dynamic expressions
data DynQty = DynQty Unit (Dec 6)
addDynQty :: DynQty -> DynQty -> Either UnitMismatch DynQty
```

**Unit Families** (for conversion):
```haskell
data UnitFamily = Mass | Energy | Emissions | Other

unitFamily :: Unit -> UnitFamily
unitFamily Kg = Mass
unitFamily G = Mass
unitFamily KWh = Energy
unitFamily Wh = Energy
-- ...

-- Can only convert within same family
canConvert :: Unit -> Unit -> Bool
canConvert u v = unitFamily u == unitFamily v
```

---

## 7. Error Recovery Strategy

**Decision**: Fail-fast with detailed errors

**Rationale**:
- DSL is typically short (< 1000 lines)
- Better to report first error clearly
- Accumulated errors can be confusing

**Error Types**:
```haskell
data RuleError
  = ParseError ParseErrorBundle
  | TypecheckError TypecheckError
  | CycleError [FieldPath]
  | EvalError EvalError

data TypecheckError
  = TypeMismatch SomeTy SomeTy SourcePos
  | UnitMismatch Unit Unit SourcePos
  | UnknownIdentifier Text SourcePos
  | UnknownField FieldPath SourcePos
  | ArityMismatch Text Int Int SourcePos

data EvalError
  = DivisionByZero SourcePos
  | RequireSomeFailed Text Text SourcePos
  | FieldNotComputed FieldPath SourcePos
```

---

## 8. Test Framework Design

**Decision**: Pure test runner, separate from evaluation

**Rationale**:
- Tests are defined in DSL alongside rules
- Example tests use fixed fixtures
- Property tests use seeded RNG

**Example Test Structure**:
```haskell
data ExampleTest = ExampleTest
  { testName :: Text
  , fixtures :: Map (Text, Text) Value  -- fact_type, fact_key -> record
  , assertions :: [(FieldPath, Comparator, Value)]
  }

runExample :: Map FieldPath (Expr t) -> ExampleTest -> Either EvalError TestResult
```

**Property Test Structure**:
```haskell
data PropertyTest = PropertyTest
  { propName :: Text
  , cases :: Int
  , seed :: Int
  , quantifiers :: [(Text, TypeExpr)]  -- variable bindings
  , implication :: (UntypedExpr, UntypedExpr)  -- precondition, property
  }

runProperty :: Map FieldPath (Expr t) -> PropertyTest -> Either EvalError [TestResult]
```

---

## 9. Performance Considerations

**Decision**: Lazy evaluation with memoization

**Rationale**:
- Haskell is lazy by default
- Field values may be referenced multiple times
- Memoization prevents redundant computation

**Pattern**:
```haskell
evalFields :: Map FieldPath (Expr t) -> EvalContext -> Either EvalError (Map FieldPath Value)
evalFields exprs ctx = fmap (Map.fromList) $ sequence
  [ (,) path <$> evalExpr (ctx { fields = memoized }) expr
  | (path, expr) <- topoSorted exprs
  ]
  where
    -- Lazily build memoization map
    memoized = ...
```

---

## Open Questions

None - SSOT fully specifies the DSL.

## Implementation Risks

1. **GADT complexity**: Team needs familiarity with advanced Haskell
2. **Error message quality**: Megaparsec helps but requires careful setup
3. **Performance edge cases**: Very long list operations could be slow

## Next Steps

Proceed to implementation following plan.md phases.
