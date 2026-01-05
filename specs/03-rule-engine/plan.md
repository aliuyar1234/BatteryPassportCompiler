# Implementation Plan: Rule Engine (DSL)

**Branch**: `03-rule-engine` | **Date**: 2025-12-28 | **Spec**: [.specify/features/03-rule-engine/spec.md](../../.specify/features/03-rule-engine/spec.md)
**Input**: Feature specification from `.specify/features/03-rule-engine/spec.md`
**Phase**: P1 | **Package**: bpc-core | **Status**: Planning

## Summary

Implement the complete Rule DSL (BPC-RULES-1) including:
- **Parser**: Megaparsec-based parser for complete EBNF grammar (SSOT 8.1)
- **Type System**: GADT-based typed AST for compile-time type safety
- **Typechecker**: Transform untyped AST to typed AST with comprehensive error reporting
- **Dependency Graph**: Topological sort using Kahn's algorithm with lexical tie-break (SSOT 7.5)
- **Built-ins**: All 15 built-in functions (SSOT 8.2)
- **Evaluator**: Pure expression evaluation with memoization
- **Test Runner**: Example and property test execution

The Rule Engine is the brain of the compiler - it defines how Facts become Passport fields through type-safe, deterministic computation.

## Technical Context

**Language/Version**: Haskell GHC 9.6.4 (GADTs, DataKinds, TypeFamilies required)
**Build System**: Cabal 3.10.2.1
**Package**: bpc-core (pure, IO-free)
**Primary Dependencies**: megaparsec >= 9.3, containers >= 0.6, mtl >= 2.3, transformers >= 0.6
**Storage**: N/A (pure functions)
**Testing**: tasty, tasty-quickcheck, tasty-hunit, tasty-golden
**Target Platform**: Cross-platform (pure Haskell)
**Performance Goals**: 10,000 lines DSL in < 1 second
**Constraints**: NO IO imports; pure evaluation; GADT type safety
**Scale/Scope**: ~2500 LOC, 8 modules, complete EBNF implementation, 15 built-ins

## Constitution Check

*GATE: All checks passed*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Determinism | ENFORCED | Pure evaluation, no IO, no randomness |
| II. Canonical Storage | N/A | DSL is source, not storage |
| III. Immutability | ENFORCED | AST is immutable after parse |
| IV. Audit Trail | N/A | No persistence in core |
| V. Layered Architecture | ENFORCED | bpc-core is IO-free, no DB/HTTP/Queue imports |
| VI. Type-Safe Rules | CRITICAL | GADTs ensure type safety at compile time, runtime type errors are design failures |

**Quality Gates:**
- Coverage Target: >= 90% for BPC.Core.Rules.*
- Property Tests: Parse → Typecheck roundtrip; Determinism; Topo sort correctness
- All 15 built-ins have unit tests with comprehensive edge cases
- SSOT 8.4 example DSL parses and typechecks successfully
- Golden tests for parser output, typecheck errors, evaluation results
- Formatting: fourmolu compliance
- Linting: hlint compliance with zero warnings

## Project Structure

### Documentation (this feature)

```text
specs/03-rule-engine/
├── plan.md              # This file (implementation plan)
├── research.md          # Parser/GADT research, algorithm analysis
├── data-model.md        # AST type definitions, EBNF reference
├── quickstart.md        # Usage guide with examples
└── contracts/           # N/A for pure library
```

### Source Code

```text
packages/bpc-core/
├── src/
│   └── BPC/
│       └── Core/
│           └── Rules/
│               ├── AST.hs          # Untyped + Typed AST (GADTs)
│               ├── Parser.hs       # DSL parser (Megaparsec)
│               ├── Lexer.hs        # Token definitions and lexer combinators
│               ├── Typecheck.hs    # Untyped → Typed AST transformation
│               ├── Graph.hs        # Dependency graph + TopoSort
│               ├── Builtins.hs     # 15 built-in function definitions
│               ├── Eval.hs         # Pure expression evaluator
│               └── Tests.hs        # Example/Property test runner
└── test/
    ├── BPC/
    │   └── Core/
    │       └── Rules/
    │           ├── ParserSpec.hs      # Parser unit tests
    │           ├── TypecheckSpec.hs   # Typecheck unit tests
    │           ├── GraphSpec.hs       # TopoSort property tests
    │           ├── BuiltinsSpec.hs    # Built-in function tests
    │           ├── EvalSpec.hs        # Evaluator tests
    │           └── TestsSpec.hs       # Test runner tests
    └── fixtures/
        └── dsl/
            ├── minimal.dsl         # SSOT 8.4 example
            ├── example.dsl         # Complex example
            ├── errors/
            │   ├── parse-error.dsl
            │   ├── type-error.dsl
            │   └── cycle.dsl
            └── golden/
                ├── parser-output/
                ├── typecheck-errors/
                └── eval-results/
```

**Structure Decision**: Single package (bpc-core) with submodules under BPC.Core.Rules. No separate package needed as this is pure domain logic with no external dependencies.

## Implementation Phases

### Phase 0: Foundation (Prerequisites)

**Goal**: Ensure project infrastructure is ready

**Dependencies**:
- Phase must complete AFTER 01-foundation (Cabal setup)
- Phase must complete AFTER 02-core-primitives (Dec, Qty types)

**Tasks**:
- [ ] Verify bpc-core.cabal has required extensions enabled (GADTs, DataKinds, TypeFamilies, PolyKinds)
- [ ] Add megaparsec, containers, mtl dependencies to bpc-core.cabal
- [ ] Create BPC.Core.Rules module hierarchy
- [ ] Set up test infrastructure with tasty

**Verification**: `cabal build bpc-core` succeeds

---

### Phase 1: Lexer & Token Definitions

**Goal**: Define all tokens and lexer combinators for the DSL

**Files**: `BPC/Core/Rules/Lexer.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Lexer where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Whitespace and comment handling
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- Keywords
keywords :: Set Text
keywords = Set.fromList
  [ "field", "let", "if", "then", "else", "assert"
  , "example", "property", "fact", "expect"
  , "forall", "implies", "cases", "seed"
  , "true", "false", "none"
  , "Bool", "Int", "Text", "Date", "Dec", "Qty", "Opt", "List", "Map", "Record"
  , "date", "qty"
  ]

-- Identifier (but not keyword)
identifier :: Parser Text
identifier = lexeme $ try $ do
  ident <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
  guard (ident `notElem` keywords)
  return (pack ident)

-- Operators
reserved :: Text -> Parser ()
reserved w = lexeme $ string w *> notFollowedBy alphaNumChar

-- Literals
stringLiteral :: Parser Text
intLiteral :: Parser Integer
decLiteral :: Parser Text
```

**Tests**:
- [ ] Identifier parsing (valid/invalid)
- [ ] Comment handling (single line)
- [ ] Keyword recognition
- [ ] Operator parsing
- [ ] String escape sequences

**Verification**: All lexer tests pass

---

### Phase 2: Untyped AST & Parser (EBNF from SSOT 8.1)

**Goal**: Parse DSL source to untyped AST following complete EBNF

**Files**: `BPC/Core/Rules/AST.hs`, `BPC/Core/Rules/Parser.hs`

**Implementation**:

```haskell
-- AST.hs
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

data FieldDecl = FieldDecl
  { fdPath :: FieldPath
  , fdType :: TypeExpr
  , fdExpr :: UntypedExpr
  , fdPos  :: SourcePos
  }

data Declaration
  = DeclField FieldDecl
  | DeclExample ExampleTest
  | DeclProperty PropertyTest
  deriving (Eq, Show)

-- Parser.hs
parseSource :: Text -> Either ParseError [Declaration]
parseSource = parse (sc *> many declaration <* eof) "<input>"

declaration :: Parser Declaration
fieldDecl :: Parser FieldDecl
exampleTest :: Parser ExampleTest
propertyTest :: Parser PropertyTest

expr :: Parser UntypedExpr
letExpr :: Parser UntypedExpr
ifExpr :: Parser UntypedExpr
assertExpr :: Parser UntypedExpr
logicExpr :: Parser UntypedExpr
-- ... operator precedence: || > && > cmp > + - > * /
```

**EBNF Productions to Implement** (from SSOT 8.1):
- [x] source, rule, test
- [x] exampleTest, propertyTest
- [x] factFixtures, assertions
- [x] fieldPath, typeExpr, primType, unitExpr
- [x] expr, letExpr, ifExpr, assertExpr, logicExpr
- [x] orExpr, andExpr, cmpExpr, addExpr, mulExpr, unaryExpr
- [x] primary, funcCall, literal
- [x] boolLit, intLit, decLit, dateLit, qtyLit, stringLit
- [x] jsonObject, jsonPair, jsonValue, jsonArray

**Tests**:
- [ ] Parse SSOT 8.4 example successfully
- [ ] Parse field declaration with all type expressions
- [ ] Parse let bindings (nested)
- [ ] Parse if/then/else
- [ ] Parse assert expressions
- [ ] Parse operator precedence correctly (property test)
- [ ] Parse example test with fixtures
- [ ] Parse property test with quantifiers
- [ ] Parse error reporting with line/column
- [ ] Reject invalid syntax with clear error messages

**Verification**:
- SSOT 8.4 example parses without errors
- Parser handles all EBNF productions
- Error messages include source positions

---

### Phase 3: Typed AST (GADT)

**Goal**: Define type-safe AST with GADTs

**Files**: `BPC/Core/Rules/AST.hs` (extended)

**Implementation**:

```haskell
{-# LANGUAGE GADTs, DataKinds, TypeFamilies, PolyKinds #-}

-- Type universe (promoted to kind)
data Ty
  = TBool
  | TInt
  | TDec Nat
  | TQty Symbol
  | TText
  | TDate
  | TOpt Ty
  | TList Ty
  | TMap Ty
  | TRecord [(Symbol, Ty)]

-- Singleton type for runtime type information
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

-- Typed expression (GADT)
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

-- Heterogeneous list for function arguments
data HList (f :: k -> Type) (ts :: [k]) where
  HNil  :: HList f '[]
  HCons :: f t -> HList f ts -> HList f (t ': ts)

-- Existential wrapper for unknown type
data SomeExpr where
  SomeExpr :: STy t -> Expr t -> SomeExpr
```

**Tests**:
- [ ] GADT constructors type-check correctly
- [ ] Singleton types reify runtime information
- [ ] HList construction and pattern matching

**Verification**: Code compiles with -Wall -Werror

---

### Phase 4: Typechecker

**Goal**: Transform untyped AST to typed AST with comprehensive error handling

**Files**: `BPC/Core/Rules/Typecheck.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Typecheck where

-- Type environment
type TypeEnv = Map Identifier SomeType
type FieldEnv = Map FieldPath TypeExpr

-- Typecheck error
data TypecheckError
  = TypeMismatch { expected :: SomeType, actual :: SomeType, location :: SourcePos }
  | UnitMismatch { unit1 :: Unit, unit2 :: Unit, location :: SourcePos }
  | UnknownIdentifier { name :: Identifier, location :: SourcePos }
  | UnknownField { path :: FieldPath, location :: SourcePos }
  | ArityMismatch { function :: Text, expected :: Int, actual :: Int, location :: SourcePos }
  deriving (Eq, Show)

-- Main typecheck function
typecheck :: FieldEnv -> TypeEnv -> UntypedExpr -> Either TypecheckError SomeExpr
typecheck fieldEnv env = go
  where
    go (ULit lit pos) = typecheckLit lit pos
    go (UVar ident pos) = lookupVar env ident pos
    go (UBinOp op e1 e2 pos) = typecheckBinOp op e1 e2 pos
    go (UApp fn args pos) = typecheckApp fieldEnv env fn args pos
    -- ...

-- Typecheck entire source file
typecheckSource :: [Declaration] -> Either TypecheckError (Map FieldPath SomeExpr)
```

**Type Rules** (SSOT 8.3):
- `+/-`: Only same Unit/Scale allowed
  - `Qty u + Qty u -> Qty u`
  - `Dec n + Dec n -> Dec n`
- `*`: Qty × Dec → Qty
  - `Qty u * Dec n -> Qty u`
- `/`: Qty ÷ Dec → Qty
  - `Qty u / Dec n -> Qty u`
- Comparisons: Only compatible types
  - `Qty u == Qty u -> Bool`
  - `Dec n < Dec n -> Bool`
- Unknown field refs → RULE_TYPE_ERROR
- Unknown units → UNIT_MISMATCH

**Tests**:
- [ ] Typecheck SSOT 8.4 example successfully
- [ ] Detect type mismatch: Dec(6) + Dec(2)
- [ ] Detect unit mismatch: Qty("kg") + Qty("kWh")
- [ ] Detect unknown field reference
- [ ] Detect unknown unit in toQty
- [ ] Detect arity mismatch in function calls
- [ ] Verify operator type rules (property tests)
- [ ] Golden tests for error messages

**Verification**:
- SSOT 8.4 example typechecks successfully
- All error cases produce clear, actionable messages

---

### Phase 5: Dependency Graph & Topological Sort

**Goal**: Build field dependency graph and sort fields using Kahn's algorithm

**Files**: `BPC/Core/Rules/Graph.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Graph where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Build dependency graph from field expressions
buildGraph :: Map FieldPath UntypedExpr -> Map FieldPath (Set FieldPath)
buildGraph fields = Map.map extractDeps fields
  where
    extractDeps :: UntypedExpr -> Set FieldPath
    extractDeps = go
      where
        go (UApp "field" [ULit (LText path) _] _) = Set.singleton (FieldPath (splitOn "." path))
        go (ULet _ e1 e2 _) = go e1 <> go e2
        go (UIf e1 e2 e3 _) = go e1 <> go e2 <> go e3
        go (UBinOp _ e1 e2 _) = go e1 <> go e2
        go (UApp _ args _) = foldMap go args
        go _ = Set.empty

-- Topological sort with Kahn's algorithm + lexical tie-break (SSOT 7.5)
topoSortFields :: Map FieldPath (Set FieldPath) -> Either CycleError [FieldPath]
topoSortFields graph = go initQueue initDegrees []
  where
    -- Calculate in-degrees
    allNodes = Map.keysSet graph
    initDegrees = Map.fromSet (\n -> Set.size (incomingEdges n)) allNodes
    incomingEdges node = Set.fromList [src | (src, deps) <- Map.toList graph, node `Set.member` deps]

    -- Initial queue: nodes with in-degree 0, sorted lexically
    initQueue = sortBy compareLexical [n | (n, d) <- Map.toList initDegrees, d == 0]

    go [] degrees result
      | all (== 0) (Map.elems degrees) = Right (reverse result)
      | otherwise = Left $ CycleError (Map.keys $ Map.filter (> 0) degrees)

    go (n:ns) degrees result = go queue' degrees' (n:result)
      where
        neighbors = Map.findWithDefault Set.empty n graph
        degrees' = foldr (Map.adjust (subtract 1)) degrees (Set.toList neighbors)
        newZeros = filter (\m -> Map.findWithDefault 0 m degrees' == 0) (Set.toList neighbors)
        queue' = mergeSorted ns newZeros  -- Maintain lexical order

compareLexical :: FieldPath -> FieldPath -> Ordering
compareLexical (FieldPath p1) (FieldPath p2) = compare p1 p2

data CycleError = CycleError
  { cycleFields :: [FieldPath]
  }
  deriving (Eq, Show)
```

**Tests**:
- [ ] Property test: After topoSort, all dependencies come before dependents
- [ ] Detect cycle: A → B → A
- [ ] Detect complex cycle: A → B → C → D → B
- [ ] Lexical tie-break: Independent fields sorted alphabetically
- [ ] Empty graph returns empty list
- [ ] Single node with no dependencies

**Verification**:
- Property test: `forall graph. sorted <- topoSortFields graph => all deps come before node`
- Cycle detection correctly identifies all nodes in cycle

---

### Phase 6: Built-in Functions (15 total from SSOT 8.2)

**Goal**: Implement all built-in functions with type signatures

**Files**: `BPC/Core/Rules/Builtins.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Builtins where

-- Built-in function enumeration
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

-- Type signature lookup
builtinSignature :: Builtin -> BuiltinSig
builtinSignature = \case
  BGetFact          -> BuiltinSig [TyPrim "Text", TyPrim "Text"] (TyOpt (TyPrim "Record"))
  BGetFactsByPrefix -> BuiltinSig [TyPrim "Text", TyPrim "Text"] (TyList (TyPrim "Record"))
  -- ... (complete list in data-model.md)

-- Name to builtin lookup
lookupBuiltin :: Text -> Maybe Builtin
lookupBuiltin "getFact" = Just BGetFact
lookupBuiltin "getFactsByPrefix" = Just BGetFactsByPrefix
-- ...
```

**Tests** (one per built-in):
1. [ ] `getFact("Battery", "battery:SKU-123")` returns Opt(Record)
2. [ ] `getFactsByPrefix("BOM", "bom:")` returns List(Record)
3. [ ] `field("battery.capacity_kwh")` resolves field reference
4. [ ] `recordGet(record, "field")` extracts field value
5. [ ] `isSome(none)` returns false, `isSome(some(x))` returns true
6. [ ] `unwrapOr(none, default)` returns default
7. [ ] `requireSome(none, "E001", "msg")` throws error with code
8. [ ] `toDec(6, 50)` converts integer to Dec(6)
9. [ ] `toQty("kg", 100)` creates Qty("kg")
10. [ ] `convert("g", "kg", qty(1000, "g"))` returns qty(1, "kg")
11. [ ] `sumQty("kWh", [qty(1,"kWh"), qty(2,"kWh")])` returns qty(3, "kWh")
12. [ ] `sumDec(2, [dec(1.5), dec(2.5)])` returns dec(4.0)
13. [ ] `map([1,2,3], \x -> x * 2)` returns [2,4,6]
14. [ ] `filter([1,2,3], \x -> x > 1)` returns [2,3]
15. [ ] `fold([1,2,3], 0, \acc x -> acc + x)` returns 6
16. [ ] `emitCompliance("clause", "PASS", "evidence")` returns true

**Edge Cases**:
- [ ] Unit mismatch in convert: kg → kWh (different families)
- [ ] Division by zero in qty arithmetic
- [ ] Empty list in sumQty/sumDec
- [ ] Invalid unit in toQty

**Verification**: All 15 built-ins have comprehensive unit tests

---

### Phase 7: Expression Evaluator

**Goal**: Pure evaluation of typed expressions with memoization

**Files**: `BPC/Core/Rules/Eval.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Eval where

-- Evaluation context
data EvalContext = EvalContext
  { facts :: Map (Text, Text) Value  -- (fact_type, fact_key) -> record
  , fields :: Map FieldPath Value    -- computed field values (memoization)
  }

-- Evaluation errors
data EvalError
  = DivisionByZero SourcePos
  | RequireSomeFailed Text Text SourcePos
  | UnitConversionFailed Text Text SourcePos
  | FieldNotComputed FieldPath SourcePos
  deriving (Eq, Show)

-- Value type (runtime representation)
data Value
  = VBool Bool
  | VInt Integer
  | VDec Int Scientific  -- scale, value
  | VQty Text Scientific  -- unit, value
  | VText Text
  | VDate Day
  | VNone
  | VSome Value
  | VList [Value]
  | VRecord (Map Text Value)
  deriving (Eq, Show)

-- Pure evaluation
evalExpr :: EvalContext -> Expr t -> Either EvalError Value
evalExpr ctx = \case
  ELitBool b -> Right (VBool b)
  ELitInt i -> Right (VInt i)
  EBinOp op e1 e2 -> do
    v1 <- evalExpr ctx e1
    v2 <- evalExpr ctx e2
    evalBinOp op v1 v2
  EApp builtin args -> evalBuiltin ctx builtin args
  EField path _ -> case Map.lookup path (fields ctx) of
    Just v -> Right v
    Nothing -> Left $ FieldNotComputed path noPos
  -- ...

-- Evaluate all fields in topological order
evalFields :: Map FieldPath (Expr t) -> EvalContext -> Either EvalError (Map FieldPath Value)
evalFields exprs ctx = do
  sorted <- topoSortFields (Map.map extractDeps exprs)
  foldM evalOne Map.empty sorted
  where
    evalOne memo path = do
      let ctx' = ctx { fields = memo }
      val <- evalExpr ctx' (exprs Map.! path)
      return (Map.insert path val memo)
```

**Tests**:
- [ ] Evaluate literals
- [ ] Evaluate binary operators (all types)
- [ ] Evaluate let bindings
- [ ] Evaluate if/then/else
- [ ] Evaluate field references (memoization)
- [ ] Division by zero error
- [ ] requireSome failure error
- [ ] Unit conversion error
- [ ] Property test: Determinism (same input → same output)

**Verification**:
- Property test: `forall input. evalFields input ctx == evalFields input ctx` (determinism)
- All evaluator tests pass

---

### Phase 8: Test Runner

**Goal**: Execute example and property tests

**Files**: `BPC/Core/Rules/Tests.hs`

**Implementation**:

```haskell
module BPC.Core.Rules.Tests where

-- Test result
data TestResult
  = Passed
  | Failed
    { expected :: Value
    , actual :: Value
    , location :: SourcePos
    }
  deriving (Eq, Show)

-- Run example test
runExampleTest :: Map FieldPath (Expr t) -> ExampleTest -> Either EvalError TestResult
runExampleTest exprs test = do
  let ctx = EvalContext
        { facts = Map.fromList [(ffType ff, ffKey ff, ffValue ff) | ff <- etFixtures test]
        , fields = Map.empty
        }
  results <- evalFields exprs ctx
  checkAssertions results (etAssertions test)

checkAssertions :: Map FieldPath Value -> [Assertion] -> Either EvalError TestResult
checkAssertions results assertions = case failures of
  [] -> Right Passed
  (f:_) -> Right f
  where
    failures = [checkOne a | a <- assertions, not (checkOne a == Passed)]
    checkOne (Assertion path cmp expected) = ...

-- Run property test with seeded RNG
runPropertyTest :: Map FieldPath (Expr t) -> PropertyTest -> Either EvalError [TestResult]
runPropertyTest exprs test = do
  let gen = mkStdGen (ptSeed test)
  replicateM (ptCases test) (generateAndTest gen)
  where
    generateAndTest gen = ...
```

**Tests**:
- [ ] Example test passes with correct fixtures
- [ ] Example test fails with incorrect expected value
- [ ] Property test generates correct number of cases
- [ ] Property test is reproducible with same seed
- [ ] Property test finds counterexample

**Verification**:
- Example tests execute correctly
- Property tests are reproducible

---

## Dependencies

### Cabal Configuration

Add to `packages/bpc-core/bpc-core.cabal`:

```cabal
library
  exposed-modules:
      BPC.Core.Rules.AST
      BPC.Core.Rules.Parser
      BPC.Core.Rules.Lexer
      BPC.Core.Rules.Typecheck
      BPC.Core.Rules.Graph
      BPC.Core.Rules.Builtins
      BPC.Core.Rules.Eval
      BPC.Core.Rules.Tests

  build-depends:
      base >= 4.16 && < 5
    , text >= 1.2
    , containers >= 0.6
    , megaparsec >= 9.3
    , parser-combinators >= 1.3
    , mtl >= 2.3
    , transformers >= 0.6
    , scientific >= 0.3
    , time >= 1.11
    , bytestring >= 0.11

  default-extensions:
      GADTs
      DataKinds
      TypeFamilies
      PolyKinds
      KindSignatures
      ScopedTypeVariables
      RankNTypes
      StandaloneDeriving
      OverloadedStrings

  ghc-options: -Wall -Wcompat -Werror
```

### External Dependencies

- **megaparsec**: Parser combinator library with excellent error messages
- **containers**: Map, Set for dependency graph
- **mtl**: MonadState, MonadError for typecheck
- **scientific**: Precise decimal arithmetic
- **time**: Day type for dates

---

## Key Algorithms

### 1. Kahn's Topological Sort (SSOT 7.5)

```text
topoSort(graph):
  in_degree = {node: count of incoming edges}
  queue = [nodes with in_degree == 0], sorted lexically
  result = []

  while queue not empty:
    node = queue.pop_front()
    result.append(node)
    for neighbor in graph[node]:
      in_degree[neighbor] -= 1
      if in_degree[neighbor] == 0:
        insert neighbor into queue (maintaining lexical order)

  if len(result) != len(graph):
    return CycleError(remaining nodes)
  return result
```

**Properties**:
- Lexical tie-break ensures determinism
- Cycle detection is built-in
- O(V + E) time complexity

---

### 2. Unit Arithmetic Rules (SSOT 8.3)

| Operation | Left | Right | Result | Error |
|-----------|------|-------|--------|-------|
| + / - | Qty u | Qty u | Qty u | UNIT_MISMATCH if units differ |
| + / - | Dec s | Dec s | Dec s | TYPE_ERROR if scales differ (or auto-scale) |
| * | Qty u | Dec s | Qty u | - |
| / | Qty u | Dec s | Qty u | DIVISION_BY_ZERO if Dec s == 0 |
| Compare | Qty u | Qty u | Bool | UNIT_MISMATCH if units differ |
| Compare | Dec s | Dec s | Bool | TYPE_ERROR if scales differ |

**Unit Families** (for conversion):
- Mass: kg, g
- Energy: kWh, Wh
- Emissions: gCO2e, kgCO2e
- Intensity: gCO2e_per_kWh
- Dimensionless: pct, each

**Conversion Rules**:
- Can only convert within same family
- Conversion factors are deterministic (hardcoded)

---

### 3. Memoization Strategy

Field evaluation uses lazy memoization:

```haskell
evalFields :: Map FieldPath (Expr t) -> EvalContext -> Either EvalError (Map FieldPath Value)
evalFields exprs ctx = foldM evalOne Map.empty sorted
  where
    sorted = topoSortFields (Map.map extractDeps exprs)
    evalOne memo path = do
      let ctx' = ctx { fields = memo }  -- Memoized values available
      val <- evalExpr ctx' (exprs Map.! path)
      return (Map.insert path val memo)
```

**Benefits**:
- Each field evaluated exactly once
- Dependencies resolved via topological order
- Pure (no IORefs needed)

---

## Error Code Reference

All error codes follow SSOT Section 2.4:

| Code | Name | Trigger | Recovery |
|------|------|---------|----------|
| RULE_PARSE_ERROR | Parse syntax error | Invalid DSL syntax | Fix syntax |
| RULE_TYPE_ERROR | Type mismatch | Type incompatibility | Fix types |
| UNIT_MISMATCH | Unit incompatibility | kg + kWh | Use same units |
| RULE_CYCLE_DETECTED | Circular dependency | A → B → A | Remove cycle |
| DIVISION_BY_ZERO | Division by zero | x / 0 | Guard division |
| FIELD_NOT_COMPUTED | Missing field | Reference before eval | Check topo sort |

---

## Verification Checklist

### Functional Requirements

- [ ] FR-001: Parser implements complete EBNF from SSOT 8.1
- [ ] FR-002: Parser ignores `-- comment` lines
- [ ] FR-003: Parser returns RULE_PARSE_ERROR with line/column on syntax errors
- [ ] FR-004: Typecheck transforms untyped AST to typed AST (GADTs) per SSOT 8.3
- [ ] FR-005: Typecheck returns RULE_TYPE_ERROR on type mismatch
- [ ] FR-006: Typecheck returns UNIT_MISMATCH on incompatible units
- [ ] FR-007: topoSortFields uses Kahn's algorithm with lexical tie-break (SSOT 7.5)
- [ ] FR-008: topoSortFields returns RULE_CYCLE_DETECTED with cycle_fields on cycles
- [ ] FR-009: All 15 built-in functions from SSOT 8.2 are implemented
- [ ] FR-010: Example tests require >= 500 cases before publish (enforced in bpc-db layer)
- [ ] FR-011: Property tests are reproducible with same seed

### Quality Gates

- [ ] SSOT 8.4 example DSL parses without errors
- [ ] SSOT 8.4 example typechecks without errors
- [ ] Parser handles all EBNF productions
- [ ] Parser rejects invalid syntax with line/column info
- [ ] Comments are ignored
- [ ] Typecheck detects unit mismatches
- [ ] Typecheck detects unknown field references
- [ ] TopoSort handles complex dependency graphs
- [ ] TopoSort detects cycles and reports all fields in cycle
- [ ] All 15 built-ins have comprehensive unit tests
- [ ] Property test: parse → typecheck or RULE_TYPE_ERROR
- [ ] Property test: evalFields is deterministic
- [ ] Performance: 10K lines in < 1 second
- [ ] NO IO imports in any module
- [ ] Code coverage >= 90%
- [ ] Compiles with -Wall -Wcompat -Werror
- [ ] fourmolu formatting passes
- [ ] hlint linting passes with zero warnings

### Constitution Compliance

- [ ] Determinism: Same inputs produce identical outputs
- [ ] IO-Free: No IO operations in bpc-core
- [ ] Type Safety: GADTs prevent runtime type errors
- [ ] Immutability: AST is immutable after parsing

---

## Complexity Tracking

**No complexity violations** - this implementation follows Constitution principles:

- Pure functions only (no IO)
- GADT type safety enforced at compile time
- Single package structure (no unnecessary splitting)
- Standard Haskell patterns (megaparsec, GADTs, topological sort)

---

## Next Steps

After Phase 8 completion:

1. **Integration with 04-compilation-pipeline**:
   - Use `evalFields` for passport field computation
   - Generate proof nodes during evaluation

2. **Integration with 05-data-layer**:
   - Store rule_package_versions with DSL source
   - Enforce >= 500 test case requirement

3. **Integration with 07-job-processing**:
   - RUN_RULE_TESTS job uses test runner
   - Validation job before PUBLISH transition

---

## References

- **SSOT 8.1**: Complete EBNF grammar
- **SSOT 8.2**: Built-in functions (15 total)
- **SSOT 8.3**: Typechecking rules
- **SSOT 8.4**: Minimal DSL example
- **SSOT 7.5**: Topological sort algorithm
- **Constitution VI**: Type-Safe Rules principle
