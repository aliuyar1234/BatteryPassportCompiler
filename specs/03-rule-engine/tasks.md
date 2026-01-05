# Tasks: Rule Engine (DSL)

**Input**: Design documents from `specs/03-rule-engine/`
**Prerequisites**: plan.md (required), spec.md (required), data-model.md
**Phase**: P1 - Core library, depends on 01-foundation + 02-core-primitives
**Package**: bpc-core (IO-free, pure Haskell, GADTs)

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US6)
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `packages/bpc-core/src/BPC/Core/Rules/`
- **Tests**: `packages/bpc-core/test/BPC/Core/Rules/`
- **Fixtures**: `packages/bpc-core/test/fixtures/dsl/`

---

## Phase 1: Setup (Package Configuration)

**Purpose**: Configure bpc-core package for Rule Engine modules

- [X] T001 Add megaparsec, containers, mtl, parser-combinators dependencies to `packages/bpc-core/bpc-core.cabal`
- [X] T002 Add GHC extensions to bpc-core.cabal: GADTs, DataKinds, TypeFamilies, PolyKinds, KindSignatures
- [X] T003 Create directory structure: `packages/bpc-core/src/BPC/Core/Rules/`
- [X] T004 Create directory structure: `packages/bpc-core/test/BPC/Core/Rules/`
- [X] T005 [P] Create `packages/bpc-core/test/fixtures/dsl/` for test DSL files

**Checkpoint**: `cabal build bpc-core` succeeds with new dependencies

---

## Phase 2: Foundational (Error Types & Shared Types)

**Purpose**: Define error types and shared data types - BLOCKS all user stories

**CRITICAL**: Error types must be defined before any implementation

- [X] T006 Create `packages/bpc-core/src/BPC/Core/Rules/Error.hs` with ParseError type
- [X] T007 Add TypecheckError to `packages/bpc-core/src/BPC/Core/Rules/Error.hs`
- [X] T008 Add CycleError to `packages/bpc-core/src/BPC/Core/Rules/Error.hs`
- [X] T009 Add EvalError to `packages/bpc-core/src/BPC/Core/Rules/Error.hs`
- [X] T010 Create `packages/bpc-core/src/BPC/Core/Rules/AST.hs` with FieldPath, Identifier types
- [X] T011 Export error and shared types from BPC.Core.Rules module

**Checkpoint**: Error types available for use in implementation

---

## Phase 3: User Story 1 - DSL Parsing (Priority: P1) MVP

**Goal**: Parse Rule DSL source code into untyped AST following SSOT 8.1 EBNF

**Independent Test**: SSOT 8.4 example DSL parses successfully with correct AST structure

### Tests for User Story 1

- [X] T012 [P] [US1] Create `packages/bpc-core/test/BPC/Core/Rules/ParserSpec.hs` with test scaffold
- [X] T013 [P] [US1] Create `packages/bpc-core/test/fixtures/dsl/minimal.dsl` with SSOT 8.4 example
- [X] T014 [US1] Add test: Parse field declaration `field battery.capacity_kwh: Dec(6) = ...;` in ParserSpec.hs
- [X] T015 [US1] Add test: Parse let-expression with nested bindings in ParserSpec.hs
- [X] T016 [US1] Add test: Parse comment `-- comment` is ignored in ParserSpec.hs
- [X] T017 [US1] Add test: Syntax error returns RULE_PARSE_ERROR with line/column in ParserSpec.hs
- [X] T018 [US1] Add test: Parse if/then/else expression in ParserSpec.hs
- [X] T019 [US1] Add property test: operator precedence correctness in ParserSpec.hs

### Implementation for User Story 1

- [X] T020 [US1] Create `packages/bpc-core/src/BPC/Core/Rules/Lexer.hs` with whitespace/comment handling
- [X] T021 [US1] Add keyword set to Lexer.hs (field, let, if, then, else, etc.)
- [X] T022 [US1] Add identifier parser (excluding keywords) to Lexer.hs
- [X] T023 [US1] Add literal parsers (string, int, decimal, date, qty) to Lexer.hs
- [X] T024 [US1] Add operator parsers to Lexer.hs
- [X] T025 [US1] Create `packages/bpc-core/src/BPC/Core/Rules/AST.hs` with UntypedExpr data type
- [X] T026 [US1] Add FieldDecl, ExampleTest, PropertyTest to AST.hs
- [X] T027 [US1] Add Declaration sum type (DeclField | DeclExample | DeclProperty) to AST.hs
- [X] T028 [US1] Create `packages/bpc-core/src/BPC/Core/Rules/Parser.hs` with parseSource function
- [X] T029 [US1] Implement expression parser with operator precedence (|| > && > cmp > + - > * /) in Parser.hs
- [X] T030 [US1] Implement fieldDecl parser in Parser.hs
- [X] T031 [US1] Implement letExpr parser in Parser.hs
- [X] T032 [US1] Implement ifExpr parser in Parser.hs
- [X] T033 [US1] Implement assertExpr parser in Parser.hs
- [X] T034 [US1] Implement funcCall parser in Parser.hs
- [X] T035 [US1] Export parseSource from BPC.Core.Rules module

**Checkpoint**: SSOT 8.4 example parses without errors; all parser tests pass

---

## Phase 4: User Story 2 - Type Checking (Priority: P1)

**Goal**: Transform untyped AST to typed AST (GADTs) with comprehensive type error detection

**Independent Test**: Type mismatch `Dec(6) + Dec(2)` or `Qty("kg") + Qty("kWh")` detected

### Tests for User Story 2

- [X] T036 [P] [US2] Create `packages/bpc-core/test/BPC/Core/Rules/TypecheckSpec.hs` with test scaffold
- [X] T037 [US2] Add test: Typecheck SSOT 8.4 example successfully in TypecheckSpec.hs
- [X] T038 [US2] Add test: Detect type mismatch Dec(6) + Dec(2) in TypecheckSpec.hs
- [X] T039 [US2] Add test: Detect unit mismatch Qty("kg") + Qty("kWh") in TypecheckSpec.hs
- [X] T040 [US2] Add test: Detect unknown field reference in TypecheckSpec.hs
- [X] T041 [US2] Add test: Detect unknown unit in toQty in TypecheckSpec.hs
- [X] T042 [US2] Add test: Detect arity mismatch in function calls in TypecheckSpec.hs
- [X] T043 [US2] Add property test: operator type rules verification in TypecheckSpec.hs

### Implementation for User Story 2

- [X] T044 [US2] Add Ty data type (type universe) to AST.hs with TBool, TInt, TDec, TQty, etc.
- [X] T045 [US2] Add STy singleton type for runtime type information to AST.hs
- [X] T046 [US2] Add Expr GADT (typed expression) to AST.hs
- [X] T047 [US2] Add HList for heterogeneous function arguments to AST.hs
- [X] T048 [US2] Add SomeExpr existential wrapper to AST.hs
- [X] T049 [US2] Create `packages/bpc-core/src/BPC/Core/Rules/Typecheck.hs` module
- [X] T050 [US2] Implement TypeEnv and FieldEnv types in Typecheck.hs
- [X] T051 [US2] Implement typecheck function for literals in Typecheck.hs
- [X] T052 [US2] Implement typecheck for binary operators with SSOT 8.3 rules in Typecheck.hs
- [X] T053 [US2] Implement typecheck for let/if/assert expressions in Typecheck.hs
- [X] T054 [US2] Implement typecheck for function application in Typecheck.hs
- [X] T055 [US2] Implement typecheckSource for entire DSL file in Typecheck.hs
- [X] T056 [US2] Export typecheck, typecheckSource from BPC.Core.Rules module

**Checkpoint**: SSOT 8.4 example typechecks successfully; all type errors detected with clear messages

---

## Phase 5: User Story 3 - Dependency Graph & Topo Sort (Priority: P1)

**Goal**: Build field dependency graph and sort using Kahn's algorithm (SSOT 7.5)

**Independent Test**: After topoSort, all dependencies come before dependents; cycles detected

### Tests for User Story 3

- [X] T057 [P] [US3] Create `packages/bpc-core/test/BPC/Core/Rules/GraphSpec.hs` with test scaffold
- [X] T058 [US3] Add test: Field A depends on B → B comes before A in GraphSpec.hs
- [X] T059 [US3] Add test: Detect cycle A → B → A in GraphSpec.hs
- [X] T060 [US3] Add test: Detect complex cycle A → B → C → D → B in GraphSpec.hs
- [X] T061 [US3] Add test: Independent fields sorted lexically (tie-break) in GraphSpec.hs
- [X] T062 [US3] Add test: Empty graph returns empty list in GraphSpec.hs
- [X] T063 [US3] Add property test: All deps come before node after sort in GraphSpec.hs

### Implementation for User Story 3

- [X] T064 [US3] Create `packages/bpc-core/src/BPC/Core/Rules/Graph.hs` module
- [X] T065 [US3] Implement extractDeps to find field references in expressions in Graph.hs
- [X] T066 [US3] Implement buildGraph from field expressions in Graph.hs
- [X] T067 [US3] Implement topoSortFields with Kahn's algorithm in Graph.hs
- [X] T068 [US3] Implement lexical tie-break (compareLexical) in Graph.hs
- [X] T069 [US3] Implement cycle detection returning CycleError with cycle_fields in Graph.hs
- [X] T070 [US3] Export buildGraph, topoSortFields from BPC.Core.Rules module

**Checkpoint**: Property test passes; cycles correctly detected with all fields in cycle

---

## Phase 6: User Story 4 - Built-in Functions (Priority: P1)

**Goal**: Implement all 16 built-in functions from SSOT 8.2

**Independent Test**: Each built-in function tested with mock facts

### Tests for User Story 4

- [X] T071 [P] [US4] Create `packages/bpc-core/test/BPC/Core/Rules/BuiltinsSpec.hs` with test scaffold
- [X] T072 [US4] Add test: getFact("Battery", "key") returns Opt(Record) in BuiltinsSpec.hs
- [X] T073 [US4] Add test: getFactsByPrefix("BOM", "bom:") returns List(Record) in BuiltinsSpec.hs
- [X] T074 [US4] Add test: field("battery.capacity_kwh") resolves reference in BuiltinsSpec.hs
- [X] T075 [US4] Add test: recordGet(record, "field") extracts value in BuiltinsSpec.hs
- [X] T076 [US4] Add test: isSome(none) = false, isSome(some(x)) = true in BuiltinsSpec.hs
- [X] T077 [US4] Add test: unwrapOr(none, default) returns default in BuiltinsSpec.hs
- [X] T078 [US4] Add test: requireSome(none, "E001", "msg") throws error in BuiltinsSpec.hs
- [X] T079 [US4] Add test: toDec(6, 50) converts to Dec(6) in BuiltinsSpec.hs
- [X] T080 [US4] Add test: toQty("kg", 100) creates Qty("kg") in BuiltinsSpec.hs
- [X] T081 [US4] Add test: convert("g", "kg", qty(1000, "g")) = qty(1, "kg") in BuiltinsSpec.hs
- [X] T082 [US4] Add test: sumQty("kWh", [qty(1), qty(2)]) = qty(3, "kWh") in BuiltinsSpec.hs
- [X] T083 [US4] Add test: sumDec(2, [dec(1.5), dec(2.5)]) = dec(4.0) in BuiltinsSpec.hs
- [X] T084 [US4] Add test: map([1,2,3], λx→x*2) = [2,4,6] in BuiltinsSpec.hs
- [X] T085 [US4] Add test: filter([1,2,3], λx→x>1) = [2,3] in BuiltinsSpec.hs
- [X] T086 [US4] Add test: fold([1,2,3], 0, λacc x→acc+x) = 6 in BuiltinsSpec.hs
- [X] T087 [US4] Add test: emitCompliance("clause", "PASS", "evidence") = true in BuiltinsSpec.hs
- [X] T088 [US4] Add edge case tests: unit mismatch, division by zero, empty list in BuiltinsSpec.hs

### Implementation for User Story 4

- [X] T089 [US4] Create `packages/bpc-core/src/BPC/Core/Rules/Builtins.hs` module
- [X] T090 [US4] Define Builtin enum with all 16 built-ins in Builtins.hs
- [X] T091 [US4] Define BuiltinSig for type signatures in Builtins.hs
- [X] T092 [US4] Implement builtinSignature lookup in Builtins.hs
- [X] T093 [US4] Implement lookupBuiltin name to Builtin lookup in Builtins.hs
- [X] T094 [US4] Export Builtin, builtinSignature, lookupBuiltin from BPC.Core.Rules module

**Checkpoint**: All 16 built-ins have tests; edge cases (empty list, division by zero) handled

---

## Phase 7: User Story 4 (continued) - Evaluator (Priority: P1)

**Goal**: Pure expression evaluation with memoization

**Independent Test**: Evaluate SSOT 8.4 example with mock facts; determinism property

### Tests for User Story 4 - Evaluator

- [X] T095 [P] [US4] Create `packages/bpc-core/test/BPC/Core/Rules/EvalSpec.hs` with test scaffold
- [X] T096 [US4] Add test: Evaluate literals (bool, int, dec, qty, text) in EvalSpec.hs
- [X] T097 [US4] Add test: Evaluate binary operators all types in EvalSpec.hs
- [X] T098 [US4] Add test: Evaluate let bindings in EvalSpec.hs
- [X] T099 [US4] Add test: Evaluate if/then/else in EvalSpec.hs
- [X] T100 [US4] Add test: Evaluate field references with memoization in EvalSpec.hs
- [X] T101 [US4] Add test: Division by zero returns error in EvalSpec.hs
- [X] T102 [US4] Add test: requireSome failure returns error in EvalSpec.hs
- [X] T103 [US4] Add property test: evalFields is deterministic in EvalSpec.hs

### Implementation for User Story 4 - Evaluator

- [X] T104 [US4] Create `packages/bpc-core/src/BPC/Core/Rules/Eval.hs` module
- [X] T105 [US4] Define EvalContext with facts and fields maps in Eval.hs
- [X] T106 [US4] Define Value data type (runtime representation) in Eval.hs
- [X] T107 [US4] Implement evalExpr for literals in Eval.hs
- [X] T108 [US4] Implement evalExpr for binary operators in Eval.hs
- [X] T109 [US4] Implement evalExpr for let/if/assert in Eval.hs
- [X] T110 [US4] Implement evalExpr for field references (memoization lookup) in Eval.hs
- [X] T111 [US4] Implement evalBuiltin for all 16 built-ins in Eval.hs
- [X] T112 [US4] Implement evalFields with topological order in Eval.hs
- [X] T113 [US4] Export evalExpr, evalFields from BPC.Core.Rules module

**Checkpoint**: SSOT 8.4 example evaluates; determinism property test passes

---

## Phase 8: User Story 5 - Example Tests (Priority: P2)

**Goal**: Parse and execute example tests from DSL

**Independent Test**: Example test with correct fixtures passes; incorrect expected value fails

### Tests for User Story 5

- [X] T114 [P] [US5] Create `packages/bpc-core/test/BPC/Core/Rules/TestsSpec.hs` with test scaffold
- [X] T115 [US5] Add test: Parse example test from DSL in TestsSpec.hs
- [X] T116 [US5] Add test: Example test passes with correct fixtures in TestsSpec.hs
- [X] T117 [US5] Add test: Example test fails with incorrect expected value in TestsSpec.hs

### Implementation for User Story 5

- [X] T118 [US5] Add exampleTest parser to Parser.hs
- [X] T119 [US5] Add factFixtures parser to Parser.hs
- [X] T120 [US5] Add assertions parser to Parser.hs
- [X] T121 [US5] Create `packages/bpc-core/src/BPC/Core/Rules/Tests.hs` module
- [X] T122 [US5] Define TestResult type (Passed | Failed) in Tests.hs
- [X] T123 [US5] Implement runExampleTest function in Tests.hs
- [X] T124 [US5] Implement checkAssertions function in Tests.hs
- [X] T125 [US5] Export runExampleTest from BPC.Core.Rules module

**Checkpoint**: Example tests execute correctly; failures report expected vs actual

---

## Phase 9: User Story 6 - Property Tests (Priority: P3)

**Goal**: Parse and execute property tests with seed-based generation

**Independent Test**: Property test generates correct number of cases; reproducible with same seed

### Tests for User Story 6

- [X] T126 [P] [US6] Add property test parsing tests to TestsSpec.hs
- [X] T127 [US6] Add test: Property test generates 500 cases in TestsSpec.hs
- [X] T128 [US6] Add test: Property test reproducible with same seed in TestsSpec.hs
- [X] T129 [US6] Add test: Property test finds counterexample in TestsSpec.hs

### Implementation for User Story 6

- [X] T130 [US6] Add propertyTest parser to Parser.hs
- [X] T131 [US6] Add forall quantifier parser to Parser.hs
- [X] T132 [US6] Add implies parser to Parser.hs
- [X] T133 [US6] Implement runPropertyTest function in Tests.hs
- [X] T134 [US6] Implement generator for test values in Tests.hs
- [X] T135 [US6] Export runPropertyTest from BPC.Core.Rules module

**Checkpoint**: Property tests reproducible; counterexamples found

---

## Phase 10: Polish & Cross-Cutting Concerns

**Purpose**: Integration, documentation, and final verification

- [X] T136 [P] Add Haddock comments to all public functions in BPC.Core.Rules modules
- [X] T137 Create comprehensive re-export module `packages/bpc-core/src/BPC/Core/Rules.hs`
- [X] T138 Verify no IO imports: `grep -r "import.*IO" packages/bpc-core/src/BPC/Core/Rules/`
- [X] T139 Run full test suite: `cabal test bpc-core` with all Rules tests passing
- [X] T140 Verify coverage >= 90%: generate HPC report for BPC.Core.Rules.*
- [X] T141 Run fourmolu check: `fourmolu -m check packages/bpc-core/src/BPC/Core/Rules/**/*.hs`
- [X] T142 Run hlint check: `hlint -h hlint.yaml packages/bpc-core/src/BPC/Core/Rules/**/*.hs`
- [X] T143 Update quickstart.md with working DSL examples
- [X] T144 Performance test: 10,000 lines DSL in < 1 second
- [X] T145 [P] Create golden tests for parser output in test/fixtures/dsl/golden/

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: Depends on 01-foundation + 02-core-primitives completion
- **Foundational (Phase 2)**: Depends on Setup - BLOCKS all user stories
- **User Stories (Phase 3-9)**: All depend on Foundational
  - US1 (Parsing) must complete before US2 (Typecheck)
  - US2 (Typecheck) must complete before US4 Evaluator
  - US3 (Topo Sort) can proceed in parallel with US1-US2
  - US4 Built-ins can start in parallel with US2
  - US4 Evaluator depends on US2 + US3 + US4 Built-ins
  - US5 + US6 (Tests) depend on US1 (parsing) + US4 Evaluator
- **Polish (Phase 10)**: Depends on all user stories

### User Story Dependencies

```
Phase 2: Foundational (Error/Shared Types)
     │
     ├─────────────────────────────────┐
     ▼                                 ▼
Phase 3: US1 (Parsing)          Phase 5: US3 (TopoSort)
     │                                 │
     ▼                                 │
Phase 4: US2 (Typecheck) ─────────────►│
     │                                 │
     ├──────────────────┐              │
     ▼                  ▼              │
Phase 6: US4       Phase 7: US4       │
(Built-ins)        (Evaluator) ◄──────┘
     │                  │
     └──────────────────┤
                        ▼
              Phase 8: US5 (Example Tests)
                        │
                        ▼
              Phase 9: US6 (Property Tests)
                        │
                        ▼
              Phase 10: Polish
```

### Parallel Opportunities

**Phase 3**: T012, T013 (test/fixture creation) can run in parallel
**Phase 4**: T036 (test scaffold) can start early
**Phase 5**: T057 (test scaffold) can run in parallel with Phase 4
**Phase 6**: T071 (test scaffold) can start early
**Phase 7**: T095 (test scaffold) can start early
**Phase 8-9**: T114, T126 (test scaffolds) can start early

---

## Parallel Example: Phase 3 Parser Tests

```bash
# Launch test setup tasks in parallel:
Task: "Create ParserSpec.hs test scaffold" (T012)
Task: "Create minimal.dsl fixture" (T013)
```

---

## Implementation Strategy

### MVP First (User Stories 1-4)

1. Complete Phase 1: Setup (T001-T005)
2. Complete Phase 2: Foundational (T006-T011)
3. Complete Phase 3: US1 Parsing (T012-T035)
4. Complete Phase 4: US2 Typecheck (T036-T056)
5. Complete Phase 5: US3 TopoSort (T057-T070)
6. Complete Phase 6-7: US4 Built-ins + Evaluator (T071-T113)
7. **STOP and VALIDATE**: SSOT 8.4 example parses, typechecks, evaluates

### Incremental Delivery

1. Setup + Foundational → Error types ready
2. Add US1 (Parsing) → DSL parses to AST
3. Add US2 (Typecheck) → Type safety enforced
4. Add US3 (TopoSort) → Dependency order computed
5. Add US4 (Built-ins + Eval) → Rules evaluate
6. Add US5 (Example Tests) → Concrete tests work
7. Add US6 (Property Tests) → Generalized testing
8. Polish → Documentation, performance

---

## Summary

- **Total Tasks**: 145
- **Phase 1 (Setup)**: 5 tasks
- **Phase 2 (Foundational)**: 6 tasks
- **Phase 3 (US1 - Parsing)**: 24 tasks
- **Phase 4 (US2 - Typecheck)**: 21 tasks
- **Phase 5 (US3 - TopoSort)**: 14 tasks
- **Phase 6 (US4 - Built-ins)**: 24 tasks
- **Phase 7 (US4 - Evaluator)**: 19 tasks
- **Phase 8 (US5 - Example Tests)**: 12 tasks
- **Phase 9 (US6 - Property Tests)**: 10 tasks
- **Phase 10 (Polish)**: 10 tasks

**MVP Scope**: Phases 1-7 (113 tasks) for complete rule evaluation
**Parallel Opportunities**: 12 tasks marked [P]
**Constitution Compliance**: All code IO-free, GADTs for type safety, determinism verified

**STATUS**: All 145 tasks completed
