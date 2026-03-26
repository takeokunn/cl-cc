# cl-cc Modern Compiler Features — Execution Plan

## Overview

Introduce modern compiler features into cl-cc with maximum strictness. Type violations, linearity violations, and effect violations are **compile errors** (not warnings). CL specification takes priority on conflicts — nil semantics, mutable aliasing, dynamic scope, CLOS dispatch are preserved as-is.

## Design Principles

| Principle | Policy |
|---|---|
| Type safety | **Strict** — type errors are compile errors. Full inference fills in unannotated code. |
| Effect tracking | **Mandatory** — every function arrow carries an effect row. Pure by default. |
| Linearity | **Opt-in with strict enforcement** — `(declare (linear x))` enforced as error. Default is unrestricted (ω) per CL semantics. |
| Null handling | **Flow-typed** — nil is a valid CL value (CL spec), but operations on nullable types without guards are errors. |
| Pattern matching | **Exhaustive** — non-exhaustive typecase/case is a compile error. |
| CL conflicts | **CL wins** — no borrow checker (CL allows aliased mutation), no lifetime annotations (CL has GC), nil cannot be eliminated. |
| Backward compat | **Not a concern** — existing tests may break. The compiler enforces the new rules. |

## What CL Spec Prohibits

These features are **excluded** because CL semantics fundamentally require them:

| Excluded Feature | CL Reason |
|---|---|
| Full borrow checker | `(setf (car x) ...)` allows aliased mutable state |
| Lifetime annotations | GC manages all object lifetimes |
| nil elimination | nil is false, empty list, and a symbol simultaneously |
| Send/Sync mandatory | CL has no thread model in the spec; shared mutable state is the norm |
| Session types | CL has no channel primitives |

## Current State

| Component | Status | Key Files |
|---|---|---|
| Type nodes | 30+ type-node kinds defined | `src/type/representation.lisp` |
| Type inference | Algorithm W for ~15 AST node types | `src/type/inference.lisp:74-191` |
| Type checking | `type-check-ast` opt-in via `:type-check` | `src/compile/pipeline.lisp:4`, `src/compile/codegen.lisp:1918-1924` |
| Effect system | Effect-def, registry, row ops; partial `infer-effects` | `src/type/effect.lisp` |
| Linearity | QTT grades (0/1/ω), semiring ops | `src/type/multiplicity.lisp` |
| Refinement types | `type-refinement` node defined, no verification | `src/type/representation.lisp:21` |
| Constraint solver | OutsideIn(X) | `src/type/solver.lisp`, `src/type/constraint.lisp` |
| Optimizer | fold, DCE, jump-thread, unreachable | `src/optimize/optimizer.lisp:1-14` |
| SSA/CFG | Cytron + Braun, dom tree, dom frontiers, loop depth | `src/optimize/ssa.lisp`, `src/optimize/cfg.lisp` |
| E-graph | Full equality saturation engine | `src/optimize/egraph.lisp` |
| Effect classification | 7 kinds, 100+ VM inst classified | `src/optimize/effects.lisp` |
| MIR | SSA IR, phi nodes, safepoints (isel not yet) | `src/emit/mir.lisp` |
| Register alloc | Linear scan with liveness | `src/emit/regalloc.lisp` |
| Compile IR | ir-value, ir-block, ir-ssa, ir-printer | `src/compile/ir/` |
| GC | 2-gen (Cheney + mark-sweep), SATB barrier | `src/runtime/gc.lisp`, `src/runtime/heap.lisp` |
| Backends | x86-64, AArch64, WASM, Mach-O, ELF | `src/emit/`, `src/emit/binary/` |

## Pipeline

Current:
```
parse → macroexpand → lower-to-AST → [opt-in type-check] → compile-AST → optimize → emit
```

New (type checking is mandatory, not opt-in):
```
parse → macroexpand → lower-to-AST
  → type-infer + flow-narrow + effect-infer    [ERRORS on failure]
  → linearity-check + exhaustiveness-check     [ERRORS on failure]
  → definite-assignment-check                  [ERRORS on failure]
  → refinement-verify (Z3)                     [ERRORS on failure]
  → compile-AST
  → escape-analysis → inline → devirtualize
  → optimize (fold, DCE, jump-thread, CSE, E-graph)
  → LICM
  → isel → regalloc → safepoint/stackmap → emit
```

## Dependency Graph

```
FR-001 (Mandatory Type Checking)
  ├── FR-002 (Flow-Sensitive Typing) ──┐
  ├── FR-003 (Null Safety) ────────────┤
  ├── FR-004 (Definite Assignment) ────┤── FR-006 (Diagnostics)
  └── FR-005 (Exhaustiveness) ─────────┘
        │
        ▼
FR-007 (Escape Analysis) ──── FR-008 (Inlining) ──── FR-009 (Devirtualization)
        │                           │
        ▼                           ▼
FR-011 (LICM) ──────────── FR-010 (ISel) ──── FR-017 (Safepoint)
                                                     │
FR-012 (Linear Move) ◄── FR-002                      ▼
FR-013 (Mandatory Effects) ◄── FR-002          FR-018 (Exception Tables)
FR-014 (Effect Handlers) ◄── FR-013
FR-015 (Refinement + Z3) ◄── FR-002
FR-016 (GADT Refine) ◄── FR-005
FR-019 (Typeclass Coherence) ── standalone
FR-020 (LSP) ◄── FR-002, FR-006
```

---

## Phase 1: Mandatory Type System

### FR-001: Mandatory Type Checking

Type checking is **always on**. Remove the `:type-check` opt-in flag from `compile-expression`. Every expression must type-check or compilation fails.

- `src/compile/pipeline.lisp` — remove `:type-check` parameter; always run `type-check-ast`
- `src/compile/codegen.lisp:1918-1924` — `type-check-ast` called unconditionally

### FR-002: Flow-Sensitive Type Narrowing

After `(typecase x (fixnum ...))` or `(if (numberp x) ...)`, narrow x's type in the then-branch and subtract in the else-branch. **Error** if an operation is applied to a type that doesn't support it after narrowing.

Operates at AST level: extend `infer-if`/`infer-typecase` with per-branch type environments.

- New `src/type/flow-narrow.lisp` (~300 lines)
- `src/type/inference.lisp` — hooks in `infer-if`, new `infer-typecase`

### FR-003: Null Safety via Flow Typing

nil remains a valid CL value (CL spec), but:
- All values returned from functions that can return nil have type `(union T null)`
- Operations that don't accept null (arithmetic, slot-value, etc.) on a nullable type are **compile errors**
- After `(if x ...)` or `(when x ...)`, null is narrowed out in the then-branch
- `car`/`cdr` on `(union list null)` → error without null guard (CL spec says `(car nil) = nil`, but we enforce the guard)

- `src/type/inference.lisp` — null-aware inference for all AST nodes
- `src/type/subtyping.lisp` — null in union relations

### FR-004: Definite Assignment Analysis

On SSA form, detect use of variables where no definition reaches (phi-node with undefined input). **Compile error** "variable used uninitialized".

- `src/optimize/ssa.lisp` — new pass `ssa-check-definite-assignment`

### FR-005: Pattern Match Exhaustiveness

For `typecase`/`case`, verify all cases are covered. **Compile error** on non-exhaustive match. `etypecase`/`ecase` already error at runtime; this makes the check static.

- New `src/type/exhaustiveness.lisp` (~200 lines)
- Uses `src/type/subtyping.lisp` hierarchy

### FR-006: Type Error Diagnostics

Multi-error recovery (collect all type errors in a compilation unit before failing). `type-error` sentinel propagation through inference. Structured error messages with source location, expected/actual types, and fix suggestions (Rust/Elm-style).

- New `src/type/diagnostics.lisp` (~300 lines)
- `src/type/inference.lisp` — error recovery in each `infer-*` method

### Phase 1 Tests

| Test File | Count | Coverage |
|---|---|---|
| `tests/unit/type/flow-narrow-tests.lisp` | ~30 | typecase/if/when narrowing, null tracking |
| `tests/unit/type/exhaustiveness-tests.lisp` | ~20 | typecase/case coverage, missing branch detection |
| `tests/unit/type/diagnostics-tests.lisp` | ~15 | Multi-error recovery, structured messages |
| `tests/unit/optimize/definite-assignment-tests.lisp` | ~15 | Uninitialized variable detection |

---

## Phase 2: Optimization Pipeline

### FR-007: Escape Analysis

For each allocation site (cons, make-instance, lambda/closure, make-array), determine if the allocated object escapes. Non-escaping → stack-allocate. Uses SSA def-use chains + CFG reachability.

- New `src/optimize/escape.lisp` (~400 lines)
- `src/optimize/optimizer.lisp` — pipeline integration

### FR-008: Function Inlining

Inline functions at call sites. AST-level inlining (substitute body, re-run type inference). Always inline `(declare (inline f))`. Auto-inline when body size < threshold. Never inline `(declare (notinline f))` or recursive calls (depth limit = 0 by default).

- New `src/optimize/inline.lisp` (~300 lines)
- Integration: after macro expansion, before codegen

### FR-009: CLOS Devirtualization

When type inference determines the concrete class, replace `vm-dispatch-generic-call` with direct method call. This is a pure optimization — semantics unchanged.

- `src/compile/codegen.lisp` — ast-call for generic functions
- Uses `src/type/inference.lisp` type results

### FR-010: Instruction Selection (MIR Tiling)

Lower VM IR → target-specific MIR via pattern-matching rewrite rules. As referenced by `src/emit/mir.lisp:5` (`[Stage 2: isel.lisp]`). Prolog-based or table-driven pattern matcher.

- New `src/emit/isel.lisp` (~500 lines)
- `src/emit/mir.lisp` — consumer integration

### FR-011: Loop-Invariant Code Motion (LICM)

Hoist loop-invariant pure expressions above loop headers. Uses `bb-loop-depth` from `src/optimize/cfg.lisp:32` and effect classification from `src/optimize/effects.lisp`.

- New pass in `src/optimize/optimizer.lisp`

### Phase 2 Tests

| Test File | Count | Coverage |
|---|---|---|
| `tests/unit/optimize/escape-tests.lisp` | ~25 | Cons/closure/object escape classification |
| `tests/unit/optimize/inline-tests.lisp` | ~20 | Size threshold, declare respect, recursive guard |
| `tests/unit/emit/isel-tests.lisp` | ~30 | Pattern match, multi-target tiling |

---

## Phase 3: Resource Safety

### FR-012: Linear Move Semantics (Strict Enforcement)

When `(declare (linear x))` or typed as `!_1 T`:
- Use exactly once → OK
- Use zero times → **compile error** "unused linear resource"
- Use more than once → **compile error** "linear variable used twice"
- Default remains unrestricted (ω) per CL semantics — linearity is opt-in but enforcement is strict

Leverages `src/type/multiplicity.lisp` semiring.

- New `src/type/linearity-check.lisp` (~250 lines)
- `src/type/inference.lisp` — usage tracking extension

### FR-013: Mandatory Effect Tracking

Every function arrow carries an effect row. Pure by default. **Compile error** when:
- A function with no declared effects performs IO or mutation
- A function declared `(effects :pure)` calls an impure function
- Effect row of callee is not subset of caller's declared effects

Complete `infer-effects` for all AST node types.

- `src/type/effect.lisp` — complete inference for all AST types
- `src/type/inference.lisp` — effect propagation through arrows
- `src/compile/pipeline.lisp` — effect checking in pipeline

### FR-014: Effect Handler Compilation

Compile algebraic effect handlers to delimited continuations (CPS-based evidence passing). `(handle ((state get put)) body)` compiles to CPS with continuation capture per Koka's evidence-passing translation.

- `src/compile/cps.lisp` — handler CPS translation
- `src/type/effect.lisp` — type-handler typing rules

### Phase 3 Tests

| Test File | Count | Coverage |
|---|---|---|
| `tests/unit/type/linearity-tests.lisp` | ~20 | Double-use error, unused linear error |
| `tests/unit/type/effect-inference-tests.lisp` | ~25 | Effect row propagation, purity violation errors |

---

## Phase 4: Advanced Type Features

### FR-015: Refinement Types + Z3

`{x : fixnum | (> x 0)}` verified via Z3 SMT solver. **Compile error** on violated or unprovable refinements.

Z3 integration:
```
type-refinement → smt.lisp (predicate → SMT-LIB2)
  → (uiop:run-program "z3" :input smt-string)
  → sat → pass
  → unsat → compile error "refinement violated"
  → unknown → compile error "refinement unprovable"
```

Queries batched per compilation unit and cached. Z3 is a **required** dependency (not optional).

- New `src/type/smt.lisp` (~350 lines)

### FR-016: GADT Pattern Match Type Refinement

When matching a GADT constructor, the type index constraint refines the scrutinee type in the branch body. Full GADT elimination rule per OutsideIn(X).

- `src/type/inference.lisp` — GADT branch refinement
- `src/type/exhaustiveness.lisp` — GADT coverage

### Phase 4 Tests

| Test File | Count | Coverage |
|---|---|---|
| `tests/unit/type/smt-tests.lisp` | ~15 | SMT-LIB2 generation, Z3 result parsing |

---

## Phase 5: Backend Completion

### FR-017: Safepoint + Stack Map Generation

At each GC point (allocation, call site), emit stack map recording GC roots. Uses `miri-meta` from `src/emit/mir.lisp:52`.

- `src/emit/mir.lisp` — safepoint metadata
- `src/runtime/gc.lisp` — stack map integration

### FR-018: Exception Tables (Zero-Cost EH)

Landing pad table for CL condition system. `handler-case`/`handler-bind` → table entries instead of dynamic `catch`/`throw`.

- New `src/emit/exception-table.lisp` (~300 lines)
- `src/compile/codegen.lisp` — handler-case compilation

### Phase 5 Tests

| Test File | Count | Coverage |
|---|---|---|
| `tests/unit/emit/stackmap-tests.lisp` | ~15 | GC root tracking |

---

## Phase 6: Typeclass Completion

### FR-019: Typeclass Coherence Checking

Detect orphan instances and overlapping instances. **Compile error** on violations.

- `src/type/typeclass.lisp` — new `check-coherence` function

---

## Phase 7: Tooling

### FR-020: LSP Server

Language Server Protocol: hover (type display), go-to-definition, diagnostics (type errors), completion. Standalone process over stdio.

- New `src/lsp/` module (~800 lines)

---

## New Files Summary

| File | Phase | FR | Lines (est.) |
|---|---|---|---|
| `src/type/flow-narrow.lisp` | 1 | FR-002,003 | ~300 |
| `src/type/exhaustiveness.lisp` | 1 | FR-005 | ~200 |
| `src/type/diagnostics.lisp` | 1 | FR-006 | ~300 |
| `src/optimize/escape.lisp` | 2 | FR-007 | ~400 |
| `src/optimize/inline.lisp` | 2 | FR-008 | ~300 |
| `src/emit/isel.lisp` | 2 | FR-010 | ~500 |
| `src/type/linearity-check.lisp` | 3 | FR-012 | ~250 |
| `src/type/smt.lisp` | 4 | FR-015 | ~350 |
| `src/emit/exception-table.lisp` | 5 | FR-018 | ~300 |
| `src/lsp/server.lisp` | 7 | FR-020 | ~800 |

## Modified Files Summary

| File | FRs | Changes |
|---|---|---|
| `src/compile/pipeline.lisp` | FR-001,all | Mandatory type checking, effect checking, linearity checking |
| `src/compile/context.lisp` | FR-001 | Remove opt-in flags |
| `src/type/inference.lisp` | FR-002,003,006,013,016 | Flow narrowing, error recovery, effect propagation, GADT |
| `src/type/effect.lisp` | FR-013,014 | Complete `infer-effects` for all AST types |
| `src/type/multiplicity.lisp` | FR-012 | Usage counting integration |
| `src/type/typeclass.lisp` | FR-019 | Coherence checking |
| `src/optimize/optimizer.lisp` | FR-007,008,011 | New passes in pipeline |
| `src/optimize/ssa.lisp` | FR-004 | Definite assignment pass |
| `src/compile/codegen.lisp` | FR-009,018 | Devirtualization, exception tables |
| `src/compile/cps.lisp` | FR-014 | Effect handler CPS translation |
| `src/emit/mir.lisp` | FR-010,017 | ISel consumer, safepoint metadata |
| `src/runtime/gc.lisp` | FR-017 | Stack map integration |
| `cl-cc.asd` | All | New file registrations |

## Non-Functional Requirements

| ID | Requirement | Target |
|---|---|---|
| NFR-001 | Pure CL | No C dependencies. Z3 is sole external binary (via process spawn). |
| NFR-002 | Z3 Required | Z3 must be available. No graceful degradation — missing Z3 is a build error. |
| NFR-003 | Self-Hosting | `./cl-cc selfhost` must pass after all phases (existing checks may need updating). |

## Phase Execution Order

| Phase | FRs | Blocking Dependencies |
|---|---|---|
| Phase 1 | FR-001 through FR-006 | None |
| Phase 2 | FR-007 through FR-011 | Phase 1 |
| Phase 3 | FR-012 through FR-014 | Phase 1 |
| Phase 4 | FR-015 through FR-016 | Phase 1 |
| Phase 5 | FR-017 through FR-018 | Phase 2 FR-010 |
| Phase 6 | FR-019 | Standalone |
| Phase 7 | FR-020 | Phase 1 |

Phases 2, 3, 4, 6 can execute in parallel after Phase 1.

## Acceptance Criteria

- All type violations are compile errors
- All effect violations are compile errors
- All non-exhaustive pattern matches are compile errors
- All uninitialized variable uses are compile errors
- All linear variable misuses are compile errors
- All refinement type violations are compile errors (via Z3)
- Escape analysis eliminates heap allocations where possible
- Inlining and devirtualization produce measurably faster code
- ISel produces correct target-specific MIR for x86-64, AArch64, WASM
