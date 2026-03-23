# CL-CC Custom Test Framework: FiveAM Removal & Zero-Dependency Migration

## Summary

FiveAM dependency を削除し、独自設計のフル機能テストフレームワーク (TAP v13 出力、assert-equal 系 API、parameterized/snapshot/pipeline/differential/pattern-matching/shrinking/combinatorial/performance/fixture/skip/timeout/dependencies/flaky-detection) に置き換え、`cl-cc/test` システムを完全 zero-dependency にする。

## Background

- `cl-cc` 本体は既に `:depends-on ()` で zero-dependency
- テストシステム `cl-cc/test` のみが `:fiveam` に依存
- 1398 箇所のアサーション (1371 `is` + 11 `is-true` + 16 `signals`) を新 API に移行
- 全移行パターンが mechanical (perl 一括変換可能)

## Current State

| Item | Detail |
|---|---|
| Dependency | `cl-cc.asd` L56: `:depends-on (:cl-cc :fiveam)` |
| Test files | 16 files under `tests/` + `tests/pbt/` |
| Total assertions | 1398 (1371 `is` + 11 `is-true` + 16 `signals`) |
| Test definitions | ~600+ `(test name ...)` forms |
| Suites | 5: `cl-cc-suite`, `control-flow-tests`, `closure-tests-suite`, `macro-suite`, `cl-cc-pbt-suite` |
| PBT framework | `tests/pbt/framework.lisp` (custom generators, wraps FiveAM internally) |

## Design Decisions

| Decision | Choice | Rationale |
|---|---|---|
| API design | 独自設計 (not FiveAM-compatible) | よりシンプルで明示的な API |
| Framework location | `tests/framework*.lisp` (3 files) | テストシステム内で完結 |
| PBT strategy | 既存カスタム `for-all` に統一 + shrinking 追加 | `tests/pbt/framework.lisp` に既に実装あり |
| Output format | TAP version 13 | CI 連携、YAML diagnostic block |
| Assertion style | `assert-equal` 系 | 明示的で読みやすい |
| Test macro | `deftest` | CL 慣例の `def-` prefix |
| Suite management | `defsuite` + `in-suite` | 階層構造を維持 |

## Assertion Predicate Analysis

### `(is ...)` Predicate Frequency (1371 total)

| Predicate | Count | New Macro |
|---|---|---|
| `=` | 501 | `assert-=` |
| `eq` | 214 | `assert-eq` |
| `equal` | 209 | `assert-equal` |
| `typep` | 117 | `assert-type` |
| `not` | 104 | `assert-false` |
| `null` | 44 | `assert-null` |
| `search` | 43 | `assert-true` |
| `string=` | 30 | `assert-string=` |
| `getf` | 23 | `assert-true` |
| `is-compile-string` | 15 | `assert-true` |
| `listp` | 14 | `assert-true` |
| `or` | 13 | `assert-true` |
| Others (14 predicates) | 43 | `assert-true` / `assert-eql` |

### Special Forms

| Form | Count | New Macro |
|---|---|---|
| `is-true` | 11 | `assert-true` |
| `signals` | 16 | `assert-signals` |

## Functional Requirements

### Core Framework (FR-001 ~ FR-013)

| ID | Requirement | Priority |
|---|---|---|
| FR-001 | `deftest` macro: `(deftest name "doc" body)` | Mandatory |
| FR-002 | `defsuite` macro: `(defsuite name &key parent description)` | Mandatory |
| FR-003 | `in-suite` macro: `(in-suite name)` | Mandatory |
| FR-004 | Core assertions: `assert-=`, `assert-eq`, `assert-equal`, `assert-eql`, `assert-null`, `assert-true`, `assert-false`, `assert-type`, `assert-string=` | Mandatory |
| FR-005 | `assert-signals`: `(assert-signals condition-type form)` | Mandatory |
| FR-006 | TAP version 13 output format | Mandatory |
| FR-007 | Failure diff: expected/actual + YAML diagnostic | Mandatory |
| FR-008 | `run-suite` / `run-tests`: suite execution + exit code | Mandatory |
| FR-009 | `defbefore` / `defafter`: fixture (setup/teardown) | Mandatory |
| FR-010 | `skip`: `(skip "reason")` → TAP `ok # SKIP` | Mandatory |
| FR-011 | `pending`: `(pending "reason")` → TAP `not ok # TODO` | Mandatory |
| FR-012 | PBT integration: `fiveam:test`/`fiveam:is` → custom framework | Mandatory |
| FR-013 | `fiveam:for-all` → existing custom `for-all` | Mandatory |

### Advanced Features (FR-014 ~ FR-020)

| ID | Requirement | Priority |
|---|---|---|
| FR-014 | `deftest-each` (parameterized tests) | Mandatory |
| FR-015 | `testing` (nested sub-cases) | Mandatory |
| FR-016 | `assert-snapshot` (snapshot/regression testing) | Mandatory |
| FR-017 | `assert-values` (multiple values testing) | Mandatory |
| FR-018 | `:timeout` option on deftest | Mandatory |
| FR-019 | Compiler DSL helpers | Mandatory |
| FR-020 | Test filtering by tag | Optional |

### Compiler-Specific Features (FR-021 ~ FR-024)

| ID | Requirement | Priority |
|---|---|---|
| FR-021 | `deftest-pipeline` (pipeline stage testing: parse → AST → CPS → VM → execute) | Mandatory |
| FR-022 | `assert-same-as-sbcl` / `deftest-differential` (differential testing vs host CL) | Mandatory |
| FR-023 | `assert-instructions-match` / `assert-ast-matches` (AST/VM pattern matching with `_` wildcard) | Mandatory |
| FR-024 | `assert-faster-than` / `assert-no-consing` (performance regression detection) | Mandatory |

### Testing Infrastructure Features (FR-025 ~ FR-028)

| ID | Requirement | Priority |
|---|---|---|
| FR-025 | PBT with shrinking (auto-minimize failing inputs) | Mandatory |
| FR-026 | `:depends-on` option (test dependency, auto-skip on failure) | Mandatory |
| FR-027 | `deftest-combinatorial` (all-combinations / pairwise testing) | Mandatory |
| FR-028 | Flaky test detection (`:repeat N` on run-suite) | Mandatory |

### Parallel & Random Execution (FR-037 ~ FR-038)

| ID | Requirement | Priority |
|---|---|---|
| FR-037 | Parallel test execution: default behavior; each test runs in its own `sb-thread`; worker count = `(sb-thread:thread-count)` or `:workers N`; tests are isolated via per-thread dynamic bindings; TAP output is buffered per-test and flushed atomically in completion order | Mandatory |
| FR-038 | Random execution order: tests are shuffled before dispatch; seed is printed as TAP comment (`# Seed: N`) so failures are reproducible; `:seed N` fixes the order for a given run; sequential opt-out via `:parallel nil :random nil` | Mandatory |

### Meta-Testing & Verification (FR-029 ~ FR-034)

| ID | Requirement | Priority |
|---|---|---|
| FR-029 | `run-mutation-test` (mutation testing: auto-mutate source, verify tests catch mutations) | Mandatory |
| FR-030 | `assert-equivalent-execution` / `assert-roundtrip-preserves` (optimization/CPS equivalence) | Mandatory |
| FR-031 | `deftest-fuzz` / `assert-no-crash` / `assert-terminates` (random program generation & fuzzing) | Mandatory |
| FR-032 | `assert-backends-agree` / `deftest-cross-backend` (x86-64 vs aarch64 result equivalence) | Mandatory |
| FR-033 | `definvariant` (contract/invariant testing: checks after every test) | Mandatory |
| FR-034 | `assert-deterministic` (determinism: repeated compilation produces identical output) | Mandatory |

### Final Layer (FR-035 ~ FR-036)

| ID | Requirement | Priority |
|---|---|---|
| FR-035 | Code coverage via `sb-cover` (`:coverage t` option on run-suite) | Mandatory |
| FR-036 | `defmetamorphic` (metamorphic testing: input transformation → output relation verification) | Mandatory |

## API Reference

### Suite & Test Definition

```lisp
;; Suite management
(defsuite cl-cc-suite :description "Main test suite")
(defsuite closure-tests :parent cl-cc-suite)
(in-suite cl-cc-suite)

;; Test definition
(deftest vm-exec-arithmetic
  "Test arithmetic ops."
  (assert-= 7 (run-string "(+ 3 4)")))

;; Test with options
(deftest infinite-loop-guard
  :timeout 5                          ; seconds (sb-ext:with-timeout)
  :depends-on vm-exec-arithmetic      ; skip if dependency failed
  (assert-signals error (run-string "(loop)")))
```

### Core Assertions

```lisp
(assert-= expected actual)           ; numeric =
(assert-eq expected actual)          ; pointer eq
(assert-eql expected actual)         ; eql
(assert-equal expected actual)       ; structural equal
(assert-string= expected actual)     ; string=
(assert-null form)                   ; null check
(assert-true form)                   ; generic truthy
(assert-false form)                  ; generic falsy
(assert-type type-name object)       ; typep check
(assert-signals condition-type form) ; condition signaling
(assert-values form val1 val2 ...)   ; multiple return values
```

### Fixture

```lisp
(defbefore :each (suite-name) body)  ; run before each test
(defafter :each (suite-name) body)   ; run after each test
```

### Skip / Pending

```lisp
(skip "reason")                      ; skip current test
(pending "reason")                   ; mark as TODO
```

### Parameterized Tests (FR-014)

```lisp
(deftest-each vm-arithmetic
  "Arithmetic operations"
  :cases (("+  3+4" "(+ 3 4)"  7)
          ("- 10-7" "(- 10 7)" 3)
          ("*  6*7" "(* 6 7)"  42))
  (name expr expected)
  (assert-= expected (run-string expr)))
;; TAP: ok 1 - vm-arithmetic [+ 3+4]
;;      ok 2 - vm-arithmetic [- 10-7]
;;      ok 3 - vm-arithmetic [* 6*7]
```

### Nested Sub-Cases (FR-015)

```lisp
(deftest vm-let-binding
  "Let binding tests"
  (testing "simple binding"
    (assert-= 42 (run-string "(let ((x 42)) x)")))
  (testing "nested binding"
    (assert-= 5 (run-string "(let ((x 2) (y 3)) (+ x y))"))))
;; TAP: ok 1 - vm-let-binding > simple binding
;;      ok 2 - vm-let-binding > nested binding
```

### Snapshot Testing (FR-016)

```lisp
(deftest compiler-output-snapshot
  (assert-snapshot "vm-add-output"
    (compile-string "(+ 1 2)")))
;; First run: saves to tests/snapshots/vm-add-output.snap
;; Later runs: compares against saved
;; Update: (run-suite 'cl-cc-suite :update-snapshots t)
```

### Compiler DSL Helpers (FR-019)

```lisp
(assert-compiles-to "(+ 1 2)" :contains 'vm-add)
(assert-evaluates-to "(+ 1 2)" 3)
(assert-evaluates-to "(+ 1 2)" 3 :stdlib t)
(assert-macro-expands-to '(when t 42) '(if t (progn 42) nil))
(assert-infers-type "(+ 1 2)" 'fixnum)
```

### Pipeline Stage Testing (FR-021)

```lisp
(deftest-pipeline "(+ 1 2)"
  :parse    (assert-type 'ast-binop _)
  :expand   (assert-equal _ raw-ast)
  :compile  (assert-true (find 'vm-add _ :key #'type-of))
  :execute  (assert-= 3 _))
;; _ is bound to each stage's output automatically
;; Tests each stage of: parse → macro-expand → compile → execute
```

### Differential Testing (FR-022)

```lisp
;; Compare cl-cc output with host CL (SBCL)
(assert-same-as-sbcl "(+ 1 2)")
(assert-same-as-sbcl "(car '(1 2 3))")

;; Bulk differential testing
(deftest-differential
  :expressions '("(+ 1 2)" "(* 3 4)" "(car '(a b))")
  :ignore-types t)    ; ignore type representation differences
;; Evaluates each expression in both cl-cc VM and SBCL eval
;; Reports any divergence
```

### AST/VM Pattern Matching (FR-023)

```lisp
;; Pattern match on instruction sequence (wildcard _ matches anything)
(assert-instructions-match
  (compile-string "(+ 1 2)")
  '((vm-const :dst _ :value 1)
    (vm-const :dst _ :value 2)
    (vm-add :dst _ :lhs _ :rhs _)))

;; AST pattern matching
(assert-ast-matches
  (parse "(let ((x 1)) x)")
  '(ast-let
    :bindings ((x (ast-int :value 1)))
    :body ((ast-var :name x))))
```

### Performance Regression (FR-024)

```lisp
(deftest vm-arithmetic-perf
  :timeout 1
  (assert-faster-than 10      ; milliseconds
    (run-string "(+ 1 2)"))
  (assert-no-consing           ; zero heap allocation
    (run-string "(+ 1 2)")))
;; Uses get-internal-real-time for timing
;; Uses sb-ext:gc + allocation tracking for consing
```

### PBT with Shrinking (FR-025)

```lisp
(defproperty addition-commutative
  ((a (gen-integer :min -1000 :max 1000))
   (b (gen-integer :min -1000 :max 1000)))
  :shrink t                    ; enable shrinking
  (= (run-string (format nil "(+ ~D ~D)" a b))
     (+ a b)))
;; Failure: a=537, b=291
;; Shrunk to: a=256, b=1  ← minimal failing case
;; Shrinking strategy: binary search for integers, sublist for lists
```

### Test Dependencies (FR-026)

```lisp
(deftest vm-basic-arithmetic
  (assert-= 3 (run-string "(+ 1 2)")))

(deftest vm-nested-arithmetic
  :depends-on vm-basic-arithmetic
  (assert-= 9 (run-string "(+ (* 2 3) 3)")))
;; If vm-basic-arithmetic fails:
;; not ok 1 - vm-basic-arithmetic
;; ok 2 - vm-nested-arithmetic # SKIP dependency failed
```

### Combinatorial Testing (FR-027)

```lisp
(deftest-combinatorial arithmetic-combos
  :params ((op '(+ - *))
           (a  '(0 1 -1 42))
           (b  '(0 1 -1 42)))
  (let* ((expr (format nil "(~A ~D ~D)" op a b))
         (expected (funcall op a b)))
    (assert-= expected (run-string expr))))
;; Generates 3 * 4 * 4 = 48 test cases automatically
;; TAP: ok 1 - arithmetic-combos [+ 0 0]
;;      ok 2 - arithmetic-combos [+ 0 1]
;;      ...
```

### Flaky Test Detection (FR-028)

```lisp
;; Run suite multiple times to detect flaky tests
(run-suite 'cl-cc-suite :repeat 3)
;; Output:
;; TAP version 13
;; ...
;; # Flaky tests detected (inconsistent across 3 runs):
;; #   vm-random-test: passed 2/3 runs
```

### Mutation Testing (FR-029)

```lisp
(run-mutation-test
  :target "src/compiler.lisp"
  :suite 'cl-cc-suite
  :mutations '(:arithmetic-swap      ; + ↔ -, * ↔ /
               :condition-negate      ; if cond → if (not cond)
               :boundary-shift        ; < → <=, > → >=
               :constant-replace      ; 0 → 1, 1 → 0
               :return-nil))          ; body → nil
;; Output:
;; Mutation Score: 94.2% (47/50 mutants killed)
;; Survivors:
;;   compiler.lisp:342 - (< x y) → (<= x y) - NOT caught
```

### Equivalence / Optimization Testing (FR-030)

```lisp
;; Verify transformations preserve semantics
(assert-equivalent-execution "(+ (* 2 3) 4)"
  :direct    #'compile-string
  :cps       #'cps-compile
  :optimized #'optimize-compile)

;; Round-trip: parse → compile → decompile → compile → compare
(assert-roundtrip-preserves
  '(let ((x 1)) (+ x 2))
  :stages '(:parse :compile :decompile))
```

### Random Program Generation / Fuzzing (FR-031)

```lisp
(deftest-fuzz compiler-robustness
  :count 1000              ; generate 1000 programs
  :max-depth 5             ; AST depth limit
  :timeout-per-test 1      ; seconds
  :features '(:arithmetic :let :if :lambda :funcall :progn)
  (let ((prog (gen-random-program features depth)))
    (assert-no-crash (compile-string prog))
    (when (compilable-p prog)
      (assert-terminates (run-string prog)))))
;; gen-random-program uses CL grammar rules to produce valid s-expressions
```

### Cross-Backend Verification (FR-032)

```lisp
(assert-backends-agree "(+ 1 2)"
  :backends '(:x86-64 :aarch64))

(deftest-cross-backend arithmetic
  :expressions '("(+ 1 2)" "(* 3 4)" "(- 10 5)"))
;; Compiles & executes on both backends, compares results
```

### Contract / Invariant Testing (FR-033)

```lisp
;; Invariants checked automatically after every test
(definvariant vm-register-sanity
  :after-each t
  (let ((state (last-vm-state)))
    (assert-true (<= (hash-table-count (vm-registers state))
                     *max-registers*))
    (assert-null (vm-call-stack state))))
```

### Determinism Testing (FR-034)

```lisp
;; Verify N compilations produce identical instruction sequences
(assert-deterministic 10
  (compile-string "(defun f (x) (+ x 1))"))
;; Catches hash-table ordering, uninitialized memory, etc.
```

### Code Coverage (FR-035)

```lisp
(run-suite 'cl-cc-suite :coverage t)
;; Output:
;; Coverage Report:
;;   src/compiler.lisp:  87.3% (412/472 forms)
;;   src/vm.lisp:        92.1% (289/314 forms)
;;   src/frontend.lisp:  78.5% (198/252 forms)
;;   Total:              86.2%
;; Uses sb-cover (SBCL built-in instrumentation)
```

### Metamorphic Testing (FR-036)

```lisp
;; Define metamorphic relations on compiled programs
(defmetamorphic commutativity
  :transform (lambda (expr)
    (destructuring-bind (op a b) (cdr expr)
      `(,op ,b ,a)))
  :relation #'=
  :applicable-when (lambda (expr)
    (member (cadr expr) '(+ *))))
;; For every (+ a b) in test suite, auto-verifies:
;;   (run-string "(+ a b)") = (run-string "(+ b a)")
```

### Parallel & Random Execution (FR-037, FR-038)

```lisp
;; Default: parallel workers + randomized order
(run-suite 'cl-cc-suite)
;; TAP output:
;;   # Seed: 3748291056
;;   # Workers: 8
;;   TAP version 13
;;   1..N
;;   ok 3 - vm-exec-subtraction        ← completion order, not definition order
;;   ok 1 - vm-exec-arithmetic
;;   not ok 7 - vm-exec-division

;; Fix seed for reproducibility
(run-suite 'cl-cc-suite :seed 3748291056)

;; Control worker count
(run-suite 'cl-cc-suite :workers 4)

;; Opt-out: sequential + fixed order (for debugging)
(run-suite 'cl-cc-suite :parallel nil :random nil)

;; Parallel + random is compatible with all other options
(run-suite 'cl-cc-suite :repeat 3 :seed 42)   ; flaky detection, fixed seed
(run-suite 'cl-cc-suite :coverage t)           ; coverage disables parallel (sb-cover constraint)
```

### Runner

```lisp
(run-suite 'cl-cc-suite)                             ; parallel+random (default)
(run-suite 'cl-cc-suite :seed N)                     ; reproducible random order
(run-suite 'cl-cc-suite :workers N)                  ; explicit worker count
(run-suite 'cl-cc-suite :parallel nil :random nil)   ; sequential, fixed order
(run-suite 'cl-cc-suite :repeat 3)                   ; flaky detection
(run-suite 'cl-cc-suite :update-snapshots t)         ; update snapshots
(run-suite 'cl-cc-suite :tags '(:compiler))          ; tag filtering
(run-suite 'cl-cc-suite :coverage t)                 ; coverage (forces :parallel nil)
(run-tests)                                           ; alias for main suite
(run-mutation-test :target "src/compiler.lisp" :suite 'cl-cc-suite) ; mutation testing
```

## TAP Output Format

```
# Seed: 3748291056
# Workers: 8
TAP version 13
1..N
ok 3 - vm-exec-subtraction
ok 1 - vm-exec-arithmetic
not ok 7 - vm-exec-division
  ---
  message: "assert-= failed"
  expected: 5
  actual: 4
  form: (run-string "(/ 10 2)")
  at: compiler-tests.lisp
  ...
ok 4 - vm-exec-skip # SKIP reason
not ok 5 - vm-exec-pending # TODO not implemented
ok 6 - vm-arithmetic [+ 3+4]
ok 2 - vm-let-binding > simple binding
ok 8 - vm-nested-arithmetic # SKIP dependency failed
ok 9 - arithmetic-combos [+ 0 0]
# To reproduce this run: (run-suite 'cl-cc-suite :seed 3748291056)
```

Notes on parallel TAP output:
- TAP test numbers reflect **definition order** (ok 1, ok 2, ...) even when printed out of order
- Lines within a single test result (the YAML block) are always contiguous — flushed atomically
- `1..N` plan line is printed before any results

## Non-Functional Requirements

| ID | Requirement |
|---|---|
| NFR-001 | Zero external dependency (pure ANSI CL + minimal SBCL extensions) |
| NFR-002 | All existing 1144 tests pass after migration |
| NFR-003 | TAP version 13 compliant output |
| NFR-004 | Framework total < 2000 lines (5 files combined) |
| NFR-005 | SBCL extensions limited to: `sb-ext:with-timeout`, `sb-ext:seed-random-state`, `sb-cover`, allocation tracking, `sb-thread` (parallel execution), `sb-thread:make-mutex`, `sb-thread:with-mutex` (TAP output locking) |

## File Layout

| File | Role | Est. Lines |
|---|---|---|
| `tests/framework.lisp` | Core: suite, deftest, assert-*, TAP, fixture, skip, dependencies, invariants | ~500 |
| `tests/framework-advanced.lisp` | Parameterized, nesting, snapshot, timeout, values, combinatorial, flaky, pipeline | ~400 |
| `tests/framework-compiler.lisp` | Compiler DSL, differential, pattern matching, performance, cross-backend, determinism, equivalence | ~400 |
| `tests/framework-meta.lisp` | Mutation testing engine, code coverage (sb-cover), metamorphic testing | ~400 |
| `tests/framework-fuzz.lisp` | Random CL program generator, fuzzing harness, grammar rules | ~300 |
| `tests/snapshots/` | Snapshot files directory | — |

## Migration Mapping

| FiveAM Form | Count | New Form | Mechanical? |
|---|---|---|---|
| `(test name ...)` | ~600 | `(deftest name ...)` | Yes |
| `(is (= e a))` | 501 | `(assert-= e a)` | Yes |
| `(is (eq e a))` | 214 | `(assert-eq e a)` | Yes |
| `(is (equal e a))` | 209 | `(assert-equal e a)` | Yes |
| `(is (typep o t))` | 117 | `(assert-type t o)` | Yes (arg swap) |
| `(is (not x))` | 104 | `(assert-false x)` | Yes |
| `(is (null x))` | 44 | `(assert-null x)` | Yes |
| `(is (search ...))` | 43 | `(assert-true (search ...))` | Yes |
| `(is (string= e a))` | 30 | `(assert-string= e a)` | Yes |
| `(is (getf ...))` | 23 | `(assert-true (getf ...))` | Yes |
| `(is (pred ...))` other | ~86 | `(assert-true (pred ...))` | Yes |
| `(is-true x)` | 11 | `(assert-true x)` | Yes |
| `(signals cond form)` | 16 | `(assert-signals cond form)` | Yes |
| `(def-suite ...)` | 5 | `(defsuite ...)` | Yes |
| `(in-suite ...)` | 16 | `(in-suite ...)` | No change |
| `(run! ...)` | 1 | `(run-suite ...)` | Yes |
| `fiveam:for-all` | ~30 | Custom `for-all` (existing) | Prefix removal |

## Constraints

- `tests/call-conv-tests-fixed.lisp` and `tests/control-flow-tests-fixed.lisp` are not in ASDF (unused?)
- `tests/pbt/framework.lisp:486` uses `sb-ext:seed-random-state` (SBCL-specific, acceptable)
- `assert-type` arg order: `(typep obj type)` → `(assert-type type obj)` (type first)
- `:timeout` uses `sb-ext:with-timeout` (SBCL-specific, acceptable)
- `assert-no-consing` uses SBCL allocation tracking (SBCL-specific, acceptable)
- `assert-same-as-sbcl` uses host CL `eval` — only works on SBCL (acceptable for this project)
- Pattern matching `_` wildcard is a convention, not a CL keyword — implemented as symbol comparison
- Mutation testing operates on AST level (not source text) — uses `lower-sexp-to-ast` + mutation rules
- `sb-cover` requires `(declaim (optimize (sb-cover:store-coverage-data 3)))` in target files
- Random program generator uses CL grammar rules — does NOT generate arbitrary strings
- `defmetamorphic` collects applicable expressions from test suite at suite-run time
- Cross-backend testing requires both x86-64 and aarch64 backends to be loaded

## Task Breakdown

### Phase 1: Core Framework

- **T1.1**: `tests/framework.lisp` — defsuite, in-suite, deftest (with :timeout, :depends-on), all core assert-* macros, TAP v13 output, YAML diagnostic, defbefore/defafter, skip/pending, run-suite/run-tests (with :repeat, :update-snapshots, :tags, :parallel, :random, :seed, :workers); parallel worker pool via `sb-thread`; per-test output buffering with `sb-thread:make-mutex`; Fisher-Yates shuffle seeded via `sb-ext:seed-random-state`
- **T1.2**: `cl-cc.asd` — remove `:fiveam`, add framework files to components
- **T1.3**: `tests/package.lisp` — remove `:fiveam` from `:use`, export framework symbols

### Phase 1.5: Advanced Features (depends: Phase 1)

- **T1.5.1**: `tests/framework-advanced.lisp` — deftest-each (parameterized), testing (nesting), assert-values, assert-snapshot + `tests/snapshots/`, deftest-combinatorial, flaky detection logic, deftest-pipeline
- **T1.5.2**: `tests/framework-compiler.lisp` — assert-compiles-to, assert-evaluates-to, assert-macro-expands-to, assert-infers-type, assert-same-as-sbcl, deftest-differential, assert-instructions-match, assert-ast-matches, assert-faster-than, assert-no-consing, assert-backends-agree, deftest-cross-backend, assert-deterministic, assert-equivalent-execution, assert-roundtrip-preserves

### Phase 1.75: Meta-Testing & PBT (depends: Phase 1)

- **T1.75.1**: `tests/pbt/framework.lisp` — Add shrinking to existing PBT framework (integer shrink: binary search, list shrink: sublists, string shrink: substrings)
- **T1.75.2**: `tests/framework-meta.lisp` — run-mutation-test (mutation engine with AST-level mutation rules), sb-cover integration (:coverage option), defmetamorphic (metamorphic relation definitions + auto-verification), definvariant (invariant registry + post-test checking)
- **T1.75.3**: `tests/framework-fuzz.lisp` — gen-random-program (CL grammar-based random program generator), deftest-fuzz, assert-no-crash, assert-terminates, compilable-p

### Phase 2: PBT Migration (depends: Phase 1)

- **T2.1**: `tests/pbt/package.lisp` — remove `:fiveam`
- **T2.2**: `tests/pbt/framework.lisp` — `fiveam:test` → `deftest`, `fiveam:is nil` → `assert-true nil`, `fiveam:run` → `run-suite`

### Phase 3: Test File Migration (depends: Phase 1, parallelizable)

- **T3.1**: `compiler-tests.lisp` (~652 assertions, largest file)
- **T3.2**: `macro-tests.lisp` (~184 assertions)
- **T3.3**: `type-tests.lisp` (~99 assertions)
- **T3.4**: `vm-heap-tests.lisp` (~78 assertions)
- **T3.5**: `clos-tests.lisp` (~74 assertions)
- **T3.6**: `closure-tests.lisp` (~44 assertions)
- **T3.7**: `regalloc-tests.lisp` (~37 assertions)
- **T3.8**: `cps-tests.lisp` (~34 assertions)
- **T3.9**: `call-conv-tests.lisp` (~31 assertions)
- **T3.10**: `control-flow-tests.lisp` (~26 assertions)
- **T3.11**: `ast-roundtrip-tests.lisp` (~9 assertions)
- **T3.12**: `prolog-tests.lisp` (~8 assertions)
- **T3.13**: PBT test files — `fiveam:for-all` → `for-all`

### Phase 4: Verification (depends: Phase 2, 3)

- **T4.1**: `nix develop --command make test` — all 1144 tests pass

### Phase 5: Advanced Test Adoption (optional, depends: Phase 1.5, 4)

- **T5.1**: Consolidate repetitive tests in `compiler-tests.lisp` using `deftest-each`
- **T5.2**: Add snapshot tests for VM/codegen output
- **T5.3**: Replace manual `multiple-value-list` patterns with `assert-values`
- **T5.4**: Add `deftest-pipeline` tests for key compilation paths
- **T5.5**: Add `deftest-differential` tests comparing cl-cc vs SBCL
- **T5.6**: Add `assert-instructions-match` for critical codegen patterns
- **T5.7**: Add `deftest-combinatorial` for arithmetic operator coverage
- **T5.8**: Enable `:shrink t` on existing PBT properties
- **T5.9**: Add `deftest-fuzz` for compiler robustness testing
- **T5.10**: Add `defmetamorphic` relations for commutativity, associativity
- **T5.11**: Add `definvariant` for VM register/stack sanity checks
- **T5.12**: Run `run-mutation-test` on `src/compiler.lisp` to measure test quality
- **T5.13**: Add `deftest-cross-backend` for x86-64/aarch64 agreement
- **T5.14**: Add `assert-deterministic` for compiler output stability

## Feature Summary

| Category | Features | Count |
|---|---|---|
| Core | defsuite, deftest, 10 assert-*, TAP, fixture, skip/pending, runner | 13 |
| Advanced | parameterized, nesting, snapshot, values, timeout, tags, combinatorial, flaky, pipeline | 7 |
| Compiler | DSL helpers, differential, pattern matching, performance, cross-backend, determinism, equivalence | 7 |
| Infrastructure | PBT shrinking, dependencies, combinatorial, flaky detection, parallel execution, random order | 6 |
| Meta-Testing | mutation testing, fuzzing, invariants, coverage, metamorphic | 5 |
| **Total** | | **36 FR** |

## Metrics

| Metric | Score |
|---|---|
| Feasibility | 85 (large scope but all features implementable in pure CL+SBCL; mutation/fuzz are most complex) |
| Objectivity | 90 |

## Practical Limits

This specification represents the **exhaustive set of testing techniques implementable in pure Common Lisp + SBCL extensions** for a compiler project. Beyond this, the next level would require:

- External tool integration (AFL/LibFuzzer for coverage-guided fuzzing)
- Theorem prover integration (ACL2, Coq) for formal verification
- Distributed test execution across machines
- Machine learning for test prioritization/selection

These are outside the scope of a zero-dependency test framework.
