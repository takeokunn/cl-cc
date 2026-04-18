;;;; cl-cc-test.asd
;;;; CL-CC test systems.
;;;;
;;;; Loaded by cl-cc.asd's eval-when block so that `--load cl-cc.asd`
;;;; makes :cl-cc/test and :cl-cc/test-clos available.

(asdf:defsystem :cl-cc/test
  :description "CL-CC tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc/bin :cl-cc/tests-framework)
  :serial t
  :components
  (;; Unit tests — each module now lives in its workspace's tests/ dir
   (:module "framework-meta-tests"
    :pathname "packages/testing/framework/tests"
    :serial t
    :components
    ((:file "framework-meta-tests")
     (:file "framework-meta-coverage-tests")
     (:file "prolog-fixture-invariant-tests")
     (:file "timing-tests")
     (:file "persistent-tests")
     (:file "persistent-tests-2")
     (:file "framework-runner-tests")
     (:file "framework-parallel-tests")))
   (:module "cli-tests"
    :pathname "packages/cli/tests"
    :serial t
    :components
    ((:file "test-support")      ; shared CLI test helpers (fake-quit, make-cli-parsed)
     (:file "args-tests")
     (:file "cli-tests")
     (:file "main-tests")
     (:file "main-dump-tests")
     (:file "main-utils-tests")))
   (:module "vm-tests"
    :pathname "packages/engine/vm/tests"
    :serial t
    :components
    ((:file "vm-instructions-tests")
     (:file "list-tests")       ; defines make-test-vm / exec1 helpers
     (:file "list-coerce-tests")
     (:file "array-tests")
     (:file "vm-execute-tests")
     (:file "vm-execute-tests-2")
     (:file "vm-call-tests")
     (:file "primitives-tests") ; execute-instruction for type predicates + arithmetic
     (:file "vm-transcendental-tests") ; transcendental math ops
     (:file "vm-numeric-tests") ; numeric tower + environment queries
     (:file "vm-extensions-tests") ; symbol plist + progv + generic arithmetic
     (:file "vm-bitwise-tests") ; bitwise integer instructions
     (:file "vm-clos-tests")    ; collect-inherited-slots, compute-CPL, eql-dispatch-index
     (:file "vm-clos-execute-tests") ; execute-instruction for class-def/make-obj/slot-read/write/boundp
     (:file "vm-run-tests") ; vm-error-type-matches-p + vm2 state/reg structure
     (:file "vm-run-vm2-tests") ; vm2 run-vm execution: basic ops, immediate ops, compare ops
     (:file "vm-run-fusion-tests") ; chained arithmetic, superinstruction fusion, extended run-vm
     (:file "vm-dispatch-tests") ; vm-classify-arg + vm-generic-function-p
     (:file "vm-dispatch-gf-tests") ; EQL specializer helpers from vm-dispatch-gf.lisp
     (:file "vm-dispatch-gf-multi-tests") ; %vm-gf-uses-composite-keys-p + vm-resolve-gf-method
     (:file "vm-bridge-tests")  ; hash-table-values, slot-definition-*, rt-plist-put, etc.
     (:file "vm-opcodes-tests")      ; vm2-state, vm2-reg-get/set, bigram analysis, fusion
     (:file "vm-opcodes-defs-tests") ; make-vm-state, vm-reg-get/set, vm-state-registers, run-vm-with-bigrams
     (:file "vm-tests") ; vm.lisp core state + helper coverage
     (:file "package-tests") ; cl-cc package export smoke tests
     (:file "conditions-tests")
     (:file "hash-tests")
     (:file "symbols-tests")
     (:file "strings-tests")
     (:file "format-tests")
     (:file "io-tests")))
   (:module "ast-tests"
    :pathname "packages/foundation/ast/tests"
    :serial t
    :components
    ((:file "ast-tests")
     (:file "ast-analysis-tests")))
   (:module "parse-tests"
    :pathname "packages/frontend/parse/tests"
    :serial t
    :components
    ((:file "cl-parser-tests")
     (:file "cl-parser-error-tests")        ; lower-sexp special forms + arity error cases
     (:file "cl-parser-ast-tests")
     (:file "cl-parser-new-tests")
     (:file "cl-parser-new-extended-tests") ; ast-to-sexp roundtrip, slot-spec, lambda edge cases, pipeline
     (:module "cl"
      :serial t
      :components
      ((:file "lower-tests")
       (:file "lower-arithmetic-tests")    ; n-ary arith, let, lambda, block, setf forms
       (:file "parser-roundtrip-tests")    ; ast-to-sexp roundtrip for all node types
       (:file "grammar-tokenstream-tests")  ; token-stream helpers + CST builders
       (:file "grammar-parser-tests")))     ; parse-cl-atom/form/list-form + parse-cl-source
     (:file "cst-tests")
     (:file "lexer-tests")
     (:file "lexer-dispatch-tests")  ; feature conditionals, hash dispatch, skip helpers
     (:file "grammar-tests")
     (:file "grammar-special-form-tests")
     (:file "pratt-tests")
     (:file "pratt-pipeline-tests") ; CST positions, parse-cl-source/all-forms/source, parse-and-lower, pratt-parse-expr/list-until
     (:file "php-tests")
     (:module "php"
      :serial t
      :components
      ((:file "parser-tests")
       (:file "grammar-tests")
       (:file "grammar-stmt-tests")))
     (:file "combinator-tests")
     (:file "combinator-tests-2")
     (:file "parser-combinator-tests")
     (:file "incremental-tests")
     (:file "cst-to-ast-tests")
     (:file "diagnostics-tests")))
   (:module "prolog-tests"
    :pathname "packages/foundation/prolog/tests"
    :serial t
    :components
    ((:file "prolog-data-tests")
     (:file "prolog-peephole-tests")
     (:file "dcg-tests")))
   (:module "expand-tests"
    :pathname "packages/frontend/expand/tests"
    :serial t
    :components
    ((:file "macro-tests")
     (:file "macro-definition-tests")
     (:file "macro-assignment-tests")
     (:file "macro-multiple-value-tests")
     (:file "macros-control-flow-tests")
     (:file "macros-control-flow-loop-tests")
     (:file "macro-lambda-list-tests")
     (:file "expander-lambda-list-defaults-tests")
     (:file "expander-core-tests")
     (:file "expander-data-tests")
     (:file "expander-test-support")
     (:file "expander-basic-tests")
     (:file "macros-basic-check-type-tests")
     (:file "macros-basic-list-tests")
     (:file "macros-basic-setf-tests")
     (:file "expander-setf-tests")
     (:file "expander-setf-places-tests")
     (:file "expander-control-tests")
     (:file "expander-array-tests")
     (:file "expander-typed-tests")
     (:file "expander-typed-params-tests") ; lambda-list-has-typed-p, strip-typed-params, register-function-type
     (:file "expander-defclass-tests")
     (:file "expander-binding-tests")
     (:file "expander-control-helpers-tests")
     (:file "expander-definitions-function-tests")
     (:file "expander-definitions-forms-tests")
     (:file "expander-definitions-type-tests")
     (:file "expander-definitions-rounding-tests")
     (:file "expander-definitions-constant-tests")
     (:file "expander-definitions-tests")
     (:file "expander-numeric-tests")
     (:file "expander-comparison-tests")  ; = < > <= >= /= and char comparison chaining
     (:file "expander-definitions-helpers-tests")
     (:file "expander-helpers-tests")
     (:file "expander-sequence-tests")
     (:file "expander-setf-places-helpers-tests")
     (:file "expander-tail-tests")
     (:file "defstruct-tests")
     (:file "loop-tests")
     (:file "loop-data-tests")
     (:file "loop-parser-tests")
     (:file "loop-emitters-tests")
     (:file "macro-rotatef-tests")
     (:file "macro-psetf-tests")
     (:file "macro-shiftf-tests")
     (:file "macro-ecase-tests")
     (:file "macro-etypecase-tests")
     (:file "macro-progv-tests")
     (:file "macro-define-modify-macro-tests")
     (:file "macros-cxr-tests")
     (:file "macros-introspection-tests")
     (:file "macros-list-utils-tests")
     (:file "macros-restarts-tests")
     (:file "macros-setops-tests")
     (:file "macros-stdlib-core-tests")
     (:file "macros-stdlib-tests")
     (:file "macros-stdlib-bind-error-tests")
     (:file "macros-stdlib-sequence-map-tests")
     (:file "macros-stdlib-io-tests")
     (:file "macros-stdlib-ansi-tests")   ; with-accessors, assert, define-condition, with-*-string, with-standard-io-syntax
     (:file "macros-stdlib-utils-tests")  ; tailp/ldiff/copy-alist/tree-equal/get-properties/nunion/nsubst*/nstring*/array-*
     (:file "macros-filesystem-tests")
     (:file "macros-compat-array-tests")      ; array compat wrappers
     (:file "macros-compat-tests")            ; remaining ANSI CL compat macros
     (:file "macros-compat-clos-tests")       ; CLOS protocol + MOP introspection macros
     (:file "macros-plist-tests")    ; getf/remf/%plist-put
     (:file "macros-list-compat-tests") ; subst/vector/member-if/maphash
     (:file "macros-hof-tests")      ; HOF macro expansions split from stdlib
     (:file "macros-hof-search-tests") ; position-if, count-if, assoc, rassoc-if variants
     (:file "macros-sequence-tests")))
   (:module "type-tests"
    :pathname "packages/foundation/type/tests"
    :serial t
    :components
    ((:file "type-tests")
     (:file "type-inference-tests")
     (:file "type-effect-tests")
     (:file "type-phase-tests")
     (:file "type-2026-nodes-tests")    ; 2026 type nodes (rigid/product/variant/exists/mu/HKT) + substitution API
     (:file "kind-tests")
     (:file "multiplicity-tests")
     (:file "row-tests")
     (:file "subtyping-tests")
     (:file "subtyping-extended-tests")  ; extended is-subtype-p, type-join/meet lattice coverage
     (:file "effect-tests")
     (:file "constraint-tests")
     (:file "solver-tests")
     (:file "solver-collect-tests")           ; collect-constraints: all typecase arms + gradual fallback
     (:file "representation-tests")
     (:file "substitution-tests")          ; substitution data structure, zonk, composition
     (:file "unification-tests")           ; product/intersection/constructor/effect-row unification
     (:file "type-children-tests")         ; type-children / type-bound-var data layer
     (:file "types-extended-coverage-tests")
     (:file "typeclass-tests")
     (:file "printer-tests")
     (:file "parser-tests")
     (:file "parser-typed-tests")
     (:file "inference-tests")
     (:file "inference-effect-tests")
     (:file "exhaustiveness-tests")))
   (:module "ir-tests"
    :pathname "packages/foundation/ir/tests"
    :serial t
    :components
    ((:file "ir-types-tests")
     (:file "ir-block-tests")
     (:file "ir-ssa-advanced-tests")
     (:file "ir-printer-tests")))
   (:module "compile-tests"
    :pathname "packages/engine/compile/tests"
    :serial t
    :components
    ((:file "cps-tests")
     (:file "cps-ast-tests")
     (:file "cps-ast-semantic-tests") ; semantic eval: binop/if/evaluable-forms/setq + structural/dispatch/sequence/block/catch/flet/unwind-protect
     (:file "cps-ast-extended-tests") ; OOP/mutation: setq/defvar/make-instance/defclass/defgeneric/defmethod
     (:file "cps-ast-functional-tests") ; functional: quote/the/values/mvb/apply/call/entry-points
     (:file "builtin-registry-tests")
     (:file "builtin-registry-data-tests")
     (:file "fr-586-set-tests")
     (:file "builtin-registry-data-ext-tests")   ; extended calling-convention data tables
     (:file "closure-tests")
     (:file "closure-escape-tests")  ; escape analysis + closure-sharing utilities
     (:file "context-tests")
     (:file "codegen-tests")
     (:file "codegen-fold-tests")       ; %fold-ast-binop, *compile-time-eval-fns*, %evaluate-ast
     (:file "codegen-phase2-helpers")
     (:file "codegen-core-tests")
     (:file "codegen-core-array-sink-tests") ; array non-escape/sink: variable-aset, branch-local, escape/shadow scenarios + single-inst/the/hole/narrows
     (:file "codegen-type-predicate-tests")
     (:file "codegen-core-let-tests")     ; %ast-let-binding-ignored-p, %ast-cons-call-p, %ast-lambda-bound-names, sink-if
     (:file "codegen-functions-tests")
     (:file "codegen-clos-tests")
     (:file "codegen-clos-slot-tests")  ; slot-value/set-slot-value/branch-local sink + phase2 CLOS helpers
     (:file "codegen-control-tests")
     (:file "codegen-calls-tests")   ; %try-compile-funcall/apply/noescape, %compile-normal-call, GF dispatch
     (:file "codegen-locals-tests")  ; target-instance, %compile-body-with-tail, type-check-ast
     (:file "codegen-io-tests")
     (:file "codegen-hash-table-tests")
     (:file "codegen-slot-predicates-tests")
     (:file "codegen-string-kwargs-tests")
     (:file "phase2-handler-tests")
     (:file "codegen-phase2-tests")
     (:file "stdlib-source-tests")
     (:file "pipeline-native-tests")))
   (:module "optimize-tests"
    :pathname "packages/engine/optimize/tests"
    :serial t
    :components
    ((:file "optimizer-tables-tests")   ; fold tables, derived lists, opt-inst-read-regs/dst
     (:file "optimizer-strength-tests") ; opt-power-of-2-p, opt-pass-strength-reduce, bswap/rotate
     (:file "optimizer-licm-tests")    ; opt-inst-loop-invariant-p, %opt-pre-*, opt-pass-licm
     (:file "optimizer-copyprop-tests")
     (:file "optimizer-strength-ext-tests") ; reassociation pass + batch concatenate
     (:file "optimizer-cse-gvn-tests")      ; CSE, GVN, dead labels, leaf detection
     (:file "optimizer-dataflow-tests")     ; SCCP: env copy/equal/merge, fold-inst, pass-sccp
     (:file "optimizer-memory-tests") ; alias analysis, interval arithmetic, DSE, store-to-load
     (:file "optimizer-flow-tests")   ; DCE, jump threading, unreachable elim, block/tail merge
     (:file "optimizer-pipeline-tests") ; parse-pipeline-string, converged-p, adaptive-iters, verify
     (:file "optimizer-inline-tests")      ; opt-max-reg-index, opt-make-renaming, opt-collect-function-defs, call-graph
     (:file "optimizer-inline-pass-tests")   ; memo utils, body-inst-set/labels, reachability
     (:file "optimizer-inline-pass-tests-2") ; inline cost/policy (inst-cost, body-cost, threshold, eligible-p), global-dce, pass-inline
     (:file "optimizer-inline-pass-ext-tests") ; extended inline policy + full pass tests
     (:file "effects-tests")
     (:file "cfg-tests")
     (:file "ssa-tests")
     (:file "egraph-tests")
     (:file "egraph-extraction-tests")
     (:file "egraph-rules-tests")
     (:file "egraph-rules-bitwise-tests")))
   (:module "emit-tests"
    :pathname "packages/backend/emit/tests"
    :serial t
    :components
    ((:file "mir-tests")
     (:file "mir-target-tests")          ; printer smoke tests + target-desc (x86-64/aarch64/riscv64/wasm32)
     (:file "regalloc-tests")
     (:file "wasm-tests")
     (:file "wasm-ir-tests")        ; wasm IR instruction structs
     (:file "wasm-types-tests")     ; wasm type encoding helpers: section IDs, primitives, GC refs, heap types, type defs, mutability, core opcodes
     (:file "wasm-opcodes-tests")   ; wasm opcode tests: i32/i64/f64 ops, conversion, reference, GC opcodes, named type indices, uniqueness
     (:file "aarch64-codegen-tests")
     (:file "aarch64-emit-tests")   ; aarch64 native emit target + regalloc
     (:file "aarch64-encoding-tests") ; aarch64 instruction encoding
     (:file "target-tests")
     (:file "wasm-extract-tests")
     (:file "wasm-trampoline-tests")
     (:file "wasm-trampoline-build-tests") ; collect-registers-from-instructions, build-trampoline-body, build-all-wasm-functions
     (:file "x86-64-regs-tests")     ; red-zone-spill-p, vm-reg-to-x86/xmm, float-vregs, const-to-int
     (:file "x86-64-sequences-tests") ; emit-idiv-r11/cqo/sequence, sal/sar/ror-cl, cmov, bool-ops
     (:file "x86-64-codegen-tests")
     (:file "x86-64-codegen-emitter-tests")
     (:file "x86-64-codegen-insn-tests") ; bswap/add/select/jump-zero/logcount/integer-length/call emitters
     (:file "x86-64-encoding-tests") ; x86-64 rex/modrm/byte/dword/qword encoding
     (:file "x86-64-encoding-size-tests") ; x86-64 instruction size, label offsets, program output
     (:file "x86-64-emit-tests")     ; x86-64 emit target + instruction emit methods
     (:file "x86-64-emit-logical-tests") ; logical/bitwise emitters + byte-count checks
     (:file "x86-64-emit-ops-tests")    ; arithmetic/comparison/unary emitters + byte-count checks
     (:file "elf-tests")             ; ELF binary format helpers: constants, buffer, strtab, builder
     (:file "elf-extended-tests")    ; ELF extended: additional buffer, strtab, builder, finalize tests
     (:file "macho-tests")           ; Mach-O binary format helpers: constants, structures, buffer, serialization
     (:file "macho-builder-tests")   ; Mach-O builder API, binary output, extended structures and serialization
     (:file "calling-convention-tests")))
   (:module "runtime-tests"
    :pathname "packages/backend/runtime/tests"
    :serial t
    :components
    ((:file "runtime-tests")
     (:file "runtime-tests-2")
     (:file "runtime-advanced-tests")
     (:file "gc-tests")
     (:file "gc-sweep-major-tests")    ; %gc-sweep-old-space (dead/live) + rt-gc-major-collect (counter/reclaim/preserve/stats)
     (:file "heap-tests")
     (:file "heap-gc-tests")          ; Instruction serialization, roundtrip, integration list-building
     (:file "heap-trace-tests")       ; Card table + address predicates (heap-trace.lisp)
     (:file "gc-write-barrier-tests") ; SATB + card write barrier (gc-write-barrier.lisp)
     (:file "value-tests")
     (:file "frame-tests")))
   (:module "bytecode-tests"
    :pathname "packages/backend/bytecode/tests"
    :serial t
    :components
    ((:file "encode-tests")
     (:file "encode-ops-objects-tests")
     (:file "decode-tests")))
   (:module "migration-safety"
    :pathname "packages/umbrella/tests"
    :components
    ((:file "migration-safety-tests")))
   (:module "integration"
    :pathname "tests/integration"
    :serial t
    :components
    ((:file "compiler-selfhost-fixtures") ; defparameter fixtures for selfhost tests (no tests)
     (:file "compiler-tests")
     (:file "compiler-tests-extended")   ; selfhost smoke: optimizer-pipeline, macro-system, type-checker
     (:file "compiler-tests-runtime")    ; destructuring-bind, append, consp, type predicates
     (:file "fr-555-copy-structure-tests")
     (:file "compiler-tests-selfhost")   ; CLOS pipeline, generic functions, mapcar
     (:file "closure-tests")
     (:file "call-conv-tests")
     (:file "control-flow-tests")
     (:file "clos-tests")
     (:file "stream-tests")
     ;; macros/expand integration tests (run VM; moved from unit/expand during migration)
     (:file "loop-macro-tests")
     (:file "loop-macro-runtime-tests")  ; extended LOOP: hash, initially, synonym pairs
     (:file "macros-basic-mvb-tests")
     (:file "macros-mutation-tests")
     (:file "macros-sequence-fold-tests")
     (:file "macros-stdlib-list-set-tests")
     (:file "predicate-tests")
     ;; compile/codegen integration test (runs VM; moved from unit/compile during migration)
     (:file "codegen-runtime-tests")
     ;; optimizer integration tests (runs VM; moved from unit/optimize during migration)
     (:file "optimizer-tests")
     (:file "optimizer-lowlevel-tests")  ; direct opt-pass-fold, type predicates, strength-reduce
     ;; pipeline integration tests (parse pipeline, run-string, incremental)
     (:file "pipeline-tests")
      ;; prolog integration tests (queries, rules, goal solving)
      (:file "prolog-tests")
     ;; standalone system load verification
     (:file "standalone-load-tests")
     (:module "pbt"
      :serial t
      :components
      ((:file "package")
       (:file "framework")
       (:file "generators")
       (:file "vm-pbt-tests")
       (:file "cps-pbt-tests")
       (:file "ast-pbt-tests")
       (:file "macro-pbt-tests")
       (:file "prolog-pbt-tests")
       (:file "vm-heap-pbt-tests")))))
   (:module "e2e"
    :pathname "tests/e2e"
    :serial t
    :components
    ((:file "selfhost-tests"))))
  :perform (asdf:test-op (op c)
              (declare (ignore op c))
              (uiop:symbol-call :cl-cc/test 'run-tests)))

(asdf:defsystem :cl-cc/test-clos
  :description "CL-CC isolated CLOS integration tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc/bin :cl-cc/tests-framework)
  :serial t
  :components
  ((:module "integration"
    :pathname "tests/integration"
    :serial t
    :components
    ((:file "clos-tests")))))
