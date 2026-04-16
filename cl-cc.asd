;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection - ASDF System Definition
;;;;
;;;; Directory structure mirrors the compilation pipeline:
;;;;   parse → expand → vm → type → compile → optimize → emit → runtime → compile-pipeline
;;;;
;;;; Phase 1 of the package-by-feature monorepo migration extracts feature
;;;; subsystems into sibling .asd files. They are pre-loaded here so the
;;;; existing `--load cl-cc.asd` build entry continues to work without
;;;; needing to update Makefile/flake.nix.

(eval-when (:load-toplevel :execute)
  (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                             :name nil :type nil)))
    ;; Bootstrap must load first — provides 12 pre-interned symbols to prolog + compile
    (asdf:load-asd (merge-pathnames "src/bootstrap/cl-cc-bootstrap.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-ast.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-prolog.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-binary.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-runtime.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-bytecode.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-ir.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-mir.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-type.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-optimize.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-emit.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-expand.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-compile.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-parse.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-vm.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-vm-tests.asd" here))
    (asdf:load-asd (merge-pathnames "cl-cc-test-framework.asd" here))))

(asdf:defsystem :cl-cc
  :description "CL-CC: Common Lisp Compiler Collection"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-binary :cl-cc-runtime :cl-cc-bytecode :cl-cc-ir :cl-cc-mir :cl-cc-type :cl-cc-optimize :cl-cc-emit :cl-cc-expand :cl-cc-compile :cl-cc-vm)
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     ;; Stage 1: Source → CST → S-expressions
     ;; parse module extracted to cl-cc-parse.asd (Phase 3)
      ;; Stage 2: S-expressions → macro-expanded S-expressions
      (:module "expand"
       :serial t
       :components
          ((:file "macro-lambda-list") ; shared lambda-list parsing + destructuring helpers
           (:file "macro")           ; core: macro-env, defmacro machinery, macroexpansion
           (:file "macros-basic")    ; bootstrap: check-type/setf/list + value helpers
           (:file "macros-control-flow") ; bootstrap control-flow macros (when/unless/cond/do*)
           (:file "macros-control-flow-case") ; case/typecase macro expansion
           (:file "macros-mutation") ; push/pop/incf/decf split from stdlib
           (:file "loop-data")       ; LOOP: grammar tables — the "Prolog database"
           (:file "loop-parser-for") ; LOOP: token predicates, CPS utils, FOR sub-parsers
           (:file "loop-parser")     ; LOOP: CPS token parser → IR plist
           (:file "loop-emitters")   ; LOOP: IR → code-fragment tables
           (:file "loop")            ; LOOP: generator — assembles tagbody from IR
           (:file "macros-setops")   ; list/set operations split from stdlib
           (:file "macros-list-utils") ; ordering and list utility helpers
           (:file "macros-restarts") ; restart/condition protocol split from stdlib
           (:file "macros-introspection") ; equalp + implementation stubs
           (:file "macros-stdlib")       ; stdlib: numeric/control macros (1+, ecase, rotatef...)
           (:file "macros-stdlib-ansi")  ; ANSI CL Phase 1 (psetf, assert, define-condition...)
           (:file "macros-stdlib-utils") ; list/tree/string/array utility macros
           (:file "macros-cxr")      ; algorithmic CXR accessor registration
           (:file "macros-hof")         ; higher-order list/search helpers (map/find/remove)
           (:file "macros-hof-search")  ; position/count/assoc search HOFs
           (:file "macros-filesystem") ; file/IO/runtime stubs split from stdlib
           (:file "macros-filesystem-ext") ; pprint, readtable, debug/introspect, compile-file stubs
           (:file "macros-sequence")      ; sequences: copy/fill/replace/mismatch/delete/substitute
           (:file "macros-sequence-fold") ; sequences: reduce/nsubstitute/map-into/merge/last/search
           (:file "macros-list-compat") ; list/sequence compatibility helpers split from stdlib
           (:file "macros-plist")      ; property list helpers
           (:file "macros-compat")      ; pkg/declare/IO/hash/coerce/LTV/feature macros
           (:file "macros-compat-clos") ; CLOS protocol + MOP introspection + print macros
           (:file "macros-compat-array") ; array compat wrappers split from macros-compat
           (:file "expander-data")      ; expander: grammar tables + dispatch table declarations
           (:file "expander-defstruct") ; expander: defstruct expansion helpers
           (:file "expander-core")
           (:file "expander-helpers")   ; expander: shared helper functions extracted from expander.lisp
           (:file "expander-definitions-helpers") ; expander: lambda-list default expansion helper
           (:file "expander-control-helpers") ; expander: binding helpers for control forms
           (:file "expander-setf-places-helpers") ; expander: setf-place cons access helper
           (:file "expander-setf-places") ; expander: setf compound-place registration table
           (:file "expander")
           (:file "expander-definitions-forms")
           (:file "expander-basic")     ; core application handlers split from expander.lisp
           (:file "expander-definitions")
           (:file "expander-control")
           (:file "expander-tail")
           (:file "expander-numeric")
           (:file "expander-sequence")))
     ;; Stage 6: VM execution — all files now in cl-cc-vm.asd (:cl-cc-vm dependency)
     ;; Stage 3: type module extracted to cl-cc-type.asd (Phase 1 migration)
     ;; Stage 4: AST → VM IR (context/closure/cps/codegen — before optimize/emit)
     (:module "compile"
      :serial t
      :components
      (;; ir module extracted to cl-cc-ir.asd (Phase 1 migration)
       (:file "context")
       (:file "closure")
        (:file "cps")
        (:file "cps-ast")
        (:file "cps-ast-control")           ; control flow forms (unwind-protect, block, return-from, tagbody, catch, throw)
        (:file "cps-ast-extended")          ; OOP/mutation forms (setq, defvar, make-instance, defclass…)
        (:file "cps-ast-functional")        ; functional forms + entry points (split from cps-ast-extended)
         (:file "builtin-registry-data") ; Core alists: unary/binary/string-cmp/char-cmp
         (:file "builtin-registry-data-ext") ; Extended alists: I/O, stream, custom, ternary
         (:file "builtin-registry")          ; Registry struct + *builtin-registry* + registration
         (:file "builtin-registry-emitters") ; 27 emit-builtin-* emitter functions
         (:file "builtin-registry-dispatch") ; *builtin-emitter-table* + *convention-arity* + emit-registered-builtin
         (:file "codegen-core")         ; binop table + primitive/if compilation
         (:file "codegen-core-control") ; block/return-from + tagbody/go + setq/quote/the
         (:file "codegen-core-let")      ; let-binding analysis: predicates + walkers
         (:file "codegen-core-let-emit") ; let-binding emitters + compile-ast (ast-let)
         (:file "codegen-clos")    ; defclass + slot accessor compilation
         (:file "codegen-gf")      ; defgeneric/defmethod/make-instance/slot-value
         (:file "codegen-functions")        ; typed params + defmacro compilation
         (:file "codegen-functions-params") ; parameter-list helpers + compile-function-body
         (:file "codegen-functions-emit")  ; lambda/defun/defvar compile-ast methods
         (:file "codegen-phase2")  ; Phase 2 AST-introspecting builtin handlers
         (:file "codegen-control") ; control-flow + multiple-values compiler methods
         (:file "codegen-io")      ; Phase 2 stream/reader/printer handlers split from phase 2
         (:file "codegen-io-ext") ; Phase 2 array/string/format/file handlers split from codegen-io
         (:file "codegen-hash-table") ; hash-table handler cluster split from phase 2
         (:file "codegen-slot-predicates") ; CLOS slot predicate handlers split from phase 2
        (:file "codegen-string-kwargs") ; string comparison/case handlers split from phase 2
        (:file "codegen-fold")          ; compile-time constant fold + partial evaluator
        (:file "codegen-fold-optimize") ; %loc macro + optimize-ast fold pass
        (:file "codegen")       ; entry points + exception handling + multiple values
        (:file "codegen-calls") ; function call compilation: %try-compile-* + ast-call method
        (:file "codegen-locals"))) ; local fn bindings: flet/labels/ast-function + emit-assembly
     ;; Stage 5: VM IR → optimized VM IR
     (:module "optimize"
      :serial t
      :components
      ((:file "effects")          ; Phase 0: effect-kind bridge (type-effect-row → optimizer)
       (:file "cfg")              ; CFG construction + RPO + forward dominators + loop depths
       (:file "cfg-analysis")    ; post-dominators + critical edge split + dom frontiers + IDF
       (:file "cfg-layout")      ; flat instruction layout + hot/cold block ordering
       (:file "ssa")              ; Phase 1: SSA data structures + phi placement + renaming
       (:file "ssa-construction")  ; ssa-construct + ssa-destroy + round-trip
       (:file "egraph")             ; Phase 2: E-graph data + union-find + patterns + rules
       (:file "egraph-saturation")  ; saturation + extraction + VM instruction interface
       (:file "egraph-rules")          ; Phase 2: defrule macro + core rewrite rules
       (:file "egraph-rules-advanced") ; Advanced rules + egraph-builtin-rules + optimize-with-egraph
       (:file "optimizer-tables")   ; Data tables + predicates (loaded before passes)
       (:file "optimizer-algebraic") ; Algebraic identity rules + classification predicates + dead-label table
       (:file "optimizer-inline")      ; Analysis: collect-function-defs, call-graph, rename helpers
       (:file "optimizer-inline-pass") ; Pass execution: global-dce, eligibility, opt-pass-inline
       (:file "optimizer-dataflow")  ; SCCP (sparse conditional constant propagation)
       (:file "optimizer-copyprop")  ; copy propagation pass + opt-map-tree utility
       (:file "optimizer-memory")   ; Alias analysis helpers
       (:file "optimizer-memory-passes") ; Dead-store-elim + store-to-load-forward passes
       (:file "optimizer-flow")       ; DCE + jump threading + unreachable + type-check elim
       (:file "optimizer-flow-passes") ; nil-check-elim + branch-correlation + block-merge + tail-merge
       (:file "optimizer-strength")     ; Core: strength-reduce + bswap/rotate recognition
       (:file "optimizer-strength-ext") ; Extended: reassociation + batch concatenation
       (:file "optimizer-cse-gvn") ; CSE + GVN + dead-label elimination + leaf-function detection
       (:file "optimizer-licm")     ; LICM + PRE + constant hoist
       (:file "optimizer")
       (:file "optimizer-pipeline"))) ; pass registry + convergence loop + optimize-instructions
     ;; Stage 7: VM IR → native code + binary formats
     (:module "emit"
      :serial t
      :components
      (;; mir + target extracted to cl-cc-mir.asd (Phase 1 migration)
       (:file "calling-convention")
       (:file "regalloc")
       (:file "regalloc-defs-uses") ; instruction-defs/uses protocol implementations
       (:file "regalloc-allocate") ; linear scan allocation + spill code + public API
       (:file "x86-64")
       (:file "x86-64-encoding")        ; register constants + REX/ModR/M/SIB primitives
       (:file "x86-64-encoding-instrs") ; MOV/ADD/SUB/CMP/JMP instruction emitters
       (:file "x86-64-sequences") ; IDIV/shift/CMOVcc/bitwise/SETcc complex sequences
       (:file "x86-64-regs")
       (:file "x86-64-emit-ops")
       (:file "x86-64-emit-ops-bits")    ; bit/shift/select emitters (not/lognot/logcount/bswap/ash/rotate/min/max/select)
       (:file "x86-64-emit-ops-logical") ; type predicates + boolean/bitwise logical emitters
       (:file "x86-64-codegen")
       (:file "x86-64-codegen-dispatch") ; per-instruction emitters + dispatch table
       (:file "aarch64")
       (:file "aarch64-codegen")  ; reg mapping + instruction encoders
       (:file "aarch64-codegen-labels") ; 64-bit immediate + size estimation + label offsets
       (:file "aarch64-emitters") ; define-a64-*-emitter macros + emit-a64-vm-* functions
       (:file "aarch64-program")  ; emitter-table, prologue/epilogue, two-pass program emitter
       (:file "wasm-types")
       (:file "wasm-ir")
       (:file "wasm-extract")
       (:file "wasm-trampoline")
       (:file "wasm-trampoline-emit")  ; data tables + emit-trampoline-instruction (per-inst dispatch)
       (:file "wasm-trampoline-build") ; body builder + register collector + module assembler
       (:file "wasm")
       (:file "wasm-emit")    ; emit-instruction methods + compile-to-wasm-wat entry point
       (:module "binary"
        :serial t
        :components
        ((:file "package")
         (:file "macho")           ; constants + structs + buffer helpers + serialization primitives
         (:file "macho-serialize") ; structure serializers + mach-o-builder class definition
         (:file "macho-build")     ; builder API (make-mach-o-builder, add-*, build-mach-o)
         (:file "elf")          ; constants + byte-buffer + strtab + builder struct + API
         (:file "elf-emit")     ; serialization (symtab/rela/shdr/finalize) + public API
         (:file "wasm")))))
     ;; Bytecode ISA v2: 32-bit instruction encoding + disassembly
     (:module "bytecode"
      :serial t
      :components
      ((:file "package")
       (:file "encode")
       (:file "encode-ops")         ; basic-op encoders (load/arith/cmp/branch/call/return)
       (:file "encode-ops-objects") ; closure/object/hash/type/values/exception + builder API
       (:file "decode")))
     ;; Runtime support: GC + heap
     (:module "runtime"
      :serial t
      :components
      ((:file "package")
       (:file "runtime")
       (:file "runtime-ops")     ; arrays, arithmetic, bitwise, comparisons, math rt-* wrappers
       (:file "runtime-math-io") ; strings, chars, symbols, hash-tables, CLOS, conditions, I/O
       (:file "value")
       (:file "value-codec")  ; encode/decode codecs: fixnum/double/pointer/char/bool + CL interop
       (:file "frame")
       (:file "heap")
       (:file "heap-trace") ; card table helpers + address predicates + rt-object-pointer-slots
       (:file "gc")               ; Sections 1-3: alloc, roots, minor GC (Cheney)
       (:file "gc-write-barrier") ; Section 4: rt-gc-write-barrier (SATB + card table)
       (:file "gc-major"))) ; Sections 5-6: major GC (tri-color mark-sweep) + stats
     ;; Stage 4 (pipeline): compile-expression, run-string, stdlib API
     ;; Loads last because it calls optimize-instructions + emit-assembly
     (:module "compile-pipeline"
      :pathname "compile"
      :serial t
      :components
      ((:file "stdlib-source")     ; *standard-library-source* core: mapcar→numeric constants
       (:file "stdlib-source-ext") ; *standard-library-source* ext: type predicates→control vars
       (:file "pipeline-stdlib")   ; stdlib expanded-form cache: snapshot/restore/build/get/warm
       (:file "pipeline")
       (:file "pipeline-native") ; native code generation + typeclass macros
       (:file "pipeline-repl")   ; REPL persistent state + run-string-repl + our-load
       ))))))

(asdf:defsystem :cl-cc/bin
  :description "CL-CC CLI tool"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :components
  ((:module "src"
    :components
    ((:module "cli"
      :serial t
      :components
      ((:file "package")
       (:file "args")
       (:file "selfhost") ; Self-hosting verification data + phases (before main.lisp)
       (:file "main")       ; Help system (%print-global-help, %print-command-help)
       (:file "main-utils") ; Utilities, flamegraph, SSA block name helpers
       (:file "main-dump")  ; ANSI colors, dump-*-phase functions, compile-opts struct
       (:file "handlers"))))))) ; Subcommand handlers + main dispatcher

(asdf:defsystem :cl-cc/test
  :description "CL-CC tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc/bin)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:module "framework"
      :serial t
      :components
      ((:file "package")
       (:file "framework")
       (:file "framework-advanced")
       (:file "framework-compiler")
       (:file "framework-meta")
       (:file "framework-fuzz")
       (:file "framework-runner")))
     (:module "unit"
      :serial t
      :components
       ((:module "cli"
         :serial t
         :components
         ((:file "args-tests")
          (:file "cli-tests")
          (:file "main-tests")))
         (:module "vm"
         :serial t
         :components
            ((:file "vm-instructions-tests")
             (:file "list-tests")       ; defines make-test-vm / exec1 helpers
            (:file "list-coerce-tests")
            (:file "array-tests")
             (:file "vm-execute-tests")
             (:file "primitives-tests") ; execute-instruction for type predicates + arithmetic
            (:file "vm-transcendental-tests") ; transcendental math ops
            (:file "vm-numeric-tests") ; numeric tower + environment queries
            (:file "vm-extensions-tests") ; symbol plist + progv + generic arithmetic
            (:file "vm-bitwise-tests") ; bitwise integer instructions
            (:file "vm-clos-tests")    ; collect-inherited-slots, compute-CPL, eql-dispatch-index
            (:file "vm-clos-execute-tests") ; execute-instruction for class-def/make-obj/slot-read/write/boundp
             (:file "vm-run-tests") ; vm-error-type-matches-p + vm2 run-vm tests
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
       (:module "ast"
        :serial t
        :components
        ((:file "ast-tests")))
       (:module "parse"
        :serial t
        :components
        ((:file "cl-parser-tests")
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
           (:file "pratt-tests")
          (:file "php-tests")
        (:module "php"
         :serial t
         :components
         ((:file "parser-tests")
          (:file "grammar-tests")))
          (:file "combinator-tests")
         (:file "incremental-tests")
         (:file "cst-to-ast-tests")
         (:file "diagnostics-tests")))
       (:module "prolog"
        :serial t
        :components
        ((:file "prolog-data-tests")
         (:file "dcg-tests")))
        (:module "expand"
           :serial t
           :components
           ((:file "macro-tests")
              (:file "macro-definition-tests")
              (:file "macro-assignment-tests")
              (:file "macro-multiple-value-tests")
               (:file "macros-control-flow-tests")
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
                (:file "macros-sequence-tests")
              ))
       (:module "type"
        :serial t
        :components
        ((:file "type-tests")
         (:file "kind-tests")
         (:file "multiplicity-tests")
         (:file "row-tests")
         (:file "subtyping-tests")
         (:file "effect-tests")
         (:file "constraint-tests")
         (:file "solver-tests")
         (:file "representation-tests")
         (:file "substitution-tests")          ; substitution data structure, zonk, composition
         (:file "unification-tests")           ; product/intersection/constructor/effect-row unification
         (:file "type-children-tests")         ; type-children / type-bound-var data layer
         (:file "types-extended-coverage-tests")
         (:file "typeclass-tests")
         (:file "printer-tests")
         (:file "parser-tests")
         (:file "inference-tests")
         (:file "exhaustiveness-tests")))
       (:module "ir"
        :serial t
        :components
        ((:file "ir-types-tests")
         (:file "ir-block-tests")
         (:file "ir-printer-tests")))
        (:module "compile"
         :serial t
         :components
         ((:file "cps-tests")
            (:file "cps-ast-tests")
            (:file "cps-ast-extended-tests") ; OOP/mutation: setq/defvar/make-instance/defclass/defgeneric/defmethod
            (:file "cps-ast-functional-tests") ; functional: quote/the/values/mvb/apply/call/entry-points
              (:file "builtin-registry-tests")
             (:file "builtin-registry-data-tests")
             (:file "builtin-registry-data-ext-tests")   ; extended calling-convention data tables
             (:file "closure-tests")
           (:file "context-tests")
             (:file "codegen-tests")
             (:file "codegen-fold-tests")       ; %fold-ast-binop, *compile-time-eval-fns*, %evaluate-ast
             (:file "codegen-phase2-helpers")
             (:file "codegen-core-tests")
             (:file "codegen-core-let-tests")     ; %ast-let-binding-ignored-p, %ast-cons-call-p, %ast-lambda-bound-names, sink-if
             (:file "codegen-functions-tests")
               (:file "codegen-clos-tests")
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
       (:module "optimize"
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
         (:file "optimizer-inline-pass-tests") ; memo utils, body-inst-set/labels, reachability, eligible-p, global-dce, pass-inline
         (:file "effects-tests")
         (:file "cfg-tests")
         (:file "ssa-tests")
         (:file "egraph-tests")
         (:file "egraph-rules-tests")))
       (:module "emit"
        :serial t
        :components
        ((:file "mir-tests")
         (:file "regalloc-tests")
         (:file "wasm-tests")
         (:file "wasm-ir-tests")        ; wasm IR instruction structs
         (:file "wasm-types-tests")     ; wasm type encoding helpers
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
         (:file "x86-64-encoding-tests") ; x86-64 rex/modrm/byte/dword/qword encoding
         (:file "x86-64-emit-tests")     ; x86-64 emit target + instruction emit methods
         (:file "x86-64-emit-logical-tests") ; logical/bitwise emitters + byte-count checks
         (:file "x86-64-emit-ops-tests")    ; arithmetic/comparison/unary emitters + byte-count checks
         (:file "elf-tests")             ; ELF binary format helpers
         (:file "macho-tests")           ; Mach-O binary format helpers
         (:file "calling-convention-tests")))
       (:module "runtime"
        :serial t
        :components
        ((:file "gc-tests")
         (:file "heap-tests")
         (:file "heap-trace-tests")       ; Card table + address predicates (heap-trace.lisp)
         (:file "gc-write-barrier-tests") ; SATB + card write barrier (gc-write-barrier.lisp)
         (:file "value-tests")
         (:file "frame-tests")))
       (:module "bytecode"
        :serial t
        :components
        ((:file "encode-tests")
         (:file "encode-ops-objects-tests")
         (:file "decode-tests")))))
     (:module "integration"
      :serial t
      :components
      ((:file "compiler-tests")
       (:file "closure-tests")
       (:file "call-conv-tests")
       (:file "control-flow-tests")
       (:file "clos-tests")
       (:file "stream-tests")
       ;; macros/expand integration tests (run VM; moved from unit/expand during migration)
       (:file "loop-macro-tests")
       (:file "macros-basic-mvb-tests")
       (:file "macros-mutation-tests")
       (:file "macros-sequence-fold-tests")
       (:file "macros-stdlib-list-set-tests")
       (:file "predicate-tests")
       ;; compile/codegen integration test (runs VM; moved from unit/compile during migration)
       (:file "codegen-runtime-tests")
       ;; optimizer integration test (runs VM; moved from unit/optimize during migration)
       (:file "optimizer-tests")
       ;; pipeline integration tests (parse pipeline, run-string, incremental)
       (:file "pipeline-tests")
       ;; prolog integration tests (queries, rules, goal solving)
       (:file "prolog-tests")
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
      :serial t
      :components
      ((:file "selfhost-tests"))))))
  :perform (asdf:test-op (op c)
              (declare (ignore op c))
              (uiop:symbol-call :cl-cc/test 'run-tests)))

(asdf:defsystem :cl-cc/test-clos
  :description "CL-CC isolated CLOS integration tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc/bin)
  :serial t
  :components
  ((:module "tests"
    :serial t
    :components
    ((:module "framework"
      :serial t
      :components
      ((:file "package")
       (:file "framework")
       (:file "framework-advanced")
       (:file "framework-compiler")
       (:file "framework-meta")
       (:file "framework-fuzz")))
     (:module "integration"
       :serial t
       :components
       ((:file "clos-tests")))))))

