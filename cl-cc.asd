;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection - ASDF System Definition
;;;;
;;;; Directory structure mirrors the compilation pipeline:
;;;;   parse → expand → vm → type → compile → optimize → emit → runtime → compile-pipeline

(asdf:defsystem :cl-cc
  :description "CL-CC: Common Lisp Compiler Collection"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     ;; Stage 1: Source → CST → S-expressions (2026 modern parser)
      (:module "parse"
       :serial t
       :components
       ((:file "cst")
        (:file "diagnostics")
        (:file "ast")
        (:file "prolog-data")
        (:file "prolog")
        (:file "dcg")
        (:file "lexer")
       (:file "lexer-dispatch")
       (:file "incremental")
       (:file "pratt")
       (:file "combinators")
        (:module "cl"
         :serial t
         :components
         ((:file "parser")
          (:file "lower")
          (:file "parser-roundtrip")
          (:file "grammar")))
       (:module "php"
        :serial t
        :components
        ((:file "lexer")
         (:file "parser")
         (:file "grammar")))
       (:file "cst-to-ast")))
      ;; Stage 2: S-expressions → macro-expanded S-expressions
      (:module "expand"
       :serial t
       :components
          ((:file "macro-lambda-list") ; shared lambda-list parsing + destructuring helpers
           (:file "macro")           ; core: macro-env, defmacro machinery, macroexpansion
           (:file "macros-basic")    ; bootstrap: check-type/setf/list + value helpers
           (:file "macros-control-flow") ; bootstrap control-flow macros
           (:file "macros-mutation") ; push/pop/incf/decf split from stdlib
           (:file "loop-data")       ; LOOP: grammar tables — the "Prolog database"
           (:file "loop-parser")     ; LOOP: CPS token parser → IR plist
           (:file "loop-emitters")   ; LOOP: IR → code-fragment tables
           (:file "loop")            ; LOOP: generator — assembles tagbody from IR
           (:file "macros-setops")   ; list/set operations split from stdlib
           (:file "macros-list-utils") ; ordering and list utility helpers
           (:file "macros-restarts") ; restart/condition protocol split from stdlib
           (:file "macros-introspection") ; equalp + implementation stubs
           (:file "macros-stdlib")   ; stdlib: numeric shorthand + ANSI CL + core stubs
           (:file "macros-cxr")      ; algorithmic CXR accessor registration
           (:file "macros-hof")      ; higher-order list/search helpers
           (:file "macros-filesystem") ; file/IO/runtime stubs split from stdlib
           (:file "macros-sequence") ; sequences: sort/reduce/substitute
           (:file "macros-list-compat") ; list/sequence compatibility helpers split from stdlib
           (:file "macros-plist")      ; property list helpers
           (:file "macros-compat")  ; ANSI CL compat: package no-ops, progv, coerce, CLOS, plist
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
     ;; Stage 6: VM execution (loaded before compile/ so VM types are available)
      (:module "vm"
        :serial t
        :components
        ((:file "package")
         (:file "vm")
         (:file "vm-instructions") ; instruction set definitions
         (:file "vm-dispatch") ; dispatch protocol + call-frame helpers
         (:file "vm-execute") ; Core execute-instruction methods + call-frame helpers
         (:file "vm-clos")    ; CLOS instruction defstructs + execute-instruction methods
        (:file "vm-run")     ; Handler-case, label table, run-vm, vm2-state
       (:file "primitives")
       (:file "vm-bitwise")        ; FR-303: ash, logand, logior, logxor, logeqv, lognot, logtest, logbitp, logcount, integer-length
       (:file "vm-transcendental") ; FR-304: expt, sqrt, exp, log, trig, hyperbolic
       (:file "vm-numeric")
       (:file "vm-extensions")
        (:file "io")
        (:file "format")
        (:file "conditions")
        (:file "list-coerce")
        (:file "list")
        (:file "array")
        (:file "strings")
        (:file "symbols")
       (:file "hash")))
     ;; Stage 3: Type checking and inference
     (:module "type"
      :serial t
      :components
      ((:file "package")
       (:file "kind")
       (:file "multiplicity")
       (:file "types-core")
       (:file "types-extended")
       (:file "types-env")
       (:file "substitution")
       (:file "unification")
       (:file "subtyping")
       (:file "effect")
       (:file "row")
       (:file "constraint")
       (:file "parser")
       (:file "typeclass")
       (:file "solver")
       (:file "inference")
       (:file "inference-effects")
       (:file "bidirectional")
       (:file "checker")
       (:file "printer")))
     ;; Stage 4: AST → VM IR (context/closure/cps/codegen — before optimize/emit)
     (:module "compile"
      :serial t
      :components
      ((:module "ir"                   ; Phase 1: compile-level SSA IR foundation
        :serial t
        :components
        ((:file "types")               ; ir-value, ir-inst, ir-block, ir-function, ir-module
         (:file "block")               ; CFG edges, RPO traversal, dominator tree
         (:file "ssa")                 ; Braun et al. 2013 SSA variable tracking
         (:file "printer")))           ; human-readable IR dump
       (:file "context")
       (:file "closure")
        (:file "cps")
        (:file "cps-ast")
         (:file "builtin-registry-data") ; Entry alists — pure data, no logic
         (:file "builtin-registry")
         (:file "codegen-core")
         (:file "codegen-clos")
         (:file "codegen-functions")
         (:file "codegen-phase2")  ; Phase 2 AST-introspecting builtin handlers
         (:file "codegen-control") ; control-flow + multiple-values compiler methods
         (:file "codegen-io")      ; Phase 2 stream/reader/printer handlers split from phase 2
         (:file "codegen-hash-table") ; hash-table handler cluster split from phase 2
         (:file "codegen-slot-predicates") ; CLOS slot predicate handlers split from phase 2
        (:file "codegen-string-kwargs") ; string comparison/case handlers split from phase 2
        (:file "codegen")))
     ;; Stage 5: VM IR → optimized VM IR
     (:module "optimize"
      :serial t
      :components
      ((:file "effects")          ; Phase 0: effect-kind bridge (type-effect-row → optimizer)
       (:file "cfg")              ; Phase 1: CFG construction + dominator tree + DF
       (:file "ssa")              ; Phase 1: SSA construction + destruction
       (:file "egraph")           ; Phase 2: E-graph engine (union-find, saturation, extraction)
       (:file "egraph-rules")     ; Phase 2: defrule macro + built-in rewrite rules
       (:file "optimizer-tables") ; Data tables + predicates (loaded before passes)
       (:file "optimizer-inline") ; Function inlining pass (opt-pass-inline)
       (:file "optimizer")))
     ;; Stage 7: VM IR → native code + binary formats
     (:module "emit"
      :serial t
      :components
      ((:file "mir")                 ; Phase 1: MIR IR — SSA CFG intermediate
       (:file "target")              ; Phase 1: target-desc — unified target descriptors
       (:file "calling-convention")
       (:file "regalloc")
       (:file "x86-64")
       (:file "x86-64-encoding")
       (:file "x86-64-codegen")
       (:file "aarch64")
       (:file "aarch64-codegen")
       (:file "wasm-types")
       (:file "wasm-ir")
       (:file "wasm-extract")
       (:file "wasm-trampoline")
       (:file "wasm")
       (:module "binary"
        :serial t
        :components
        ((:file "package")
         (:file "macho")
         (:file "elf")
         (:file "wasm")))))
     ;; Bytecode ISA v2: 32-bit instruction encoding + disassembly
     (:module "bytecode"
      :serial t
      :components
      ((:file "package")
       (:file "encode")
       (:file "decode")))
     ;; Runtime support: GC + heap
     (:module "runtime"
      :serial t
      :components
      ((:file "package")
       (:file "runtime")
       (:file "value")
       (:file "frame")
       (:file "heap")
       (:file "gc")))
     ;; Stage 4 (pipeline): compile-expression, run-string, stdlib API
     ;; Loads last because it calls optimize-instructions + emit-assembly
     (:module "compile-pipeline"
      :pathname "compile"
      :serial t
      :components
      ((:file "stdlib-source") ; *standard-library-source* string (separated for readability)
       (:file "pipeline")))))))

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
       (:file "main")))))))

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
       (:file "framework-fuzz")))
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
            (:file "vm-clos-tests")    ; execute-instruction for CLOS instructions
             (:file "vm-run-tests") ; vm-error-type-matches-p + vm2 run-vm tests
             (:file "vm-dispatch-tests") ; vm-classify-arg + vm-generic-function-p
              (:file "vm-tests") ; vm.lisp core state + helper coverage
              (:file "package-tests") ; cl-cc package export smoke tests
              (:file "conditions-tests")
             (:file "hash-tests")
            (:file "symbols-tests")
            (:file "strings-tests")
            (:file "format-tests")
            (:file "io-tests")))
       (:module "parse"
        :serial t
        :components
        ((:file "ast-tests")
          (:file "cl-parser-tests")
          (:module "cl"
           :serial t
           :components
           ((:file "lower-tests")))
           (:file "cst-tests")
          (:file "lexer-tests")
          (:file "grammar-tests")
           (:file "prolog-data-tests")
           (:file "prolog-tests")
           (:file "pratt-tests")
          (:file "php-tests")
        (:module "php"
         :serial t
         :components
         ((:file "parser-tests")
          (:file "grammar-tests")))
          (:file "combinator-tests")
         (:file "dcg-tests")
         (:file "incremental-tests")
         (:file "cst-to-ast-tests")
         (:file "diagnostics-tests")))
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
                (:file "loop-macro-tests")
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
             (:file "macros-stdlib-list-set-tests")
             (:file "macros-stdlib-io-tests")
             (:file "macros-filesystem-tests")
              (:file "macros-compat-array-tests")   ; array compat wrappers
              (:file "macros-compat-tests")         ; remaining ANSI CL compat macros
               (:file "macros-plist-tests")    ; getf/remf/%plist-put
              (:file "macros-list-compat-tests") ; subst/vector/member-if/maphash
               (:file "macros-hof-tests")      ; HOF macro expansions split from stdlib
                (:file "predicate-tests")
                (:file "macros-mutation-tests")
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
         (:file "typeclass-tests")
         (:file "printer-tests")
         (:file "parser-tests")))
        (:module "compile"
         :serial t
         :components
         ((:module "ir"
           :serial t
           :components
           ((:file "ir-types-tests")
            (:file "ir-printer-tests")))
            (:file "cps-tests")
            (:file "cps-ast-tests")
              (:file "builtin-registry-tests")
             (:file "builtin-registry-data-tests")
             (:file "closure-tests")
           (:file "context-tests")
             (:file "codegen-tests")
             (:file "codegen-phase2-helpers")
             (:file "codegen-core-tests")
             (:file "codegen-functions-tests")
              (:file "codegen-runtime-tests")
               (:file "codegen-clos-tests")
              (:file "codegen-control-tests")
              (:file "codegen-io-tests")
              (:file "codegen-hash-table-tests")
              (:file "codegen-slot-predicates-tests")
              (:file "codegen-string-kwargs-tests")
             (:file "phase2-handler-tests")
             (:file "codegen-phase2-tests")
             (:file "stdlib-source-tests")))
       (:module "optimize"
        :serial t
        :components
        ((:file "optimizer-tests")
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
         (:file "aarch64-codegen-tests")
         (:file "target-tests")
         (:file "wasm-extract-tests")
         (:file "wasm-trampoline-tests")
         (:file "x86-64-codegen-tests")
         (:file "calling-convention-tests")))
       (:module "runtime"
        :serial t
        :components
        ((:file "gc-tests")
         (:file "heap-tests")
         (:file "value-tests")
         (:file "frame-tests")))
       (:module "bytecode"
        :serial t
        :components
        ((:file "encode-tests")
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
       (:file "selfhost-tests")
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
         (:file "vm-heap-pbt-tests"))))))))
  :perform (asdf:test-op (op c)
              (declare (ignore op c))
              (uiop:symbol-call :cl-cc/test 'run-tests)))
