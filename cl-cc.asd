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
       (:file "prolog")
       (:file "dcg")
       (:file "lexer")
       (:file "incremental")
       (:file "pratt")
       (:file "combinators")
       (:module "cl"
        :serial t
        :components
        ((:file "parser")
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
      ((:file "macro")
       (:file "expander")))
     ;; Stage 6: VM execution (loaded before compile/ so VM types are available)
     (:module "vm"
      :serial t
      :components
      ((:file "package")
       (:file "vm")
       (:file "primitives")
       (:file "io")
       (:file "conditions")
       (:file "list")
       (:file "strings")
       (:file "hash")))
     ;; Stage 3: Type checking and inference
     (:module "type"
      :serial t
      :components
      ((:file "package")
       (:file "kind")
       (:file "multiplicity")
       (:file "representation")
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
       (:file "builtin-registry")
       (:file "codegen")))
     ;; Stage 5: VM IR → optimized VM IR
     (:module "optimize"
      :serial t
      :components
      ((:file "effects")        ; Phase 0: effect-kind bridge (type-effect-row → optimizer)
       (:file "cfg")            ; Phase 1: CFG construction + dominator tree + DF
       (:file "ssa")            ; Phase 1: SSA construction + destruction
       (:file "egraph")         ; Phase 2: E-graph engine (union-find, saturation, extraction)
       (:file "egraph-rules")   ; Phase 2: defrule macro + built-in rewrite rules
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
      ((:file "pipeline")))))))

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
         (:file "cli-tests")))
       (:module "vm"
        :serial t
        :components
        ((:file "vm2-tests")
         (:file "conditions-tests")
         (:file "hash-tests")
         (:file "strings-tests")
         (:file "io-tests")))
       (:module "parse"
        :serial t
        :components
        ((:file "ast-tests")
         (:file "cl-parser-tests")
         (:file "cst-tests")
         (:file "lexer-tests")
         (:file "grammar-tests")
         (:file "prolog-tests")
         (:file "pratt-tests")
         (:file "php-tests")
         (:file "combinator-tests")
         (:file "dcg-tests")
         (:file "incremental-tests")
         (:file "cst-to-ast-tests")
         (:file "diagnostics-tests")))
       (:module "expand"
        :serial t
        :components
        ((:file "macro-tests")
         (:file "lambda-list-tests")
         (:file "defstruct-tests")
         (:file "expander-tests")
         (:file "loop-macro-tests")
         (:file "macro-advanced-tests")))
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
         (:file "builtin-registry-tests")
         (:file "closure-tests")
         (:file "context-tests")
         (:file "codegen-tests")))
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
