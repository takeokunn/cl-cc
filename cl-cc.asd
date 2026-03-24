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
     ;; Stage 1: Source → S-expressions + AST
     (:module "parse"
      :serial t
      :components
      ((:file "package")
       (:file "ast")
       (:file "reader")
       (:file "prolog")
       (:file "combinators")
       (:module "cl"
        :serial t
        :components
        ((:file "lexer")
         (:file "parser")))
       (:module "php"
        :serial t
        :components
        ((:file "lexer")
         (:file "parser")))))
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
       (:file "representation")
       (:file "unification")
       (:file "subtyping")
       (:file "parser")
       (:file "inference")))
     ;; Stage 4: AST → VM IR (context/closure/cps/codegen — before optimize/emit)
     (:module "compile"
      :serial t
      :components
      ((:file "context")
       (:file "closure")
       (:file "cps")
       (:file "codegen")))
     ;; Stage 5: VM IR → optimized VM IR
     (:module "optimize"
      :serial t
      :components
      ((:file "optimizer")))
     ;; Stage 7: VM IR → native code + binary formats
     (:module "emit"
      :serial t
      :components
      ((:file "calling-convention")
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
     ;; Runtime support: GC + heap
     (:module "runtime"
      :serial t
      :components
      ((:file "package")
       (:file "runtime")
       (:file "heap")
       (:file "gc")))
     ;; Stage 4 (pipeline): compile-expression, run-string, stdlib API
     ;; Loads last because it calls optimize-instructions + emit-assembly
     (:module "compile-pipeline"
      :pathname "compile"
      :serial t
      :components
      ((:file "pipeline")))))))

(asdf:defsystem :cl-cc/test
  :description "CL-CC tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
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
      ((:module "parse"
        :serial t
        :components
        ((:file "ast-tests")
         (:file "prolog-tests")
         (:file "parser-combinator-tests")
         (:file "php-tests")))
       (:module "expand"
        :serial t
        :components
        ((:file "macro-tests")))
       (:module "type"
        :serial t
        :components
        ((:file "type-tests")))
       (:module "compile"
        :serial t
        :components
        ((:file "cps-tests")))
       (:module "optimize"
        :serial t
        :components
        ((:file "optimizer-tests")))
       (:module "emit"
        :serial t
        :components
        ((:file "regalloc-tests")
         (:file "wasm-tests")))
       (:module "runtime"
        :serial t
        :components
        ((:file "gc-tests")
         (:file "heap-tests")))))
     (:module "integration"
      :serial t
      :components
      ((:file "compiler-tests")
       (:file "closure-tests")
       (:file "call-conv-tests")
       (:file "control-flow-tests")
       (:file "clos-tests")
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
