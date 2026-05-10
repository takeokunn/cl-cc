;;;; cl-cc-compile.asd --- ASDF system for the compilation engine
;;;;
;;;; Compilation engine: AST→VM instruction transformation.
;;;; Includes context, builtin registry, codegen-* files.
;;;; Depends on :cl-cc-codegen for machine code emission primitives
;;;; (x86-64-target, emit-instruction, etc.).
;;;;
;;;; Dependency order: bootstrap → ast → ... → vm → expand → codegen → compile

(asdf:defsystem :cl-cc-compile
  :description "Compilation engine: AST→VM instruction transformation"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-type
               :cl-cc-optimize :cl-cc-vm :cl-cc-expand :cl-cc-cps :cl-cc-codegen
               :cl-cc-target :cl-cc-regalloc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ;; Compiler context and primitive helpers
   (:file "context")
   ;; Builtin dispatch registry
   (:file "builtin-registry-data")
   (:file "builtin-registry-data-ext")
   (:file "builtin-registry")
   (:file "builtin-registry-emitters")
   (:file "builtin-registry-dispatch")
   ;; Core AST→VM compilation
   (:file "codegen-core")
   (:file "codegen-core-control")
   (:file "codegen-core-let")
   (:file "codegen-core-let-walkers")
   (:file "codegen-core-let-emit")
   (:file "codegen-core-let-emit-pass")
   (:file "codegen-clos")
   (:file "codegen-gf")
   (:file "codegen-functions")
   (:file "codegen-functions-params")
   (:file "codegen-functions-emit")
   (:file "codegen-phase2")
   (:file "codegen-control")
   (:file "codegen-values-helpers")
   (:file "codegen-values")
   (:file "codegen-io")
   (:file "codegen-io-ext")
   (:file "codegen-hash-table")
   (:file "codegen-slot-predicates")
   (:file "codegen-string-kwargs")
   (:file "codegen-fold")
   (:file "codegen-fold-eval")
   (:file "codegen-fold-optimize")
   (:file "codegen")
   (:file "codegen-calls")
   ;; Local function bindings and assembly dispatch (uses machine code layer)
   (:file "codegen-locals")))
