;;;; cl-cc-codegen.asd — skeleton for the codegen feature package
;;;;
;;;; Phase 1 of the ideal package-by-feature redesign (post 2026-05-01 plan).
;;;; This system is intentionally empty — files will migrate here in
;;;; subsequent phases. Currently NOT wired into root cl-cc.asd, so loading
;;;; this system as part of `:cl-cc` is a no-op.

(asdf:defsystem :cl-cc-codegen
  :description "Per-target code generation (x86-64, AArch64, WASM, VM)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse
               :cl-cc-optimize :cl-cc-vm :cl-cc-mir :cl-cc-target
               :cl-cc-regalloc :cl-cc-expand :cl-cc-cps)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "context")  ; absorbed from cl-cc-compile (compiler-context, *repl-* globals)
   ;; Group J: emit per-target files (use :cl-cc/emit). Loaded first because
   ;; codegen entry points (emit-assembly in codegen-locals) call into them.
   (:file "x86-64")
   (:file "x86-64-encoding")
   (:file "x86-64-encoding-instrs")
   (:file "x86-64-sequences")
   (:file "x86-64-regs")
   (:file "x86-64-emit-ops")
   (:file "x86-64-emit-ops-bits")
   (:file "x86-64-emit-ops-logical")
   (:file "x86-64-codegen")
   (:file "x86-64-codegen-dispatch")
   (:file "aarch64")
   (:file "aarch64-codegen")
   (:file "aarch64-codegen-labels")
   (:file "aarch64-emitters")
   (:file "aarch64-program")
   (:file "wasm-types")
   (:file "wasm-ir")
   (:file "wasm-extract")
   (:file "wasm-trampoline")
   (:file "wasm-trampoline-tables")
   (:file "wasm-trampoline-emit")
   (:file "wasm-trampoline-build")
   (:file "wasm")
   (:file "wasm-emit")
   ;; Group I: codegen + builtin registry (use :cl-cc/compile).
   (:file "builtin-registry-data")
   (:file "builtin-registry-data-ext")
   (:file "builtin-registry")
   (:file "builtin-registry-emitters")
   (:file "builtin-registry-dispatch")
   (:file "codegen-core")
   (:file "codegen-core-control")
   (:file "codegen-core-let")
   (:file "codegen-core-let-walkers")
   (:file "codegen-core-let-emit")
   (:file "codegen-clos")
   (:file "codegen-gf")
   (:file "codegen-functions")
   (:file "codegen-functions-params")
   (:file "codegen-functions-emit")
   (:file "codegen-phase2")
   (:file "codegen-control")
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
   (:file "codegen-locals")))
