;;;; src/compile/package.lisp --- cl-cc/compile package definition
;;;;
;;;; Compilation pipeline: CPS transformation, codegen (AST -> VM IR),
;;;; builtin registry, pipeline entry points (compile-expression,
;;;; compile-string, run-string), REPL state, and our-eval/our-load.
;;;;
;;;; Extracted as a Phase 2 sibling system (:cl-cc-compile). The package
;;;; facade is loaded first (no dependencies); the umbrella :cl-cc system
;;;; then sets up (use-package :cl-cc :cl-cc/compile) so the source files
;;;; can access VM instruction types and accessors unqualified.

(defpackage :cl-cc/compile
  (:use :cl)
  (:export
   ;; --- cps.lisp / cps-ast.lisp --- CPS transformation ----------------
   #:cps-transform
   #:cps-transform*
   #:cps-transform-ast
   #:cps-transform-ast*
   #:cps-transform-sequence
   #:cps-transform-eval

   ;; --- pipeline.lisp --- compilation entry points ---------------------
   #:compile-expression
   #:compile-string
   #:run-string
   #:run-string-typed
   ;; NOTE: our-eval is NOT exported here. It is pre-interned by
   ;; :cl-cc/prolog (prolog-query.lisp uses it at compile time) and
   ;; exported from :cl-cc via package-exports-1.lisp.

   ;; --- pipeline-repl.lisp --- REPL state + our-load ------------------
   #:run-string-repl
   #:reset-repl-state
   #:*repl-vm-state*
   #:our-load

   ;; --- codegen-locals.lisp --- assembly emission ----------------------
   #:emit-assembly

   ;; --- codegen.lisp --- compilation-result + compile-toplevel-forms ---
   #:compile-toplevel-forms
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-program
   #:compilation-result-assembly
   #:compilation-result-globals
   #:compilation-result-type
   #:compilation-result-type-env
   #:compilation-result-cps
   #:compilation-result-ast
   #:compilation-result-vm-instructions
   #:compilation-result-optimized-instructions))
