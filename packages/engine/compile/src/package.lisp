;;;; packages/engine/compile/src/package.lisp --- cl-cc/compile package definition
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
   ;; --- context.lisp --- compiler context class + helpers ---------------
   #:compiler-context
   #:ctx-instructions #:ctx-next-register #:ctx-next-label
   #:ctx-env #:ctx-type-env #:ctx-safety
   #:ctx-block-env #:ctx-tagbody-env
   #:ctx-global-functions #:ctx-global-variables
   #:ctx-global-classes #:ctx-global-generics #:ctx-global-generic-params
   #:ctx-current-function-name #:ctx-current-function-label
   #:ctx-current-function-params #:ctx-current-function-simple-p
   #:ctx-top-level-p #:ctx-boxed-vars
   #:ctx-noescape-cons-bindings #:ctx-noescape-array-bindings
   #:ctx-noescape-instance-bindings #:ctx-noescape-closure-bindings
   #:ctx-tail-position
   #:*builtin-special-variables* #:*repl-global-variables*
   #:*repl-label-counter* #:*repl-capture-label-counter*
   #:*labels-boxed-fns* #:*compiling-typed-fn*
   #:make-register #:make-label #:emit #:lookup-var

   ;; --- codegen-functions.lisp --- lambda-list helpers for expand -------
   #:lambda-list-has-typed-p #:strip-typed-params
   #:register-function-type

   ;; --- codegen-fold-optimize.lisp --- AST optimizer ---------------------
   #:optimize-ast

   ;; --- codegen-locals.lisp --- target + type-check helpers -------------
   #:target-instance #:type-check-ast

   ;; --- codegen-core.lisp --- AST compilation generic function ---------
   #:compile-ast

   ;; --- cps-ast-functional.lisp --- CPS transform entry point ----------
   #:maybe-cps-transform

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
