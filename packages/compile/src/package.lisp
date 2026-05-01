;;;; packages/compile/src/package.lisp --- cl-cc/compile package definition
;;;;
;;;; Phase 6 (post 2026-05-01): :cl-cc/compile is now a facade that
;;;; bridges to :cl-cc/codegen. The actual codegen source files
;;;; declare (in-package :cl-cc/codegen). Tests and downstream callers
;;;; that use `cl-cc/compile::%foo` continue to work because the
;;;; eval-when block below SHADOWING-IMPORTs all of cl-cc/codegen's
;;;; internal symbols into cl-cc/compile, making them accessible via
;;;; the reader's `pkg::name` form (which calls `intern`, which returns
;;;; already-accessible symbols rather than creating new ones).
;;;;
;;;; Pipeline-level compilation entry points (compile-expression, run-string,
;;;; ...) live in :cl-cc/pipeline; their symbols are pre-interned in
;;;; :cl-cc/codegen so `cl-cc/compile:run-string` works through the bridge.

(defpackage :cl-cc/compile
  (:use :cl :cl-cc/bootstrap :cl-cc/ast :cl-cc/prolog :cl-cc/parse :cl-cc/vm
        :cl-cc/optimize :cl-cc/codegen)
  (:import-from :cl-cc/cps
                #:cps-transform
                #:cps-transform*
                #:cps-transform-ast
                #:cps-transform-ast*
                #:cps-transform-sequence
                #:cps-transform-eval)
  (:export
   ;; Re-export of codegen symbols ---
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
   #:*cps-compile-unsupported-ast-types*
   #:*cps-native-compile-unsupported-ast-types*
   #:*enable-cps-vm-primary-path*
   #:*compile-expression-cps-recursion-guard*
   #:%cps-compile-safe-ast-p
   #:%cps-vm-compile-safe-ast-p
   #:%cps-native-compile-safe-ast-p
   #:%cps-identity-entry-form

   #:optimize-ast
   #:target-instance #:type-check-ast
   #:compile-ast

   ;; CPS transformation re-exports ---
   #:cps-transform
   #:cps-transform*
   #:cps-transform-ast
   #:cps-transform-ast*
   #:cps-transform-sequence
   #:cps-transform-eval

   ;; Pipeline entry points ---
   #:compile-expression
   #:compile-string
   #:run-string
   #:run-string-typed
   #:run-string-repl
   #:reset-repl-state
   #:*repl-vm-state*
   #:our-load
   #:emit-assembly
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

;;; Bridge: import all internal symbols of :cl-cc/codegen so legacy
;;; `cl-cc/compile::%foo` references resolve to the codegen home symbol.
(eval-when (:load-toplevel :execute)
  (let ((codegen-pkg (find-package :cl-cc/codegen))
        (compile-pkg (find-package :cl-cc/compile)))
    (when codegen-pkg
      (do-symbols (sym codegen-pkg)
        (when (eq (symbol-package sym) codegen-pkg)
          (handler-case
              (unless (find-symbol (symbol-name sym) compile-pkg)
                (import (list sym) compile-pkg))
            (package-error () nil)))))))
