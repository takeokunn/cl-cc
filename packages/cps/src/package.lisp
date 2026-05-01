;;;; packages/cps/src/package.lisp — feature package for cl-cc/cps
;;;;
;;;; Phase 6 (post 2026-05-01): CPS transformation extracted into its own
;;;; feature package. Files moved here from :cl-cc/compile.
;;;; Uses cl-cc/bootstrap and cl-cc/ast so cps source files can reference
;;;; AST node accessors (ast-int, ast-binop, etc.) unqualified.

(defpackage :cl-cc/cps
  (:use :cl :cl-cc/bootstrap :cl-cc/ast :cl-cc/parse)
  (:export
   ;; ─── cps.lisp / cps-ast.lisp — CPS transformation entry points ───
   #:cps-transform
   #:cps-transform*
   #:cps-transform-ast
   #:cps-transform-ast*
   #:cps-transform-sequence
   #:cps-transform-eval
   #:cps-simplify-form
   #:maybe-cps-transform))
