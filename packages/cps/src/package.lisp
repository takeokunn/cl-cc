;;;; packages/cps/src/package.lisp — feature package for cl-cc/cps
;;;;
;;;; CPS transformation: continuation-passing style conversion for the
;;;; compilation pipeline. Uses cl-cc/bootstrap and cl-cc/ast so cps
;;;; source files can reference AST node accessors unqualified.

(defpackage :cl-cc/cps
  (:use :cl :cl-cc/bootstrap :cl-cc/ast)
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
