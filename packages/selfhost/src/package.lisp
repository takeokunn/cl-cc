;;;; packages/selfhost/src/package.lisp — feature package for cl-cc/selfhost
;;;;
;;;; Self-hosting subsystem: our-eval meta-circular evaluator, stdlib cache warm-up.
;;;;
;;;; our-eval is pre-interned in :cl-cc/bootstrap so downstream packages
;;;; (:cl-cc/expand, :cl-cc/compile, etc.) can reference it without a
;;;; circular dependency. The defun in pipeline-selfhost.lisp updates
;;;; that same symbol.

(defpackage :cl-cc/selfhost
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/ast
        :cl-cc/prolog
        :cl-cc/parse
        :cl-cc/optimize
        :cl-cc/emit
        :cl-cc/expand
        :cl-cc/compile
        :cl-cc/vm
        :cl-cc/stdlib
        :cl-cc/pipeline)
  (:export
   ;; our-eval is pre-interned in :cl-cc/bootstrap; the defun here sets the function binding.
   #:our-eval
   ;; warm-stdlib-cache initializes the standard library VM snapshot.
   #:warm-stdlib-cache))
