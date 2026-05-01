;;;; packages/selfhost/src/package.lisp — feature package for cl-cc/selfhost
;;;;
;;;; Phase 4 strict-packaging: pipeline-selfhost.lisp moved here from
;;;; packages/pipeline/src/.
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
  (:export))
