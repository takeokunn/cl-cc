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
  (:shadowing-import-from :cl-cc/expand
    #:define-syntax)
  (:shadowing-import-from :cl-cc/vm
    #:get-universal-time #:get-internal-real-time #:get-internal-run-time
    #:internal-time-units-per-second #:sleep #:time
    #:encode-universal-time #:decode-universal-time
    #:random-state #:random-state-p #:make-random-state #:*random-state* #:random
    #:*print-base* #:*print-radix* #:*print-circle*
    #:*print-pretty* #:*print-level* #:*print-length*
    #:*print-readably* #:*print-pprint-dispatch*
    #:with-standard-io-syntax
    #:pprint-logical-block #:pprint-indent #:pprint-newline #:pprint-tab
    #:copy-pprint-dispatch #:set-pprint-dispatch #:get-pprint-dispatch
    #:*readtable* #:copy-readtable
    #:set-macro-character #:get-macro-character
    #:set-dispatch-macro-character #:get-dispatch-macro-character
    #:readtable-case
    #:lisp-implementation-type #:lisp-implementation-version
    #:machine-type #:machine-version #:machine-instance
    #:software-type #:software-version
    #:room #:apropos #:apropos-list)
  (:export
   ;; our-eval is pre-interned in :cl-cc/bootstrap; the defun here sets the function binding.
   #:our-eval
   ;; warm-stdlib-cache initializes the standard library VM snapshot.
   #:warm-stdlib-cache))
