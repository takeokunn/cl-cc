;;;; packages/repl/src/package.lisp — feature package for cl-cc/repl
;;;;
;;;; REPL subsystem: interactive evaluation loop, REPL state management,
;;;; and host-definition bridging.

(defpackage :cl-cc/repl
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
        :cl-cc/pipeline
        :cl-cc/selfhost)
  (:export
   ;; REPL state — symbols genuinely owned by repl
    #:*repl-accessor-map*
    #:*repl-defstruct-read-only-accessor-map*
    #:*repl-defstruct-type-registry*
    #:*repl-setf-compound-place-handlers*
    #:*repl-pool-instructions*
    #:*repl-pool-labels*
    #:*repl-global-vars-persistent*
    #:*repl-history*
    #:*repl-history-limit*
    #:*repl-defstruct-registry*
    #:*our-load-host-definition-mode*
    #:repl-history
    #:repl-history-entry
    #:repl-history-previous
    #:repl-history-next
    #:repl-completion-candidates
    #:repl-edit-input-line
    #:with-fresh-repl-state
    #:run-form-repl
    ;; Internal helpers exposed to tests via cl-cc:: inheritance.
    #:%ensure-repl-state
    #:%repl-record-history
    #:%whitespace-symbol-p
   #:%prescan-in-package
   #:%handle-host-only-top-level-form))
