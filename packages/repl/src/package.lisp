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
