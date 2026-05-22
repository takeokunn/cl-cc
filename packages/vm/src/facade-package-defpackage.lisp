;;;; packages/vm/src/facade-package.lisp -- cl-cc/vm package definition
;;;;
;;;; VM instruction set subsystem: instruction defstructs, VM state, heap
;;;; operations, I/O, conditions, CLOS dispatch, hash tables, strings, etc.
;;;;
;;;; Extracted as a Phase 2 sibling system (:cl-cc-vm). The package
;;;; facade is loaded first (no dependencies); the umbrella :cl-cc system
;;;; then sets up (use-package :cl-cc :cl-cc/vm) so the source files
;;;; can access all symbols unqualified.
;;;;
;;;; Export lists are organized by domain in separate files:
;;;;   exports-instructions.lisp  — instruction types, constructors, predicates
;;;;   exports-runtime.lisp       — VM state, I/O, strings, hash, CLOS, macros
;;;;   exports-opcodes.lisp       — opcode constants, VM2 state, dispatch tables
;;;;   exports-conditions.lisp    — condition types, handlers, misc symbols

(defpackage :cl-cc/vm
  (:use :cl :cl-cc/bootstrap)
  (:shadow #:get-universal-time
           #:get-internal-real-time
           #:get-internal-run-time
           #:internal-time-units-per-second
           #:sleep
           #:time
           #:encode-universal-time
           #:decode-universal-time
           #:random-state
           #:random-state-p
           #:make-random-state
           #:*random-state*
           #:random
           #:*print-base*
           #:*print-radix*
           #:*print-circle*
           #:*print-pretty*
           #:*print-level*
           #:*print-length*
           #:*print-readably*
           #:*print-pprint-dispatch*
           #:with-standard-io-syntax
           #:pprint-logical-block
           #:pprint-indent
           #:pprint-newline
           #:pprint-tab
           #:copy-pprint-dispatch
           #:set-pprint-dispatch
           #:get-pprint-dispatch
           #:*readtable*
           #:copy-readtable
           #:set-macro-character
           #:get-macro-character
            #:set-dispatch-macro-character
            #:get-dispatch-macro-character
            #:readtable-case
            #:sequence
            #:elt
            #:length
            #:subseq
            #:copy-structure
            #:method-qualifiers
            #:compute-applicable-methods
            #:find-method
            #:add-method
            #:remove-method
            #:ensure-generic-function))

(in-package :cl-cc/vm)

;;; Hook variables for circular dependency resolution.
;;; These are set at load time by the respective upstream packages.
(defvar *vm-eval-hook* nil
  "Set by cl-cc/compile (pipeline.lisp). Called by the VM's vm-eval instruction.")
(defvar *vm-macroexpand-1-hook* nil
  "Set by cl-cc/expand (macro.lisp). Called by the VM's vm-macroexpand-1-inst instruction.")
(defvar *vm-macroexpand-hook* nil
  "Set by cl-cc/expand (macro.lisp). Called by the VM's vm-macroexpand-inst instruction.")
(defvar *vm-compile-string-hook* nil
  "Set by cl-cc/compile (pipeline.lisp). Called by run-string-with-io in io-runners.lisp.")
(defvar *vm-parse-forms-hook* nil
  "Set by cl-cc/parse (parser.lisp). Called by the VM's vm-read-sexp-inst instruction.")
