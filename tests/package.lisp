(defpackage :cl-cc/test
  (:use :cl :fiveam)
  (:import-from :cl-cc
                 ;; Compiler
                 :compile-string
                 :run-string
                 :run-string-typed
                 :compile-expression
                 :compile-ast
                 ;; CPS
                 :cps-transform
                 :cps-transform-ast
                 :cps-transform-ast*
                 :cps-transform-eval
                 ;; VM Program
                 :vm-program
                 :vm-program-instructions
                 :vm-program-result-register
                 ;; Macro System
                 :our-macroexpand-1
                 :our-macroexpand
                 ;; AST
                 :ast-to-sexp
                 :lower-sexp-to-ast
                 :ast-node
                 :ast-int
                 :ast-var
                 :ast-binop
                 :ast-if
                 :ast-progn
                 :ast-print
                 :ast-let
                 :ast-lambda
                 :ast-function
                 :ast-flet
                 :ast-labels
                 :ast-block
                 :ast-return-from
                 :ast-tagbody
                 :ast-go
                 :ast-setq
                 :ast-multiple-value-call
                 :ast-multiple-value-prog1
                 :ast-catch
                 :ast-throw
                 :ast-unwind-protect
                 :ast-handler-case
                 :ast-handler-case-form
                 :ast-handler-case-clauses
                 :ast-call
                 :ast-quote
                 :ast-the
                 ;; AST accessors
                 :ast-lambda-params
                 :ast-lambda-body
                 :ast-flet-bindings
                 :ast-labels-bindings
                 ;; VM State and heap operations
                 :vm-state
                 :vm-state-heap
                 :vm-heap-counter
                 :vm-call-stack
                 :vm-closure-env
                 ;; VM Heap object classes
                 :vm-heap-object
                 :vm-cons-cell
                 :vm-heap-car
                 :vm-heap-cdr
                 :vm-closure-object
                 :vm-closure-entry-label
                 :vm-closure-params
                 :vm-closure-captured-values
                 ;; VM Instruction classes
                 :vm-instruction
                 :vm-const
                 :vm-move
                 :vm-binop
                 :vm-add
                 :vm-sub
                 :vm-mul
                 :vm-label
                 :vm-jump
                 :vm-jump-zero
                 :vm-print
                 :vm-halt
                 :vm-closure
                 :vm-call
                 :vm-ret
                 :vm-func-ref
                 :vm-push
                 :vm-pop
                 :vm-frame
                 :vm-restore-frame
                 :vm-env-ref
                 ;; VM Heap operations
                 :vm-alloc
                 :vm-cons
                 :vm-car
                 :vm-cdr
                 :vm-rplaca
                 :vm-rplacd
                 ;; VM Heap closure operations
                 :vm-make-closure
                 :vm-make-closure-params
                 :vm-env-regs
                 :vm-closure-ref-idx
                 ;; VM Helpers
                 :vm-reg-get
                 :vm-reg-set
                 :vm-heap-alloc
                 :vm-heap-get
                 :vm-heap-set
                 :vm-closure-ref
                 ;; VM Instruction accessors
                 :vm-dst
                 :vm-src
                 :vm-lhs
                 :vm-rhs
                 :vm-name
                 :vm-label-name
                 :vm-reg
                 :vm-value
                 :vm-captured-vars
                 :vm-func-reg
                 :vm-args
                 :vm-var-name
                 :vm-car-reg
                 :vm-cdr-reg
                 :vm-cons-reg
                 :vm-val-reg
                 :vm-closure-reg
                 :vm-closure-index
                 :vm-alloc-size
                 :execute-instruction
                :instruction->sexp
                 :sexp->instruction
                 ;; Prolog
                 :unify
                 :substitute-variables
                 :logic-var-p
                 :our-macroexpand-all
                 ;; CLOS AST
                 :ast-defclass
                 :ast-defclass-name
                 :ast-defclass-superclasses
                 :ast-defclass-slots
                 :ast-slot-def
                 :ast-slot-name
                 :ast-slot-initarg
                 :ast-slot-initform
                 :ast-slot-reader
                 :ast-slot-writer
                 :ast-slot-accessor
                 :ast-defgeneric
                 :ast-defgeneric-name
                 :ast-defgeneric-params
                 :ast-defmethod
                 :ast-defmethod-name
                 :ast-defmethod-specializers
                 :ast-defmethod-params
                 :ast-defmethod-body
                 :ast-make-instance
                 :ast-make-instance-class
                 :ast-make-instance-initargs
                 :ast-slot-value
                 :ast-slot-value-object
                 :ast-slot-value-slot
                 :ast-set-slot-value
                 :ast-set-slot-value-object
                 :ast-set-slot-value-slot
                 :ast-set-slot-value-value
                 :parse-slot-spec
                 :slot-def-to-sexp
                 ;; Register Allocation
                 :calling-convention
                 :cc-gpr-pool
                 :cc-return-register
                 :cc-scratch-register
                 :*x86-64-calling-convention*
                 :*aarch64-calling-convention*
                 :live-interval
                 :interval-vreg
                 :interval-start
                 :interval-end
                 :interval-phys-reg
                 :interval-spill-slot
                 :regalloc-result
                 :regalloc-spill-count
                 :regalloc-lookup
                 :instruction-defs
                 :instruction-uses
                 :compute-live-intervals
                 :allocate-registers
                 :vm-spill-store
                 :vm-spill-load
                 :our-eval)
  (:import-from :cl-cc/type
                 ;; Type classes
                 :type-node
                 :type-primitive
                 :type-variable
                 :type-function
                 :type-unknown
                 :type-scheme
                 ;; Type accessors
                 :type-primitive-name
                 :type-variable-id
                 :type-variable-name
                 :type-function-params
                 :type-function-return
                 :type-scheme-quantified-vars
                 :type-scheme-type
                 ;; Singleton instances
                 :type-int
                 :type-string
                 :type-bool
                 :type-symbol
                 :type-null
                 :type-any
                 :+type-unknown+
                 ;; Type constructors
                 :make-type-variable
                 :make-type-scheme
                 :type-to-scheme
                 ;; Type predicates and equality
                 :type-variable-p
                 :type-variable-equal-p
                 :type-equal-p
                 :type-to-string
                 ;; Substitution
                 :empty-subst
                 :subst-lookup
                 :extend-subst
                 :type-substitute
                 :compose-subst
                 ;; Unification
                 :type-unify
                 :type-unify-lists
                 :type-occurs-p
                 ;; Free variables
                 :type-free-vars
                 ;; Generalization / Instantiation
                 :generalize
                 :instantiate
                 :normalize-type-variables
                 ;; Type environment
                 :type-env-empty
                 :type-env-extend
                 :type-env-lookup
                 ;; Type inference
                 :fresh-type-var
                 :reset-type-vars!
                 :infer
                 :infer-with-env
                 ;; Conditions
                 :type-inference-error
                 :unbound-variable-error
                 :type-mismatch-error)
  (:export :run-tests :cl-cc-suite))

(in-package :cl-cc/test)

;; Define the main test suite
(def-suite cl-cc-suite
  :description "CL-CC Test - Main suite for all tests")
