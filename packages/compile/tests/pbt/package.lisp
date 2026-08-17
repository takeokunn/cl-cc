;;;; tests/pbt/package.lisp - Property-Based Testing Package
;;;
;;; Home of CL-CC's property-based tests. Generation, shrinking and the
;;; pass/fail verdict all belong to cl-weave's native property API
;;; (CL-WEAVE:IT-PROPERTY plus the GEN-* combinators); the home-grown
;;; QuickCheck-style framework this package used to carry is gone.
;;;
;;; No :EXPORT clause: every symbol here is consumed only by other files in this
;;; same directory. The previous export list was ~80 names of framework surface
;;; (DEFPROPERTY, the GEN-* constructors, the GENERATOR protocol, the typed-AST
;;; accessors) and nothing outside packages/compile/tests/pbt/ ever referenced
;;; any of it.

(defpackage :cl-cc/pbt
  (:use :cl)
  (:import-from :cl-weave #:expect)
  (:import-from :cl-cc/test
                 #:cl-cc-integration-suite
                 #:defsuite
                 #:in-suite
                 #:run-string)
  (:import-from :cl-cc
                :ast-node :ast-int :ast-var :ast-binop :ast-if :ast-progn :ast-print
                :ast-let :ast-lambda :ast-function :ast-flet :ast-labels :ast-block
                :ast-return-from :ast-tagbody :ast-go :ast-setq :ast-multiple-value-call
                :ast-multiple-value-prog1 :ast-catch :ast-throw :ast-unwind-protect
                :ast-call :ast-quote :ast-the
                :make-ast-int :make-ast-var :make-ast-binop :make-ast-if :make-ast-progn
                :make-ast-print :make-ast-let :make-ast-lambda :make-ast-function
                :make-ast-flet :make-ast-labels :make-ast-block :make-ast-return-from
                :make-ast-tagbody :make-ast-go :make-ast-setq :make-ast-multiple-value-call
                :make-ast-multiple-value-prog1 :make-ast-call :make-ast-quote :make-ast-the
                :make-ast-catch :make-ast-throw :make-ast-unwind-protect
                :ast-to-sexp :lower-sexp-to-ast
                :ast-int-value :ast-var-name :ast-binop-op :ast-binop-lhs :ast-binop-rhs
                :ast-if-cond :ast-if-then :ast-if-else :ast-progn-forms :ast-print-expr
                :ast-let-bindings :ast-let-body :ast-lambda-params :ast-lambda-body
                :ast-function-name :ast-flet-bindings :ast-flet-body :ast-labels-bindings
                :ast-labels-body :ast-block-name :ast-block-body :ast-return-from-name
                :ast-return-from-value :ast-tagbody-tags :ast-go-tag :ast-setq-var
                :ast-setq-value :ast-mv-call-func :ast-mv-call-args :ast-mv-prog1-first
                :ast-mv-prog1-forms :ast-catch-tag :ast-catch-body :ast-throw-tag
                :ast-throw-value :ast-unwind-protected :ast-unwind-cleanup :ast-call-func
                :ast-call-args :ast-quote-value :ast-the-type :ast-the-value
                :cps-transform
                :vm-state :vm-io-state :vm-heap-counter :vm-cons-cell :vm-closure-object
                :vm-closure-entry-label :vm-closure-params :vm-closure-captured-regs :vm-closure-captured-vals
                :vm-const :vm-move :vm-add :vm-sub :vm-mul :vm-cons :vm-car :vm-cdr
                :vm-rplaca :vm-rplacd :vm-make-closure :vm-closure-ref-idx
                :make-vm-add :make-vm-car :make-vm-cdr :make-vm-closure-ref-idx
                :make-vm-cons :make-vm-const :make-vm-make-closure :make-vm-move
                :make-vm-mul :make-vm-rplaca :make-vm-rplacd :make-vm-sub
                :vm-reg-get :vm-reg-set :vm-heap-get :execute-instruction
                :instruction->sexp :sexp->instruction :vm-dst :vm-src :vm-lhs :vm-rhs
                :vm-value :vm-car-reg :vm-cdr-reg :vm-cons-reg :vm-val-reg
                :vm-closure-reg :vm-closure-index)
  (:import-from :cl-prolog-kit
                :unify
                :logic-substitute :logic-var-p))

(in-package :cl-cc/pbt)
