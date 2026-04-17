;;;; packages/foundation/ast/src/package.lisp - CL-CC AST Package
;;;;
;;;; Phase 1.2 of the package-by-feature monorepo migration. This package owns
;;;; the AST node struct definitions and the ast-children / ast-bound-names
;;;; data protocol. The facade :cl-cc package uses :cl-cc/ast so downstream
;;;; compiler modules continue to see AST symbols unqualified.

(defpackage :cl-cc/ast
  (:use :cl)
  (:export
   ;; Base and intermediate structs
   #:ast-node #:make-ast-node #:ast-node-p
   #:ast-source-file #:ast-source-line #:ast-source-column
   #:ast-callable #:make-ast-callable #:ast-callable-p
   #:ast-local-fns #:make-ast-local-fns #:ast-local-fns-p
   #:ast-local-fns-bindings #:ast-local-fns-body
   ;; Leaves
   #:ast-int #:make-ast-int #:ast-int-p #:ast-int-value
   #:ast-var #:make-ast-var #:ast-var-p #:ast-var-name
   #:ast-hole #:make-ast-hole #:ast-hole-p
   #:ast-binop #:make-ast-binop #:ast-binop-p
   #:ast-binop-op #:ast-binop-lhs #:ast-binop-rhs
   #:ast-if #:make-ast-if #:ast-if-p
   #:ast-if-cond #:ast-if-then #:ast-if-else
   #:ast-progn #:make-ast-progn #:ast-progn-p #:ast-progn-forms
   #:ast-print #:make-ast-print #:ast-print-p #:ast-print-expr
   #:ast-let #:make-ast-let #:ast-let-p
   #:ast-let-bindings #:ast-let-declarations #:ast-let-body
   ;; Function/lambda
   #:ast-lambda #:make-ast-lambda #:ast-lambda-p
   #:ast-lambda-params #:ast-lambda-optional-params #:ast-lambda-rest-param
   #:ast-lambda-key-params #:ast-lambda-declarations #:ast-lambda-body #:ast-lambda-env
   #:ast-function #:make-ast-function #:ast-function-p #:ast-function-name
   #:ast-flet #:make-ast-flet #:ast-flet-p
   #:ast-flet-bindings #:ast-flet-body
   #:ast-labels #:make-ast-labels #:ast-labels-p
   #:ast-labels-bindings #:ast-labels-body
   #:ast-defun #:make-ast-defun #:ast-defun-p
   #:ast-defun-name #:ast-defun-params #:ast-defun-optional-params
   #:ast-defun-rest-param #:ast-defun-key-params
   #:ast-defun-declarations #:ast-defun-body
   #:ast-defvar #:make-ast-defvar #:ast-defvar-p
   #:ast-defvar-name #:ast-defvar-value #:ast-defvar-kind
   #:ast-defmacro #:make-ast-defmacro #:ast-defmacro-p
   #:ast-defmacro-name #:ast-defmacro-lambda-list #:ast-defmacro-body
   ;; Block/control
   #:ast-block #:make-ast-block #:ast-block-p
   #:ast-block-name #:ast-block-body
   #:ast-return-from #:make-ast-return-from #:ast-return-from-p
   #:ast-return-from-name #:ast-return-from-value
   #:ast-tagbody #:make-ast-tagbody #:ast-tagbody-p #:ast-tagbody-tags
   #:ast-go #:make-ast-go #:ast-go-p #:ast-go-tag
   ;; Assignment
   #:ast-setq #:make-ast-setq #:ast-setq-p
   #:ast-setq-var #:ast-setq-value
   ;; Multiple values
   #:ast-multiple-value-call #:make-ast-multiple-value-call #:ast-multiple-value-call-p
   #:ast-mv-call-func #:ast-mv-call-args
   #:ast-multiple-value-prog1 #:make-ast-multiple-value-prog1 #:ast-multiple-value-prog1-p
   #:ast-mv-prog1-first #:ast-mv-prog1-forms
   #:ast-values #:make-ast-values #:ast-values-p #:ast-values-forms
   #:ast-multiple-value-bind #:make-ast-multiple-value-bind #:ast-multiple-value-bind-p
   #:ast-mvb-vars #:ast-mvb-values-form #:ast-mvb-body
   #:ast-apply #:make-ast-apply #:ast-apply-p
   #:ast-apply-func #:ast-apply-args
   ;; Exception
   #:ast-catch #:make-ast-catch #:ast-catch-p
   #:ast-catch-tag #:ast-catch-body
   #:ast-throw #:make-ast-throw #:ast-throw-p
   #:ast-throw-tag #:ast-throw-value
   #:ast-unwind-protect #:make-ast-unwind-protect #:ast-unwind-protect-p
   #:ast-unwind-protected #:ast-unwind-cleanup
   #:ast-handler-case #:make-ast-handler-case #:ast-handler-case-p
   #:ast-handler-case-form #:ast-handler-case-clauses
   ;; Call/quote/the
   #:ast-call #:make-ast-call #:ast-call-p
   #:ast-call-func #:ast-call-args
   #:ast-quote #:make-ast-quote #:ast-quote-p #:ast-quote-value
   #:ast-the #:make-ast-the #:ast-the-p
   #:ast-the-type #:ast-the-value
   ;; CLOS
   #:ast-slot-def #:make-ast-slot-def #:ast-slot-def-p
   #:ast-slot-name #:ast-slot-initarg #:ast-slot-initform
   #:ast-slot-reader #:ast-slot-writer #:ast-slot-accessor
   #:ast-slot-type #:ast-slot-allocation
   #:ast-defclass #:make-ast-defclass #:ast-defclass-p
   #:ast-defclass-name #:ast-defclass-superclasses
   #:ast-defclass-slots #:ast-defclass-default-initargs
   #:ast-defgeneric #:make-ast-defgeneric #:ast-defgeneric-p
   #:ast-defgeneric-name #:ast-defgeneric-params #:ast-defgeneric-combination
   #:ast-defmethod #:make-ast-defmethod #:ast-defmethod-p
   #:ast-defmethod-name #:ast-defmethod-qualifier #:ast-defmethod-specializers
   #:ast-defmethod-params #:ast-defmethod-body
   #:ast-make-instance #:make-ast-make-instance #:ast-make-instance-p
   #:ast-make-instance-class #:ast-make-instance-initargs
   #:ast-slot-value #:make-ast-slot-value #:ast-slot-value-p
   #:ast-slot-value-object #:ast-slot-value-slot
   #:ast-set-slot-value #:make-ast-set-slot-value #:ast-set-slot-value-p
   #:ast-set-slot-value-object #:ast-set-slot-value-slot #:ast-set-slot-value-value
   #:ast-set-gethash #:make-ast-set-gethash #:ast-set-gethash-p
   #:ast-set-gethash-key #:ast-set-gethash-table #:ast-set-gethash-value
   ;; Data layer protocol
   #:ast-children #:ast-bound-names
   ;; Source utilities + error reporting
   #:ast-location-string
   #:ast-compilation-error
   #:ast-error-location #:ast-error-format-control #:ast-error-format-arguments
   #:ast-error))
