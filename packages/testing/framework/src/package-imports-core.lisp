(cl:in-package :cl-cc/test)

;;; Core imports from :cl-cc — AST nodes, VM instructions, heap operations,
;;; Prolog, CLOS AST, and related helpers.
;;; Originally lines 43-281 of package.lisp.

(import '(cl-cc:compile-ast
          ;; VM Program
          cl-cc:vm-program
          cl-cc:vm-program-instructions
          cl-cc:vm-program-result-register
          ;; AST node types
          cl-cc:ast-node
          cl-cc:ast-int
          cl-cc:ast-var
          cl-cc:ast-binop
          cl-cc:ast-if
          cl-cc:ast-progn
          cl-cc:ast-progn-forms
          cl-cc:ast-print
          cl-cc:ast-let
          cl-cc:ast-lambda
          cl-cc:ast-function
          cl-cc:ast-flet
          cl-cc:ast-labels
          cl-cc:ast-block
          cl-cc:ast-return-from
          cl-cc:ast-tagbody
          cl-cc:ast-go
          cl-cc:ast-setq
          cl-cc:ast-multiple-value-call
          cl-cc:ast-multiple-value-prog1
          cl-cc:ast-catch
          cl-cc:ast-throw
          cl-cc:ast-unwind-protect
          cl-cc:ast-handler-case
          cl-cc:ast-handler-case-form
          cl-cc:ast-handler-case-clauses
          cl-cc:ast-call
          cl-cc:ast-quote
          cl-cc:ast-the
          ;; AST constructors (defstruct BOA)
          cl-cc:make-ast-int
          cl-cc:make-ast-var
          cl-cc:make-ast-binop
          cl-cc:make-ast-if
          cl-cc:make-ast-progn
          cl-cc:make-ast-print
          cl-cc:make-ast-let
          cl-cc:make-ast-lambda
          cl-cc:make-ast-function
          cl-cc:make-ast-flet
          cl-cc:make-ast-labels
          cl-cc:make-ast-block
          cl-cc:make-ast-return-from
          cl-cc:make-ast-tagbody
          cl-cc:make-ast-go
          cl-cc:make-ast-setq
          cl-cc:make-ast-call
          cl-cc:make-ast-quote
          cl-cc:make-ast-catch
          cl-cc:make-ast-throw
          cl-cc:make-ast-unwind-protect
          ;; AST accessors
          cl-cc:ast-lambda-params
          cl-cc:ast-lambda-body
          cl-cc:ast-flet-bindings
          cl-cc:ast-labels-bindings
          ;; VM State classes and heap operations
          cl-cc:vm-io-state
          cl-cc:vm-state-heap
          cl-cc:vm-heap-counter
          cl-cc:vm-call-stack
          cl-cc:vm-closure-env
          ;; VM Heap object classes
          cl-cc:vm-heap-object
          cl-cc:vm-cons-cell
          cl-cc:vm-closure-object
          cl-cc:vm-closure-entry-label
          cl-cc:vm-closure-params
          cl-cc:vm-closure-captured-values
          ;; VM Instruction classes
          cl-cc:vm-instruction
          cl-cc:vm-const
          cl-cc:vm-move
          cl-cc:vm-binop
          cl-cc:vm-add
          cl-cc:vm-sub
          cl-cc:vm-mul
          cl-cc:vm-label
          cl-cc:vm-jump
          cl-cc:vm-jump-zero
          cl-cc:vm-print
          cl-cc:vm-halt
          cl-cc:vm-closure
          cl-cc:vm-call
          cl-cc:vm-ret
          cl-cc:vm-func-ref
          cl-cc:vm-push
          cl-cc:vm-pop
          ;; Additional VM instruction types used as hash keys in fold
          ;; tables and as defstruct type tags in emitter tests.
          cl-cc:vm-lcm cl-cc:vm-gcd cl-cc:vm-ash cl-cc:vm-rotate cl-cc:vm-bswap
          cl-cc:vm-logand cl-cc:vm-logior cl-cc:vm-logxor cl-cc:vm-logeqv
          cl-cc:vm-logtest cl-cc:vm-logbitp cl-cc:vm-logcount cl-cc:vm-integer-length
          cl-cc:vm-min cl-cc:vm-max cl-cc:vm-rem cl-cc:vm-mod cl-cc:vm-div cl-cc:vm-truncate
          cl-cc:vm-floor-inst cl-cc:vm-ceiling-inst cl-cc:vm-round-inst
          cl-cc:vm-lognot cl-cc:vm-rational cl-cc:vm-rationalize cl-cc:vm-numerator cl-cc:vm-denominator
          cl-cc:vm-make-array
          cl-cc:vm-eq cl-cc:vm-num-eq cl-cc:vm-lt cl-cc:vm-gt cl-cc:vm-le cl-cc:vm-ge
          cl-cc:vm-and cl-cc:vm-or cl-cc:vm-abs cl-cc:vm-inc cl-cc:vm-dec cl-cc:vm-not
          cl-cc:vm-neg cl-cc:vm-null-p
          cl-cc:vm-concatenate cl-cc:vm-select
          cl-cc:vm-cons-p cl-cc:vm-symbol-p cl-cc:vm-number-p cl-cc:vm-integer-p cl-cc:vm-function-p
          cl-cc:vm-integer-add cl-cc:vm-integer-sub cl-cc:vm-integer-mul
          cl-cc:vm-float-add cl-cc:vm-float-sub cl-cc:vm-float-mul cl-cc:vm-float-div
          ;; VM Instruction constructors (defstruct)
          cl-cc:make-vm-add cl-cc:make-vm-call cl-cc:make-vm-car cl-cc:make-vm-cdr
          cl-cc:make-vm-closure-ref-idx cl-cc:make-vm-cons cl-cc:make-vm-const
          cl-cc:make-vm-halt cl-cc:make-vm-jump-zero cl-cc:make-vm-label
          cl-cc:make-vm-make-closure cl-cc:make-vm-move cl-cc:make-vm-mul
          cl-cc:make-vm-rplaca cl-cc:make-vm-rplacd cl-cc:make-vm-sub
          cl-cc:make-vm-closure cl-cc:make-vm-jump cl-cc:make-vm-ret cl-cc:make-vm-func-ref
          cl-cc:make-vm-print cl-cc:make-vm-spill-load cl-cc:make-vm-spill-store
          ;; Additional constructors for effects/optimizer tests
          cl-cc:make-vm-neg cl-cc:make-vm-inc cl-cc:make-vm-dec
          cl-cc:make-vm-lt cl-cc:make-vm-gt cl-cc:make-vm-le cl-cc:make-vm-ge
          cl-cc:make-vm-null-p cl-cc:make-vm-cons-p cl-cc:make-vm-number-p
          cl-cc:make-vm-set-global cl-cc:make-vm-get-global
          cl-cc:make-vm-not cl-cc:make-vm-lognot
          cl-cc:make-vm-ash cl-cc:make-vm-logand cl-cc:make-vm-logior
          cl-cc:make-vm-logxor cl-cc:make-vm-logeqv cl-cc:make-vm-logtest
          cl-cc:make-vm-logbitp cl-cc:make-vm-logcount cl-cc:make-vm-integer-length
          cl-cc:make-vm-bswap
          cl-cc:make-vm-format-inst cl-cc:make-vm-make-string
          ;; Additional VM constructors required by emitter/codegen tests
          cl-cc:make-vm-abs cl-cc:make-vm-and cl-cc:make-vm-or
          cl-cc:make-vm-eq cl-cc:make-vm-num-eq
          cl-cc:make-vm-div cl-cc:make-vm-mod cl-cc:make-vm-rem cl-cc:make-vm-truncate
          cl-cc:make-vm-min cl-cc:make-vm-max cl-cc:make-vm-rotate
          cl-cc:make-vm-concatenate cl-cc:make-vm-make-array
          cl-cc:make-vm-float-add cl-cc:make-vm-integer-add cl-cc:make-vm-integer-mul
          cl-cc:make-vm-register-function cl-cc:make-vm-select
          ;; VM Heap operations
          cl-cc:vm-cons
          cl-cc:vm-car
          cl-cc:vm-cdr
          cl-cc:vm-rplaca
          cl-cc:vm-rplacd
          ;; VM Heap closure operations
          cl-cc:vm-make-closure
          cl-cc:vm-make-closure-params
          cl-cc:vm-env-regs
          cl-cc:vm-closure-ref-idx
          ;; VM Helpers
          cl-cc:vm-reg-get
          cl-cc:vm-reg-set
          cl-cc:vm-heap-alloc
          cl-cc:vm-heap-get
          cl-cc:vm-heap-set
          ;; VM Instruction accessors
          cl-cc:vm-dst
          cl-cc:vm-src
          cl-cc:vm-lhs
          cl-cc:vm-rhs
          cl-cc:vm-name
          cl-cc:vm-label-name
          cl-cc:vm-reg
          cl-cc:vm-value
          cl-cc:vm-captured-vars
          cl-cc:vm-func-reg
          cl-cc:vm-args
          cl-cc:vm-var-name
          cl-cc:vm-car-reg
          cl-cc:vm-cdr-reg
          cl-cc:vm-cons-reg
          cl-cc:vm-val-reg
          cl-cc:vm-closure-reg
          cl-cc:vm-closure-index
          cl-cc:execute-instruction
          cl-cc:instruction->sexp
          cl-cc:sexp->instruction
          ;; Prolog
          cl-cc:unify
          cl-cc:substitute-variables
          cl-cc:logic-var-p
          ;; CLOS AST
          cl-cc:ast-defclass
          cl-cc:ast-defclass-name
          cl-cc:ast-defclass-superclasses
          cl-cc:ast-defclass-slots
          cl-cc:ast-slot-def
          cl-cc:ast-slot-name
          cl-cc:ast-slot-initarg
          cl-cc:ast-slot-initform
          cl-cc:ast-slot-reader
          cl-cc:ast-slot-writer
          cl-cc:ast-slot-accessor
          cl-cc:ast-defgeneric
          cl-cc:ast-defgeneric-name
          cl-cc:ast-defgeneric-params
          cl-cc:ast-defmethod
          cl-cc:ast-defmethod-name
          cl-cc:ast-defmethod-specializers
          cl-cc:ast-defmethod-params
          cl-cc:ast-defmethod-body
          cl-cc:ast-make-instance
          cl-cc:ast-make-instance-class
          cl-cc:ast-make-instance-initargs
          cl-cc:ast-slot-value
          cl-cc:ast-slot-value-object
          cl-cc:ast-slot-value-slot
          cl-cc:ast-set-slot-value
          cl-cc:ast-set-slot-value-object
          cl-cc:ast-set-slot-value-slot
          cl-cc:ast-set-slot-value-value
          ;; AST predicates needed for PHP tests
          cl-cc:ast-int-p
          cl-cc:ast-int-value
          cl-cc:ast-print-p
          cl-cc:ast-print-expr
          cl-cc:ast-let-p
          cl-cc:ast-setq-p
          cl-cc:ast-if-p
          cl-cc:ast-defun-p
          cl-cc:ast-defun-name
          cl-cc:ast-quote-p
          cl-cc:ast-quote-value
          cl-cc:ast-defclass-p
          cl-cc:ast-call-p
          cl-cc:ast-binop-p
          ;; Prolog type-inference functor atoms
          cl-cc:integer-type cl-cc:boolean-type cl-cc:const cl-cc:binop cl-cc:cmp))
