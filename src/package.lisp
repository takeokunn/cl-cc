(defpackage :cl-cc
  (:use :cl)
  (:export
    ;; Core compiler
    :parse-source
    :parse-all-forms
    :cps-transform
    :cps-transform-ast
    :cps-transform-ast*
    :cps-transform-eval
   :compile-expression
   :compile-string
   :emit-assembly
   :run-compiled
   :run-string
   :run-string-typed
   :our-eval
   :type-check-ast
   :lookup-function-type
   :register-function-type
   :*function-type-registry*

   ;; Prolog system
   :logic-var-p
   :unify
   :occurs-check
   :logic-substitute
   :substitute-variables
   :def-fact
   :def-rule
   :query-all
   :query-one
   :query-first-n
   :solve-goal
   :solve-conjunction
   :clear-prolog-database
   :*prolog-rules*
   :prolog-goal
   :prolog-rule
   :goal-predicate
   :goal-args
   :rule-head
   :rule-body
   :rename-variables
   :apply-prolog-peephole
   :*enable-prolog-peephole*

   ;; Macro System
   :macro-env
   :macro-env-table
   :compilation-env
   :env-macros
   :env-functions
   :env-variables
   :env-parent
   :make-compilation-env
   :env-lookup-macro
   :env-lookup-function
   :env-lookup-variable
   :env-add-macro
   :env-add-function
   :env-add-variable
   :parse-lambda-list
   :destructure-lambda-list
   :our-defmacro
   :our-macroexpand-1
   :our-macroexpand
   :our-macroexpand-all
   :register-macro
   :lookup-macro
   :*macro-environment*

    ;; Built-in macro exports
    :our-d

    ;; Control flow macro exports
    :dolist
    :dotimes
    :do
    :do*
    :case
    :typecase
    :loop

   ;; ----------------------------------------------------------------------------
   ;; AST Base Class and Source Location
   ;; ----------------------------------------------------------------------------
   :ast-node
   :ast-source-file
   :ast-source-line
   :ast-source-column
   :ast-location-string
   :ast-error
   :ast-compilation-error

   ;; ----------------------------------------------------------------------------
   ;; Core AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-int
   :ast-int-value
   :ast-var
   :ast-var-name
   :ast-binop
   :ast-binop-op
   :ast-binop-lhs
   :ast-binop-rhs
   :ast-if
   :ast-if-cond
   :ast-if-then
   :ast-if-else
   :ast-progn
   :ast-progn-forms
   :ast-print
   :ast-print-expr
   :ast-let
   :ast-let-bindings
   :ast-let-body

   ;; ----------------------------------------------------------------------------
   ;; Function and Lambda AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-lambda
   :ast-lambda-params
   :ast-lambda-optional-params
   :ast-lambda-rest-param
   :ast-lambda-key-params
   :ast-lambda-body
   :ast-lambda-env
   :ast-function
   :ast-function-name
   :ast-flet
   :ast-flet-bindings
   :ast-flet-body
   :ast-labels
   :ast-labels-bindings
   :ast-labels-body
   :ast-call
   :ast-call-func
   :ast-call-args

   ;; ----------------------------------------------------------------------------
   ;; Block and Control Flow AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-block
   :ast-block-name
   :ast-block-body
   :ast-return-from
   :ast-return-from-name
   :ast-return-from-value
   :ast-tagbody
   :ast-tagbody-tags
   :ast-go
   :ast-go-tag

   ;; ----------------------------------------------------------------------------
   ;; Assignment AST Class
   ;; ----------------------------------------------------------------------------
   :ast-setq
   :ast-setq-var
   :ast-setq-value

   ;; ----------------------------------------------------------------------------
   ;; Multiple Values AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-multiple-value-call
   :ast-mv-call-func
   :ast-mv-call-args
   :ast-multiple-value-prog1
   :ast-mv-prog1-first
   :ast-mv-prog1-forms
   :ast-values
   :ast-values-forms
   :ast-multiple-value-bind
   :ast-mvb-vars
   :ast-mvb-values-form
   :ast-mvb-body
   :ast-apply
   :ast-apply-func
   :ast-apply-args

   ;; ----------------------------------------------------------------------------
   ;; Exception Handling AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-catch
   :ast-catch-tag
   :ast-catch-body
   :ast-throw
   :ast-throw-tag
   :ast-throw-value
   :ast-unwind-protect
   :ast-unwind-protected
   :ast-unwind-cleanup
   :ast-handler-case
   :ast-handler-case-form
   :ast-handler-case-clauses

   ;; ----------------------------------------------------------------------------
   ;; Additional AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-quote
   :ast-quote-value
   :ast-the
   :ast-the-type
   :ast-the-value

   ;; ----------------------------------------------------------------------------
   ;; Top-Level Definition AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-defun
   :ast-defun-name
   :ast-defun-params
   :ast-defun-optional-params
   :ast-defun-rest-param
   :ast-defun-key-params
   :ast-defun-body
   :ast-defvar
   :ast-defvar-name
   :ast-defvar-value
   :ast-defmacro
   :ast-defmacro-name
   :ast-defmacro-lambda-list
   :ast-defmacro-body

   ;; ----------------------------------------------------------------------------
   ;; CLOS AST Classes
   ;; ----------------------------------------------------------------------------
   :ast-slot-def
   :ast-slot-name
   :ast-slot-initarg
   :ast-slot-initform
   :ast-slot-reader
   :ast-slot-writer
   :ast-slot-accessor
   :ast-slot-type
   :ast-defclass
   :ast-defclass-name
   :ast-defclass-superclasses
   :ast-defclass-slots
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
   :ast-set-gethash
   :ast-set-gethash-key
   :ast-set-gethash-table
   :ast-set-gethash-value
   :parse-compiler-lambda-list
   :lambda-list-has-extended-p
   :parse-slot-spec
   :slot-def-to-sexp

   ;; ----------------------------------------------------------------------------
   ;; Multi-Form Compilation
   ;; ----------------------------------------------------------------------------
   :compile-toplevel-forms

   ;; ----------------------------------------------------------------------------
   ;; AST Transformation Utilities
   ;; ----------------------------------------------------------------------------
   :lower-sexp-to-ast
   :ast-to-sexp
   :make-ast-with-location

   ;; ----------------------------------------------------------------------------
   ;; Source Location Structures and Functions
   ;; ----------------------------------------------------------------------------
   :source-location
   :source-location-p
   :source-location-file
   :source-location-line
   :source-location-column
   :source-location-position
   :source-annotated-sexp
   :source-annotated-sexp-p
   :source-annotated-sexp-sexp
   :print-location
   :format-with-location

   ;; ----------------------------------------------------------------------------
   ;; Enhanced Reader Functions
   ;; ----------------------------------------------------------------------------
   :read-with-location
   :read-from-string-with-location
   :read-all-with-locations
   :read-file-with-locations
    :parse-source-with-location
    :parse-source-from-file
    :lower-sexp-with-location-to-ast
    :reader-error-with-location

     ;; ----------------------------------------------------------------------------
     ;; VM Heap Object Classes
     ;; ----------------------------------------------------------------------------
     :vm-heap-object
     :vm-heap-address
     :vm-heap-address-value
     :vm-cons-cell
     :vm-cons-cell-car
     :vm-cons-cell-cdr
     :vm-heap-car
     :vm-heap-cdr
     :vm-closure-object
     :vm-closure-entry-label
     :vm-closure-params
     :vm-closure-captured-values

    ;; ----------------------------------------------------------------------------
    ;; VM Instruction Classes
    ;; ----------------------------------------------------------------------------
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
    :vm-values
    :vm-mv-bind
    :vm-dst-regs
    :vm-apply
    :vm-register-function
    :vm-function-registry
    :vm-resolve-function
    :vm-generic-function-p
    :vm-resolve-gf-method
    :vm-set-global
    :vm-get-global
    :vm-global-name
    :vm-global-vars
    :vm-values-list
    :vm-establish-handler
    :vm-remove-handler
    :vm-signal-error
    :vm-sync-handler-regs
    :vm-handler-label
    :vm-handler-result-reg
    :vm-error-type
    :vm-error-reg
    :vm-handler-stack

    ;; ----------------------------------------------------------------------------
    ;; VM Heap Operations
    ;; ----------------------------------------------------------------------------
    :vm-alloc
    :vm-alloc-size
    :vm-cons
    :vm-car-reg
    :vm-cdr-reg
    :vm-car
    :vm-cdr
    :vm-rplaca
    :vm-rplacd
    :vm-cons-reg
    :vm-val-reg

    ;; ----------------------------------------------------------------------------
    ;; VM Closure Heap Operations
    ;; ----------------------------------------------------------------------------
    :vm-make-closure
    :vm-make-closure-params
    :vm-env-regs
    :vm-closure-ref-idx
    :vm-closure-reg
    :vm-closure-index

    ;; ----------------------------------------------------------------------------
    ;; VM State and Utilities
    ;; ----------------------------------------------------------------------------
    :vm-program
    :vm-program-instructions
    :vm-program-result-register
    :vm-state
    :vm-state-registers
    :vm-state-heap
    :vm-heap-counter
    :vm-call-stack
    :vm-frame-pointer
    :vm-return-stack
     :vm-closure-env
     :vm-heap-alloc
     :vm-heap-get
     :vm-heap-set
     :get-vm-heap
     :vm-reg-get
     :vm-reg-set
     :vm-closure-ref

    ;; ----------------------------------------------------------------------------
    ;; VM Execution
    ;; ----------------------------------------------------------------------------
    :execute-instruction

    ;; ----------------------------------------------------------------------------
    ;; Instruction Conversion
    ;; ----------------------------------------------------------------------------
    :instruction->sexp
    :sexp->instruction
    :build-label-table
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

     ;; ----------------------------------------------------------------------------
     ;; VM Primitive Instructions - Type Predicates
     ;; ----------------------------------------------------------------------------
     :vm-eq
     :vm-cons-p
     :vm-null-p
     :vm-symbol-p
     :vm-number-p
     :vm-integer-p
     :vm-function-p

     ;; ----------------------------------------------------------------------------
     ;; VM Primitive Instructions - Comparisons
     ;; ----------------------------------------------------------------------------
     :vm-lt
     :vm-gt
     :vm-le
     :vm-ge
     :vm-num-eq

     ;; ----------------------------------------------------------------------------
     ;; VM Primitive Instructions - Arithmetic Extensions
     ;; ----------------------------------------------------------------------------
     :vm-div
     :vm-mod
     :vm-neg
     :vm-abs
     :vm-inc
     :vm-dec

     ;; ----------------------------------------------------------------------------
     ;; VM Primitive Instructions - Boolean Operations
     ;; ----------------------------------------------------------------------------
     :vm-not
     :vm-and
     :vm-or

     ;; ----------------------------------------------------------------------------
     ;; VM List Operations - Instruction Classes
     ;; ----------------------------------------------------------------------------
     :vm-cons
     :vm-car
     :vm-cdr
     :vm-list
     :vm-length
     :vm-reverse
     :vm-append
     :vm-member
     :vm-nth
     :vm-nthcdr
     :vm-first
     :vm-second
     :vm-third
     :vm-fourth
     :vm-fifth
     :vm-rest
     :vm-last
     :vm-butlast
     :vm-rplaca
     :vm-rplacd
     :vm-list-length
     :vm-endp
     :vm-null
     :vm-push
     :vm-pop

     ;; ----------------------------------------------------------------------------
     ;; VM List Operations - Accessors
     ;; ----------------------------------------------------------------------------
     :vm-car-reg
     :vm-cdr-reg
     :vm-list-count
     :vm-src-regs
     :vm-item-reg
     :vm-list-reg
     :vm-index-reg
     :vm-cons-reg
     :vm-val-reg

     ;; ----------------------------------------------------------------------------
     ;; VM I/O State and Operations
     ;; ----------------------------------------------------------------------------
     :vm-io-state
     :vm-open-files
     :vm-file-counter
     :vm-standard-input
     :vm-standard-output
     :vm-string-streams
     :+stdin-handle+
     :+stdout-handle+
     :+eof-value+
     :vm-get-stream
     :vm-allocate-file-handle
     :vm-stream-open-p
     :run-compiled-with-io
     :run-string-with-io

     ;; ----------------------------------------------------------------------------
     ;; VM I/O Instruction Classes
     ;; ----------------------------------------------------------------------------
     :vm-open-file
     :vm-close-file
     :vm-read-char
     :vm-read-line
     :vm-write-char
     :vm-write-string
     :vm-peek-char
     :vm-unread-char
     :vm-file-position
     :vm-file-length
     :vm-eof-p
     :vm-make-string-stream
     :vm-get-string-from-stream

     ;; ----------------------------------------------------------------------------
     ;; VM I/O Instruction Accessors
     ;; ----------------------------------------------------------------------------
     :vm-file-handle
     :vm-file-direction
     :vm-char-reg
     :vm-str-reg
     :vm-position-reg
     :vm-stream-direction
     :vm-initial-string
      :io-sexp->instruction

      ;; ----------------------------------------------------------------------------
      ;; VM Reader/Printer - Reader State
      ;; ----------------------------------------------------------------------------
      :vm-reader-state
      :vm-reader-state-p
      :vrs-stream
      :vrs-char
      :vrs-line
      :vrs-column
      :vrs-eof-reached
      :vrs-readtable
      :make-vm-reader-state
      :vm-reader-advance
      :vm-reader-peek
      :vm-reader-skip-whitespace

      ;; ----------------------------------------------------------------------------
      ;; VM Reader/Printer - Reader Functions
      ;; ----------------------------------------------------------------------------
      :vm-read
      :vm-read-preserving-whitespace
      :vm-read-from-string
      :vm-read-delimited-list
      :vm-read-all-from-string
      :vm-read-expr

      ;; ----------------------------------------------------------------------------
      ;; VM Reader/Printer - Printer Functions
      ;; ----------------------------------------------------------------------------
      :vm-prin1
      :vm-princ
      :vm-print
      :vm-write-to-string
      :vm-prin1-to-string
      :vm-princ-to-string

      ;; ----------------------------------------------------------------------------
      ;; VM Reader/Printer - Conditions
      ;; ----------------------------------------------------------------------------
      :vm-reader-error
      :vm-reader-error-message
      :vm-reader-error-line
      :vm-reader-error-column

      ;; ----------------------------------------------------------------------------
      ;; VM Reader/Printer - Instruction Classes
      ;; ----------------------------------------------------------------------------
      :vm-read-sexp
      :vm-read-from-string-instruction
      :vm-print-sexp
      :vm-prin1-sexp
      :vm-princ-sexp
      :vm-write-to-string-instruction
      :vm-make-string-output-stream-inst
      :vm-get-output-stream-string-inst
      :vm-stream-write-string-inst
      :vm-stream-reg
      :vm-read-from-string-inst
      :vm-read-sexp-inst
      ;; VM Primitives - recently added
      :vm-eval
      :vm-type-of
      :vm-make-list
      :vm-alphanumericp
      :vm-make-reader
      :vm-reader-read
      :vm-reader-advance-instruction
      :vm-reader-peek-instruction
      :vm-reader-reg
      :vm-stream-reg
      :vm-string-reg

       ;; ----------------------------------------------------------------------------
       ;; VM Reader/Printer - Heap Objects
       ;; ----------------------------------------------------------------------------
       :vm-reader-state-object
       :vm-rso-stream
       :vm-rso-line
       :vm-rso-column
       :vm-rso-char
       :vm-rso-eof-reached

       ;; ----------------------------------------------------------------------------
       ;; VM String Operations - Instruction Classes
       ;; ----------------------------------------------------------------------------
       :vm-string=
       :vm-string<
       :vm-string>
       :vm-string<=
       :vm-string>=
       :vm-string-equal
       :vm-string-lessp
       :vm-string-greaterp
       :vm-string-not-equal
       :vm-string-length
       :vm-char
       :vm-char-code
       :vm-code-char
       :vm-char=
       :vm-char<
       :vm-subseq
       :vm-concatenate
       :vm-string-upcase
       :vm-string-downcase
       :vm-string-capitalize
       :vm-string-trim
       :vm-string-left-trim
       :vm-string-right-trim
       :vm-search-string

       ;; ----------------------------------------------------------------------------
       ;; VM String Operations - Instruction Accessors
       ;; ----------------------------------------------------------------------------
       :vm-str1
       :vm-str2
       :vm-string-reg
       :vm-index
       :vm-char1
       :vm-char2
       :vm-start
       :vm-end
       :vm-char-bag
       :vm-pattern

       ;; ----------------------------------------------------------------------------
       ;; VM Hash Table Operations - Heap Object
       ;; ----------------------------------------------------------------------------
       :vm-hash-table-object
       :vm-hash-table-internal

       ;; ----------------------------------------------------------------------------
       ;; VM Hash Table Operations - Instruction Classes
       ;; ----------------------------------------------------------------------------
       :vm-make-hash-table
       :vm-gethash
       :vm-sethash
       :vm-remhash
       :vm-clrhash
       :vm-hash-table-count
       :vm-hash-table-p
       :vm-maphash
       :vm-hash-table-keys
       :vm-hash-table-values
       :vm-hash-table-test

        ;; ----------------------------------------------------------------------------
        ;; VM Hash Table Operations - Instruction Accessors
        ;; ----------------------------------------------------------------------------
        :vm-hash-test
        :vm-found-dst
        :vm-hash-key
        :vm-hash-value
        :vm-hash-table-reg
        :vm-hash-default
        :vm-hash-fn

       ;; ----------------------------------------------------------------------------
       ;; CLOS VM Instructions
       ;; ----------------------------------------------------------------------------
       :vm-class-def
       :vm-class-name-sym
       :vm-superclasses
       :vm-slot-names
       :vm-slot-initargs
       :vm-make-obj
       :vm-class-reg
       :vm-initarg-regs
       :vm-slot-read
       :vm-obj-reg
       :vm-slot-name-sym
       :vm-slot-write
       :vm-value-reg
       :vm-register-method
       :vm-gf-reg
       :vm-method-specializer
       :vm-method-reg
       :vm-generic-call

       ;; ----------------------------------------------------------------------------
       ;; CLOS Compiler Context
       ;; ----------------------------------------------------------------------------
       :ctx-global-classes
       :ctx-global-generics

       ;; ----------------------------------------------------------------------------
       ;; Backend Code Generation
       ;; ----------------------------------------------------------------------------
       :compile-to-x86-64-bytes

       ;; ----------------------------------------------------------------------------
       ;; Register Allocation
       ;; ----------------------------------------------------------------------------
       :calling-convention
       :cc-gpr-pool
       :cc-caller-saved
       :cc-callee-saved
       :cc-arg-registers
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
       :regalloc-assignment
       :regalloc-spill-map
       :regalloc-spill-count
       :regalloc-instructions
       :regalloc-lookup
       :instruction-defs
       :instruction-uses
       :compute-live-intervals
       :linear-scan-allocate
       :allocate-registers
       :vm-spill-store
       :vm-spill-load
       :vm-spill-src
       :vm-spill-dst
       :vm-spill-slot

       :vm-reg-to-x86-with-alloc
       :*current-regalloc*
       :*phys-reg-to-x86-code*
       :*phys-reg-to-asm-string*
       :target-regalloc

       ;; ----------------------------------------------------------------------------
       ;; Native Compilation
       ;; ----------------------------------------------------------------------------
       :compile-to-native
       :compile-file-to-native
       :compile-toplevel-forms))

(in-package :cl-cc)
