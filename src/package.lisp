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

   ;; AST Base Class and Source Location
   :ast-node
   :ast-source-file
   :ast-source-line
   :ast-source-column
   :ast-location-string
   :ast-error
   :ast-compilation-error

   ;; Core AST Classes
   :ast-int
   :make-ast-int
   :ast-int-value
   :ast-var
   :make-ast-var
   :ast-var-name
   :ast-binop
   :make-ast-binop
   :ast-binop-op
   :ast-binop-lhs
   :ast-binop-rhs
   :ast-if
   :make-ast-if
   :ast-if-cond
   :ast-if-then
   :ast-if-else
   :ast-progn
   :make-ast-progn
   :ast-progn-forms
   :ast-print
   :make-ast-print
   :ast-print-expr
   :ast-let
   :make-ast-let
   :ast-let-bindings
   :ast-let-body

   ;; Function and Lambda AST Classes
   :ast-lambda
   :make-ast-lambda
   :ast-lambda-params
   :ast-lambda-optional-params
   :ast-lambda-rest-param
   :ast-lambda-key-params
   :ast-lambda-body
   :ast-lambda-env
   :ast-function
   :make-ast-function
   :ast-function-name
   :ast-flet
   :make-ast-flet
   :ast-flet-bindings
   :ast-flet-body
   :ast-labels
   :make-ast-labels
   :ast-labels-bindings
   :ast-labels-body
   :ast-call
   :make-ast-call
   :ast-call-func
   :ast-call-args

   ;; Block and Control Flow AST Classes
   :ast-block
   :make-ast-block
   :ast-block-name
   :ast-block-body
   :ast-return-from
   :make-ast-return-from
   :ast-return-from-name
   :ast-return-from-value
   :ast-tagbody
   :make-ast-tagbody
   :ast-tagbody-tags
   :ast-go
   :make-ast-go
   :ast-go-tag

   ;; Assignment AST Class
   :ast-setq
   :make-ast-setq
   :ast-setq-var
   :ast-setq-value

   ;; Multiple Values AST Classes
   :ast-multiple-value-call
   :make-ast-multiple-value-call
   :ast-mv-call-func
   :ast-mv-call-args
   :ast-multiple-value-prog1
   :make-ast-multiple-value-prog1
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

   ;; Exception Handling AST Classes
   :ast-catch
   :make-ast-catch
   :ast-catch-tag
   :ast-catch-body
   :ast-throw
   :make-ast-throw
   :ast-throw-tag
   :ast-throw-value
   :ast-unwind-protect
   :make-ast-unwind-protect
   :ast-unwind-protected
   :ast-unwind-cleanup
   :ast-handler-case
   :ast-handler-case-form
   :ast-handler-case-clauses

   ;; Additional AST Classes
   :ast-quote
   :make-ast-quote
   :ast-quote-value
   :ast-the
   :make-ast-the
   :ast-the-type
   :ast-the-value

   ;; Top-Level Definition AST Classes
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

   ;; CLOS AST Classes
   :ast-slot-def
   :make-ast-slot-def
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

   ;; Multi-Form Compilation
   :compile-toplevel-forms

   ;; Compilation Result
   :compilation-result
   :make-compilation-result
   :compilation-result-program
   :compilation-result-assembly
   :compilation-result-globals
   :compilation-result-type
   :compilation-result-cps

   ;; AST Transformation Utilities
   :lower-sexp-to-ast
   :ast-to-sexp
   :make-ast-with-location

   ;; Source Location Structures and Functions
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

   ;; Enhanced Reader Functions
   :read-with-location
   :read-from-string-with-location
   :read-all-with-locations
   :read-file-with-locations
    :parse-source-with-location
    :parse-source-from-file
    :lower-sexp-with-location-to-ast
    :reader-error-with-location

     ;; VM Heap Object Classes
     :vm-heap-object
     :vm-heap-address
     :make-vm-heap-address
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

    ;; VM Instruction Classes
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

    ;; VM Instruction Constructors (defstruct make-* functions)
    :make-vm-instruction
    :make-vm-abs :make-vm-acons :make-vm-add :make-vm-alpha-char-p
    :make-vm-alphanumericp :make-vm-and :make-vm-append :make-vm-apply
    :make-vm-aref :make-vm-array-length :make-vm-aset :make-vm-assoc
    :make-vm-atom :make-vm-bind-restart :make-vm-binop :make-vm-butlast
    :make-vm-call :make-vm-car :make-vm-cdr :make-vm-ceiling-inst
    :make-vm-cerror :make-vm-char :make-vm-char-code :make-vm-char-downcase
    :make-vm-char-upcase :make-vm-char< :make-vm-char= :make-vm-characterp
    :make-vm-class-def :make-vm-close-file :make-vm-closure
    :make-vm-closure-ref-idx :make-vm-clrhash :make-vm-code-char
    :make-vm-coerce-to-list :make-vm-coerce-to-string :make-vm-coerce-to-vector
    :make-vm-concatenate :make-vm-cons :make-vm-cons-p :make-vm-const
    :make-vm-copy-list :make-vm-copy-tree :make-vm-dec :make-vm-digit-char-p
    :make-vm-div :make-vm-endp :make-vm-env-ref :make-vm-eof-p :make-vm-eq
    :make-vm-equal :make-vm-error-instruction :make-vm-establish-handler
    :make-vm-eval :make-vm-evenp :make-vm-fifth :make-vm-file-length
    :make-vm-file-position :make-vm-first :make-vm-floor-inst
    :make-vm-format-inst :make-vm-fourth :make-vm-fresh-line-inst
    :make-vm-func-ref :make-vm-function-p :make-vm-ge :make-vm-generic-call
    :make-vm-gensym-inst :make-vm-get-global
    :make-vm-get-output-stream-string-inst :make-vm-get-string-from-stream
    :make-vm-gethash :make-vm-gt :make-vm-halt :make-vm-hash-table-count
    :make-vm-hash-table-keys :make-vm-hash-table-p :make-vm-hash-table-test
    :make-vm-hash-table-values :make-vm-inc :make-vm-integer-p
    :make-vm-intern-symbol :make-vm-invoke-restart :make-vm-jump
    :make-vm-jump-zero :make-vm-keywordp :make-vm-label :make-vm-last
    :make-vm-le :make-vm-length :make-vm-list :make-vm-list-length
    :make-vm-listp :make-vm-lower-case-p :make-vm-lt :make-vm-make-array
    :make-vm-make-closure :make-vm-make-hash-table :make-vm-make-list
    :make-vm-make-obj :make-vm-make-reader
    :make-vm-make-string-output-stream-inst :make-vm-make-string-stream
    :make-vm-make-symbol :make-vm-maphash :make-vm-max :make-vm-member
    :make-vm-min :make-vm-mod :make-vm-move :make-vm-mul :make-vm-mv-bind
    :make-vm-nconc :make-vm-neg :make-vm-not :make-vm-nreverse :make-vm-nth
    :make-vm-nthcdr :make-vm-null :make-vm-null-p :make-vm-num-eq
    :make-vm-number-p :make-vm-oddp :make-vm-open-file :make-vm-or
    :make-vm-parse-integer :make-vm-peek-char :make-vm-pop :make-vm-pop-handler
    :make-vm-prin1 :make-vm-prin1-sexp :make-vm-princ :make-vm-princ-sexp
    :make-vm-print :make-vm-print-inst :make-vm-print-sexp :make-vm-push
    :make-vm-push-handler :make-vm-read-char :make-vm-read-from-string-inst
    :make-vm-read-from-string-rp :make-vm-read-line :make-vm-read-sexp
    :make-vm-read-sexp-inst :make-vm-reader-advance-rp :make-vm-reader-peek-rp
    :make-vm-reader-read :make-vm-register-function :make-vm-register-method
    :make-vm-rem :make-vm-remhash :make-vm-remove-handler :make-vm-rest
    :make-vm-ret :make-vm-reverse :make-vm-rplaca :make-vm-rplacd
    :make-vm-search-string :make-vm-second :make-vm-set-global :make-vm-sethash
    :make-vm-signal :make-vm-signal-error :make-vm-slot-read :make-vm-slot-write
    :make-vm-spill-load :make-vm-spill-store
    :make-vm-stream-write-string-inst :make-vm-string-capitalize
    :make-vm-string-coerce :make-vm-string-downcase :make-vm-string-equal
    :make-vm-string-greaterp :make-vm-string-left-trim :make-vm-string-length
    :make-vm-string-lessp :make-vm-string-not-equal :make-vm-string-right-trim
    :make-vm-string-trim :make-vm-string-upcase :make-vm-string<
    :make-vm-string<= :make-vm-string= :make-vm-string> :make-vm-string>=
    :make-vm-stringp :make-vm-sub :make-vm-subseq :make-vm-subst
    :make-vm-symbol-name :make-vm-symbol-p :make-vm-sync-handler-regs
    :make-vm-terpri-inst :make-vm-third :make-vm-truncate :make-vm-type-of
    :make-vm-typep :make-vm-unread-char :make-vm-upper-case-p :make-vm-values
    :make-vm-values-to-list :make-vm-vector-push-extend :make-vm-vectorp
    :make-vm-warn :make-vm-write-char :make-vm-write-string
    :make-vm-write-to-string-inst :make-vm-write-to-string-rp

    ;; VM Heap Operations
    :vm-alloc
    :vm-cons
    :vm-car-reg
    :vm-cdr-reg
    :vm-car
    :vm-cdr
    :vm-rplaca
    :vm-rplacd
    :vm-cons-reg
    :vm-val-reg

    ;; VM Closure Heap Operations
    :vm-make-closure
    :vm-make-closure-params
    :vm-env-regs
    :vm-closure-ref-idx
    :vm-closure-reg
    :vm-closure-index

    ;; VM State and Utilities
    :vm-program
    :vm-program-instructions
    :vm-program-result-register
    :vm-state
    :vm-state-registers
    :vm-state-heap
    :vm-heap-counter
    :vm-call-stack
    :vm-return-stack
     :vm-closure-env
     :vm-heap-alloc
     :vm-heap-get
     :vm-heap-set
     :get-vm-heap
     :vm-reg-get
     :vm-reg-set
     :vm-closure-ref

    ;; VM Execution
    :execute-instruction

    ;; Instruction Conversion
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

     ;; VM Primitive Instructions - Type Predicates
     :vm-eq
     :vm-cons-p
     :vm-null-p
     :vm-symbol-p
     :vm-number-p
     :vm-integer-p
     :vm-function-p

     ;; VM Primitive Instructions - Comparisons
     :vm-lt
     :vm-gt
     :vm-le
     :vm-ge
     :vm-num-eq

     ;; VM Primitive Instructions - Arithmetic Extensions
     :vm-div
     :vm-mod
     :vm-neg
     :vm-abs
     :vm-inc
     :vm-dec

     ;; VM Primitive Instructions - Boolean Operations
     :vm-not
     :vm-and
     :vm-or

     ;; VM List Operations - Instruction Classes
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

     ;; VM List Operations - Accessors
     :vm-car-reg
     :vm-cdr-reg
     :vm-list-count
     :vm-src-regs
     :vm-item-reg
     :vm-list-reg
     :vm-index-reg
     :vm-cons-reg
     :vm-val-reg

     ;; VM I/O State and Operations
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

     ;; VM I/O Instruction Classes
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

     ;; VM I/O Instruction Accessors
     :vm-file-handle
     :vm-file-direction
     :vm-char-reg
     :vm-str-reg
     :vm-position-reg
     :vm-stream-direction
     :vm-initial-string

      ;; VM Reader/Printer - Reader State
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

      ;; VM Reader/Printer - Reader Functions
      :vm-read
      :vm-read-preserving-whitespace
      :vm-read-from-string
      :vm-read-delimited-list
      :vm-read-all-from-string
      :vm-read-expr

      ;; VM Reader/Printer - Printer Functions
      :vm-prin1
      :vm-princ
      :vm-print
      :vm-write-to-string
      :vm-prin1-to-string
      :vm-princ-to-string

      ;; VM Reader/Printer - Conditions
      :vm-reader-error
      :vm-reader-error-message
      :vm-reader-error-line
      :vm-reader-error-column

      ;; VM Reader/Printer - Instruction Classes
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

       ;; VM Reader/Printer - Heap Objects
       :vm-reader-state-object
       :vm-rso-stream
       :vm-rso-line
       :vm-rso-column
       :vm-rso-char
       :vm-rso-eof-reached

       ;; VM String Operations - Instruction Classes
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

       ;; VM String Operations - Instruction Accessors
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

       ;; VM Hash Table Operations - Heap Object
       :vm-hash-table-object
       :vm-hash-table-internal

       ;; VM Hash Table Operations - Instruction Classes
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

        ;; VM Hash Table Operations - Instruction Accessors
        :vm-hash-test
        :vm-found-dst
        :vm-hash-key
        :vm-hash-value
        :vm-hash-table-reg
        :vm-hash-default
        :vm-hash-fn

       ;; CLOS VM Instructions
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

       ;; CLOS Compiler Context
       :ctx-global-classes
       :ctx-global-generics

       ;; Backend Code Generation
       :compile-to-x86-64-bytes

       ;; Register Allocation
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
       :make-live-interval
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

       ;; Native Compilation
       :compile-to-native
       :compile-file-to-native
       :compile-toplevel-forms))

(in-package :cl-cc)
