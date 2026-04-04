(defpackage :cl-cc
  (:use :cl)
  (:export
    ;; CST (Concrete Syntax Tree) — modern parser layer
    :cst-node :cst-node-p :cst-node-kind :cst-node-start-byte :cst-node-end-byte :cst-node-source-file
    :cst-token :cst-token-p :cst-token-value :make-cst-token
    :cst-interior :cst-interior-p :cst-interior-children :make-cst-interior
    :cst-error-node :cst-error-node-p :cst-error-p :cst-error-node-message :make-cst-error-node
    :cst-child :cst-children :cst-walk :cst-collect-errors
    :cst-to-sexp :sexp-to-cst :sexp-head-to-kind
    :cst-location-string
    :cst-trivia :make-cst-trivia
    ;; Lexer
    :lexer-token :lexer-token-type :lexer-token-value
    :lexer-token-start-byte :lexer-token-end-byte
    :lexer-token-line :lexer-token-column :lexer-token-trivia
    :make-lexer :lex-all :lexer-read-token
    ;; CST parser
    :parse-cl-source :parse-cl-source-single
    ;; Diagnostics
    :diagnostic :make-diagnostic :diagnostic-severity :diagnostic-message :diagnostic-span
    :make-parse-error :make-parse-warning :parse-failure
    :byte-offset-to-line-col :source-line-at
    :format-diagnostic :format-diagnostic-list
    ;; Incremental parsing
    :edit-operation :make-edit-operation
    :parse-incremental :cst-equal-p
    :cache-lookup :cache-store :invalidate-parse-cache
    ;; DCG
    :def-dcg-rule :phrase :phrase-rest :phrase-all
    :dcg-fresh-var :dcg-reset-counter

    ;; CST → AST bridge
    :lower-cst-to-ast
    :lower-cst-list-to-ast
    :parse-and-lower
    :parse-and-lower-one

    ;; Core compiler
    :parse-source
    :parse-all-forms
    :cps-transform
    :cps-transform*
    :cps-transform-ast
    :cps-transform-ast*
    :cps-transform-sequence
    :cps-transform-eval
   :compile-expression
   :compile-string
   :emit-assembly
   :run-compiled
   :run-program-slice
   :run-string
   :run-string-repl
   :run-string-typed
   :reset-repl-state
   :*repl-vm-state*
   :optimize-instructions
   :%get-instructions
   :our-eval
   :our-load
   :type-check-ast
   :register-function-type
   :*function-type-registry*

   ;; Prolog system
   :logic-var-p
   :unify
   :unify-failed-p
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
   :make-prolog-rule
   :prolog-rule
   :prolog-cut
   :goal-predicate
   :goal-args
   :rule-head
   :rule-body
   :rename-variables
   :apply-prolog-peephole
   :*enable-prolog-peephole*
   ;; Prolog type-inference functors (atoms used as structured term tags in rules)
   :integer-type
   :boolean-type
   :const
   :binop
   :cmp

   ;; Macro System
   :macro-env
   :macro-env-table
   :parse-lambda-list
   :destructure-lambda-list
   :our-defmacro
   :our-macroexpand-1
   :our-macroexpand
   :our-macroexpand-all
   :register-macro
   :lookup-macro
   :*macro-environment*
   :*macro-eval-fn*

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
   :ast-children
   :ast-bound-names

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
   :make-ast-values
   :ast-values-forms
   :ast-multiple-value-bind
   :make-ast-multiple-value-bind
   :ast-mvb-vars
   :ast-mvb-values-form
   :ast-mvb-body
   :ast-apply
   :make-ast-apply
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
   :ast-slot-allocation
   :ast-defclass
   :ast-defclass-name
   :ast-defclass-superclasses
   :ast-defclass-slots
   :ast-defclass-default-initargs
   :ast-defgeneric
   :ast-defgeneric-name
   :ast-defgeneric-params
   :ast-defmethod
   :ast-defmethod-name
   :ast-defmethod-qualifier
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
    :compilation-result-type-env
    :compilation-result-cps
    :compilation-result-ast
    :compilation-result-vm-instructions
    :compilation-result-optimized-instructions

   ;; AST Transformation Utilities
   :lower-sexp-to-ast
   :ast-to-sexp

     ;; VM Heap Object Classes
     :vm-heap-object
     :vm-heap-address
     :make-vm-heap-address
     :vm-heap-address-value
     :vm-cons-cell
     :vm-cons-cell-car
     :vm-cons-cell-cdr
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
    :vm-integer-add
    :vm-integer-sub
    :vm-integer-mul
    :vm-float-add
    :vm-float-sub
    :vm-float-mul
    :vm-float-div
    :vm-sub
    :vm-mul
    :vm-label
    :vm-jump
    :vm-jump-zero
    :vm-print
    :vm-halt
    :vm-closure
    :vm-call
    :vm-tail-call
    :vm-ret
    :vm-func-ref
    :vm-push
    :vm-pop
    :vm-values
    :vm-spread-values
    :vm-mv-bind
    :vm-dst-regs
    :vm-apply
    :vm-register-function
    :vm-function-registry
    :vm-resolve-function
    :vm-register-host-bridge
    :*vm-host-bridge-functions*
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
    ;; Catch/throw
    :vm-establish-catch
    :vm-throw
    :vm-catch-tag-reg
    :vm-catch-handler-label
    :vm-catch-result-reg
    :vm-throw-tag-reg
    :vm-throw-value-reg
    :vm-handler-stack

    ;; VM Instruction Constructors (defstruct make-* functions)
    :make-vm-instruction
     :make-vm-abs :make-vm-acons :make-vm-add :make-vm-alpha-char-p
     :make-vm-integer-add :make-vm-integer-sub :make-vm-integer-mul
     :make-vm-float-add :make-vm-float-sub :make-vm-float-mul :make-vm-float-div
     :make-vm-alphanumericp :make-vm-and :make-vm-append :make-vm-apply
    :make-vm-adjust-array :make-vm-aref :make-vm-aref-multi :vm-aref-multi
    :make-vm-array-length :make-vm-aset :make-vm-assoc
    :make-vm-array-adjustable-p :make-vm-array-dimension
    :make-vm-array-dimensions :make-vm-array-displacement :make-vm-array-has-fill-pointer-p
    :make-vm-array-rank :make-vm-array-row-major-index :make-vm-array-total-size
    :make-vm-atom :make-vm-binop :make-vm-bit-access
    :make-vm-bit-and :make-vm-bit-not :make-vm-bit-or :make-vm-bit-set :make-vm-bit-xor
    :make-vm-butlast :make-vm-sbit
    :make-vm-call :make-vm-tail-call :make-vm-call-next-method :make-vm-car :make-vm-cdr :make-vm-ceiling-inst
    :make-vm-char :make-vm-char-code :make-vm-char-downcase
    :make-vm-char-not-greaterp :make-vm-char-not-lessp
    :make-vm-char-upcase :make-vm-char< :make-vm-char= :make-vm-characterp
    :make-vm-class-def :make-vm-close-file :make-vm-closure
    :make-vm-closure-ref-idx :make-vm-clrhash :make-vm-code-char
    :make-vm-coerce-to-list :make-vm-coerce-to-string :make-vm-coerce-to-vector
    :make-vm-clear-values
    :make-vm-concatenate :make-vm-cons :make-vm-cons-p :make-vm-const
    :make-vm-complex :make-vm-conjugate
    :make-vm-copy-list :make-vm-copy-tree :make-vm-dec :make-vm-denominator
    :make-vm-digit-char-p :make-vm-digit-char
    :make-vm-both-case-p :make-vm-graphic-char-p :make-vm-standard-char-p
    :make-vm-char-name :make-vm-name-char
    :make-vm-div :make-vm-cl-div :vm-cl-div :make-vm-endp :make-vm-eof-p :make-vm-eq
    :make-vm-equal :make-vm-establish-handler :make-vm-establish-catch
    :make-vm-ensure-values
    :make-vm-eval :make-vm-macroexpand-1-inst :make-vm-macroexpand-inst
    :vm-macroexpand-1-inst :vm-macroexpand-inst
    :make-vm-sxhash :vm-sxhash
    :make-vm-class-name-fn :make-vm-class-of-fn :make-vm-find-class
    :vm-class-name-fn :vm-class-of-fn :vm-find-class
    :make-vm-evenp :make-vm-fceiling :make-vm-ffloor
    :make-vm-fifth :make-vm-sixth :make-vm-seventh :make-vm-eighth :make-vm-ninth :make-vm-tenth
    :make-vm-file-length :make-vm-fill-pointer-inst
    :make-vm-file-position :make-vm-first :make-vm-floor-inst
    :make-vm-fmakunbound :make-vm-format-inst :make-vm-fourth
    :make-vm-fround :make-vm-fresh-line-inst :make-vm-ftruncate
     :make-vm-func-ref :make-vm-function-p :make-vm-ge :make-vm-generic-call
     :make-vm-select
    :make-vm-generic-add :make-vm-generic-sub :make-vm-generic-mul
    :make-vm-generic-div :make-vm-generic-eq :make-vm-generic-lt :make-vm-generic-gt
    :vm-generic-add :vm-generic-sub :vm-generic-mul
    :vm-generic-div :vm-generic-eq :vm-generic-lt :vm-generic-gt
    :make-vm-gensym-inst :make-vm-get-global
    :make-vm-get-output-stream-string-inst :make-vm-get-string-from-stream
    :make-vm-get-universal-time :make-vm-get-internal-real-time :make-vm-get-internal-run-time
    :make-vm-decode-universal-time :make-vm-encode-universal-time
    :make-vm-lisp-implementation-type :make-vm-lisp-implementation-version
    :make-vm-machine-type :make-vm-machine-version :make-vm-machine-instance
    :make-vm-software-type :make-vm-software-version
    :make-vm-short-site-name :make-vm-long-site-name
    :make-vm-gcd :make-vm-gethash :make-vm-gt :make-vm-halt
    :make-vm-hash-table-count
    :make-vm-hash-table-keys :make-vm-hash-table-p :make-vm-hash-table-test
    :make-vm-hash-table-values
    :make-vm-hash-table-size :make-vm-hash-table-rehash-size :make-vm-hash-table-rehash-threshold :make-vm-inc :make-vm-integer-p
    :make-vm-imagpart :make-vm-intern-symbol :make-vm-jump
    :make-vm-jump-zero :make-vm-keywordp :make-vm-label :make-vm-last
    :make-vm-lcm :make-vm-le :make-vm-length :make-vm-list-length
    :make-vm-listp :make-vm-lower-case-p :make-vm-lt :make-vm-make-array
    :make-vm-make-closure :make-vm-make-hash-table :make-vm-make-list
    :make-vm-make-obj
    :make-vm-make-string-output-stream-inst :make-vm-make-string-stream
    :make-vm-make-symbol :make-vm-max :make-vm-member
    :make-vm-min :make-vm-mod :make-vm-move :make-vm-mul :make-vm-mv-bind
    :make-vm-makunbound :make-vm-nconc :make-vm-nreconc :make-vm-nbutlast
    :make-vm-neg :make-vm-not
    :make-vm-nreverse :make-vm-nth :make-vm-numerator
    :make-vm-identity :make-vm-constantly :make-vm-complement
    :make-vm-next-method-p
    :make-vm-nthcdr :make-vm-null :make-vm-null-p :make-vm-num-eq
    :make-vm-number-p :make-vm-oddp :make-vm-open-file :make-vm-or
    :make-vm-parse-integer :make-vm-peek-char :make-vm-phase
    :make-vm-pop
    :make-vm-progv-enter :make-vm-progv-exit
    :make-vm-prin1 :make-vm-prin1-sexp :make-vm-princ :make-vm-princ-sexp
    :make-vm-print :make-vm-print-inst :make-vm-print-sexp :make-vm-push
    :make-vm-read-char :make-vm-read-from-string-inst
    :make-vm-read-from-string-rp :make-vm-read-line :make-vm-read-sexp
    :make-vm-read-sexp-inst :make-vm-reader-advance-rp :make-vm-reader-peek-rp
    :make-vm-reader-read :make-vm-register-function :make-vm-register-method
    :make-vm-rational :make-vm-rationalize :make-vm-realpart
    :make-vm-random :make-vm-make-random-state
    :make-vm-rem :make-vm-remhash :make-vm-remove-handler :make-vm-remprop
    :make-vm-rest :make-vm-row-major-aref
    :make-vm-ret :make-vm-reverse :make-vm-rplaca :make-vm-rplacd
    :make-vm-search-string :make-vm-second :make-vm-set-global :make-vm-sethash
    :make-vm-signal-error :make-vm-slot-read :make-vm-slot-write
    :make-vm-spill-load :make-vm-spill-store
    :make-vm-stream-write-string-inst :make-vm-string-capitalize
    :make-vm-string-coerce :make-vm-string-downcase :make-vm-string-equal
    :make-vm-string-greaterp :make-vm-string-left-trim :make-vm-string-length
    :make-vm-string-lessp :make-vm-string-not-equal :make-vm-string-right-trim
    :make-vm-string-trim :make-vm-string-upcase :make-vm-string-downcase :make-vm-string-capitalize
    :make-vm-nstring-upcase :make-vm-nstring-downcase :make-vm-nstring-capitalize
    :make-vm-string<
    :make-vm-string<= :make-vm-string= :make-vm-string> :make-vm-string>=
    :make-vm-stringp :make-vm-string-set :make-vm-sub :make-vm-subseq :make-vm-subst
    :make-vm-set-fill-pointer :make-vm-set-symbol-plist
    :make-vm-slot-boundp :make-vm-slot-exists-p :make-vm-slot-makunbound
    :make-vm-svref :make-vm-svset
    :make-vm-symbol-get :make-vm-symbol-name
    :make-vm-symbol-p :make-vm-symbol-plist :make-vm-symbol-set
    :make-vm-sync-handler-regs
    :make-vm-terpri-inst :make-vm-third :make-vm-throw :make-vm-truncate :make-vm-type-of
    :make-vm-typep :make-vm-unread-char :make-vm-upper-case-p :make-vm-values
    :make-vm-spread-values :make-vm-values-to-list :make-vm-vector-pop :make-vm-vector-push
    :make-vm-vector-push-extend :make-vm-vectorp :make-vm-simple-vector-p
    :make-vm-warn :make-vm-write-byte :make-vm-write-char :make-vm-write-line
    :make-vm-write-string :make-vm-write-to-string-inst :make-vm-write-to-string-rp
    :make-vm-princ-to-string-inst :vm-princ-to-string-inst
    ;; Stream predicates + control
    :make-vm-streamp :make-vm-input-stream-p :make-vm-output-stream-p
    :make-vm-open-stream-p :make-vm-interactive-stream-p :make-vm-stream-element-type-inst
    :make-vm-read-byte :make-vm-listen-inst
    :make-vm-force-output :make-vm-finish-output :make-vm-clear-input :make-vm-clear-output
    :make-vm-load-file

    ;; VM Heap Operations
    :vm-cons
    :vm-car
    :vm-cdr
    :vm-rplaca
    :vm-rplacd

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
    :vm-method-call-stack
     :vm-closure-env
     :vm-heap-alloc
     :vm-heap-get
     :vm-heap-set
     :vm-reg-get
     :vm-reg-set
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
     :vm-select
     :vm-select-cond-reg
     :vm-select-then-reg
     :vm-select-else-reg

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
     :vm-sixth :vm-seventh :vm-eighth :vm-ninth :vm-tenth
     :vm-rest
     :vm-last
     :vm-butlast :vm-nbutlast
     :vm-nreconc
     :vm-identity :vm-constantly :vm-complement
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

      ;; VM Reader/Printer - Instruction Classes
      :vm-read-sexp
      :vm-print-sexp
      :vm-prin1-sexp
      :vm-princ-sexp
      :vm-make-string-output-stream-inst
      :vm-get-output-stream-string-inst
      :vm-stream-write-string-inst
      :vm-read-from-string-inst
      :vm-read-sexp-inst
      ;; VM Primitives - recently added
      :vm-eval
      :vm-type-of
      :vm-make-list
      :vm-alphanumericp
      :vm-reader-read

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
       :vm-nstring-upcase
       :vm-nstring-downcase
       :vm-nstring-capitalize
       :vm-string-trim
       :vm-string-left-trim
       :vm-string-right-trim
       :vm-search-string
       :vm-string-set

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
       :vm-hash-table-keys
       :vm-hash-table-values
       :vm-hash-table-test
       :vm-hash-table-size
       :vm-hash-table-rehash-size
       :vm-hash-table-rehash-threshold

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
       :vm-class-slots
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
       :vm-method-qualifier
       :vm-method-reg
       :vm-generic-call

       ;; CLOS Compiler Context
       :ctx-global-classes
       :ctx-global-generics

       ;; Backend Code Generation
       :compile-to-x86-64-bytes
       :compile-to-aarch64-bytes

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
       :compile-toplevel-forms

       ;; Parser Combinator Engine
       :*grammar-rules*
       :def-grammar-rule
       :query-grammar
       :clear-grammar-rules
       :parse-combinator
       :parse-ok-p
       :parse-with-grammar

       ;; PHP Frontend
       :tokenize-php-source
       :parse-php-source
       :parse-source-for-language

       ;; AST Predicates (defstruct-generated, needed by test suite)
       :ast-int-p
       :ast-print-p
       :ast-let-p
       :ast-setq-p
       :ast-if-p
       :ast-defun-p
       :ast-quote-p
       :ast-defclass-p
       :ast-call-p
       :ast-binop-p

       ;; ── Compile-Level IR Foundation (src/compile/ir/) ────────────────────
       ;; IR Value (SSA virtual register)
       :ir-value :make-ir-value :ir-value-p
       :irv-id :irv-type :irv-def
       ;; IR Instruction (base)
       :ir-inst :make-ir-inst :ir-inst-p
       :iri-result :iri-block
       :ir-operands
       ;; IR Basic Block
       :ir-block :make-ir-block :ir-block-p
       :irb-id :irb-label :irb-params :irb-insts :irb-terminator
       :irb-predecessors :irb-successors :irb-sealed-p :irb-incomplete-phis
       ;; IR Function
       :ir-function :make-ir-function :ir-function-p
       :irf-name :irf-params :irf-entry :irf-blocks :irf-return-type
       :irf-value-counter :irf-block-counter :irf-current-defs
       ;; IR Module
       :ir-module :make-ir-module :ir-module-p
       :irm-functions :irm-globals
       ;; Allocators and builder
       :ir-new-value :ir-new-block :ir-make-function
       ;; CFG utilities
       :ir-add-edge :ir-emit :ir-set-terminator
       :ir-rpo :ir-dominators
       :ir-collect-uses :ir-verify-ssa
       ;; SSA variable tracking (Braun et al. 2013)
       :ir-write-var :ir-read-var :ir-seal-block
       ;; Printer
       :ir-format-value :ir-print-inst :ir-print-block
       :ir-print-function :ir-function-to-string :ir-print-module

       ;; ── MIR: Target-Neutral SSA Intermediate Representation ──────────────
       ;; MIR Value (SSA virtual register)
       :mir-value :make-mir-value :mir-value-p
       :mirv-id :mirv-name :mirv-type :mirv-def-inst :mirv-use-count
       ;; MIR Constant
       :mir-const :make-mir-const :mir-const-p
       :mirc-value :mirc-type
       ;; MIR Instruction
       :mir-inst :make-mir-inst :mir-inst-p
       :miri-op :miri-dst :miri-srcs :miri-type :miri-block :miri-meta
       ;; MIR Basic Block
       :mir-block :make-mir-block :mir-block-p
       :mirb-id :mirb-label :mirb-insts :mirb-preds :mirb-succs
       :mirb-sealed-p :mirb-phis :mirb-incomplete-phis
       ;; MIR Function
       :mir-function :make-mir-function :mir-function-p
       :mirf-name :mirf-params :mirf-blocks :mirf-entry
       :mirf-current-defs :mirf-value-counter :mirf-block-counter
       ;; MIR Module
       :mir-module :make-mir-module :mir-module-p
       :mirm-functions :mirm-globals :mirm-string-table
       ;; Generic op vocabulary
       :*mir-generic-ops*
       ;; Builder API
       :mir-new-value :mir-new-block :mir-make-function
       :mir-emit :mir-add-pred :mir-add-succ
       :mir-write-var :mir-read-var :mir-seal-block
       ;; CFG utilities
       :mir-rpo :mir-dominators
       ;; Printer
       :mir-format-value :mir-print-inst :mir-print-block :mir-print-function

       ;; ── Target Descriptors ───────────────────────────────────────────────
       :target-desc :make-target-desc :target-desc-p
       :target-name :target-word-size :target-endianness
       :target-gpr-count :target-gpr-names
       :target-arg-regs :target-ret-reg
       :target-callee-saved :target-scratch-regs
       :target-stack-alignment :target-legal-ops :target-features
       ;; Predefined targets
       :*x86-64-target* :*aarch64-target* :*riscv64-target* :*wasm32-target*
       ;; Target registry
       :*target-registry* :register-target :find-target
       ;; Target predicates and utilities
       :target-64-bit-p :target-has-feature-p
       :target-allocatable-regs :target-caller-saved
       :target-reg-index :target-op-legal-p :target-op-expand

       ;; ── Optimizer Phase 0: Effect-Kind System ────────────────────────────
       :vm-inst-effect-kind
       :opt-inst-pure-p
       :opt-inst-dce-eligible-p
       :opt-inst-cse-eligible-p
       :effect-row->effect-kind

       ;; ── Optimizer Phase 1: CFG + Dominator Tree + DF ─────────────────────
       ;; basic-block struct
       :basic-block :make-basic-block :basic-block-p
       :bb-id :bb-label :bb-instructions :bb-predecessors :bb-successors
       :bb-idom :bb-dom-children :bb-dom-frontier :bb-loop-depth :bb-rpo-index
       ;; cfg struct
       :cfg :make-cfg :cfg-p
       :cfg-blocks :cfg-entry :cfg-exit :cfg-label->block :cfg-next-id
       ;; CFG builder and algorithms
        :cfg-build :cfg-block-count :cfg-get-block-by-label
        :cfg-compute-rpo :cfg-compute-dominators :cfg-compute-dominance-frontiers
        :cfg-dominates-p :cfg-idf :cfg-flatten :cfg-split-critical-edges

       ;; ── Optimizer Phase 1: SSA Construction + Destruction ────────────────
       ;; ssa-rename-state struct
       :ssa-rename-state :make-ssa-rename-state :ssa-rename-state-p
       :ssr-counters :ssr-stacks
       :ssr-push-new-version :ssr-current-version :ssr-pop-version
       ;; ssa-phi struct
       :ssa-phi :make-ssa-phi :ssa-phi-p
       :phi-dst :phi-args :phi-reg
        ;; SSA algorithms
        :ssa-versioned-reg :ssa-construct :ssa-destroy :ssa-round-trip
        :ssa-place-phis :ssa-rename :ssa-sequentialize-copies
        :ssa-eliminate-trivial-phis

       ;; ── Optimizer Phase 2: E-Graph Engine ────────────────────────────────
       ;; e-node struct
       :e-node :make-e-node :e-node-p
       :en-op :en-children :en-eclass
       ;; e-class struct
       :e-class :make-e-class :e-class-p
       :ec-id :ec-nodes :ec-parents :ec-data
       ;; e-graph struct
       :e-graph :make-e-graph :e-graph-p
       :eg-classes :eg-memo :eg-union-find :eg-worklist :eg-next-id
        ;; E-graph operations
        :egraph-find :egraph-add :egraph-merge :egraph-rebuild
        :egraph-saturate :egraph-extract :egraph-default-cost
        :egraph-stats :egraph-pattern-var-p :egraph-match-pattern
        :optimize-with-egraph

       ;; ── Optimizer Phase 2: E-Graph Rules ─────────────────────────────────
       :defrule :egraph-rule-register :egraph-builtin-rules
       :optimize-with-egraph))

(in-package :cl-cc)
