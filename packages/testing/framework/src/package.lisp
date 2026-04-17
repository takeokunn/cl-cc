(defpackage :cl-cc/test
  (:use :cl)
  (:import-from :cl-cc/compile
                 ;; Compiler — moved from :cl-cc to :cl-cc/compile (Phase 2)
                 :compile-string
                 :run-string
                 :run-string-repl
                 :reset-repl-state
                 :run-string-typed
                 :compile-expression
                 ;; CPS
                 :cps-transform
                 :cps-transform-ast
                 :cps-transform-ast*
                 :cps-transform-eval
                 ;; Eval
                 :our-eval
                 ;; Compilation result
                 :compilation-result
                 :make-compilation-result
                 :compilation-result-program
                 :compilation-result-assembly
                 :compilation-result-globals
                 :compilation-result-type
                 :compilation-result-cps)
  (:import-from :cl-cc/parse
                 ;; Parse — moved from :cl-cc to :cl-cc/parse (Phase 2)
                 :ast-to-sexp
                 :lower-sexp-to-ast
                 :parse-slot-spec
                 :slot-def-to-sexp
                 ;; Grammar combinator engine
                 :*grammar-rules*
                 :def-grammar-rule
                 :query-grammar
                 :clear-grammar-rules
                 :parse-combinator
                 :parse-ok-p
                 :parse-with-grammar
                 ;; PHP frontend
                 :tokenize-php-source
                 :parse-php-source)
  (:import-from :cl-cc
                 :compile-ast
                 ;; VM Program
                 :vm-program
                 :vm-program-instructions
                 :vm-program-result-register
                 ;; Macro System — our-macroexpand-1/our-macroexpand moved to
                 ;; :import-from :cl-cc/expand below
                 ;; ast-to-sexp, lower-sexp-to-ast moved to :import-from :cl-cc/parse above
                 :ast-node
                 :ast-int
                 :ast-var
                 :ast-binop
                 :ast-if
                 :ast-progn
                 :ast-progn-forms
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
                 ;; AST constructors (defstruct BOA)
                 :make-ast-int
                 :make-ast-var
                 :make-ast-binop
                 :make-ast-if
                 :make-ast-progn
                 :make-ast-print
                 :make-ast-let
                 :make-ast-lambda
                 :make-ast-function
                 :make-ast-flet
                 :make-ast-labels
                 :make-ast-block
                 :make-ast-return-from
                 :make-ast-tagbody
                 :make-ast-go
                 :make-ast-setq
                 :make-ast-call
                 :make-ast-quote
                 :make-ast-catch
                 :make-ast-throw
                 :make-ast-unwind-protect
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
                 ;; Additional VM instruction types used as hash keys in fold
                 ;; tables and as defstruct type tags in emitter tests.
                 :vm-lcm :vm-gcd :vm-ash :vm-rotate :vm-bswap
                 :vm-logand :vm-logior :vm-logxor :vm-logeqv
                 :vm-logtest :vm-logbitp :vm-logcount :vm-integer-length
                 :vm-min :vm-max :vm-rem :vm-mod :vm-div :vm-truncate
                 :vm-floor-inst :vm-ceiling-inst :vm-round-inst
                 :vm-lognot :vm-rational :vm-rationalize :vm-numerator :vm-denominator
                 :vm-make-array
                 :vm-eq :vm-num-eq :vm-lt :vm-gt :vm-le :vm-ge
                 :vm-and :vm-or :vm-abs :vm-inc :vm-dec :vm-not
                 :vm-neg :vm-null-p
                 :vm-concatenate :vm-select
                 :vm-cons-p :vm-symbol-p :vm-number-p :vm-integer-p :vm-function-p
                 :vm-integer-add :vm-integer-sub :vm-integer-mul
                 :vm-float-add :vm-float-sub :vm-float-mul :vm-float-div
                 ;; VM Instruction constructors (defstruct)
                 :make-vm-add :make-vm-call :make-vm-car :make-vm-cdr
                 :make-vm-closure-ref-idx :make-vm-cons :make-vm-const
                 :make-vm-halt :make-vm-jump-zero :make-vm-label
                 :make-vm-make-closure :make-vm-move :make-vm-mul
                 :make-vm-rplaca :make-vm-rplacd :make-vm-sub
                 :make-vm-closure :make-vm-jump :make-vm-ret :make-vm-func-ref
                 :make-vm-print :make-vm-spill-load :make-vm-spill-store
                  ;; Additional constructors for effects/optimizer tests
                  :make-vm-neg :make-vm-inc :make-vm-dec
                  :make-vm-lt :make-vm-gt :make-vm-le :make-vm-ge
                  :make-vm-null-p :make-vm-cons-p :make-vm-number-p
                  :make-vm-set-global :make-vm-get-global
                  :make-vm-not :make-vm-lognot
                  :make-vm-ash :make-vm-logand :make-vm-logior
                  :make-vm-logxor :make-vm-logeqv :make-vm-logtest
                  :make-vm-logbitp :make-vm-logcount :make-vm-integer-length
                  :make-vm-bswap
                  :make-vm-format-inst :make-vm-make-string
                  ;; Additional VM constructors required by emitter/codegen tests
                  :make-vm-abs :make-vm-and :make-vm-or
                  :make-vm-eq :make-vm-num-eq
                  :make-vm-div :make-vm-mod :make-vm-rem :make-vm-truncate
                  :make-vm-min :make-vm-max :make-vm-rotate
                  :make-vm-concatenate :make-vm-make-array
                  :make-vm-float-add :make-vm-integer-add :make-vm-integer-mul
                  :make-vm-register-function :make-vm-select
                 ;; VM Heap operations
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
                 :execute-instruction
                :instruction->sexp
                 :sexp->instruction
                 ;; Prolog
                 :unify
                 :substitute-variables
                 :logic-var-p
                 ;; our-macroexpand-all moved to :import-from :cl-cc/expand below
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
                 ;; parse-slot-spec, slot-def-to-sexp moved to :import-from :cl-cc/parse above
                 ;; Register Allocation — moved to :import-from :cl-cc/emit below
                 ;; our-eval, compilation-result, make-compilation-result,
                 ;; compilation-result-program/-assembly/-globals/-type/-cps
                 ;; moved to :import-from :cl-cc/compile above
                 ;; Parser Combinator Engine + PHP Frontend moved to :import-from :cl-cc/parse above
                 ;; AST predicates needed for PHP tests
                 :ast-int-p
                 :ast-int-value
                 :ast-print-p
                 :ast-print-expr
                 :ast-let-p
                 :ast-setq-p
                 :ast-if-p
                 :ast-defun-p
                 :ast-defun-name
                 :ast-quote-p
                 :ast-quote-value
                 :ast-defclass-p
                 :ast-defclass-name
                 :ast-call-p
                 :ast-binop-p
                 ;; MIR + target descriptors moved to :shadowing-import-from :cl-cc/mir below
                 ;; Prolog type-inference functor atoms
                 :integer-type :boolean-type :const :binop :cmp)
  (:import-from :cl-cc/mir
                 ;; MIR — SSA intermediate representation
                 :mir-value :make-mir-value :mir-value-p
                 :mirv-id :mirv-name :mirv-type :mirv-def-inst :mirv-use-count
                 :mir-const :make-mir-const :mir-const-p
                 :mirc-value :mirc-type
                 :mir-inst :make-mir-inst :mir-inst-p
                 :miri-op :miri-dst :miri-srcs :miri-type :miri-block :miri-meta
                 :mir-block :make-mir-block :mir-block-p
                 :mirb-id :mirb-label :mirb-insts :mirb-preds :mirb-succs
                 :mirb-sealed-p :mirb-phis :mirb-incomplete-phis
                 :mir-function :make-mir-function :mir-function-p
                 :mirf-name :mirf-params :mirf-blocks :mirf-entry
                 :mirf-current-defs :mirf-value-counter :mirf-block-counter
                 :mir-module :make-mir-module :mir-module-p
                 :mirm-functions :mirm-globals :mirm-string-table
                 :*mir-generic-ops*
                 :mir-new-value :mir-new-block :mir-make-function
                 :mir-emit :mir-add-pred :mir-add-succ
                 :mir-write-var :mir-read-var :mir-seal-block
                 :mir-rpo :mir-dominators
                 :mir-format-value :mir-print-inst :mir-print-block :mir-print-function
                 ;; Target descriptors
                 :target-desc :make-target-desc :target-desc-p
                 :target-name :target-word-size :target-endianness
                 :target-gpr-count :target-gpr-names
                 :target-arg-regs :target-ret-reg
                 :target-callee-saved :target-scratch-regs
                 :target-stack-alignment :target-legal-ops :target-features
                 :*x86-64-target* :*aarch64-target* :*riscv64-target* :*wasm32-target*
                 :*target-registry* :register-target :find-target
                 :target-64-bit-p :target-has-feature-p
                 :target-allocatable-regs :target-caller-saved
                 :target-reg-index :target-op-legal-p :target-op-expand)
  (:import-from :cl-cc/optimize
                 ;; Optimizer — top-level entry point
                 :optimize-instructions)
  (:import-from :cl-cc/emit
                 ;; Register Allocation
                 :calling-convention
                 :cc-gpr-pool
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
                 :regalloc-spill-count
                 :regalloc-lookup
                 :instruction-defs
                 :instruction-uses
                 :compute-live-intervals
                 :allocate-registers
                 :vm-spill-store
                 :vm-spill-load
                 ;; Native code generation
                 :compile-to-x86-64-bytes
                 :compile-to-aarch64-bytes
                 :compile-to-native
                 :compile-file-to-native)
  (:import-from :cl-cc/expand
                 ;; Macro System
                 :our-macroexpand-1
                 :our-macroexpand
                 :our-macroexpand-all)
  (:shadowing-import-from :cl-cc/type
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
                 :type-float
                 :type-string
                 :type-bool
                 :type-symbol
                 :type-cons
                 :type-null
                 :type-any
                 :type-char
                 :type-unit
                 :+type-unknown+
                 ;; Type constructors
                  :make-type-primitive
                  :make-type-variable
                  :make-type-function-raw
                  :make-type-scheme
                  :type-constructor
                  :type-constructor-p
                  :type-constructor-name
                  :type-constructor-args
                  :make-type-constructor
                  :make-type-constructor-raw
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
                 :type-mismatch-error
                 ;; Type constructors (convenience wrappers)
                 :make-type-function
                 ;; Phase 3: Bidirectional type checking
                 :synthesize
                 :check
                 :check-body
                 ;; Phase 4: Typeclass support
                 :make-type-class
                 :type-class-p
                 :type-class-name
                 :register-typeclass
                 :lookup-typeclass
                 :make-type-class-constraint
                 :type-class-constraint-p
                 :type-class-constraint-class-name
                 :type-class-constraint-type-arg
                 :make-type-qualified
                 :type-qualified-p
                 :type-qualified-constraints
                 :type-qualified-type
                 :register-typeclass-instance
                 :has-typeclass-instance-p
                 ;; Phase 5: Effect types
                 :make-type-effect
                 :type-effect-p
                 :type-effect-name
                 :make-type-effect-row
                 :type-effect-row-p
                 :type-effect-row-effects
                 :type-effect-row-row-var
                 :+pure-effect-row+
                 :+io-effect-row+
                 :make-type-effectful-function
                 :type-effectful-function
                 :type-effectful-function-effects
                  ;; Phase 6: Rank-N polymorphism
                  :make-type-forall
                  :type-forall-p
                  :type-forall-var
                  :type-forall-type
                  :type-forall-body
                  ;; Type-lambda
                  :make-type-lambda
                  :type-lambda-p
                  :type-lambda-var
                  :type-lambda-knd
                  :type-lambda-body
                  ;; Kind system (additional)
                 :kind-node-p
                 :fresh-kind-var
                 :kind-var-p
                 :kind-var-equal-p
                 :kind-type-p
                 :make-kind-type
                 :kind-arrow-p
                 :kind-arrow-from
                 :kind-arrow-to
                 :kind-fun
                 :kind-effect-p
                 :kind-row-p
                 :kind-row-elem
                 :kind-equal-p
                 :kind-to-string
                 ;; Kind singletons (constants)
                 :+kind-type+
                 :+kind-effect+
                 :+kind-constraint+
                 :+kind-multiplicity+
                 :+kind-row-type+
                 :+kind-row-effect+
                 ;; Multiplicity
                 :+mult-zero+
                 :+mult-one+
                 :+mult-omega+
                 :multiplicity-p
                 :mult-add
                 :mult-mul
                 :mult-leq
                 :mult-to-string
                 ;; Type-arrow (full API)
                 :make-type-arrow
                 :make-type-arrow-raw
                 :type-arrow-p
                 :type-arrow-params
                 :type-arrow-return
                 :type-arrow-effects
                 :type-arrow-mult
                 ;; Type-app (HKT application)
                 :make-type-app
                 :type-app-p
                 :type-app-fun
                 :type-app-arg
                 ;; Type-error sentinel
                 :make-type-error
                 :type-error-p
                 ;; Type-exists
                 :make-type-exists
                 :type-exists-p
                 :type-exists-var
                 :type-exists-body
                 ;; Type-linear
                 :make-type-linear
                 :type-linear-p
                 :type-linear-base
                 :type-linear-grade
                 ;; Type-mu (recursive)
                 :make-type-mu
                 :type-mu-p
                 :type-mu-var
                 :type-mu-body
                 ;; Type-product (tuple)
                 :make-type-product
                 :type-product-p
                 :type-product-elems
                 ;; Type-record
                 :make-type-record
                 :type-record-p
                 :type-record-fields
                 :type-record-row-var
                 ;; Type-union
                 :make-type-union
                 :type-union-p
                 :type-union-types
                 ;; Type-intersection
                 :make-type-intersection
                 :type-intersection-p
                 :type-intersection-types
                 ;; Type-unknown (struct constructor)
                 :make-type-unknown
                 ;; Type-variant
                 :make-type-variant
                 :type-variant-p
                 :type-variant-cases
                 :type-variant-row-var
                 ;; Type-refinement
                 :type-refinement-p
                 :type-refinement-base
                 :type-refinement-predicate
                 ;; Type-rigid (skolem)
                 :fresh-rigid-var
                 :type-rigid-p
                 :type-rigid-name
                 :type-rigid-equal-p
                  ;; Type-unknown
                  :type-unknown-p
                  ;; ANSI upgrade helpers
                  :upgraded-array-element-type
                  :upgraded-complex-part-type
                  ;; Type-primitive
                  :type-primitive-p
                 ;; Old type-var API (aliases)
                 :type-var-p
                 :type-var-name
                 :type-var-equal-p
                 ;; Substitution (full API)
                 :make-substitution
                 :substitution-p
                 :substitution-generation
                 :subst-extend
                 :subst-extend!
                 :subst-compose
                 :zonk
                 ;; Typeclass (extended API)
                  :make-typeclass-def
                  :typeclass-def-p
                  :typeclass-def-name
                  :typeclass-def-type-params
                  :typeclass-def-methods
                  :typeclass-instance-p
                  :typeclass-instance-class-name
                  :typeclass-instance-methods
                  :lookup-typeclass-instance
                 ;; Row polymorphism
                 :row-extend
                 :row-restrict
                 :row-select
                 :row-labels
                 :row-closed-p
                 :row-open-p
                 ;; Effect op
                 :make-type-effect-op
                 ;; Constraint language
                 :constraint :constraint-p :constraint-kind :constraint-args
                 :make-equal-constraint :make-subtype-constraint
                 :make-typeclass-constraint :make-implication-constraint
                 :make-effect-subset-constraint :make-mult-leq-constraint
                 :make-row-lacks-constraint :make-kind-equal-constraint
                 :make-constraint
                 ;; Constraint solver
                 :solve-constraints :collect-constraints)
  (:export :run-tests
           :cl-cc-suite
           :cl-cc-unit-suite
           :cl-cc-integration-suite
           :cl-cc-e2e-suite
           :run-suite
           :deftest
           :defsuite
           :in-suite
           :assert-=
           :assert-eq
           :assert-eql
           :assert-equal
           :assert-string=
           :assert-null
           :assert-true
           :assert-false
           :assert-type
           :assert-signals
           :assert-values
           :assert-type-equal
           :assert-unifies
           :assert-not-unifies
           :assert-snapshot
           :assert-compiles-to
           :assert-evaluates-to
           :assert-macro-expands-to
           :assert-infers-type
           :deftest-each
           :testing
           :deftest-combinatorial
           :deftest-pipeline
           :defbefore
           :defafter
           :skip
           :pending
           :deftest-fuzz
           :assert-no-crash
           :assert-terminates
           :defmetamorphic
           :definvariant
           :run-mutation-test))
