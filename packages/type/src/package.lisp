;;;; package.lisp - cl-cc/type Package Definition
;;;;
;;;; Exports the complete 2026 type system API.

(defpackage :cl-cc/type
  (:use :cl)
  ;; Shadow cl:type-error so we can define our own type-error struct
  (:shadow #:type-error #:subtypep
           #:upgraded-array-element-type
           #:upgraded-complex-part-type)
  (:export

   ;; ─── Kind system ─────────────────────────────────────────────────────
   #:kind-node  #:kind-node-p
   #:kind-type   #:kind-type-p   #:make-kind-type   #:+kind-type+
   #:kind-arrow  #:kind-arrow-p  #:make-kind-arrow  #:kind-arrow-from #:kind-arrow-to #:kind-fun
   #:kind-effect #:kind-effect-p #:make-kind-effect #:+kind-effect+
   #:kind-row    #:kind-row-p    #:make-kind-row    #:kind-row-elem
   #:+kind-row-type+ #:+kind-row-effect+
   #:kind-constraint   #:kind-constraint-p   #:make-kind-constraint   #:+kind-constraint+
   #:kind-multiplicity #:kind-multiplicity-p #:make-kind-multiplicity #:+kind-multiplicity+
   #:kind-var  #:kind-var-p  #:fresh-kind-var  #:kind-var-id #:kind-var-name
   #:kind-var-equal-p  #:kind-equal-p  #:kind-to-string

   ;; ─── Multiplicity ─────────────────────────────────────────────────────
   #:multiplicity  #:+mult-zero+ #:+mult-one+ #:+mult-omega+
   #:multiplicity-p  #:mult-add #:mult-mul #:mult-leq  #:mult-to-string

   ;; ─── Concrete advanced semantics ──────────────────────────────────────
   #:concurrency-traits #:concurrency-traits-p #:make-concurrency-traits
   #:concurrency-traits-type #:concurrency-traits-send #:concurrency-traits-sync
   #:concurrency-traits-note #:*concurrency-trait-registry*
   #:register-concurrency-traits #:lookup-concurrency-traits
   #:sendable-type-p #:shareable-type-p #:validate-send #:validate-sync
   #:validate-spawn-argument #:validate-shared-reference

   #:+security-label-order+ #:normalize-security-label #:security-label-rank
   #:security-label-p #:security-label<= #:join-security-labels #:meet-security-labels
   #:labeled-value #:labeled-value-p #:labeled-value-value #:labeled-value-label
   #:labeled-value-tainted-p #:labeled-value-audit-trail
   #:make-labeled-value #:labeled-value-flow-allowed-p
   #:sanitize-labeled-value #:declassify-labeled-value

   #:region-lifetime-error #:region-token #:region-token-p #:region-token-id
   #:region-token-generation #:region-token-active-p #:make-region-token #:close-region
   #:region-active-p #:region-ref #:region-ref-p #:region-ref-token
   #:region-ref-generation #:region-ref-value #:region-alloc #:region-ref-valid-p
   #:region-deref #:with-region

   #:capability #:capability-p #:capability-permissions #:make-capability
   #:capability-allows-p #:capability-implies-p #:restrict-capability
   #:delegate-capability #:capability-effects

   #:unit-mismatch-error #:unit-definition #:unit-definition-p #:unit-definition-name
   #:unit-definition-dimension #:unit-definition-scale #:*unit-registry*
   #:define-unit #:find-unit #:unit-designator-p #:unit-dimension= #:unit-compatible-p
   #:measure #:measure-p #:measure-value #:measure-unit #:make-measure
   #:convert-unit #:convert-measure #:measure+ #:measure- #:measure* #:measure/

   #:route-validation-error #:route #:route-p #:route-method #:route-path
   #:route-parameters #:route-request-type #:route-response-type #:make-route
   #:route-valid-p #:route-template-parameters #:build-route-path #:match-route-path
   #:api-spec #:api-spec-p #:api-spec-routes #:api-spec-valid-p #:route-form-valid-p

   #:ffi-validation-error #:+ffi-scalar-kinds+
   #:ffi-scalar-type #:ffi-scalar-type-p #:ffi-scalar-type-kind #:make-ffi-scalar-type
   #:ffi-pointer-type #:ffi-pointer-type-p #:ffi-pointer-type-pointee
   #:ffi-pointer-type-borrowed-p #:ffi-pointer-type-nullable-p #:make-ffi-pointer-type
   #:ffi-callback-type #:ffi-callback-type-p #:ffi-callback-type-argument-types
   #:ffi-callback-type-return-type #:make-ffi-callback-type
   #:ffi-function-descriptor #:ffi-function-descriptor-p #:ffi-function-descriptor-name
   #:ffi-function-descriptor-argument-types #:ffi-function-descriptor-return-type
   #:ffi-function-descriptor-abi #:make-ffi-function-descriptor
   #:ffi-type-valid-p #:ffi-lisp-type-compatible-p #:ffi-descriptor-form-valid-p

   #:qtt-binding #:qtt-binding-p #:qtt-binding-name #:qtt-binding-type
   #:qtt-binding-multiplicity #:make-qtt-binding #:normalize-multiplicity
   #:valid-multiplicity-p #:multiplicity<= #:multiplicity+ #:multiplicity*
   #:multiplicity-zero-p #:multiplicity-one-p #:multiplicity-unrestricted-p
   #:usage-satisfies-multiplicity-p #:qtt-erased-p

   #:finite-semiring #:finite-semiring-p #:finite-semiring-name #:finite-semiring-elements
   #:finite-semiring-zero #:finite-semiring-one #:finite-semiring-add
   #:finite-semiring-multiply #:finite-semiring-preorder #:make-finite-semiring
   #:grade-designator-p #:finite-semiring-valid-p #:graded-value #:graded-value-p
   #:graded-value-grade #:graded-value-payload #:graded-value-semiring
   #:make-graded-value #:grade<= #:graded-add #:graded-compose #:make-qtt-semiring

   #:universe-sort #:universe-sort-p #:universe-sort-kind #:universe-sort-level
   #:make-universe-sort #:valid-universe-sort-p #:universe<= #:max-universe
   #:cic-proposition #:cic-proposition-p #:cic-proposition-name
   #:cic-proposition-universe #:cic-proposition-payload #:make-cic-proposition
   #:cic-proof #:cic-proof-p #:cic-proof-proposition #:cic-proof-witness
   #:cic-proof-premises #:make-cic-proof #:cic-proof-valid-p #:proof-erasable-p
   #:cic-inductive #:cic-inductive-p #:cic-inductive-name #:cic-inductive-universe
   #:cic-inductive-constructors #:make-cic-inductive #:cic-inductive-valid-p
   #:cic-large-elimination-allowed-p

   #:termination-evidence #:termination-evidence-p #:termination-evidence-strategy
   #:termination-evidence-measures #:termination-evidence-partial-p
   #:make-termination-evidence #:structural-decrease-p #:lexicographic-decrease-p
   #:termination-evidence-valid-p #:termination-evidence-form-valid-p

   #:proof-obligation #:proof-obligation-p #:proof-obligation-name
   #:proof-obligation-checker #:proof-obligation-description #:make-proof-obligation
   #:proof-evidence #:proof-evidence-p #:proof-evidence-obligation-name
   #:proof-evidence-payload #:make-proof-evidence
    #:proof-carrying-code #:proof-carrying-code-p #:proof-carrying-code-artifact
    #:proof-carrying-code-obligations #:proof-carrying-code-evidence
    #:make-proof-carrying-code #:make-nonzero-obligation #:make-type-obligation
    #:verify-proof-obligation #:verify-proof-evidence #:verify-proof-carrying-code
    #:proof-evidence-form-valid-p

    ;; ─── Datatype generic programming (FR-1602) ───────────────────────────
    #:generic-u1 #:generic-u1-p #:make-generic-u1
    #:generic-k1 #:generic-k1-p #:make-generic-k1 #:generic-k1-value #:generic-k1-type
    #:generic-m1 #:generic-m1-p #:make-generic-m1 #:generic-m1-meta #:generic-m1-representation
    #:generic-product #:generic-product-p #:make-generic-product #:generic-product-left #:generic-product-right
    #:generic-sum #:generic-sum-p #:make-generic-sum #:generic-sum-tag #:generic-sum-value
    #:generic-instance #:generic-instance-p #:make-generic-instance
    #:generic-instance-type #:generic-instance-representation #:generic-instance-show #:generic-instance-traverse
    #:*generic-instance-registry*
    #:register-generic-instance #:lookup-generic-instance
    #:generic-representation-of #:generic-show #:generic-transform #:generic-query
    #:generic-representation-valid-p

    ;; ─── Typed channels / actors / STM / coroutines / SIMD ────────────────
    #:typed-channel #:typed-channel-p
    #:send-channel #:send-channel-p #:send-channel-channel
    #:recv-channel #:recv-channel-p #:recv-channel-channel
    #:make-typed-channel #:make-buffered-channel
    #:channel-payload-type #:channel-send #:channel-recv #:close-typed-channel #:make-channel-type

    #:typed-actor-ref #:typed-actor-ref-p #:typed-actor-ref-message-type #:typed-actor-ref-state
    #:typed-actor-ref-remote-p #:typed-actor-ref-mailbox
    #:make-actor-ref #:actor-ref-type #:actor-message-accepted-p #:actor-send #:actor-stop

    #:tvar #:tvar-p #:tvar-type #:tvar-value
    #:stm-action #:stm-action-p #:stm-action-result-type #:stm-action-thunk #:stm-action-effects
    #:*stm-transaction-active*
    #:make-tvar #:stm-return #:stm-read #:stm-write #:stm-bind #:atomically #:make-stm-type

    #:typed-generator #:typed-generator-p #:typed-generator-yield-type #:typed-generator-return-type
    #:typed-coroutine #:typed-coroutine-p #:typed-coroutine-send-type #:typed-coroutine-receive-type
    #:typed-coroutine-return-type #:typed-coroutine-done-p
    #:make-generator #:generator-next #:make-coroutine #:coroutine-resume
    #:make-generator-type #:make-coroutine-type

    #:simd-vector #:simd-vector-p #:simd-vector-element-type #:simd-vector-lanes #:simd-vector-values
    #:make-simd-vector #:simd-map #:simd-add #:make-simd-type

    ;; ─── Type-level utility API (FR-1701/1702/1803/1804/3303/3304/3305) ──
    #:make-api-type #:api-route-lookup #:route-response-type-for

    #:frozen-value #:frozen-value-p #:make-frozen-value #:frozen-value-value #:frozen-value-type
    #:make-type-level-natural #:type-level-natural-p #:type-level-natural-value #:known-nat-value
    #:type-plus #:type-mul
    #:make-length-indexed-vector-type #:make-matrix-type #:matrix-mul-type
    #:make-type-level-string #:type-level-string-p #:type-level-string-value
    #:has-field-type #:get-field-type #:template-literal-type
    #:make-hlist-type #:hlist-head-type #:hlist-tail-type #:format-type
    #:readonly-type #:writable-type #:deep-readonly-type #:freeze
    #:partial-type #:required-type #:pick-type #:omit-type
    #:exclude-type #:extract-type #:non-nullable-type #:return-type-of

    ;; ─── Type node base ───────────────────────────────────────────────────
    #:type-node  #:type-node-source-location  #:type-node-kind

   ;; ─── Primitive ───────────────────────────────────────────────────────
   #:type-primitive   #:type-primitive-p   #:make-type-primitive  #:type-primitive-name
   #:type-int #:type-float #:type-string #:type-bool
   #:type-symbol #:type-cons #:type-null #:type-any  #:type-char #:type-unit

    ;; ─── Type variable ────────────────────────────────────────────────────
    #:type-var    #:type-var-p    #:fresh-type-var
    #:type-var-id #:type-var-name #:type-var-link
    #:type-var-upper-bound #:type-var-lower-bound
    #:type-var-equal-p
   #:reset-type-vars!

    ;; ─── Rigid variables ──────────────────────────────────────────────────
    #:type-rigid    #:type-rigid-p    #:fresh-rigid-var
    #:type-rigid-id #:type-rigid-name  #:type-rigid-equal-p

   ;; ─── Arrow ───────────────────────────────────────────────────────────
   #:type-arrow    #:type-arrow-p    #:make-type-arrow  #:make-type-arrow-raw
   #:type-arrow-params #:type-arrow-return #:type-arrow-effects #:type-arrow-mult

   ;; ─── Product ─────────────────────────────────────────────────────────
   #:type-product  #:type-product-p  #:make-type-product  #:type-product-elems

   ;; ─── Record / Variant ────────────────────────────────────────────────
   #:type-record   #:type-record-p   #:make-type-record
   #:type-record-fields #:type-record-row-var
   #:type-variant  #:type-variant-p  #:make-type-variant
   #:type-variant-cases #:type-variant-row-var

   ;; ─── Union / Intersection ────────────────────────────────────────────
   #:type-union    #:type-union-p    #:make-type-union  #:make-type-union-raw  #:type-union-types
   #:type-intersection  #:type-intersection-p  #:make-type-intersection
   #:make-type-intersection-raw  #:type-intersection-types

   ;; ─── Forall / Exists ─────────────────────────────────────────────────
    #:type-forall   #:type-forall-p   #:make-type-forall
     #:type-forall-var #:type-forall-knd #:type-forall-body
   #:type-exists   #:type-exists-p   #:make-type-exists
   #:type-exists-var #:type-exists-knd #:type-exists-body

   ;; ─── HKT / Lambda / Mu ───────────────────────────────────────────────
   #:type-app    #:type-app-p    #:make-type-app  #:type-app-fun #:type-app-arg
   #:type-lambda   #:type-lambda-p   #:make-type-lambda
   #:type-lambda-var #:type-lambda-knd #:type-lambda-body
   #:type-mu   #:type-mu-p   #:make-type-mu  #:type-mu-var #:type-mu-body
   #:type-constructor #:make-type-constructor #:type-constructor-p
   #:type-constructor-name #:type-constructor-args

   ;; ─── Refinement / Linear / Capability ───────────────────────────────
   #:type-refinement   #:type-refinement-p   #:make-type-refinement
   #:type-refinement-base #:type-refinement-predicate
   #:type-linear   #:type-linear-p   #:make-type-linear
   #:type-linear-base #:type-linear-grade
    #:type-capability   #:type-capability-p   #:make-type-capability
    #:type-capability-base #:type-capability-cap

    ;; ─── Advanced feature metadata / nodes ───────────────────────────────
    #:type-advanced-feature #:type-advanced-feature-p #:make-type-advanced-feature
    #:type-advanced-feature-fr-id #:type-advanced-feature-title #:type-advanced-feature-heads
    #:+type-advanced-feature-specs+
    #:*type-advanced-feature-registry*
    #:register-type-advanced-feature #:lookup-type-advanced-feature
    #:list-type-advanced-features #:list-type-advanced-feature-ids
    #:canonicalize-type-advanced-feature-id
     #:type-advanced-head-p #:type-advanced-feature-id-for-head
     #:type-advanced #:type-advanced-p #:make-type-advanced
     #:type-advanced-feature-id #:type-advanced-name #:type-advanced-args
     #:type-advanced-properties #:type-advanced-evidence
     #:type-advanced-property #:type-advanced-property-present-p
      #:type-advanced-security-label<= #:type-advanced-route-p
      #:type-advanced-semantic-domain #:type-advanced-semantics-implemented-p
      #:validate-type-advanced #:type-advanced-valid-p
      #:make-type-dynamic #:make-type-type-rep
       #:advanced-call-policy #:advanced-call-policy-p
       #:advanced-call-policy-function-name #:advanced-call-policy-summary
       #:advanced-call-policy-exact-args #:advanced-call-policy-min-args
       #:advanced-call-policy-validator #:advanced-call-policy-return-type
       #:*advanced-call-policy-registry* #:register-advanced-call-policy
       #:lookup-advanced-call-policy #:validate-advanced-call #:infer-advanced-call
       #:ffi-descriptor-from-form #:ffi-descriptor-lisp-type
       #:type-interface-module #:type-interface-module-p
       #:type-interface-module-name #:type-interface-module-exports
       #:type-interface-module-fingerprint #:type-interface-module-exported-types
       #:*type-interface-registry* #:register-type-interface
       #:lookup-type-interface #:lookup-type-interface-export
       #:*smt-solver-registry* #:register-smt-solver #:lookup-smt-solver
       #:solve-smt-constraint
       #:*type-checker-plugin-registry* #:register-type-checker-plugin
       #:lookup-type-checker-plugin #:run-type-checker-plugin
       #:*type-synthesis-strategy-registry* #:register-type-synthesis-strategy
       #:lookup-type-synthesis-strategy #:run-type-synthesis

       ;; ─── Effect types ────────────────────────────────────────────────────
      #:type-effect-row   #:type-effect-row-p   #:make-type-effect-row
      #:type-effect-row-effects #:type-effect-row-row-var
     #:+pure-effect-row+ #:+io-effect-row+
     #:type-effect-op   #:type-effect-op-p   #:make-type-effect-op
     #:type-effect-op-name #:type-effect-op-args
     #:type-handler   #:type-handler-p   #:make-type-handler
    #:type-handler-effect #:type-handler-input #:type-handler-output

   ;; ─── GADT ────────────────────────────────────────────────────────────
   #:type-gadt-con   #:type-gadt-con-p   #:make-type-gadt-con
   #:type-gadt-con-name #:type-gadt-con-arg-types #:type-gadt-con-index-type

   ;; ─── Constraint / Qualified ──────────────────────────────────────────
    #:type-constraint   #:type-constraint-p   #:make-type-constraint
    #:type-constraint-class-name #:type-constraint-type-arg
     #:type-qualified   #:type-qualified-p   #:make-type-qualified
     #:type-qualified-constraints #:type-qualified-body

   ;; ─── Error sentinel / unknown ────────────────────────────────────────
   #:type-error   #:type-error-p   #:make-type-error  #:type-error-message
   #:type-unknown-p
   #:+type-unknown+

   ;; ─── Type scheme ─────────────────────────────────────────────────────
   #:type-scheme   #:type-scheme-p
   #:type-scheme-quantified-vars #:type-scheme-type
   #:make-type-scheme  #:make-type-scheme-raw  #:type-to-scheme
   #:generalize    #:instantiate

   ;; ─── Type environment ────────────────────────────────────────────────
   #:type-env  #:type-env-p  #:make-type-env #:type-env-bindings
   #:type-env-empty  #:type-env-lookup  #:type-env-extend  #:type-env-extend*
   #:type-env-free-vars

   ;; ─── Structural utilities ────────────────────────────────────────────
   #:type-equal-p  #:type-free-vars #:type-children #:type-bound-var
   #:type-to-string  #:normalize-type-variables

   ;; ─── Substitution ────────────────────────────────────────────────────
   #:substitution  #:substitution-p  #:make-substitution
   #:substitution-bindings  #:substitution-generation
   #:subst-lookup  #:subst-extend  #:subst-extend!  #:subst-compose
   #:zonk-env
   #:zonk
   #:type-occurs-p  #:apply-unification

   ;; ─── Unification ─────────────────────────────────────────────────────
   #:type-unify  #:type-unify-lists
    #:type-inference-error  #:type-inference-error-message
    #:typed-hole-error
    #:type-mismatch-error  #:type-mismatch-error-expected  #:type-mismatch-error-actual
   #:unbound-variable-error  #:unbound-variable-name  #:unbound-variable-error-name
   #:check-qualified-constraints

   ;; ─── Effect system ───────────────────────────────────────────────────
   #:effect-def  #:effect-def-p  #:make-effect-def
   #:effect-def-name  #:effect-def-type-params  #:effect-def-operations
   #:*effect-registry*  #:register-effect  #:lookup-effect
   #:*effect-signature-table*  #:*constant-effect-table*
   #:*pure-ast-effect-types*
   #:register-effect-signature  #:lookup-effect-signature
   #:effect-row-union  #:effect-row-subset-p
   #:infer-effects  #:infer-with-effects  #:check-body-effects

   ;; ─── Row polymorphism ────────────────────────────────────────────────
   #:row-extend  #:row-restrict  #:row-select  #:row-labels
   #:row-closed-p  #:row-open-p
   #:effect-row-extend  #:effect-row-restrict  #:effect-row-member-p

   ;; ─── Constraint language ─────────────────────────────────────────────
   #:constraint  #:constraint-p  #:constraint-kind  #:constraint-args
   #:make-equal-constraint        #:make-subtype-constraint
   #:make-typeclass-constraint    #:make-implication-constraint
   #:make-effect-subset-constraint #:make-mult-leq-constraint
   #:make-row-lacks-constraint    #:make-kind-equal-constraint
   #:constraint-free-vars  #:constraint-substitute

   ;; ─── Subtyping ───────────────────────────────────────────────────────
   #:type-constructor-def  #:type-constructor-def-p
   #:*type-constructor-registry*
   #:register-type-constructor  #:lookup-type-constructor
    #:subtypep  #:is-subtype-p
     #:type-join  #:type-meet
     #:*subtype-table*  #:type-name-subtype-p  #:find-common-supertype
     #:*protocol-type-registry* #:register-protocol-type #:lookup-protocol-type
     #:upgraded-array-element-type  #:upgraded-complex-part-type

    ;; ─── Typeclass system ────────────────────────────────────────────────
    #:typeclass-def  #:typeclass-def-p  #:make-typeclass-def
    #:typeclass-def-name  #:typeclass-def-type-params  #:typeclass-def-superclasses
    #:typeclass-def-methods  #:typeclass-def-defaults  #:typeclass-def-associated-types
    #:*typeclass-registry*  #:register-typeclass  #:lookup-typeclass
     #:typeclass-instance  #:typeclass-instance-p  #:typeclass-instance-class-name
     #:typeclass-instance-methods
     #:*typeclass-instance-registry*
     #:register-typeclass-instance  #:lookup-typeclass-instance
     #:has-typeclass-instance-p  #:check-typeclass-constraint
     #:dict-env-extend  #:dict-env-lookup

   ;; ─── Constraint solver ───────────────────────────────────────────────
   #:collect-constraints  #:solve-constraints

   ;; ─── Inference engine ────────────────────────────────────────────────
   #:narrow-union-type  #:extract-type-guard
   #:infer  #:infer-with-env
   #:infer-binop  #:infer-if  #:infer-let  #:infer-lambda
   #:infer-call   #:infer-progn  #:infer-args
   #:annotate-type
   #:*class-type-registry*  #:register-class-type
   #:lookup-class-type  #:lookup-slot-type
   #:*class-method-type-registry* #:register-class-method-type
   #:lookup-class-method-types #:lookup-class-method-type
   #:*type-alias-registry*  #:register-type-alias  #:lookup-type-alias
   #:*type-predicate-table*  #:register-type-predicate
   #:syntactic-value-p

   ;; ─── Bidirectional checker ───────────────────────────────────────────
   #:synthesize  #:check  #:check-body
   #:check-skolem-escape  #:skolem-appears-in-type-p

   ;; ─── Parser ──────────────────────────────────────────────────────────
   #:type-parse-error  #:type-parse-error-message
   #:parse-type-specifier  #:parse-primitive-type  #:parse-compound-type
   #:parse-function-type
   #:parse-lambda-list-with-types  #:parse-typed-parameter
   #:parse-typed-optional-parameter
   #:extract-return-type
   #:ast-defun-typed  #:ast-defun-typed-name  #:ast-defun-typed-params
   #:ast-defun-typed-param-types  #:ast-defun-typed-return-type
   #:ast-defun-typed-body  #:ast-defun-typed-source-location  #:make-ast-defun-typed
   #:ast-lambda-typed  #:ast-lambda-typed-params  #:ast-lambda-typed-param-types
   #:ast-lambda-typed-return-type  #:ast-lambda-typed-body
   #:ast-lambda-typed-env  #:ast-lambda-typed-source-location  #:make-ast-lambda-typed
   #:parse-typed-defun  #:parse-typed-lambda
   #:looks-like-type-specifier-p
   #:*lambda-list-keywords*

   ;; ─── Exhaustiveness checking ─────────────────────────────────────────
   #:check-typecase-exhaustiveness
   #:check-etypecase-completeness
   #:useful-typecase-arms
   #:typecase-arm-subsumed-p

   ;; ─── Printer ─────────────────────────────────────────────────────────
   #:unparse-type

   ))

(declaim (special type-int type-float type-string type-bool type-symbol
                  type-cons type-null type-any type-char type-unit
                  +type-unknown+))

(in-package :cl-cc/type)

;; Define *type-alias-registry* early so type/parser.lisp can reference it
;; before type/inference.lisp is loaded. defvar is idempotent.
(defvar *type-alias-registry* (make-hash-table :test #'eq)
  "Maps type alias names to their type specifications.")
