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

   ;; ─── Type node base ───────────────────────────────────────────────────
   #:type-node  #:type-node-source-location  #:type-node-kind

   ;; ─── Primitive ───────────────────────────────────────────────────────
   #:type-primitive   #:type-primitive-p   #:make-type-primitive  #:type-primitive-name
   #:type-int #:type-float #:type-string #:type-bool
   #:type-symbol #:type-cons #:type-null #:type-any  #:type-char #:type-unit

   ;; ─── Type variable ────────────────────────────────────────────────────
   #:type-var    #:type-var-p    #:fresh-type-var
   #:type-var-id #:type-var-name #:type-var-link  #:type-var-equal-p
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
