;;;; package.lisp - Type Representation Package Definition
;;;;
;;;; This module defines the package for HM type system with gradual typing.

(defpackage :cl-cc/type
  (:use :cl)
  (:export
   ;; Type Base Class
   #:type-node
   #:type-node-source-location

   ;; Type Classes
   #:type-primitive
   #:type-variable
   #:type-function
   #:type-tuple
   #:type-union
   #:type-intersection
   #:type-constructor
   #:type-unknown

   ;; Type Accessors
   #:type-primitive-name
   #:type-variable-id
   #:type-variable-name
   #:type-function-params
   #:type-function-return
   #:type-tuple-elements
   #:type-union-types
   #:type-intersection-types
   #:type-constructor-name
   #:type-constructor-args

   ;; Type Constructors
   #:make-type-primitive
   #:make-type-variable
   #:make-type-function
   #:make-type-function-raw
   #:make-type-tuple
   #:make-type-tuple-raw
   #:make-type-union
   #:make-type-union-raw
   #:make-type-intersection
   #:make-type-intersection-raw
   #:make-type-constructor
   #:make-type-constructor-raw
   #:make-type-unknown
   #:make-type-env
   #:make-type-constraint
   #:make-type-scheme
   #:make-type-scheme-raw
   #:make-ast-defun-typed
   #:make-ast-lambda-typed

    ;; Singleton Type Instances
    #:type-int
    #:type-string
    #:type-bool
    #:type-symbol
    #:type-cons
    #:type-null
    #:type-any
    #:+type-unknown+

    ;; Type Predicates
    #:type-variable-p
    #:type-constructor-p
    #:type-variable-equal-p
    #:type-equal-p
    #:type-to-string

    ;; Substitution Operations
    #:empty-subst
    #:subst-lookup
    #:extend-subst
    #:type-substitute
    #:apply-subst
    #:apply-subst-env
    #:compose-subst

    ;; Unification
    #:type-unify
    #:type-unify-lists
    #:type-occurs-p

    ;; Free Variables
    #:type-free-vars
    #:environment-free-vars

    ;; Type Schemes (Let-Polymorphism)
    #:type-scheme
    #:type-scheme-quantified-vars
    #:type-scheme-type
    #:make-type-scheme
    #:type-to-scheme
    #:generalize
    #:instantiate

    ;; Utility
    #:normalize-type-variables
    #:apply-unification

    ;; Type Parser - Conditions
    #:type-parse-error
    #:type-parse-error-message

    ;; Type Parser - Core Functions
    #:parse-type-specifier
    #:parse-primitive-type
    #:parse-compound-type
    #:parse-function-type

    ;; Type Parser - Lambda List Functions
    #:parse-lambda-list-with-types
    #:parse-typed-parameter
    #:parse-typed-optional-parameter
    #:parse-typed-rest-parameter

    ;; Type Parser - Return Type Functions
    #:extract-return-type

    ;; Type Parser - Typed AST Classes
    #:ast-defun-typed
    #:ast-defun-typed-name
    #:ast-defun-typed-params
    #:ast-defun-typed-param-types
    #:ast-defun-typed-return-type
    #:ast-defun-typed-body
    #:ast-defun-typed-source-location

    #:ast-lambda-typed
    #:ast-lambda-typed-params
    #:ast-lambda-typed-param-types
    #:ast-lambda-typed-return-type
    #:ast-lambda-typed-body
    #:ast-lambda-typed-env
    #:ast-lambda-typed-source-location

    ;; Type Parser - AST Parsers
    #:parse-typed-defun
    #:parse-typed-lambda
    #:parse-typed-lambda-list
    #:extract-return-type-from-body

     ;; Type Parser - Utilities
     #:make-type-function-from-spec
     #:unparse-type
     #:looks-like-type-specifier-p
     #:*lambda-list-keywords*

     ;; Type Environment
     #:type-env
     #:type-env-empty
     #:type-env-lookup
     #:type-env-extend
     #:type-env-extend*
     #:type-env-to-alist
     #:type-env-free-vars

     ;; Type Inference - Algorithm W
     #:fresh-type-var
     #:reset-type-vars!
     #:infer
     #:infer-binop
     #:infer-if
     #:infer-let
     #:infer-lambda
     #:infer-call
     #:infer-progn
     #:infer-sequence
     #:infer-args
     #:infer-top-level
     #:infer-with-env
     #:annotate-type

     ;; Class Type Registry
     #:*class-type-registry*
     #:register-class-type
     #:lookup-class-type
     #:lookup-slot-type

     ;; Type Alias Registry
     #:*type-alias-registry*
     #:register-type-alias
     #:lookup-type-alias

     ;; Type Inference Conditions
     #:type-inference-error
     #:type-inference-error-message
     #:unbound-variable-error
     #:type-mismatch-error

     ;; Constraint Solving
     #:type-constraint
     #:make-constraint
     #:collect-constraints
     #:solve-constraints

     ;; Generalization/Instantiation Helpers
     #:generalize-in-env
     #:instantiate-scheme

     ;; Bidirectional Type Checking (Phase 3)
     #:synthesize
     #:check
     #:check-body

     ;; Typeclass Registry (Phase 4)
     #:*typeclass-registry*
     #:register-typeclass
     #:lookup-typeclass
     #:*typeclass-instance-registry*
     #:register-typeclass-instance
     #:lookup-typeclass-instance
     #:has-typeclass-instance-p
     #:check-typeclass-constraint

     ;; Phase 4: Typeclass Type Nodes
     #:type-class
     #:make-type-class
     #:type-class-name
     #:type-class-type-param
     #:type-class-methods
     #:type-class-p

     #:type-class-constraint
     #:make-type-class-constraint
     #:type-class-constraint-class-name
     #:type-class-constraint-type-arg
     #:type-class-constraint-p

     #:type-qualified
     #:make-type-qualified
     #:type-qualified-constraints
     #:type-qualified-type
     #:type-qualified-p

     ;; Phase 5: Effect Type Nodes
     #:type-effect
     #:make-type-effect
     #:type-effect-name
     #:type-effect-p

     #:type-effect-row
     #:make-type-effect-row
     #:type-effect-row-effects
     #:type-effect-row-row-var
     #:type-effect-row-p

     #:+pure-effect-row+
     #:+io-effect-row+

     #:type-effectful-function
     #:make-type-effectful-function
     #:type-effectful-function-effects

     ;; Phase 5: Effect Inference Functions
     #:infer-effects
     #:infer-with-effects
     #:effect-row-union
     #:effect-row-subset-p
     #:check-body-effects
     #:register-effect-signature
     #:lookup-effect-signature
     #:*effect-signature-table*

     ;; Phase 6: Rank-N Polymorphism
     #:type-forall
     #:make-type-forall
     #:type-forall-var
     #:type-forall-type
     #:type-forall-p

     ;; Phase 6 Rank-N skolem constants
     #:type-skolem
     #:make-type-skolem
     #:type-skolem-p
     #:type-skolem-id
     #:type-skolem-name
     #:type-skolem-equal-p
     #:check-skolem-escape
     #:skolem-appears-in-type-p))

(in-package :cl-cc/type)
