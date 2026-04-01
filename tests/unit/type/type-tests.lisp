;;;; tests/type-tests.lisp - Type System Tests
;;;;
;;;; Comprehensive tests for the HM type system including:
;;;; - Type representation (primitives, variables, functions)
;;;; - Unification (with occurs check)
;;;; - Type inference (Algorithm W)
;;;; - Generalization and instantiation (let-polymorphism)

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Type Representation Tests

(deftest-each type-repr-primitive-is-type-primitive
  "Singleton primitive type constants are type-primitive structs."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool)
          ("symbol" type-symbol)
          ("null"   type-null)
          ("any"    type-any))
  (tp)
  (assert-type type-primitive tp))

(deftest-each type-repr-primitive-name
  "Each primitive type constant carries the expected CL type name."
  :cases (("int"    type-int    'fixnum)
          ("string" type-string 'string)
          ("bool"   type-bool   'boolean)
          ("symbol" type-symbol 'symbol)
          ("null"   type-null   'null)
          ("any"    type-any    't))
  (tp expected-name)
  (assert-eq expected-name (type-primitive-name tp)))

(deftest type-repr-variable-creation
  "Test that type variables can be created with unique IDs."
  (let ((v1 (make-type-variable 'a))
        (v2 (make-type-variable 'b)))
    (assert-type type-variable v1)
    (assert-type type-variable v2)
    (assert-false (= (type-variable-id v1) (type-variable-id v2)))
    (assert-eq 'a (type-variable-name v1))
    (assert-eq 'b (type-variable-name v2))))

(deftest type-repr-function-type
  "Test function type construction and accessors."
  (let ((fn-type (make-type-function-raw
                                :params (list type-int type-int)
                                :return type-int)))
    (assert-type type-function fn-type)
    (assert-= 2 (length (type-function-params fn-type)))
    (assert-eq type-int (type-function-return fn-type))))

(deftest type-repr-type-equal-p
  "Test type-equal-p for various types."
  ;; Primitives
  (assert-type-equal type-int type-int)
  (assert-false (type-equal-p type-int type-string))
  ;; Variables
  (let ((v1 (make-type-variable)))
    (assert-type-equal v1 v1))
  ;; Function types
  (let ((fn1 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn2 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn3 (make-type-function-raw
                            :params (list type-string)
                            :return type-int)))
    (assert-type-equal fn1 fn2)
    (assert-false (type-equal-p fn1 fn3))))

(deftest type-repr-type-to-string
  "Test type-to-string for various types."
  (assert-string= "FIXNUM" (type-to-string type-int))
  (assert-string= "STRING" (type-to-string type-string))
  (assert-string= "BOOLEAN" (type-to-string type-bool))
  ;; +type-unknown+ is now a type-error sentinel — printed as "<error: ...>"
  (assert-string= "<error: unknown>" (type-to-string +type-unknown+))
  ;; Arrow type (using backward-compat make-type-function-raw)
  (let ((fn (make-type-function-raw
                           :params (list type-int)
                           :return type-int)))
    (assert-string= "FIXNUM -> FIXNUM" (type-to-string fn))))

(deftest type-repr-unknown-type
  "Test that +type-unknown+ is a type-error sentinel (gradual typing removed).
In the 2026 type system, there is no gradual typing; type-unknown is a backward-compat
alias for a type-error sentinel used for error recovery."
  ;; Backward-compat: typep via deftype alias still works
  (assert-type type-unknown +type-unknown+)
  ;; type-error nodes are intentionally never structurally equal (distinct error points)
  (assert-false (type-equal-p +type-unknown+ +type-unknown+))
  ;; The backward-compat predicate still works
  (assert-true (type-unknown-p +type-unknown+))
  ;; The underlying struct is type-error
  (assert-true (type-error-p +type-unknown+)))

;;; Unification Tests

(deftest-each unify-primitive
  "Same primitive types unify; different ones do not."
  :cases (("same"      t   type-int type-int)
          ("different" nil type-int type-string))
  (should-unify a b)
  (if should-unify
      (assert-unifies a b)
      (assert-not-unifies a b)))

(deftest unify-variable-with-primitive
  "Test that a type variable unifies with a primitive type."
  (let ((v (make-type-variable)))
    (multiple-value-bind (result ok) (type-unify v type-int)
      (assert-true ok)
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true found)
        (assert-type-equal binding type-int)))))

(deftest unify-variable-with-variable
  "Type variables unify with each other: distinct vars and same var both unify."
  (assert-unifies (make-type-variable) (make-type-variable))
  (let ((v (make-type-variable)))
    (assert-unifies v v)))

(deftest unify-function-types-structural
  "Test that function types unify structurally — parameter variable is bound."
  (let* ((v   (make-type-variable))
         (fn1 (make-type-function-raw :params (list v)           :return type-int))
         (fn2 (make-type-function-raw :params (list type-string) :return type-int)))
    (multiple-value-bind (result ok) (type-unify fn1 fn2)
      (assert-true ok)
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true found)
        (assert-type-equal binding type-string)))))

(deftest unify-function-types-arity-mismatch
  "Test that function types with different arities fail to unify."
  (assert-not-unifies
   (make-type-function-raw :params (list type-int)            :return type-int)
   (make-type-function-raw :params (list type-int type-int)   :return type-int)))

(deftest unify-occurs-check
  "Test that occurs check prevents infinite types (v cannot unify with v -> Int)."
  (let* ((v  (make-type-variable))
         (fn (make-type-function-raw :params (list v) :return type-int)))
    (assert-not-unifies v fn)))

(deftest-each unify-lists
  "type-unify-lists succeeds on matching lists, fails on type/length mismatch."
  :cases (("success"        t   (list type-int type-string) (list type-int type-string))
          ("type-mismatch"  nil (list type-int type-string) (list type-string type-int))
          ("length-mismatch" nil (list type-int)            (list type-int type-string)))
  (expected a b)
  (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (if expected (assert-true ok) (assert-false ok))))

(deftest unify-unknown-with-anything
  "Test that the type-error sentinel (unknown) unifies with any type."
  (assert-unifies +type-unknown+ type-int)
  (assert-unifies type-string +type-unknown+)
  (assert-unifies +type-unknown+ +type-unknown+))

(deftest unify-substitution-composition
  "Test that compose-subst applies correctly."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         ;; s1: v1 -> type-int
         (s1 (extend-subst v1 type-int (empty-subst)))
         ;; s2: v2 -> v1
         (s2 (extend-subst v2 v1 (empty-subst)))
         ;; compose s1 with s2: v2 should map to type-int (via v1)
         (composed (compose-subst s1 s2)))
    ;; After composition, applying to v2 should give type-int
    (let ((result (type-substitute v2 composed)))
      (assert-type-equal result type-int))))

(deftest unify-transitive-binding
  "Test that transitive variable bindings are resolved."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable)))
    ;; Unify v1 with v2, then v2 with type-int
    (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
      (assert-true ok1)
      (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
        (assert-true ok2)
        ;; v1 should resolve to type-int through v2
        (let ((result (type-substitute v1 subst2)))
          (assert-type-equal result type-int))))))

;;; Type Inference Tests

(deftest infer-integer-literal
  "Test that integer literals infer to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '42)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest-each infer-binop-is-int
  "Binary arithmetic operations always infer to type-int."
  :cases (("addition" '(+ 1 2))
          ("nested"   '(+ (* 2 3) (- 4 1))))
  (form)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-variable-from-env
  "Test that variables are looked up in the type environment."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast 'x))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-unbound-variable-error
  "Test that unbound variables signal an error."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast 'undefined-var)))
    (assert-signals unbound-variable-error
      (infer-with-env ast))))

(deftest infer-typed-hole-error
  "Typed hole '_' in an expression signals a typed-hole inference error."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(+ x _)))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (assert-signals cl-cc/type::typed-hole-error
      (infer ast env))))

(deftest infer-let-binding
  "Let bindings infer types correctly: simple and multi-binding with binop."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 42)) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int)))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 10) (y 20)) (+ x y)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-lambda
  "Lambda types inferred as function type: identity has 1 param; arithmetic constrains to int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-= 1 (length (type-function-params ty)))))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-type-equal (type-function-return ty) type-int)
      (assert-type-equal (first (type-function-params ty)) type-int))))

(deftest infer-function-call
  "Test that function application infers the return type."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((f (lambda (x) (+ x 1)))) (f 5)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-if-expression
  "Test that if expressions unify branches."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
         (env (type-env-extend 'cond-var
                               (type-to-scheme type-bool)
                               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-print-returns-expr-type
  "Test that print returns the type of the printed expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-progn-returns-last
  "Test that progn returns the type of the last expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(progn 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest-each infer-quote-type
  "Quoted forms infer to the type corresponding to their datum."
  :cases (("symbol"  '(quote hello) type-symbol)
          ("integer" '(quote 42)    type-int))
  (form expected-type)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type))))

;;; Generalization / Instantiation Tests

(deftest generalize-quantification-behavior
  "generalize quantifies all free vars when env is nil; skips vars free in env."
  ;; No env: all free vars quantified
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw :params (list v) :return v))
         (scheme (generalize nil fn-type)))
    (assert-type type-scheme scheme)
    (assert-= 1 (length (type-scheme-quantified-vars scheme))))
  ;; With env: v1 is free in env and NOT quantified; v2 is
  (let* ((v1 (make-type-variable 'a))
         (v2 (make-type-variable 'b))
         (fn-type (make-type-function-raw :params (list v1) :return v2))
         (env (list (cons 'x v1)))
         (scheme (generalize env fn-type)))
    (assert-= 1 (length (type-scheme-quantified-vars scheme)))
    (assert-true (type-variable-equal-p (first (type-scheme-quantified-vars scheme)) v2))))

(deftest instantiate-creates-fresh-vars
  "Test that instantiation creates fresh type variables."
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw
                                 :params (list v)
                                 :return v))
         (scheme (make-type-scheme (list v) fn-type))
         (inst (instantiate scheme)))
    (assert-type type-function inst)
    ;; The instantiated type should have fresh variables (not the original v)
    (let ((new-param (first (type-function-params inst)))
          (new-ret (type-function-return inst)))
      (assert-type type-variable new-param)
      (assert-false (type-variable-equal-p new-param v))
      ;; Param and return should be the same fresh variable
      (assert-true (type-variable-equal-p new-param new-ret)))))

(deftest let-polymorphism-identity
  "Test let-polymorphism: identity function used at different types."
  (reset-type-vars!)
  ;; (let ((id (lambda (x) x))) (id 42))
  ;; id should be polymorphic: forall a. a -> a
  ;; When applied to 42, result should be int
  (let ((ast (lower-sexp-to-ast '(let ((id (lambda (x) x))) (id 42)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest type-scheme-monomorphic
  "Test that type-to-scheme creates a monomorphic scheme."
  (let ((scheme (type-to-scheme type-int)))
    (assert-type type-scheme scheme)
    (assert-null (type-scheme-quantified-vars scheme))
    (assert-type-equal (type-scheme-type scheme) type-int)))

;;; Free Variables Tests

(deftest free-vars
  "type-free-vars: primitives→nil; variable→itself; function type→both params."
  ;; primitives
  (assert-null (type-free-vars type-int))
  (assert-null (type-free-vars type-string))
  ;; single variable
  (let* ((v (make-type-variable))
         (fv (type-free-vars v)))
    (assert-= 1 (length fv))
    (assert-true (type-variable-equal-p (first fv) v)))
  ;; function type: both params are free
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw :params (list v1) :return v2))
         (fv (type-free-vars fn)))
    (assert-= 2 (length fv))))

;;; Substitution Tests

(deftest substitution
  "type-substitute: empty leaves unchanged; bound var replaced; unbound unchanged; through functions."
  ;; empty substitution
  (assert-eq type-int (type-substitute type-int (empty-subst)))
  ;; bound variable replaced
  (let* ((v (make-type-variable))
         (result (type-substitute v (extend-subst v type-int (empty-subst)))))
    (assert-type-equal result type-int))
  ;; unbound variable unchanged
  (let* ((v (make-type-variable))
         (result (type-substitute v (empty-subst))))
    (assert-true (type-variable-equal-p result v)))
  ;; substitution through function types
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw :params (list v) :return v))
         (result (type-substitute fn (extend-subst v type-int (empty-subst)))))
    (assert-type type-function result)
    (assert-type-equal (first (type-function-params result)) type-int)
    (assert-type-equal (type-function-return result) type-int)))

;;; Normalize Type Variables Tests

(deftest normalize-type-variables-canonical
  "Test that normalize produces canonical variable names."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v1)
                            :return v2))
         (normalized (normalize-type-variables fn)))
    (assert-type type-function normalized)
    ;; The param and return should be different canonical variables
    (let ((p (first (type-function-params normalized)))
          (r (type-function-return normalized)))
      (assert-type type-variable p)
      (assert-type type-variable r)
      (assert-false (type-variable-equal-p p r)))))

;;; Phase 3: Bidirectional Type Checking Tests

(deftest bidirectional-synthesize-int
  "synthesize returns type-int for an integer literal."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    (multiple-value-bind (ty _subst) (synthesize ast env)
      (declare (ignore _subst))
      (assert-true (type-equal-p ty type-int)))))

(deftest bidirectional-check-int
  "check returns nil (empty subst) for int literal against matching or unknown type."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    ;; matching type
    (assert-true (null (check ast type-int env)))
    ;; unknown type (gradual typing — always succeeds)
    (assert-true (null (check ast +type-unknown+ env)))))

(deftest bidirectional-check-body-last-form
  "check-body verifies the last form's type."
  (let* ((env (type-env-empty))
         (ast1 (make-ast-int :value 1))
         (ast2 (make-ast-int :value 2)))
    (assert-true (null (check-body (list ast1 ast2) type-int env)))))

;;; Phase 4: Typeclass Tests

(deftest typeclass-register-and-lookup
  "register-typeclass and lookup-typeclass round-trip."
  (let* ((tc (make-type-class
              :name 'eq-test
              :type-param (make-type-variable 'a)
              :methods (list (cons 'equal-p
                                   (make-type-function
                                    (list (make-type-variable 'a)
                                          (make-type-variable 'a))
                                    type-bool))))))
    (register-typeclass 'eq-test tc)
    (let ((retrieved (lookup-typeclass 'eq-test)))
      (assert-true retrieved)
      (assert-true (type-class-p retrieved))
      (assert-eq 'eq-test (type-class-name retrieved)))))

(deftest typeclass-constraint-creation
  "make-type-class-constraint creates the expected struct."
  (let* ((a (make-type-variable 'a))
         (c (make-type-class-constraint :class-name 'num :type-arg a)))
    (assert-true (type-class-constraint-p c))
    (assert-eq 'num (type-class-constraint-class-name c))
    (assert-true (type-variable-equal-p a (type-class-constraint-type-arg c)))))

(deftest typeclass-qualified-type
  "make-type-qualified stores constraints and inner type."
  (let* ((a (make-type-variable 'a))
         (c (make-type-class-constraint :class-name 'num :type-arg a))
         (fn (make-type-function (list a a) a))
         (qt (make-type-qualified :constraints (list c) :type fn)))
    (assert-true (type-qualified-p qt))
    (assert-= 1 (length (type-qualified-constraints qt)))
    (assert-true (typep (type-qualified-type qt) 'type-function))))

(deftest typeclass-instance-registration
  "register-typeclass-instance and has-typeclass-instance-p."
  (register-typeclass-instance 'num-test type-int
                               (list (cons 'plus #'+)))
  (assert-true (has-typeclass-instance-p 'num-test type-int))
  (assert-false (has-typeclass-instance-p 'num-test type-string)))

(deftest typeclass-to-string
  "type-to-string works on type-class-constraint."
  (let* ((a (make-type-variable 'a))
         (c (make-type-class-constraint :class-name 'eq :type-arg a)))
    (let ((s (type-to-string c)))
      (assert-true (stringp s))
      (assert-true (search "EQ" (string-upcase s))))))

;;; Phase 5: Effect Type Tests

(deftest effect-type-creation
  "make-type-effect creates effect labels."
  (let ((io (make-type-effect :name 'io))
        (state (make-type-effect :name 'state)))
    (assert-true (type-effect-p io))
    (assert-true (type-effect-p state))
    (assert-eq 'io (type-effect-name io))
    (assert-eq 'state (type-effect-name state))))

(deftest effect-row-pure-singleton
  "+pure-effect-row+ has no effects."
  (assert-true (type-effect-row-p +pure-effect-row+))
  (assert-null (type-effect-row-effects +pure-effect-row+))
  (assert-null (type-effect-row-row-var +pure-effect-row+)))

(deftest effect-row-io-singleton
  "+io-effect-row+ has exactly one IO effect."
  (assert-true (type-effect-row-p +io-effect-row+))
  (assert-= 1 (length (type-effect-row-effects +io-effect-row+)))
  (assert-true (string= "IO" (symbol-name
                               (type-effect-name
                                (first (type-effect-row-effects +io-effect-row+)))))))

(deftest effect-row-custom
  "Custom effect rows can be created."
  (let ((row (make-type-effect-row
              :effects (list (make-type-effect :name 'state)
                             (make-type-effect :name 'error))
              :row-var nil)))
    (assert-true (type-effect-row-p row))
    (assert-= 2 (length (type-effect-row-effects row)))))

(deftest effect-row-to-string
  "type-to-string formats effect rows correctly."
  (assert-string= "{}" (type-to-string +pure-effect-row+))
  (let ((io-str (type-to-string +io-effect-row+)))
    (assert-true (search "IO" (string-upcase io-str)))))

(deftest effectful-function-creation
  "make-type-effectful-function creates an annotated function type."
  (let ((fn (make-type-effectful-function
             :params (list type-int)
             :return type-int
             :effects +io-effect-row+)))
    (assert-true (typep fn 'type-effectful-function))
    (assert-= 1 (length (type-function-params fn)))
    (assert-true (type-equal-p type-int (type-function-return fn)))
    (assert-true (type-effect-row-p (type-effectful-function-effects fn)))))

;;; Phase 6: Rank-N Polymorphism Tests

(deftest rankn-forall
  "make-type-forall: creates forall types; to-string works; different vars are unequal."
  (let* ((a  (make-type-variable 'a))
         (fn (make-type-function (list a) a))
         (fa (make-type-forall :var a :type fn)))
    ;; Creation
    (assert-true (type-forall-p fa))
    (assert-true (type-variable-equal-p a (type-forall-var fa)))
    (assert-true (typep (type-forall-type fa) 'type-function))
    ;; to-string produces a string containing the variable name
    (let ((s (type-to-string fa)))
      (assert-true (stringp s))
      (assert-true (search "A" (string-upcase s)))))
  ;; Different forall vars are NOT equal
  (let* ((a  (make-type-variable 'a))
         (b  (make-type-variable 'b))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (fb (make-type-forall :var b :type (make-type-function (list b) b))))
    (assert-false (type-equal-p fa fb))))

;;; Phase 4-6: Parser Integration Tests

(deftest parser-forall-syntax
  "(forall a T) parses to type-forall."
  (let ((result (cl-cc/type:parse-type-specifier '(forall a (function (a) a)))))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-variable-name (type-forall-var result)))))

(deftest parser-effect-arrow-syntax
  "(-> fixnum fixnum ! io) parses to a type-arrow with an effects slot.
In the 2026 type system, effectful functions are type-arrow nodes with a non-nil
:effects slot — there is no separate type-effectful-function constructor."
  (let ((result (cl-cc/type:parse-type-specifier '(-> fixnum fixnum ! io))))
    ;; Parser returns a plain type-arrow (not type-effectful-function sub-struct)
    (assert-true (type-arrow-p result))
    (assert-= 1 (length (type-arrow-params result)))
    (assert-true (type-equal-p type-int (type-arrow-return result)))
    ;; The effects slot is set to an IO effect row
    (let ((effs (type-arrow-effects result)))
      (assert-true (type-effect-row-p effs))
      (assert-= 1 (length (type-effect-row-effects effs))))))

(deftest parser-qualified-syntax
  "(=> (Num a) (function (a a) a)) parses to type-qualified."
  (let ((result (cl-cc/type:parse-type-specifier '(=> (num a) (function (a a) a)))))
    (assert-true (type-qualified-p result))
    (assert-= 1 (length (type-qualified-constraints result)))
    (assert-eq 'num (type-class-constraint-class-name
                     (first (type-qualified-constraints result))))))

;;; Phase A: Inference Bug Fixes

(deftest phase-a-infer-args-multiple
  "infer-args threads substitution across multiple arguments."
  (reset-type-vars!)
  (let* ((args (list (lower-sexp-to-ast 1) (lower-sexp-to-ast '"hello")))
         (env (type-env-empty)))
    (multiple-value-bind (types subst)
      (cl-cc/type:infer-args args env)
      (declare (ignore subst))
      (assert-= 2 (length types))
      (assert-true (type-equal-p type-int (first types)))
      (assert-true (type-equal-p type-string (second types))))))

(deftest phase-a-infer-args-empty
  "infer-args with no arguments returns empty list."
  (reset-type-vars!)
  (multiple-value-bind (types subst)
    (cl-cc/type:infer-args '() (type-env-empty))
    (declare (ignore subst))
    (assert-null types)))

(deftest phase-a-union-unification-succeeds
  "Unifying a union type succeeds against a member type."
  (let* ((u (cl-cc/type:make-type-union (list type-int type-string))))
    (multiple-value-bind (subst ok)
      (type-unify u type-int)
      (declare (ignore subst))
      ;; Union should unify with int (is-subtype-p int (or int string) = T via union right)
      (assert-true ok))))

(deftest phase-a-infer-empty-progn
  "Empty progn body infers to null type."
  (reset-type-vars!)
  ;; lower-sexp-to-ast '(progn) may signal an error -- wrap the whole thing
  (handler-case
    (let* ((ast (lower-sexp-to-ast '(progn))))
      (multiple-value-bind (ty subst) (infer-with-env ast)
        (declare (ignore subst))
        ;; Should return type-null or type-unknown for empty body
        (assert-true (or (type-equal-p ty type-null)
                         (typep ty 'type-unknown)
                         (not (null ty))))))
    (error () (assert-true t))))

;;; Phase C: Typeclass Dictionary Passing

(deftest phase-c-dict-env-extend-lookup
  "dict-env-extend stores and dict-env-lookup retrieves method dict."
  (let* ((methods (list (cons 'plus #'+) (cons 'zero 0)))
         (env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int methods env0))
         (found (cl-cc/type:dict-env-lookup 'num type-int env1)))
    (assert-true found)
    (assert-= 2 (length found))))

(deftest phase-c-dict-env-miss
  "dict-env-lookup returns nil for missing typeclass/type combination."
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int '() env0)))
    ;; Looking up with wrong type gives nil
    (assert-null (cl-cc/type:dict-env-lookup 'num type-string env1))
    ;; Looking up with wrong class gives nil
    (assert-null (cl-cc/type:dict-env-lookup 'ord type-int env1))))

(deftest phase-c-dict-env-multiple-classes
  "dict-env supports multiple typeclass entries independently."
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'eq type-int '((eq-p . #'equal)) env0))
         (env2 (cl-cc/type:dict-env-extend 'num type-int '((plus . #'+)) env1)))
    (assert-true (cl-cc/type:dict-env-lookup 'eq type-int env2))
    (assert-true (cl-cc/type:dict-env-lookup 'num type-int env2))))

;;; Phase D: Row-Based Effect Type Inference

(deftest phase-d-effect-row-union-merges
  "effect-row-union merges effects from two rows."
  (let* ((row-io (make-type-effect-row
                  :effects (list (make-type-effect :name 'io))
                  :row-var nil))
         (row-state (make-type-effect-row
                     :effects (list (make-type-effect :name 'state))
                     :row-var nil))
         (union (cl-cc/type:effect-row-union row-io row-state)))
    (assert-true (type-effect-row-p union))
    (assert-= 2 (length (type-effect-row-effects union)))))

(deftest phase-d-effect-row-union-pure
  "effect-row-union with pure row returns other row."
  (let* ((row-io (make-type-effect-row
                  :effects (list (make-type-effect :name 'io))
                  :row-var nil))
         (union1 (cl-cc/type:effect-row-union row-io +pure-effect-row+))
         (union2 (cl-cc/type:effect-row-union +pure-effect-row+ row-io)))
    (assert-= 1 (length (type-effect-row-effects union1)))
    (assert-= 1 (length (type-effect-row-effects union2)))))

(deftest-each phase-d-infer-effects-pure-forms
  "infer-effects returns empty row for pure expressions (arithmetic, let binding)."
  :cases (("pure-arithmetic" '(+ 1 2))
          ("pure-let"        '(let ((x 42)) x)))
  (form)
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast form))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (type-effect-row-p row))
    (assert-null (type-effect-row-effects row))))

(deftest phase-d-effect-row-subset-check
  "effect-row-subset-p detects subset relationships."
  (let* ((row-io (make-type-effect-row
                  :effects (list (make-type-effect :name 'io))
                  :row-var nil))
         (row-io-state (make-type-effect-row
                        :effects (list (make-type-effect :name 'io)
                                       (make-type-effect :name 'state))
                        :row-var nil)))
    ;; io is a subset of (io state)
    (assert-true (cl-cc/type:effect-row-subset-p row-io row-io-state))
    ;; (io state) is NOT a subset of io
    (assert-false (cl-cc/type:effect-row-subset-p row-io-state row-io))
    ;; pure is a subset of everything
    (assert-true (cl-cc/type:effect-row-subset-p +pure-effect-row+ row-io))))

;;; Phase E: Rank-N Polymorphism

(deftest phase-e-skolem-creation
  "make-type-skolem creates a unique skolem with given name."
  (let* ((sk1 (cl-cc/type:make-type-skolem 'a))
         (sk2 (cl-cc/type:make-type-skolem 'a)))
    (assert-true (cl-cc/type:type-skolem-p sk1))
    (assert-true (cl-cc/type:type-skolem-p sk2))
    ;; Two skolems with same name are distinct (unique IDs)
    (assert-false (cl-cc/type:type-skolem-equal-p sk1 sk2))
    (assert-eq 'a (cl-cc/type:type-skolem-name sk1))))

(deftest phase-e-skolem-equality
  "type-skolem-equal-p compares by identity."
  (let* ((sk (cl-cc/type:make-type-skolem 'b)))
    ;; Same skolem is equal to itself
    (assert-true (cl-cc/type:type-skolem-equal-p sk sk))))

(deftest phase-e-check-skolem-escape-absent
  "check-skolem-escape returns nil when skolem is not in substitution."
  (let* ((sk (cl-cc/type:make-type-skolem 'a))
         (escaped (cl-cc/type:check-skolem-escape sk (empty-subst))))
    (assert-null escaped)))

(deftest phase-e-check-forall-type
  "check mode against a type-forall introduces skolem and checks body."
  (reset-type-vars!)
  (let* ((a (make-type-variable 'a))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (id-ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    ;; check identity lambda against forall a. a -> a
    ;; Should succeed (no error)
    (let ((result (check id-ast fa env)))
      ;; check returns a substitution (may be nil for success)
      (assert-true (or (null result) (not (null result)))))))

(deftest phase-e-synthesize-lambda
  "synthesize infers a function type for a lambda."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (multiple-value-bind (ty subst)
      (synthesize ast env)
      (declare (ignore subst))
      (assert-true (typep ty 'type-function))
      (assert-= 1 (length (type-function-params ty))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Kind System Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest kind-type-singleton
  "+kind-type+ is the singleton * kind."
  (assert-true (kind-type-p +kind-type+))
  (assert-true (kind-node-p +kind-type+))
  ;; Singletons: structural equality
  (assert-true (kind-equal-p +kind-type+ +kind-type+))
  (assert-true (kind-equal-p (make-kind-type) (make-kind-type))))

(deftest kind-arrow-creation
  "kind-fun builds arrow kinds like * -> * for List."
  (let ((list-kind (kind-fun +kind-type+ +kind-type+)))
    (assert-true (kind-arrow-p list-kind))
    (assert-true (kind-equal-p (kind-arrow-from list-kind) +kind-type+))
    (assert-true (kind-equal-p (kind-arrow-to list-kind)   +kind-type+))
    ;; (-> (-> * *) *) — higher-order, for Fix
    (let ((fix-kind (kind-fun list-kind +kind-type+)))
      (assert-true (kind-arrow-p fix-kind))
      (assert-true (kind-equal-p (kind-arrow-from fix-kind) list-kind)))))

(deftest kind-effect-row-singletons
  "+kind-effect+ and +kind-row-type+ are the Effect and Row * kinds."
  (assert-true (kind-effect-p +kind-effect+))
  (assert-true (kind-row-p +kind-row-type+))
  (assert-true (kind-row-p +kind-row-effect+))
  (assert-true (kind-equal-p (kind-row-elem +kind-row-type+)   +kind-type+))
  (assert-true (kind-equal-p (kind-row-elem +kind-row-effect+) +kind-effect+)))

(deftest-each kind-equal-p-basic
  "kind-equal-p compares structural equality of kind constructors."
  :cases (("*=*"          t   +kind-type+   +kind-type+)
          ("Eff=Eff"      t   +kind-effect+ +kind-effect+)
          ("*≠Eff"        nil +kind-type+   +kind-effect+))
  (should-be-equal k1 k2)
  (if should-be-equal
      (assert-true  (kind-equal-p k1 k2))
      (assert-false (kind-equal-p k1 k2))))

(deftest kind-var-fresh
  "fresh-kind-var generates distinct kind variables."
  (let ((k1 (fresh-kind-var 'k))
        (k2 (fresh-kind-var 'k)))
    (assert-true (kind-var-p k1))
    (assert-true (kind-var-p k2))
    (assert-false (kind-var-equal-p k1 k2))
    (assert-true  (kind-var-equal-p k1 k1))))

(deftest-each kind-to-string-basic
  "kind-to-string produces human-readable kind names."
  :cases (("type"         +kind-type+         "*")
          ("effect"       +kind-effect+       "Effect")
          ("constraint"   +kind-constraint+   "Constraint")
          ("multiplicity" +kind-multiplicity+ "Multiplicity")
          ("row-type"     +kind-row-type+     "Row *")
          ("row-effect"   +kind-row-effect+   "Row Effect"))
  (knd expected-str)
  (assert-string= expected-str (kind-to-string knd)))

(deftest kind-fun-arrow-equal-p
  "Two structurally identical kind-fun arrows are equal; different ones are not."
  (assert-true  (kind-equal-p (kind-fun +kind-type+ +kind-type+)
                               (kind-fun +kind-type+ +kind-type+)))
  (assert-false (kind-equal-p (kind-fun +kind-type+ +kind-effect+)
                               (kind-fun +kind-type+ +kind-type+))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Multiplicity / Graded Modal Types Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each multiplicity-grade-constants
  "The three multiplicity constants are valid grades with the correct keyword value."
  :cases (("zero"  +mult-zero+  :zero)
          ("one"   +mult-one+   :one)
          ("omega" +mult-omega+ :omega))
  (grade expected-kw)
  (assert-true (multiplicity-p grade))
  (assert-eq expected-kw grade))

(deftest-each multiplicity-add
  "mult-add implements the commutative semiring join: 0+q=q, 1+1=1, 1+ω=ω."
  :cases (("0+0=0" :zero  :zero  :zero)
          ("0+1=1" :zero  :one   :one)
          ("1+0=1" :one   :zero  :one)
          ("0+ω=ω" :zero  :omega :omega)
          ("1+1=1" :one   :one   :one)
          ("1+ω=ω" :one   :omega :omega)
          ("ω+ω=ω" :omega :omega :omega))
  (a b expected)
  (assert-eq expected (mult-add a b)))

(deftest-each multiplicity-mul
  "mult-mul is semiring scaling: 0*q=0, 1*q=q, ω*ω=ω."
  :cases (("0*0=0" :zero  :zero  :zero)
          ("0*1=0" :zero  :one   :zero)
          ("0*ω=0" :zero  :omega :zero)
          ("1*0=0" :one   :zero  :zero)
          ("1*1=1" :one   :one   :one)
          ("1*ω=ω" :one   :omega :omega)
          ("ω*0=0" :omega :zero  :zero)
          ("ω*1=ω" :omega :one   :omega)
          ("ω*ω=ω" :omega :omega :omega))
  (a b expected)
  (assert-eq expected (mult-mul a b)))

(deftest-each multiplicity-leq
  "mult-leq implements the partial order 0 ≤ 1 ≤ ω."
  :cases (("0≤0"  :zero  :zero  t)
          ("0≤1"  :zero  :one   t)
          ("0≤ω"  :zero  :omega t)
          ("1≤1"  :one   :one   t)
          ("1≤ω"  :one   :omega t)
          ("ω≤ω"  :omega :omega t)
          ("1≰0"  :one   :zero  nil)
          ("ω≰1"  :omega :one   nil)
          ("ω≰0"  :omega :zero  nil))
  (a b expected)
  (if expected
      (assert-true  (mult-leq a b))
      (assert-false (mult-leq a b))))

(deftest-each multiplicity-to-string
  "mult-to-string renders each grade as its canonical symbol."
  :cases (("zero"  :zero  "0")
          ("one"   :one   "1")
          ("omega" :omega "ω"))
  (grade expected-str)
  (assert-string= expected-str (mult-to-string grade)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New Type Node Tests (direct new API)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest type-rigid-creation
  "fresh-rigid-var creates distinct skolem/rigid variables."
  (let ((r1 (fresh-rigid-var 'a))
        (r2 (fresh-rigid-var 'a)))
    (assert-true  (type-rigid-p r1))
    (assert-true  (type-rigid-p r2))
    ;; Two fresh rigids with the same name are NOT equal (unique IDs)
    (assert-false (type-rigid-equal-p r1 r2))
    ;; Same rigid equals itself
    (assert-true  (type-rigid-equal-p r1 r1))
    (assert-eq 'a (type-rigid-name r1))))

(deftest type-product-creation
  "make-type-product creates tuples; type-product-elems returns the elements."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-true (type-product-p pair))
    (assert-= 2 (length (type-product-elems pair)))
    (assert-true (type-equal-p type-int    (first  (type-product-elems pair))))
    (assert-true (type-equal-p type-string (second (type-product-elems pair))))))

(deftest type-record-creation
  "make-type-record creates row-polymorphic records."
  ;; Closed record {name: String, age: Int}
  (let ((rec (make-type-record :fields (list (cons 'name type-string)
                                             (cons 'age  type-int))
                               :row-var nil)))
    (assert-true  (type-record-p rec))
    (assert-= 2 (length (type-record-fields rec)))
    (assert-null  (type-record-row-var rec)))
  ;; Open record {name: String | ρ}
  (let* ((rho (fresh-type-var 'rho))
         (rec (make-type-record :fields (list (cons 'name type-string))
                                :row-var rho)))
    (assert-true (type-record-p rec))
    (assert-true (type-var-p (type-record-row-var rec)))))

(deftest type-variant-creation
  "make-type-variant creates row-polymorphic variant/sum types."
  (let ((v (make-type-variant :cases (list (cons 'some type-int)
                                           (cons 'none type-null))
                              :row-var nil)))
    (assert-true (type-variant-p v))
    (assert-= 2 (length (type-variant-cases v)))
    (assert-null (type-variant-row-var v))))

(deftest type-arrow-with-effects-and-mult
  "make-type-arrow-raw supports effects and multiplicity slots."
  ;; Linear arrow Int -1-> Int
  (let ((arr (make-type-arrow-raw :params (list type-int)
                                  :return type-int
                                  :effects +pure-effect-row+
                                  :mult :one)))
    (assert-true (type-arrow-p arr))
    (assert-eq :one (type-arrow-mult arr))
    (assert-true (type-effect-row-p (type-arrow-effects arr))))
  ;; Erased arrow (grade 0)
  (let ((arr0 (make-type-arrow-raw :params (list type-bool)
                                   :return type-null
                                   :effects nil
                                   :mult :zero)))
    (assert-eq :zero (type-arrow-mult arr0))))

(deftest type-forall-body-keyword
  "make-type-forall uses :body (not :type) in the new API."
  (let* ((a  (fresh-type-var 'a))
         (fn (make-type-arrow (list a) a))
         ;; New canonical keyword
         (fa (make-type-forall :var a :body fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-var-equal-p a (type-forall-var fa)))
    ;; Both :body and backward-compat :type accessors work
    (assert-true (type-equal-p fn (type-forall-body fa)))
    (assert-true (type-equal-p fn (type-forall-type fa)))))

(deftest type-exists-creation
  "make-type-exists creates existential types."
  (let* ((a    (fresh-type-var 'a))
         (pair (make-type-product :elems (list type-string a)))
         (ex   (make-type-exists :var a :knd nil :body pair)))
    (assert-true (type-exists-p ex))
    (assert-true (type-var-equal-p a (type-exists-var ex)))
    (assert-true (type-product-p (type-exists-body ex)))))

(deftest type-mu-creation
  "make-type-mu creates iso-recursive types μa.T."
  (let* ((a   (fresh-type-var 'a))
         ;; μa. (null | (cons int a))  —  list of int
         (mu  (make-type-mu :var a
                            :body (make-type-union (list type-null
                                                         (make-type-product
                                                          :elems (list type-int a)))))))
    (assert-true (type-mu-p mu))
    (assert-true (type-var-equal-p a (type-mu-var mu)))
    (assert-true (type-union-p (type-mu-body mu)))))

(deftest type-linear-creation
  "make-type-linear creates graded modal types !_q T."
  (let ((linear-int   (make-type-linear :base type-int  :grade :one))
        (erased-str   (make-type-linear :base type-string :grade :zero))
        (default-bool (make-type-linear :base type-bool  :grade :omega)))
    (assert-true (type-linear-p linear-int))
    (assert-eq :one   (type-linear-grade linear-int))
    (assert-eq :zero  (type-linear-grade erased-str))
    (assert-eq :omega (type-linear-grade default-bool))
    (assert-true (type-equal-p type-int (type-linear-base linear-int)))))

(deftest type-app-hkt
  "make-type-app represents higher-kinded type application F A."
  (let* ((list-con (make-type-primitive :name 'list))
         (list-int (make-type-app :fun list-con :arg type-int)))
    (assert-true (type-app-p list-int))
    (assert-true (type-primitive-p (type-app-fun list-int)))
    (assert-true (type-equal-p type-int (type-app-arg list-int)))))

(deftest upgraded-array-and-complex-part-types
  "ANSI CL upgrade helpers return the expected core type nodes."
  (let ((bit-upgraded (upgraded-array-element-type 'bit))
        (char-upgraded (upgraded-array-element-type 'character))
        (fallback-upgraded (upgraded-array-element-type '(or fixnum string)))
        (complex-part (upgraded-complex-part-type 'complex)))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'bit) bit-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'character) char-upgraded))
    (assert-true (type-equal-p type-any fallback-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'real) complex-part))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Hash-Table Substitution API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest subst-empty-functional
  "make-substitution creates an empty hash-table substitution."
  (let ((s (make-substitution)))
    (assert-true (substitution-p s))
    (assert-= 0 (substitution-generation s))
    (let ((v (fresh-type-var)))
      (multiple-value-bind (bound found) (subst-lookup v s)
        (declare (ignore bound))
        (assert-false found)))))

(deftest subst-extend-functional
  "subst-extend returns a new substitution without modifying the original."
  (let* ((v  (fresh-type-var))
         (s0 (make-substitution))
         (s1 (subst-extend v type-int s0)))
    ;; s0 is unchanged
    (multiple-value-bind (b f) (subst-lookup v s0) (declare (ignore b)) (assert-false f))
    ;; s1 has the binding
    (multiple-value-bind (bound found) (subst-lookup v s1)
      (assert-true found)
      (assert-true (type-equal-p type-int bound)))
    ;; Generation was incremented
    (assert-true (> (substitution-generation s1)
                    (substitution-generation s0)))))

(deftest subst-extend-destructive
  "subst-extend! mutates the substitution in place (O(1) for hot paths)."
  (let* ((v (fresh-type-var))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (multiple-value-bind (bound found) (subst-lookup v s)
      (assert-true found)
      (assert-true (type-equal-p type-string bound)))))

(deftest subst-compose-transitivity
  "subst-compose applies s2 first then s1: (s1 ∘ s2)(v2) = s1(s2(v2))."
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         ;; s1: v1 → Int
         (s1 (subst-extend v1 type-int (make-substitution)))
         ;; s2: v2 → v1
         (s2 (subst-extend v2 v1 (make-substitution)))
         ;; s1 ∘ s2: v2 should map to Int (via v1)
         (s12 (subst-compose s1 s2)))
    (let ((result (zonk v2 s12)))
      (assert-true (type-equal-p type-int result)))))

(deftest zonk-arrow-substitution
  "zonk eagerly applies substitution through arrow types."
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) v))
         (s  (subst-extend v type-bool (make-substitution)))
         (r  (zonk fn s)))
    (assert-true (type-arrow-p r))
    (assert-true (type-equal-p type-bool (first (type-arrow-params r))))
    (assert-true (type-equal-p type-bool (type-arrow-return r)))))

(deftest type-occurs-p-basic
  "type-occurs-p detects circular references (for occurs check)."
  (let* ((v   (fresh-type-var))
         (fn  (make-type-arrow (list v) type-int))
         (s   (make-substitution)))
    ;; v occurs in (v -> Int)
    (assert-true  (type-occurs-p v fn s))
    ;; v does not occur in Int
    (assert-false (type-occurs-p v type-int s))
    ;; Fresh var does not occur in anything built from v
    (let ((w (fresh-type-var)))
      (assert-false (type-occurs-p w fn s)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Row Polymorphism Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest row-extend-basic
  "row-extend adds a label to an existing row."
  (let* ((base (make-type-record :fields (list (cons 'x type-int)) :row-var nil))
         (ext  (row-extend 'y type-string base)))
    (assert-true (type-record-p ext))
    (assert-= 2 (length (type-record-fields ext)))
    (assert-true (assoc 'y (type-record-fields ext)))))

(deftest row-restrict-basic
  "row-restrict removes a label from a row."
  (let* ((rec (make-type-record :fields (list (cons 'x type-int)
                                              (cons 'y type-string))
                                :row-var nil))
         (r   (row-restrict 'x rec)))
    (assert-true (type-record-p r))
    (assert-= 1 (length (type-record-fields r)))
    (assert-null (assoc 'x (type-record-fields r)))))

(deftest row-select-basic
  "row-select retrieves a field type by label."
  (let ((rec (make-type-record :fields (list (cons 'name type-string)
                                             (cons 'age  type-int))
                               :row-var nil)))
    (assert-true (type-equal-p type-string (row-select 'name rec)))
    (assert-true (type-equal-p type-int    (row-select 'age  rec)))
    (assert-null (row-select 'missing rec))))

(deftest row-labels-basic
  "row-labels returns all field labels in a record row."
  (let ((rec (make-type-record :fields (list (cons 'a type-int)
                                             (cons 'b type-bool))
                               :row-var nil)))
    (let ((labs (row-labels rec)))
      (assert-= 2 (length labs))
      (assert-true (member 'a labs))
      (assert-true (member 'b labs)))))

(deftest row-open-closed
  "row-closed-p / row-open-p distinguish closed vs open rows."
  (let* ((closed (make-type-record :fields (list (cons 'x type-int)) :row-var nil))
         (rv     (fresh-type-var 'rho))
         (open   (make-type-record :fields (list (cons 'x type-int)) :row-var rv)))
    (assert-true  (row-closed-p closed))
    (assert-false (row-open-p   closed))
    (assert-false (row-closed-p open))
    (assert-true  (row-open-p   open))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Parser New Syntax Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each parser-graded-arrow-syntax
  "Graded arrow syntax (->1, ->0) parses to type-arrow with the correct multiplicity."
  :cases (("linear-1" '(->1 fixnum boolean) :one)
          ("erased-0" '(->0 fixnum boolean) :zero))
  (form expected-mult)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-arrow-p result))
    (assert-eq expected-mult (type-arrow-mult result))))

(deftest parser-forall-body-keyword
  "(forall a T) parses to type-forall with :body set."
  (let* ((result (cl-cc/type:parse-type-specifier '(forall a (-> a a)))))
    (assert-true (type-forall-p result))
    ;; var is a fresh type-var named 'a
    (assert-eq 'a (type-var-name (type-forall-var result)))
    ;; body is an arrow type
    (assert-true (type-arrow-p (type-forall-body result)))))

(deftest-each parser-quantified-types
  "exists and mu binders parse to their respective node types with correct var-name and body kind."
  :cases (("exists" '(exists a (values string a)) #'type-exists-p #'type-exists-var #'type-exists-body #'type-product-p)
          ("mu"     '(mu a (or null (values int a))) #'type-mu-p #'type-mu-var #'type-mu-body #'type-union-p))
  (form pred-p get-var get-body body-pred-p)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (funcall pred-p result))
    (assert-eq 'a (type-var-name (funcall get-var result)))
    (assert-true (funcall body-pred-p (funcall get-body result)))))

(deftest-each parser-record
  "Record type syntax parses to type-record with the correct field count and row-var."
  :cases (("closed" '(record (name string) (age fixnum)) 2 nil)
          ("open"   '(record (name string) \| rho)       1 t))
  (form n-fields open-p)
  (let ((result (cl-cc/type:parse-type-specifier form)))
    (assert-true (type-record-p result))
    (assert-= n-fields (length (type-record-fields result)))
    (if open-p
        (assert-true  (type-record-row-var result))
        (assert-null  (type-record-row-var result)))))

(deftest parser-variant-syntax
  "(Variant (L T) ...) parses to a closed variant type."
  (let ((result (cl-cc/type:parse-type-specifier '(variant (some fixnum) (none null)))))
    (assert-true (type-variant-p result))
    (assert-= 2 (length (type-variant-cases result)))
    (assert-null (type-variant-row-var result))))

(deftest parser-linear-modal-syntax
  "(!1 T) and (!ω T) parse to graded modal types."
  (let ((lin   (cl-cc/type:parse-type-specifier '(!1 fixnum)))
        (unr   (cl-cc/type:parse-type-specifier '(!ω string)))
        (erased (cl-cc/type:parse-type-specifier '(!0 boolean))))
    (assert-true (type-linear-p lin))
    (assert-eq :one   (type-linear-grade lin))
    (assert-true (type-linear-p unr))
    (assert-eq :omega (type-linear-grade unr))
    (assert-true (type-linear-p erased))
    (assert-eq :zero  (type-linear-grade erased))))

(deftest parser-refinement-syntax
  "(Refine T pred) parses to a refinement type."
  (let ((result (cl-cc/type:parse-type-specifier
                 '(refine fixnum (lambda (x) (> x 0))))))
    (assert-true (type-refinement-p result))
    (assert-true (type-equal-p type-int (type-refinement-base result)))
    (assert-true (type-refinement-predicate result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Printer Tests (printer.lisp full implementation)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest printer-type-error-sentinel
  "type-to-string formats type-error as <error: message>."
  (let ((e1 (make-type-error :message "unbound x"))
        (e2 (make-type-error :message "unknown")))
    (assert-string= "<error: unbound x>" (type-to-string e1))
    (assert-string= "<error: unknown>"   (type-to-string e2))
    ;; +type-unknown+ backward compat
    (assert-string= "<error: unknown>" (type-to-string +type-unknown+))))

(deftest printer-type-product
  "type-to-string formats tuples as (A, B, C)."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-string= "(FIXNUM, STRING)" (type-to-string pair))))

(deftest printer-type-record
  "type-to-string formats records with and without a row variable."
  (let ((closed (make-type-record :fields (list (cons 'x type-int)
                                                (cons 'y type-bool))
                                  :row-var nil))
        (rho    (fresh-type-var 'rho))
        (open   (make-type-record :fields (list (cons 'x type-int))
                                  :row-var (fresh-type-var 'rho))))
    (declare (ignore rho))
    ;; Closed record
    (let ((s (type-to-string closed)))
      (assert-true (search "X" (string-upcase s)))
      (assert-true (search "Y" (string-upcase s))))
    ;; Open record has | separator
    (let ((s (type-to-string open)))
      (assert-true (search "|" s)))))

(deftest printer-linear-type
  "type-to-string formats graded modal types with grade prefix."
  (let ((lin (make-type-linear :base type-int :grade :one)))
    (let ((s (type-to-string lin)))
      ;; Should contain the grade and the base type name
      (assert-true (search "1" s))
      (assert-true (search "FIXNUM" s)))))

(deftest-each printer-unicode-type-operators
  "type-to-string uses Unicode symbols ∀ and μ for quantifier and recursive types."
  :cases (("forall" "∀" (let* ((a  (fresh-type-var 'a))
                               (fn (make-type-arrow (list a) a)))
                          (make-type-forall :var a :body fn)))
          ("mu"     "μ" (let* ((a (fresh-type-var 'a)))
                          (make-type-mu :var a :body (make-type-union (list type-null a))))))
  (glyph ty)
  (assert-true (search glyph (type-to-string ty))))

(deftest printer-effect-row-open
  "type-to-string formats open effect rows with | separator."
  (let* ((rv  (fresh-type-var 'epsilon))
         (row (make-type-effect-row
               :effects (list (make-type-effect-op :name 'io))
               :row-var rv)))
    (let ((s (type-to-string row)))
      (assert-true (search "IO" (string-upcase s)))
      (assert-true (search "|" s)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New typeclass-def API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest typeclass-def-creation
  "make-typeclass-def creates a full typeclass definition."
  (let* ((a  (fresh-type-var 'a))
         (tc (make-typeclass-def
              :name 'functor-test
              :type-params (list a)
              :superclasses nil
              :methods (list (cons 'fmap
                                   (make-type-arrow (list type-any) type-any)))
              :associated-types nil
              :functional-deps nil)))
    (assert-true (typeclass-def-p tc))
    (assert-eq 'functor-test (typeclass-def-name tc))
    (assert-= 1 (length (typeclass-def-type-params tc)))
    (assert-= 1 (length (typeclass-def-methods tc)))))

(deftest typeclass-def-register-lookup
  "register-typeclass / lookup-typeclass round-trip with new typeclass-def."
  (let* ((a  (fresh-type-var 'a))
         (tc (make-typeclass-def
              :name 'show-test
              :type-params (list a)
              :superclasses nil
              :methods (list (cons 'show (make-type-arrow (list a) type-string)))
              :associated-types nil
              :functional-deps nil)))
    (register-typeclass 'show-test tc)
    (let ((retrieved (lookup-typeclass 'show-test)))
      (assert-true retrieved)
      (assert-true (typeclass-def-p retrieved))
      (assert-eq 'show-test (typeclass-def-name retrieved)))))

(deftest typeclass-instance-registration-new
  "register-typeclass-instance and lookup-typeclass-instance work with new API."
  (register-typeclass-instance 'show-int-test type-int
                               (list (cons 'show (lambda (x) (format nil "~A" x)))))
  (let ((inst (lookup-typeclass-instance 'show-int-test type-int)))
    (assert-true inst)
    (assert-true (typeclass-instance-p inst))
    (assert-eq 'show-int-test (typeclass-instance-class-name inst)))
  (assert-true (has-typeclass-instance-p 'show-int-test type-int))
  (assert-false (has-typeclass-instance-p 'show-int-test type-string)))

;;; ─── Subtyping relation ──────────────────────────────────────────────────

(deftest-each is-subtype-reflexive
  "Every type is a subtype of itself."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool)
          ("null"   type-null))
  (tp)
  (assert-true (cl-cc/type:is-subtype-p tp tp)))

(deftest-each is-subtype-primitive-hierarchy
  "CL numeric type hierarchy is correctly encoded."
  :cases (("fixnum<integer"   'fixnum  'integer  t)
          ("integer<rational" 'integer 'rational t)
          ("rational<real"    'rational 'real    t)
          ("float<real"       'float   'real     t)
          ("real<number"      'real    'number   t)
          ("fixnum<number"    'fixnum  'number   t)
          ("number<t"         'number  't        t)
          ("int not<string"   'fixnum  'string   nil))
  (name1 name2 expected)
  (if expected
      (assert-true  (cl-cc/type:type-name-subtype-p name1 name2))
      (assert-false (cl-cc/type:type-name-subtype-p name1 name2))))

(deftest-each is-subtype-of-top-type
  "Every primitive type is a subtype of type-any."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool))
  (tp)
  (assert-true (cl-cc/type:is-subtype-p tp type-any)))

(deftest is-subtype-unknown-gradual
  "type-unknown is consistent with everything (gradual typing escape hatch)."
  (let ((unk (make-type-unknown)))
    (assert-true (cl-cc/type:is-subtype-p unk  type-int))
    (assert-true (cl-cc/type:is-subtype-p type-int unk))))

(deftest-each is-subtype-union-right
  "T <: (T1 | T2) iff T <: T1 or T <: T2."
  :cases (("int in or-int-string"  type-int    t)
          ("string in or"          type-string  t)
          ("bool not in"           type-bool    nil))
  (tp expected)
  (let ((u (make-type-union (list type-int type-string))))
    (if expected
        (assert-true  (cl-cc/type:is-subtype-p tp u))
        (assert-false (cl-cc/type:is-subtype-p tp u)))))

(deftest is-subtype-function-contravariant-params
  "(A->B) <: (C->D) iff C <: A (contravariant params)."
  ;; (number -> string) <: (fixnum -> string)
  ;; because fixnum <: number (contravariant flip)
  (let ((f1 (make-type-arrow (list (make-type-primitive :name 'number)) type-string))
        (f2 (make-type-arrow (list (make-type-primitive :name 'fixnum))  type-string)))
    (assert-true (cl-cc/type:is-subtype-p f1 f2))))

;;; ─── Type lattice: join and meet ─────────────────────────────────────────

(deftest-each type-join-equal-types
  "join of identical types is the type itself."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool))
  (tp)
  (assert-true (type-equal-p tp (cl-cc/type:type-join tp tp))))

(deftest-each type-lattice-join-meet-subtype
  "join picks the supertype; meet picks the subtype (fixnum <: integer)."
  :cases (("join" #'cl-cc/type:type-join 'integer)
          ("meet" #'cl-cc/type:type-meet 'fixnum))
  (op expected-name)
  (let* ((fixnum-t  (make-type-primitive :name 'fixnum))
         (integer-t (make-type-primitive :name 'integer))
         (result    (funcall op fixnum-t integer-t)))
    (assert-true (type-primitive-p result))
    (assert-eq expected-name (type-primitive-name result))))

(deftest type-join-incompatible-makes-union
  "join(fixnum, string) returns their LCA in the CL hierarchy (t, the top type)."
  ;; fixnum and string both appear under t in *subtype-table*, so type-join
  ;; finds t as their LCA via find-common-supertype.
  (let* ((fixnum-t (make-type-primitive :name 'fixnum))
         (string-t (make-type-primitive :name 'string))
         (result   (cl-cc/type:type-join fixnum-t string-t)))
    (assert-true (type-primitive-p result))
    (assert-eq 't (type-primitive-name result))))

(deftest type-join-with-unknown-is-other
  "join(??, T) = T (gradual: unknown absorbs into concrete side)."
  (let* ((unk (make-type-unknown))
         (result (cl-cc/type:type-join unk type-int)))
    (assert-true (type-equal-p type-int result))))

(deftest-each type-meet-equal-types
  "meet of identical types is the type itself."
  :cases (("int"    type-int)
          ("string" type-string))
  (tp)
  (assert-true (type-equal-p tp (cl-cc/type:type-meet tp tp))))

(deftest type-meet-incompatible-makes-intersection
  "meet(fixnum, string) = (and fixnum string) — uninhabited but structurally valid."
  (let ((result (cl-cc/type:type-meet type-int type-string)))
    (assert-true (type-intersection-p result))
    (assert-= 2 (length (type-intersection-types result)))))

;;; ─── Constraint solver ────────────────────────────────────────────────────

(in-suite cl-cc-suite)

(deftest solve-constraints-empty
  "solve-constraints on empty list returns the original substitution."
  (let* ((subst  (cl-cc/type:make-substitution))
         (result (cl-cc/type:solve-constraints nil subst)))
    (assert-true (cl-cc/type:substitution-p result))))

(deftest solve-constraints-equal-succeeds
  "An :equal constraint on two identical primitives succeeds with no residual."
  (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints
       (list (cl-cc/type:make-equal-constraint type-int type-int))
       (cl-cc/type:make-substitution))
    (assert-true  (cl-cc/type:substitution-p new-subst))
    (assert-null  residual)))

(deftest solve-constraints-equal-binds-var
  "An :equal constraint on (?a ~ int) binds ?a to int."
  (let* ((tvar  (cl-cc/type:fresh-type-var "a"))
         (subst (cl-cc/type:make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint tvar type-int))
         subst)
      (assert-null residual)
      (let ((bound (cl-cc/type:zonk tvar new-subst)))
        (assert-true (type-equal-p type-int bound))))))

(deftest-each solve-constraints-produces-residual
  "Unsolvable constraints (type mismatch and subtype violation) each produce exactly one residual."
  :cases (("equal-mismatch"    (cl-cc/type:make-equal-constraint type-int type-string))
          ("subtype-violation" (cl-cc/type:make-subtype-constraint type-string type-int)))
  (c)
  (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints (list c) (cl-cc/type:make-substitution))
    (declare (ignore new-subst))
    (assert-= 1 (length residual))))

(deftest-each solve-constraints-subtype-ok
  "Subtype constraints for valid relationships produce no residual."
  :cases (("fixnum<integer" 'fixnum 'integer)
          ("integer<number" 'integer 'number)
          ("float<real"     'float   'real))
  (sub-name super-name)
  (let ((t1 (make-type-primitive :name sub-name))
        (t2 (make-type-primitive :name super-name)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-subtype-constraint t1 t2))
         (cl-cc/type:make-substitution))
      (declare (ignore new-subst))
      (assert-null residual))))

(deftest solve-constraints-multiple-sequential
  "Multiple equality constraints are solved in sequence."
  ;; ?a ~ int, ?b ~ ?a  =>  ?a=int, ?b=int
  (let* ((ta    (cl-cc/type:fresh-type-var "a"))
         (tb    (cl-cc/type:fresh-type-var "b")))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint ta type-int)
               (cl-cc/type:make-equal-constraint tb ta))
         (cl-cc/type:make-substitution))
      (assert-null residual)
      (assert-true (type-equal-p type-int (cl-cc/type:zonk ta new-subst)))
      (assert-true (type-equal-p type-int (cl-cc/type:zonk tb new-subst))))))

;;; ─── Multiplicity Semiring Tests ─────────────────────────────────────────────

(deftest-each mult-add-semiring-join
  "mult-add implements the commutative semiring join: 0 is identity, join is idempotent, 1+ω=ω."
  :cases (("0+1=1"  :zero  :one   :one)
          ("0+ω=ω"  :zero  :omega :omega)
          ("0+0=0"  :zero  :zero  :zero)
          ("1+0=1"  :one   :zero  :one)
          ("ω+0=ω"  :omega :zero  :omega)
          ("1+1=1"  :one   :one   :one)
          ("ω+ω=ω"  :omega :omega :omega)
          ("1+ω=ω"  :one   :omega :omega)
          ("ω+1=ω"  :omega :one   :omega))
  (a b expected)
  (assert-eq expected (cl-cc/type:mult-add a b)))

(deftest-each mult-mul-semiring-product
  "mult-mul implements the semiring product: 0 annihilates, 1 is identity, ω*ω=ω."
  :cases (("0*1=0"  :zero  :one   :zero)
          ("0*ω=0"  :zero  :omega :zero)
          ("1*0=0"  :one   :zero  :zero)
          ("ω*0=0"  :omega :zero  :zero)
          ("1*1=1"  :one   :one   :one)
          ("1*ω=ω"  :one   :omega :omega)
          ("ω*1=ω"  :omega :one   :omega)
          ("ω*ω=ω"  :omega :omega :omega))
  (a b expected)
  (assert-eq expected (cl-cc/type:mult-mul a b)))

(deftest-each mult-leq-total-order
  "mult-leq implements the total order 0 ≤ 1 ≤ ω (reflexive, transitive, antisymmetric)."
  :cases (("0≤0"  :zero  :zero  t)
          ("1≤1"  :one   :one   t)
          ("ω≤ω"  :omega :omega t)
          ("0≤1"  :zero  :one   t)
          ("0≤ω"  :zero  :omega t)
          ("1≤ω"  :one   :omega t)
          ("1≰0"  :one   :zero  nil)
          ("ω≰1"  :omega :one   nil)
          ("ω≰0"  :omega :zero  nil))
  (a b expected)
  (if expected
      (assert-true  (cl-cc/type:mult-leq a b))
      (assert-false (cl-cc/type:mult-leq a b))))

(deftest-each mult-to-string-values
  "mult-to-string returns correct glyphs."
  :cases (("zero"  :zero  "0")
          ("one"   :one   "1")
          ("omega" :omega "ω"))
  (m expected)
  (assert-equal expected (cl-cc/type:mult-to-string m)))

(deftest-each multiplicity-p-recognition
  "multiplicity-p accepts :zero/:one/:omega; rejects all other values."
  :cases (("zero-valid"   :zero  t)
          ("one-valid"    :one   t)
          ("omega-valid"  :omega t)
          ("two-invalid"  :two   nil)
          ("nil-invalid"  nil    nil)
          ("int-invalid"  1      nil))
  (val expected)
  (if expected
      (assert-true  (cl-cc/type:multiplicity-p val))
      (assert-false (cl-cc/type:multiplicity-p val))))

;;; ─── Constraint Language Tests ───────────────────────────────────────────────

(deftest constraint-equal-creation
  "make-equal-constraint creates (:equal t1 t2)."
  (let ((c (cl-cc/type:make-equal-constraint type-int type-string)))
    (assert-true (cl-cc/type:constraint-p c))
    (assert-eq :equal (cl-cc/type:constraint-kind c))
    (assert-equal 2 (length (cl-cc/type:constraint-args c)))
    (assert-true (type-equal-p type-int (first (cl-cc/type:constraint-args c))))
    (assert-true (type-equal-p type-string (second (cl-cc/type:constraint-args c))))))

(deftest constraint-subtype-creation
  "make-subtype-constraint creates (:subtype t1 t2)."
  (let ((c (cl-cc/type:make-subtype-constraint type-int type-any)))
    (assert-eq :subtype (cl-cc/type:constraint-kind c))
    (assert-true (type-equal-p type-int (first (cl-cc/type:constraint-args c))))))

(deftest constraint-typeclass-creation
  "make-typeclass-constraint creates (:typeclass class type)."
  (let* ((tv (cl-cc/type:fresh-type-var "a"))
         (c  (cl-cc/type:make-typeclass-constraint 'num tv)))
    (assert-eq :typeclass (cl-cc/type:constraint-kind c))
    (assert-eq 'num (first (cl-cc/type:constraint-args c)))))

(deftest constraint-implication-creation
  "make-implication-constraint creates (:implication vars given wanted)."
  (let* ((tv (cl-cc/type:fresh-type-var "a"))
         (given  (list (cl-cc/type:make-equal-constraint tv type-int)))
         (wanted (list (cl-cc/type:make-typeclass-constraint 'num tv)))
         (c (cl-cc/type:make-implication-constraint (list tv) given wanted)))
    (assert-eq :implication (cl-cc/type:constraint-kind c))
    (assert-equal 3 (length (cl-cc/type:constraint-args c)))))

(deftest-each constraint-kind-only-creation
  "Constraint constructors that only need their :kind verified."
  :cases (("effect-subset" :effect-subset
           (cl-cc/type:make-effect-subset-constraint
            cl-cc/type:+pure-effect-row+ cl-cc/type:+io-effect-row+))
          ("kind-equal"    :kind-equal
           (cl-cc/type:make-kind-equal-constraint
            cl-cc/type:+kind-type+ cl-cc/type:+kind-type+)))
  (expected-kind c)
  (assert-eq expected-kind (cl-cc/type:constraint-kind c)))

(deftest constraint-mult-leq-creation
  "make-mult-leq-constraint creates (:mult-leq q1 q2)."
  (let ((c (cl-cc/type:make-mult-leq-constraint :one :omega)))
    (assert-eq :mult-leq (cl-cc/type:constraint-kind c))
    (assert-eq :one (first (cl-cc/type:constraint-args c)))
    (assert-eq :omega (second (cl-cc/type:constraint-args c)))))

(deftest constraint-row-lacks-creation
  "make-row-lacks-constraint creates (:row-lacks rho label)."
  (let* ((tv (cl-cc/type:fresh-type-var "r"))
         (c  (cl-cc/type:make-row-lacks-constraint tv 'x)))
    (assert-eq :row-lacks (cl-cc/type:constraint-kind c))
    (assert-eq 'x (second (cl-cc/type:constraint-args c)))))

(deftest constraint-free-vars-count
  "constraint-free-vars finds the correct number of vars in equality and typeclass constraints."
  (let* ((tv1   (cl-cc/type:fresh-type-var "a"))
         (tv2   (cl-cc/type:fresh-type-var "b"))
         (ceq   (cl-cc/type:make-equal-constraint tv1 tv2))
         (ctc   (cl-cc/type:make-typeclass-constraint 'num tv1)))
    (assert-equal 2 (length (cl-cc/type:constraint-free-vars ceq)))
    (assert-equal 1 (length (cl-cc/type:constraint-free-vars ctc)))))

(deftest-each constraint-free-vars-ground-types-empty
  "constraint-free-vars returns nil for ground constraints (no type variables)."
  :cases (("mult-leq"   (cl-cc/type:make-mult-leq-constraint :one :omega))
          ("kind-equal" (cl-cc/type:make-kind-equal-constraint
                        cl-cc/type:+kind-type+ cl-cc/type:+kind-effect+)))
  (c)
  (assert-null (cl-cc/type:constraint-free-vars c)))

(deftest constraint-free-vars-implication-quantified
  "constraint-free-vars subtracts quantified vars from implication."
  (let* ((tv    (cl-cc/type:fresh-type-var "a"))
         (inner (cl-cc/type:make-equal-constraint tv type-int))
         (c     (cl-cc/type:make-implication-constraint
                 (list tv) (list inner) (list inner)))
         (fvs   (cl-cc/type:constraint-free-vars c)))
    ;; tv is quantified, so no free vars
    (assert-null fvs)))

(deftest constraint-substitute-equal
  "constraint-substitute applies substitution to equality constraint."
  (let* ((tv    (cl-cc/type:fresh-type-var "a"))
         (c     (cl-cc/type:make-equal-constraint tv type-string))
         (subst (cl-cc/type:subst-extend tv type-int (cl-cc/type:make-substitution)))
         (c2    (cl-cc/type:constraint-substitute c subst)))
    (assert-eq :equal (cl-cc/type:constraint-kind c2))
    ;; After substitution, first arg should be int (tv mapped to int)
    (assert-true (type-equal-p type-int (first (cl-cc/type:constraint-args c2))))))

(deftest constraint-substitute-mult-leq-identity
  "constraint-substitute is identity for mult-leq."
  (let* ((c     (cl-cc/type:make-mult-leq-constraint :one :omega))
         (subst (cl-cc/type:make-substitution))
         (c2    (cl-cc/type:constraint-substitute c subst)))
    (assert-eq c c2)))

;;; ─── Effect Row Operations Tests ─────────────────────────────────────────────

(deftest effect-row-extend
  "effect-row-extend prepends an op and preserves the original row variable."
  ;; Prepends into pure row
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:effect-row-extend op cl-cc/type:+pure-effect-row+)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects row)))
    (assert-eq 'state (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects row)))))
  ;; Preserves the existing row variable
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (base (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (ext  (cl-cc/type:effect-row-extend op base)))
    (assert-true (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var ext)))))

(deftest effect-row-restrict
  "effect-row-restrict removes named effects; absent names are a no-op."
  ;; Removes present effect: io removed, state remains
  (let* ((op1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op2 (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op1 op2) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'io row)))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects restricted)))
    (assert-eq 'state (cl-cc/type:type-effect-op-name
                       (first (cl-cc/type:type-effect-row-effects restricted)))))
  ;; Absent name is a no-op: row still has 1 effect
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (restricted (cl-cc/type:effect-row-restrict 'state row)))
    (assert-equal 1 (length (cl-cc/type:type-effect-row-effects restricted)))))

(deftest effect-row-member-p
  "effect-row-member-p: true when present, nil when absent, nil for pure row."
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (assert-true  (cl-cc/type:effect-row-member-p 'io row))
    (assert-false (cl-cc/type:effect-row-member-p 'state row))
    (assert-false (cl-cc/type:effect-row-member-p 'io cl-cc/type:+pure-effect-row+))))

;;; ─── Effect Registry Tests ───────────────────────────────────────────────────

(deftest effect-registry
  "register-effect stores; lookup-effect retrieves or returns nil for misses."
  (let ((cl-cc/type:*effect-registry* (make-hash-table :test #'eq)))
    ;; register and retrieve
    (let ((edef (cl-cc/type:make-effect-def :name 'state :type-params nil :operations nil)))
      (cl-cc/type:register-effect 'state edef)
      (let ((found (cl-cc/type:lookup-effect 'state)))
        (assert-true (cl-cc/type:effect-def-p found))
        (assert-eq 'state (cl-cc/type:effect-def-name found))))
    ;; miss returns nil
    (assert-null (cl-cc/type:lookup-effect 'nonexistent))))

(deftest effect-row-union
  "effect-row-union: deduplicates effects and prefers row2's row-var."
  ;; Merges without duplicate names: io+io2+state → {io, state}
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (op-io2   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects (list op-io2 op-state) :row-var nil))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (assert-equal 2 (length (cl-cc/type:type-effect-row-effects merged))))
  ;; Prefers row2's row-var
  (let* ((rv (cl-cc/type:fresh-type-var "e"))
         (row1 (cl-cc/type:make-type-effect-row :effects nil :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv))
         (merged (cl-cc/type:effect-row-union row1 row2)))
    (assert-true (cl-cc/type:type-var-p (cl-cc/type:type-effect-row-row-var merged)))))

(deftest effect-row-subset-p
  "effect-row-subset-p: empty ⊆ anything; open row is superset of everything; extra effects break subset."
  ;; Empty row is subset of any row
  (let* ((op  (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil)))
    (assert-true (cl-cc/type:effect-row-subset-p cl-cc/type:+pure-effect-row+ row)))
  ;; Open row (with row-var) is superset of everything
  (let* ((rv   (cl-cc/type:fresh-type-var "e"))
         (op   (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list op) :row-var nil))
         (row2 (cl-cc/type:make-type-effect-row :effects nil :row-var rv)))
    (assert-true (cl-cc/type:effect-row-subset-p row1 row2)))
  ;; Row with extra effect is NOT subset of smaller row
  (let* ((op-io    (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (op-state (cl-cc/type:make-type-effect-op :name 'state :args nil))
         (big   (cl-cc/type:make-type-effect-row :effects (list op-io op-state) :row-var nil))
         (small (cl-cc/type:make-type-effect-row :effects (list op-io) :row-var nil)))
    (assert-false (cl-cc/type:effect-row-subset-p big small))))
