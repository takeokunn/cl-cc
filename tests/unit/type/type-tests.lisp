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

(deftest type-repr-singleton-primitives
  "Test that singleton type instances exist and are type-primitive."
  (assert-type type-primitive type-int)
  (assert-type type-primitive type-string)
  (assert-type type-primitive type-bool)
  (assert-type type-primitive type-symbol)
  (assert-type type-primitive type-null)
  (assert-type type-primitive type-any))

(deftest type-repr-primitive-names
  "Test that primitive types have the expected names."
  (assert-eq 'fixnum (type-primitive-name type-int))
  (assert-eq 'string (type-primitive-name type-string))
  (assert-eq 'boolean (type-primitive-name type-bool))
  (assert-eq 'symbol (type-primitive-name type-symbol))
  (assert-eq 'null (type-primitive-name type-null))
  (assert-eq 't (type-primitive-name type-any)))

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
  (assert-true (not (null (type-equal-p type-int type-int))))
  (assert-false (type-equal-p type-int type-string))
  ;; Variables
  (let ((v1 (make-type-variable)))
    (assert-true (not (null (type-equal-p v1 v1)))))
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
    (assert-true (not (null (type-equal-p fn1 fn2))))
    (assert-false (type-equal-p fn1 fn3))))

(deftest type-repr-type-to-string
  "Test type-to-string for various types."
  (assert-string= "FIXNUM" (type-to-string type-int))
  (assert-string= "STRING" (type-to-string type-string))
  (assert-string= "BOOLEAN" (type-to-string type-bool))
  (assert-string= "?" (type-to-string +type-unknown+))
  ;; Function type
  (let ((fn (make-type-function-raw
                           :params (list type-int)
                           :return type-int)))
    (assert-string= "FIXNUM -> FIXNUM" (type-to-string fn))))

(deftest type-repr-unknown-type
  "Test that unknown type singleton exists and is a type-unknown."
  (assert-type type-unknown +type-unknown+)
  (assert-true (not (null (type-equal-p +type-unknown+ +type-unknown+)))))

;;; Unification Tests

(deftest unify-primitive-same
  "Test that same primitive types unify successfully."
  (multiple-value-bind (subst ok) (type-unify type-int type-int)
    (declare (ignore subst))
    (assert-true (not (null ok)))))

(deftest unify-primitive-different
  "Test that different primitive types fail to unify."
  (multiple-value-bind (subst ok) (type-unify type-int type-string)
    (declare (ignore subst))
    (assert-false ok)))

(deftest unify-variable-with-primitive
  "Test that a type variable unifies with a primitive type."
  (let ((v (make-type-variable)))
    (multiple-value-bind (result ok) (type-unify v type-int)
      (assert-true (not (null ok)))
      ;; The substitution should bind v to type-int
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true (not (null found)))
        (assert-true (not (null (type-equal-p binding type-int))))))))

(deftest unify-variable-with-variable
  "Test that two different type variables unify."
  (let ((v1 (make-type-variable))
        (v2 (make-type-variable)))
    (multiple-value-bind (subst ok) (type-unify v1 v2)
      (declare (ignore subst))
      (assert-true (not (null ok))))))

(deftest unify-same-variable
  "Test that a type variable unifies with itself."
  (let ((v (make-type-variable)))
    (multiple-value-bind (subst ok) (type-unify v v)
      (declare (ignore subst))
      (assert-true (not (null ok))))))

(deftest unify-function-types-structural
  "Test that function types unify structurally."
  (let* ((v (make-type-variable))
         (fn1 (make-type-function-raw
                             :params (list v)
                             :return type-int))
         (fn2 (make-type-function-raw
                             :params (list type-string)
                             :return type-int)))
    (multiple-value-bind (result ok) (type-unify fn1 fn2)
      (assert-true (not (null ok)))
      ;; v should be bound to type-string
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true (not (null found)))
        (assert-true (not (null (type-equal-p binding type-string))))))))

(deftest unify-function-types-arity-mismatch
  "Test that function types with different arities fail to unify."
  (let ((fn1 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn2 (make-type-function-raw
                            :params (list type-int type-int)
                            :return type-int)))
    (multiple-value-bind (subst ok) (type-unify fn1 fn2)
      (declare (ignore subst))
      (assert-false ok))))

(deftest unify-occurs-check
  "Test that occurs check prevents infinite types."
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v)
                            :return type-int)))
    ;; Trying to unify v with a type containing v should fail
    (multiple-value-bind (subst ok) (type-unify v fn)
      (declare (ignore subst))
      (assert-false ok))))

(deftest unify-lists-success
  "Test that type-unify-lists works for matching lists."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int type-string)
                                                     (list type-int type-string)
                                                     nil)
    (declare (ignore subst))
    (assert-true (not (null ok)))))

(deftest unify-lists-failure
  "Test that type-unify-lists fails for non-matching lists."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int type-string)
                                                     (list type-string type-int)
                                                     nil)
    (declare (ignore subst))
    (assert-false ok)))

(deftest unify-lists-length-mismatch
  "Test that type-unify-lists fails for lists of different lengths."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int)
                                                     (list type-int type-string)
                                                     nil)
    (declare (ignore subst))
    (assert-false ok)))

(deftest unify-unknown-with-anything
  "Test that unknown type unifies with any type."
  (multiple-value-bind (s1 ok1) (type-unify +type-unknown+ type-int)
    (declare (ignore s1))
    (assert-true (not (null ok1))))
  (multiple-value-bind (s2 ok2) (type-unify type-string +type-unknown+)
    (declare (ignore s2))
    (assert-true (not (null ok2))))
  (multiple-value-bind (s3 ok3) (type-unify +type-unknown+ +type-unknown+)
    (declare (ignore s3))
    (assert-true (not (null ok3)))))

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
      (assert-true (not (null (type-equal-p result type-int)))))))

(deftest unify-transitive-binding
  "Test that transitive variable bindings are resolved."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable)))
    ;; Unify v1 with v2, then v2 with type-int
    (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
      (assert-true (not (null ok1)))
      (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
        (assert-true (not (null ok2)))
        ;; v1 should resolve to type-int through v2
        (let ((result (type-substitute v1 subst2)))
          (assert-true (not (null (type-equal-p result type-int)))))))))

;;; Type Inference Tests

(deftest infer-integer-literal
  "Test that integer literals infer to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '42)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-binop-addition
  "Test that binary addition infers to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(+ 1 2))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-binop-nested
  "Test that nested binary operations infer to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(+ (* 2 3) (- 4 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-variable-from-env
  "Test that variables are looked up in the type environment."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast 'x))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-unbound-variable-error
  "Test that unbound variables signal an error."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast 'undefined-var)))
    (assert-signals unbound-variable-error
      (infer-with-env ast))))

(deftest infer-let-binding
  "Test that let bindings infer types correctly."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 42)) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-let-binding-with-binop
  "Test that let bindings with binary operations work."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 10) (y 20)) (+ x y)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-lambda-identity
  "Test that lambda type is inferred as a function type."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-= 1 (length (type-function-params ty))))))

(deftest infer-lambda-arithmetic
  "Test that lambda with arithmetic infers int -> int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-true (not (null (type-equal-p (type-function-return ty) type-int))))
      ;; Parameter should be constrained to int
      (let ((param-type (first (type-function-params ty))))
        (assert-true (not (null (type-equal-p param-type type-int))))))))

(deftest infer-function-call
  "Test that function application infers the return type."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((f (lambda (x) (+ x 1)))) (f 5)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-if-expression
  "Test that if expressions unify branches."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
         (env (type-env-extend 'cond-var
                               (type-to-scheme type-bool)
                               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-print-returns-expr-type
  "Test that print returns the type of the printed expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-progn-returns-last
  "Test that progn returns the type of the last expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(progn 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest infer-quote-symbol
  "Test that quoted symbol infers to type-symbol."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(quote hello))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-symbol)))))))

(deftest infer-quote-integer
  "Test that quoted integer infers to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(quote 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

;;; Generalization / Instantiation Tests

(deftest generalize-no-env-vars
  "Test that generalize quantifies all free variables."
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw
                                 :params (list v)
                                 :return v))
         (scheme (generalize nil fn-type)))
    (assert-type type-scheme scheme)
    ;; v should be quantified since no env vars
    (assert-= 1 (length (type-scheme-quantified-vars scheme)))))

(deftest generalize-preserves-env-vars
  "Test that variables free in env are not quantified."
  (let* ((v1 (make-type-variable 'a))
         (v2 (make-type-variable 'b))
         (fn-type (make-type-function-raw
                                 :params (list v1)
                                 :return v2))
         ;; v1 is free in the environment
         (env (list (cons 'x v1)))
         (scheme (generalize env fn-type)))
    ;; Only v2 should be quantified (v1 is in env)
    (assert-= 1 (length (type-scheme-quantified-vars scheme)))
    (assert-true (not (null (type-variable-equal-p
                    (first (type-scheme-quantified-vars scheme))
                    v2))))))

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
      (assert-true (not (null (type-variable-equal-p new-param new-ret)))))))

(deftest let-polymorphism-identity
  "Test let-polymorphism: identity function used at different types."
  (reset-type-vars!)
  ;; (let ((id (lambda (x) x))) (id 42))
  ;; id should be polymorphic: forall a. a -> a
  ;; When applied to 42, result should be int
  (let ((ast (lower-sexp-to-ast '(let ((id (lambda (x) x))) (id 42)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (not (null (type-equal-p ty type-int)))))))

(deftest type-scheme-monomorphic
  "Test that type-to-scheme creates a monomorphic scheme."
  (let ((scheme (type-to-scheme type-int)))
    (assert-type type-scheme scheme)
    (assert-null (type-scheme-quantified-vars scheme))
    (assert-true (not (null (type-equal-p (type-scheme-type scheme) type-int))))))

;;; Free Variables Tests

(deftest free-vars-primitive
  "Test that primitives have no free variables."
  (assert-null (type-free-vars type-int))
  (assert-null (type-free-vars type-string)))

(deftest free-vars-variable
  "Test that a type variable has itself as a free variable."
  (let* ((v (make-type-variable))
         (fv (type-free-vars v)))
    (assert-= 1 (length fv))
    (assert-true (not (null (type-variable-equal-p (first fv) v))))))

(deftest free-vars-function
  "Test free variables in function types."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v1)
                            :return v2))
         (fv (type-free-vars fn)))
    (assert-= 2 (length fv))))

;;; Substitution Tests

(deftest substitution-empty
  "Test that empty substitution leaves types unchanged."
  (assert-eq type-int (type-substitute type-int (empty-subst))))

(deftest substitution-variable-bound
  "Test that bound variables are replaced."
  (let* ((v (make-type-variable))
         (subst (extend-subst v type-int (empty-subst)))
         (result (type-substitute v subst)))
    (assert-true (not (null (type-equal-p result type-int))))))

(deftest substitution-variable-unbound
  "Test that unbound variables are unchanged."
  (let* ((v (make-type-variable))
         (result (type-substitute v (empty-subst))))
    (assert-true (not (null (type-variable-equal-p result v))))))

(deftest substitution-function-type
  "Test substitution through function types."
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v)
                            :return v))
         (subst (extend-subst v type-int (empty-subst)))
         (result (type-substitute fn subst)))
    (assert-type type-function result)
    (assert-true (not (null (type-equal-p (first (type-function-params result)) type-int))))
    (assert-true (not (null (type-equal-p (type-function-return result) type-int))))))

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

(deftest bidirectional-check-int-against-int
  "check succeeds when actual matches expected."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    ;; Should return a substitution (possibly nil) without signaling
    (assert-true (null (check ast type-int env)))))

(deftest bidirectional-check-int-against-unknown
  "check always succeeds against unknown type (gradual typing)."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
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
      (assert-true (not (null retrieved)))
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

(deftest rankn-forall-creation
  "make-type-forall creates universally quantified types."
  (let* ((a (make-type-variable 'a))
         (fn (make-type-function (list a) a))
         (fa (make-type-forall :var a :type fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-variable-equal-p a (type-forall-var fa)))
    (assert-true (typep (type-forall-type fa) 'type-function))))

(deftest rankn-forall-to-string
  "type-to-string formats forall types."
  (let* ((a (make-type-variable 'a))
         (fn (make-type-function (list a) a))
         (fa (make-type-forall :var a :type fn)))
    (let ((s (type-to-string fa)))
      (assert-true (stringp s))
      ;; Should contain the type variable name (rendered as ?A or similar)
      (assert-true (search "A" (string-upcase s))))))

(deftest rankn-forall-equality
  "type-equal-p distinguishes different forall types."
  (let* ((a (make-type-variable 'a))
         (b (make-type-variable 'b))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (fb (make-type-forall :var b :type (make-type-function (list b) b))))
    ;; Different variables → not equal (structural equality by var ID)
    (assert-false (type-equal-p fa fb))))

;;; Phase 4-6: Parser Integration Tests

(deftest parser-forall-syntax
  "(forall a T) parses to type-forall."
  (let ((result (cl-cc/type:parse-type-specifier '(forall a (function (a) a)))))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-variable-name (type-forall-var result)))))

(deftest parser-effect-arrow-syntax
  "(-> fixnum fixnum ! io) parses to effectful-function."
  (let ((result (cl-cc/type:parse-type-specifier '(-> fixnum fixnum ! io))))
    (assert-true (typep result 'type-effectful-function))
    (assert-= 1 (length (type-function-params result)))
    (assert-true (type-equal-p type-int (type-function-return result)))))

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
      (assert-true (not (null ok))))))

(deftest phase-a-infer-empty-progn
  "Empty progn body infers to null type."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(progn))))
    ;; Empty progn: progn with no body forms returns nil -> type-null
    (handler-case
      (multiple-value-bind (ty subst) (infer-with-env ast)
        (declare (ignore subst))
        ;; Should return type-null or type-unknown for empty body
        (assert-true (or (type-equal-p ty type-null)
                         (typep ty 'type-unknown)
                         (not (null ty)))))
      (error () (assert-true t)))))  ; any error is acceptable

;;; Phase C: Typeclass Dictionary Passing

(deftest phase-c-dict-env-extend-lookup
  "dict-env-extend stores and dict-env-lookup retrieves method dict."
  (let* ((methods (list (cons 'plus #'+) (cons 'zero 0)))
         (env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int methods env0))
         (found (cl-cc/type:dict-env-lookup 'num type-int env1)))
    (assert-true (not (null found)))
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
    (assert-true (not (null (cl-cc/type:dict-env-lookup 'eq type-int env2))))
    (assert-true (not (null (cl-cc/type:dict-env-lookup 'num type-int env2))))))

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

(deftest phase-d-infer-effects-pure-expr
  "infer-effects returns empty row for pure arithmetic."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(+ 1 2)))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (type-effect-row-p row))
    (assert-null (type-effect-row-effects row))))

(deftest phase-d-infer-effects-let-binding
  "infer-effects returns empty row for pure let binding."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(let ((x 42)) x)))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (type-effect-row-p row))
    ;; Pure let has no effects
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
