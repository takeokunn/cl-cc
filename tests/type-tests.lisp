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
