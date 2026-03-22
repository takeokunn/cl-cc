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

(test type-repr-singleton-primitives
  "Test that singleton type instances exist and are type-primitive."
  (is (typep type-int 'type-primitive))
  (is (typep type-string 'type-primitive))
  (is (typep type-bool 'type-primitive))
  (is (typep type-symbol 'type-primitive))
  (is (typep type-null 'type-primitive))
  (is (typep type-any 'type-primitive)))

(test type-repr-primitive-names
  "Test that primitive types have the expected names."
  (is (eq 'fixnum (type-primitive-name type-int)))
  (is (eq 'string (type-primitive-name type-string)))
  (is (eq 'boolean (type-primitive-name type-bool)))
  (is (eq 'symbol (type-primitive-name type-symbol)))
  (is (eq 'null (type-primitive-name type-null)))
  (is (eq 't (type-primitive-name type-any))))

(test type-repr-variable-creation
  "Test that type variables can be created with unique IDs."
  (let ((v1 (make-type-variable 'a))
        (v2 (make-type-variable 'b)))
    (is (typep v1 'type-variable))
    (is (typep v2 'type-variable))
    (is (not (= (type-variable-id v1) (type-variable-id v2))))
    (is (eq 'a (type-variable-name v1)))
    (is (eq 'b (type-variable-name v2)))))

(test type-repr-function-type
  "Test function type construction and accessors."
  (let ((fn-type (make-type-function-raw
                                :params (list type-int type-int)
                                :return type-int)))
    (is (typep fn-type 'type-function))
    (is (= 2 (length (type-function-params fn-type))))
    (is (eq type-int (type-function-return fn-type)))))

(test type-repr-type-equal-p
  "Test type-equal-p for various types."
  ;; Primitives
  (is (not (null (type-equal-p type-int type-int))))
  (is (not (type-equal-p type-int type-string)))
  ;; Variables
  (let ((v1 (make-type-variable)))
    (is (not (null (type-equal-p v1 v1)))))
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
    (is (not (null (type-equal-p fn1 fn2))))
    (is (not (type-equal-p fn1 fn3)))))

(test type-repr-type-to-string
  "Test type-to-string for various types."
  (is (string= "FIXNUM" (type-to-string type-int)))
  (is (string= "STRING" (type-to-string type-string)))
  (is (string= "BOOLEAN" (type-to-string type-bool)))
  (is (string= "?" (type-to-string +type-unknown+)))
  ;; Function type
  (let ((fn (make-type-function-raw
                           :params (list type-int)
                           :return type-int)))
    (is (string= "FIXNUM -> FIXNUM" (type-to-string fn)))))

(test type-repr-unknown-type
  "Test that unknown type singleton exists and is a type-unknown."
  (is (typep +type-unknown+ 'type-unknown))
  (is (not (null (type-equal-p +type-unknown+ +type-unknown+)))))

;;; Unification Tests

(test unify-primitive-same
  "Test that same primitive types unify successfully."
  (multiple-value-bind (subst ok) (type-unify type-int type-int)
    (declare (ignore subst))
    (is (not (null ok)))))

(test unify-primitive-different
  "Test that different primitive types fail to unify."
  (multiple-value-bind (subst ok) (type-unify type-int type-string)
    (declare (ignore subst))
    (is (not ok))))

(test unify-variable-with-primitive
  "Test that a type variable unifies with a primitive type."
  (let ((v (make-type-variable)))
    (multiple-value-bind (result ok) (type-unify v type-int)
      (is (not (null ok)))
      ;; The substitution should bind v to type-int
      (multiple-value-bind (binding found) (subst-lookup v result)
        (is (not (null found)))
        (is (not (null (type-equal-p binding type-int))))))))

(test unify-variable-with-variable
  "Test that two different type variables unify."
  (let ((v1 (make-type-variable))
        (v2 (make-type-variable)))
    (multiple-value-bind (subst ok) (type-unify v1 v2)
      (declare (ignore subst))
      (is (not (null ok))))))

(test unify-same-variable
  "Test that a type variable unifies with itself."
  (let ((v (make-type-variable)))
    (multiple-value-bind (subst ok) (type-unify v v)
      (declare (ignore subst))
      (is (not (null ok))))))

(test unify-function-types-structural
  "Test that function types unify structurally."
  (let* ((v (make-type-variable))
         (fn1 (make-type-function-raw
                             :params (list v)
                             :return type-int))
         (fn2 (make-type-function-raw
                             :params (list type-string)
                             :return type-int)))
    (multiple-value-bind (result ok) (type-unify fn1 fn2)
      (is (not (null ok)))
      ;; v should be bound to type-string
      (multiple-value-bind (binding found) (subst-lookup v result)
        (is (not (null found)))
        (is (not (null (type-equal-p binding type-string))))))))

(test unify-function-types-arity-mismatch
  "Test that function types with different arities fail to unify."
  (let ((fn1 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn2 (make-type-function-raw
                            :params (list type-int type-int)
                            :return type-int)))
    (multiple-value-bind (subst ok) (type-unify fn1 fn2)
      (declare (ignore subst))
      (is (not ok)))))

(test unify-occurs-check
  "Test that occurs check prevents infinite types."
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v)
                            :return type-int)))
    ;; Trying to unify v with a type containing v should fail
    (multiple-value-bind (subst ok) (type-unify v fn)
      (declare (ignore subst))
      (is (not ok)))))

(test unify-lists-success
  "Test that type-unify-lists works for matching lists."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int type-string)
                                                     (list type-int type-string)
                                                     nil)
    (declare (ignore subst))
    (is (not (null ok)))))

(test unify-lists-failure
  "Test that type-unify-lists fails for non-matching lists."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int type-string)
                                                     (list type-string type-int)
                                                     nil)
    (declare (ignore subst))
    (is (not ok))))

(test unify-lists-length-mismatch
  "Test that type-unify-lists fails for lists of different lengths."
  (multiple-value-bind (subst ok) (type-unify-lists (list type-int)
                                                     (list type-int type-string)
                                                     nil)
    (declare (ignore subst))
    (is (not ok))))

(test unify-unknown-with-anything
  "Test that unknown type unifies with any type."
  (multiple-value-bind (s1 ok1) (type-unify +type-unknown+ type-int)
    (declare (ignore s1))
    (is (not (null ok1))))
  (multiple-value-bind (s2 ok2) (type-unify type-string +type-unknown+)
    (declare (ignore s2))
    (is (not (null ok2))))
  (multiple-value-bind (s3 ok3) (type-unify +type-unknown+ +type-unknown+)
    (declare (ignore s3))
    (is (not (null ok3)))))

(test unify-substitution-composition
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
      (is (not (null (type-equal-p result type-int)))))))

(test unify-transitive-binding
  "Test that transitive variable bindings are resolved."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable)))
    ;; Unify v1 with v2, then v2 with type-int
    (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
      (is (not (null ok1)))
      (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
        (is (not (null ok2)))
        ;; v1 should resolve to type-int through v2
        (let ((result (type-substitute v1 subst2)))
          (is (not (null (type-equal-p result type-int)))))))))

;;; Type Inference Tests

(test infer-integer-literal
  "Test that integer literals infer to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '42)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-binop-addition
  "Test that binary addition infers to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(+ 1 2))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-binop-nested
  "Test that nested binary operations infer to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(+ (* 2 3) (- 4 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-variable-from-env
  "Test that variables are looked up in the type environment."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast 'x))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-unbound-variable-error
  "Test that unbound variables signal an error."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast 'undefined-var)))
    (signals unbound-variable-error
      (infer-with-env ast))))

(test infer-let-binding
  "Test that let bindings infer types correctly."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 42)) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-let-binding-with-binop
  "Test that let bindings with binary operations work."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((x 10) (y 20)) (+ x y)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-lambda-identity
  "Test that lambda type is inferred as a function type."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (typep ty 'type-function))
      (is (= 1 (length (type-function-params ty)))))))

(test infer-lambda-arithmetic
  "Test that lambda with arithmetic infers int -> int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (typep ty 'type-function))
      (is (not (null (type-equal-p (type-function-return ty) type-int))))
      ;; Parameter should be constrained to int
      (let ((param-type (first (type-function-params ty))))
        (is (not (null (type-equal-p param-type type-int))))))))

(test infer-function-call
  "Test that function application infers the return type."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(let ((f (lambda (x) (+ x 1)))) (f 5)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-if-expression
  "Test that if expressions unify branches."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
         (env (type-env-extend 'cond-var
                               (type-to-scheme type-bool)
                               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-print-returns-expr-type
  "Test that print returns the type of the printed expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-progn-returns-last
  "Test that progn returns the type of the last expression."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(progn 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test infer-quote-symbol
  "Test that quoted symbol infers to type-symbol."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(quote hello))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-symbol)))))))

(test infer-quote-integer
  "Test that quoted integer infers to type-int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(quote 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

;;; Generalization / Instantiation Tests

(test generalize-no-env-vars
  "Test that generalize quantifies all free variables."
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw
                                 :params (list v)
                                 :return v))
         (scheme (generalize nil fn-type)))
    (is (typep scheme 'type-scheme))
    ;; v should be quantified since no env vars
    (is (= 1 (length (type-scheme-quantified-vars scheme))))))

(test generalize-preserves-env-vars
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
    (is (= 1 (length (type-scheme-quantified-vars scheme))))
    (is (not (null (type-variable-equal-p
                    (first (type-scheme-quantified-vars scheme))
                    v2))))))

(test instantiate-creates-fresh-vars
  "Test that instantiation creates fresh type variables."
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw
                                 :params (list v)
                                 :return v))
         (scheme (make-type-scheme (list v) fn-type))
         (inst (instantiate scheme)))
    (is (typep inst 'type-function))
    ;; The instantiated type should have fresh variables (not the original v)
    (let ((new-param (first (type-function-params inst)))
          (new-ret (type-function-return inst)))
      (is (typep new-param 'type-variable))
      (is (not (type-variable-equal-p new-param v)))
      ;; Param and return should be the same fresh variable
      (is (not (null (type-variable-equal-p new-param new-ret)))))))

(test let-polymorphism-identity
  "Test let-polymorphism: identity function used at different types."
  (reset-type-vars!)
  ;; (let ((id (lambda (x) x))) (id 42))
  ;; id should be polymorphic: forall a. a -> a
  ;; When applied to 42, result should be int
  (let ((ast (lower-sexp-to-ast '(let ((id (lambda (x) x))) (id 42)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (is (not (null (type-equal-p ty type-int)))))))

(test type-scheme-monomorphic
  "Test that type-to-scheme creates a monomorphic scheme."
  (let ((scheme (type-to-scheme type-int)))
    (is (typep scheme 'type-scheme))
    (is (null (type-scheme-quantified-vars scheme)))
    (is (not (null (type-equal-p (type-scheme-type scheme) type-int))))))

;;; Free Variables Tests

(test free-vars-primitive
  "Test that primitives have no free variables."
  (is (null (type-free-vars type-int)))
  (is (null (type-free-vars type-string))))

(test free-vars-variable
  "Test that a type variable has itself as a free variable."
  (let* ((v (make-type-variable))
         (fv (type-free-vars v)))
    (is (= 1 (length fv)))
    (is (not (null (type-variable-equal-p (first fv) v))))))

(test free-vars-function
  "Test free variables in function types."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v1)
                            :return v2))
         (fv (type-free-vars fn)))
    (is (= 2 (length fv)))))

;;; Substitution Tests

(test substitution-empty
  "Test that empty substitution leaves types unchanged."
  (is (eq type-int (type-substitute type-int (empty-subst)))))

(test substitution-variable-bound
  "Test that bound variables are replaced."
  (let* ((v (make-type-variable))
         (subst (extend-subst v type-int (empty-subst)))
         (result (type-substitute v subst)))
    (is (not (null (type-equal-p result type-int))))))

(test substitution-variable-unbound
  "Test that unbound variables are unchanged."
  (let* ((v (make-type-variable))
         (result (type-substitute v (empty-subst))))
    (is (not (null (type-variable-equal-p result v))))))

(test substitution-function-type
  "Test substitution through function types."
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v)
                            :return v))
         (subst (extend-subst v type-int (empty-subst)))
         (result (type-substitute fn subst)))
    (is (typep result 'type-function))
    (is (not (null (type-equal-p (first (type-function-params result)) type-int))))
    (is (not (null (type-equal-p (type-function-return result) type-int))))))

;;; Normalize Type Variables Tests

(test normalize-type-variables-canonical
  "Test that normalize produces canonical variable names."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v1)
                            :return v2))
         (normalized (normalize-type-variables fn)))
    (is (typep normalized 'type-function))
    ;; The param and return should be different canonical variables
    (let ((p (first (type-function-params normalized)))
          (r (type-function-return normalized)))
      (is (typep p 'type-variable))
      (is (typep r 'type-variable))
      (is (not (type-variable-equal-p p r))))))
