;;;; tests/type-tests.lisp - Type System Tests
;;;;
;;;; Comprehensive tests for the HM type system including:
;;;; - Type representation (primitives, variables, functions)
;;;; - Unification (with occurs check)
;;;; - Type inference (Algorithm W)
;;;; - Generalization and instantiation (let-polymorphism)

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest type-repr-variable-and-function-creation
  "Type variables have unique IDs and names; function types expose params/return."
  ;; Variables: distinct IDs, correct names
  (let ((v1 (fresh-type-var :name 'a))
        (v2 (fresh-type-var :name 'b)))
    (assert-type type-var v1)
    (assert-type type-var v2)
    (assert-false (= (type-var-id v1) (type-var-id v2)))
    (assert-eq 'a (type-var-name v1))
    (assert-eq 'b (type-var-name v2)))
  ;; Function type: accessors
  (let ((fn-type (make-type-arrow-raw
                  :params (list type-int type-int)
                  :return type-int)))
    (assert-type type-arrow fn-type)
    (assert-= 2 (length (type-arrow-params fn-type)))
    (assert-eq type-int (type-arrow-return fn-type))))

(deftest-each type-repr-equality-and-strings
  "type-equal-p for primitives, variables, function types; arrow types format as 'A -> B'."
  :cases (("primitive-same"
           (lambda ()
             (assert-type-equal type-int type-int)))
          ("primitive-distinct"
           (lambda ()
             (assert-false (type-equal-p type-int type-string))))
          ("variable-self"
           (lambda ()
             (let ((v (fresh-type-var)))
               (assert-type-equal v v))))
          ("function-equal"
           (lambda ()
             (let ((fn1 (make-type-arrow-raw :params (list type-int) :return type-int))
                   (fn2 (make-type-arrow-raw :params (list type-int) :return type-int)))
               (assert-type-equal fn1 fn2))))
          ("function-different"
           (lambda ()
             (let ((fn1 (make-type-arrow-raw :params (list type-int)    :return type-int))
                   (fn3 (make-type-arrow-raw :params (list type-string) :return type-int)))
               (assert-false (type-equal-p fn1 fn3)))))
          ("arrow-to-string"
           (lambda ()
             (let ((fn (make-type-arrow-raw :params (list type-int) :return type-int)))
               (assert-string= "FIXNUM -> FIXNUM" (type-to-string fn))))))
  (verify)
  (funcall verify))

(deftest-each type-repr-primitive-type-to-string
  "type-to-string formats primitive and sentinel types correctly."
  :cases (("int"     "FIXNUM"           type-int)
          ("string"  "STRING"           type-string)
          ("bool"    "BOOLEAN"          type-bool)
          ("unknown" "?"                cl-cc/type:+type-unknown+))
  (expected type)
  (assert-string= expected (type-to-string type)))


(deftest type-repr-unknown-type
  "type-unknown remains an error marker, not a structural type alias."
  (assert-false (type-equal-p cl-cc/type:+type-unknown+ cl-cc/type:+type-unknown+))
  (assert-true (type-error-p cl-cc/type:+type-unknown+)))

;;; Unification Tests

(deftest-each unify-primitive
  "Same primitive types unify; different ones do not."
  :cases (("same"      t   type-int type-int)
          ("different" nil type-int type-string))
  (should-unify a b)
  (if should-unify
      (assert-unifies a b)
      (assert-not-unifies a b)))

(deftest-each unify-advanced-cases
  "Advanced unification: variable binds; structural binding propagates; arity/occurs failures; substitution chains."
  :cases (("variable-binds"
           (lambda ()
             (let ((v (fresh-type-var)))
               (multiple-value-bind (result ok) (type-unify v type-int)
                 (assert-true ok)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (assert-true found)
                   (assert-type-equal binding type-int))))
             (assert-unifies (fresh-type-var) (fresh-type-var))
             (let ((v (fresh-type-var)))
               (assert-unifies v v))))
          ("structural-binding"
           (lambda ()
             (let* ((v   (fresh-type-var))
                    (fn1 (make-type-arrow-raw :params (list v) :return type-int))
                    (fn2 (make-type-arrow-raw :params (list type-string) :return type-int)))
               (multiple-value-bind (result ok) (type-unify fn1 fn2)
                 (assert-true ok)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (assert-true found)
                   (assert-type-equal binding type-string))))))
          ("failure-cases"
           (lambda ()
             (assert-not-unifies
              (make-type-arrow-raw :params (list type-int) :return type-int)
              (make-type-arrow-raw :params (list type-int type-int) :return type-int))
             (let* ((v  (fresh-type-var))
                    (fn (make-type-arrow-raw :params (list v) :return type-int)))
               (assert-not-unifies v fn))))
          ("subst-chains"
           (lambda ()
             (let* ((v1       (fresh-type-var))
                    (v2       (fresh-type-var))
                    (s1       (subst-extend v1 type-int (make-substitution)))
                    (s2       (subst-extend v2 v1 (make-substitution)))
                    (composed (subst-compose s1 s2)))
               (assert-type-equal type-int (zonk v2 composed)))
             (let* ((v1 (fresh-type-var))
                    (v2 (fresh-type-var)))
               (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
                 (assert-true ok1)
                 (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
                   (assert-true ok2)
                   (assert-type-equal type-int (zonk v1 subst2))))))))
  (verify)
  (funcall verify))

(deftest-each unify-lists
  "type-unify-lists succeeds on matching lists, fails on type/length mismatch."
  :cases (("success"         t (list type-int type-string) (list type-int type-string))
          ("type-mismatch"   nil (list type-int type-string) (list type-string type-int))
          ("length-mismatch" nil (list type-int) (list type-int type-string)))
  (expected a b)
  (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (assert-equal expected (not (null ok)))))

(deftest-each unify-type-error-fails
  "type-error sentinels do not unify with concrete or unknown types."
  :cases (("unknown-int"     cl-cc/type:+type-unknown+ type-int)
          ("string-unknown"  type-string    cl-cc/type:+type-unknown+)
          ("unknown-unknown" cl-cc/type:+type-unknown+ cl-cc/type:+type-unknown+))
  (a b)
  (assert-not-unifies a b))
