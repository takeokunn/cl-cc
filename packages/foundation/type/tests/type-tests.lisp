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
  (let ((v1 (make-type-variable 'a))
        (v2 (make-type-variable 'b)))
    (assert-type type-variable v1)
    (assert-type type-variable v2)
    (assert-false (= (type-variable-id v1) (type-variable-id v2)))
    (assert-eq 'a (type-variable-name v1))
    (assert-eq 'b (type-variable-name v2)))
  ;; Function type: accessors
  (let ((fn-type (make-type-function-raw
                  :params (list type-int type-int)
                  :return type-int)))
    (assert-type type-function fn-type)
    (assert-= 2 (length (type-function-params fn-type)))
    (assert-eq type-int (type-function-return fn-type))))

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
             (let ((v (make-type-variable)))
               (assert-type-equal v v))))
          ("function-equal"
           (lambda ()
             (let ((fn1 (make-type-function-raw :params (list type-int) :return type-int))
                   (fn2 (make-type-function-raw :params (list type-int) :return type-int)))
               (assert-type-equal fn1 fn2))))
          ("function-different"
           (lambda ()
             (let ((fn1 (make-type-function-raw :params (list type-int)    :return type-int))
                   (fn3 (make-type-function-raw :params (list type-string) :return type-int)))
               (assert-false (type-equal-p fn1 fn3)))))
          ("arrow-to-string"
           (lambda ()
             (let ((fn (make-type-function-raw :params (list type-int) :return type-int)))
               (assert-string= "FIXNUM -> FIXNUM" (type-to-string fn))))))
  (verify)
  (funcall verify))

(deftest-each type-repr-primitive-type-to-string
  "type-to-string formats primitive and sentinel types correctly."
  :cases (("int"     "FIXNUM"           type-int)
          ("string"  "STRING"           type-string)
          ("bool"    "BOOLEAN"          type-bool)
          ("unknown" "?"                +type-unknown+))
  (expected type)
  (assert-string= expected (type-to-string type)))


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

(deftest-each unify-advanced-cases
  "Advanced unification: variable binds; structural binding propagates; arity/occurs failures; substitution chains."
  :cases (("variable-binds"
           (lambda ()
             (let ((v (make-type-variable)))
               (multiple-value-bind (result ok) (type-unify v type-int)
                 (assert-true ok)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (assert-true found)
                   (assert-type-equal binding type-int))))
             (assert-unifies (make-type-variable) (make-type-variable))
             (let ((v (make-type-variable)))
               (assert-unifies v v))))
          ("structural-binding"
           (lambda ()
             (let* ((v   (make-type-variable))
                    (fn1 (make-type-function-raw :params (list v) :return type-int))
                    (fn2 (make-type-function-raw :params (list type-string) :return type-int)))
               (multiple-value-bind (result ok) (type-unify fn1 fn2)
                 (assert-true ok)
                 (multiple-value-bind (binding found) (subst-lookup v result)
                   (assert-true found)
                   (assert-type-equal binding type-string))))))
          ("failure-cases"
           (lambda ()
             (assert-not-unifies
              (make-type-function-raw :params (list type-int) :return type-int)
              (make-type-function-raw :params (list type-int type-int) :return type-int))
             (let* ((v  (make-type-variable))
                    (fn (make-type-function-raw :params (list v) :return type-int)))
               (assert-not-unifies v fn))))
          ("subst-chains"
           (lambda ()
             (let* ((v1       (make-type-variable))
                    (v2       (make-type-variable))
                    (s1       (extend-subst v1 type-int (empty-subst)))
                    (s2       (extend-subst v2 v1 (empty-subst)))
                    (composed (compose-subst s1 s2)))
               (assert-type-equal type-int (type-substitute v2 composed)))
             (let* ((v1 (make-type-variable))
                    (v2 (make-type-variable)))
               (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
                 (assert-true ok1)
                 (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
                   (assert-true ok2)
                   (assert-type-equal type-int (type-substitute v1 subst2))))))))
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

(deftest-each unify-unknown-with-anything
  "The type-error sentinel (unknown) unifies with any type."
  :cases (("unknown-int"     +type-unknown+ type-int)
          ("string-unknown"  type-string    +type-unknown+)
          ("unknown-unknown" +type-unknown+ +type-unknown+))
  (a b)
  (assert-unifies a b))

