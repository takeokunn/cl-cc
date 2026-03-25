;;;; tests/unit/type/representation-tests.lisp — Type Representation Tests
;;;;
;;;; Tests for src/type/representation.lisp:
;;;; Advanced type nodes, type-equal-p branches, type-free-vars,
;;;; type-env operations, backward-compat aliases, type-constructor encoding.

(in-package :cl-cc/test)

(defsuite representation-suite :description "Type representation and structural equality tests")

;;; ─── type-equal-p: product ─────────────────────────────────────────────────

(deftest type-equal-product-same
  "Equal products with same elements."
  (let ((p1 (make-type-product :elems (list type-int type-string)))
        (p2 (make-type-product :elems (list type-int type-string))))
    (assert-true (type-equal-p p1 p2))))

(deftest type-equal-product-different-length
  "Products with different lengths are not equal."
  (let ((p1 (make-type-product :elems (list type-int)))
        (p2 (make-type-product :elems (list type-int type-string))))
    (assert-false (type-equal-p p1 p2))))

(deftest type-equal-product-different-elems
  "Products with different elements are not equal."
  (let ((p1 (make-type-product :elems (list type-int type-int)))
        (p2 (make-type-product :elems (list type-int type-string))))
    (assert-false (type-equal-p p1 p2))))

;;; ─── type-equal-p: union / intersection ────────────────────────────────────

(deftest type-equal-union-same
  "Equal unions."
  (let ((u1 (make-type-union (list type-int type-string)))
        (u2 (make-type-union (list type-int type-string))))
    (assert-true (type-equal-p u1 u2))))

(deftest type-equal-intersection-same
  "Equal intersections."
  (let ((i1 (make-type-intersection (list type-int type-any)))
        (i2 (make-type-intersection (list type-int type-any))))
    (assert-true (type-equal-p i1 i2))))

;;; ─── type-equal-p: forall / exists ─────────────────────────────────────────

(deftest type-equal-forall-same
  "Forall types with same var and body are equal."
  (let* ((v (fresh-type-var "a"))
         (f1 (make-type-forall :var v :body (make-type-arrow (list v) type-int)))
         (f2 (make-type-forall :var v :body (make-type-arrow (list v) type-int))))
    (assert-true (type-equal-p f1 f2))))

(deftest type-equal-forall-different-body
  "Forall types with different bodies are not equal."
  (let* ((v (fresh-type-var "a"))
         (f1 (make-type-forall :var v :body type-int))
         (f2 (make-type-forall :var v :body type-string)))
    (assert-false (type-equal-p f1 f2))))

(deftest type-equal-exists-same
  "Exists types with same var and body are equal."
  (let* ((v (fresh-type-var "a"))
         (e1 (make-type-exists :var v :body type-int))
         (e2 (make-type-exists :var v :body type-int)))
    (assert-true (type-equal-p e1 e2))))

;;; ─── type-equal-p: type-app ────────────────────────────────────────────────

(deftest type-equal-app-same
  "Type applications with same fun and arg are equal."
  (let ((a1 (make-type-app :fun type-int :arg type-string))
        (a2 (make-type-app :fun type-int :arg type-string)))
    (assert-true (type-equal-p a1 a2))))

(deftest type-equal-app-different-arg
  "Type applications with different args are not equal."
  (let ((a1 (make-type-app :fun type-int :arg type-string))
        (a2 (make-type-app :fun type-int :arg type-int)))
    (assert-false (type-equal-p a1 a2))))

;;; ─── type-equal-p: type-mu ─────────────────────────────────────────────────

(deftest type-equal-mu-same
  "Mu types with same var and body are equal."
  (let* ((v (fresh-type-var "a"))
         (m1 (make-type-mu :var v :body type-int))
         (m2 (make-type-mu :var v :body type-int)))
    (assert-true (type-equal-p m1 m2))))

;;; ─── type-equal-p: type-linear ─────────────────────────────────────────────

(deftest type-equal-linear-same
  "Linear types with same grade and base are equal."
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l2 (make-type-linear :base type-int :grade :one)))
    (assert-true (type-equal-p l1 l2))))

(deftest type-equal-linear-different-grade
  "Linear types with different grades are not equal."
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l2 (make-type-linear :base type-int :grade :omega)))
    (assert-false (type-equal-p l1 l2))))

;;; ─── type-equal-p: effect-row / effect-op ──────────────────────────────────

(deftest type-equal-effect-row-same
  "Same effect rows are equal."
  (assert-true (type-equal-p +pure-effect-row+ +pure-effect-row+)))

(deftest type-equal-effect-row-different
  "Pure and IO effect rows are not equal."
  (assert-false (type-equal-p +pure-effect-row+ +io-effect-row+)))

(deftest type-equal-effect-op-same
  "Same effect ops are equal."
  (let ((e1 (make-type-effect-op :name 'io :args nil))
        (e2 (make-type-effect-op :name 'io :args nil)))
    (assert-true (type-equal-p e1 e2))))

(deftest type-equal-effect-op-different-name
  "Effect ops with different names are not equal."
  (let ((e1 (make-type-effect-op :name 'io :args nil))
        (e2 (make-type-effect-op :name 'state :args nil)))
    (assert-false (type-equal-p e1 e2))))

;;; ─── type-equal-p: constraint / qualified ──────────────────────────────────

(deftest type-equal-constraint-same
  "Type-constraint nodes with same class and arg are equal."
  (let ((c1 (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
        (c2 (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int)))
    (assert-true (type-equal-p c1 c2))))

(deftest type-equal-constraint-different-class
  "Type-constraint nodes with different class names are not equal."
  (let ((c1 (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
        (c2 (cl-cc/type::make-type-constraint :class-name 'ord :type-arg type-int)))
    (assert-false (type-equal-p c1 c2))))

(deftest type-equal-qualified-same
  "Qualified types with same constraints and body are equal."
  (let* ((tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (q1 (make-type-qualified :constraints (list tc) :body type-int))
         (q2 (make-type-qualified :constraints (list tc) :body type-int)))
    (assert-true (type-equal-p q1 q2))))

;;; ─── type-equal-p: error sentinel ──────────────────────────────────────────

(deftest type-equal-error-never-equal
  "Two type-error nodes are never equal (even same object)."
  (let ((e (make-type-error :message "test")))
    (assert-false (type-equal-p e e))))

(deftest type-equal-error-vs-primitive
  "type-error is not equal to any primitive."
  (assert-false (type-equal-p (make-type-error :message "x") type-int)))

;;; ─── type-equal-p: rigid ───────────────────────────────────────────────────

(deftest type-equal-rigid-same
  "Same rigid var is equal to itself."
  (let ((r (fresh-rigid-var "a")))
    (assert-true (type-equal-p r r))))

(deftest type-equal-rigid-different
  "Different rigid vars are not equal."
  (let ((r1 (fresh-rigid-var "a"))
        (r2 (fresh-rigid-var "b")))
    (assert-false (type-equal-p r1 r2))))

;;; ─── type-free-vars ────────────────────────────────────────────────────────

(deftest free-vars-primitive-empty
  "Primitive types have no free variables."
  (assert-null (type-free-vars type-int)))

(deftest free-vars-single-var
  "A type-var is its own free variable."
  (let* ((v (fresh-type-var "a"))
         (fvs (type-free-vars v)))
    (assert-equal 1 (length fvs))
    (assert-true (type-var-equal-p v (first fvs)))))

(deftest free-vars-arrow
  "Arrow type collects free vars from params and return."
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (arr (make-type-arrow (list v1) v2))
         (fvs (type-free-vars arr)))
    (assert-equal 2 (length fvs))))

(deftest free-vars-forall-binds
  "Forall binds its var, removing it from free vars."
  (let* ((v (fresh-type-var "a"))
         (f (make-type-forall :var v :body v))
         (fvs (type-free-vars f)))
    (assert-null fvs)))

(deftest free-vars-exists-binds
  "Exists binds its var."
  (let* ((v (fresh-type-var "a"))
         (e (make-type-exists :var v :body v))
         (fvs (type-free-vars e)))
    (assert-null fvs)))

(deftest free-vars-mu-binds
  "Mu binds its recursion var."
  (let* ((v (fresh-type-var "a"))
         (m (make-type-mu :var v :body v))
         (fvs (type-free-vars m)))
    (assert-null fvs)))

(deftest free-vars-product
  "Product collects free vars from all elements."
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (p (make-type-product :elems (list v1 type-int v2)))
         (fvs (type-free-vars p)))
    (assert-equal 2 (length fvs))))

(deftest free-vars-record
  "Record collects free vars from fields and row-var."
  (let* ((v (fresh-type-var "a"))
         (rv (fresh-type-var "rho"))
         (r (make-type-record :fields (list (cons 'x v)) :row-var rv))
         (fvs (type-free-vars r)))
    (assert-equal 2 (length fvs))))

(deftest free-vars-variant
  "Variant collects free vars from cases and row-var."
  (let* ((v (fresh-type-var "a"))
         (vr (make-type-variant :cases (list (cons 'x v)) :row-var nil))
         (fvs (type-free-vars vr)))
    (assert-equal 1 (length fvs))))

(deftest free-vars-linear
  "Linear type collects free vars from base."
  (let* ((v (fresh-type-var "a"))
         (l (make-type-linear :base v :grade :one))
         (fvs (type-free-vars l)))
    (assert-equal 1 (length fvs))))

(deftest free-vars-app
  "Type-app collects free vars from fun and arg."
  (let* ((v1 (fresh-type-var "f"))
         (v2 (fresh-type-var "a"))
         (a (make-type-app :fun v1 :arg v2))
         (fvs (type-free-vars a)))
    (assert-equal 2 (length fvs))))

(deftest free-vars-effect-row
  "Effect-row collects free vars from row-var."
  (let* ((rv (fresh-type-var "ε"))
         (er (make-type-effect-row :effects nil :row-var rv))
         (fvs (type-free-vars er)))
    (assert-equal 1 (length fvs))))

(deftest free-vars-qualified
  "Qualified type collects free vars from constraints and body."
  (let* ((v (fresh-type-var "a"))
         (tc (cl-cc/type::make-type-constraint :class-name 'eq :type-arg v))
         (q (make-type-qualified :constraints (list tc) :body v))
         (fvs (type-free-vars q)))
    ;; v appears in both constraint and body but is deduplicated
    (assert-equal 1 (length fvs))))

;;; ─── Type environment operations ───────────────────────────────────────────

(deftest type-env-empty-lookup-fails
  "Lookup in empty env returns nil."
  (let ((env (type-env-empty)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type::type-env-lookup 'x env)
      (assert-null scheme)
      (assert-false found-p))))

(deftest type-env-extend-and-lookup
  "Extend then lookup returns the scheme."
  (let* ((env (type-env-empty))
         (scheme (type-to-scheme type-int))
         (env2 (cl-cc/type::type-env-extend 'x scheme env)))
    (multiple-value-bind (result found-p)
        (cl-cc/type::type-env-lookup 'x env2)
      (assert-true found-p)
      (assert-true (type-equal-p type-int (cl-cc/type::type-scheme-type result))))))

(deftest type-env-extend-star
  "Extend* adds multiple bindings at once."
  (let* ((env (type-env-empty))
         (bindings (list (cons 'x (type-to-scheme type-int))
                         (cons 'y (type-to-scheme type-string))))
         (env2 (cl-cc/type::type-env-extend* bindings env)))
    (assert-true (nth-value 1 (cl-cc/type::type-env-lookup 'x env2)))
    (assert-true (nth-value 1 (cl-cc/type::type-env-lookup 'y env2)))))

(deftest type-env-free-vars
  "type-env-free-vars collects vars from all bindings."
  (let* ((v (fresh-type-var "a"))
         (env (cl-cc/type::type-env-extend 'x (type-to-scheme v) (type-env-empty)))
         (fvs (cl-cc/type::type-env-free-vars env)))
    (assert-equal 1 (length fvs))))

;;; ─── Backward-compat aliases ───────────────────────────────────────────────

(deftest compat-make-type-variable
  "make-type-variable creates a fresh type-var."
  (let ((v (cl-cc/type::make-type-variable 'x)))
    (assert-true (type-var-p v))
    (assert-eq 'x (type-var-name v))))

(deftest compat-type-constructor-roundtrip
  "make-type-constructor and accessors round-trip."
  (let ((tc (cl-cc/type::make-type-constructor 'list (list type-int))))
    (assert-true (cl-cc/type::type-constructor-p tc))
    (assert-eq 'list (cl-cc/type::type-constructor-name tc))
    (assert-equal 1 (length (cl-cc/type::type-constructor-args tc)))))

(deftest compat-type-unknown-singleton
  "type-unknown-p recognizes +type-unknown+."
  (assert-true (type-unknown-p +type-unknown+))
  (assert-false (type-unknown-p type-int)))

(deftest compat-make-type-function-alias
  "make-type-function is an alias for make-type-arrow (pure, omega)."
  (let ((f (make-type-function (list type-int) type-string)))
    (assert-true (type-arrow-p f))
    (assert-eq :omega (type-arrow-mult f))))

;;; ─── type-to-string ────────────────────────────────────────────────────────

(deftest type-to-string-primitive
  "Primitive types print as their CL name."
  (assert-true (stringp (type-to-string type-int))))

(deftest type-to-string-arrow
  "Arrow types include -> in output."
  (let ((s (type-to-string (make-type-arrow (list type-int) type-string))))
    (assert-true (search "->" s))))

(deftest type-to-string-unknown
  "Unknown type prints as a string."
  (let ((s (type-to-string +type-unknown+)))
    (assert-true (stringp s))
    (assert-true (> (length s) 0))))

(deftest type-to-string-nil
  "nil type prints as a string."
  (assert-true (stringp (type-to-string nil))))
