;;;; tests/unit/type/representation-tests.lisp — Type Representation Tests
;;;;
;;;; Tests for src/type/representation.lisp:
;;;; Advanced type nodes, type-equal-p branches, type-free-vars,
;;;; type-env operations, and type-constructor encoding.

(in-package :cl-cc/test)

(defsuite representation-suite :description "Type representation and structural equality tests"
  :parent cl-cc-unit-suite)


(in-suite representation-suite)
;;; ─── type-equal-p: product ─────────────────────────────────────────────────

(deftest type-equal-product-same-elements-is-true
  "type-equal-p: two products with the same elements are equal."
  (assert-true (type-equal-p (make-type-product :elems (list type-int type-string))
                              (make-type-product :elems (list type-int type-string)))))

(deftest type-equal-product-different-length-is-false
  "type-equal-p: products with different element counts are not equal."
  (assert-false (type-equal-p (make-type-product :elems (list type-int))
                               (make-type-product :elems (list type-int type-string)))))

(deftest type-equal-product-different-element-is-false
  "type-equal-p: products with the same length but a differing element are not equal."
  (assert-false (type-equal-p (make-type-product :elems (list type-int type-int))
                               (make-type-product :elems (list type-int type-string)))))

(deftest type-equal-forall-same-body-is-true
  "type-equal-p: two forall types with the same var and body are equal."
  (let ((v (fresh-type-var "a")))
    (assert-true (type-equal-p
                  (make-type-forall :var v :body (make-type-arrow (list v) type-int))
                  (make-type-forall :var v :body (make-type-arrow (list v) type-int))))))

(deftest type-equal-forall-different-body-is-false
  "type-equal-p: forall types with different bodies are not equal."
  (let ((v (fresh-type-var "a")))
    (assert-false (type-equal-p
                   (make-type-forall :var v :body type-int)
                   (make-type-forall :var v :body type-string)))))

(deftest type-equal-exists-and-mu-same-is-true
  "type-equal-p: exists and mu types with same var and body are equal."
  (let ((v (fresh-type-var "a")))
    (assert-true (type-equal-p (make-type-exists :var v :body type-int)
                                (make-type-exists :var v :body type-int)))
    (assert-true (type-equal-p (make-type-mu :var v :body type-int)
                                (make-type-mu :var v :body type-int)))))

;;; ─── type-equal-p: union / intersection ────────────────────────────────────

(deftest-each type-equal-union-intersection-same
  "Equal unions and equal intersections are recognized by type-equal-p."
  :cases (("union"        (make-type-union        (list type-int type-string))
                          (make-type-union        (list type-int type-string)))
          ("intersection" (make-type-intersection (list type-int type-any))
                          (make-type-intersection (list type-int type-any))))
  (a b)
  (assert-true (type-equal-p a b)))

;;; ─── type-equal-p: type-app / type-linear ──────────────────────────────────

(deftest type-equal-type-app-same-is-true
  "type-equal-p: two type-apps with identical fun and arg are equal."
  (assert-true (type-equal-p (make-type-app :fun type-int :arg type-string)
                              (make-type-app :fun type-int :arg type-string))))

(deftest type-equal-type-app-different-arg-is-false
  "type-equal-p: type-apps with the same fun but different arg are not equal."
  (assert-false (type-equal-p (make-type-app :fun type-int :arg type-string)
                               (make-type-app :fun type-int :arg type-int))))

(deftest type-equal-linear-same-grade-is-true
  "type-equal-p: linear types with the same base and grade are equal."
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l2 (make-type-linear :base type-int :grade :one)))
    (assert-true (type-equal-p l1 l2))))

(deftest type-equal-linear-different-grade-is-false
  "type-equal-p: linear types with different grades are not equal."
  (let ((l1 (make-type-linear :base type-int :grade :one))
        (l3 (make-type-linear :base type-int :grade :omega)))
    (assert-false (type-equal-p l1 l3))))

;;; ─── type-equal-p: effect-row / effect-op ──────────────────────────────────

(deftest type-equal-effect-rows-and-ops
  "type-equal-p on effect rows (pure/io) and effect ops (same/different name)."
  (assert-true  (type-equal-p +pure-effect-row+ +pure-effect-row+))
  (assert-false (type-equal-p +pure-effect-row+ +io-effect-row+))
  (assert-true  (type-equal-p (make-type-effect-op :name 'io    :args nil)
                               (make-type-effect-op :name 'io    :args nil)))
  (assert-false (type-equal-p (make-type-effect-op :name 'io    :args nil)
                               (make-type-effect-op :name 'state :args nil))))

;;; ─── type-equal-p: constraint / qualified ──────────────────────────────────

(deftest type-equal-constraint-and-qualified
  "type-equal-p on type-constraint (same/different class) and qualified types."
  (assert-true  (type-equal-p (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)
                               (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)))
  (assert-false (type-equal-p (cl-cc/type:make-type-constraint :class-name 'eq  :type-arg type-int)
                               (cl-cc/type:make-type-constraint :class-name 'ord :type-arg type-int)))
  (let* ((tc (cl-cc/type:make-type-constraint :class-name 'eq :type-arg type-int))
         (q1 (make-type-qualified :constraints (list tc) :body type-int))
         (q2 (make-type-qualified :constraints (list tc) :body type-int)))
    (assert-true (type-equal-p q1 q2))))

;;; ─── type-equal-p: error sentinel and rigid vars ────────────────────────────

(deftest type-equal-error-node-never-equal
  "type-error nodes are never equal to anything, not even themselves."
  (let ((e (make-type-error :message "test")))
    (assert-false (type-equal-p e e))
    (assert-false (type-equal-p e type-int))))

(deftest type-equal-rigid-var-equal-only-to-itself
  "type-equal-p: a rigid var is equal to itself but not to a different fresh rigid var."
  (let ((r  (fresh-rigid-var "a"))
        (r1 (fresh-rigid-var "a"))
        (r2 (fresh-rigid-var "b")))
    (assert-true  (type-equal-p r r))
    (assert-false (type-equal-p r1 r2))))

;;; ─── type-free-vars ────────────────────────────────────────────────────────

(deftest free-vars-primitive-has-no-free-vars
  "type-free-vars: a primitive type has no free variables."
  (assert-null (type-free-vars type-int)))

(deftest free-vars-type-var-is-its-own-free-var
  "type-free-vars: a type-var is its own free variable (singleton list)."
  (let* ((v (fresh-type-var "a"))
         (fvs (type-free-vars v)))
    (assert-equal 1 (length fvs))
    (assert-true (type-var-equal-p v (first fvs)))))

(deftest free-vars-binding-forms-remove-bound-var
  "type-free-vars: forall, exists, and mu each remove their bound variable from free vars."
  (let ((v (fresh-type-var "a")))
    (assert-null (type-free-vars (make-type-forall :var v :body v)))
    (assert-null (type-free-vars (make-type-exists :var v :body v)))
    (assert-null (type-free-vars (make-type-mu :var v :body v)))))

(deftest-each free-vars-count-cases
  "type-free-vars returns the correct count of free variables for each type form."
  :cases (("arrow"      (lambda ()
                          (let ((v1 (fresh-type-var "a")) (v2 (fresh-type-var "b")))
                            (make-type-arrow (list v1) v2)))
           2)
          ("product"    (lambda ()
                          (let ((v1 (fresh-type-var "a")) (v2 (fresh-type-var "b")))
                            (make-type-product :elems (list v1 type-int v2))))
           2)
          ("record"     (lambda ()
                          (let ((v (fresh-type-var "a")) (rv (fresh-type-var "rho")))
                            (make-type-record :fields (list (cons 'x v)) :row-var rv)))
           2)
          ("variant"    (lambda ()
                          (let ((v (fresh-type-var "a")))
                            (make-type-variant :cases (list (cons 'x v)) :row-var nil)))
           1)
          ("linear"     (lambda ()
                          (let ((v (fresh-type-var "a")))
                            (make-type-linear :base v :grade :one)))
           1)
          ("type-app"   (lambda ()
                          (let ((v1 (fresh-type-var "f")) (v2 (fresh-type-var "a")))
                            (make-type-app :fun v1 :arg v2)))
           2)
          ("effect-row" (lambda ()
                          (let ((rv (fresh-type-var "ε")))
                            (make-type-effect-row :effects nil :row-var rv)))
           1)
          ("qualified"  (lambda ()
                          (let* ((v (fresh-type-var "a"))
                                 (tc (cl-cc/type:make-type-constraint :class-name 'eq :type-arg v)))
                            (make-type-qualified :constraints (list tc) :body v)))
           1))
  (make-ty expected-count)
  (assert-equal expected-count (length (type-free-vars (funcall make-ty)))))

;;; ─── Type environment operations ───────────────────────────────────────────

(deftest type-env-lookup-in-empty-returns-nil
  "type-env-lookup in empty env returns nil scheme and found-p=nil."
  (let ((env (type-env-empty)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type:type-env-lookup 'x env)
      (assert-null scheme)
      (assert-false found-p))))

(deftest type-env-extend-and-lookup-finds-scheme
  "type-env-extend + type-env-lookup round-trips the bound scheme."
  (let* ((env  (type-env-empty))
         (env2 (cl-cc/type:type-env-extend 'x (type-to-scheme type-int) env)))
    (multiple-value-bind (result found-p)
        (cl-cc/type:type-env-lookup 'x env2)
      (assert-true found-p)
      (assert-true (type-equal-p type-int (cl-cc/type:type-scheme-type result))))))

(deftest type-env-extend*-adds-multiple-bindings
  "type-env-extend* adds all bindings from an alist; both x and y are findable."
  (let* ((env (type-env-empty))
         (bindings (list (cons 'x (type-to-scheme type-int))
                         (cons 'y (type-to-scheme type-string))))
         (env2 (cl-cc/type:type-env-extend* bindings env)))
    (assert-true (nth-value 1 (cl-cc/type:type-env-lookup 'x env2)))
    (assert-true (nth-value 1 (cl-cc/type:type-env-lookup 'y env2)))))

(deftest type-env-free-vars-collects-from-bindings
  "type-env-free-vars returns the free vars from all bound schemes."
  (let* ((v (fresh-type-var "a"))
         (env (cl-cc/type:type-env-extend 'x (type-to-scheme v) (type-env-empty)))
         (fvs (cl-cc/type:type-env-free-vars env)))
    (assert-equal 1 (length fvs))))

;;; ─── Constructor and printing cases ────────────────────────────────────────

(deftest type-var-constructor-name-is-preserved
  "fresh-type-var preserves the name symbol and returns a type-var-p."
  (let ((v (cl-cc/type:fresh-type-var 'x)))
    (assert-true (type-var-p v))
    (assert-eq 'x (type-var-name v))))

(deftest type-constructor-name-and-args
  "make-type-constructor stores name and args accessible via their readers."
  (let ((tc (cl-cc/type:make-type-constructor 'list (list type-int))))
    (assert-true (cl-cc/type:type-constructor-p tc))
    (assert-eq 'list (cl-cc/type:type-constructor-name tc))
    (assert-equal 1 (length (cl-cc/type:type-constructor-args tc)))))

(deftest type-specifier-parse-unparse-roundtrip
  "parse-type-specifier + unparse-type round-trips (Option string) faithfully."
  (let* ((ty (cl-cc/type:parse-type-specifier '(Option string)))
         (spec (cl-cc/type:unparse-type ty)))
    (assert-eq 'Option (first spec))
    (assert-true (type-equal-p ty (cl-cc/type:parse-type-specifier spec)))))

(deftest type-arrow-default-mult-is-omega
  "make-type-arrow without explicit mult defaults to :omega."
  (let ((f (make-type-arrow (list type-int) type-string)))
    (assert-true (type-arrow-p f))
    (assert-eq :omega (type-arrow-mult f))))

(deftest type-to-string-returns-string-for-all-forms
  "type-to-string returns a string for primitives, +type-unknown+, nil, and arrows."
  (assert-true (stringp (type-to-string type-int)))
  (assert-true (stringp (type-to-string cl-cc/type:+type-unknown+)))
  (assert-true (stringp (type-to-string nil)))
  (assert-true (search "->" (type-to-string (make-type-arrow (list type-int) type-string)))))
