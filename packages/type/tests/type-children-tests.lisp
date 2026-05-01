;;;; tests/unit/type/type-children-tests.lisp — Type Children & Bound-Var Tests
;;;;
;;;; Tests for type-children and type-bound-var in src/type/representation.lisp.
;;;; Verifies the structural data layer that enables data/logic separation
;;;; in type-free-vars, type-occurs-p, and zonk.

(in-package :cl-cc/test)

(defsuite type-children-suite
  :description "type-children / type-bound-var data layer tests"
  :parent cl-cc-unit-suite)


(in-suite type-children-suite)
;;; ─── type-children: leaf types return nil ──────────────────────────────────

(deftest-each type-children-atomic-leaf-types
  "Leaf types (primitive, variable, rigid, error, nil) have no children."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool)
          ("var"    (fresh-type-var "a"))
          ("rigid"  (fresh-rigid-var "r"))
          ("error"  (make-type-error :message "test"))
          ("nil"    nil))
  (leaf-type)
  (assert-null (cl-cc/type:type-children leaf-type)))

;;; ─── type-children: compound types ─────────────────────────────────────────

(deftest type-children-pure-arrow-yields-params-and-return
  "Pure arrow children: params and return type, 3 total."
  (let* ((arr (make-type-arrow (list type-int type-string) type-bool))
         (ch  (cl-cc/type:type-children arr)))
    (assert-equal 3 (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (assert-true (type-equal-p type-string (second ch)))
    (assert-true (type-equal-p type-bool (third ch)))))

(deftest type-children-arrow-with-effects-has-effect-row-as-child
  "Arrow with effects: 3rd child is the effect row."
  (let* ((arr (make-type-arrow (list type-int) type-bool :effects +io-effect-row+))
         (ch  (cl-cc/type:type-children arr)))
    (assert-equal 3 (length ch))
    (assert-true (type-effect-row-p (third ch)))))

(deftest type-children-product
  "Product type returns its elements."
  (let* ((p  (make-type-product :elems (list type-int type-string type-bool)))
         (ch (cl-cc/type:type-children p)))
    (assert-equal 3 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest-each type-children-binary-collection-types
  "Union and intersection types return their 2 members as children."
  :cases (("union"        (make-type-union (list type-int type-string)))
          ("intersection" (make-type-intersection (list type-int type-string))))
  (ty)
  (assert-equal 2 (length (cl-cc/type:type-children ty))))

(deftest-each type-children-record-variants
  "Record children: closed → field values; open → fields + row variable."
  :cases (("closed" nil 2 type-string)
          ("open"   t   2 nil))
  (open-p expected-count second-type-or-nil)
  (let* ((rv (when open-p (fresh-type-var "rho")))
         (fields (if open-p
                     (list (cons :x type-int))
                     (list (cons :x type-int) (cons :y type-string))))
         (r  (make-type-record :fields fields :row-var rv))
         (ch (cl-cc/type:type-children r)))
    (assert-equal expected-count (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (if open-p
        (assert-true  (type-var-p (second ch)))
        (assert-true  (type-equal-p second-type-or-nil (second ch))))))

(deftest-each type-children-variant-variants
  "Variant children: closed → case values; open → cases + row variable."
  :cases (("closed" nil)
          ("open"   t))
  (open-p)
  (let* ((rv (when open-p (fresh-type-var "rho")))
         (cases (if open-p
                    (list (cons :ok type-int))
                    (list (cons :some type-int) (cons :none type-null))))
         (v  (make-type-variant :cases cases :row-var rv))
         (ch (cl-cc/type:type-children v)))
    (assert-equal 2 (length ch))
    (when open-p (assert-true (type-var-p (second ch))))))

(deftest-each type-children-quantifier-return-body
  "Forall and exists return only the body (1 child)."
  :cases (("forall" (make-type-forall :var (fresh-type-var "a") :body type-int)  type-int)
          ("exists" (make-type-exists :var (fresh-type-var "a") :body type-string) type-string))
  (ty expected-body)
  (let ((ch (cl-cc/type:type-children ty)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p expected-body (first ch)))))

(deftest type-children-app
  "Type application returns fun and arg."
  (let* ((app (make-type-app :fun type-int :arg type-string))
         (ch  (cl-cc/type:type-children app)))
    (assert-equal 2 (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (assert-true (type-equal-p type-string (second ch)))))

(deftest type-children-lambda-and-mu
  "Type lambda and mu return only the body (1 child)."
  (let ((a (fresh-type-var "a")))
    (let ((ch (cl-cc/type:type-children (cl-cc/type::make-type-lambda :var a :body type-int))))
      (assert-equal 1 (length ch))
      (assert-true (type-equal-p type-int (first ch))))
    (let ((ch (cl-cc/type:type-children (make-type-mu :var a :body type-int))))
      (assert-equal 1 (length ch)))))

(deftest-each type-children-wrapper-types
  "Refinement, linear, and capability all return only the base type (1 child)."
  :cases (("refinement"  (cl-cc/type::make-type-refinement :base type-int :predicate #'numberp))
          ("linear"      (make-type-linear :base type-int :grade :one))
          ("capability"  (cl-cc/type::make-type-capability :base type-int :cap 'read)))
  (ty)
  (let ((ch (cl-cc/type:type-children ty)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest-each type-children-effect-row-variants
  "Effect row children: closed → effects only; open → effects + row variable."
  :cases (("closed" nil 1)
          ("open"   t   2))
  (open-p expected-count)
  (let* ((rv  (when open-p (fresh-type-var "rho")))
         (eff (make-type-effect-op :name 'io :args nil))
         (row (make-type-effect-row :effects (list eff) :row-var rv))
         (ch  (cl-cc/type:type-children row)))
    (assert-equal expected-count (length ch))
    (assert-true (cl-cc/type::type-effect-op-p (first ch)))
    (when open-p (assert-true (type-var-p (second ch))))))

(deftest-each type-children-effect-op-cases
  "Effect op: no args → nil children; with args → children list matching the args."
  :cases (("no-args"   'io    nil              nil)
          ("with-args" 'state (list type-int)  1))
  (name args expected-count)
  (let* ((eff (make-type-effect-op :name name :args args))
         (ch  (cl-cc/type:type-children eff)))
    (if expected-count
        (progn (assert-equal expected-count (length ch))
               (assert-true (type-equal-p type-int (first ch))))
        (assert-null ch))))

(deftest type-children-handler-has-three-children
  "Handler type has 3 children: effect, input, output."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h   (cl-cc/type::make-type-handler :effect eff :input type-int :output type-string))
         (ch  (cl-cc/type:type-children h)))
    (assert-equal 3 (length ch))))

(deftest type-children-constraint-has-one-child
  "Constraint type has 1 child: the type-arg."
  (let* ((c  (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (ch (cl-cc/type:type-children c)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-qualified-has-two-children
  "Qualified type has 2 children: constraints and body."
  (let* ((c  (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (q  (make-type-qualified :constraints (list c) :body type-string))
         (ch (cl-cc/type:type-children q)))
    (assert-equal 2 (length ch))
    (assert-true (type-equal-p type-string (second ch)))))

;;; ─── type-bound-var ────────────────────────────────────────────────────────

(deftest-each type-bound-var-binding-types
  "Forall, exists, type-lambda, and mu all bind a type variable."
  :cases (("forall" (let ((a (fresh-type-var "a"))) (make-type-forall :var a :body type-int)))
          ("exists" (let ((a (fresh-type-var "a"))) (make-type-exists :var a :body type-int)))
          ("lambda" (let ((a (fresh-type-var "a"))) (cl-cc/type::make-type-lambda :var a :body type-int)))
          ("mu"     (let ((a (fresh-type-var "a"))) (make-type-mu :var a :body type-int))))
  (ty)
  (assert-true (type-var-p (cl-cc/type:type-bound-var ty))))

(deftest-each type-bound-var-non-binding-cases
  "Non-binding types (primitive, var, arrow, product, union) return nil for type-bound-var."
  :cases (("primitive" type-int)
          ("var"       (fresh-type-var "a"))
          ("arrow"     (make-type-arrow (list type-int) type-bool))
          ("product"   (make-type-product :elems (list type-int)))
          ("union"     (make-type-union (list type-int type-string))))
  (ty)
  (assert-null (cl-cc/type:type-bound-var ty)))

;;; ─── Integration: type-free-vars still works correctly ─────────────────────

(deftest type-free-vars-via-children-simple
  "type-free-vars finds var in arrow type."
  (let* ((a   (fresh-type-var "a"))
         (arr (make-type-arrow (list a) type-int)))
    (assert-equal 1 (length (type-free-vars arr)))
    (assert-true (type-var-equal-p a (first (type-free-vars arr))))))

(deftest type-free-vars-forall-bound-var-excluded
  "type-free-vars on (forall a a) returns nil — the bound var is excluded."
  (let* ((a (fresh-type-var "a"))
         (f (make-type-forall :var a :body a)))
    (assert-null (type-free-vars f))))

(deftest type-free-vars-forall-only-unbound-vars-are-free
  "type-free-vars on (forall a (a→b)) returns only b."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (f (make-type-forall :var a :body (make-type-arrow (list a) b))))
    (assert-equal 1 (length (type-free-vars f)))
    (assert-true (type-var-equal-p b (first (type-free-vars f))))))

(deftest type-free-vars-via-children-record
  "type-free-vars finds vars in record fields and row-var."
  (let* ((a  (fresh-type-var "a"))
         (rv (fresh-type-var "rho"))
         (r  (make-type-record :fields (list (cons :x a)) :row-var rv)))
    (assert-equal 2 (length (type-free-vars r)))))

(deftest type-free-vars-via-children-mu
  "type-free-vars excludes mu-bound var."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (m (make-type-mu :var a :body (make-type-product :elems (list a b)))))
    (assert-equal 1 (length (type-free-vars m)))
    (assert-true (type-var-equal-p b (first (type-free-vars m))))))

(deftest type-free-vars-via-children-nested
  "type-free-vars finds vars in deeply nested type."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (ty (make-type-union
              (list (make-type-arrow (list a) type-int)
                    (make-type-product :elems (list b type-string))))))
    (assert-equal 2 (length (type-free-vars ty)))))

;;; ─── Integration: type-occurs-p still works correctly ──────────────────────

(deftest type-occurs-p-direct-arrow-and-absent
  "type-occurs-p: true for direct match and match in arrow; false for absent var."
  (let ((a (fresh-type-var "a"))
        (b (fresh-type-var "b"))
        (s (make-substitution)))
    (assert-true  (type-occurs-p a a s))
    (assert-true  (type-occurs-p a (make-type-arrow (list a) type-int) s))
    (assert-false (type-occurs-p b (make-type-arrow (list a) type-int) s))))

(deftest type-occurs-p-follows-subst-chain
  "type-occurs-p follows substitution chains: a occurs in b when b→a."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (subst (make-substitution)))
    (subst-extend! b a subst)
    (assert-true (type-occurs-p a b subst))))

(deftest type-occurs-p-finds-var-in-nested-union
  "type-occurs-p finds var nested inside union→product→arrow."
  (let* ((a (fresh-type-var "a"))
         (ty (make-type-union
              (list type-int
                    (make-type-product :elems
                      (list type-string
                            (make-type-arrow (list a) type-bool)))))))
    (assert-true (type-occurs-p a ty (make-substitution)))))
