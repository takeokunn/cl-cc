;;;; tests/unit/type/type-children-tests.lisp — Type Children & Bound-Var Tests
;;;;
;;;; Tests for type-children and type-bound-var in src/type/representation.lisp.
;;;; Verifies the structural data layer that enables data/logic separation
;;;; in type-free-vars, type-occurs-p, and zonk.

(in-package :cl-cc/test)

(defsuite type-children-suite :description "type-children / type-bound-var data layer tests")

;;; ─── type-children: leaf types return nil ──────────────────────────────────

(deftest type-children-primitive
  "Primitive types have no children."
  (assert-null (type-children type-int))
  (assert-null (type-children type-string))
  (assert-null (type-children type-bool)))

(deftest type-children-var
  "Type variables have no children."
  (assert-null (type-children (fresh-type-var "a"))))

(deftest type-children-rigid
  "Rigid variables have no children."
  (assert-null (type-children (fresh-rigid-var "r"))))

(deftest type-children-error
  "Error sentinels have no children."
  (assert-null (type-children (make-type-error :message "test"))))

(deftest type-children-nil
  "nil returns nil."
  (assert-null (type-children nil)))

;;; ─── type-children: compound types ─────────────────────────────────────────

(deftest type-children-arrow-pure
  "Arrow type returns params + return (no effects when nil)."
  (let* ((arr (make-type-arrow (list type-int type-string) type-bool))
         (ch  (type-children arr)))
    (assert-equal 3 (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (assert-true (type-equal-p type-string (second ch)))
    (assert-true (type-equal-p type-bool (third ch)))))

(deftest type-children-arrow-with-effects
  "Arrow type includes effect row when present."
  (let* ((arr (make-type-arrow (list type-int) type-bool :effects +io-effect-row+))
         (ch  (type-children arr)))
    (assert-equal 3 (length ch))
    (assert-true (type-effect-row-p (third ch)))))

(deftest type-children-product
  "Product type returns its elements."
  (let* ((p  (make-type-product :elems (list type-int type-string type-bool)))
         (ch (type-children p)))
    (assert-equal 3 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-union
  "Union type returns its alternatives."
  (let* ((u  (make-type-union (list type-int type-string)))
         (ch (type-children u)))
    (assert-equal 2 (length ch))))

(deftest type-children-intersection
  "Intersection type returns its types."
  (let* ((i  (make-type-intersection (list type-int type-string)))
         (ch (type-children i)))
    (assert-equal 2 (length ch))))

(deftest type-children-record-closed
  "Closed record returns field values."
  (let* ((r  (make-type-record :fields (list (cons :x type-int) (cons :y type-string))
                               :row-var nil))
         (ch (type-children r)))
    (assert-equal 2 (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (assert-true (type-equal-p type-string (second ch)))))

(deftest type-children-record-open
  "Open record includes row variable."
  (let* ((rv (fresh-type-var "rho"))
         (r  (make-type-record :fields (list (cons :x type-int))
                               :row-var rv))
         (ch (type-children r)))
    (assert-equal 2 (length ch))
    (assert-true (type-var-p (second ch)))))

(deftest type-children-variant-closed
  "Closed variant returns case values."
  (let* ((v  (make-type-variant :cases (list (cons :some type-int) (cons :none type-null))
                                :row-var nil))
         (ch (type-children v)))
    (assert-equal 2 (length ch))))

(deftest type-children-variant-open
  "Open variant includes row variable."
  (let* ((rv (fresh-type-var "rho"))
         (v  (make-type-variant :cases (list (cons :ok type-int)) :row-var rv))
         (ch (type-children v)))
    (assert-equal 2 (length ch))
    (assert-true (type-var-p (second ch)))))

(deftest type-children-forall
  "Forall returns only the body (not the bound var)."
  (let* ((a (fresh-type-var "a"))
         (f (make-type-forall :var a :body type-int))
         (ch (type-children f)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-exists
  "Exists returns only the body."
  (let* ((a (fresh-type-var "a"))
         (e (make-type-exists :var a :body type-string))
         (ch (type-children e)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-string (first ch)))))

(deftest type-children-app
  "Type application returns fun and arg."
  (let* ((app (make-type-app :fun type-int :arg type-string))
         (ch  (type-children app)))
    (assert-equal 2 (length ch))
    (assert-true (type-equal-p type-int (first ch)))
    (assert-true (type-equal-p type-string (second ch)))))

(deftest type-children-lambda
  "Type lambda returns only the body."
  (let* ((a (fresh-type-var "a"))
         (lam (cl-cc/type::make-type-lambda :var a :body type-int))
         (ch  (type-children lam)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-mu
  "Mu (recursive) returns only the body."
  (let* ((a (fresh-type-var "a"))
         (m (make-type-mu :var a :body type-int))
         (ch (type-children m)))
    (assert-equal 1 (length ch))))

(deftest type-children-refinement
  "Refinement returns only the base type."
  (let* ((r  (cl-cc/type::make-type-refinement :base type-int :predicate #'numberp))
         (ch (type-children r)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-linear
  "Linear returns only the base type."
  (let* ((lin (make-type-linear :base type-int :grade :one))
         (ch  (type-children lin)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-capability
  "Capability returns only the base type."
  (let* ((cap (cl-cc/type::make-type-capability :base type-int :cap 'read))
         (ch  (type-children cap)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-effect-row-closed
  "Closed effect row returns its effects."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (row (make-type-effect-row :effects (list eff) :row-var nil))
         (ch  (type-children row)))
    (assert-equal 1 (length ch))
    (assert-true (cl-cc/type::type-effect-op-p (first ch)))))

(deftest type-children-effect-row-open
  "Open effect row includes row variable."
  (let* ((rv  (fresh-type-var "rho"))
         (eff (make-type-effect-op :name 'io :args nil))
         (row (make-type-effect-row :effects (list eff) :row-var rv))
         (ch  (type-children row)))
    (assert-equal 2 (length ch))
    (assert-true (type-var-p (second ch)))))

(deftest type-children-effect-op-no-args
  "Effect op with no args returns nil."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (ch  (type-children eff)))
    (assert-null ch)))

(deftest type-children-effect-op-with-args
  "Effect op with args returns its args."
  (let* ((eff (make-type-effect-op :name 'state :args (list type-int)))
         (ch  (type-children eff)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-handler
  "Handler returns effect, input, and output."
  (let* ((eff (make-type-effect-op :name 'io :args nil))
         (h   (cl-cc/type::make-type-handler :effect eff :input type-int :output type-string))
         (ch  (type-children h)))
    (assert-equal 3 (length ch))))

(deftest type-children-constraint
  "Constraint returns the type-arg."
  (let* ((c  (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (ch (type-children c)))
    (assert-equal 1 (length ch))
    (assert-true (type-equal-p type-int (first ch)))))

(deftest type-children-qualified
  "Qualified returns constraints + body."
  (let* ((c  (cl-cc/type::make-type-constraint :class-name 'eq :type-arg type-int))
         (q  (make-type-qualified :constraints (list c) :body type-string))
         (ch (type-children q)))
    (assert-equal 2 (length ch))
    (assert-true (type-equal-p type-string (second ch)))))

;;; ─── type-bound-var ────────────────────────────────────────────────────────

(deftest type-bound-var-forall
  "Forall binds a type variable."
  (let* ((a (fresh-type-var "a"))
         (f (make-type-forall :var a :body type-int)))
    (assert-true (type-var-equal-p a (type-bound-var f)))))

(deftest type-bound-var-exists
  "Exists binds a type variable."
  (let* ((a (fresh-type-var "a"))
         (e (make-type-exists :var a :body type-int)))
    (assert-true (type-var-equal-p a (type-bound-var e)))))

(deftest type-bound-var-lambda
  "Type lambda binds a type variable."
  (let* ((a (fresh-type-var "a"))
         (lam (cl-cc/type::make-type-lambda :var a :body type-int)))
    (assert-true (type-var-equal-p a (type-bound-var lam)))))

(deftest type-bound-var-mu
  "Mu binds a type variable."
  (let* ((a (fresh-type-var "a"))
         (m (make-type-mu :var a :body type-int)))
    (assert-true (type-var-equal-p a (type-bound-var m)))))

(deftest type-bound-var-non-binding
  "Non-binding types return nil."
  (assert-null (type-bound-var type-int))
  (assert-null (type-bound-var (fresh-type-var "a")))
  (assert-null (type-bound-var (make-type-arrow (list type-int) type-bool)))
  (assert-null (type-bound-var (make-type-product :elems (list type-int))))
  (assert-null (type-bound-var (make-type-union (list type-int type-string)))))

;;; ─── Integration: type-free-vars still works correctly ─────────────────────

(deftest type-free-vars-via-children-simple
  "type-free-vars finds var in arrow type."
  (let* ((a   (fresh-type-var "a"))
         (arr (make-type-arrow (list a) type-int)))
    (assert-equal 1 (length (type-free-vars arr)))
    (assert-true (type-var-equal-p a (first (type-free-vars arr))))))

(deftest type-free-vars-via-children-forall-binds
  "type-free-vars correctly excludes bound var in forall."
  (let* ((a (fresh-type-var "a"))
         (f (make-type-forall :var a :body a)))
    (assert-null (type-free-vars f))))

(deftest type-free-vars-via-children-forall-free
  "type-free-vars finds free var in forall body."
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

(deftest type-occurs-p-via-children-direct
  "type-occurs-p finds var directly."
  (let* ((a (fresh-type-var "a")))
    (assert-true (type-occurs-p a a (make-substitution)))))

(deftest type-occurs-p-via-children-in-arrow
  "type-occurs-p finds var in arrow params."
  (let* ((a   (fresh-type-var "a"))
         (arr (make-type-arrow (list a) type-int)))
    (assert-true (type-occurs-p a arr (make-substitution)))))

(deftest type-occurs-p-via-children-absent
  "type-occurs-p returns nil when var absent."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (arr (make-type-arrow (list a) type-int)))
    (assert-false (type-occurs-p b arr (make-substitution)))))

(deftest type-occurs-p-via-children-through-subst
  "type-occurs-p follows substitution chains."
  (let* ((a (fresh-type-var "a"))
         (b (fresh-type-var "b"))
         (subst (make-substitution)))
    (subst-extend! b a subst)
    (assert-true (type-occurs-p a b subst))))

(deftest type-occurs-p-via-children-nested
  "type-occurs-p finds var in nested structure."
  (let* ((a (fresh-type-var "a"))
         (ty (make-type-union
              (list type-int
                    (make-type-product :elems
                      (list type-string
                            (make-type-arrow (list a) type-bool)))))))
    (assert-true (type-occurs-p a ty (make-substitution)))))
