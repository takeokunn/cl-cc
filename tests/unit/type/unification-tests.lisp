;;;; tests/unit/type/unification-tests.lisp — Unification Tests
;;;
;;; Tests for type-unify, type-unify-lists, and unify-effect-rows
;;; focusing on coverage gaps: product types, intersection types,
;;; type constructors, effect rows, and edge cases.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Product Type Unification ───────────────────────────────────────────

(deftest unify-product-same-types
  "Unifying identical product types succeeds."
  (let ((p (cl-cc/type:make-type-product
            :elems (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify p p)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-product-with-vars
  "Unifying a product with variables binds them."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (p1 (cl-cc/type:make-type-product :elems (list a cl-cc/type:type-string)))
         (p2 (cl-cc/type:make-type-product :elems (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify p1 p2)
      (assert-true ok)
      (let ((bound (zonk a s)))
        (assert-eq 'fixnum (cl-cc/type:type-primitive-name bound))))))

(deftest unify-product-length-mismatch
  "Products with different lengths fail to unify."
  (let ((p1 (cl-cc/type:make-type-product :elems (list cl-cc/type:type-int)))
        (p2 (cl-cc/type:make-type-product :elems (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify p1 p2)
      (declare (ignore s))
      (assert-false ok))))

;;; ─── Intersection Type Unification ─────────────────────────────────────

(deftest unify-intersection-same
  "Identical intersections unify."
  (let ((i (cl-cc/type:make-type-intersection-raw
            :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify i i)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-intersection-length-mismatch
  "Intersections of different lengths fail."
  (let ((i1 (cl-cc/type:make-type-intersection-raw :types (list cl-cc/type:type-int)))
        (i2 (cl-cc/type:make-type-intersection-raw
             :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify i1 i2)
      (declare (ignore s))
      (assert-false ok))))

;;; ─── Union Type Unification ─────────────────────────────────────────────

(deftest unify-union-both-same
  "Two identical unions unify."
  (let ((u (cl-cc/type:make-type-union-raw
            :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify u u)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-union-right-intro
  "A union unifies with a member type (right intro)."
  (let ((u (cl-cc/type:make-type-union-raw
            :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify u cl-cc/type:type-int)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-union-left-intro
  "A member type unifies with a union (left intro)."
  (let ((u (cl-cc/type:make-type-union-raw
            :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify cl-cc/type:type-string u)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-union-non-member-fails
  "A non-member type fails to unify with a union."
  (let ((u (cl-cc/type:make-type-union-raw
            :types (list cl-cc/type:type-int cl-cc/type:type-string))))
    (multiple-value-bind (s ok) (type-unify u cl-cc/type:type-bool)
      (declare (ignore s))
      (assert-false ok))))

;;; ─── Primitive Unification Edge Cases ───────────────────────────────────

(deftest unify-different-primitives-fail
  "Different primitive types fail to unify."
  (multiple-value-bind (s ok) (type-unify cl-cc/type:type-int cl-cc/type:type-string)
    (declare (ignore s))
    (assert-false ok)))

(deftest unify-error-type-with-anything
  "type-error unifies with any type."
  (let ((err (cl-cc/type:make-type-error)))
    (multiple-value-bind (s ok) (type-unify err cl-cc/type:type-int)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))
    (multiple-value-bind (s ok) (type-unify cl-cc/type:type-string err)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

;;; ─── Variable Binding ───────────────────────────────────────────────────

(deftest unify-var-bound-in-subst
  "A variable already bound in subst is followed."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (multiple-value-bind (s2 ok) (type-unify a cl-cc/type:type-int s)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s2)))))

(deftest unify-var-bound-conflicting-fails
  "A variable bound to int fails to unify with string."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (multiple-value-bind (s2 ok) (type-unify a cl-cc/type:type-string s)
      (declare (ignore s2))
      (assert-false ok))))

;;; ─── type-unify-lists ───────────────────────────────────────────────────

(deftest unify-lists-empty
  "Two empty lists unify successfully."
  (multiple-value-bind (s ok) (type-unify-lists nil nil (make-substitution))
    (assert-true ok)
    (assert-true (cl-cc/type:substitution-p s))))

(deftest unify-lists-length-mismatch
  "Lists of different lengths fail."
  (multiple-value-bind (s ok) (type-unify-lists
                                (list cl-cc/type:type-int)
                                nil
                                (make-substitution))
    (declare (ignore s))
    (assert-false ok)))

(deftest unify-lists-pairwise
  "Lists are unified element-wise."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b)))
    (multiple-value-bind (s ok)
        (type-unify-lists (list a b)
                          (list cl-cc/type:type-int cl-cc/type:type-string)
                          (make-substitution))
      (assert-true ok)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name (zonk a s)))
      (assert-eq 'string (cl-cc/type:type-primitive-name (zonk b s))))))

(deftest unify-lists-partial-failure
  "If any pair fails, the whole list unification fails."
  (multiple-value-bind (s ok)
      (type-unify-lists (list cl-cc/type:type-int cl-cc/type:type-int)
                        (list cl-cc/type:type-int cl-cc/type:type-string)
                        (make-substitution))
    (declare (ignore s))
    (assert-false ok)))

;;; ─── Arrow Unification Edge Cases ───────────────────────────────────────

(deftest unify-arrow-arity-mismatch
  "Arrows with different param counts fail."
  (let ((f1 (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                             :return cl-cc/type:type-int))
        (f2 (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int cl-cc/type:type-int)
                                             :return cl-cc/type:type-int)))
    (multiple-value-bind (s ok) (type-unify f1 f2)
      (declare (ignore s))
      (assert-false ok))))

(deftest unify-arrow-return-mismatch
  "Arrows with matching params but different return types fail."
  (let ((f1 (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                             :return cl-cc/type:type-int))
        (f2 (cl-cc/type:make-type-arrow-raw :params (list cl-cc/type:type-int)
                                             :return cl-cc/type:type-string)))
    (multiple-value-bind (s ok) (type-unify f1 f2)
      (declare (ignore s))
      (assert-false ok))))

;;; ─── Effect Row Unification ─────────────────────────────────────────────

(deftest unify-effect-rows-both-empty
  "Two empty effect rows (no row-var) unify."
  (let ((r1 (cl-cc/type:make-type-effect-row :effects nil :row-var nil))
        (r2 (cl-cc/type:make-type-effect-row :effects nil :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-effect-rows-same-effects
  "Rows with same effects unify."
  (let* ((e1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e2 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e1) :row-var nil))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e2) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (assert-true ok)
      (assert-true (cl-cc/type:substitution-p s)))))

(deftest unify-effect-rows-open-absorbs-extra
  "An open row (with row-var) absorbs extra effects from the other side."
  (let* ((rv (cl-cc/type:make-type-variable 'r))
         (e-io (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e-exn (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e-io) :row-var rv))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e-io e-exn) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (assert-true ok)
      ;; rv should be bound to {exn}
      (let ((bound (zonk rv s)))
        (assert-true (cl-cc/type:type-effect-row-p bound))))))

(deftest unify-effect-rows-closed-rejects-extra
  "A closed row (no row-var) rejects extra effects."
  (let* ((e-io (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (e-exn (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (r1 (cl-cc/type:make-type-effect-row :effects (list e-io) :row-var nil))
         (r2 (cl-cc/type:make-type-effect-row :effects (list e-io e-exn) :row-var nil)))
    (multiple-value-bind (s ok) (type-unify r1 r2)
      (declare (ignore s))
      (assert-false ok))))

;;; ─── Occurs Check in Unification ────────────────────────────────────────

(deftest unify-occurs-check-circular
  "Occurs check prevents circular type: a ~ (a -> int)."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (fn (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int)))
    (multiple-value-bind (s ok) (type-unify a fn)
      (declare (ignore s))
      (assert-false ok))))
