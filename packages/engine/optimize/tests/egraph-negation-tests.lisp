;;;; tests/unit/optimize/egraph-negation-tests.lisp — E-Graph Negation Rule Tests
;;;
;;; Continuation of egraph-rules-tests.lisp covering negation rules:
;;;   - Negation: mul-neg1
;;;   - Negation: double-neg / not-not
;;;   - Negation: add-neg / sub-neg
;;;
;;; Helpers (make-eg-const, eg-merged-p, eg-all-nodes, eg-class-contains-op-p,
;;; eg-any-class-data-eql-p, eg-any-class-has-op-p, eg-saturate,
;;; eg-rule-registered-p) are defined in egraph-rules-tests.lisp which loads
;;; first; both files share the :cl-cc/test package.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Negation: mul-neg1 ──────────────────────────────────────────────────
;;; mul-neg1-r: (mul ?x (const -1)) → (neg ?x)
;;; RHS (neg ?x) is an actual neg-node, not a const.  Pre-add (neg x) so
;;; the rule can merge mul-id with the pre-existing neg class.

(deftest-each egraph-rule-mul-neg1-fires
  "mul-neg1: (mul ?x (const -1)) and (mul (const -1) ?x) both merge with (neg ?x)."
  :cases (("rhs-neg1" :rhs)
          ("lhs-neg1" :lhs))
  (side)
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (cn1 (make-eg-const eg -1))
         (mul (if (eq side :rhs)
                  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul x cn1)
                  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul cn1 x)))
         (neg (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg))))

;;; ─── Negation: double-neg / not-not ─────────────────────────────────────

(deftest-each egraph-rule-double-negation-fires
  "Double negation: (op (op ?x)) merges with ?x for both neg and not."
  :cases (("double-neg" 'cl-cc/optimize::neg)
          ("not-not"    'cl-cc/optimize::not))
  (op)
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (op1  (cl-cc/optimize::egraph-add eg op x))
         (op2  (cl-cc/optimize::egraph-add eg op op1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg op2 x))))

(deftest-each egraph-rule-negated-comparison-fires
  "Negated comparisons merge with their dual comparison."
  :cases (("not-lt" 'cl-cc/optimize::not 'cl-cc/optimize::lt 'cl-cc/optimize::ge)
          ("not-gt" 'cl-cc/optimize::not 'cl-cc/optimize::gt 'cl-cc/optimize::le)
          ("not-le" 'cl-cc/optimize::not 'cl-cc/optimize::le 'cl-cc/optimize::gt)
          ("not-ge" 'cl-cc/optimize::not 'cl-cc/optimize::ge 'cl-cc/optimize::lt))
  (not-op cmp-op dual-op)
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize::egraph-add eg cmp-op x y))
         (not (cl-cc/optimize::egraph-add eg not-op cmp))
         (dual (cl-cc/optimize::egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg not dual))))

;;; ─── Negation: add-neg / sub-neg ─────────────────────────────────────────
;;; add-neg: (add ?x (neg ?y)) → (sub ?x ?y)
;;; sub-neg: (sub ?x (neg ?y)) → (add ?x ?y)
;;; Use var for x and a const for y so x ≠ y (different memo keys).

(deftest-each egraph-rule-neg-rewrites
  "add-neg/sub-neg: outer(x, neg(y)) merges with the inverse-op(x, y)."
  :cases (("add-neg" 7 'cl-cc/optimize::add 'cl-cc/optimize::sub)
          ("sub-neg" 5 'cl-cc/optimize::sub 'cl-cc/optimize::add))
  (const-val outer-op inverse-op)
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg const-val))
         (ny   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg y))
         (expr (cl-cc/optimize::egraph-add eg outer-op x ny))
         (dual (cl-cc/optimize::egraph-add eg inverse-op x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg expr dual))))
