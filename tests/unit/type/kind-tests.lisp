;;;; tests/unit/type/kind-tests.lisp — Kind System Tests
;;;;
;;;; Tests for src/type/kind.lisp:
;;;; kind structs, kind-fun, kind-equal-p, kind-to-string, kind variables.

(in-package :cl-cc/test)

(defsuite kind-suite :description "Kind system tests")

;;; ─── kind struct predicates ──────────────────────────────────────────────────

(deftest kind-type-singleton
  "kind-type-p recognizes the singleton * kind."
  (assert-true (kind-type-p +kind-type+)))

(deftest kind-effect-singleton
  "kind-effect-p recognizes the singleton Effect kind."
  (assert-true (kind-effect-p +kind-effect+)))

(deftest kind-constraint-singleton
  "+kind-constraint+ is a kind-constraint."
  (assert-true (cl-cc/type::kind-constraint-p +kind-constraint+)))

(deftest kind-multiplicity-singleton
  "+kind-multiplicity+ is a kind-multiplicity."
  (assert-true (cl-cc/type::kind-multiplicity-p +kind-multiplicity+)))

(deftest kind-row-type-singleton
  "+kind-row-type+ has elem = *."
  (assert-true (kind-row-p +kind-row-type+))
  (assert-true (kind-type-p (kind-row-elem +kind-row-type+))))

(deftest kind-row-effect-singleton
  "+kind-row-effect+ has elem = Effect."
  (assert-true (kind-row-p +kind-row-effect+))
  (assert-true (kind-effect-p (kind-row-elem +kind-row-effect+))))

;;; ─── kind-fun (arrow construction) ──────────────────────────────────────────

(deftest kind-fun-builds-arrow
  "kind-fun returns a kind-arrow with correct from/to."
  (let ((k (kind-fun +kind-type+ +kind-type+)))
    (assert-true (kind-arrow-p k))
    (assert-true (kind-type-p (kind-arrow-from k)))
    (assert-true (kind-type-p (kind-arrow-to k)))))

(deftest kind-fun-nested
  "kind-fun can build nested arrows: * -> * -> *."
  (let ((k (kind-fun +kind-type+ (kind-fun +kind-type+ +kind-type+))))
    (assert-true (kind-arrow-p k))
    (assert-true (kind-type-p (kind-arrow-from k)))
    (assert-true (kind-arrow-p (kind-arrow-to k)))))

;;; ─── kind variables ─────────────────────────────────────────────────────────

(deftest kind-var-fresh
  "fresh-kind-var returns a kind-var."
  (let ((kv (fresh-kind-var)))
    (assert-true (kind-var-p kv))))

(deftest kind-var-unique-ids
  "Two fresh kind vars have different IDs."
  (let ((kv1 (fresh-kind-var))
        (kv2 (fresh-kind-var)))
    (assert-false (kind-var-equal-p kv1 kv2))))

(deftest kind-var-self-equal
  "A kind var equals itself."
  (let ((kv (fresh-kind-var)))
    (assert-true (kind-var-equal-p kv kv))))

(deftest kind-var-with-name
  "fresh-kind-var with a name stores it."
  (let ((kv (fresh-kind-var "test")))
    (assert-true (kind-var-p kv))
    (assert-equal "test" (cl-cc/type::kind-var-name kv))))

;;; ─── kind-equal-p ───────────────────────────────────────────────────────────

(deftest-each kind-equal-same
  "Same-type singletons are equal."
  :cases (("type-type"     +kind-type+       +kind-type+)
          ("effect-effect"  +kind-effect+     +kind-effect+)
          ("constraint"     +kind-constraint+ +kind-constraint+)
          ("multiplicity"   +kind-multiplicity+ +kind-multiplicity+))
  (k1 k2)
  (assert-true (kind-equal-p k1 k2)))

(deftest-each kind-not-equal-different
  "Different kind singletons are not equal."
  :cases (("type-vs-effect"  +kind-type+   +kind-effect+)
          ("type-vs-row"     +kind-type+   +kind-row-type+)
          ("effect-vs-row"   +kind-effect+ +kind-row-effect+))
  (k1 k2)
  (assert-false (kind-equal-p k1 k2)))

(deftest kind-equal-arrow
  "Two identical arrows are equal."
  (assert-true (kind-equal-p
                (kind-fun +kind-type+ +kind-type+)
                (kind-fun +kind-type+ +kind-type+))))

(deftest kind-not-equal-arrow-diff
  "Arrows with different components are not equal."
  (assert-false (kind-equal-p
                 (kind-fun +kind-type+ +kind-effect+)
                 (kind-fun +kind-type+ +kind-type+))))

(deftest kind-equal-row
  "Two Row * kinds are equal."
  (assert-true (kind-equal-p +kind-row-type+ +kind-row-type+)))

(deftest kind-not-equal-row-diff
  "Row * and Row Effect are not equal."
  (assert-false (kind-equal-p +kind-row-type+ +kind-row-effect+)))

;;; ─── kind-to-string ─────────────────────────────────────────────────────────

(deftest-each kind-to-string-values
  "kind-to-string produces expected strings."
  :cases (("star"          "*"            +kind-type+)
          ("effect"        "Effect"       +kind-effect+)
          ("constraint"    "Constraint"   +kind-constraint+)
          ("multiplicity"  "Multiplicity" +kind-multiplicity+)
          ("row-star"      "Row *"        +kind-row-type+)
          ("row-effect"    "Row Effect"   +kind-row-effect+))
  (expected kind)
  (assert-equal expected (kind-to-string kind)))

(deftest kind-to-string-arrow
  "Arrow kind prints as '* -> *'."
  (assert-equal "* -> *" (kind-to-string (kind-fun +kind-type+ +kind-type+))))

(deftest kind-to-string-nested-arrow
  "Nested arrow kind parenthesizes left: '(* -> *) -> *'."
  (let ((inner (kind-fun +kind-type+ +kind-type+)))
    (assert-equal "(* -> *) -> *" (kind-to-string (kind-fun inner +kind-type+)))))

(deftest kind-to-string-var-with-name
  "Kind var with name prints as 'k<name>'."
  (let ((kv (fresh-kind-var "foo")))
    (assert-equal "kfoo" (kind-to-string kv))))
