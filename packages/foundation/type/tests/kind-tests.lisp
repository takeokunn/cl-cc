;;;; tests/unit/type/kind-tests.lisp — Kind System Tests
;;;;
;;;; Tests for src/type/kind.lisp:
;;;; kind structs, kind-fun, kind-equal-p, kind-to-string, kind variables.

(in-package :cl-cc/test)

(defsuite kind-suite :description "Kind system tests"
  :parent cl-cc-unit-suite)


(in-suite kind-suite)
;;; ─── kind struct predicates ──────────────────────────────────────────────────

(deftest-each kind-singleton-predicates
  "Each kind singleton constant is recognized by its own predicate."
  :cases (("type"         #'kind-type-p              +kind-type+)
          ("effect"       #'kind-effect-p             +kind-effect+)
          ("constraint"   #'cl-cc/type::kind-constraint-p   +kind-constraint+)
          ("multiplicity" #'cl-cc/type::kind-multiplicity-p +kind-multiplicity+))
  (pred-fn kind)
  (assert-true (funcall pred-fn kind)))

(deftest-each kind-row-singletons
  "Row kind singletons are kind-row-p with the expected element kind."
  :cases (("row-type"   +kind-row-type+   #'kind-type-p)
          ("row-effect" +kind-row-effect+ #'kind-effect-p))
  (row-kind elem-pred)
  (assert-true (kind-row-p row-kind))
  (assert-true (funcall elem-pred (kind-row-elem row-kind))))

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

(deftest kind-var-properties
  "fresh-kind-var: is a kind-var, unique IDs, self-equal, stores optional name."
  (let ((kv  (fresh-kind-var))
        (kv1 (fresh-kind-var))
        (kv2 (fresh-kind-var))
        (kvn (fresh-kind-var "test")))
    (assert-true  (kind-var-p kv))
    (assert-false (kind-var-equal-p kv1 kv2))
    (assert-true  (kind-var-equal-p kv kv))
    (assert-true  (kind-var-p kvn))
    (assert-equal "test" (cl-cc/type::kind-var-name kvn))))

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

(deftest kind-equal-arrow-and-row
  "Arrow equality (same/different components) and row kind equality."
  (assert-true  (kind-equal-p (kind-fun +kind-type+ +kind-type+)
                               (kind-fun +kind-type+ +kind-type+)))
  (assert-false (kind-equal-p (kind-fun +kind-type+ +kind-effect+)
                               (kind-fun +kind-type+ +kind-type+)))
  (assert-true  (kind-equal-p +kind-row-type+ +kind-row-type+))
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

(deftest kind-to-string-computed-kinds
  "Arrow kind prints as '* -> *'; nested arrow parenthesizes left; named var prints as 'k<name>'."
  (assert-equal "* -> *" (kind-to-string (kind-fun +kind-type+ +kind-type+)))
  (let ((inner (kind-fun +kind-type+ +kind-type+)))
    (assert-equal "(* -> *) -> *" (kind-to-string (kind-fun inner +kind-type+))))
  (let ((kv (fresh-kind-var "foo")))
    (assert-equal "kfoo" (kind-to-string kv))))

(deftest kind-type-singleton
  "+kind-type+ is the singleton * kind."
  (assert-true (kind-type-p +kind-type+))
  (assert-true (kind-node-p +kind-type+))
  (assert-true (kind-equal-p +kind-type+ +kind-type+))
  (assert-true (kind-equal-p (make-kind-type) (make-kind-type))))

(deftest kind-arrow-creation
  "kind-fun builds arrow kinds like * -> * for List."
  (let ((list-kind (kind-fun +kind-type+ +kind-type+)))
    (assert-true (kind-arrow-p list-kind))
    (assert-true (kind-equal-p (kind-arrow-from list-kind) +kind-type+))
    (assert-true (kind-equal-p (kind-arrow-to list-kind)   +kind-type+))
    (let ((fix-kind (kind-fun list-kind +kind-type+)))
      (assert-true (kind-arrow-p fix-kind))
      (assert-true (kind-equal-p (kind-arrow-from fix-kind) list-kind)))))

(deftest kind-effect-row-singletons
  "+kind-effect+ and +kind-row-type+ are the Effect and Row * kinds."
  (assert-true (kind-effect-p +kind-effect+))
  (assert-true (kind-row-p +kind-row-type+))
  (assert-true (kind-row-p +kind-row-effect+))
  (assert-true (kind-equal-p (kind-row-elem +kind-row-type+)   +kind-type+))
  (assert-true (kind-equal-p (kind-row-elem +kind-row-effect+) +kind-effect+)))

(deftest-each kind-equal-p-basic
  "kind-equal-p compares structural equality of kind constructors."
  :cases (("*=*"          t   +kind-type+   +kind-type+)
          ("Eff=Eff"      t   +kind-effect+ +kind-effect+)
          ("*≠Eff"        nil +kind-type+   +kind-effect+))
  (should-be-equal k1 k2)
  (if should-be-equal
      (assert-true  (kind-equal-p k1 k2))
      (assert-false (kind-equal-p k1 k2))))

(deftest kind-var-fresh-and-equality
  "fresh-kind-var generates distinct variables; kind-fun arrows compare structurally."
  (let ((k1 (fresh-kind-var 'k))
        (k2 (fresh-kind-var 'k)))
    (assert-true (kind-var-p k1))
    (assert-true (kind-var-p k2))
    (assert-false (kind-var-equal-p k1 k2))
    (assert-true  (kind-var-equal-p k1 k1)))
  (assert-true  (kind-equal-p (kind-fun +kind-type+ +kind-type+)
                               (kind-fun +kind-type+ +kind-type+)))
  (assert-false (kind-equal-p (kind-fun +kind-type+ +kind-effect+)
                               (kind-fun +kind-type+ +kind-type+))))
