;;;; tests/unit/type/multiplicity-tests.lisp — Multiplicity System Tests
;;;;
;;;; Tests for src/type/multiplicity.lisp:
;;;; multiplicity-p, mult-add (semiring join), mult-mul (semiring scale),
;;;; mult-leq (ordering), mult-to-string.

(in-package :cl-cc/test)

(defsuite multiplicity-suite :description "Graded multiplicity system tests"
  :parent cl-cc-unit-suite)


(in-suite multiplicity-suite)
;;; ─── multiplicity-p ─────────────────────────────────────────────────────────

(deftest-each mult-valid-grades
  "Valid multiplicity grades are recognized."
  :cases (("zero"  t :zero)
          ("one"   t :one)
          ("omega" t :omega))
  (expected grade)
  (assert-true (multiplicity-p grade)))

(deftest-each mult-invalid-grades
  "Invalid values are rejected."
  :cases (("nil"     nil)
          ("integer" 0)
          ("string"  "one")
          ("keyword" :two))
  (val)
  (assert-false (multiplicity-p val)))

;;; ─── mult-add (semiring join) ───────────────────────────────────────────────

(deftest-each mult-add-identity
  "0 is the additive identity: 0 + q = q, q + 0 = q."
  :cases (("zero-left"  :zero  :zero  :zero)
          ("one-left"   :one   :zero  :one)
          ("omega-left"  :omega :zero :omega)
          ("zero-right" :zero  :zero  :zero)
          ("one-right"  :zero  :one   :one)
          ("omega-right" :zero :omega :omega))
  (q1 q2 expected)
  (assert-eq expected (mult-add q1 q2)))

(deftest-each mult-add-idempotent
  "q + q = q (join is idempotent)."
  :cases (("zero"  :zero :zero)
          ("one"   :one  :one)
          ("omega" :omega :omega))
  (grade expected)
  (assert-eq expected (mult-add grade grade)))

(deftest mult-add-one-omega
  "1 + ω = ω (mixed grades go to unrestricted)."
  (assert-eq :omega (mult-add :one :omega))
  (assert-eq :omega (mult-add :omega :one)))

;;; ─── mult-mul (semiring scale) ──────────────────────────────────────────────

(deftest-each mult-mul-zero-absorbs
  "0 * q = 0, q * 0 = 0 (zero absorbs)."
  :cases (("zero-zero"   :zero  :zero  :zero)
          ("zero-one"    :zero  :one   :zero)
          ("zero-omega"  :zero  :omega :zero)
          ("one-zero"    :one   :zero  :zero)
          ("omega-zero"  :omega :zero  :zero))
  (q1 q2 expected)
  (assert-eq expected (mult-mul q1 q2)))

(deftest-each mult-mul-one-identity
  "1 * q = q, q * 1 = q (one is multiplicative identity)."
  :cases (("one-one"     :one   :one   :one)
          ("one-omega"   :one   :omega :omega)
          ("omega-one"   :omega :one   :omega))
  (q1 q2 expected)
  (assert-eq expected (mult-mul q1 q2)))

(deftest mult-mul-omega-omega
  "ω * ω = ω."
  (assert-eq :omega (mult-mul :omega :omega)))

;;; ─── mult-leq (ordering) ────────────────────────────────────────────────────

(deftest-each mult-leq-reflexive
  "q ≤ q is always true (reflexive)."
  :cases (("zero"  :zero)
          ("one"   :one)
          ("omega" :omega))
  (grade)
  (assert-true (mult-leq grade grade)))

(deftest-each mult-leq-zero-bottom
  "0 ≤ q is always true (zero is bottom)."
  :cases (("zero-one"   :one)
          ("zero-omega" :omega))
  (upper)
  (assert-true (mult-leq :zero upper)))

(deftest-each mult-leq-omega-top
  "q ≤ ω is always true (omega is top)."
  :cases (("zero-omega" :zero)
          ("one-omega"  :one))
  (lower)
  (assert-true (mult-leq lower :omega)))

(deftest-each mult-leq-false-cases
  "Non-trivial ordering: 1 ≤ 0 and ω ≤ 1 are both false."
  :cases (("one-not-leq-zero"   :one   :zero)
          ("omega-not-leq-one"  :omega :one))
  (lower upper)
  (assert-false (mult-leq lower upper)))

;;; ─── mult-to-string ─────────────────────────────────────────────────────────

(deftest-each mult-to-string-values
  "mult-to-string produces the expected output."
  :cases (("zero"  "0" :zero)
          ("one"   "1" :one)
          ("omega" "ω" :omega))
  (expected grade)
  (assert-equal expected (mult-to-string grade)))
