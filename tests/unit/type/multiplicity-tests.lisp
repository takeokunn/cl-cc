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

(deftest-each multiplicity-p-recognition
  "multiplicity-p accepts :zero/:one/:omega; rejects all other values."
  :cases (("zero-valid"   :zero  t)
          ("one-valid"    :one   t)
          ("omega-valid"  :omega t)
          ("two-invalid"  :two   nil)
          ("nil-invalid"  nil    nil)
          ("int-invalid"  1      nil))
  (val expected)
  (if expected
      (assert-true  (cl-cc/type:multiplicity-p val))
      (assert-false (cl-cc/type:multiplicity-p val))))

(deftest-each multiplicity-grade-constants
  "The three multiplicity constants are valid grades with the correct keyword value."
  :cases (("zero"  +mult-zero+  :zero)
          ("one"   +mult-one+   :one)
          ("omega" +mult-omega+ :omega))
  (grade expected-kw)
  (assert-true (multiplicity-p grade))
  (assert-eq expected-kw grade))

(deftest-each multiplicity-add
  "mult-add implements the commutative semiring join: 0+q=q, 1+1=1, 1+ω=ω."
  :cases (("0+0=0" :zero  :zero  :zero)
          ("0+1=1" :zero  :one   :one)
          ("1+0=1" :one   :zero  :one)
          ("0+ω=ω" :zero  :omega :omega)
          ("1+1=1" :one   :one   :one)
          ("1+ω=ω" :one   :omega :omega)
          ("ω+ω=ω" :omega :omega :omega))
  (a b expected)
  (assert-eq expected (mult-add a b)))

(deftest-each multiplicity-mul
  "mult-mul is semiring scaling: 0*q=0, 1*q=q, ω*ω=ω."
  :cases (("0*0=0" :zero  :zero  :zero)
          ("0*1=0" :zero  :one   :zero)
          ("0*ω=0" :zero  :omega :zero)
          ("1*0=0" :one   :zero  :zero)
          ("1*1=1" :one   :one   :one)
          ("1*ω=ω" :one   :omega :omega)
          ("ω*0=0" :omega :zero  :zero)
          ("ω*1=ω" :omega :one   :omega)
          ("ω*ω=ω" :omega :omega :omega))
  (a b expected)
  (assert-eq expected (mult-mul a b)))

(deftest-each multiplicity-leq
  "mult-leq implements the partial order 0 ≤ 1 ≤ ω."
  :cases (("0≤0"  :zero  :zero  t)
          ("0≤1"  :zero  :one   t)
          ("0≤ω"  :zero  :omega t)
          ("1≤1"  :one   :one   t)
          ("1≤ω"  :one   :omega t)
          ("ω≤ω"  :omega :omega t)
          ("1≰0"  :one   :zero  nil)
          ("ω≰1"  :omega :one   nil)
          ("ω≰0"  :omega :zero  nil))
  (a b expected)
  (if expected
      (assert-true  (mult-leq a b))
      (assert-false (mult-leq a b))))

(deftest-each multiplicity-to-string
  "mult-to-string renders each grade as its canonical symbol."
  :cases (("zero"  :zero  "0")
          ("one"   :one   "1")
          ("omega" :omega "ω"))
  (grade expected-str)
  (assert-string= expected-str (mult-to-string grade)))
