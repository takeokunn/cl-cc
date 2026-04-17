;;;; tests/unit/type/exhaustiveness-tests.lisp
;;;; FR-1903: Exhaustiveness / Coverage Checking Tests
;;;;
;;;; Tests for check-typecase-exhaustiveness, check-etypecase-completeness,
;;;; useful-typecase-arms, and typecase-arm-subsumed-p.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── typecase-arm-subsumed-p ──────────────────────────────────────────────

(deftest-each exhaustiveness-arm-subsumed-p-cases
  "typecase-arm-subsumed-p: T subsumes all; exact match; nominal supertype; unrelated fails."
  :cases (("int-by-T"          'integer '(t)                     t)
          ("string-by-T"       'string  '(t)                     t)
          ("fixnum-by-T"       'fixnum  '(t)                     t)
          ("integer-by-exact"  'integer '(integer)               t)
          ("string-in-list"    'string  '(symbol integer string)  t)
          ("fixnum-by-integer" 'fixnum  '(integer)               t)
          ("cons-by-list"      'cons    '(list)                   t)
          ("string-not-int"    'string  '(integer)               nil)
          ("symbol-not-in"     'symbol  '(integer string)        nil)
          ("int-no-arms"       'integer nil                       nil))
  (arm-type covered expected)
  (if expected
      (assert-true  (cl-cc/type:typecase-arm-subsumed-p arm-type covered))
      (assert-false (cl-cc/type:typecase-arm-subsumed-p arm-type covered))))

;;; ─── check-typecase-exhaustiveness ───────────────────────────────────────

(deftest-each exhaustiveness-basic-coverage-cases
  "check-typecase-exhaustiveness: T at end → exhaustive; without T → not exhaustive."
  :cases (("with-t"    '(integer string t) t)
          ("without-t" '(integer string)   nil))
  (arms expected-exhaustive)
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (if expected-exhaustive
        (assert-true  exhaustive-p)
        (assert-false exhaustive-p))
    (assert-null unreachable)
    (assert-null warnings)))

(deftest exhaustiveness-detects-duplicate-arm
  "Duplicate arm (same type twice) is flagged as unreachable."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer integer t))
    (assert-true exhaustive-p)
    (assert-= 1 (length unreachable))
    (assert-= 1 (first unreachable))        ; second 'integer (index 1) is unreachable
    (assert-= 1 (length warnings))))

(deftest exhaustiveness-detects-subsumed-arm
  "fixnum after integer is flagged as unreachable."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer fixnum t))
    (assert-true exhaustive-p)
    (assert-true (member 1 unreachable))    ; fixnum (index 1) subsumed by integer
    (assert-true (> (length warnings) 0))))

(deftest exhaustiveness-t-after-t-is-unreachable
  "Second T arm is flagged as unreachable."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer t t))
    (assert-true exhaustive-p)
    (assert-true (member 2 unreachable))    ; second T (index 2) unreachable
    (assert-true (> (length warnings) 0))))

(deftest-each exhaustiveness-boundary-cases
  "check-typecase-exhaustiveness: single T arm → exhaustive; empty arms → not exhaustive."
  :cases (("single-T" '(t)  t)
          ("empty"    '()   nil))
  (arms expected-exhaustive)
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness arms)
    (if expected-exhaustive
        (assert-true  exhaustive-p)
        (assert-false exhaustive-p))
    (assert-null unreachable)
    (assert-null warnings)))

;;; ─── check-etypecase-completeness ────────────────────────────────────────

(deftest etypecase-completeness-with-t-is-exhaustive
  "etypecase with T arm is exhaustive — no missing-arm warning."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-etypecase-completeness '(integer string t))
    (assert-true exhaustive-p)
    (assert-null unreachable)
    (assert-null warnings)))

(deftest etypecase-completeness-without-t-warns
  "etypecase without T arm produces a runtime-error warning."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-etypecase-completeness '(integer string))
    (assert-false exhaustive-p)
    (assert-null unreachable)
    (assert-= 1 (length warnings))
    (assert-true (search "etypecase" (first warnings)))))

;;; ─── useful-typecase-arms ─────────────────────────────────────────────────

(deftest useful-arms-removes-subsumed
  "useful-typecase-arms filters out subsumed arms."
  (let ((useful (cl-cc/type:useful-typecase-arms '(integer fixnum string t))))
    ;; fixnum is subsumed by integer, so result is (integer string t)
    (assert-= 3 (length useful))
    (assert-false (member 'fixnum useful))))

(deftest useful-arms-keeps-all-distinct
  "useful-typecase-arms keeps all arms when none are subsumed."
  (let ((useful (cl-cc/type:useful-typecase-arms '(integer string symbol))))
    (assert-= 3 (length useful))))

(deftest useful-arms-empty-input
  "useful-typecase-arms returns nil for empty input."
  (assert-null (cl-cc/type:useful-typecase-arms '())))
