;;;; tests/unit/type/exhaustiveness-tests.lisp
;;;; FR-1903: Exhaustiveness / Coverage Checking Tests
;;;;
;;;; Tests for check-typecase-exhaustiveness, check-etypecase-completeness,
;;;; useful-typecase-arms, and typecase-arm-subsumed-p.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── typecase-arm-subsumed-p ──────────────────────────────────────────────

(deftest exhaustiveness-subsumed-by-t
  "Any arm type is subsumed by T."
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'integer '(t)))
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'string  '(t)))
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'fixnum  '(t))))

(deftest exhaustiveness-subsumed-by-exact
  "An arm type is subsumed by an identical type."
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'integer '(integer)))
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'string  '(symbol integer string))))

(deftest exhaustiveness-subsumed-by-supertype
  "fixnum is subsumed by integer (nominal subtype relation)."
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'fixnum '(integer)))
  (assert-true (cl-cc/type:typecase-arm-subsumed-p 'cons   '(list))))

(deftest exhaustiveness-not-subsumed-when-unrelated
  "string is not subsumed by integer."
  (assert-false (cl-cc/type:typecase-arm-subsumed-p 'string  '(integer)))
  (assert-false (cl-cc/type:typecase-arm-subsumed-p 'symbol  '(integer string)))
  (assert-false (cl-cc/type:typecase-arm-subsumed-p 'integer nil)))

;;; ─── check-typecase-exhaustiveness ───────────────────────────────────────

(deftest exhaustiveness-exhaustive-with-t
  "A typecase with T at the end is exhaustive."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer string t))
    (assert-true exhaustive-p)
    (assert-null unreachable)
    (assert-null warnings)))

(deftest exhaustiveness-not-exhaustive-without-t
  "A typecase without T is not exhaustive."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(integer string))
    (assert-false exhaustive-p)
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

(deftest exhaustiveness-single-t-is-exhaustive
  "A single T arm alone is exhaustive with no warnings."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '(t))
    (assert-true exhaustive-p)
    (assert-null unreachable)
    (assert-null warnings)))

(deftest exhaustiveness-empty-arms-not-exhaustive
  "Empty arm list is not exhaustive."
  (multiple-value-bind (exhaustive-p unreachable warnings)
      (cl-cc/type:check-typecase-exhaustiveness '())
    (assert-false exhaustive-p)
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
