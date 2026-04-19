;;;; tests/unit/expand/macros-hof-search-tests.lisp
;;;; Coverage tests for src/expand/macros-hof-search.lisp
;;;;
;;;; Macros tested: position-if, position-if-not, count-if, count-if-not,
;;;; find-if-not, assoc, assoc-if, assoc-if-not, rassoc-if, rassoc-if-not

(in-package :cl-cc/test)

(in-suite macros-hof-suite)

;;; ── position-if / position-if-not ──────────────────────────────────────────

(deftest position-if-expansion-cases
  "position-if: no-key → let+block; with-key → let."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result))))
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest position-if-not-cases
  "position-if-not: delegates to complement; with-key forwards :key."
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst))))
    (assert-eq 'position-if (car result))
    (assert-equal 'complement (caadr result)))
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst :key #'car))))
    (assert-eq 'position-if (car result))
    (assert-true (member :key result))))

;;; ── count-if / count-if-not ─────────────────────────────────────────────────

(deftest count-if-expansion-cases
  "count-if: no-key → let+dolist; with-key → let."
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result))))
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest count-if-not-cases
  "count-if-not: delegates to complement; with-key forwards :key."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst))))
    (assert-eq 'count-if (car result))
    (assert-equal 'complement (caadr result)))
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq 'count-if (car result))
    (assert-true (member :key result))))

;;; ── find-if-not ─────────────────────────────────────────────────────────────

(deftest find-if-not-cases
  "find-if-not: delegates to complement; with-key forwards :key."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst))))
    (assert-eq 'find-if (car result))
    (assert-equal 'complement (caadr result)))
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (assert-eq 'find-if (car result))
    (assert-true (member :key result))))

;;; ── assoc / assoc-if / assoc-if-not ────────────────────────────────────────

(deftest assoc-expansion-cases
  "assoc: no-key → let+block; with-:test → let+block; assoc-if → let+dolist; assoc-if-not → complement."
  (let ((result (our-macroexpand-1 '(assoc item alist))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result))))
  (let ((result (our-macroexpand-1 '(assoc item alist :test #'equal))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result))))
  (let ((result (our-macroexpand-1 '(assoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result))))
  (let ((result (our-macroexpand-1 '(assoc-if-not #'oddp alist))))
    (assert-eq 'assoc-if (car result))
    (assert-equal 'complement (caadr result))))

;;; ── rassoc-if / rassoc-if-not ───────────────────────────────────────────────

(deftest rassoc-if-cases
  "rassoc-if: let+dolist checking CDR; rassoc-if-not delegates to complement."
  (let ((result (our-macroexpand-1 '(rassoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result))))
  (let ((result (our-macroexpand-1 '(rassoc-if-not #'oddp alist))))
    (assert-eq 'rassoc-if (car result))
    (assert-equal 'complement (caadr result))))
