;;;; tests/unit/expand/macros-hof-search-tests.lisp
;;;; Coverage tests for src/expand/macros-hof-search.lisp
;;;;
;;;; Macros tested: position-if, position-if-not, count-if, count-if-not,
;;;; find-if-not, assoc, assoc-if, assoc-if-not, rassoc-if, rassoc-if-not

(in-package :cl-cc/test)

(in-suite macros-hof-suite)

;;; ── position-if / position-if-not ──────────────────────────────────────────

(deftest position-if-without-key-uses-block
  "position-if without :key expands to let+block for indexed scan."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest position-if-with-key-expands-to-let
  "position-if with :key expands to a let (no block needed for key path)."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest position-if-not-delegates-to-complement
  "position-if-not without :key delegates to position-if with complement."
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst))))
    (assert-eq 'position-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest position-if-not-with-key-forwards-key
  "position-if-not with :key forwards the :key argument to position-if."
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst :key #'car))))
    (assert-eq 'position-if (car result))
    (assert-true (member :key result))))

;;; ── count-if / count-if-not ─────────────────────────────────────────────────

(deftest count-if-without-key-uses-dolist
  "count-if without :key expands to let+dolist for counting pass."
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest count-if-with-key-expands-to-let
  "count-if with :key expands to a let (key application inline)."
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest count-if-not-delegates-to-complement
  "count-if-not without :key delegates to count-if with complement."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst))))
    (assert-eq 'count-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest count-if-not-with-key-forwards-key
  "count-if-not with :key forwards the :key argument to count-if."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq 'count-if (car result))
    (assert-true (member :key result))))

;;; ── find-if-not ─────────────────────────────────────────────────────────────

(deftest find-if-not-delegates-to-complement
  "find-if-not without :key delegates to find-if with complement."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst))))
    (assert-eq 'find-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest find-if-not-with-key-forwards-key
  "find-if-not with :key forwards the :key argument to find-if."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (assert-eq 'find-if (car result))
    (assert-true (member :key result))))

;;; ── assoc / assoc-if / assoc-if-not ────────────────────────────────────────

(deftest assoc-without-test-uses-block
  "assoc without :test expands to let+block for linear alist scan."
  (let ((result (our-macroexpand-1 '(assoc item alist))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest assoc-with-test-also-uses-block
  "assoc with :test still expands to let+block (test applied inside)."
  (let ((result (our-macroexpand-1 '(assoc item alist :test #'equal))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest assoc-if-uses-dolist
  "assoc-if expands to let+dolist for predicate-based alist scan."
  (let ((result (our-macroexpand-1 '(assoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest assoc-if-not-delegates-to-complement
  "assoc-if-not delegates to assoc-if with complement."
  (let ((result (our-macroexpand-1 '(assoc-if-not #'oddp alist))))
    (assert-eq 'assoc-if (car result))
    (assert-equal 'complement (caadr result))))

;;; ── rassoc-if / rassoc-if-not ───────────────────────────────────────────────

(deftest rassoc-if-uses-dolist
  "rassoc-if expands to let+dolist that checks the CDR of each pair."
  (let ((result (our-macroexpand-1 '(rassoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest rassoc-if-not-delegates-to-complement
  "rassoc-if-not delegates to rassoc-if with complement."
  (let ((result (our-macroexpand-1 '(rassoc-if-not #'oddp alist))))
    (assert-eq 'rassoc-if (car result))
    (assert-equal 'complement (caadr result))))
