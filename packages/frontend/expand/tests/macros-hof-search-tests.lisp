;;;; tests/unit/expand/macros-hof-search-tests.lisp
;;;; Coverage tests for src/expand/macros-hof-search.lisp
;;;;
;;;; Macros tested: position-if, position-if-not, count-if, count-if-not,
;;;; find-if-not, assoc, assoc-if, assoc-if-not, rassoc-if, rassoc-if-not

(in-package :cl-cc/test)

(in-suite macros-hof-suite)

;;; ── position-if / position-if-not ──────────────────────────────────────────

(deftest position-if-expands-to-let
  "position-if without :key expands to a LET with a BLOCK inside."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest position-if-with-key-expands-to-let
  "position-if with :key expands to a LET (uses key-expansion helper)."
  (let ((result (our-macroexpand-1 '(position-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest position-if-not-delegates-to-complement
  "position-if-not expands to (position-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst))))
    (assert-eq 'position-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest position-if-not-with-key-includes-key
  "position-if-not with :key forwards :key to position-if."
  (let ((result (our-macroexpand-1 '(position-if-not #'oddp lst :key #'car))))
    (assert-eq 'position-if (car result))
    (assert-true (member :key result))))

;;; ── count-if / count-if-not ─────────────────────────────────────────────────

(deftest count-if-expands-to-let
  "count-if without :key expands to a LET with DOLIST inside."
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest count-if-with-key-expands-to-let
  "count-if with :key expands to a LET (uses key-expansion path)."
  (let ((result (our-macroexpand-1 '(count-if #'oddp lst :key #'car))))
    (assert-eq 'let (car result))))

(deftest count-if-not-delegates-to-complement
  "count-if-not expands to (count-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst))))
    (assert-eq 'count-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest count-if-not-with-key-includes-key
  "count-if-not with :key forwards :key to count-if."
  (let ((result (our-macroexpand-1 '(count-if-not #'oddp lst :key #'car))))
    (assert-eq 'count-if (car result))
    (assert-true (member :key result))))

;;; ── find-if-not ─────────────────────────────────────────────────────────────

(deftest find-if-not-delegates-to-complement
  "find-if-not without :key expands to (find-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst))))
    (assert-eq 'find-if (car result))
    (assert-equal 'complement (caadr result))))

(deftest find-if-not-with-key-forwards-key
  "find-if-not with :key forwards :key to find-if."
  (let ((result (our-macroexpand-1 '(find-if-not #'oddp lst :key #'car))))
    (assert-eq 'find-if (car result))
    (assert-true (member :key result))))

;;; ── assoc / assoc-if / assoc-if-not ────────────────────────────────────────

(deftest assoc-no-keys-expands-to-let-eql
  "assoc without keyword args expands to a LET with EQL test inside BLOCK."
  (let ((result (our-macroexpand-1 '(assoc item alist))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest assoc-with-test-expands-to-let-block
  "assoc with :test expands to a LET with BLOCK containing FUNCALL."
  (let ((result (our-macroexpand-1 '(assoc item alist :test #'equal))))
    (assert-eq 'let (car result))
    (assert-eq 'block (car (caddr result)))))

(deftest assoc-if-expands-to-let
  "assoc-if expands to a LET with DOLIST iterating the alist."
  (let ((result (our-macroexpand-1 '(assoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest assoc-if-not-delegates-to-complement
  "assoc-if-not expands to (assoc-if (complement pred) alist)."
  (let ((result (our-macroexpand-1 '(assoc-if-not #'oddp alist))))
    (assert-eq 'assoc-if (car result))
    (assert-equal 'complement (caadr result))))

;;; ── rassoc-if / rassoc-if-not ───────────────────────────────────────────────

(deftest rassoc-if-expands-to-let
  "rassoc-if expands to a LET with DOLIST checking each pair's CDR."
  (let ((result (our-macroexpand-1 '(rassoc-if #'oddp alist))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))

(deftest rassoc-if-not-delegates-to-complement
  "rassoc-if-not expands to (rassoc-if (complement pred) alist)."
  (let ((result (our-macroexpand-1 '(rassoc-if-not #'oddp alist))))
    (assert-eq 'rassoc-if (car result))
    (assert-equal 'complement (caadr result))))
