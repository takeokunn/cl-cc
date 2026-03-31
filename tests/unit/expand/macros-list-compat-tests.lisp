;;;; tests/unit/expand/macros-list-compat-tests.lisp
;;;; Coverage tests for src/expand/macros-list-compat.lisp

(in-package :cl-cc/test)

(defsuite macros-list-compat-suite
  :description "Tests for macros-list-compat.lisp"
  :parent cl-cc-suite)

(in-suite macros-list-compat-suite)

;;; ── SUBST-IF / SUBST-IF-NOT ────────────────────────────────────────────────

(deftest subst-if-expansion
  "SUBST-IF expands to a recursive LABELS walk."
  (let ((result (our-macroexpand-1 '(subst-if new pred tree))))
    (assert-eq 'let (car result))
    (assert-eq 'labels (car (caddr result)))))

(deftest subst-if-not-expansion
  "SUBST-IF-NOT delegates to SUBST-IF with COMPLEMENT."
  (let ((result (our-macroexpand-1 '(subst-if-not new pred tree))))
    (assert-eq (car result) 'subst-if)
    (assert-eq (car (third result)) 'complement)))

;;; ── VECTOR ──────────────────────────────────────────────────────────────────

(deftest vector-expansion
  "VECTOR expands to MAKE-ARRAY or a LET around MAKE-ARRAY."
  (let ((empty-result (our-macroexpand-1 '(vector))))
    (assert-equal empty-result '(make-array 0)))
  (let ((result (our-macroexpand-1 '(vector a b c))))
    (assert-eq 'let (car result))
    (assert-eq 'make-array (car (cadr (car (second result)))))))

;;; ── MEMBER-IF / MEMBER-IF-NOT ───────────────────────────────────────────────

(deftest member-if-outer-is-let
  "MEMBER-IF expands to a LET binding the predicate."
  (assert-eq (car (our-macroexpand-1 '(member-if pred lst))) 'let))

(deftest member-if-body-is-do-loop
  "MEMBER-IF uses a DO loop walking cdr-by-cdr."
  (let* ((result (our-macroexpand-1 '(member-if pred lst)))
         (body (caddr result)))
    (assert-eq (car body) 'do)))

(deftest member-if-not-delegates-to-member-if-complement
  "MEMBER-IF-NOT expands to (member-if (complement pred) list)."
  (let ((result (our-macroexpand-1 '(member-if-not pred lst))))
    (assert-eq (car result) 'member-if)
    (assert-eq (caadr result) 'complement)))

;;; ── MAPHASH ─────────────────────────────────────────────────────────────────

(deftest maphash-expansion
  "MAPHASH expands to a LET that iterates over hash-table keys."
  (let ((result (our-macroexpand-1 '(maphash fn table))))
    (assert-eq 'let (car result))
    (assert-eq 'dolist (car (caddr result)))))
