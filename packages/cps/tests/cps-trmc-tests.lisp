;;;; packages/cps/tests/cps-trmc-tests.lisp
;;;; Unit tests for the TRMC (Tail Recursion Modulo Cons) transformation
;;;; functions defined in packages/cps/src/cps.lisp.
;;;;
;;;; These tests exercise the transformation functions directly, not via
;;;; run-string or the full compilation pipeline.

(in-package :cl-cc/test)

(defsuite cps-trmc-suite
  :description "TRMC transformation unit tests"
  :parent cl-cc-unit-suite)

(in-suite cps-trmc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers (local to this suite)
;;; ─────────────────────────────────────────────────────────────────────────

(defun %trmc-contains-p (form sym)
  "Return T if SYM appears anywhere in FORM (recursive tree walk)."
  (cond ((eq form sym) t)
        ((consp form) (or (%trmc-contains-p (car form) sym)
                          (%trmc-contains-p (cdr form) sym)))
        (t nil)))

(defun %trmc-rewritten-p (source rewritten)
  "Return T when REWRITTEN has the accumulator-worker shape expected of TRMC output."
  (and (not (equal source rewritten))
       (%trmc-contains-p rewritten 'labels)
       (or (%trmc-contains-p rewritten 'nreverse)
           (%trmc-contains-p rewritten 'nreconc))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 1 — Simple cons tail-recursive pattern produces labels/accumulator
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-trmc-simple-cons-is-transformed
  "trmc-transform-defun-form rewrites (cons x (self ...)) into a labels worker."
  (let* ((source '(defun trmc-simple (n)
                   (if (zerop n)
                       nil
                       (cons n (trmc-simple (1- n))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    ;; The output must differ from the input and carry the accumulator shape.
    (assert-true (%trmc-rewritten-p source rewritten))
    ;; The rewritten form is still a DEFUN with the same name.
    (assert-equal 'defun (first rewritten))
    (assert-equal 'trmc-simple (second rewritten))
    ;; Evaluating and calling the rewritten function gives the correct result.
    (eval rewritten)
    (assert-equal '(3 2 1) (funcall (symbol-function 'trmc-simple) 3))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 2 — Pure tail recursion (no cons) is NOT transformed
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-trmc-pure-tail-recursion-not-transformed
  "trmc-transform-defun-form must leave pure tail-recursive functions alone."
  (let* ((source '(defun trmc-sum (n acc)
                   (if (zerop n)
                       acc
                       (trmc-sum (1- n) (+ acc n)))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    ;; No TRMC pattern found — form is returned unchanged.
    (assert-equal source rewritten)
    ;; Sanity-check: no labels injected.
    (assert-false (%trmc-contains-p rewritten 'labels))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 3 — Multi-head cons chain (nested cons) is transformed correctly
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-trmc-multi-head-cons-chain-transformed
  "trmc-transform-defun-form handles (cons a (cons b (self ...))) correctly."
  (let* ((source '(defun trmc-two-heads (n)
                   (if (zerop n)
                       nil
                       (cons n (cons (- n) (trmc-two-heads (1- n)))))))
         (rewritten (cl-cc/cps::trmc-transform-defun-form source)))
    (assert-true (%trmc-rewritten-p source rewritten))
    ;; Running the rewritten function produces the right flat list.
    (eval rewritten)
    (assert-equal '(2 -2 1 -1) (funcall (symbol-function 'trmc-two-heads) 2))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 4 — Idempotency: applying the transform twice gives the same result
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-trmc-idempotent
  "Applying trmc-transform-defun-form twice produces an identical output."
  (let* ((source '(defun trmc-idem (n)
                   (if (zerop n)
                       nil
                       (cons n (trmc-idem (1- n))))))
         (once   (cl-cc/cps::trmc-transform-defun-form source))
         (twice  (cl-cc/cps::trmc-transform-defun-form once)))
    ;; The first application transforms the form.
    (assert-true (%trmc-rewritten-p source once))
    ;; The second application must not alter the already-transformed form.
    ;; After the first pass the self-recursive call is to the worker gensym,
    ;; which does not match the outer name, so the pass is a no-op.
    (assert-equal once twice)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 5 — Edge case: *enable-trmc* nil disables the transformation
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-trmc-disabled-by-parameter
  "When *enable-trmc* is nil, trmc-transform-defun-form is a no-op."
  (let ((source '(defun trmc-disabled (n)
                  (if (zerop n)
                      nil
                      (cons n (trmc-disabled (1- n)))))))
    (let ((cl-cc/cps:*enable-trmc* nil))
      (assert-equal source (cl-cc/cps::trmc-transform-defun-form source)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Test 6 — Edge cases: lambda-lists containing non-symbol elements are skipped
;;; ─────────────────────────────────────────────────────────────────────────
;;;
;;; The guard in trmc-transform-defun-form is (every #'symbolp lambda-list).
;;; Lambda-list elements that are lists (default forms) cause the guard to
;;; fire, leaving the form untouched.  Also covers non-defun forms.

(deftest-each cps-trmc-non-simple-lambda-list-skipped
  "trmc-transform-defun-form leaves forms with non-symbol lambda-list elements unchanged."
  :cases
  (("optional-with-default"
    '(defun trmc-opt (n &optional (acc nil))
      (if (zerop n)
          acc
          (cons n (trmc-opt (1- n))))))

   ("key-with-default"
    '(defun trmc-key (n &key (step 1))
      (if (zerop n)
          nil
          (cons n (trmc-key (- n step))))))

   ("not-a-defun"
    '(defmacro trmc-mac (n)
      `(cons ,n (trmc-mac (1- ,n))))))
  (source)
  (assert-equal source (cl-cc/cps::trmc-transform-defun-form source)))
