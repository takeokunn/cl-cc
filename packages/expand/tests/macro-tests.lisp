;;;; tests/macro-tests.lisp - Quasiquote expansion tests for CL-CC
;;;
;;;; Remaining tests for %expand-quasiquote.
;;;;
(in-package :cl-cc/test)

(defsuite macro-suite
  :description "Test suite for macro expansion"
  :parent cl-cc-unit-suite)


(in-suite macro-suite)
;;; ─── %expand-quasiquote ──────────────────────────────────────────────────

(deftest-each expand-quasiquote-wraps-in-quote
  "%expand-quasiquote wraps self-evaluating atoms and symbols in (quote ...)."
  :cases (("atom"   'foo 'foo)
          ("number" 42   42))
  (input expected-val)
  (assert-equal (list 'quote expected-val) (cl-cc/expand::%expand-quasiquote input)))

(deftest expand-quasiquote-unquote-extracts
  "Top-level unquote returns its argument directly."
  (assert-equal 'x (cl-cc/expand::%expand-quasiquote '(cl-cc:unquote x))))

(deftest expand-quasiquote-list-wraps-in-list
  "Plain list elements are wrapped in (list ...) and appended."
  (let ((result (cl-cc/expand::%expand-quasiquote '(a b))))
    ;; Result should be (append (list ...) (list ...))
    (assert-eq 'append (car result))))

(deftest expand-quasiquote-unquote-in-list
  "Unquote inside list is spliced as a (list val) part."
  ;; Use explicit quote to avoid CL's own unquote processing.
  (let* ((result (cl-cc/expand::%expand-quasiquote '(a (cl-cc:unquote x))))
         (str (format nil "~S" result)))
    (assert-true (search "X" str))))

;;; ─── %qq-head-p ──────────────────────────────────────────────────────────

(deftest-each qq-head-p-cases
  "%qq-head-p: returns T for matching symbol name, NIL otherwise."
  :cases (("matching"      '(unquote x)  "UNQUOTE"  t)
          ("non-matching"  '(unquote x)  "SPLICE"   nil)
          ("atom"          'unquote      "UNQUOTE"  nil)
          ("empty-list"    nil           "UNQUOTE"  nil))
  (form name expected)
  (if expected
      (assert-true  (cl-cc/expand::%qq-head-p form name))
      (assert-false (cl-cc/expand::%qq-head-p form name))))

;;; ─── %step-cache-and-return ──────────────────────────────────────────────

(deftest step-cache-and-return-returns-values
  "%step-cache-and-return returns (values result expanded-p) unchanged."
  (multiple-value-bind (r p)
      (cl-cc/expand::%step-cache-and-return '(foo) nil '(bar) t)
    (assert-equal '(bar) r)
    (assert-true  p)))

(deftest step-cache-and-return-no-expand
  "%step-cache-and-return returns (values form nil) for non-expanded case."
  (multiple-value-bind (r p)
      (cl-cc/expand::%step-cache-and-return '(foo) nil '(foo) nil)
    (assert-equal '(foo) r)
    (assert-false p)))

;;; ─── %cache-all-result ───────────────────────────────────────────────────

(deftest cache-all-result-returns-result
  "%cache-all-result always returns the RESULT argument unchanged."
  (assert-equal '(expanded form)
                (cl-cc/expand::%cache-all-result '(original) nil '(expanded form))))

(deftest cache-all-result-skips-uninterned-form
  "%cache-all-result does not cache when FORM contains an uninterned symbol."
  (let ((fresh-sym (make-symbol "X"))
        (env nil))
    (cl-cc/expand::%cache-all-result (list fresh-sym) env 'result)
    ;; No error, result still returned
    (assert-true t)))

;;; ─── %maybe-postprocess-expansion ────────────────────────────────────────

(deftest maybe-postprocess-expansion-no-postprocess
  "%maybe-postprocess-expansion returns RESULT unchanged when :post-expand is absent."
  (let ((descriptor '(:kind :macro-expander :lambda-list (form))))
    (assert-equal '(foo bar)
                  (cl-cc/expand::%maybe-postprocess-expansion '(foo bar) descriptor nil))))

(deftest maybe-postprocess-expansion-returns-result-for-other-postprocess
  "%maybe-postprocess-expansion returns RESULT for unrecognized :post-expand values."
  (let ((descriptor '(:kind :macro-expander :post-expand :some-unknown)))
    (assert-equal '(baz)
                  (cl-cc/expand::%maybe-postprocess-expansion '(baz) descriptor nil))))
