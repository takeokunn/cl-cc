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
  (assert-equal 'x (cl-cc/expand::%expand-quasiquote '(cl-cc::unquote x))))

(deftest expand-quasiquote-list-wraps-in-list
  "Plain list elements are wrapped in (list ...) and appended."
  (let ((result (cl-cc/expand::%expand-quasiquote '(a b))))
    ;; Result should be (append (list ...) (list ...))
    (assert-eq 'append (car result))))

(deftest expand-quasiquote-unquote-in-list
  "Unquote inside list is spliced as a (list val) part."
  ;; Use explicit quote to avoid CL's own unquote processing.
  (let* ((result (cl-cc/expand::%expand-quasiquote '(a (cl-cc::unquote x))))
         (str (format nil "~S" result)))
    (assert-true (search "X" str))))
