;;;; tests/unit/expand/macros-stdlib-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp (remaining forms)

(in-package :cl-cc/test)

(defsuite macros-stdlib-suite
  :description "Tests for macros-stdlib.lisp: remaining forms"
  :parent cl-cc-suite)

(in-suite macros-stdlib-suite)

(deftest with-open-stream-expansion
  "WITH-OPEN-STREAM introduces LET and UNWIND-PROTECT."
  (let ((result (our-macroexpand-1 '(with-open-stream (s stream) (write-char #\x s)))))
    (assert-eq 'let (car result))
    (assert-eq 'unwind-protect (car (caddr result)))))

(deftest prog-expansion
  "PROG and PROG* expand to BLOCK NIL with LET / LET*."
  (let ((prog-result (our-macroexpand-1 '(prog ((x 1)) x)))
        (prog*-result (our-macroexpand-1 '(prog* ((x 1)) x))))
    (assert-eq 'block (car prog-result))
    (assert-eq 'let (car (caddr prog-result)))
    (assert-eq 'block (car prog*-result))
    (assert-eq 'let* (car (caddr prog*-result)))))

(deftest destructuring-bind-expansion
  "DESTRUCTURING-BIND delegates to the lambda-list destructurer."
  (let ((result (our-macroexpand-1 '(destructuring-bind (a b) expr a))))
    (assert-eq 'let (car result))
    (assert-eq 'let* (car (caddr result)))))
