;;;; tests/unit/expand/macros-stdlib-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp (remaining forms)

(in-package :cl-cc/test)

(defsuite macros-stdlib-suite
  :description "Tests for macros-stdlib.lisp: remaining forms"
  :parent cl-cc-unit-suite)

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

(deftest fr-839-make-iterator-and-next-over-list
  "MAKE-ITERATOR and ITERATOR-NEXT return values plus has-more-p."
  (let ((iterator (cl-cc/expand:make-iterator '(a b))))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (assert-eq 'a value)
      (assert-true morep))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (assert-eq 'b value)
      (assert-true morep))
    (multiple-value-bind (value morep) (cl-cc/expand:iterator-next iterator)
      (assert-null value)
      (assert-false morep))))

(deftest fr-839-doiterator-expansion-uses-with-iterator
  "DOITERATOR expands through WITH-ITERATOR and ITERATOR-NEXT."
  (let ((result (our-macroexpand-1 '(doiterator (x '(1 2) :done) (print x)))))
    (assert-eq 'cl-cc/expand:with-iterator (car result))
    (assert-true (search "ITERATOR-NEXT" (prin1-to-string result)))))
