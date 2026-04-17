;;;; tests/unit/expand/macro-psetf-tests.lisp — PSETF macro tests

(in-package :cl-cc/test)

(defsuite macro-psetf-suite
  :description "PSETF expansion tests"
  :parent cl-cc-unit-suite)

(in-suite macro-psetf-suite)

(deftest psetf-empty
  "PSETF with no arguments expands to a LET with empty bindings returning nil"
  (let ((result (our-macroexpand-1 '(psetf))))
    (assert-eq (car result) 'let)
    (assert-null (cadr result))
    (assert-null (car (last result)))))

(deftest psetf-structure-cases
  "PSETF with one pair has one temp binding; with two pairs captures both values before any assignment."
  (let ((result (our-macroexpand-1 '(psetf x 10))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 1)
    (assert-= (cadr (caadr result)) 10)
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'x)
    (assert-null (car (last result))))
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 2)
    (assert-= (cadr (first (cadr result))) 1)
    (assert-= (cadr (second (cadr result))) 2)
    (assert-eq (car (caddr result)) 'setf)
    (assert-eq (cadr (caddr result)) 'a)
    (assert-eq (car (cadddr result)) 'setf)
    (assert-eq (cadr (cadddr result)) 'b)
    (assert-null (car (last result)))))

(deftest psetf-odd-args-signals-error
  "PSETF with an odd number of arguments signals an error"
  (assert-signals error (our-macroexpand-1 '(psetf x 1 y))))

(deftest psetf-three-pairs
  "PSETF with three pairs captures all values before all assignments"
  (let ((result (our-macroexpand-1 '(psetf a 1 b 2 c 3))))
    (assert-eq (car result) 'let)
    (assert-= (length (cadr result)) 3)
    (assert-eq (car (caddr result)) 'setf)))

(deftest integration-psetf-full-expansion
  "Full expansion of PSETF: top-level is LET (macro fully expanded)"
  (let ((result (our-macroexpand '(psetf x 1 y 2))))
    (assert-eq (car result) 'let)))
