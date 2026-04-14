;;;; tests/unit/expand/macro-assignment-tests.lisp — Macro assignment tests

(in-package :cl-cc/test)

(defsuite macro-assignment-suite
  :description "Macro assignment expansion tests"
  :parent cl-cc-unit-suite)


(in-suite macro-assignment-suite)
(deftest setf-macro-simple-symbol
  "Test SETF with simple symbol"
  (assert-equal (our-macroexpand-1 '(setf x 10))
                '(setq x 10)))

(deftest-each setf-macro-unknown-place-errors
  "SETF signals an error for place forms with unknown accessor names."
  :cases (("car-place"  '(setf (car x)  10))
          ("cons-place" '(setf (cons x) 10)))
  (form)
  (assert-signals error (our-macroexpand-1 form)))


(deftest psetq-macro-full-expansion
  "Test full expansion of PSETQ"
  (let ((result (our-macroexpand '(psetq a 1 b 2))))
    ;; Expansion: (let ((#:A 1) (#:B 2)) (setq a #:A) (setq b #:B) nil)
    (assert-eq (car result) 'let)
    (assert-true (consp (cadr result)))
    ;; Body forms are SETQ (not wrapped in PROGN)
    (assert-eq (car (caddr result)) 'setq)))

(deftest psetq-macro-behavior
  "PSETQ: empty expands to nil; 1-pair and N-pair forms produce outer LET with bindings, body starting with SETQ."
  (assert-equal (our-macroexpand-1 '(psetq))
                nil)
  (let ((result (our-macroexpand-1 '(psetq a 1))))
    (assert-eq 'let (car result))
    (assert-true (consp (cadr result)))
    (assert-eq 'setq (car (caddr result))))
  (let ((result (our-macroexpand-1 '(psetq a 1 b 2 c 3))))
    (assert-eq 'let (car result))
    (assert-true (consp (cadr result)))))
