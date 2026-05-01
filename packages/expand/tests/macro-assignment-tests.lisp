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

(deftest psetq-macro-empty-returns-nil
  "PSETQ with no args expands to nil."
  (assert-null (our-macroexpand-1 '(psetq))))

(deftest-each psetq-macro-nonzero-produces-let
  "PSETQ with bindings expands to LET + SETQ body."
  :cases (("one-pair"   '(psetq a 1))
          ("three-pair" '(psetq a 1 b 2 c 3)))
  (form)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'let (car result))
    (assert-true (consp (cadr result)))))
