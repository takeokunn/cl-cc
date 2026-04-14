;;;; tests/unit/expand/expander-definitions-rounding-tests.lisp — Definition-form rounding tests

(in-package :cl-cc/test)

(defsuite expander-definitions-rounding-suite :description "Definition-form rounding unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-definitions-rounding-suite)
(deftest expander-rounding-one-arg-normalization
  "1-arg rounding forms are normalized to 2-arg (op n 1) for all *rounding-ops*."
  (let ((cases '((floor . (floor n))
                 (ceiling . (ceiling n))
                 (truncate . (truncate n))
                 (round . (round n)))))
    (dolist (case cases)
      (let ((result (cl-cc::compiler-macroexpand-all (cdr case))))
        (assert-eq (car case) (car result))
        (assert-eq 'n (second result))
        (assert-equal 1 (third result))))))

(deftest expander-floor-two-arg-unchanged
  "compiler-macroexpand-all: (floor x d) with explicit divisor passes through."
  (let ((result (cl-cc::compiler-macroexpand-all '(floor n 3))))
    (assert-eq 'floor (car result))
    (assert-eq 'n (second result))
    (assert-equal 3 (third result))))
