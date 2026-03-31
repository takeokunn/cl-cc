;;;; tests/unit/expand/expander-numeric-tests.lisp — Numeric expander tests

(in-package :cl-cc/test)

(defsuite expander-numeric-suite
  :description "Numeric expander unit tests"
  :parent cl-cc-suite)

(in-suite expander-numeric-suite)

(deftest-each expander-numeric-identity-and-unary
  "Variadic numeric expanders normalize identity and unary forms."
  :cases (("plus"   '(+)    0)
          ("times"  '(*)    1)
          ("minus"  '(- 7)  '(- 0 7))
          ("slash"  '(/ 7)  '(/ 1 7))
          ("gcd"    '(gcd)  0)
          ("lcm"    '(lcm)  1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest expander-numeric-comparison-chain-uses-temporaries
  "Variadic comparisons expand into chained comparisons with temporary bindings."
  (let ((result (cl-cc::compiler-macroexpand-all '(= a b c))))
    (assert-eq 'let (car result))
    (let ((body (caddr result)))
      (assert-eq 'if (car body))
      (assert-eq '= (car (second body)))
      (assert-eq '= (car (third body))))))

(deftest expander-numeric-log-float-sign-and-float
  "Log, float-sign, and float normalize their arities."
  (assert-equal '(log x) (cl-cc::compiler-macroexpand-all '(log x)))
  (assert-equal '(/ (log x) (log y))
                (cl-cc::compiler-macroexpand-all '(log x y)))
  (assert-equal '(* (float-sign x) (abs y))
                (cl-cc::compiler-macroexpand-all '(float-sign x y)))
  (assert-equal '(float x) (cl-cc::compiler-macroexpand-all '(float x y))))
