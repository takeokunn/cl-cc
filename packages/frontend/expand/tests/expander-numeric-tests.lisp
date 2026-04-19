;;;; tests/unit/expand/expander-numeric-tests.lisp — Numeric expander tests

(in-package :cl-cc/test)

(defsuite expander-numeric-suite
  :description "Numeric expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-numeric-suite)

;;; ─── + and * (variadic fold) ─────────────────────────────────────────────

(deftest-each expander-plus-times-identity
  "0-arg + and * return their identity elements."
  :cases (("plus"  '(+) 0)
          ("times" '(*) 1))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-unary-passthrough
  "1-arg + and * return the argument unchanged."
  :cases (("plus"  '(+ x)  'x)
          ("times" '(* x)  'x))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-binary-builtin
  "2-arg + and * produce the binary builtin form."
  :cases (("plus"  '(+ a b) '(+ a b))
          ("times" '(* a b) '(* a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-nary-left-fold
  "3-arg + and * produce left-nested binary calls."
  :cases (("plus"  '(+ a b c) '(+ (+ a b) c))
          ("times" '(* a b c) '(* (* a b) c)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

;;; ─── - (subtraction / unary negation) ───────────────────────────────────

(deftest expander-minus-cases
  "- : 0-arg→error; 1-arg→(- 0 x); 2-arg→passthrough; 3-arg→left-fold."
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(-)))
  (assert-equal '(- 0 7)     (cl-cc/expand::compiler-macroexpand-all '(- 7)))
  (assert-equal '(- a b)     (cl-cc/expand::compiler-macroexpand-all '(- a b)))
  (assert-equal '(- (- a b) c) (cl-cc/expand::compiler-macroexpand-all '(- a b c))))

;;; ─── / (division / reciprocal) ───────────────────────────────────────────

(deftest expander-slash-cases
  "/ : 0-arg→error; 1-arg→(/ 1 x); 2-arg→passthrough; 3-arg→left-fold."
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(/)))
  (assert-equal '(/ 1 x)       (cl-cc/expand::compiler-macroexpand-all '(/ x)))
  (assert-equal '(/ a b)       (cl-cc/expand::compiler-macroexpand-all '(/ a b)))
  (assert-equal '(/ (/ a b) c) (cl-cc/expand::compiler-macroexpand-all '(/ a b c))))

;;; ─── log ─────────────────────────────────────────────────────────────────

(deftest expander-log-cases
  "log: 1-arg passthrough; 2-arg→(/ (log x) (log y)); 3-arg→error."
  (assert-equal '(log x) (cl-cc/expand::compiler-macroexpand-all '(log x)))
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(log x y))))
    (assert-eq '/ (first result))
    (assert-equal '(log x) (second result))
    (assert-equal '(log y) (third result)))
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(log x y z))))

;;; ─── min / max ────────────────────────────────────────────────────────────

(deftest-each expander-min-max-zero-arg-signals-error
  "0-arg min and max signal an error."
  :cases (("min" '(min))
          ("max" '(max)))
  (form)
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-min-max-unary-identity
  "1-arg min and max return the argument unchanged."
  :cases (("min" '(min x) 'x)
          ("max" '(max x) 'x))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-min-max-binary-builtin
  "2-arg min and max produce the binary form."
  :cases (("min" '(min a b) '(min a b))
          ("max" '(max a b) '(max a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-min-max-nary-left-fold
  "3-arg min and max produce a left-nested fold."
  :cases (("min" '(min a b c) 'min)
          ("max" '(max a b c) 'max))
  (form expected-op)
  (let ((result (cl-cc/expand::compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

;;; ─── gcd / lcm ────────────────────────────────────────────────────────────

(deftest-each expander-gcd-lcm-zero-arg-identity
  "0-arg gcd returns 0; 0-arg lcm returns 1."
  :cases (("gcd" '(gcd) 0)
          ("lcm" '(lcm) 1))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-unary-wraps-abs
  "1-arg gcd and lcm wrap the argument in abs."
  :cases (("gcd" '(gcd x) '(abs x))
          ("lcm" '(lcm x) '(abs x)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-binary-builtin
  "2-arg gcd and lcm produce the binary form."
  :cases (("gcd" '(gcd a b) '(gcd a b))
          ("lcm" '(lcm a b) '(lcm a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

;;; ─── float-sign ──────────────────────────────────────────────────────────

(deftest expander-float-sign-cases
  "float-sign: 1-arg passthrough; 2-arg→(* (float-sign x) (abs y)); 3-arg→error."
  (assert-equal '(float-sign x) (cl-cc/expand::compiler-macroexpand-all '(float-sign x)))
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(float-sign x y))))
    (assert-eq '* (first result))
    (assert-equal '(float-sign x) (second result))
    (assert-equal '(abs y) (third result)))
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(float-sign x y z))))

;;; ─── float ───────────────────────────────────────────────────────────────

(deftest expander-float-cases
  "float: 1-arg passthrough; 2-arg drops prototype; 3-arg→error."
  (assert-equal '(float x) (cl-cc/expand::compiler-macroexpand-all '(float x)))
  (assert-equal '(float x) (cl-cc/expand::compiler-macroexpand-all '(float x 1.0)))
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(float x 1.0 extra))))

;;; ─── logand / logior / logxor / logeqv ──────────────────────────────────

(deftest-each expander-logical-bitwise-zero-arg-identity
  "0-arg bitwise ops return their identity elements."
  :cases (("logand" '(logand) -1)
          ("logior" '(logior) 0)
          ("logxor" '(logxor) 0)
          ("logeqv" '(logeqv) -1))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-unary-passthrough
  "1-arg bitwise ops return the argument unchanged."
  :cases (("logand" '(logand x) 'x)
          ("logior" '(logior x) 'x)
          ("logxor" '(logxor x) 'x)
          ("logeqv" '(logeqv x) 'x))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-binary-builtin
  "2-arg bitwise ops produce the binary form."
  :cases (("logand" '(logand a b) '(logand a b))
          ("logior" '(logior a b) '(logior a b))
          ("logxor" '(logxor a b) '(logxor a b))
          ("logeqv" '(logeqv a b) '(logeqv a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-nary-left-fold
  "3-arg bitwise ops produce a left-nested fold with the correct op."
  :cases (("logand" '(logand a b c) 'logand)
          ("logior" '(logior a b c) 'logior))
  (form expected-op)
  (let ((result (cl-cc/expand::compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

