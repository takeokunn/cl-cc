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
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-plus-times-unary-passthrough
  "1-arg + and * return the argument unchanged."
  :cases (("plus"  '(+ x)  'x)
          ("times" '(* x)  'x))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-plus-times-binary-builtin
  "2-arg + and * produce the binary builtin form."
  :cases (("plus"  '(+ a b) '(+ a b))
          ("times" '(* a b) '(* a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-plus-times-nary-left-fold
  "3-arg + and * produce left-nested binary calls."
  :cases (("plus"  '(+ a b c) '(+ (+ a b) c))
          ("times" '(* a b c) '(* (* a b) c)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

;;; ─── - (subtraction / unary negation) ───────────────────────────────────

(deftest expander-minus-zero-arg-signals-error
  "- with zero arguments signals an error."
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all '(-))))

(deftest expander-minus-unary-is-negation
  "- with one arg rewrites to (- 0 x)."
  (assert-equal '(- 0 7) (cl-cc/expand:compiler-macroexpand-all '(- 7))))

(deftest expander-minus-binary-is-passthrough
  "- with two args passes through unchanged."
  (assert-equal '(- a b) (cl-cc/expand:compiler-macroexpand-all '(- a b))))

(deftest expander-minus-nary-is-left-fold
  "- with three args left-folds into nested binary subtractions."
  (assert-equal '(- (- a b) c) (cl-cc/expand:compiler-macroexpand-all '(- a b c))))

;;; ─── / (division / reciprocal) ───────────────────────────────────────────

(deftest expander-slash-zero-arg-signals-error
  "/ with zero arguments signals an error."
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all '(/))))

(deftest expander-slash-unary-is-reciprocal
  "/ with one arg rewrites to (/ 1 x)."
  (assert-equal '(/ 1 x) (cl-cc/expand:compiler-macroexpand-all '(/ x))))

(deftest expander-slash-binary-is-passthrough
  "/ with two args passes through unchanged."
  (assert-equal '(/ a b) (cl-cc/expand:compiler-macroexpand-all '(/ a b))))

(deftest expander-slash-nary-is-left-fold
  "/ with three args left-folds into nested binary divisions."
  (assert-equal '(/ (/ a b) c) (cl-cc/expand:compiler-macroexpand-all '(/ a b c))))

;;; ─── log ─────────────────────────────────────────────────────────────────

(deftest expander-log-unary-is-passthrough
  "log with one arg passes through unchanged."
  (assert-equal '(log x) (cl-cc/expand:compiler-macroexpand-all '(log x))))

(deftest expander-log-binary-is-change-of-base
  "log with two args expands to (/ (log x) (log base)) change-of-base form."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(log x y))))
    (assert-eq '/ (first result))
    (assert-equal '(log x) (second result))
    (assert-equal '(log y) (third result))))

(deftest expander-log-three-arg-signals-error
  "log with three arguments signals an error."
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all '(log x y z))))

;;; ─── min / max ────────────────────────────────────────────────────────────

(deftest-each expander-min-max-zero-arg-signals-error
  "0-arg min and max signal an error."
  :cases (("min" '(min))
          ("max" '(max)))
  (form)
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-min-max-unary-identity
  "1-arg min and max return the argument unchanged."
  :cases (("min" '(min x) 'x)
          ("max" '(max x) 'x))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-min-max-binary-builtin
  "2-arg min and max produce the binary form."
  :cases (("min" '(min a b) '(min a b))
          ("max" '(max a b) '(max a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-min-max-nary-left-fold
  "3-arg min and max produce a left-nested fold."
  :cases (("min" '(min a b c) 'min)
          ("max" '(max a b c) 'max))
  (form expected-op)
  (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

;;; ─── gcd / lcm ────────────────────────────────────────────────────────────

(deftest-each expander-gcd-lcm-zero-arg-identity
  "0-arg gcd returns 0; 0-arg lcm returns 1."
  :cases (("gcd" '(gcd) 0)
          ("lcm" '(lcm) 1))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-unary-wraps-abs
  "1-arg gcd and lcm wrap the argument in abs."
  :cases (("gcd" '(gcd x) '(abs x))
          ("lcm" '(lcm x) '(abs x)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-binary-builtin
  "2-arg gcd and lcm produce the binary form."
  :cases (("gcd" '(gcd a b) '(gcd a b))
          ("lcm" '(lcm a b) '(lcm a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

;;; ─── float-sign ──────────────────────────────────────────────────────────

(deftest expander-float-sign-unary-is-passthrough
  "float-sign with one arg passes through unchanged."
  (assert-equal '(float-sign x) (cl-cc/expand:compiler-macroexpand-all '(float-sign x))))

(deftest expander-float-sign-binary-scales-abs
  "float-sign with two args expands to (* (float-sign x) (abs y))."
  (let ((result (cl-cc/expand:compiler-macroexpand-all '(float-sign x y))))
    (assert-eq '* (first result))
    (assert-equal '(float-sign x) (second result))
    (assert-equal '(abs y) (third result))))

(deftest expander-float-sign-three-arg-signals-error
  "float-sign with three arguments signals an error."
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all '(float-sign x y z))))

;;; ─── float ───────────────────────────────────────────────────────────────

(deftest expander-float-unary-is-passthrough
  "float with one arg passes through unchanged."
  (assert-equal '(float x) (cl-cc/expand:compiler-macroexpand-all '(float x))))

(deftest expander-float-binary-drops-prototype
  "float with two args drops the prototype, normalizing to (float x)."
  (assert-equal '(float x) (cl-cc/expand:compiler-macroexpand-all '(float x 1.0))))

(deftest expander-float-three-arg-signals-error
  "float with three arguments signals an error."
  (assert-signals error (cl-cc/expand:compiler-macroexpand-all '(float x 1.0 extra))))

;;; ─── logand / logior / logxor / logeqv ──────────────────────────────────

(deftest-each expander-logical-bitwise-zero-arg-identity
  "0-arg bitwise ops return their identity elements."
  :cases (("logand" '(logand) -1)
          ("logior" '(logior) 0)
          ("logxor" '(logxor) 0)
          ("logeqv" '(logeqv) -1))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-unary-passthrough
  "1-arg bitwise ops return the argument unchanged."
  :cases (("logand" '(logand x) 'x)
          ("logior" '(logior x) 'x)
          ("logxor" '(logxor x) 'x)
          ("logeqv" '(logeqv x) 'x))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-binary-builtin
  "2-arg bitwise ops produce the binary form."
  :cases (("logand" '(logand a b) '(logand a b))
          ("logior" '(logior a b) '(logior a b))
          ("logxor" '(logxor a b) '(logxor a b))
          ("logeqv" '(logeqv a b) '(logeqv a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand:compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-nary-left-fold
  "3-arg bitwise ops produce a left-nested fold with the correct op."
  :cases (("logand" '(logand a b c) 'logand)
          ("logior" '(logior a b c) 'logior))
  (form expected-op)
  (let ((result (cl-cc/expand:compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

