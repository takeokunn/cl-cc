;;;; tests/unit/expand/expander-numeric-tests.lisp — Numeric expander tests

(in-package :cl-cc/test)

(defsuite expander-numeric-suite
  :description "Numeric expander unit tests"
  :parent cl-cc-suite)

(in-suite expander-numeric-suite)

;;; ─── + and * (variadic fold) ─────────────────────────────────────────────

(deftest-each expander-plus-times-identity
  "0-arg + and * return their identity elements."
  :cases (("plus"  '(+) 0)
          ("times" '(*) 1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-unary-passthrough
  "1-arg + and * return the argument unchanged."
  :cases (("plus"  '(+ x)  'x)
          ("times" '(* x)  'x))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-binary-builtin
  "2-arg + and * produce the binary builtin form."
  :cases (("plus"  '(+ a b) '(+ a b))
          ("times" '(* a b) '(* a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-plus-times-nary-left-fold
  "3-arg + and * produce left-nested binary calls."
  :cases (("plus"  '(+ a b c) '(+ (+ a b) c))
          ("times" '(* a b c) '(* (* a b) c)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

;;; ─── - (subtraction / unary negation) ───────────────────────────────────

(deftest expander-minus-zero-arg-signals-error
  "0-arg - signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(-))))

(deftest expander-minus-unary-negation
  "1-arg - expands to (- 0 x)."
  (assert-equal '(- 0 7) (cl-cc::compiler-macroexpand-all '(- 7))))

(deftest expander-minus-binary-passthrough
  "2-arg - produces the binary form unchanged."
  (assert-equal '(- a b) (cl-cc::compiler-macroexpand-all '(- a b))))

(deftest expander-minus-nary-left-fold
  "3-arg - produces left-nested subtraction."
  (assert-equal '(- (- a b) c) (cl-cc::compiler-macroexpand-all '(- a b c))))

;;; ─── / (division / reciprocal) ───────────────────────────────────────────

(deftest expander-slash-zero-arg-signals-error
  "0-arg / signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(/))))

(deftest expander-slash-unary-reciprocal
  "1-arg / expands to (/ 1 x)."
  (assert-equal '(/ 1 x) (cl-cc::compiler-macroexpand-all '(/ x))))

(deftest expander-slash-binary-passthrough
  "2-arg / produces the binary form unchanged."
  (assert-equal '(/ a b) (cl-cc::compiler-macroexpand-all '(/ a b))))

(deftest expander-slash-nary-left-fold
  "3-arg / produces left-nested division."
  (assert-equal '(/ (/ a b) c) (cl-cc::compiler-macroexpand-all '(/ a b c))))

;;; ─── log ─────────────────────────────────────────────────────────────────

(deftest expander-log-unary-natural
  "1-arg log passes through as the natural logarithm."
  (assert-equal '(log x) (cl-cc::compiler-macroexpand-all '(log x))))

(deftest expander-log-binary-change-of-base
  "2-arg log expands to (/ (log x) (log y))."
  (let ((result (cl-cc::compiler-macroexpand-all '(log x y))))
    (assert-eq '/ (first result))
    (assert-equal '(log x) (second result))
    (assert-equal '(log y) (third result))))

(deftest expander-log-three-arg-signals-error
  "3-arg log signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(log x y z))))

;;; ─── min / max ────────────────────────────────────────────────────────────

(deftest-each expander-min-max-zero-arg-signals-error
  "0-arg min and max signal an error."
  :cases (("min" '(min))
          ("max" '(max)))
  (form)
  (assert-signals error (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-min-max-unary-identity
  "1-arg min and max return the argument unchanged."
  :cases (("min" '(min x) 'x)
          ("max" '(max x) 'x))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-min-max-binary-builtin
  "2-arg min and max produce the binary form."
  :cases (("min" '(min a b) '(min a b))
          ("max" '(max a b) '(max a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-min-max-nary-left-fold
  "3-arg min and max produce a left-nested fold."
  :cases (("min" '(min a b c) 'min)
          ("max" '(max a b c) 'max))
  (form expected-op)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

;;; ─── gcd / lcm ────────────────────────────────────────────────────────────

(deftest-each expander-gcd-lcm-zero-arg-identity
  "0-arg gcd returns 0; 0-arg lcm returns 1."
  :cases (("gcd" '(gcd) 0)
          ("lcm" '(lcm) 1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-unary-wraps-abs
  "1-arg gcd and lcm wrap the argument in abs."
  :cases (("gcd" '(gcd x) '(abs x))
          ("lcm" '(lcm x) '(abs x)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-gcd-lcm-binary-builtin
  "2-arg gcd and lcm produce the binary form."
  :cases (("gcd" '(gcd a b) '(gcd a b))
          ("lcm" '(lcm a b) '(lcm a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

;;; ─── float-sign ──────────────────────────────────────────────────────────

(deftest expander-float-sign-unary
  "1-arg float-sign passes through unchanged."
  (assert-equal '(float-sign x) (cl-cc::compiler-macroexpand-all '(float-sign x))))

(deftest expander-float-sign-binary
  "2-arg float-sign expands to (* (float-sign x) (abs y))."
  (let ((result (cl-cc::compiler-macroexpand-all '(float-sign x y))))
    (assert-eq '* (first result))
    (assert-equal '(float-sign x) (second result))
    (assert-equal '(abs y) (third result))))

(deftest expander-float-sign-three-arg-signals-error
  "3-arg float-sign signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(float-sign x y z))))

;;; ─── float ───────────────────────────────────────────────────────────────

(deftest expander-float-unary
  "1-arg float passes through."
  (assert-equal '(float x) (cl-cc::compiler-macroexpand-all '(float x))))

(deftest expander-float-binary-drops-prototype
  "2-arg float drops the prototype and produces (float number)."
  (assert-equal '(float x) (cl-cc::compiler-macroexpand-all '(float x 1.0))))

(deftest expander-float-three-arg-signals-error
  "3-arg float signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(float x 1.0 extra))))

;;; ─── logand / logior / logxor / logeqv ──────────────────────────────────

(deftest-each expander-logical-bitwise-zero-arg-identity
  "0-arg bitwise ops return their identity elements."
  :cases (("logand" '(logand) -1)
          ("logior" '(logior) 0)
          ("logxor" '(logxor) 0)
          ("logeqv" '(logeqv) -1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-unary-passthrough
  "1-arg bitwise ops return the argument unchanged."
  :cases (("logand" '(logand x) 'x)
          ("logior" '(logior x) 'x)
          ("logxor" '(logxor x) 'x)
          ("logeqv" '(logeqv x) 'x))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-binary-builtin
  "2-arg bitwise ops produce the binary form."
  :cases (("logand" '(logand a b) '(logand a b))
          ("logior" '(logior a b) '(logior a b))
          ("logxor" '(logxor a b) '(logxor a b))
          ("logeqv" '(logeqv a b) '(logeqv a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-logical-bitwise-nary-left-fold
  "3-arg bitwise ops produce a left-nested fold with the correct op."
  :cases (("logand" '(logand a b c) 'logand)
          ("logior" '(logior a b c) 'logior))
  (form expected-op)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq expected-op (car result))
    (assert-true (consp (second result)))
    (assert-eq expected-op (car (second result)))))

;;; ─── = / < / > / <= / >= (numeric comparison chaining) ─────────────────

(deftest-each expander-numeric-cmp-zero-arg-error
  "0-arg numeric comparison operators signal an error."
  :cases (("eq"  '(=))
          ("lt"  '(<))
          ("gt"  '(>))
          ("lte" '(<=))
          ("gte" '(>=)))
  (form)
  (assert-signals error (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-numeric-cmp-unary-returns-t
  "1-arg numeric comparison operators return T."
  :cases (("eq"  '(= x)  t)
          ("lt"  '(< x)  t)
          ("gt"  '(> x)  t)
          ("lte" '(<= x) t)
          ("gte" '(>= x) t))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-numeric-cmp-binary-builtin
  "2-arg numeric comparison operators produce the binary form."
  :cases (("eq"  '(= a b)  '(= a b))
          ("lt"  '(< a b)  '(< a b))
          ("lte" '(<= a b) '(<= a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest expander-numeric-cmp-nary-chain
  "3-arg = expands into a chain of pairwise comparisons."
  (let ((result (cl-cc::compiler-macroexpand-all '(= a b c))))
    ;; let-binding wrapper with AND/IF chain
    (assert-true (member (car result) '(let if and)))))

;;; ─── /= (not-equal, all-distinct) ────────────────────────────────────────

(deftest expander-neq-zero-arg-signals-error
  "0-arg /= signals an error."
  (assert-signals error (cl-cc::compiler-macroexpand-all '(/=))))

(deftest expander-neq-unary-returns-t
  "1-arg /= returns T."
  (assert-equal t (cl-cc::compiler-macroexpand-all '(/= x))))

(deftest expander-neq-binary-expands-to-not-eq
  "2-arg /= expands to (not (= a b))."
  (let ((result (cl-cc::compiler-macroexpand-all '(/= a b))))
    (assert-eq 'not (first result))
    (assert-eq '= (car (second result)))))

(deftest expander-neq-nary-generates-let-with-pairs
  "3-arg /= expands to nested IF checks over pairwise inequality."
  (let ((result (cl-cc::compiler-macroexpand-all '(/= a b c))))
    ;; Top level must be (let ...)
    (assert-eq 'let (first result))
    ;; Body should start with nested IF checks over pairwise (/=) expansion.
    (let ((body (third result)))
      (assert-eq 'if (first body)))))

;;; ─── char comparison chaining ────────────────────────────────────────────

(deftest-each expander-char-cmp-zero-arg-error
  "0-arg character comparison operators signal an error."
  :cases (("char="  '(char=))
          ("char<"  '(char<))
          ("char<=" '(char<=)))
  (form)
  (assert-signals error (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-char-cmp-unary-returns-t
  "1-arg character comparison operators return T."
  :cases (("char="      '(char= c)      t)
          ("char-equal" '(char-equal c) t))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

(deftest-each expander-char-cmp-binary-builtin
  "2-arg character comparison operators produce the binary form."
  :cases (("char="  '(char= a b)  '(char= a b))
          ("char<"  '(char< a b)  '(char< a b))
          ("char-lessp" '(char-lessp a b) '(char-lessp a b)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))

;;; ─── expander-numeric-identity-and-unary (original tests kept) ──────────

(deftest expander-numeric-comparison-chain-uses-temporaries
  "Variadic comparisons expand into chained comparisons with temporary bindings."
  (let ((result (cl-cc::compiler-macroexpand-all '(= a b c))))
    (assert-eq 'let (car result))
    (let ((body (caddr result)))
      (assert-eq 'if (car body))
      (assert-eq '= (car (second body)))
      (assert-eq '= (car (third body))))))

(deftest-each expander-numeric-arity-normalization
  "log, float-sign, and float normalize their arities."
  :cases (("log-1"        '(log x)         '(log x))
          ("log-2"        '(log x y)        '(/ (log x) (log y)))
          ("float-sign-2" '(float-sign x y) '(* (float-sign x) (abs y)))
          ("float-2"      '(float x y)      '(float x)))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))
