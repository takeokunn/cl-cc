;;;; tests/unit/expand/expander-comparison-tests.lisp — Numeric and character comparison expander tests

(in-package :cl-cc/test)

(defsuite expander-comparison-suite
  :description "Numeric and character comparison expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-comparison-suite)

;;; ─── = / < / > / <= / >= (numeric comparison chaining) ─────────────────

(deftest-each expander-numeric-cmp-zero-arg-error
  "0-arg numeric comparison operators signal an error."
  :cases (("eq"  '(=))
          ("lt"  '(<))
          ("gt"  '(>))
          ("lte" '(<=))
          ("gte" '(>=)))
  (form)
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-numeric-cmp-unary-returns-t
  "1-arg numeric comparison operators return T."
  :cases (("eq"  '(= x)  t)
          ("lt"  '(< x)  t)
          ("gt"  '(> x)  t)
          ("lte" '(<= x) t)
          ("gte" '(>= x) t))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-numeric-cmp-binary-builtin
  "2-arg numeric comparison operators produce the binary form."
  :cases (("eq"  '(= a b)  '(= a b))
          ("lt"  '(< a b)  '(< a b))
          ("lte" '(<= a b) '(<= a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest expander-numeric-cmp-nary-chain
  "3-arg = expands into a chain of pairwise comparisons."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(= a b c))))
    ;; let-binding wrapper with AND/IF chain
    (assert-true (member (car result) '(let if and)))))

;;; ─── /= (not-equal, all-distinct) ────────────────────────────────────────

(deftest expander-neq-zero-arg-signals-error
  "0-arg /= signals an error."
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all '(/=))))

(deftest expander-neq-unary-returns-t
  "1-arg /= returns T."
  (assert-equal t (cl-cc/expand::compiler-macroexpand-all '(/= x))))

(deftest expander-neq-binary-expands-to-not-eq
  "2-arg /= expands to (not (= a b))."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(/= a b))))
    (assert-eq 'not (first result))
    (assert-eq '= (car (second result)))))

(deftest expander-neq-nary-generates-let-with-pairs
  "3-arg /= expands to nested IF checks over pairwise inequality."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(/= a b c))))
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
  (assert-signals error (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-char-cmp-unary-returns-t
  "1-arg character comparison operators return T."
  :cases (("char="      '(char= c)      t)
          ("char-equal" '(char-equal c) t))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

(deftest-each expander-char-cmp-binary-builtin
  "2-arg character comparison operators produce the binary form."
  :cases (("char="  '(char= a b)  '(char= a b))
          ("char<"  '(char< a b)  '(char< a b))
          ("char-lessp" '(char-lessp a b) '(char-lessp a b)))
  (form expected)
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))

;;; ─── expander-numeric-identity-and-unary (original tests kept) ──────────

(deftest expander-numeric-comparison-chain-uses-temporaries
  "Variadic comparisons expand into chained comparisons with temporary bindings."
  (let ((result (cl-cc/expand::compiler-macroexpand-all '(= a b c))))
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
  (assert-equal expected (cl-cc/expand::compiler-macroexpand-all form)))
