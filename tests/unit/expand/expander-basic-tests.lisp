;;;; tests/unit/expand/expander-basic-tests.lisp — Basic expander form tests

(in-package :cl-cc/test)

(defsuite expander-basic-suite
  :description "Basic expander unit tests"
  :parent cl-cc-suite)

(in-suite expander-basic-suite)

(deftest expand-apply-named-fn-binary
  "expand-apply-named-fn for a binary builtin (cons) normalises to (apply #'cons ...)."
  (let ((result (cl-cc::expand-apply-named-fn 'cons 'args)))
    (assert-eq 'apply (car result))
    (assert-eq 'function (caadr result))
    (assert-eq 'cons (cadadr result))
    (assert-eq 'args (third result))))

(deftest expand-apply-named-fn-variadic-plus
  "expand-apply-named-fn for + generates a fold loop (not (apply #'+ ...))."
  (let* ((result (cl-cc::expand-apply-named-fn '+ 'args))
         (str    (format nil "~S" result)))
    (assert-false (search "(APPLY" str))))

(deftest-each expander-function-builtin-wraps-lambda
  "#'builtin always expands to a lambda; arity matches the builtin type."
  :cases (("binary"   'cons 2)
          ("unary"    'car  1)
          ("variadic" '+    nil))
  (name expected-arity)
  (let ((result (assert-expansion-head `(function ,name) 'lambda)))
    (when expected-arity
      (assert-equal expected-arity (length (second result))))))

(deftest expander-function-non-builtin-passthrough
  "compiler-macroexpand-all: #'user-fn (not a builtin) passes through as (function user-fn)."
  (let ((result (assert-expansion-head '(function my-user-defined-fn) 'function)))
    (assert-eq 'my-user-defined-fn (second result))))

(deftest-each expander-funcall-quoted-to-direct-call
  "(funcall 'name ...) becomes (name ...) for both user-defined and builtin functions.
Uses a distinctive unique symbol for the user-fn case so prior tests can't
have seeded a compiler-macro / type annotation for common names like 'foo
that would trip the expander during recursion."
  :cases (("user-fn" '(funcall 'my-unique-test-fn x) 'my-unique-test-fn 'x)
          ("builtin" '(funcall 'car lst)             'car               'lst))
  (form expected-head expected-arg)
  (let ((result (assert-expansion-head form expected-head)))
    (assert-eq expected-arg  (second result))))

(deftest-each expander-make-hash-table-adjusts-test
  "make-hash-table normalizes #'fn tests to quoted symbols."
  :cases (("function" '(make-hash-table :test #'equal))
          ("quoted"   '(make-hash-table :test 'eql)))
  (form)
  (let ((result (assert-expansion-head form 'make-hash-table)))
    (assert-eq :test (second result))))
