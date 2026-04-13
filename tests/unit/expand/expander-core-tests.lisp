;;;; tests/unit/expand/expander-core-tests.lisp — Core expander helper tests

(in-package :cl-cc/test)

(defsuite expander-core-suite :description "Core expander helper unit tests"
  :parent cl-cc-suite)


(in-suite expander-core-suite)
(deftest-each reduce-variadic-op
  "reduce-variadic-op builds left-nested call trees for any arity."
  :cases (("zero-plus"  '+ nil       0 0)
          ("zero-mul"   '* nil       1 1)
          ("one-arg"    '+ '(x)      0 'x)
          ("two-args"   '+ '(a b)    0 '(+ a b))
          ("three-args" '+ '(a b c)  0 '(+ (+ a b) c))
          ("four-args"  '* '(a b c d) 1 '(* (* (* a b) c) d)))
  (op args id expected)
  (assert-equal expected (cl-cc::reduce-variadic-op op args id)))

(deftest-each expand-all-atom-passthrough
  "compiler-macroexpand-all passes atoms (int, string, symbol) through unchanged."
  :cases (("integer" 42)
          ("string"  "hello")
          ("symbol"  'x))
  (form)
  (assert-equal form (cl-cc::compiler-macroexpand-all form)))

(deftest expand-all-quote
  "compiler-macroexpand-all preserves quoted forms."
  (assert-equal '(quote (1 2 3))
                (cl-cc::compiler-macroexpand-all '(quote (1 2 3)))))

(deftest expand-all-if
  "compiler-macroexpand-all recurses into if branches."
  (let ((result (cl-cc::compiler-macroexpand-all '(if t 1 2))))
    (assert-equal 'if (first result))
    (assert-equal t (second result))
    (assert-equal 1 (third result))
    (assert-equal 2 (fourth result))))

(deftest expand-all-let
  "compiler-macroexpand-all expands let binding values."
  (let ((result (cl-cc::compiler-macroexpand-all '(let ((x 1)) x))))
    (assert-equal 'let (first result))
    (assert-equal 'x (caar (second result)))))

(deftest-each expander-variadic-fold-nesting
  "3-arg variadic forms nest left-associatively: (OP a b c) → (OP (OP a b) c)."
  :cases (("multiply" '*      '(* a b c))
          ("append"   'append '(append a b c))
          ("minus"    '-      '(- a b c)))
  (op form)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq op (car result))
    (assert-true (consp (second result)))
    (assert-eq op (car (second result)))))

(deftest-each expander-variadic-zero-arg-identity
  "(+) → 0 and (*) → 1 (their respective identity elements)."
  :cases (("plus"  '(+) 0)
          ("times" '(*) 1))
  (form expected)
  (assert-equal expected (cl-cc::compiler-macroexpand-all form)))
