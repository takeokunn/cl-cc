;;;; tests/unit/compile/codegen-fold-eval-tests.lisp
;;;; Unit tests for compile-time partial evaluator in codegen-fold-eval.lisp
;;;;
;;;; Covers: %evaluate-ast (all node types), %evaluate-ast-sequence,
;;;;   *compile-time-multi-arg-fns*, *compile-time-unary-pred-fns*,
;;;;   %compile-time-eval-known-call, %compile-time-pair-bindings,
;;;;   %compile-time-append-env, %compile-time-eval-call.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %evaluate-ast ────────────────────────────────────────────────────────

(deftest evaluate-ast-constants
  "%evaluate-ast evaluates ast-int and ast-quote to their values."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-int :value 17) 10)
      (assert-true ok)
      (assert-= 17 value))
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-quote :value 'hello) 10)
      (assert-true ok)
      (assert-eq 'hello value))))

(deftest evaluate-ast-var-found-in-env
  "%evaluate-ast resolves a bound variable from the compile-time env."
  (multiple-value-bind (value ok)
      (let ((cl-cc/compile::*compile-time-value-env* '((n . 42)))
            (cl-cc/compile::*compile-time-function-env* nil))
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-var :name 'n) 10))
    (assert-true ok)
    (assert-= 42 value)))

(deftest evaluate-ast-unbound-variable-returns-nil-nil
  "%evaluate-ast returns (values nil nil) for an unbound variable."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-var :name 'x) 10)
      (assert-null value)
      (assert-null ok))))

(deftest evaluate-ast-exhausted-depth-returns-nil-nil
  "%evaluate-ast returns (values nil nil) when the depth counter is negative."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast:make-ast-int :value 5) -1)
      (assert-null value)
      (assert-null ok))))

(deftest evaluate-ast-arithmetic-binop
  "%evaluate-ast evaluates a constant arithmetic binop to an integer."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-binop :op '+ :lhs (cl-cc/ast:make-ast-int :value 3)
                                    :rhs (cl-cc/ast:make-ast-int :value 4))
         10))
    (assert-true ok)
    (assert-= 7 value)))

;;; ─── %evaluate-ast-sequence ───────────────────────────────────────────────

(deftest evaluate-ast-sequence-empty-returns-nil-true
  "%evaluate-ast-sequence on an empty list returns (values nil t)."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence nil nil nil 10)
      (assert-true ok)
      (assert-null value))))

(deftest evaluate-ast-sequence-two-constants-returns-last
  "%evaluate-ast-sequence of two constants returns the last value."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence
         (list (cl-cc/ast:make-ast-int :value 1) (cl-cc/ast:make-ast-int :value 2))
         nil nil 10)
      (assert-true ok)
      (assert-= 2 value))))

(deftest evaluate-ast-sequence-unknown-var-returns-nil-nil
  "%evaluate-ast-sequence returns (values nil nil) when a sequence element cannot be evaluated."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence
         (list (cl-cc/ast:make-ast-var :name 'unk-xyz))
         nil nil 10)
      (assert-null ok)
      (assert-null value))))

;;; ─── %evaluate-ast (extended) ─────────────────────────────────────────────

(deftest-each evaluate-ast-ast-if-cases
  "%evaluate-ast ast-if: truthy condition picks then-branch; falsy picks else-branch."
  :cases (("truthy" (cl-cc/ast:make-ast-int   :value 1)   42)
          ("falsy"  (cl-cc/ast:make-ast-quote :value nil)  0))
  (cond-node expected)
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-if :cond cond-node
                                 :then (cl-cc/ast:make-ast-int :value 42)
                                 :else (cl-cc/ast:make-ast-int :value 0))
         10))
    (assert-true ok)
    (assert-= expected value)))

(deftest evaluate-ast-progn-two-forms-returns-last-value
  "%evaluate-ast ast-progn with two constant forms returns the last value."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-progn :forms (list (cl-cc/ast:make-ast-int :value 1)
                                                  (cl-cc/ast:make-ast-int :value 2)))
         10)
      (assert-true ok)
      (assert-= 2 value))))

(deftest evaluate-ast-progn-with-unknown-var-returns-nil-nil
  "%evaluate-ast ast-progn returns (values nil nil) when an inner form cannot be evaluated."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-progn :forms (list (cl-cc/ast:make-ast-var :name 'unk-xyz)))
         10)
      (assert-null ok)
      (assert-null value))))

(deftest evaluate-ast-let-binding-returns-bound-value
  "%evaluate-ast ast-let: binding x=5 and returning x yields value=5."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-let :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 5)))
                                  :body (list (cl-cc/ast:make-ast-var :name 'x)))
         10))
    (assert-true ok)
    (assert-= 5 value)))

(deftest evaluate-ast-the-passes-through-to-inner-value
  "%evaluate-ast ast-the: passes through to inner value."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-the :type 'fixnum :value (cl-cc/ast:make-ast-int :value 99))
         10))
    (assert-true ok)
    (assert-= 99 value)))

(deftest evaluate-ast-unknown-node-returns-nil
  "%evaluate-ast returns (values nil nil) for nodes whose op cannot be evaluated."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-binop :op 'unknown-op :lhs (cl-cc/ast:make-ast-var :name 'x)
                                    :rhs (cl-cc/ast:make-ast-var :name 'y))
         10))
    (assert-null ok)
    (assert-null value)))

;;; ─── *compile-time-multi-arg-fns* / *compile-time-unary-pred-fns* ───────────

(deftest compile-time-multi-arg-fns-contains-all-expected
  "*compile-time-multi-arg-fns* has all N-ary arithmetic and comparison ops."
  (dolist (sym '(+ - * = < <= > >=))
    (assert-true (member sym cl-cc/compile::*compile-time-multi-arg-fns* :test #'eq))))

(deftest compile-time-unary-pred-fns-contains-all-expected
  "*compile-time-unary-pred-fns* has all supported unary predicates."
  (dolist (sym '(not zerop plusp minusp oddp evenp numberp integerp consp null symbolp stringp functionp))
    (assert-true (member sym cl-cc/compile::*compile-time-unary-pred-fns* :test #'eq))))

;;; ─── %compile-time-eval-known-call ───────────────────────────────────────────

(deftest-each compile-time-eval-known-call-multi-arg-fns
  "%compile-time-eval-known-call evaluates all registered N-ary arithmetic/comparison fns."
  :cases (("add"  '+  '(3 4)   7)
          ("sub"  '-  '(10 3)  7)
          ("mul"  '*  '(4 5)   20)
          ("eq"   '=  '(7 7)   t)
          ("lt"   '<  '(3 5)   t)
          ("lte"  '<= '(5 5)   t)
          ("gt"   '>  '(9 3)   t)
          ("gte"  '>= '(4 4)   t))
  (name args expected)
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (assert-true ok)
    (assert-equal expected result)))

(deftest-each compile-time-eval-known-call-unary-preds
  "%compile-time-eval-known-call evaluates all registered unary predicates."
  :cases (("not-nil"    'not      '(nil)   t)
          ("not-t"      'not      '(t)     nil)
          ("zerop"      'zerop    '(0)     t)
          ("plusp"      'plusp    '(5)     t)
          ("minusp"     'minusp   '(-3)    t)
          ("oddp"       'oddp     '(3)     t)
          ("evenp"      'evenp    '(4)     t)
          ("numberp"    'numberp  '(42)    t)
          ("integerp"   'integerp '(1)     t)
          ("consp"      'consp    '((a))   t)
          ("null-nil"   'null     '(nil)   t)
          ("symbolp"    'symbolp  '(foo)   t)
          ("stringp"    'stringp  '("hi")  t))
  (name args expected)
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call name args)
    (assert-true ok)
    (assert-equal expected result)))

(deftest compile-time-eval-known-call-division-integer
  "%compile-time-eval-known-call / with exact integer division returns the quotient."
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(12 4))
    (assert-true ok)
    (assert-= 3 result)))

(deftest compile-time-eval-known-call-division-ratio
  "%compile-time-eval-known-call / returns the ratio for non-exact division (no integerp guard)."
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(7 2))
    (assert-true ok)
    (assert-equal 7/2 result)))

(deftest compile-time-eval-known-call-division-by-zero
  "%compile-time-eval-known-call / returns nil for division by zero."
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call '/ '(5 0))
    (assert-null ok)
    (assert-null result)))

(deftest compile-time-eval-known-call-unknown-returns-nil-nil
  "%compile-time-eval-known-call returns (values nil nil) for unrecognized names."
  (multiple-value-bind (result ok)
      (cl-cc/compile::%compile-time-eval-known-call 'totally-unknown-fn '(1))
    (assert-null result)
    (assert-null ok)))

;;; ─── %compile-time-pair-bindings ─────────────────────────────────────────────

(deftest-each compile-time-pair-bindings-cases
  "%compile-time-pair-bindings pairs params and args into an alist."
  :cases (("empty"   nil       nil       nil)
          ("single"  '(x)      '(1)      '((x . 1)))
          ("multi"   '(x y z)  '(1 2 3)  '((x . 1) (y . 2) (z . 3)))
          ("fewer"   '(x y)    '(1)      '((x . 1))))
  (params args expected)
  (assert-equal expected (cl-cc/compile::%compile-time-pair-bindings params args)))

;;; ─── %compile-time-append-env ────────────────────────────────────────────────

(deftest compile-time-append-env-prepends-bindings
  "%compile-time-append-env prepends new bindings in front of the existing env."
  (let ((result (cl-cc/compile::%compile-time-append-env
                 '((a . 1) (b . 2)) '((c . 3)))))
    (assert-equal '((a . 1) (b . 2) (c . 3)) result)))

(deftest compile-time-append-env-empty-bindings-returns-env
  "%compile-time-append-env with empty bindings returns the original env."
  (let ((env '((x . 99))))
    (assert-equal env (cl-cc/compile::%compile-time-append-env nil env))))

;;; ─── %evaluate-ast block/return-from ────────────────────────────────────────

(deftest evaluate-ast-block-normal-exit
  "%evaluate-ast ast-block returns the last form value on normal exit."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-block
          :name 'done
          :body (list (cl-cc/ast:make-ast-int :value 42)))
         10)
      (assert-true ok)
      (assert-= 42 value))))

(deftest evaluate-ast-return-from-exits-block
  "%evaluate-ast ast-return-from unwinds to the named block."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast:make-ast-block
          :name 'early
          :body (list
                 (cl-cc/ast:make-ast-return-from
                  :name 'early
                  :value (cl-cc/ast:make-ast-int :value 7))
                 (cl-cc/ast:make-ast-int :value 99)))
         10)
      (assert-true ok)
      (assert-= 7 value))))

;;; ─── %compile-time-eval-call ──────────────────────────────────────────────

(deftest compile-time-eval-call-string-length-folds
  "%compile-time-eval-call evaluates STRING-LENGTH of a literal string at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'string-length) (list "hello") 10)
      (assert-true ok)
      (assert-= 5 value))))

(deftest compile-time-eval-call-builtin-not-folds
  "%compile-time-eval-call evaluates the built-in NOT function at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-var :name 'not) (list nil) 10)
      (assert-true ok)
      (assert-true value))))

(deftest compile-time-eval-call-lambda-application-folds
  "%compile-time-eval-call evaluates an inline lambda application at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast:make-ast-lambda :params '(x)
                                     :body (list (cl-cc/ast:make-ast-var :name 'x))
                                     :optional-params nil :rest-param nil :key-params nil)
         (list 7) 10)
      (assert-true ok)
      (assert-= 7 value))))

(deftest compile-time-eval-call-unknown-function-returns-nil
  "%compile-time-eval-call returns NIL for an unknown function symbol."
  (%with-clean-ct-env
    (assert-null (cl-cc/compile::%compile-time-eval-call
                  (cl-cc/ast:make-ast-var :name 'completely-unknown-fn-xyz)
                  (list 1) 10))))
