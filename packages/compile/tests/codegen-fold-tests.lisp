;;;; tests/unit/compile/codegen-fold-tests.lisp
;;;; Unit tests for compile-time constant folding in codegen-fold.lisp
;;;;
;;;; Covers the internal helpers:
;;;;   %ast-constant-number-value, %fold-ast-binop, *compile-time-eval-fns*,
;;;;   %compile-time-eval-binop, %compile-time-lookup, %ast-constant-node-p,
;;;;   %ast->compile-time-value, %compile-time-value->ast, %evaluate-ast,
;;;;   %evaluate-ast-sequence, %compile-time-eval-call.
;;;;
;;;; High-level optimize-ast is already covered in codegen-tests.lisp and
;;;; codegen-core-tests.lisp; this file targets the internal data+logic layer.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defmacro %with-clean-ct-env (&body body)
  "Bind a clean compile-time environment for evaluate-ast tests."
  `(let ((cl-cc/compile::*compile-time-value-env* nil)
         (cl-cc/compile::*compile-time-function-env* nil))
     ,@body))

;;; ─── %ast-constant-number-value ───────────────────────────────────────────

(deftest-each ast-constant-number-value-extracts-integer
  "%ast-constant-number-value returns the integer for ast-int and integer ast-quote."
  :cases (("ast-int"           42   (cl-cc/ast::make-ast-int   :value 42))
          ("ast-quote-integer" 7    (cl-cc/ast::make-ast-quote :value 7)))
  (expected node)
  (assert-= expected (cl-cc/compile::%ast-constant-number-value node)))

(deftest ast-constant-number-value-nil-for-non-integer-quote
  "%ast-constant-number-value returns NIL for an ast-quote wrapping a non-integer."
  (assert-null (cl-cc/compile::%ast-constant-number-value (cl-cc/ast::make-ast-quote :value "hello"))))

(deftest ast-constant-number-value-nil-for-variable-node
  "%ast-constant-number-value returns NIL for a variable node."
  (assert-null (cl-cc/compile::%ast-constant-number-value (cl-cc/ast::make-ast-var :name 'x))))

;;; ─── %ast-constant-node-p ─────────────────────────────────────────────────

(deftest-each ast-constant-node-p-true-for-constants
  "%ast-constant-node-p is true for ast-int and ast-quote."
  :cases (("int"   (cl-cc/ast::make-ast-int   :value 1))
          ("quote" (cl-cc/ast::make-ast-quote :value 'x)))
  (node)
  (assert-true (cl-cc/compile::%ast-constant-node-p node)))

(deftest ast-constant-node-p-false-for-var
  "%ast-constant-node-p is false for a variable node."
  (assert-null
   (cl-cc/compile::%ast-constant-node-p (cl-cc/ast::make-ast-var :name 'x))))

;;; ─── %ast->compile-time-value ─────────────────────────────────────────────

(deftest ast->compile-time-value-extracts-integer-from-ast-int
  "%ast->compile-time-value returns the integer value from an ast-int node."
  (assert-= 99 (cl-cc/compile::%ast->compile-time-value (cl-cc/ast::make-ast-int :value 99))))

(deftest ast->compile-time-value-extracts-list-from-ast-quote
  "%ast->compile-time-value returns the quoted value from an ast-quote node."
  (assert-equal '(a b) (cl-cc/compile::%ast->compile-time-value (cl-cc/ast::make-ast-quote :value '(a b)))))

(deftest ast->compile-time-value-returns-nil-for-variable
  "%ast->compile-time-value returns NIL for a non-constant variable node."
  (assert-null (cl-cc/compile::%ast->compile-time-value (cl-cc/ast::make-ast-var :name 'x))))

;;; ─── %compile-time-value->ast ─────────────────────────────────────────────

(deftest compile-time-value->ast-wraps-integers-and-symbols
  "%compile-time-value->ast wraps integers as ast-int and other values as ast-quote."
  (let* ((proto     (cl-cc/ast::make-ast-int :value 0))
         (int-node  (cl-cc/compile::%compile-time-value->ast 5     proto))
         (sym-node  (cl-cc/compile::%compile-time-value->ast 'hello proto)))
    (assert-true (typep int-node 'cl-cc::ast-int))
    (assert-= 5 (cl-cc/ast::ast-int-value int-node))
    (assert-true (typep sym-node 'cl-cc::ast-quote))
    (assert-eq 'hello (cl-cc/ast::ast-quote-value sym-node))))

;;; ─── %compile-time-eval-binop ─────────────────────────────────────────────

(deftest-each compile-time-eval-binop-arithmetic
  "%compile-time-eval-binop correctly evaluates basic arithmetic."
  :cases (("add" '+  3 4 7)
          ("sub" '-  9 4 5)
          ("mul" '*  3 7 21))
  (op a b expected)
  (assert-= expected (cl-cc/compile::%compile-time-eval-binop op a b)))

(deftest-each compile-time-eval-binop-division
  "%compile-time-eval-binop: division folds only when result is an exact integer."
  :cases (("exact"    12  3 4)     ; 12/3 = 4 — integer, folds
          ("non-exact" 7  2 nil)   ; 7/2 = 3.5 — not integer, returns nil
          ("div-zero"  5  0 nil))  ; division by zero — returns nil
  (lhs rhs expected)
  (if expected
      (assert-= expected (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs))
      (assert-null (cl-cc/compile::%compile-time-eval-binop '/ lhs rhs))))

(deftest-each compile-time-eval-binop-unary
  "%compile-time-eval-binop: 1+ and 1- are unary (rhs must be nil)."
  :cases (("1+ integer"  '1+  5 nil 6)
          ("1- integer"  '1-  5 nil 4)
          ("1+ non-nil-rhs" '1+ 5 99 nil)   ; rhs != nil → not foldable
          ("1- non-nil-rhs" '1- 5 99 nil))
  (op lhs rhs expected)
  (if expected
      (assert-= expected (cl-cc/compile::%compile-time-eval-binop op lhs rhs))
      (assert-null (cl-cc/compile::%compile-time-eval-binop op lhs rhs))))

;;; ─── %compile-time-lookup ─────────────────────────────────────────────────

(deftest-each compile-time-lookup-cases
  "%compile-time-lookup: returns value+T for known names; nil+nil for absent names."
  :cases (("found"     'x '((x . 42) (y . 7)) 42  t)
          ("not-found" 'z '((x . 1)  (y . 2)) nil nil))
  (name alist expected-value expected-found)
  (multiple-value-bind (value found-p)
      (cl-cc/compile::%compile-time-lookup name alist)
    (assert-equal expected-value  value)
    (assert-equal expected-found found-p)))

;;; ─── *compile-time-eval-fns* ──────────────────────────────────────────────

(deftest compile-time-eval-fns-registered
  "*compile-time-eval-fns* has arithmetic (+, -, *, /) and predicate (not, zerop, null, etc.) entries."
  (dolist (sym '(+ - * / not zerop null numberp integerp symbolp))
    (assert-true (gethash sym cl-cc/compile::*compile-time-eval-fns*))))

(deftest-each compile-time-eval-fns-evaluation-cases
  "Entries in *compile-time-eval-fns* correctly evaluate their arguments."
  :cases (("plus-sum"  '+    '(3 4 5) 12)
          ("null-nil"  'null '(nil)    t))
  (fn-name args expected)
  (let ((fn (gethash fn-name cl-cc/compile::*compile-time-eval-fns*)))
    (multiple-value-bind (result ok)
        (funcall fn args)
      (assert-true ok)
      (assert-equal expected result))))

;;; ─── %fold-ast-binop ──────────────────────────────────────────────────────

(deftest fold-ast-binop-folds-integer-literals
  "%fold-ast-binop returns an ast-int when both operands are integer literals."
  (let* ((node (cl-cc/ast::make-ast-binop
                :op '+
                :lhs (cl-cc/ast::make-ast-int :value 0)
                :rhs (cl-cc/ast::make-ast-int :value 0)))
         (lhs  (cl-cc/ast::make-ast-int :value 10))
         (rhs  (cl-cc/ast::make-ast-int :value 32))
         (result (cl-cc/compile::%fold-ast-binop node lhs rhs)))
    (assert-true (typep result 'cl-cc::ast-int))
    (assert-= 42 (cl-cc/ast::ast-int-value result))))

(deftest fold-ast-binop-does-not-fold-non-constants
  "%fold-ast-binop returns a binop unchanged when either operand is not a constant."
  (let* ((node (cl-cc/ast::make-ast-binop
                :op '+
                :lhs (cl-cc/ast::make-ast-int :value 0)
                :rhs (cl-cc/ast::make-ast-int :value 0)))
         (lhs  (cl-cc/ast::make-ast-var :name 'x))
         (rhs  (cl-cc/ast::make-ast-int :value 5))
         (result (cl-cc/compile::%fold-ast-binop node lhs rhs)))
    (assert-true (typep result 'cl-cc::ast-binop))))

;;; ─── %evaluate-ast ────────────────────────────────────────────────────────

(deftest evaluate-ast-constants
  "%evaluate-ast evaluates ast-int and ast-quote to their values."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast::make-ast-int :value 17) 10)
      (assert-true ok)
      (assert-= 17 value))
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast::make-ast-quote :value 'hello) 10)
      (assert-true ok)
      (assert-eq 'hello value))))

(deftest evaluate-ast-var-found-in-env
  "%evaluate-ast resolves a bound variable from the compile-time env."
  (multiple-value-bind (value ok)
      (let ((cl-cc/compile::*compile-time-value-env* '((n . 42)))
            (cl-cc/compile::*compile-time-function-env* nil))
        (cl-cc/compile::%evaluate-ast (cl-cc/ast::make-ast-var :name 'n) 10))
    (assert-true ok)
    (assert-= 42 value)))

(deftest evaluate-ast-unbound-variable-returns-nil-nil
  "%evaluate-ast returns (values nil nil) for an unbound variable."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast::make-ast-var :name 'x) 10)
      (assert-null value)
      (assert-null ok))))

(deftest evaluate-ast-exhausted-depth-returns-nil-nil
  "%evaluate-ast returns (values nil nil) when the depth counter is negative."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast (cl-cc/ast::make-ast-int :value 5) -1)
      (assert-null value)
      (assert-null ok))))

(deftest evaluate-ast-arithmetic-binop
  "%evaluate-ast evaluates a constant arithmetic binop to an integer."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-binop :op '+ :lhs (cl-cc/ast::make-ast-int :value 3)
                                    :rhs (cl-cc/ast::make-ast-int :value 4))
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
         (list (cl-cc/ast::make-ast-int :value 1) (cl-cc/ast::make-ast-int :value 2))
         nil nil 10)
      (assert-true ok)
      (assert-= 2 value))))

(deftest evaluate-ast-sequence-unknown-var-returns-nil-nil
  "%evaluate-ast-sequence returns (values nil nil) when a sequence element cannot be evaluated."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast-sequence
         (list (cl-cc/ast::make-ast-var :name 'unk-xyz))
         nil nil 10)
      (assert-null ok)
      (assert-null value))))

;;; ─── %evaluate-ast (extended) ─────────────────────────────────────────────

(deftest-each evaluate-ast-ast-if-cases
  "%evaluate-ast ast-if: truthy condition picks then-branch; falsy picks else-branch."
  :cases (("truthy" (cl-cc/ast::make-ast-int   :value 1)   42)
          ("falsy"  (cl-cc/ast::make-ast-quote :value nil)  0))
  (cond-node expected)
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-if :cond cond-node
                                 :then (cl-cc/ast::make-ast-int :value 42)
                                 :else (cl-cc/ast::make-ast-int :value 0))
         10))
    (assert-true ok)
    (assert-= expected value)))

(deftest evaluate-ast-progn-two-forms-returns-last-value
  "%evaluate-ast ast-progn with two constant forms returns the last value."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-progn :forms (list (cl-cc/ast::make-ast-int :value 1)
                                                  (cl-cc/ast::make-ast-int :value 2)))
         10)
      (assert-true ok)
      (assert-= 2 value))))

(deftest evaluate-ast-progn-with-unknown-var-returns-nil-nil
  "%evaluate-ast ast-progn returns (values nil nil) when an inner form cannot be evaluated."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-progn :forms (list (cl-cc/ast::make-ast-var :name 'unk-xyz)))
         10)
      (assert-null ok)
      (assert-null value))))

(deftest evaluate-ast-let-binding-returns-bound-value
  "%evaluate-ast ast-let: binding x=5 and returning x yields value=5."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-let :bindings (list (cons 'x (cl-cc/ast::make-ast-int :value 5)))
                                  :body (list (cl-cc/ast::make-ast-var :name 'x)))
         10))
    (assert-true ok)
    (assert-= 5 value)))

(deftest evaluate-ast-the-passes-through-to-inner-value
  "%evaluate-ast ast-the: passes through to inner value."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-the :type 'fixnum :value (cl-cc/ast::make-ast-int :value 99))
         10))
    (assert-true ok)
    (assert-= 99 value)))

(deftest evaluate-ast-unknown-node-returns-nil
  "%evaluate-ast returns (values nil nil) for nodes whose op cannot be evaluated."
  (multiple-value-bind (value ok)
      (%with-clean-ct-env
        (cl-cc/compile::%evaluate-ast
         (cl-cc/ast::make-ast-binop :op 'unknown-op :lhs (cl-cc/ast::make-ast-var :name 'x)
                                    :rhs (cl-cc/ast::make-ast-var :name 'y))
         10))
    (assert-null ok)
    (assert-null value)))

;;; ─── %compile-time-eval-call ──────────────────────────────────────────────

(deftest compile-time-eval-call-string-length-folds
  "%compile-time-eval-call evaluates STRING-LENGTH of a literal string at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast::make-ast-var :name 'string-length) (list "hello") 10)
      (assert-true ok)
      (assert-= 5 value))))

(deftest compile-time-eval-call-builtin-not-folds
  "%compile-time-eval-call evaluates the built-in NOT function at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast::make-ast-var :name 'not) (list nil) 10)
      (assert-true ok)
      (assert-true value))))

(deftest compile-time-eval-call-lambda-application-folds
  "%compile-time-eval-call evaluates an inline lambda application at compile time."
  (%with-clean-ct-env
    (multiple-value-bind (value ok)
        (cl-cc/compile::%compile-time-eval-call
         (cl-cc/ast::make-ast-lambda :params '(x)
                                     :body (list (cl-cc/ast::make-ast-var :name 'x))
                                     :optional-params nil :rest-param nil :key-params nil)
         (list 7) 10)
      (assert-true ok)
      (assert-= 7 value))))

(deftest compile-time-eval-call-unknown-function-returns-nil
  "%compile-time-eval-call returns NIL for an unknown function symbol."
  (%with-clean-ct-env
    (assert-null (cl-cc/compile::%compile-time-eval-call
                  (cl-cc/ast::make-ast-var :name 'completely-unknown-fn-xyz)
                  (list 1) 10))))
