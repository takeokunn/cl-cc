;;;; tests/unit/compile/codegen-fold-tests.lisp
;;;; Unit tests for compile-time constant folding in codegen-fold.lisp
;;;;
;;;; Covers the internal helpers:
;;;;   %ast-constant-number-value, %fold-ast-binop, *compile-time-eval-fns*,
;;;;   %compile-time-eval-binop, %compile-time-lookup, %ast-constant-node-p,
;;;;   %ast->compile-time-value, %compile-time-value->ast, %evaluate-ast.
;;;;
;;;; High-level optimize-ast is already covered in codegen-tests.lisp and
;;;; codegen-core-tests.lisp; this file targets the internal data+logic layer.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── %ast-constant-number-value ───────────────────────────────────────────

(deftest-each ast-constant-number-value-extracts-integer
  "%ast-constant-number-value returns the integer for ast-int and integer ast-quote."
  :cases (("ast-int"           42   (cl-cc::make-ast-int   :value 42))
          ("ast-quote-integer" 7    (cl-cc::make-ast-quote :value 7)))
  (expected node)
  (assert-= expected (cl-cc::%ast-constant-number-value node)))

(deftest ast-constant-number-value-nil-for-non-integer-quote
  "%ast-constant-number-value returns nil for a quoted non-integer."
  (assert-null
   (cl-cc::%ast-constant-number-value
    (cl-cc::make-ast-quote :value "hello"))))

(deftest ast-constant-number-value-nil-for-var
  "%ast-constant-number-value returns nil for a variable reference."
  (assert-null
   (cl-cc::%ast-constant-number-value
    (cl-cc::make-ast-var :name 'x))))

;;; ─── %ast-constant-node-p ─────────────────────────────────────────────────

(deftest-each ast-constant-node-p-true-for-constants
  "%ast-constant-node-p is true for ast-int and ast-quote."
  :cases (("int"   (cl-cc::make-ast-int   :value 1))
          ("quote" (cl-cc::make-ast-quote :value 'x)))
  (node)
  (assert-true (cl-cc::%ast-constant-node-p node)))

(deftest ast-constant-node-p-false-for-var
  "%ast-constant-node-p is false for a variable node."
  (assert-null
   (cl-cc::%ast-constant-node-p (cl-cc::make-ast-var :name 'x))))

;;; ─── %ast->compile-time-value ─────────────────────────────────────────────

(deftest ast->compile-time-value-returns-int-value
  "%ast->compile-time-value extracts the integer from an ast-int node."
  (assert-= 99
            (cl-cc::%ast->compile-time-value
             (cl-cc::make-ast-int :value 99))))

(deftest ast->compile-time-value-returns-quote-value
  "%ast->compile-time-value returns the quoted datum from an ast-quote node."
  (assert-equal '(a b)
                (cl-cc::%ast->compile-time-value
                 (cl-cc::make-ast-quote :value '(a b)))))

(deftest ast->compile-time-value-nil-for-var
  "%ast->compile-time-value returns nil for a non-constant node."
  (assert-null
   (cl-cc::%ast->compile-time-value (cl-cc::make-ast-var :name 'x))))

;;; ─── %compile-time-value->ast ─────────────────────────────────────────────

(deftest compile-time-value->ast-integer-yields-ast-int
  "%compile-time-value->ast wraps an integer as an ast-int node."
  (let ((node (cl-cc::%compile-time-value->ast
               5 (cl-cc::make-ast-int :value 0))))
    (assert-true (typep node 'cl-cc::ast-int))
    (assert-= 5 (cl-cc::ast-int-value node))))

(deftest compile-time-value->ast-non-integer-yields-ast-quote
  "%compile-time-value->ast wraps a non-integer as an ast-quote node."
  (let ((node (cl-cc::%compile-time-value->ast
               'hello (cl-cc::make-ast-int :value 0))))
    (assert-true (typep node 'cl-cc::ast-quote))
    (assert-eq 'hello (cl-cc::ast-quote-value node))))

;;; ─── %compile-time-eval-binop ─────────────────────────────────────────────

(deftest-each compile-time-eval-binop-arithmetic
  "%compile-time-eval-binop correctly evaluates basic arithmetic."
  :cases (("add" '+  3 4 7)
          ("sub" '-  9 4 5)
          ("mul" '*  3 7 21))
  (op a b expected)
  (assert-= expected (cl-cc::%compile-time-eval-binop op a b)))

(deftest-each compile-time-eval-binop-division
  "%compile-time-eval-binop: division folds only when result is an exact integer."
  :cases (("exact"    12  3 4)     ; 12/3 = 4 — integer, folds
          ("non-exact" 7  2 nil)   ; 7/2 = 3.5 — not integer, returns nil
          ("div-zero"  5  0 nil))  ; division by zero — returns nil
  (lhs rhs expected)
  (if expected
      (assert-= expected (cl-cc::%compile-time-eval-binop '/ lhs rhs))
      (assert-null (cl-cc::%compile-time-eval-binop '/ lhs rhs))))

(deftest-each compile-time-eval-binop-unary
  "%compile-time-eval-binop: 1+ and 1- are unary (rhs must be nil)."
  :cases (("1+ integer"  '1+  5 nil 6)
          ("1- integer"  '1-  5 nil 4)
          ("1+ non-nil-rhs" '1+ 5 99 nil)   ; rhs != nil → not foldable
          ("1- non-nil-rhs" '1- 5 99 nil))
  (op lhs rhs expected)
  (if expected
      (assert-= expected (cl-cc::%compile-time-eval-binop op lhs rhs))
      (assert-null (cl-cc::%compile-time-eval-binop op lhs rhs))))

;;; ─── %compile-time-lookup ─────────────────────────────────────────────────

(deftest compile-time-lookup-finds-existing-binding
  "%compile-time-lookup returns (values value t) for a known name."
  (multiple-value-bind (value found-p)
      (cl-cc::%compile-time-lookup 'x '((x . 42) (y . 7)))
    (assert-true found-p)
    (assert-= 42 value)))

(deftest compile-time-lookup-returns-nil-for-missing
  "%compile-time-lookup returns nil (not two values) for an absent name."
  (multiple-value-bind (value found-p)
      (cl-cc::%compile-time-lookup 'z '((x . 1) (y . 2)))
    (assert-null value)
    (assert-null found-p)))

;;; ─── *compile-time-eval-fns* ──────────────────────────────────────────────

(deftest compile-time-eval-fns-registered-for-arithmetic
  "*compile-time-eval-fns* has + - * / registered."
  (dolist (sym '(+ - * /))
    (assert-true (gethash sym cl-cc::*compile-time-eval-fns*))))

(deftest compile-time-eval-fns-registered-for-predicates
  "*compile-time-eval-fns* has not/zerop/null/numberp registered."
  (dolist (sym '(not zerop null numberp integerp symbolp))
    (assert-true (gethash sym cl-cc::*compile-time-eval-fns*))))

(deftest compile-time-eval-fns-plus-evaluates-sum
  "The + entry in *compile-time-eval-fns* correctly sums a list of numbers."
  (let ((fn (gethash '+ cl-cc::*compile-time-eval-fns*)))
    (multiple-value-bind (result ok)
        (funcall fn '(3 4 5))
      (assert-true ok)
      (assert-= 12 result))))

(deftest compile-time-eval-fns-null-detects-nil
  "The NULL entry in *compile-time-eval-fns* returns T for nil."
  (let ((fn (gethash 'null cl-cc::*compile-time-eval-fns*)))
    (multiple-value-bind (result ok)
        (funcall fn '(nil))
      (assert-true ok)
      (assert-true result))))

;;; ─── %fold-ast-binop ──────────────────────────────────────────────────────

(deftest fold-ast-binop-folds-integer-literals
  "%fold-ast-binop returns an ast-int when both operands are integer literals."
  (let* ((node (cl-cc::make-ast-binop
                :op '+
                :lhs (cl-cc::make-ast-int :value 0)
                :rhs (cl-cc::make-ast-int :value 0)))
         (lhs  (cl-cc::make-ast-int :value 10))
         (rhs  (cl-cc::make-ast-int :value 32))
         (result (cl-cc::%fold-ast-binop node lhs rhs)))
    (assert-true (typep result 'cl-cc::ast-int))
    (assert-= 42 (cl-cc::ast-int-value result))))

(deftest fold-ast-binop-does-not-fold-non-constants
  "%fold-ast-binop returns a binop unchanged when either operand is not a constant."
  (let* ((node (cl-cc::make-ast-binop
                :op '+
                :lhs (cl-cc::make-ast-int :value 0)
                :rhs (cl-cc::make-ast-int :value 0)))
         (lhs  (cl-cc::make-ast-var :name 'x))
         (rhs  (cl-cc::make-ast-int :value 5))
         (result (cl-cc::%fold-ast-binop node lhs rhs)))
    (assert-true (typep result 'cl-cc::ast-binop))))

;;; ─── %evaluate-ast ────────────────────────────────────────────────────────

(deftest evaluate-ast-integer-literal
  "%evaluate-ast evaluates an ast-int to its value."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* nil)
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast (cl-cc::make-ast-int :value 17) 10))
    (assert-true ok)
    (assert-= 17 value)))

(deftest evaluate-ast-quote-symbol
  "%evaluate-ast evaluates an ast-quote to its datum."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* nil)
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast (cl-cc::make-ast-quote :value 'hello) 10))
    (assert-true ok)
    (assert-eq 'hello value)))

(deftest evaluate-ast-var-found-in-env
  "%evaluate-ast resolves a bound variable from the compile-time env."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* '((n . 42)))
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast (cl-cc::make-ast-var :name 'n) 10))
    (assert-true ok)
    (assert-= 42 value)))

(deftest evaluate-ast-var-not-found-returns-nil
  "%evaluate-ast returns (values nil nil) for an unbound variable."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* nil)
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast (cl-cc::make-ast-var :name 'x) 10))
    (assert-null value)
    (assert-null ok)))

(deftest evaluate-ast-depth-limit-returns-nil
  "%evaluate-ast returns (values nil nil) when depth runs out."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* nil)
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast (cl-cc::make-ast-int :value 5) -1))
    (assert-null value)
    (assert-null ok)))

(deftest evaluate-ast-arithmetic-binop
  "%evaluate-ast evaluates a constant arithmetic binop to an integer."
  (multiple-value-bind (value ok)
      (let ((cl-cc::*compile-time-value-env* nil)
            (cl-cc::*compile-time-function-env* nil))
        (cl-cc::%evaluate-ast
         (cl-cc::make-ast-binop
          :op '+
          :lhs (cl-cc::make-ast-int :value 3)
          :rhs (cl-cc::make-ast-int :value 4))
         10))
    (assert-true ok)
    (assert-= 7 value)))
