;;;; tests/unit/core/cps-tests.lisp — CPS Transformation tests
;;;;
;;;; Covers:
;;;;   1. Helper functions: eval-cps-ast, run-cps-ast, is-cps-lambda
;;;;   2. Structure predicate tests (single-param-lambda-p, funcall-of-single-lambda-p, etc.)
;;;;   3. S-expression CPS bootstrap transformer — semantic evaluation
;;;;
;;;; AST-based CPS tests continue in cps-ast-tests.lisp.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun eval-cps-ast (ast)
  "CPS-transform an AST node to a (lambda (k) ...) sexp and evaluate it.
Returns a function that takes a continuation."
  (eval (cl-cc:cps-transform-ast* ast)))

(defun run-cps-ast (ast)
  "Run CPS-transformed AST with the identity continuation."
  (funcall (eval-cps-ast ast) #'identity))

(defun is-cps-lambda (result)
  "Return t if RESULT is a (lambda (k) ...) sexp as produced by cps-transform-ast*."
  (and (listp result)
       (eq 'lambda (car result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Structure predicate tests (extracted for readability)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-single-param-lambda-p
  "%single-param-lambda-p: recognizes exactly (lambda (x) body) forms."
  :cases (("yes-single"     t   '(lambda (x) x))
          ("yes-with-body"  t   '(lambda (k) (funcall f k)))
          ("no-multi-param" nil '(lambda (x y) x))
          ("no-zero-param"  nil '(lambda () 42))
          ("no-not-lambda"  nil '(funcall f x))
          ("no-atom"        nil 42))
  (expected form)
  (assert-equal expected (cl-cc/compile::%single-param-lambda-p form)))

(deftest-each cps-funcall-of-single-lambda-p
  "%funcall-of-single-lambda-p: recognizes (funcall (lambda (x) body) arg)."
  :cases (("yes"            t   '(funcall (lambda (x) x) 42))
          ("no-multi-args"  nil '(funcall (lambda (x) x) 1 2))
          ("no-plain-fn"    nil '(funcall f 42))
          ("no-multi-param" nil '(funcall (lambda (x y) x) 1))
          ("no-atom"        nil 42))
  (expected form)
  (assert-equal expected (cl-cc/compile::%funcall-of-single-lambda-p form)))

(deftest cps-single-param-lambda-parts
  "%single-param-lambda-parts extracts the parameter and body of a one-arg lambda."
  (multiple-value-bind (param body)
      (cl-cc/compile::%single-param-lambda-parts '(lambda (k) (funcall next k)))
    (assert-eq 'k param)
    (assert-equal '(funcall next k) body)))

(deftest cps-funcall-single-lambda-parts
  "%funcall-single-lambda-parts extracts beta-reduction pieces from a funcall form."
  (multiple-value-bind (param body arg)
      (cl-cc/compile::%funcall-single-lambda-parts '(funcall (lambda (x) (+ x 1)) 41))
    (assert-eq 'x param)
    (assert-equal '(+ x 1) body)
    (assert-= 41 arg)))

(deftest-each cps-eta-reducible-lambda-p
  "%eta-reducible-lambda-p: recognizes (lambda (x) (funcall f x))."
  :cases (("yes"              t   '(lambda (k) (funcall next k)))
          ("no-wrong-param"   nil '(lambda (k) (funcall next x)))
          ("no-extra-body"    nil '(lambda (k) (print k) (funcall next k)))
          ("no-plain-lambda"  nil '(lambda (k) k))
          ("no-multi-param"   nil '(lambda (k j) (funcall next k))))
  (expected form)
  (assert-equal expected (cl-cc/compile::%eta-reducible-lambda-p form)))

(deftest-each cps-simplify-form-reductions
  "cps-simplify-form applies beta-reduction and eta-reduction."
  :cases (("beta-reduce" 42    '(funcall (lambda (x) x) 42))
          ("eta-reduce"  'next '(lambda (k) (funcall next k))))
  (expected form)
  (assert-equal expected (cl-cc/compile::cps-simplify-form form)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; S-expression CPS (bootstrap transformer)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-sexp-transform
  "S-expression CPS transformer: evaluate with identity continuation"
  :cases (("integer"           '42               42)
          ("add"               '(+ 1 2)           3)
          ("sub"               '(- 10 3)          7)
          ("mul"               '(* 3 4)          12)
          ("if-true"           '(if 1 10 20)     10)
          ("if-false"          '(if nil 10 20)   20)
          ("progn-returns-last" '(progn 1 2 3)    3)
          ("let-binding"       '(let ((x 1) (y 2)) (+ x y)) 3))
  (expr expected)
  (let ((fn (cl-cc:cps-transform-eval expr)))
    (assert-equal expected (funcall fn #'identity))))
