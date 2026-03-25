;;;; tests/unit/compile/closure-tests.lisp — Unit tests for find-free-variables
;;;;
;;;; Tests the free variable analysis on hand-constructed AST nodes.
;;;; Each test builds an AST directly (no parsing) and checks the result
;;;; of find-free-variables against the expected set of free variables.

(in-package :cl-cc/test)

(defsuite closure-suite :description "Free variable analysis unit tests")

;;; ─── Literals ─────────────────────────────────────────────────────────────

(deftest free-vars-int-literal
  "Integer literals have no free variables."
  (assert-equal nil
    (cl-cc::find-free-variables (cl-cc::make-ast-int :value 42))))

(deftest free-vars-quote
  "Quoted forms have no free variables."
  (assert-equal nil
    (cl-cc::find-free-variables (cl-cc::make-ast-quote :value '(a b c)))))

;;; ─── Simple references ────────────────────────────────────────────────────

(deftest free-vars-single-var
  "A single variable reference is free."
  (assert-equal '(x)
    (cl-cc::find-free-variables (cl-cc::make-ast-var :name 'x))))

(deftest free-vars-binop-two-vars
  "A binop with two different vars has both free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

(deftest free-vars-binop-same-var
  "A binop referencing the same var twice returns it once (union)."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'x)))))
    (assert-equal '(x) result)))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest free-vars-let-shadows-body
  "Let-bound variables are not free in the body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                  :body (list (cl-cc::make-ast-var :name 'x))))))
    (assert-equal nil result)))

(deftest free-vars-let-binding-expr-free
  "Variables in let binding expressions are free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-var :name 'y)))
                  :body (list (cl-cc::make-ast-var :name 'x))))))
    ;; y is free (in the binding expr), x is bound
    (assert-equal '(y) result)))

(deftest free-vars-let-body-has-unbound
  "Let body references to unbound variables remain free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'z)))))))
    ;; x is bound, z is free
    (assert-equal '(z) result)))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(deftest free-vars-lambda-shadows-params
  "Lambda required params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x y)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'y)))))))
    (assert-equal nil result)))

(deftest free-vars-lambda-captures-outer
  "Lambda body referencing outer variables marks them free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'z)))))))
    ;; x is a param (bound), z is free
    (assert-equal '(z) result)))

(deftest free-vars-lambda-rest-param
  "Lambda rest param is not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x)
                  :rest-param 'rest
                  :body (list (cl-cc::make-ast-var :name 'rest))))))
    (assert-equal nil result)))

(deftest free-vars-lambda-optional-param
  "Lambda optional params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc::make-ast-int :value 0)))
                  :body (list (cl-cc::make-ast-var :name 'a))))))
    (assert-equal nil result)))

(deftest free-vars-lambda-optional-default-free
  "Free variables in optional param defaults are captured."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc::make-ast-var :name 'z)))
                  :body (list (cl-cc::make-ast-var :name 'a))))))
    ;; a is bound as optional, z is free from the default expression
    (assert-equal '(z) result)))

(deftest free-vars-lambda-key-param
  "Lambda keyword params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc::make-ast-var :name 'k))))))
    (assert-equal nil result)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(deftest free-vars-nested-let
  "Inner let does not shadow outer free variables."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                  :body (list
                         (cl-cc::make-ast-let
                          :bindings (list (cons 'y (cl-cc::make-ast-var :name 'x)))
                          :body (list (cl-cc::make-ast-binop
                                       :op '+
                                       :lhs (cl-cc::make-ast-var :name 'y)
                                       :rhs (cl-cc::make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (assert-equal '(w) result)))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(deftest free-vars-defun-shadows-params
  "Defun params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'a)
                               :rhs (cl-cc::make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (assert-equal '(c) result)))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(deftest free-vars-setq
  "Setq marks both the variable and the value expression's free vars."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-setq
                  :var 'x
                  :value (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(deftest free-vars-call-symbol-func
  "Call with a symbol function name has free vars only from args."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func 'foo
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    ;; Symbol func doesn't contribute free vars, only the arg x
    (assert-equal '(x) result)))

(deftest free-vars-call-ast-func
  "Call with an AST function node has free vars from both func and args."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func (cl-cc::make-ast-var :name 'f)
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    (assert-true (and (member 'f result) (member 'x result)))
    (assert-equal 2 (length result))))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(deftest free-vars-flet-shadows-func-name
  "Flet function names are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-flet
                  :bindings (list (list 'my-fn '(a) (cl-cc::make-ast-var :name 'a)))
                  :body (list (cl-cc::make-ast-call
                               :func 'my-fn
                               :args (list (cl-cc::make-ast-int :value 1))))))))
    ;; my-fn is shadowed by flet, a is shadowed inside the binding body
    (assert-equal nil result)))

(deftest free-vars-labels-captures-outer
  "Labels bodies with outer references mark them free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-labels
                  :bindings (list (list 'rec '(n) (cl-cc::make-ast-var :name 'limit)))
                  :body (list (cl-cc::make-ast-call
                               :func 'rec
                               :args (list (cl-cc::make-ast-int :value 0))))))))
    ;; rec is bound by labels, n is param inside binding, limit is free
    (assert-equal '(limit) result)))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(deftest free-vars-if-all-branches
  "If collects free vars from condition, then, and else branches."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-var :name 'p)
                  :then (cl-cc::make-ast-var :name 'x)
                  :else (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'p result) (member 'x result) (member 'y result)))
    (assert-equal 3 (length result))))

(deftest free-vars-progn
  "Progn collects free vars from all forms."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-progn
                  :forms (list (cl-cc::make-ast-var :name 'a)
                               (cl-cc::make-ast-var :name 'b))))))
    (assert-true (and (member 'a result) (member 'b result)))
    (assert-equal 2 (length result))))
