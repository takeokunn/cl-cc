;;;; tests/unit/compile/cps-ast-extended-tests.lisp
;;;; Unit tests for src/compile/cps-ast-extended.lisp
;;;;
;;;; Covers: cps-transform-ast for ast-quote, ast-setq, ast-the, ast-values,
;;;;   ast-apply, ast-call (general calls), ast-defclass, ast-defgeneric,
;;;;   ast-defmethod; entry points maybe-cps-transform, cps-transform-ast*,
;;;;   cps-transform*, cps-transform-eval.
;;;;
;;;; Tests inspect the *structure* of the produced S-expression rather than
;;;; evaluating it (which would require a full runtime environment).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; Helper: transform a node with a fixed continuation symbol.
(defun cps-with-k (node)
  "Return the CPS form for NODE with continuation symbol K."
  (cl-cc::cps-transform-ast node 'k))

;;; ─── ast-quote ────────────────────────────────────────────────────────────

(deftest cps-quote-calls-k-with-quoted-value
  "CPS transform of ast-quote produces (funcall k (quote val))."
  (let ((result (cps-with-k (cl-cc::make-ast-quote :value 'hello))))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal '(quote hello) (third result))))

(deftest cps-quote-preserves-list-value
  "CPS transform of ast-quote with a list value preserves the list."
  (let ((result (cps-with-k (cl-cc::make-ast-quote :value '(a b c)))))
    (assert-equal '(quote (a b c)) (third result))))

;;; ─── ast-the ─────────────────────────────────────────────────────────────

(deftest cps-the-wraps-value-with-the-declaration
  "CPS transform of ast-the wraps the inner value in (the type ...)."
  (let* ((node (cl-cc::make-ast-the
                :type 'integer
                :value (cl-cc::make-ast-int :value 5)))
         (result (cps-with-k node)))
    ;; Result is a (cps-transform-ast (ast-int 5) (lambda (v) (funcall k (the integer v))))
    ;; The continuation arg is a lambda containing (the integer ...) and (funcall k ...)
    (assert-true (consp result))
    ;; Walk into the lambda body to find (the integer ...)
    (labels ((contains-the-p (form)
               (if (consp form)
                   (or (eq (car form) 'the)
                       (some #'contains-the-p form))
                   nil)))
      (assert-true (contains-the-p result)))))

;;; ─── ast-setq ────────────────────────────────────────────────────────────

(deftest cps-setq-contains-setq-and-funcall-k
  "CPS transform of ast-setq produces a form with (setq var ...) and (funcall k ...)."
  (let* ((node (cl-cc::make-ast-setq
                :var 'x
                :value (cl-cc::make-ast-int :value 0)))
         (result (cps-with-k node)))
    (labels ((contains-sym-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-sym-p sub sym)) (cdr form)))
                   nil)))
      (assert-true (contains-sym-p result 'setq))
      (assert-true (contains-sym-p result 'funcall)))))

;;; ─── ast-values ──────────────────────────────────────────────────────────

(deftest cps-values-empty-produces-funcall-k-nil
  "CPS transform of (values) with no forms yields (funcall k nil)."
  (let* ((node (cl-cc::make-ast-values :forms nil))
         (result (cps-with-k node)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-null (third result))))

(deftest cps-values-single-form-produces-multiple-value-call
  "CPS transform of (values x) with one form contains multiple-value-call."
  (let* ((node (cl-cc::make-ast-values
                :forms (list (cl-cc::make-ast-int :value 42))))
         (result (cps-with-k node)))
    (labels ((contains-p (form sym)
               (if (consp form)
                   (or (eq (car form) sym)
                       (some (lambda (sub) (contains-p sub sym)) (cdr form)))
                   nil)))
      (assert-true (contains-p result 'multiple-value-call)))))

;;; ─── ast-apply ───────────────────────────────────────────────────────────

(deftest cps-apply-contains-apply-call
  "CPS transform of ast-apply produces a form containing (apply func ...)."
  (let* ((node (cl-cc::make-ast-apply
                :func (cl-cc::make-ast-var :name 'f)
                :args (list (cl-cc::make-ast-var :name 'args))))
         (result (cps-with-k node)))
    (labels ((contains-apply-p (form)
               (if (consp form)
                   (or (eq (car form) 'apply)
                       (some #'contains-apply-p (cdr form)))
                   nil)))
      (assert-true (contains-apply-p result)))))

;;; ─── ast-call ─────────────────────────────────────────────────────────────

(deftest cps-call-no-args-produces-funcall-k-form
  "CPS transform of a zero-arg call produces (funcall k (func))."
  (let* ((node (cl-cc::make-ast-call :func 'compute :args nil))
         (result (cps-with-k node)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal '(compute) (third result))))

(deftest cps-call-with-args-threads-args-through-lambdas
  "CPS transform of a call with args wraps each arg evaluation in a lambda."
  (let* ((node (cl-cc::make-ast-call
                :func 'add
                :args (list (cl-cc::make-ast-int :value 1)
                            (cl-cc::make-ast-int :value 2))))
         (result (cps-with-k node)))
    ;; Should contain (funcall k (add ...)) nested in lambdas
    (labels ((contains-funcall-k-p (form)
               (if (consp form)
                   (or (and (eq (car form) 'funcall) (eq (second form) 'k))
                       (some #'contains-funcall-k-p (cdr form)))
                   nil)))
      (assert-true (contains-funcall-k-p result)))))

;;; ─── ast-defgeneric ───────────────────────────────────────────────────────

(deftest cps-defgeneric-produces-progn-defgeneric-funcall-k
  "CPS transform of ast-defgeneric produces (progn (defgeneric ...) (funcall k 'name))."
  (let* ((node (cl-cc::make-ast-defgeneric :name 'area :params '(shape)))
         (result (cps-with-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defgeneric (caadr result))
    (let ((last-form (car (last result))))
      (assert-eq 'funcall (car last-form))
      (assert-eq 'k (second last-form)))))

;;; ─── cps-transform-ast* ───────────────────────────────────────────────────

(deftest cps-transform-ast-star-returns-lambda-form
  "cps-transform-ast* wraps the transformed node in (lambda (k) ...)."
  (let* ((node (cl-cc::make-ast-int :value 7))
         (result (cl-cc::cps-transform-ast* node)))
    (assert-eq 'lambda (car result))
    (assert-= 1 (length (second result)))   ; single param
    (assert-true (symbolp (car (second result))))))  ; param is gensym

;;; ─── cps-transform* ───────────────────────────────────────────────────────

(deftest cps-transform-star-sexp-returns-cps-form
  "cps-transform* on an S-expression delegates to the sexp CPS transformer."
  (let ((result (cl-cc::cps-transform* '42)))
    ;; For a literal, cps-transform produces a form that calls the continuation
    (assert-true (consp result))))

(deftest cps-transform-star-ast-node-returns-lambda-form
  "cps-transform* on an AST node delegates to cps-transform-ast* and returns (lambda (k) ...)."
  (let* ((node (cl-cc::make-ast-quote :value 'x))
         (result (cl-cc::cps-transform* node)))
    (assert-eq 'lambda (car result))
    ;; Lambda should have one parameter
    (assert-= 1 (length (second result)))))

;;; ─── maybe-cps-transform ──────────────────────────────────────────────────

(deftest maybe-cps-transform-returns-non-nil-for-valid-ast-node
  "maybe-cps-transform returns a non-nil CPS form for a well-formed AST node."
  (let* ((node (cl-cc::make-ast-int :value 1))
         (result (cl-cc::maybe-cps-transform node)))
    (assert-true result)))

(deftest maybe-cps-transform-returns-non-nil-for-valid-sexp
  "maybe-cps-transform returns a non-nil CPS form for a valid S-expression."
  (let ((result (cl-cc::maybe-cps-transform '42)))
    (assert-true result)))

;;; ─── cps-transform-eval ───────────────────────────────────────────────────

(defun %cps-unwrap (v)
  "Apply identity to a CPS-transformed lambda to retrieve the underlying value.
cps-transform-eval returns the raw CPS lambda; other callers depend on that
shape, so we unwrap locally in these tests rather than changing the function."
  (if (functionp v) (funcall v #'identity) v))

(deftest cps-transform-eval-evaluates-integer-literal
  "cps-transform-eval evaluates the CPS-transformed form and returns a CPS lambda."
  (assert-= 42 (%cps-unwrap (cl-cc::cps-transform-eval '42))))

(deftest cps-transform-eval-evaluates-arithmetic
  "cps-transform-eval evaluates (+ 3 4) to 7 after CPS transformation."
  (assert-= 7 (%cps-unwrap (cl-cc::cps-transform-eval '(+ 3 4)))))
