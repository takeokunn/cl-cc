(in-package :cl-cc/pbt)

;; ----------------------------------------------------------------------------
;; CPS Transformation Property-Based Tests
;; ----------------------------------------------------------------------------
;; This module tests the CPS transformation with property-based testing.
;; Properties verify that CPS conversion preserves semantics while transforming
;; expressions into continuation-passing style.

;; ----------------------------------------------------------------------------
;; Test Helpers
;; ----------------------------------------------------------------------------

(defun cps-to-sexp (ast)
  "Convert AST to S-expression."
  (ast-to-sexp ast))

(defun cps-of-expr (expr)
  "Apply CPS transformation to an expression."
  (cps-transform expr))

(defun cps-lambda-form-p (cps-expr)
  "Return T when CPS-EXPR is a lambda form."
  (and (consp cps-expr)
       (eq (car cps-expr) 'lambda)))

(defun cps-continuation-p (cps-expr)
  "Check if CPS expression is a lambda with continuation parameter.
   Returns T if CPS-EXPR has the form (lambda (k) ...)."
  (and (cps-lambda-form-p cps-expr)
       (consp (second cps-expr))
       (symbolp (car (second cps-expr)))
       (= (length (second cps-expr)) 1)))

(defun cps-lambda-single-parameter-p (cps-expr)
  "Return T when CPS-EXPR is a single-parameter lambda."
  (and (cps-lambda-form-p cps-expr)
       (consp (second cps-expr))
       (= (length (second cps-expr)) 1)))

(defun get-continuation-name (cps-expr)
  "Extract the continuation parameter name from a CPS expression."
  (and (cps-continuation-p cps-expr)
       (car (second cps-expr))))

(defun cps-continuation-name-k-p (cps-expr)
  "Return T when the continuation parameter is named K."
  (and (cps-continuation-p cps-expr)
       (string= (symbol-name (get-continuation-name cps-expr)) "K")))

(defmacro define-cps-shape-properties (&rest specs)
  "Define a batch of CPS shape properties from a data table."
  `(progn
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (name args expr check) spec
                   `(defproperty ,name ,args
                       (funcall #',check (cps-transform ,expr)))))
                specs)))

(in-suite cl-cc-pbt-suite)

(define-cps-shape-properties
  (cps-constant-introduces-continuation
      (n (gen-integer :min -1000 :max 1000))
      n
      cps-continuation-p)

  (cps-variable-preserves-symbol
      (sym (gen-symbol :prefix "VAR"))
      sym
      cps-continuation-p)

  (cps-continuation-is-named-k
      (n (gen-integer :min -1000 :max 1000))
      n
      cps-continuation-name-k-p)

  (cps-addition-produces-lambda
      (a (gen-integer :min -100 :max 100)
       b (gen-integer :min -100 :max 100))
      `(+ ,a ,b)
      cps-lambda-form-p)

  (cps-subtraction-produces-lambda
      (a (gen-integer :min -100 :max 100)
       b (gen-integer :min -100 :max 100))
      `(- ,a ,b)
      cps-lambda-form-p)

  (cps-multiplication-produces-lambda
      (a (gen-integer :min -100 :max 100)
       b (gen-integer :min -100 :max 100))
      `(* ,a ,b)
      cps-lambda-form-p)

  (cps-if-produces-lambda
      (cond-val (gen-one-of '(-1 0 1))
       then-val (gen-integer :min -100 :max 100)
       else-val (gen-integer :min -100 :max 100))
      `(if ,cond-val ,then-val ,else-val)
      cps-lambda-form-p)

  (cps-let-produces-lambda
      (val1 (gen-integer :min -100 :max 100))
      `(let ((x ,val1)) x)
      cps-lambda-form-p)

  (cps-progn-produces-lambda
      (vals (gen-list-of (gen-integer :min -10 :max 10)
                         :min-length 1 :max-length 3))
      `(progn ,@vals)
      cps-lambda-form-p)

  (cps-lambda-has-one-parameter
      (n (gen-integer :min -100 :max 100))
      n
      cps-lambda-single-parameter-p)

  (cps-nested-addition-produces-lambda
      (a (gen-integer :min -50 :max 50)
       b (gen-integer :min -50 :max 50)
       c (gen-integer :min -50 :max 50))
      `(+ (+ ,a ,b) ,c)
      cps-lambda-form-p)

  (cps-print-produces-lambda
      (val (gen-integer :min -100 :max 100))
      `(print ,val)
      cps-continuation-p))

;; -------------------------------------------------------------------------
;; Property 13: Identity function for simple cases
;; -------------------------------------------------------------------------

(defproperty cps-identity-for-simple-variable
    (sym (gen-symbol :prefix "SIMPLE"))
  (let* ((cps-result (cps-transform sym))
         (body (caddr cps-result)))
    ;; Body should be a funcall expression with the variable
    (and (consp body)
         (eq (car body) 'funcall)
         (eq (third body) sym))))
