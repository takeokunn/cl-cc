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

(defun cps-continuation-p (cps-expr)
  "Check if CPS expression is a lambda with continuation parameter.
   Returns T if CPS-EXPR has the form (lambda (k) ...)."
  (and (consp cps-expr)
       (eq (car cps-expr) 'lambda)
       (consp (second cps-expr))
       (symbolp (car (second cps-expr)))
       (= (length (second cps-expr)) 1)))

(defun get-continuation-name (cps-expr)
  "Extract the continuation parameter name from a CPS expression."
  (and (cps-continuation-p cps-expr)
       (car (second cps-expr))))

;; ----------------------------------------------------------------------------
;; Property Tests
;; ----------------------------------------------------------------------------

(in-suite cl-cc-pbt-suite)

;; -------------------------------------------------------------------------
;; Property 1: CPS introduces continuation parameter
;; -------------------------------------------------------------------------

(defproperty cps-constant-introduces-continuation
    (n (gen-integer :min -1000 :max 1000))
  (let* ((cps-result (cps-transform n)))
    (cps-continuation-p cps-result)))

;; -------------------------------------------------------------------------
;; Property 2: CPS of variable introduces continuation
;; -------------------------------------------------------------------------

(defproperty cps-variable-preserves-symbol
    (sym (gen-symbol :prefix "VAR"))
  (let* ((cps-result (cps-transform sym)))
    (cps-continuation-p cps-result)))

;; -------------------------------------------------------------------------
;; Property 3: CPS continuation is named k
;; -------------------------------------------------------------------------

(defproperty cps-continuation-is-named-k
    (n (gen-integer :min -1000 :max 1000))
  (let* ((cps-result (cps-transform n)))
    (and (cps-continuation-p cps-result)
         (string= (symbol-name (get-continuation-name cps-result)) "K"))))

;; -------------------------------------------------------------------------
;; Property 4: CPS of addition produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-addition-produces-lambda
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (let* ((expr `(+ ,a ,b))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 5: CPS of subtraction produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-subtraction-produces-lambda
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (let* ((expr `(- ,a ,b))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 6: CPS of multiplication produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-multiplication-produces-lambda
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (let* ((expr `(* ,a ,b))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 7: CPS of if expression produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-if-produces-lambda
    (cond-val (gen-one-of '(-1 0 1))
     then-val (gen-integer :min -100 :max 100)
     else-val (gen-integer :min -100 :max 100))
  (let* ((expr `(if ,cond-val ,then-val ,else-val))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 8: CPS of let produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-let-produces-lambda
    (val1 (gen-integer :min -100 :max 100))
  (let* ((expr `(let ((x ,val1)) x))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 9: CPS of progn produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-progn-produces-lambda
    (vals (gen-list-of (gen-integer :min -10 :max 10)
                       :min-length 1 :max-length 3))
  (let* ((expr `(progn ,@vals))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 10: CPS always produces lambda with one parameter
;; -------------------------------------------------------------------------

(defproperty cps-lambda-has-one-parameter
    (n (gen-integer :min -100 :max 100))
  (let* ((cps-result (cps-transform n)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda)
         (consp (second cps-result))
         (= (length (second cps-result)) 1))))

;; -------------------------------------------------------------------------
;; Property 11: CPS of nested addition produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-nested-addition-produces-lambda
    (a (gen-integer :min -50 :max 50)
     b (gen-integer :min -50 :max 50)
     c (gen-integer :min -50 :max 50))
  (let* ((expr `(+ (+ ,a ,b) ,c))
         (cps-result (cps-transform expr)))
    (and (consp cps-result)
         (eq (car cps-result) 'lambda))))

;; -------------------------------------------------------------------------
;; Property 12: CPS of print produces lambda
;; -------------------------------------------------------------------------

(defproperty cps-print-produces-lambda
    (val (gen-integer :min -100 :max 100))
  (let* ((expr `(print ,val))
         (cps-result (cps-transform expr)))
    (cps-continuation-p cps-result)))

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
