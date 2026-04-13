;;;; cps.lisp - Continuation-Passing Style Transformation
;;;;
;;;; This module provides:
;;;; - S-expression based CPS transformation (minimal, for bootstrap)
;;;; - AST-based CPS transformation with full special form support

(in-package :cl-cc)

;;; S-Expression Based CPS Transformation (Minimal Bootstrap)
;;;
;;; Handles a small subset of CL sufficient for bootstrapping:
;;; integers, symbols, arithmetic (+/-/*), if, progn, let, print.

(defun %cps-sexp-binop (op a b k)
  "CPS-transform (OP A B) — evaluates A then B, applies OP, passes result to K."
  (let ((va (gensym "A"))
        (vb (gensym "B")))
    (%cps-sexp-node a
      `(lambda (,va)
         ,(%cps-sexp-node b
             `(lambda (,vb)
                (funcall ,k (,op ,va ,vb))))))))

(defun %cps-sexp-progn (forms k)
  "CPS-transform a sequence of forms, passing only the last result to K."
  (if (null (cdr forms))
      (%cps-sexp-node (car forms) k)
      (let ((tmp (gensym "TMP")))
        (%cps-sexp-node (car forms)
          `(lambda (,tmp)
             (declare (ignore ,tmp))
             ,(%cps-sexp-progn (cdr forms) k))))))

(defun %cps-sexp-let-bindings (bindings body k)
  "CPS-transform LET bindings left-to-right, then execute BODY with continuation K."
  (if (null bindings)
      (%cps-sexp-node body k)
      (let* ((sym (first  (car bindings)))
             (val (second (car bindings)))
             (tmp (gensym (symbol-name sym))))
        (%cps-sexp-node val
          `(lambda (,tmp)
             (let ((,sym ,tmp))
               ,(%cps-sexp-let-bindings (cdr bindings) body k)))))))

(defun %cps-falsep (value)
  "Return true when VALUE is falsy in the bootstrap CPS language.

Bootstrap CPS follows the language-level truthiness used by compiled code:
both NIL and numeric zero are false."
  (or (null value)
      (and (numberp value)
           (zerop value))))

(defun %cps-sexp-if (node k)
  "CPS-transform an IF form represented by NODE."
  (let ((v (gensym "COND")))
    (%cps-sexp-node (second node)
      `(lambda (,v)
         (if (%cps-falsep ,v)
             ,(%cps-sexp-node (fourth node) k)
             ,(%cps-sexp-node (third node) k))))))

(defun %cps-sexp-print (node k)
  "CPS-transform a PRINT form represented by NODE."
  (let ((v (gensym "PRINT")))
    (%cps-sexp-node (second node)
      `(lambda (,v)
         (print ,v)
         (funcall ,k ,v)))))

(defun %cps-sexp-node (node k)
  "CPS-transform a single bootstrap S-expression NODE with continuation K."
  (cond
    ((integerp node) `(funcall ,k ,node))
    ((symbolp  node) `(funcall ,k ,node))
    ((consp node)
     (case (car node)
       ((+ - *)
        (%cps-sexp-binop (car node) (second node) (third node) k))
        (if
         (%cps-sexp-if node k))
        (progn
         (%cps-sexp-progn (cdr node) k))
        (let
         (%cps-sexp-let-bindings (second node)
                                 `(progn ,@(cddr node))
                                 k))
        (print
         (%cps-sexp-print node k))
        (otherwise
         (error "Unsupported form in CPS: ~S" (car node)))))
    (t
      (error "Unsupported node in CPS: ~S" node))))

(defun %cps-simplify-subtree (tree var replacement)
  (cond
    ((eq tree var) replacement)
    ((consp tree)
     (cons (%cps-simplify-subtree (car tree) var replacement)
           (%cps-simplify-subtree (cdr tree) var replacement)))
    (t tree)))

(defun %single-param-lambda-p (form)
  "T if FORM is (lambda (x) body) with exactly one parameter."
  (and (consp form)
       (eq (car form) 'lambda)
       (let ((params (second form)))
         (and (consp params) (null (cdr params))))
       (= (length form) 3)))

(defun %funcall-of-single-lambda-p (form)
  "T if FORM is (funcall (lambda (x) body) arg) — beta-reducible."
  (and (consp form)
       (eq (car form) 'funcall)
       (%single-param-lambda-p (second form))
       (not (null (cddr form)))
       (null (cdddr form))))

(defun %eta-reducible-lambda-p (form)
  "T if FORM is (lambda (x) (funcall f x)) — eta-reducible."
  (and (%single-param-lambda-p form)
       (let ((body (third form)))
         (and (consp body)
              (eq (car body) 'funcall)
              (consp (cddr body))
              (null (cdddr body))
              (eq (third body) (first (second form)))))))

(defun %cps-beta-reduce (form)
  "Beta-reduce (funcall (lambda (x) body) arg) → body[x/arg]."
  (if (%funcall-of-single-lambda-p form)
      (let* ((lambda-form (second form))
             (param (first (second lambda-form)))
             (body  (third lambda-form))
             (arg   (third form)))
        (%cps-simplify-subtree body param arg))
      form))

(defun %cps-eta-reduce (form)
  "Eta-reduce (lambda (x) (funcall f x)) → f."
  (if (%eta-reducible-lambda-p form)
      (second (third form))
      form))

(defun %cps-simplify-once (form)
  "Apply one recursive beta/eta simplification pass to FORM."
  (labels ((walk (x)
             (if (consp x)
                 (%cps-eta-reduce
                  (%cps-beta-reduce
                   (mapcar #'walk x)))
                 x)))
    (walk form)))

(defun cps-simplify-form (form)
  "Repeatedly simplify FORM until a fixed point is reached."
  (loop
    for current = form then next
    for next = (%cps-simplify-once current)
    until (equal current next)
    finally (return current)))

(defun cps-transform (expr)
  "Minimal CPS conversion for bootstrap language. Produces (lambda (k) ...).
The outer continuation parameter is always named K for inspection and tests."
  (cps-simplify-form `(lambda (k) ,(%cps-sexp-node expr 'k))))

;;; AST-based CPS implementation moved to cps-ast.lisp.
