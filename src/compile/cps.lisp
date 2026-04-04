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
         (let ((v (gensym "COND")))
           (%cps-sexp-node (second node)
             `(lambda (,v)
                (if ,v
                    ,(%cps-sexp-node (third node) k)
                    ,(%cps-sexp-node (fourth node) k))))))
        (progn
         (%cps-sexp-progn (cdr node) k))
       (let
        (%cps-sexp-let-bindings (second node)
                                `(progn ,@(cddr node))
                                k))
       (print
        (let ((v (gensym "PRINT")))
          (%cps-sexp-node (second node)
            `(lambda (,v)
               (print ,v)
               (funcall ,k ,v)))))
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

(defun %cps-beta-reduce (form)
  (if (and (consp form)
           (eq (car form) 'funcall)
           (consp (second form))
           (eq (car (second form)) 'lambda)
           (consp (second (second form)))
           (null (cddr (second (second form))))
           (= (length (second form)) 3))
      (let* ((lambda-form (second form))
             (params (second lambda-form))
             (body (third lambda-form))
             (arg (third form)))
        (%cps-simplify-subtree body (first params) arg))
      form))

(defun %cps-eta-reduce (form)
  (if (and (consp form)
           (eq (car form) 'lambda)
           (consp (second form))
           (null (cdr (second form)))
           (consp (third form))
           (eq (car (third form)) 'funcall)
           (consp (cddr (third form)))
           (null (cdddr (third form)))
           (eq (third (third form)) (first (second form)))
           )
      (second (third form))
      form))

(defun cps-simplify-form (form)
  (let ((reduced
          (labels ((walk (x)
                     (cond
                       ((consp x)
                        (walk (%cps-eta-reduce
                               (%cps-beta-reduce
                                (cons (walk (car x)) (walk (cdr x)))))))
                       (t x))))
            (walk form))))
    (if (equal reduced form) reduced (cps-simplify-form reduced))))

(defun cps-transform (expr)
  "Minimal CPS conversion for bootstrap language. Produces (lambda (k) ...).
The outer continuation parameter is always named K for inspection and tests."
  (cps-simplify-form `(lambda (k) ,(%cps-sexp-node expr 'k))))

;;; AST-based CPS implementation moved to cps-ast.lisp.
