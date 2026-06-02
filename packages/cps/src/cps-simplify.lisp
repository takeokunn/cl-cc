;;;; cps-simplify.lisp — CPS Simplification (Beta/Eta Reduction)
;;;;
;;;; This module provides:
;;;; - %cps-simplify-subtree — substitution helper
;;;; - %single-param-lambda-parts / %single-param-lambda-p
;;;; - %funcall-single-lambda-parts / %funcall-of-single-lambda-p
;;;; - %eta-reducible-lambda-p
;;;; - %cps-beta-reduce / %cps-eta-reduce
;;;; - %cps-simplify-walk — bottom-up beta/eta reducer
;;;; - %cps-simplify-fixed-point — fixed-point iteration driver
;;;; - cps-simplify-form — public entry point
;;;;
;;;; Load order: after cps-trampoline.lisp, before cps.lisp.

(in-package :cl-cc/cps)

(defun %cps-simplify-subtree (tree var replacement)
  "Substitute all occurrences of VAR in TREE with REPLACEMENT."
  (cond
    ((eq tree var) replacement)
    ((consp tree)
     (cons (%cps-simplify-subtree (car tree) var replacement)
           (%cps-simplify-subtree (cdr tree) var replacement)))
    (t tree)))

(defun %single-param-lambda-parts (form)
  "Return (values param body) when FORM is a one-argument lambda, else NIL values."
  (when (and (consp form)
             (eq (car form) 'lambda)
             (let ((params (second form)))
               (and (consp params) (null (cdr params))))
             (= (length form) 3))
    (values (first (second form))
            (third form))))

(defun %single-param-lambda-p (form)
  "T if FORM is (lambda (x) body) with exactly one parameter."
  (multiple-value-bind (param body)
      (%single-param-lambda-parts form)
    (declare (ignore body))
    (not (null param))))

(defun %funcall-single-lambda-parts (form)
  "Return (values param body arg) when FORM is a beta-reducible funcall, else NIL values."
  (when (and (consp form)
             (eq (car form) 'funcall)
             (not (null (cddr form)))
             (null (cdddr form)))
    (multiple-value-bind (param body)
        (%single-param-lambda-parts (second form))
      (when param
        (values param body (third form))))))

(defun %funcall-of-single-lambda-p (form)
  "T if FORM is (funcall (lambda (x) body) arg) — beta-reducible."
  (multiple-value-bind (param body arg)
      (%funcall-single-lambda-parts form)
    (declare (ignore body arg))
    (not (null param))))

(defun %eta-reducible-lambda-p (form)
  "T if FORM is (lambda (x) (funcall f x)) — eta-reducible."
  (multiple-value-bind (param body)
      (%single-param-lambda-parts form)
    (and param
         (consp body)
         (eq (car body) 'funcall)
         (consp (cddr body))
         (null (cdddr body))
         (eq (third body) param))))

(defun %cps-beta-reduce (form)
  "Beta-reduce (funcall (lambda (x) body) arg) to body[x/arg]."
  (multiple-value-bind (param body arg)
      (%funcall-single-lambda-parts form)
    (if param
        (%cps-simplify-subtree body param arg)
        form)))

(defun %cps-eta-reduce (form)
  "Eta-reduce (lambda (x) (funcall f x)) to f."
  (if (%eta-reducible-lambda-p form)
      (second (third form))
      form))

(defun %cps-simplify-walk (x)
  "Recursively apply beta/eta reduction bottom-up to sexp X."
  (if (consp x)
      (%cps-eta-reduce
       (%cps-beta-reduce
        (mapcar #'%cps-simplify-walk x)))
      x))

(defun %cps-simplify-fixed-point (form step-fn)
  "Apply STEP-FN to FORM until a fixed point is reached."
  (loop
    for current = form then next
    for next = (funcall step-fn current)
    until (equal current next)
    finally (return current)))

(defun cps-simplify-form (form)
  "Repeatedly simplify FORM until a fixed point is reached."
  (%cps-simplify-fixed-point form #'%cps-simplify-walk))
