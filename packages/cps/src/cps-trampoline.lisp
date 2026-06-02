;;;; cps-trampoline.lisp — CPS Trampoline Infrastructure
;;;;
;;;; This module provides:
;;;; - cps-trampoline-thunk struct
;;;; - cps-trampoline-run — force thunks until a plain value is produced
;;;; - %cps-tail-funcall-form-p, %cps-trampoline-tail-form, %cps-wrap-trampoline-run
;;;; - cps-trampoline-form — source-level trampoline rewrite for continuation lambdas
;;;;
;;;; Load order: before cps.lisp and cps-simplify.lisp; no dependencies within cps.

(in-package :cl-cc/cps)

(defstruct cps-trampoline-thunk
  "A zero-argument CPS trampoline thunk returned from a tail continuation."
  (function (lambda () nil) :type function))

(defun cps-trampoline-run (value)
  "Force CPS trampoline thunks until VALUE is no longer a thunk."
  (loop for current = value then (funcall (cps-trampoline-thunk-function current))
        while (cps-trampoline-thunk-p current)
        finally (return current)))

(defun %cps-tail-funcall-form-p (form)
  "Return T when FORM is a direct tail-position FUNCALL form."
  (and (consp form)
       (eq (car form) 'funcall)))

(defun %cps-trampoline-tail-form (form)
  "Return FORM wrapped as a trampoline thunk for deferred tail dispatch."
  (list 'list
        :cps-trampoline-thunk
        (list 'lambda nil form)))

(defun %cps-wrap-trampoline-run (form)
  "Wrap FORM in a self-contained trampoline loop understood by generated CPS code."
  (list 'labels
        (list (list 'cps-trampoline-run-internal
                    (list 'value)
                    (list 'if
                          (list 'and
                                (list 'consp 'value)
                                (list 'eq (list 'car 'value) :cps-trampoline-thunk))
                          (list 'cps-trampoline-run-internal
                                (list 'funcall (list 'cadr 'value)))
                          'value)))
        (list 'cps-trampoline-run-internal form)))

(defun cps-trampoline-form (form)
  "Convert continuation lambdas whose body is only a tail call into thunk producers."
  (cond
    ((and (consp form)
          (eq (car form) 'lambda)
          (consp (cdr form))
          (consp (second form))
          (null (cddr (second form)))
          (= (length form) 3)
          (%cps-tail-funcall-form-p (third form)))
     (list 'lambda
           (second form)
           (%cps-trampoline-tail-form (cps-trampoline-form (third form)))))
    ((consp form)
     (mapcar #'cps-trampoline-form form))
    (t form)))
