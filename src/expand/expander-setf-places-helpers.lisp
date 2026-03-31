(in-package :cl-cc)

;;; Helper extracted from expander-helpers.lisp.
;;; Only setf-place cons-cell expansion uses this logic.

(defun expand-setf-cons-place (place value)
  "Expand (setf (ACCESSOR ARGS...) value) for cons-cell accessors to rplaca/rplacd."
  (let ((v (gensym "V")))
    (case (car place)
      ((car first)
       `(let ((,v ,value)) (rplaca ,(second place) ,v) ,v))
      ((cdr rest)
       `(let ((,v ,value)) (rplacd ,(second place) ,v) ,v))
      (nth
       `(let ((,v ,value)) (rplaca (nthcdr ,(second place) ,(third place)) ,v) ,v))
      (cadr
       `(let ((,v ,value)) (rplaca (cdr ,(second place)) ,v) ,v))
      (cddr
       `(let ((,v ,value)) (rplacd (cdr ,(second place)) ,v) ,v)))))
