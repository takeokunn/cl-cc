(in-package :cl-cc/expand)

;;; Helper extracted from expander-helpers.lisp.
;;; Only setf-place cons-cell expansion uses this logic.

(defun expand-setf-cons-place (place value)
  "Expand (setf (ACCESSOR ARGS...) value) for cons-cell accessors to rplaca/rplacd."
  (let ((v (gensym "V")))
    (case (car place)
      ((car first)
       (list 'let (list (list v value))
             (list 'rplaca (second place) v)
             v))
      ((cdr rest)
       (list 'let (list (list v value))
             (list 'rplacd (second place) v)
             v))
      (nth
       (list 'let (list (list v value))
             (list 'rplaca (list 'nthcdr (second place) (third place)) v)
             v))
      (cadr
       (list 'let (list (list v value))
             (list 'rplaca (list 'cdr (second place)) v)
             v))
      (cddr
       (list 'let (list (list v value))
             (list 'rplacd (list 'cdr (second place)) v)
             v)))))
