(in-package :cl-cc/expand)

;;; Helpers used only by expander-control.lisp.

(defun expand-let-binding (b)
  "Macro-expand the value in a LET binding, leaving the binding name untouched."
  (if (and (consp b) (symbolp (car b)))
      (list (car b) (compiler-macroexpand-all (cadr b)))
      b))

(defun expand-flet-labels-binding (binding)
  "Macro-expand only the body forms of an FLET/LABELS binding; leave params untouched."
  (if (and (consp binding) (>= (length binding) 3))
      (list* (first binding) (second binding)
             (mapcar #'compiler-macroexpand-all (cddr binding)))
      binding))
