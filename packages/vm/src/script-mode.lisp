;;;; packages/vm/src/script-mode.lisp --- script-mode runtime scaffolding

(in-package :cl-cc/vm)

(defvar *script-argv* nil
  "Command-line arguments visible to Lisp script-mode programs.")

(defun script-argv ()
  "Return a copy of the current script-mode argv list."
  (copy-list *script-argv*))

(export '(*script-argv* script-argv))
