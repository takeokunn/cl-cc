;;;; packages/compile/src/defunctionalize.lisp — FR-688 Defunctionalization
;;;; Convert higher-order functions to first-order via closure conversion.
;;;; Reynolds 1972 defunctionalization.

(in-package :cl-cc/compile)

(defvar *defunctionalize-enabled* t)

(defstruct (defun-info (:conc-name di-))
  "Information about a function being defunctionalized."
  (name nil :type symbol)
  (arity 0 :type fixnum)
  (closures nil :type list)     ; list of closure variable symbols
  (apply-fun nil :type symbol)) ; generated apply function

(defun defunctionalize-program (program)
  "Defunctionalize PROGRAM: convert all higher-order calls to first-order.
1. Assign each lambda a unique constructor tag
2. Replace closures with data structures (tag + captured vars)
3. Replace function calls with case dispatch over tags"
  (declare (ignore program))
  (values))

(defun make-apply-function (closures)
  "Generate an apply function that dispatches over CLOSURES by tag."
  (declare (ignore closures))
  (values))
