;;;; packages/engine/vm/src/package.lisp - VM Module Package Header
;;;;
;;;; All VM code lives in the :cl-cc package (defined in package.lisp).
;;;; This file exists for ASDF module ordering and switches to the correct package.

(in-package :cl-cc/vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %type-package-function (name)
    (let ((pkg (find-package :cl-cc/type)))
      (when pkg
        (let ((sym (find-symbol name pkg)))
          (when (and sym (fboundp sym))
            (symbol-function sym))))))

  (defun type-refinement-p (value)
    (let ((fn (%type-package-function "TYPE-REFINEMENT-P")))
      (and fn (funcall fn value))))

  (defun type-refinement-base (value)
    (let ((fn (%type-package-function "TYPE-REFINEMENT-BASE")))
      (when fn (funcall fn value))))

  (defun type-refinement-predicate (value)
    (let ((fn (%type-package-function "TYPE-REFINEMENT-PREDICATE")))
      (when fn (funcall fn value)))))
