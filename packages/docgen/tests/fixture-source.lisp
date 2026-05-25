(in-package :cl-user)

(defun documented-function (x)
  "Return X unchanged."
  x)

(defun undocumented-function (x)
  x)

(defmacro documented-macro (&body body)
  "Evaluate BODY in order."
  `(progn ,@body))

(defstruct documented-struct
  "A documented fixture structure."
  value)

(defclass documented-class ()
  ()
  (:documentation "A documented fixture class."))

(defparameter *documented-parameter* 42
  "A documented fixture parameter.")

(defvar *documented-var* nil
  "A documented fixture variable.")
