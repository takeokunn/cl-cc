;;;; macros-compat-array.lisp — Array compatibility wrappers
(in-package :cl-cc/expand)


(register-macro 'adjustable-array-p
  (lambda (form env)
    (declare (ignore env))
    (%ignore-argument-expand (second form) t)))

(register-macro 'array-has-fill-pointer-p
  (lambda (form env)
    (declare (ignore env))
    (%ignore-argument-expand (second form) nil)))
