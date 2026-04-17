;;;; macros-compat-array.lisp — Array compatibility wrappers
(in-package :cl-cc/expand)

(our-defmacro adjustable-array-p (array)
  (%ignore-argument-expand array t))

(our-defmacro array-has-fill-pointer-p (array)
  (%ignore-argument-expand array nil))
