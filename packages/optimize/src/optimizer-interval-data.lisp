(in-package :cl-cc/optimize)

(defparameter *opt-interval-binop-table*
  '((vm-add . opt-interval-add)
    (vm-integer-add . opt-interval-add)
    (vm-sub . opt-interval-sub)
    (vm-integer-sub . opt-interval-sub)
    (vm-mul . opt-interval-mul)
    (vm-integer-mul . opt-interval-mul))
  "Maps binary arithmetic instruction types to their interval combinator functions.")

(defparameter *opt-interval-unary-table*
  `((vm-neg . opt-interval-neg)
    (vm-abs . opt-interval-abs)
    (vm-inc . ,(lambda (a) (opt-interval-add a (opt-make-interval 1 1))))
    (vm-dec . ,(lambda (a) (opt-interval-sub a (opt-make-interval 1 1)))))
  "Maps unary arithmetic instruction types to interval transformers.")

(defparameter *opt-checked-arithmetic-elision-table*
  '((vm-add-checked . (opt-interval-add . make-vm-integer-add))
    (vm-sub-checked . (opt-interval-sub . make-vm-integer-sub))
    (vm-mul-checked . (opt-interval-mul . make-vm-integer-mul)))
  "Maps checked arithmetic instruction types to interval proof and unchecked constructors.")
