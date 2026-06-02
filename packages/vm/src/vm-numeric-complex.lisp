(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Complex Number Unboxing Plans
;;;
;;; Contains: vm-complex-unbox-plan, vm-complex-unboxed-add-plan,
;;; vm-complex-add-with-unboxing — complex number boxing/unboxing helpers.
;;;
;;; Load order: after vm-numeric-bignum-algorithms.lisp,
;;;             before vm-numeric-tower.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun vm-complex-unbox-plan (value &key local-p)
  "Describe whether VALUE can be represented as split real/imag registers."
  (if (and local-p (complexp value))
      (list :representation :split-registers
            :real (realpart value)
            :imag (imagpart value))
      (list :representation :boxed
            :value value)))

(defun vm-complex-unboxed-add-plan (lhs rhs &key local-p)
  "Return a split-register addition plan for local complex operands, or NIL."
  (let ((lhs-plan (vm-complex-unbox-plan lhs :local-p local-p))
        (rhs-plan (vm-complex-unbox-plan rhs :local-p local-p)))
    (when (and (eq (getf lhs-plan :representation) :split-registers)
               (eq (getf rhs-plan :representation) :split-registers))
      (list :representation :split-registers
            :real (+ (getf lhs-plan :real) (getf rhs-plan :real))
            :imag (+ (getf lhs-plan :imag) (getf rhs-plan :imag))))))

(defun vm-complex-add-with-unboxing (lhs rhs &key local-p)
  "Execute complex addition via unboxed plan when possible, else boxed fallback."
  (let ((plan (vm-complex-unboxed-add-plan lhs rhs :local-p local-p)))
    (if plan
        (complex (getf plan :real) (getf plan :imag))
        (+ lhs rhs))))
