(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Bignum Host-Integer Interface (FR-952/FR-955/FR-956)
;;;
;;; Provides externalized adapter functions that perform bignum arithmetic
;;; and return results as host integers.  These sit between the internal
;;; vm-bignum layer (vm-bignum.lisp) and callers that operate in host-integer
;;; space (primitives.lisp, vm-execute.lisp, vm-extensions.lisp,
;;; vm-bignum-rational.lisp).
;;;
;;; Load order: after vm-bignum.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun vm-bignum-add-integers (lhs rhs)
  "Native VM bignum addition, returning a host integer."
  (vm-bignum-to-integer (vm-bignum-add lhs rhs)))

(defun vm-bignum-subtract-integers (lhs rhs)
  "Native VM bignum subtraction, returning a host integer."
  (vm-bignum-to-integer (vm-bignum-sub lhs rhs)))

(defun vm-bignum-multiply-integers (lhs rhs)
  "Multiply integer operands using native VM bignum arithmetic, returning a host integer."
  (vm-bignum-to-integer (vm-bignum-mul lhs rhs)))

(defun vm-bignum-burnikel-ziegler-divide (lhs rhs &key (rounding :truncate))
  "Divide LHS by RHS using native VM bignum arithmetic, returning host integer quotient/remainder.

ROUNDING selects the rounding mode: :truncate (default), :floor, :ceiling, or :round."
  (labels ((trunc (a b)
             (multiple-value-bind (q r) (vm-bignum-div a b)
               (values (vm-bignum-to-integer q) (vm-bignum-to-integer r))))
           (floor2 (a b)
             (multiple-value-bind (q r) (trunc a b)
               (if (and (not (zerop r)) (minusp (* a b)))
                   (values (1- q) (+ r b))
                   (values q r))))
           (ceiling2 (a b)
             (multiple-value-bind (q r) (trunc a b)
               (if (and (not (zerop r)) (plusp (* a b)))
                   (values (1+ q) (- r b))
                   (values q r))))
           (round2 (a b)
             (multiple-value-bind (q r) (trunc a b)
               (let* ((abs-r (abs r))
                      (abs-b (abs b))
                      (cmp (- (* 2 abs-r) abs-b)))
                 (cond
                   ((minusp cmp) (values q r))
                   ((or (plusp cmp) (oddp q))
                    (let ((q2 (if (plusp (* a b)) (1+ q) (1- q))))
                      (values q2 (- a (* q2 b)))))
                   (t (values q r)))))))
    (ecase rounding
      (:truncate (trunc lhs rhs))
      (:floor (floor2 lhs rhs))
      (:ceiling (ceiling2 lhs rhs))
      (:round (round2 lhs rhs)))))
