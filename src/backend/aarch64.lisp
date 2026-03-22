(in-package :cl-cc)

(defclass aarch64-target (target) ())

(defmethod target-register ((target aarch64-target) virtual-register)
  (declare (ignore target))
  (let* ((index (or (parse-integer (subseq (symbol-name virtual-register) 1)
                                   :junk-allowed t)
                    0))
         (pool '("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7")))
    (when (>= index (length pool))
      (error "Register spilling is not implemented yet (needed: ~A)" virtual-register))
    (nth index pool)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-const) stream)
  (format stream "  mov ~A, #~A~%"
          (target-register target (vm-dst inst))
          (vm-value inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-move) stream)
  (format stream "  mov ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-src inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-add) stream)
  (format stream "  add ~A, ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-lhs inst))
          (target-register target (vm-rhs inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-sub) stream)
  (format stream "  sub ~A, ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-lhs inst))
          (target-register target (vm-rhs inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-mul) stream)
  (format stream "  mul ~A, ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-lhs inst))
          (target-register target (vm-rhs inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-label) stream)
  (format stream "~A:~%" (vm-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-jump) stream)
  (format stream "  b ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-jump-zero) stream)
  (format stream "  cmp ~A, #0~%"
          (target-register target (vm-reg inst)))
  (format stream "  b.eq ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-print) stream)
  (declare (ignore stream))
  (error "print backend emission is not implemented yet (~A)" (vm-reg inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-halt) stream)
  (format stream "  mov x0, ~A~%"
          (target-register target (vm-reg inst)))
  (format stream "  ret~%"))
