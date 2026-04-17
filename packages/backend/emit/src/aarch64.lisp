(in-package :cl-cc/emit)

(defclass aarch64-target (target) ())

(defparameter *aarch64-abi-arg-regs*
  '("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7")
  "AAPCS64 argument registers used for runtime helper calls.")

(defun %aarch64-vm-register-p (x)
  (and (symbolp x) (member x '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7) :test #'eq)))

(defun %aarch64-emit-abi-arg (target stream abi-reg arg)
  (if (%aarch64-vm-register-p arg)
      (format stream "  mov ~A, ~A~%" abi-reg (target-register target arg))
      (format stream "  mov ~A, ~A~%" abi-reg arg)))

(defun %aarch64-emit-runtime-call (target stream callee args &optional result-reg)
  (let* ((reg-args (subseq args 0 (min (length args) (length *aarch64-abi-arg-regs*))))
         (extra-args (nthcdr (length *aarch64-abi-arg-regs*) args)))
    (loop for arg in reg-args
          for abi-reg in *aarch64-abi-arg-regs*
          do (%aarch64-emit-abi-arg target stream abi-reg arg))
    (loop for arg in (reverse extra-args)
          do (if (%aarch64-vm-register-p arg)
                 (format stream "  str ~A, [sp, #-16]!~%" (target-register target arg))
                 (progn
                   (format stream "  mov x9, ~A~%" arg)
                   (format stream "  str x9, [sp, #-16]!~%"))))
    (format stream "  bl ~A~%" callee)
    (when result-reg
      (format stream "  mov ~A, x0~%" (target-register target result-reg)))
    (when extra-args
      (format stream "  add sp, sp, #~D~%" (* 16 (length extra-args))))))

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
  (declare (ignore target))
  (format stream "  .align 4~%")
  (format stream "~A:~%" (vm-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-jump) stream)
  (format stream "  b ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-jump-zero) stream)
  (format stream "  cmp ~A, #0~%"
          (target-register target (vm-reg inst)))
  (format stream "  b.eq ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-print) stream)
  (%aarch64-emit-runtime-call target stream
                             "rt-print"
                             (list (vm-reg inst))
                             nil))

(defmethod emit-instruction ((target aarch64-target) (inst vm-halt) stream)
  (format stream "  mov x0, ~A~%"
          (target-register target (vm-reg inst)))
  (format stream "  ret~%"))

(defmethod emit-instruction ((target aarch64-target) (inst vm-class-def) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-defclass"
                              (list (vm-class-name-sym inst)
                                    (vm-superclasses inst)
                                    (vm-slot-names inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-make-obj) stream)
  (%aarch64-emit-runtime-call target stream
                              (if (null (vm-initarg-regs inst))
                                  "rt-make-instance-0"
                                  "rt-make-instance")
                              (append (list (vm-class-reg inst))
                                      (loop for (key . reg) in (vm-initarg-regs inst)
                                            append (list key reg)))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-slot-read) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-slot-value"
                              (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-slot-write) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-slot-set"
                              (list (vm-obj-reg inst)
                                    (vm-slot-name-sym inst)
                                    (vm-value-reg inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-register-method) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-register-method"
                              (list (vm-gf-reg inst)
                                    (vm-method-specializer inst)
                                    (vm-method-reg inst))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-generic-call) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-call-generic"
                              (cons (vm-gf-reg inst) (vm-args inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-slot-boundp) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-slot-boundp"
                              (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-slot-makunbound) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-slot-makunbound"
                              (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-slot-exists-p) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-slot-exists-p"
                              (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-class-name-fn) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-class-name"
                              (list (vm-src inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-class-of-fn) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-class-of"
                              (list (vm-src inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-find-class) stream)
  (%aarch64-emit-runtime-call target stream
                              "rt-find-class"
                              (list (vm-src inst))
                              (vm-dst inst)))

(defmethod emit-instruction ((target aarch64-target) (inst vm-spill-store) stream)
  (let* ((src-reg (vm-spill-src inst))
         (asm-str (string-downcase (symbol-name src-reg))))
    (format stream "  str ~A, [x29, #-~A]~%"
            asm-str
            (* (vm-spill-slot inst) 8))))

(defmethod emit-instruction ((target aarch64-target) (inst vm-spill-load) stream)
  (let* ((dst-reg (vm-spill-dst inst))
         (asm-str (string-downcase (symbol-name dst-reg))))
    (format stream "  ldr ~A, [x29, #-~A]~%"
            asm-str
            (* (vm-spill-slot inst) 8))))
