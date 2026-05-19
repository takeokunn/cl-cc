(in-package :cl-cc/codegen)

(defclass aarch64-target (target) ())

(defparameter *aarch64-abi-arg-regs*
  '("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7")
  "AAPCS64 argument registers used for runtime helper calls.")

;; Extended register pool for AArch64 text emission.
;; Registers are caller-saved (safe for allocation without prologue/epilogue):
;;   x0-x7: argument/result registers (direct mapping for :R0-:R7)
;;   x8:    indirect result register
;;   x9-x15: temporary registers
;;   x16-x17: intra-procedure-call scratch (IP0/IP1)
;; Callee-saved registers (x19-x28) are NOT included because the text emitter
;; does not generate save/restore sequences. Using them would violate the ABI.
;; When the pool is exhausted, registers must be spilled to the stack frame.
(defparameter *aarch64-register-pool*
  '("x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7"
    "x8" "x9" "x10" "x11" "x12" "x13" "x14" "x15"
    "x16" "x17")
  "AArch64 caller-saved register pool (18 registers). Virtual registers beyond
this pool are spilled to the stack frame.")

;; Scratch registers used temporarily during spill materialization.
;; x9 and x10 are caller-saved and safe to use as temporaries in all emit methods.
(defparameter *aarch64-scratch-regs*
  '("x9" "x10" "x11" "x12")
  "Scratch registers used to materialize spilled virtual registers.")

(defvar *current-aarch64-spill-base* 0
  "Next available stack offset for AArch64 text-emission spills.")

(defvar *current-aarch64-spill-home*
  (make-hash-table :test #'eq)
  "Hash table mapping virtual register keywords to stack offsets for spills.")

(defun %aarch64-virtual-register-index (virtual-register)
  "Extract the numeric index from a virtual register keyword like :R0 → 0, :R10 → 10."
  (or (parse-integer (subseq (symbol-name virtual-register) 1)
                     :junk-allowed t)
      0))

(defun %aarch64-register-from-pool (index)
  "Return the AArch64 register name string for INDEX, or NIL if out of pool."
  (when (< index (length *aarch64-register-pool*))
    (nth index *aarch64-register-pool*)))

(defun %aarch64-spill-home-offset (virtual-register)
  "Return the stack offset for a spilled VIRTUAL-REGISTER.
Allocates a new spill slot if one doesn't exist yet.
Offsets are negative, relative to the frame pointer (x29)."
  (let ((existing (gethash virtual-register *current-aarch64-spill-home*)))
    (if existing
        existing
        (progn
          (incf *current-aarch64-spill-base* 8)
          (let ((offset *current-aarch64-spill-base*))
            (setf (gethash virtual-register *current-aarch64-spill-home*) offset)
            offset)))))

(defun %aarch64-emit-spill-store (virtual-register scratch-reg stream)
  "Emit store of SCRATCH-REG into the spill home for VIRTUAL-REGISTER."
  (let ((offset (%aarch64-spill-home-offset virtual-register)))
    (format stream "  str ~A, [x29, #-~D]~%" scratch-reg offset)))

(defun %aarch64-emit-spill-load (virtual-register scratch-reg stream)
  "Emit load of VIRTUAL-REGISTER's spill home into SCRATCH-REG."
  (let ((offset (%aarch64-spill-home-offset virtual-register)))
    (format stream "  ldr ~A, [x29, #-~D]~%" scratch-reg offset)))

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
  (let* ((index (%aarch64-virtual-register-index virtual-register))
         (reg (%aarch64-register-from-pool index)))
    (if reg
        reg
        ;; Register beyond the pool — must be spilled.
        ;; The caller is responsible for loading/storing around use.
        (error "AArch64 register spilling needed for ~A (index ~D > pool size ~D).
This virtual register must be spilled to the stack frame before use.
Call %aarch64-emit-spill-load / %aarch64-emit-spill-store to materialize."
               virtual-register
               index
               (length *aarch64-register-pool)))))

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

(defmethod emit-instruction ((target aarch64-target) (inst vm-truncate) stream)
  (format stream "  sdiv ~A, ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-lhs inst))
          (target-register target (vm-rhs inst))))

(defun %aarch64-fp-register (target virtual-register)
  "Return the scalar double register name corresponding to VIRTUAL-REGISTER."
  (let ((name (target-register target virtual-register)))
    (format nil "d~A" (subseq name 1))))

(defmacro define-aarch64-float-emit-method (inst-type mnemonic)
  `(defmethod emit-instruction ((target aarch64-target) (inst ,inst-type) stream)
     (format stream "  ~A ~A, ~A, ~A~%"
             ,mnemonic
             (%aarch64-fp-register target (vm-dst inst))
             (%aarch64-fp-register target (vm-lhs inst))
             (%aarch64-fp-register target (vm-rhs inst)))))

(define-aarch64-float-emit-method vm-float-add "fadd")
(define-aarch64-float-emit-method vm-float-sub "fsub")
(define-aarch64-float-emit-method vm-float-mul "fmul")
(define-aarch64-float-emit-method vm-float-div "fdiv")

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
                                    (vm-method-reg inst)
                                    (vm-method-qualifier inst))))

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
