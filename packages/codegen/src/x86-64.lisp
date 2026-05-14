(in-package :cl-cc/codegen)

(defclass target () ())

(defclass x86-64-target (target)
  ((regalloc :initarg :regalloc :initform nil :accessor target-regalloc)
   (spill-base-reg :initarg :spill-base-reg :initform :rbp :accessor target-spill-base-reg)))

(defgeneric target-register (target virtual-register))
(defgeneric emit-instruction (target instruction stream))

(defparameter *x86-64-abi-arg-regs*
  '("rdi" "rsi" "rdx" "rcx" "r8" "r9")
  "System V AMD64 argument registers used for runtime helper calls.")

(defun %x86-64-vm-register-p (x)
  (and (symbolp x) (member x '(:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7) :test #'eq)))

(defun %x86-64-emit-abi-arg (target stream abi-reg arg)
  (if (%x86-64-vm-register-p arg)
      (format stream "  mov ~A, ~A~%" abi-reg (target-register target arg))
      (format stream "  mov ~A, ~A~%" abi-reg arg)))

(defun %x86-64-emit-runtime-call (target stream callee args &optional result-reg)
  (let* ((reg-args (subseq args 0 (min (length args) (length *x86-64-abi-arg-regs*))))
         (extra-args (nthcdr (length *x86-64-abi-arg-regs*) args)))
    (loop for arg in reg-args
          for abi-reg in *x86-64-abi-arg-regs*
          do (%x86-64-emit-abi-arg target stream abi-reg arg))
    (loop for arg in (reverse extra-args)
          do (if (%x86-64-vm-register-p arg)
                 (format stream "  push ~A~%" (target-register target arg))
                 (format stream "  push ~A~%" arg)))
    (format stream "  call ~A~%" callee)
    (when result-reg
      (format stream "  mov ~A, rax~%" (target-register target result-reg)))
    (when extra-args
      (format stream "  add rsp, ~D~%" (* 8 (length extra-args))))))

(defmethod emit-instruction (target (inst vm-instruction) stream)
  (declare (ignore target stream))
  (error "Unsupported x86-64 instruction: ~A" (type-of inst)))

(defparameter *phys-reg-to-asm-string*
  '((:rax . "rax") (:rcx . "rcx") (:rdx . "rdx") (:rbx . "rbx")
    (:rbp . "rbp") (:rsi . "rsi") (:rdi . "rdi") (:r8 . "r8") (:r9 . "r9")
    (:r10 . "r10") (:r11 . "r11") (:r12 . "r12") (:r13 . "r13")
    (:r14 . "r14") (:r15 . "r15"))
  "Mapping from physical register keywords to assembly strings.")

(defparameter *fallback-register-pool*
  '((:r0 . "rax") (:r1 . "rbx") (:r2 . "rcx") (:r3 . "rdx")
    (:r4 . "r8")  (:r5 . "r9")  (:r6 . "r10") (:r7 . "r11"))
  "Naive virtual→physical mapping used when no register allocator is present (R0..R7 only).")

(defmethod target-register ((target x86-64-target) virtual-register)
  (let ((ra (target-regalloc target)))
    (if ra
        (let ((phys (gethash virtual-register (regalloc-assignment ra))))
          (unless phys
            (error "Virtual register ~A not allocated (possibly spilled)" virtual-register))
          (let ((entry (assoc phys *phys-reg-to-asm-string*)))
            (unless entry
              (error "Unknown physical register: ~A" phys))
            (cdr entry)))
        (let ((entry (assoc virtual-register *fallback-register-pool*)))
          (unless entry
            (error "x86-64 emission requires register allocation before target-register (~A)" virtual-register))
          (cdr entry)))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-const) stream)
  (format stream "  mov ~A, ~A~%"
          (target-register target (vm-dst inst))
          (vm-value inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-move) stream)
  (format stream "  mov ~A, ~A~%"
          (target-register target (vm-dst inst))
          (target-register target (vm-src inst))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-add) stream)
  (let ((dst (target-register target (vm-dst inst)))
        (lhs (target-register target (vm-lhs inst)))
        (rhs (target-register target (vm-rhs inst))))
    (format stream "  mov ~A, ~A~%" dst lhs)
    (format stream "  add ~A, ~A~%" dst rhs)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-sub) stream)
  (let ((dst (target-register target (vm-dst inst)))
        (lhs (target-register target (vm-lhs inst)))
        (rhs (target-register target (vm-rhs inst))))
    (format stream "  mov ~A, ~A~%" dst lhs)
    (format stream "  sub ~A, ~A~%" dst rhs)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-mul) stream)
  (let ((dst (target-register target (vm-dst inst)))
        (lhs (target-register target (vm-lhs inst)))
        (rhs (target-register target (vm-rhs inst))))
    (format stream "  mov ~A, ~A~%" dst lhs)
    (format stream "  imul ~A, ~A~%" dst rhs)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-label) stream)
  (declare (ignore target))
  (format stream "  .align 4~%")
  (format stream "~A:~%" (vm-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-jump) stream)
  (format stream "  jmp ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-jump-zero) stream)
  (format stream "  cmp ~A, 0~%"
          (target-register target (vm-reg inst)))
  (format stream "  je ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-print) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-print"
                             (list (vm-reg inst))
                             nil))

(defmethod emit-instruction ((target x86-64-target) (inst vm-halt) stream)
  (format stream "  mov rax, ~A~%"
          (target-register target (vm-reg inst)))
  (format stream "  ret~%"))

(defmethod emit-instruction ((target x86-64-target) (inst vm-ret) stream)
  (format stream "  mov rax, ~A~%"
          (target-register target (vm-reg inst)))
  (format stream "  ret~%"))

(defmethod emit-instruction ((target x86-64-target) (inst vm-call) stream)
  (format stream "  call ~A~%"
          (target-register target (vm-func-reg inst)))
  (format stream "  mov ~A, rax~%"
          (target-register target (vm-dst inst))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-tail-call) stream)
  (format stream "  jmp ~A~%"
          (target-register target (vm-func-reg inst))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-class-def) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-defclass"
                             (list (vm-class-name-sym inst)
                                   (vm-superclasses inst)
                                   (vm-slot-names inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-make-obj) stream)
  (%x86-64-emit-runtime-call target stream
                             (if (null (vm-initarg-regs inst))
                                 "rt-make-instance-0"
                                 "rt-make-instance")
                             (append (list (vm-class-reg inst))
                                     (loop for (key . reg) in (vm-initarg-regs inst)
                                           append (list key reg)))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-slot-read) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-slot-value"
                             (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-slot-write) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-slot-set"
                             (list (vm-obj-reg inst)
                                   (vm-slot-name-sym inst)
                                   (vm-value-reg inst))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-register-method) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-register-method"
                             (list (vm-gf-reg inst)
                                   (vm-method-specializer inst)
                                   (vm-method-reg inst))))

(defmethod emit-instruction ((target x86-64-target) (inst vm-generic-call) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-call-generic"
                             (cons (vm-gf-reg inst) (vm-args inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-slot-boundp) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-slot-boundp"
                             (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-slot-makunbound) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-slot-makunbound"
                             (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-slot-exists-p) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-slot-exists-p"
                             (list (vm-obj-reg inst) (vm-slot-name-sym inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-class-name-fn) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-class-name"
                             (list (vm-src inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-class-of-fn) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-class-of"
                             (list (vm-src inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-find-class) stream)
  (%x86-64-emit-runtime-call target stream
                             "rt-find-class"
                             (list (vm-src inst))
                             (vm-dst inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-spill-store) stream)
  (let* ((src-reg (vm-spill-src inst))
         (asm-str (cdr (assoc src-reg *phys-reg-to-asm-string*)))
         (base (string-downcase (symbol-name (target-spill-base-reg target)))))
    (format stream "  mov [~A - ~A], ~A~%"
            base
            (* (vm-spill-slot inst) 8)
            asm-str)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-spill-load) stream)
  (let* ((dst-reg (vm-spill-dst inst))
         (asm-str (cdr (assoc dst-reg *phys-reg-to-asm-string*)))
         (base (string-downcase (symbol-name (target-spill-base-reg target)))))
    (format stream "  mov ~A, [~A - ~A]~%"
            asm-str
            base
            (* (vm-spill-slot inst) 8))))
