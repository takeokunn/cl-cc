(in-package :cl-cc)

(defclass target () ())

(defclass x86-64-target (target)
  ((regalloc :initarg :regalloc :initform nil :accessor target-regalloc)))

(defgeneric target-register (target virtual-register))
(defgeneric emit-instruction (target instruction stream))

;; Fallback: skip unsupported instructions silently (demo backends are partial).
(defmethod emit-instruction (target (inst vm-instruction) stream)
  (declare (ignore target stream)))

(defparameter *phys-reg-to-asm-string*
  '((:rax . "rax") (:rcx . "rcx") (:rdx . "rdx") (:rbx . "rbx")
    (:rsi . "rsi") (:rdi . "rdi") (:r8 . "r8") (:r9 . "r9")
    (:r10 . "r10") (:r11 . "r11") (:r12 . "r12") (:r13 . "r13")
    (:r14 . "r14") (:r15 . "r15"))
  "Mapping from physical register keywords to assembly strings.")

(defmethod target-register ((target x86-64-target) virtual-register)
  (let ((ra (target-regalloc target)))
    (if ra
        ;; Use register allocation result
        (let ((phys (gethash virtual-register (regalloc-assignment ra))))
          (unless phys
            (error "Virtual register ~A not allocated (possibly spilled)" virtual-register))
          (let ((entry (assoc phys *phys-reg-to-asm-string*)))
            (unless entry
              (error "Unknown physical register: ~A" phys))
            (cdr entry)))
        ;; Fallback: naive mapping for backward compatibility
        (let* ((index (or (parse-integer (subseq (symbol-name virtual-register) 1)
                                         :junk-allowed t)
                          0))
               (pool '("rax" "rbx" "rcx" "rdx" "r8" "r9" "r10" "r11")))
          (when (>= index (length pool))
            (error "Register spilling is not implemented yet (needed: ~A)" virtual-register))
          (nth index pool)))))

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
  (format stream "~A:~%" (vm-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-jump) stream)
  (format stream "  jmp ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-jump-zero) stream)
  (format stream "  cmp ~A, 0~%"
          (target-register target (vm-reg inst)))
  (format stream "  je ~A~%" (vm-label-name inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-print) stream)
  (declare (ignore stream))
  (error "print backend emission is not implemented yet (~A)" (vm-reg inst)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-halt) stream)
  (format stream "  mov rax, ~A~%"
          (target-register target (vm-reg inst)))
  (format stream "  ret~%"))

(defmethod emit-instruction ((target x86-64-target) (inst vm-spill-store) stream)
  (let* ((src-reg (vm-spill-src inst))
         (asm-str (cdr (assoc src-reg *phys-reg-to-asm-string*))))
    (format stream "  mov [rbp - ~A], ~A~%"
            (* (vm-spill-slot inst) 8)
            asm-str)))

(defmethod emit-instruction ((target x86-64-target) (inst vm-spill-load) stream)
  (let* ((dst-reg (vm-spill-dst inst))
         (asm-str (cdr (assoc dst-reg *phys-reg-to-asm-string*))))
    (format stream "  mov ~A, [rbp - ~A]~%"
            asm-str
            (* (vm-spill-slot inst) 8))))
