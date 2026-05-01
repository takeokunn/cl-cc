;;;; packages/emit/src/x86-64-regs.lisp — Register maps and VM→x86-64 register translation
;;;
;;; Contains virtual-register state variables, VM→physical register maps,
;;; and the translation functions used by all x86-64 emitters.

(in-package :cl-cc/codegen)

(defvar *current-regalloc* nil
  "When non-nil, the current regalloc-result used during code generation.")

(defparameter *current-spill-base-reg* +rbp+
  "Base register used for spill load/store emission in the current function.")

(defvar *current-float-vregs* nil
  "When non-nil, hash table of virtual registers currently known to hold unboxed floats.")

(defun x86-64-red-zone-spill-p (leaf-p spill-count)
  "Return true when a leaf function can keep spill slots in the SysV red zone."
  (and leaf-p
       (plusp spill-count)
       (<= spill-count 16)))

(defparameter *vm-reg-map*
  `((:R0 . ,+rax+)
    (:R1 . ,+rcx+)
    (:R2 . ,+rdx+)
    (:R3 . ,+rbx+)
    (:R4 . ,+rsi+)
    (:R5 . ,+rdi+)
    (:R6 . ,+r8+)
    (:R7 . ,+r9+))
  "Mapping from VM keyword registers to x86-64 register codes.")

(defparameter *vm-fp-reg-map*
  `((:R0 . ,+xmm0+)
    (:R1 . ,+xmm1+)
    (:R2 . ,+xmm2+)
    (:R3 . ,+xmm3+)
    (:R4 . ,+xmm4+)
    (:R5 . ,+xmm5+)
    (:R6 . ,+xmm6+)
    (:R7 . ,+xmm7+))
  "Naive mapping from VM keyword registers to XMM register codes.")

(declaim (special *phys-reg-to-x86-code* *phys-fp-reg-to-x86-code*))

(defun vm-reg-to-x86 (vm-reg)
  "Map VM register to x86-64 register code.
   When *current-regalloc* is set, uses register allocation results.
   Otherwise falls back to naive mapping."
  (let ((phys-entry (assoc vm-reg *phys-reg-to-x86-code*)))
    (if phys-entry
        (cdr phys-entry)
        (if *current-regalloc*
            (vm-reg-to-x86-with-alloc *current-regalloc* vm-reg)
            (let ((entry (assoc vm-reg *vm-reg-map*)))
              (unless entry
                (error "VM register ~A has no x86-64 mapping (only R0-R7 supported)" vm-reg))
              (cdr entry))))))

(defparameter *phys-fp-reg-to-x86-code*
  '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
    (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7)
    (:xmm8 . 8) (:xmm9 . 9) (:xmm10 . 10) (:xmm11 . 11)
    (:xmm12 . 12) (:xmm13 . 13) (:xmm14 . 14) (:xmm15 . 15))
  "Mapping from physical FP register keywords to XMM register codes.")

(defun vm-reg-to-xmm (vm-reg)
  "Map VM register to XMM register code for native float operations.
Falls back to the naive R0..R7 -> XMM0..XMM7 mapping when regalloc has not
assigned dedicated FP registers yet."
  (let ((phys-entry (assoc vm-reg *phys-fp-reg-to-x86-code*)))
    (if phys-entry
        (cdr phys-entry)
        (if *current-regalloc*
            (let* ((phys (gethash vm-reg (regalloc-assignment *current-regalloc*)))
                   (entry (and phys (assoc phys *phys-fp-reg-to-x86-code*))))
              (if entry
                  (cdr entry)
                  (let ((fallback (assoc vm-reg *vm-fp-reg-map*)))
                    (unless fallback
                      (error "VM register ~A has no XMM mapping" vm-reg))
                    (cdr fallback))))
            (let ((entry (assoc vm-reg *vm-fp-reg-map*)))
              (unless entry
                (error "VM register ~A has no XMM mapping" vm-reg))
              (cdr entry))))))

(defun x86-64-float-vreg-p (vreg)
  (and *current-float-vregs* (gethash vreg *current-float-vregs*)))

(defun x86-64-compute-float-vregs (instructions)
  "Conservatively mark VM virtual registers that hold unboxed floats."
  (let ((float-vregs (make-hash-table :test #'eq)))
    (flet ((mark (reg)
             (when reg
               (setf (gethash reg float-vregs) t))))
      (dolist (inst instructions)
        (typecase inst
          (vm-const
           (when (floatp (vm-value inst))
             (mark (vm-dst inst))))
          ((or vm-float-add vm-float-sub vm-float-mul vm-float-div)
           (mark (vm-dst inst))
           (mark (vm-lhs inst))
           (mark (vm-rhs inst)))))
      (loop with changed = t
            while changed
            do (setf changed nil)
               (dolist (inst instructions)
                 (when (typep inst 'vm-move)
                   (let ((dst (vm-dst inst))
                         (src (vm-src inst)))
                     (cond
                       ((and (gethash src float-vregs)
                             (not (gethash dst float-vregs)))
                        (setf (gethash dst float-vregs) t)
                        (setf changed t))
                       ((and (gethash dst float-vregs)
                             (not (gethash src float-vregs)))
                        (setf (gethash src float-vregs) t)
                        (setf changed t))))))))
    float-vregs))

(defparameter *phys-reg-to-x86-code*
  `((:rax . ,+rax+) (:rcx . ,+rcx+) (:rdx . ,+rdx+) (:rbx . ,+rbx+)
    (:rsi . ,+rsi+) (:rdi . ,+rdi+) (:r8 . ,+r8+) (:r9 . ,+r9+)
    (:r10 . ,+r10+) (:r11 . ,+r11+) (:r12 . ,+r12+) (:r13 . ,+r13+)
    (:r14 . ,+r14+) (:r15 . ,+r15+))
  "Mapping from physical register keywords to x86-64 register codes.")

(defun vm-reg-to-x86-with-alloc (ra vm-reg)
  "Map VM register to x86-64 register code using register allocation result.
   RA is a regalloc-result, VM-REG is a virtual register keyword.
   Returns the integer register code (e.g., +rax+ = 0)."
  (let ((phys (gethash vm-reg (regalloc-assignment ra))))
    (unless phys
      (error "Virtual register ~A not allocated (possibly spilled)" vm-reg))
    (let ((entry (assoc phys *phys-reg-to-x86-code*)))
      (unless entry
        (error "Unknown physical register: ~A" phys))
      (cdr entry))))

(defun vm-const-to-integer (value)
  "Coerce a VM constant value to an integer for native code emission.
   In integer-only binary mode: nil->0, t->1, integers pass through, other->0."
  (cond ((null value) 0)
        ((eq value t) 1)
        ((integerp value) value)
        (t 0)))

(defun x86-64-double-float-bits (value)
  "Return the IEEE754 bit pattern for VALUE as an unsigned 64-bit integer."
  (logand #xFFFFFFFFFFFFFFFF
          (sb-kernel:double-float-bits (float value 1.0d0))))
