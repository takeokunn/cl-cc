;;;; packages/backend/emit/src/aarch64-emitters.lisp - AArch64 VM Instruction Emitters
;;;;
;;;; Per-VM-instruction emit-a64-vm-* functions that translate individual
;;;; VM IR instructions into AArch64 machine code via the encode-* layer.
;;;; Includes: define-a64-binary-emitter / define-a64-csel-emitter macros,
;;;; emit-a64-vm-{const,move,select,bswap,rotate,halt,jump,jump-zero,
;;;; spill-store,spill-load,ret} and all arithmetic/comparison/logic emitters.
;;;;
;;;; Register mapping, instruction encoders, and the label-offset builder are
;;;; in aarch64-codegen.lisp (loads before this file).
;;;;
;;;; Load order: after aarch64-codegen.lisp, before aarch64-program.lisp.

(in-package :cl-cc/emit)

(defmacro define-a64-binary-emitter (fn-name encode-fn)
  "Define an AArch64 binary VM instruction emitter using ENCODE-FN(rd rn rm)."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
       (emit-a64-instr (,encode-fn rd rn rm) stream))))

(defmacro define-a64-csel-emitter (fn-name description cond-code)
  "Define an AArch64 CSEL-based min/max emitter with COND-CODE."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
       (emit-a64-instr (encode-cmp rn rm) stream)
       (emit-a64-instr (encode-csel rd rn rm ,cond-code) stream))))

(defun emit-a64-vm-const (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (value (vm-value inst)))
    ;; Handle negative values via logical masking to 64-bit
    (emit-a64-mov-imm64 rd (logand value #xFFFFFFFFFFFFFFFF) stream)))

(defun emit-a64-vm-move (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (unless (= rd rn)
      (emit-a64-instr (encode-mov-rr rd rn) stream))))

(define-a64-binary-emitter emit-a64-vm-add encode-add)
(define-a64-binary-emitter emit-a64-vm-sub encode-sub)
(define-a64-binary-emitter emit-a64-vm-mul encode-mul)
(define-a64-csel-emitter emit-a64-vm-min "vm-min: dst = min(lhs, rhs) — signed, branchless via CSEL LT." 11)
(define-a64-csel-emitter emit-a64-vm-max "vm-max: dst = max(lhs, rhs) — signed, branchless via CSEL GT." 12)

(defun emit-a64-vm-select (inst stream)
  "vm-select: dst = cond ? then : else  (branchless via CSEL NE)."
  (let ((rd (a64-reg (vm-dst inst)))
        (cond (a64-reg (vm-select-cond-reg inst)))
        (then (a64-reg (vm-select-then-reg inst)))
        (else (a64-reg (vm-select-else-reg inst))))
    (emit-a64-instr (encode-mov-rr rd else) stream)
    (emit-a64-instr (encode-cmp cond 31) stream)
    (emit-a64-instr (encode-csel rd then rd 1) stream)))

(defun emit-a64-vm-bswap (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (emit-a64-instr (encode-rev32 rd rn) stream)))

(defun emit-a64-vm-rotate (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-mov-rr rd rn) stream)
    (emit-a64-instr (encode-rorv rd rd rm) stream)))

(defun emit-a64-vm-halt (inst stream)
  "Move result to X0 (return register). Always emits exactly one MOV instruction."
  (let ((result-reg (a64-reg (vm-reg inst))))
    (emit-a64-instr (encode-mov-rr 0 result-reg) stream)))

(defun emit-a64-vm-jump (inst stream current-pos label-offsets)
  "Emit B #imm26 for unconditional jump."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; offset in instruction units: (target_byte_offset - current_byte_offset) / 4
         (byte-offset (- target-pos current-pos))
         (imm26 (ash byte-offset -2)))
    (emit-a64-instr (encode-b imm26) stream)))

(defun emit-a64-vm-jump-zero (inst stream current-pos label-offsets)
  "Emit CBZ Xn, #imm19 for conditional branch on zero."
  (let* ((rn (a64-reg (vm-reg inst)))
         (target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         (byte-offset (- target-pos current-pos))
         (imm19 (ash byte-offset -2)))
    (emit-a64-instr (encode-cbz rn imm19) stream)))

(defun emit-a64-vm-spill-store (inst stream)
  "STUR Xs, [X29, #-slot*8] (store to spill slot via frame pointer)."
  (let* ((src-phys (vm-spill-src inst))
         (src-num (cdr (assoc src-phys *aarch64-reg-number*)))
         (offset (- (* (vm-spill-slot inst) 8))))  ; negative offset from X29
    (emit-a64-instr (encode-stur src-num 29 offset) stream)))

(defun emit-a64-vm-spill-load (inst stream)
  "LDUR Xd, [X29, #-slot*8] (load from spill slot via frame pointer)."
  (let* ((dst-phys (vm-spill-dst inst))
         (dst-num (cdr (assoc dst-phys *aarch64-reg-number*)))
         (offset (- (* (vm-spill-slot inst) 8))))
    (emit-a64-instr (encode-ldur dst-num 29 offset) stream)))

(defun emit-a64-vm-ret (inst stream)
  "Emit inline epilogue (restore all callee-saved registers) then RET.
   Must stay in sync with emit-a64-prologue and a64-instruction-size for vm-ret."
  (declare (ignore inst))
  (dolist (pair (reverse (a64-used-callee-saved-pairs *current-a64-regalloc*)))
    (destructuring-bind (rn rm) pair
      (emit-a64-instr (encode-ldp-post rn rm +a64-sp+ 2) stream)))
  (emit-a64-instr +a64-ret+ stream))

