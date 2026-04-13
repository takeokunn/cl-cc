;;;; src/emit/x86-64-codegen.lisp — Two-pass label resolution, dispatch, and public API
;;;
;;; Instruction size table, label offset computation, jump-emitting dispatch,
;;; and the top-level emit-vm-program / compile-to-x86-64-bytes entry points.
;;; Load order: after x86-64-emit-ops.lisp

(in-package :cl-cc)

;;; Two-Pass Code Generation (Labels + Jumps)

(defparameter *x86-64-instruction-sizes*
  (let ((ht (make-hash-table :test #'eq)))
    ;; Constants and copies
    (setf (gethash 'vm-const ht) 10)       ; REX + opcode + 8-byte immediate
    (setf (gethash 'vm-move ht) 3)         ; REX + opcode + ModR/M
    ;; Arithmetic: mov + op
    (setf (gethash 'vm-add ht) 6)          ; mov + add (3+3)
    (setf (gethash 'vm-integer-add ht) 6)  ; mov + add
    (setf (gethash 'vm-sub ht) 6)          ; mov + sub (3+3)
    (setf (gethash 'vm-integer-sub ht) 6)  ; mov + sub
    (setf (gethash 'vm-mul ht) 7)          ; mov + imul (3+4, 0F AF)
    (setf (gethash 'vm-integer-mul ht) 7)  ; mov + imul
    ;; Control flow
    (setf (gethash 'vm-halt ht) 3)         ; mov result to RAX
    (setf (gethash 'vm-label ht) 0)        ; Labels emit no code
    (setf (gethash 'vm-jump ht) 5)         ; JMP rel32
    (setf (gethash 'vm-jump-zero ht) 9)    ; TEST + JE rel32 (3 + 6)
    (setf (gethash 'vm-ret ht) 1)          ; RET
    ;; No-ops in native codegen
    (setf (gethash 'vm-print ht) 0)
    (setf (gethash 'vm-closure ht) 0)
    (setf (gethash 'vm-call ht) 6)
    (setf (gethash 'vm-tail-call ht) 3)
    ;; Register spilling
    (setf (gethash 'vm-spill-store ht) 4)  ; MOV [rbp-disp8], reg
    (setf (gethash 'vm-spill-load ht) 4)   ; MOV reg, [rbp-disp8]
    ;; Comparison: CMP(3) + SETcc(3-4) + MOVZX(4) = 12 max
    (dolist (tp '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq))
      (setf (gethash tp ht) 12))
    ;; Logical NOT: TEST+SETE+MOVZX = 11→12; bitwise NOT: MOV+NOT = 7
    (setf (gethash 'vm-not ht) 12)
    (setf (gethash 'vm-lognot ht) 7)
    (setf (gethash 'vm-logcount ht) 5)
    (setf (gethash 'vm-integer-length ht) 16)
    (setf (gethash 'vm-bswap ht) 6)
    ;; Unary arithmetic: MOV(3) + op(3-4) = 7
    (setf (gethash 'vm-neg ht) 7)
    (setf (gethash 'vm-inc ht) 7)
    (setf (gethash 'vm-dec ht) 7)
    ;; Abs: MOV + CMP-imm32 + JGE-short + NEG = 15
    (setf (gethash 'vm-abs ht) 15)
    ;; Min/max: MOV + CMP + CMOV = 10
    (setf (gethash 'vm-min ht) 10)
    (setf (gethash 'vm-max ht) 10)
    (setf (gethash 'vm-select ht) 10)
    ;; Ash: fixed 24-byte sequence
    (setf (gethash 'vm-ash ht) 24)
    ;; Rotate: MOV + MOV + ROR + save/restore RCX = 11 bytes
    (setf (gethash 'vm-rotate ht) 11)
    ;; IDIV-based: truncate/rem = 21, floor-div = 34, floor-mod = 37
    (setf (gethash 'vm-truncate ht) 21)
    (setf (gethash 'vm-rem ht) 21)
    (setf (gethash 'vm-div ht) 34)
    (setf (gethash 'vm-mod ht) 37)
    ;; Boolean logical: XOR+TEST+JE+TEST+JE+ADD = 17
    (setf (gethash 'vm-and ht) 17)
    (setf (gethash 'vm-or ht) 17)
    ;; Binary logical: MOV + op = 6
    (dolist (tp '(vm-logand vm-logior vm-logxor))
      (setf (gethash tp ht) 6))
    ;; Scalar float ops: MOVSD + op = 8
    (dolist (tp '(vm-float-add vm-float-sub vm-float-mul vm-float-div))
      (setf (gethash tp ht) 8))
    ;; XNOR = 9, logtest = 14, logbitp = 15
    (setf (gethash 'vm-logeqv ht) 9)
    (setf (gethash 'vm-logtest ht) 14)
    (setf (gethash 'vm-logbitp ht) 15)
    ;; Type predicates: null-p = 11; others = 10 (MOV imm64)
    (setf (gethash 'vm-null-p ht) 11)
    (dolist (tp '(vm-number-p vm-integer-p vm-cons-p vm-symbol-p vm-function-p))
      (setf (gethash tp ht) 10))
    ht)
  "Maps VM instruction struct-type symbols to their x86-64 encoded byte sizes.
   Used by the first pass of two-pass code generation to build label offset tables.")

(defun instruction-size (inst)
  "Estimate the size in bytes of the x86-64 encoding for a VM instruction.
   Used in first pass to build label offset table."
  (cond
    ((typep inst 'vm-const)
     (if (floatp (vm-value inst)) 15 10))
    ((typep inst 'vm-move)
     (if (or (x86-64-float-vreg-p (vm-dst inst))
             (x86-64-float-vreg-p (vm-src inst)))
         (let ((dst (vm-reg-to-xmm (vm-dst inst)))
               (src (vm-reg-to-xmm (vm-src inst))))
           (if (= dst src) 0 4))
         (let ((dst (vm-reg-to-x86 (vm-dst inst)))
               (src (vm-reg-to-x86 (vm-src inst))))
           (if (= dst src) 0 3))))
    ((typep inst 'vm-halt)
     (if (x86-64-float-vreg-p (vm-reg inst))
         (let ((result-reg (vm-reg-to-xmm (vm-reg inst))))
           (if (= result-reg +xmm0+) 0 4))
         (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
           (if (= result-reg +rax+) 0 3))))
    (t
     (or (gethash (type-of inst) *x86-64-instruction-sizes*) 0))))

(defun build-label-offsets (instructions prologue-size)
  "Build a hash table mapping label names to byte offsets.
   First pass: walk instructions, accumulate sizes."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (instruction-size inst)))
    offsets))

;; Per-instruction emitters (emit-vm-halt-inst through emit-vm-spill-load-inst),
;; *x86-64-emitter-entries*, *x86-64-emitter-table*, and
;; emit-vm-instruction-with-labels are in x86-64-codegen-dispatch.lisp (loaded next).

(defun emit-vm-program (program stream)
  "Emit machine code for entire VM program.
   Uses two-pass approach: first pass builds label offset table,
   second pass emits code with resolved jump targets."
  (let* ((instructions (vm-program-instructions program))
         (cfg (cfg-build instructions))
         (leaf-p (vm-program-leaf-p program))
         (spill-count (regalloc-spill-count *current-regalloc*))
         (red-zone-spill-p (x86-64-red-zone-spill-p leaf-p spill-count))
         (callee-saved (x86-64-used-callee-saved-regs *current-regalloc*
                                                         *x86-64-calling-convention*))
         (save-regs (if (or (and leaf-p (zerop spill-count))
                            red-zone-spill-p)
                        callee-saved
                         (cons +rbp+ callee-saved)))
         (*current-spill-base-reg* (if red-zone-spill-p +rsp+ +rbp+))
         ;; Each push/pop is 1 byte in the current encoder.
         (prologue-size (length save-regs))
           (ordered-instructions (if (cfg-entry cfg)
                                     (progn
                                      (cfg-compute-dominators cfg)
                                      (cfg-compute-loop-depths cfg)
                                      (cfg-flatten-hot-cold cfg))
                                    instructions))
          ;; First pass: build label offset table
          (label-offsets (build-label-offsets ordered-instructions prologue-size)))

    ;; Prologue: save only the callee-saved registers actually used
    (dolist (reg save-regs)
      (emit-push-r64 reg stream))

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size))
      (dolist (inst ordered-instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    ;; Epilogue: restore callee-saved registers in reverse order
    (dolist (reg (reverse save-regs))
      (emit-pop-r64 reg stream))

    ;; Return
    (emit-ret stream)))

;;; Public API

(defun compile-to-x86-64-bytes (program)
  "Compile VM program to x86-64 machine code bytes.

   Returns: (simple-array (unsigned-byte 8) (*))"
  ;; Run register allocation before emitting machine code
  (let* ((instructions (vm-program-instructions program))
         (float-vregs (x86-64-compute-float-vregs instructions))
         (ra (allocate-registers instructions *x86-64-calling-convention* float-vregs))
         (allocated-program (make-vm-program
                               :instructions (regalloc-instructions ra)
                               :result-register (vm-program-result-register program)
                               :leaf-p (vm-program-leaf-p program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs))
      (with-output-to-vector (stream)
        (emit-vm-program allocated-program stream)))))
