;;;; packages/backend/emit/src/x86-64-codegen.lisp — Two-pass label resolution, dispatch, and public API
;;;
;;; Instruction size table, label offset computation, jump-emitting dispatch,
;;; and the top-level emit-vm-program / compile-to-x86-64-bytes entry points.
;;; Load order: after x86-64-emit-ops.lisp

(in-package :cl-cc/emit)

;;; Two-Pass Code Generation (Labels + Jumps)

;;; ============================================================
;;; x86-64 Instruction Size Table — Data / Logic Separation
;;; ============================================================
;;;
;;; Encoding DATA is declared in *X86-64-INSTRUCTION-SIZE-SPECS*.
;;; The LOGIC that populates the hash table is in POPULATE-SIZE-TABLE.
;;; Each spec entry: (type-spec size)
;;;   type-spec = symbol   → single mapping
;;;   type-spec = (s1 s2…) → group mapping (all get same size)

(defparameter *x86-64-instruction-size-specs*
  '(
    ;; Constants and copies
    (vm-const                          10)  ; REX + opcode + 8-byte immediate
    (vm-move                            3)  ; REX + opcode + ModR/M
    ;; Arithmetic: mov + op
    ((vm-add vm-integer-add)            6)  ; mov + add (3+3)
    ((vm-sub vm-integer-sub)            6)  ; mov + sub
    ((vm-mul vm-integer-mul)            7)  ; mov + imul (3+4, 0F AF)
    ;; Control flow
    (vm-halt                            3)  ; mov result to RAX
    (vm-label                           0)  ; Labels emit no code
    (vm-jump                            5)  ; JMP rel32
    (vm-jump-zero                       9)  ; TEST + JE rel32 (3 + 6)
    (vm-ret                             1)  ; RET
    ;; No-ops in native codegen
    ((vm-print vm-closure)              0)
    (vm-call                            6)
    (vm-tail-call                       3)
    ;; Register spilling
    (vm-spill-store                     4)  ; MOV [rbp-disp8], reg
    (vm-spill-load                      4)  ; MOV reg, [rbp-disp8]
    ;; Comparison: CMP(3) + SETcc(3-4) + MOVZX(4) = 12 max
    ((vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq) 12)
    ;; Logical NOT / bitwise NOT
    (vm-not                            12)  ; TEST+SETE+MOVZX
    (vm-lognot                          7)  ; MOV+NOT
    (vm-logcount                        5)
    (vm-integer-length                 16)
    (vm-bswap                           6)
    ;; Unary arithmetic: MOV(3) + op(3-4) = 7
    ((vm-neg vm-inc vm-dec)             7)
    ;; Abs: MOV + CMP-imm32 + JGE-short + NEG = 15
    (vm-abs                            15)
    ;; Min/max/select: MOV + CMP + CMOV = 10
    ((vm-min vm-max vm-select)         10)
    ;; Ash: fixed 24-byte sequence
    (vm-ash                            24)
    ;; Rotate: MOV + MOV + ROR + save/restore RCX = 11 bytes
    (vm-rotate                         11)
    ;; IDIV-based: truncate/rem = 21, floor-div = 34, floor-mod = 37
    ((vm-truncate vm-rem)              21)
    (vm-div                            34)
    (vm-mod                            37)
    ;; Boolean logical: XOR+TEST+JE+TEST+JE+ADD = 17
    ((vm-and vm-or)                    17)
    ;; Binary logical: MOV + op = 6
    ((vm-logand vm-logior vm-logxor)    6)
    ;; Scalar float ops: MOVSD + op = 8
    ((vm-float-add vm-float-sub vm-float-mul vm-float-div) 8)
    ;; Misc bitwise
    (vm-logeqv                          9)
    (vm-logtest                        14)
    (vm-logbitp                        15)
    ;; Type predicates: null-p = 11; others = 10 (MOV imm64)
    (vm-null-p                         11)
    ((vm-number-p vm-integer-p vm-cons-p vm-symbol-p vm-function-p) 10))
  "Declarative spec: VM instruction types → x86-64 encoded byte sizes.
   Each entry is (type-spec size) where type-spec is a symbol or list of symbols.")

(defun populate-size-table (specs)
  "Build an eq-hash-table from declarative size specifications."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry specs ht)
      (destructuring-bind (types size) entry
        (if (consp types)
            (dolist (tp types) (setf (gethash tp ht) size))
            (setf (gethash types ht) size))))))

(defparameter *x86-64-instruction-sizes*
  (populate-size-table *x86-64-instruction-size-specs*)
  "Maps VM instruction struct-type symbols to their x86-64 encoded byte sizes.
   Used by the first pass of two-pass code generation to build label offset tables.")

(defun instruction-size (inst)
  "Estimate the size in bytes of the x86-64 encoding for a VM instruction.
   Used in first pass to build label offset table."
  (typecase inst
    (vm-const
     (if (floatp (vm-value inst)) 15 10))
    (vm-move
     (if (or (x86-64-float-vreg-p (vm-dst inst))
             (x86-64-float-vreg-p (vm-src inst)))
         (let ((dst (vm-reg-to-xmm (vm-dst inst)))
               (src (vm-reg-to-xmm (vm-src inst))))
           (if (= dst src) 0 4))
         (let ((dst (vm-reg-to-x86 (vm-dst inst)))
               (src (vm-reg-to-x86 (vm-src inst))))
           (if (= dst src) 0 3))))
    (vm-halt
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
