;;;; packages/emit/src/x86-64-codegen.lisp — Two-pass label resolution, dispatch, and public API
;;;
;;; Instruction size table, label offset computation, jump-emitting dispatch,
;;; and the top-level emit-vm-program / compile-to-x86-64-bytes entry points.
;;; Load order: after x86-64-emit-ops.lisp

(in-package :cl-cc/codegen)

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
    ;; Checked arithmetic (FR-303): MOV + ALU + JO(rel32) + UD2
    ((vm-add-checked vm-sub-checked)   14)  ; mov + add/sub + jo + ud2 (3+3+6+2)
    (vm-mul-checked                    15)  ; mov + imul + jo + ud2 (3+4+6+2)
    ((vm-integer-mul-high-u vm-integer-mul-high-s) 19) ; mul-high sequence + mov
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
    ((vm-float-add vm-float-sub vm-float-mul vm-float-div vm-sqrt) 8)
    ;; Libm-call transcendental ops (FR-286): MOVSD + MOV+addr + CALL + MOVSD = 21
    ((vm-sin-inst vm-cos-inst vm-exp-inst vm-log-inst
      vm-tan-inst vm-asin-inst vm-acos-inst vm-atan-inst) 21)
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
    ((or vm-spill-store vm-spill-load)
     (let* ((offset (x86-64-spill-slot-offset (vm-spill-slot inst)))
            (mod (cond
                   ((and (zerop offset)
                         (/= (logand *current-spill-base-reg* #x7) 5))
                    0)
                   ((typep offset '(signed-byte 8))
                    1)
                   (t
                    2)))
            (sib-p (= (logand *current-spill-base-reg* #x7) 4))
            (addr-size (+ 1
                          (if sib-p 1 0)
                          (ecase mod
                            (0 0)
                            (1 1)
                            (2 4)))))
       (+ 2 addr-size)))
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

(defconstant +stack-probe-page-size+ 4096
  "Page stride used by native backend stack probing.")

(defconstant +x86-64-stack-probe-size+ 9
  "Byte size of one x86-64 OR [RSP-disp32], imm8 stack probe.")

(defun stack-probe-count (frame-size)
  "Return how many guard-page probes are needed for FRAME-SIZE bytes."
  (if (>= frame-size +stack-probe-page-size+)
      (floor frame-size +stack-probe-page-size+)
      0))

(defun x86-64-stack-frame-size (save-regs spill-frame-size)
  "Return conservative stack-frame bytes represented by saves plus allocated spill space."
  (+ (* 8 (length save-regs))
     spill-frame-size))

(defun emit-x86-64-stack-probes (stream probe-count)
  "Emit one non-mutating page touch per PROBE-COUNT below RSP."
  (loop for page from 1 to probe-count
        do (emit-or-mem-rsp-disp32-imm8 (- (* page +stack-probe-page-size+)) 0 stream)))

(defun x86-64-tls-base-register ()
  "Return the selected x86-64 TLS base register from optimizer planning."
  (let ((plan (opt-build-tls-plan :target :x86-64 :hot-access-p t)))
    (opt-tls-plan-base-register plan)))

(defun x86-64-atomic-lowering-plan (operation memory-order)
  "Return x86-64 atomic lowering metadata for OPERATION and MEMORY-ORDER.

Result plist keys:
- :opcode          selected representative opcode keyword
- :pre-fence       list of fence opcodes before atomic op
- :post-fence      list of fence opcodes after atomic op"
  (let* ((plan (opt-build-atomic-plan
                :target :x86-64
                :operation operation
                :memory-order memory-order))
         (pre-fence (case memory-order
                      (:seq-cst '(:mfence))
                      (otherwise nil)))
         (post-fence (case memory-order
                       (:seq-cst '(:mfence))
                       (otherwise nil))))
    (list :opcode (opt-atomic-plan-opcode plan)
          :pre-fence pre-fence
          :post-fence post-fence)))

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
          (frame-pointer-p (and (not *x86-64-omit-frame-pointer*)
                                (not red-zone-spill-p)
                                (or (not leaf-p)
                                    (plusp spill-count))))
          (callee-saved (x86-64-used-callee-saved-regs *current-regalloc*
                                                        (x86-64-codegen-target)))
          (save-regs (if frame-pointer-p
                         (cons +rbp+ callee-saved)
                         callee-saved))
          (spill-frame-size (if (and (not frame-pointer-p)
                                     (not red-zone-spill-p)
                                     (plusp spill-count))
                                (* 8 spill-count)
                                0))
          (*current-spill-base-reg* (if frame-pointer-p +rbp+ +rsp+))
          (*current-spill-offset-bias* (if frame-pointer-p 0 spill-frame-size))
          (probe-count (stack-probe-count
                         (x86-64-stack-frame-size save-regs spill-frame-size)))
          (probe-size (* probe-count +x86-64-stack-probe-size+))
          (spill-frame-adjust-size (if (plusp spill-frame-size) 7 0))
          ;; Each push/pop is 1 byte in the current encoder.
          (prologue-size (+ probe-size
                            (length save-regs)
                            spill-frame-adjust-size))
             (ordered-instructions (if (cfg-entry cfg)
                                       (progn
                                        (cfg-compute-dominators cfg)
                                       (cfg-compute-loop-depths cfg)
                                      (cfg-flatten-hot-cold cfg))
                                    instructions))
           ;; First pass: build label offset table
           (label-offsets (build-label-offsets ordered-instructions prologue-size)))

    ;; Stack probing: touch each guard page before the large frame is used.
    (emit-x86-64-stack-probes stream probe-count)

    ;; Prologue: save only the callee-saved registers actually used
    (dolist (reg save-regs)
      (emit-push-r64 reg stream))

    (when (plusp spill-frame-size)
      (emit-sub-ri32 +rsp+ spill-frame-size stream))

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size))
      (dolist (inst ordered-instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    (when (plusp spill-frame-size)
      (emit-add-ri32 +rsp+ spill-frame-size stream))

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
         (target (x86-64-codegen-target))
         (ra (allocate-registers instructions target float-vregs))
         (allocated-program (make-vm-program
                               :instructions (regalloc-instructions ra)
                               :result-register (vm-program-result-register program)
                               :leaf-p (vm-program-leaf-p program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs))
      (with-output-to-vector (stream)
        (emit-vm-program allocated-program stream)))))
