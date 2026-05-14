;;;; packages/emit/src/aarch64-codegen.lisp - AArch64 Machine Code Generation
;;;
;;; Generates native AArch64 machine code bytes from VM instructions.
;;; All AArch64 instructions are 32 bits (4 bytes) fixed-width.

(in-package :cl-cc/codegen)

;;; Physical register number mapping

(defparameter *aarch64-reg-number*
  '((:x0 . 0) (:x1 . 1) (:x2 . 2) (:x3 . 3)
    (:x4 . 4) (:x5 . 5) (:x6 . 6) (:x7 . 7)
    (:x8 . 8) (:x9 . 9) (:x10 . 10) (:x11 . 11)
    (:x12 . 12) (:x13 . 13) (:x14 . 14) (:x15 . 15)
    (:x16 . 16) (:x17 . 17) (:x19 . 19) (:x20 . 20)
    (:x21 . 21) (:x22 . 22) (:x23 . 23) (:x24 . 24)
    (:x25 . 25) (:x26 . 26) (:x27 . 27) (:x28 . 28)
    (:x29 . 29) (:x30 . 30)))

;;; AArch64 special register numbers (defined early so all functions below can reference them)
(defconstant +a64-sp+ 31 "Stack pointer register number.")
(defconstant +a64-zr+ 31 "Zero register number when used in non-SP operand positions.")
(defconstant +a64-fp+ 29 "Frame pointer / X29.")
(defconstant +a64-lr+ 30 "Link register / X30.")
(defconstant +a64-scs+ 18 "Shadow call stack pointer / X18.")
(defconstant +a64-scs-tmp+ 17 "Scratch register used for shadow call stack checks / X17.")
(defconstant +a64-stack-probe-scratch+ 16 "Scratch register used for stack probing / X16.")

;;; Dynamic variable holding current regalloc during code generation

(defparameter *current-a64-regalloc* nil
  "Current regalloc-result during AArch64 code generation.")

(defparameter *a64-omit-frame-pointer* t
  "When true, prefer SP-relative spill slots over reserving X29 as a frame pointer.")

(defun a64-codegen-target ()
  "Return the AArch64 target descriptor for the active frame-pointer policy."
  (if *a64-omit-frame-pointer*
      (make-target-desc
       :name (target-name *aarch64-target*)
       :word-size (target-word-size *aarch64-target*)
       :endianness (target-endianness *aarch64-target*)
       :gpr-count (target-gpr-count *aarch64-target*)
       :gpr-names (target-gpr-names *aarch64-target*)
       :arg-regs (target-arg-regs *aarch64-target*)
       :ret-reg (target-ret-reg *aarch64-target*)
       :fp-arg-regs (target-fp-arg-regs *aarch64-target*)
       :fp-ret-reg (target-fp-ret-reg *aarch64-target*)
       :callee-saved '(:x19 :x20 :x21 :x22 :x23 :x24 :x25 :x26 :x27 :x28 :x29)
       :scratch-regs (remove :x29 (target-scratch-regs *aarch64-target*) :test #'eq)
       :stack-alignment (target-stack-alignment *aarch64-target*)
       :legal-ops (target-legal-ops *aarch64-target*)
       :features (target-features *aarch64-target*))
      *aarch64-target*))

(defparameter *current-a64-spill-base-reg* +a64-fp+
  "Base register used for AArch64 spill load/store emission in the current function.")

(defparameter *current-a64-spill-offset-bias* 0
  "Additional byte bias applied before translating AArch64 spill slots to stack offsets.")

(defun a64-spill-slot-offset (slot)
  "Return the byte offset for spill SLOT relative to *CURRENT-A64-SPILL-BASE-REG*."
  (- *current-a64-spill-offset-bias* (* slot 8)))

;;; Byte emission helper

(defun emit-a64-instr (word32 stream)
  "Emit a 32-bit AArch64 instruction as 4 bytes little-endian."
  (funcall stream (logand word32 #xFF))
  (funcall stream (logand (ash word32 -8) #xFF))
  (funcall stream (logand (ash word32 -16) #xFF))
  (funcall stream (logand (ash word32 -24) #xFF)))

;;; Register lookup

(defun a64-reg (vm-reg)
  "Map VM virtual register keyword to AArch64 physical register number.
   Uses *current-a64-regalloc* when set, otherwise errors."
  (if *current-a64-regalloc*
      (let ((phys (gethash vm-reg (regalloc-assignment *current-a64-regalloc*))))
        (unless phys
          (error "Virtual register ~A not allocated" vm-reg))
        (let ((entry (assoc phys *aarch64-reg-number*)))
          (unless entry
            (error "Unknown physical register: ~A" phys))
          (cdr entry)))
      (let* ((index (or (parse-integer (subseq (symbol-name vm-reg) 1)
                                       :junk-allowed t)
                        0)))
        index)))

;;; ============================================================
;;; AArch64 Instruction Encoding — Data / Logic Separation
;;; ============================================================
;;;
;;; The DEFENC macro separates encoding DATA (base opcode, field layout)
;;; from encoding LOGIC (logior + ash + logand packing).
;;;
;;; Each call specifies:
;;;   (mnemonic base-constant lambda-list docstring (param mask shift)...)
;;;
;;; The macro generates ENCODE-<MNEMONIC> functions that pack bit fields
;;; into 32-bit instruction words.

(defmacro defenc (mnemonic base lambda-list docstring &rest fields)
  "Define an AArch64 instruction encoder from a data specification.
   MNEMONIC: symbol — generates ENCODE-<MNEMONIC>.
   BASE:     base opcode constant (fixed bits).
   FIELDS:   ((param mask shift)...) — each field is OR'd into BASE."
  (let ((fname (intern (format nil "ENCODE-~A" (symbol-name mnemonic)))))
    `(defun ,fname ,lambda-list
       ,docstring
       (logior ,base
               ,@(loop for (param mask shift) in fields
                       collect `(ash (logand ,param ,mask) ,shift))))))

;;; --- Move Instructions ---

;; MOVZ Xd, #imm16, LSL #(hw*16)
;; Encoding: 1_10_100101_hw_imm16_Rd
(defenc movz #xD2800000 (rd imm16 &optional (hw 0))
  "MOVZ Xd, #imm16, LSL #(hw*16). hw=0..3 for shifts 0,16,32,48."
  (rd #x1F 0) (imm16 #xFFFF 5) (hw 3 21))

;; MOVK Xd, #imm16, LSL #(hw*16)
(defenc movk #xF2800000 (rd imm16 &optional (hw 0))
  "MOVK Xd, #imm16, LSL #(hw*16)."
  (rd #x1F 0) (imm16 #xFFFF 5) (hw 3 21))

;; MOV Xd, Xn (register) = ORR Xd, XZR, Xn
;; Encoding: #xAA0003E0 | (Xn << 16) | Xd
(defenc mov-rr #xAA0003E0 (rd rn)
  "MOV Xd, Xn (register copy via ORR Xd, XZR, Xn)."
  (rd #x1F 0) (rn #x1F 16))

;;; --- Byte Manipulation ---

;; REV Wd, Wn (32-bit byte reverse)
(defenc rev32 #x5AC00800 (rd rn)
  "REV Wd, Wn (reverse bytes in low 32 bits)."
  (rd #x1F 0) (rn #x1F 5))

;;; --- Arithmetic (3-register) ---

;; ADD Xd, Xn, Xm (64-bit, no shift)
(defenc add #x8B000000 (rd rn rm)
  "ADD Xd, Xn, Xm (64-bit)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; ADD Xd, Xn, #imm12{, LSL #12}
(defenc add-imm #x91000000 (rd rn imm12 &optional (shift12 0))
  "ADD Xd, Xn, #imm12, optionally shifted left by 12 when SHIFT12=1."
  (rd #x1F 0) (rn #x1F 5) (imm12 #xFFF 10) (shift12 1 22))

;; ADDS Xd, Xn, Xm (64-bit, sets flags)
(defenc adds #xAB000000 (rd rn rm)
  "ADDS Xd, Xn, Xm (64-bit, sets NZCV flags)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; SUB Xd, Xn, Xm (64-bit, no shift)
(defenc sub #xCB000000 (rd rn rm)
  "SUB Xd, Xn, Xm (64-bit)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; SUB Xd, Xn, #imm12{, LSL #12}
(defenc sub-imm #xD1000000 (rd rn imm12 &optional (shift12 0))
  "SUB Xd, Xn, #imm12, optionally shifted left by 12 when SHIFT12=1."
  (rd #x1F 0) (rn #x1F 5) (imm12 #xFFF 10) (shift12 1 22))

;; SUBS Xd, Xn, Xm (64-bit, sets flags)
(defenc subs #xEB000000 (rd rn rm)
  "SUBS Xd, Xn, Xm (64-bit, sets NZCV flags)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; MUL Xd, Xn, Xm = MADD Xd, Xn, Xm, XZR
(defenc mul #x9B007C00 (rd rn rm)
  "MUL Xd, Xn, Xm (via MADD Xd, Xn, Xm, XZR)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; SMULH Xd, Xn, Xm (signed high 64 bits of 128-bit product)
(defenc smulh #x9B407C00 (rd rn rm)
  "SMULH Xd, Xn, Xm (signed high half of 64x64->128 multiply)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; SBFM Xd, Xn, #immr, #63 = ASR Xd, Xn, #immr (64-bit arithmetic shift right immediate)
(defenc asr #x9340FC00 (rd rn immr)
  "ASR Xd, Xn, #immr (64-bit arithmetic shift right, encodes as SBFM)."
  (rd #x1F 0) (rn #x1F 5) (immr #x3F 16))

;; FSQRT Dd, Dn (scalar double-precision square root)
(defenc fsqrt #x1E61C000 (rd rn)
  "FSQRT Dd, Dn (scalar double-precision square root)."
  (rd #x1F 0) (rn #x1F 5))

;; FMOV Dd, Dn (scalar double-precision floating-point register move)
(defenc fmov-dd #x1E604000 (rd rn)
  "FMOV Dd, Dn (scalar double-precision floating-point register-to-register move)."
  (rd #x1F 0) (rn #x1F 5))

;; UMULH Xd, Xn, Xm (unsigned high 64 bits of 128-bit product)
(defenc umulh #x9BC07C00 (rd rn rm)
  "UMULH Xd, Xn, Xm (unsigned high half of 64x64->128 multiply)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; CMP Xn, Xm (alias for SUBS XZR, Xn, Xm)
(defenc cmp #xEB00001F (rn rm)
  "CMP Xn, Xm (compare signed 64-bit registers)."
  (rn #x1F 5) (rm #x1F 16))

;;; --- Conditional ---

;; CSEL Xd, Xn, Xm, cond
(defenc csel #x9A800000 (rd rn rm cond)
  "CSEL Xd, Xn, Xm, cond (conditional select)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16) (cond #xF 12))

;;; --- Shift / Rotate ---

;; RORV Xd, Xn, Xm (rotate right variable)
(defenc rorv #x9AC02C00 (rd rn rm)
  "RORV Xd, Xn, Xm (rotate right by bottom bits of Xm)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;;; --- Branch ---

;; CBZ Xn, #imm (Compare and Branch if Zero)
(defenc cbz #xB4000000 (rn imm19)
  "CBZ Xn, #imm19 (branch if Xn == 0). imm19 in instruction units."
  (rn #x1F 0) (imm19 #x7FFFF 5))

;; B #imm (Unconditional branch)
(defenc b #x14000000 (imm26)
  "B #imm26 (unconditional branch). imm26 in instruction units."
  (imm26 #x3FFFFFF 0))

;; BLR Xn (Branch with Link to Register - indirect call)
(defenc blr #xD63F0000 (rn)
  "BLR Xn (indirect call through register)."
  (rn #x1F 5))

;; BR Xn (Branch to Register - indirect tail call)
(defenc br #xD61F0000 (rn)
  "BR Xn (indirect tail jump through register)."
  (rn #x1F 5))

;;; --- RET (fixed encoding, no variable fields) ---

(defconstant +a64-ret+ #xD65F03C0
  "AArch64 RET instruction (return via X30).")

;;; --- Memory: Unscaled ---

;; STUR Xt, [Xn, #simm9] (handles negative offsets)
(defenc stur #xF8000000 (rt rn simm9)
  "STUR Xt, [Xn, #simm9] (unscaled store, supports negative offsets)."
  (rt #x1F 0) (rn #x1F 5) (simm9 #x1FF 12))

;; LDUR Xt, [Xn, #simm9] (handles negative offsets)
(defenc ldur #xF8400000 (rt rn simm9)
  "LDUR Xt, [Xn, #simm9] (unscaled load, supports negative offsets)."
  (rt #x1F 0) (rn #x1F 5) (simm9 #x1FF 12))

;;; --- Memory: Indexed ---

;; STR Xt, [Xn], #simm9 (post-indexed store)
(defenc str-post #xF8000400 (rt rn simm9)
  "STR Xt, [Xn], #simm9 (post-indexed store with writeback)."
  (rt #x1F 0) (rn #x1F 5) (simm9 #x1FF 12))

;; LDR Xt, [Xn, #simm9]! (pre-indexed load)
(defenc ldr-pre #xF8400C00 (rt rn simm9)
  "LDR Xt, [Xn, #simm9]! (pre-indexed load with writeback)."
  (rt #x1F 0) (rn #x1F 5) (simm9 #x1FF 12))

;;; --- Memory: Pair ---

;; STP Xt1, Xt2, [Xn, #imm7*8]! (pre-index store pair)
(defenc stp-pre #xA9800000 (rt1 rt2 rn imm7)
  "STP Xt1, Xt2, [Xn, #imm7*8]! (pre-indexed). imm7 can be negative."
  (rt1 #x1F 0) (rn #x1F 5) (rt2 #x1F 10) (imm7 #x7F 15))

;; LDP Xt1, Xt2, [Xn], #imm7*8 (post-index load pair)
(defenc ldp-post #xA8C00000 (rt1 rt2 rn imm7)
  "LDP Xt1, Xt2, [Xn], #imm7*8 (post-indexed). imm7 can be positive."
  (rt1 #x1F 0) (rn #x1F 5) (rt2 #x1F 10) (imm7 #x7F 15))

;;; --- Conditional Branch ---

;; B.cond #imm
(defenc b-cond #x54000000 (imm19 cond)
  "B.cond #imm19 with condition code COND. imm19 is in instruction units."
  (imm19 #x7FFFF 5) (cond #xF 0))

;;; --- Exception ---

;; BRK #imm16
(defenc brk #xD4200000 (&optional (imm16 0))
  "BRK #imm16 exception instruction."
  (imm16 #xFFFF 5))

(defun aarch64-tls-base-register ()
  "Return the selected AArch64 TLS base register from optimizer planning."
  (let ((plan (opt-build-tls-plan :target :aarch64 :hot-access-p t)))
    (opt-tls-plan-base-register plan)))

(defun aarch64-atomic-lowering-plan (operation memory-order)
  "Return AArch64 atomic lowering metadata for OPERATION and MEMORY-ORDER.

Result plist keys:
- :opcode          selected representative opcode keyword
- :pre-fence       list of fence opcodes before atomic op
- :post-fence      list of fence opcodes after atomic op"
  (let* ((plan (opt-build-atomic-plan
                :target :aarch64
                :operation operation
                :memory-order memory-order))
         (pre-fence (case memory-order
                      ((:acquire :acq-rel :seq-cst) '(:dmb-ish))
                      (otherwise nil)))
         (post-fence (case memory-order
                       ((:release :acq-rel :seq-cst) '(:dmb-ish))
                       (otherwise nil))))
    (list :opcode (opt-atomic-plan-opcode plan)
          :pre-fence pre-fence
          :post-fence post-fence)))

;; emit-a64-mov-imm64, a64-imm64-size, *a64-instruction-sizes*, a64-instruction-size,
;; and build-a64-label-offsets are in aarch64-codegen-labels.lisp (loaded next).

;;; VM Instruction Emitters
