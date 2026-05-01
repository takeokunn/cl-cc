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
(defconstant +a64-fp+ 29 "Frame pointer / X29.")
(defconstant +a64-lr+ 30 "Link register / X30.")
(defconstant +a64-scs+ 18 "Shadow call stack pointer / X18.")
(defconstant +a64-scs-tmp+ 17 "Scratch register used for shadow call stack checks / X17.")

;;; Dynamic variable holding current regalloc during code generation

(defparameter *current-a64-regalloc* nil
  "Current regalloc-result during AArch64 code generation.")

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

;; SUB Xd, Xn, Xm (64-bit, no shift)
(defenc sub #xCB000000 (rd rn rm)
  "SUB Xd, Xn, Xm (64-bit)."
  (rd #x1F 0) (rn #x1F 5) (rm #x1F 16))

;; MUL Xd, Xn, Xm = MADD Xd, Xn, Xm, XZR
(defenc mul #x9B007C00 (rd rn rm)
  "MUL Xd, Xn, Xm (via MADD Xd, Xn, Xm, XZR)."
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

;; emit-a64-mov-imm64, a64-imm64-size, *a64-instruction-sizes*, a64-instruction-size,
;; and build-a64-label-offsets are in aarch64-codegen-labels.lisp (loaded next).

;;; VM Instruction Emitters
