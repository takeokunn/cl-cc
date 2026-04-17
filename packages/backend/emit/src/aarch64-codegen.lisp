;;;; packages/backend/emit/src/aarch64-codegen.lisp - AArch64 Machine Code Generation
;;;
;;; Generates native AArch64 machine code bytes from VM instructions.
;;; All AArch64 instructions are 32 bits (4 bytes) fixed-width.

(in-package :cl-cc/emit)

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

;;; AArch64 Instruction Encodings

;;; MOVZ Xd, #imm16, LSL #(hw*16)
;;; Encoding: 1_10_100101_hw_imm16_Rd
;;; Base constant: #xD2800000 (sf=1, opc=MOVZ, fixed bits)
(defun encode-movz (rd imm16 &optional (hw 0))
  "MOVZ Xd, #imm16, LSL #(hw*16). hw=0..3 for shifts 0,16,32,48."
  (logior #xD2800000
          (ash (logand hw 3) 21)
          (ash (logand imm16 #xFFFF) 5)
          (logand rd #x1F)))

;;; MOVK Xd, #imm16, LSL #(hw*16) - move with keep (preserves other bits)
;;; Base constant: #xF2800000 (sf=1, opc=MOVK, fixed bits)
(defun encode-movk (rd imm16 &optional (hw 0))
  "MOVK Xd, #imm16, LSL #(hw*16)."
  (logior #xF2800000
          (ash (logand hw 3) 21)
          (ash (logand imm16 #xFFFF) 5)
          (logand rd #x1F)))

;;; MOV Xd, Xn (register) = ORR Xd, XZR, Xn
;;; Encoding: #xAA0003E0 | (Xn << 16) | Xd
;;; (XZR=31 in Rn position at bits 9-5, already embedded in base)
(defun encode-mov-rr (rd rn)
  "MOV Xd, Xn (register copy via ORR Xd, XZR, Xn)."
  (logior #xAA0003E0
          (ash (logand rn #x1F) 16)
          (logand rd #x1F)))

;;; REV Wd, Wn (32-bit byte reverse)
;;; Encoding: #x5AC00800 | (Rn << 5) | Rd
(defun encode-rev32 (rd rn)
  "REV Wd, Wn (reverse bytes in low 32 bits)."
  (logior #x5AC00800
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; ADD Xd, Xn, Xm (64-bit, no shift)
;;; Encoding: #x8B000000 | (Xm << 16) | (Xn << 5) | Xd
(defun encode-add (rd rn rm)
  "ADD Xd, Xn, Xm (64-bit)."
  (logior #x8B000000
          (ash (logand rm #x1F) 16)
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; SUB Xd, Xn, Xm (64-bit, no shift)
;;; Encoding: #xCB000000 | (Xm << 16) | (Xn << 5) | Xd
(defun encode-sub (rd rn rm)
  "SUB Xd, Xn, Xm (64-bit)."
  (logior #xCB000000
          (ash (logand rm #x1F) 16)
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; MUL Xd, Xn, Xm = MADD Xd, Xn, Xm, XZR
;;; Encoding: #x9B007C00 | (Xm << 16) | (Xn << 5) | Xd
;;; (XZR=31 in Ra position at bits 14-10, already embedded as #x7C00)
(defun encode-mul (rd rn rm)
  "MUL Xd, Xn, Xm (via MADD Xd, Xn, Xm, XZR)."
  (logior #x9B007C00
          (ash (logand rm #x1F) 16)
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; CMP Xn, Xm (alias for SUBS XZR, Xn, Xm)
;;; Encoding: #xEB00001F | (Rm << 16) | (Rn << 5)
(defun encode-cmp (rn rm)
  "CMP Xn, Xm (compare signed 64-bit registers)."
  (logior #xEB00001F
          (ash (logand rm #x1F) 16)
          (ash (logand rn #x1F) 5)))

;;; CSEL Xd, Xn, Xm, cond
;;; Encoding: #x9A800000 | (Rm << 16) | (cond << 12) | (Rn << 5) | Rd
(defun encode-csel (rd rn rm cond)
  "CSEL Xd, Xn, Xm, cond (conditional select)."
  (logior #x9A800000
          (ash (logand rm #x1F) 16)
          (ash (logand cond #xF) 12)
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; RORV Xd, Xn, Xm (rotate right variable)
;;; Encoding: #x9AC02C00 | (Rm << 16) | (Rn << 5) | Rd
(defun encode-rorv (rd rn rm)
  "RORV Xd, Xn, Xm (rotate right by bottom bits of Xm)."
  (logior #x9AC02C00
          (ash (logand rm #x1F) 16)
          (ash (logand rn #x1F) 5)
          (logand rd #x1F)))

;;; CBZ Xn, #imm (Compare and Branch if Zero)
;;; Encoding: #xB4000000 | (imm19 << 5) | Xn
;;; imm19 is PC-relative offset in units of 4 bytes (instructions)
(defun encode-cbz (rn imm19)
  "CBZ Xn, #imm19 (branch if Xn == 0). imm19 in instruction units."
  (logior #xB4000000
          (ash (logand imm19 #x7FFFF) 5)
          (logand rn #x1F)))

;;; B #imm (Unconditional branch)
;;; Encoding: #x14000000 | imm26
;;; imm26 is PC-relative offset in units of 4 bytes
(defun encode-b (imm26)
  "B #imm26 (unconditional branch). imm26 in instruction units."
  (logior #x14000000
          (logand imm26 #x3FFFFFF)))

;;; BLR Xn (Branch with Link to Register - indirect call)
;;; Encoding: #xD63F0000 | (Xn << 5)
(defun encode-blr (rn)
  "BLR Xn (indirect call through register)."
  (logior #xD63F0000
          (ash (logand rn #x1F) 5)))

;;; BR Xn (Branch to Register - indirect tail call)
;;; Encoding: #xD61F0000 | (Xn << 5)
(defun encode-br (rn)
  "BR Xn (indirect tail jump through register)."
  (logior #xD61F0000
          (ash (logand rn #x1F) 5)))

;;; RET (Return using X30/LR)
;;; Fixed encoding: #xD65F03C0
(defconstant +a64-ret+ #xD65F03C0
  "AArch64 RET instruction (return via X30).")

;;; STUR Xt, [Xn, #simm9] (Store Unscaled - handles negative offsets)
;;; Encoding: #xF8000000 | (simm9 << 12) | (Xn << 5) | Xt
;;; simm9 is a 9-bit signed byte offset
(defun encode-stur (rt rn simm9)
  "STUR Xt, [Xn, #simm9] (unscaled store, supports negative offsets)."
  (logior #xF8000000
          (ash (logand simm9 #x1FF) 12)
          (ash (logand rn #x1F) 5)
          (logand rt #x1F)))

;;; LDUR Xt, [Xn, #simm9] (Load Unscaled - handles negative offsets)
;;; Encoding: #xF8400000 | (simm9 << 12) | (Xn << 5) | Xt
(defun encode-ldur (rt rn simm9)
  "LDUR Xt, [Xn, #simm9] (unscaled load, supports negative offsets)."
  (logior #xF8400000
          (ash (logand simm9 #x1FF) 12)
          (ash (logand rn #x1F) 5)
          (logand rt #x1F)))

;;; STR Xt, [Xn], #simm9 (post-indexed store)
(defun encode-str-post (rt rn simm9)
  "STR Xt, [Xn], #simm9 (post-indexed store with writeback)."
  (logior #xF8000400
          (ash (logand simm9 #x1FF) 12)
          (ash (logand rn #x1F) 5)
          (logand rt #x1F)))

;;; LDR Xt, [Xn, #simm9]! (pre-indexed load)
(defun encode-ldr-pre (rt rn simm9)
  "LDR Xt, [Xn, #simm9]! (pre-indexed load with writeback)."
  (logior #xF8400C00
          (ash (logand simm9 #x1FF) 12)
          (ash (logand rn #x1F) 5)
          (logand rt #x1F)))

;;; STP Xt1, Xt2, [Xn, #imm7*8]! (pre-index store pair)
;;; Encoding: #xA9800000 | (imm7 << 15) | (Xt2 << 10) | (Xn << 5) | Xt1
;;; imm7 is a 7-bit signed value; actual offset = imm7 * 8
(defun encode-stp-pre (rt1 rt2 rn imm7)
  "STP Xt1, Xt2, [Xn, #imm7*8]! (pre-indexed). imm7 can be negative."
  (logior #xA9800000
          (ash (logand imm7 #x7F) 15)
          (ash (logand rt2 #x1F) 10)
          (ash (logand rn #x1F) 5)
          (logand rt1 #x1F)))

;;; LDP Xt1, Xt2, [Xn], #imm7*8 (post-index load pair)
;;; Encoding: #xA8C00000 | (imm7 << 15) | (Xt2 << 10) | (Xn << 5) | Xt1
(defun encode-ldp-post (rt1 rt2 rn imm7)
  "LDP Xt1, Xt2, [Xn], #imm7*8 (post-indexed). imm7 can be positive."
  (logior #xA8C00000
          (ash (logand imm7 #x7F) 15)
          (ash (logand rt2 #x1F) 10)
          (ash (logand rn #x1F) 5)
          (logand rt1 #x1F)))

;;; B.cond #imm (conditional branch)
(defun encode-b-cond (imm19 cond)
  "B.cond #imm19 with condition code COND. imm19 is in instruction units."
  (logior #x54000000
          (ash (logand imm19 #x7FFFF) 5)
          (logand cond #xF)))

;;; BRK #imm16
(defun encode-brk (&optional (imm16 0))
  "BRK #imm16 exception instruction."
  (logior #xD4200000
          (ash (logand imm16 #xFFFF) 5)))

;; emit-a64-mov-imm64, a64-imm64-size, *a64-instruction-sizes*, a64-instruction-size,
;; and build-a64-label-offsets are in aarch64-codegen-labels.lisp (loaded next).

;;; VM Instruction Emitters

