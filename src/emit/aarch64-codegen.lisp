;;;; src/backend/aarch64-codegen.lisp - AArch64 Machine Code Generation
;;;
;;; Generates native AArch64 machine code bytes from VM instructions.
;;; All AArch64 instructions are 32 bits (4 bytes) fixed-width.

(in-package :cl-cc)

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

;;; B.EQ #imm (Conditional branch, equal / zero flag)
;;; Encoding: #x54000000 | (imm19 << 5) | cond
;;; EQ=0, NE=1
(defun encode-b-cond (imm19 cond)
  "B.cond #imm19. cond: 0=EQ, 1=NE. imm19 in instruction units."
  (logior #x54000000
          (ash (logand imm19 #x7FFFF) 5)
          (logand cond #xF)))

;;; BLR Xn (Branch with Link to Register - indirect call)
;;; Encoding: #xD63F0000 | (Xn << 5)
(defun encode-blr (rn)
  "BLR Xn (indirect call through register)."
  (logior #xD63F0000
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

;;; 64-bit Immediate Loading

(defun emit-a64-mov-imm64 (rd value stream)
  "Emit MOVZ/MOVK sequence to load a 64-bit integer VALUE into register RD."
  (let ((chunk0 (logand value #xFFFF))
        (chunk1 (logand (ash value -16) #xFFFF))
        (chunk2 (logand (ash value -32) #xFFFF))
        (chunk3 (logand (ash value -48) #xFFFF)))
    ;; Always emit MOVZ for chunk0
    (emit-a64-instr (encode-movz rd chunk0 0) stream)
    ;; Emit MOVK for non-zero upper chunks
    (when (or (not (zerop chunk1)) (not (zerop chunk2)) (not (zerop chunk3)))
      (emit-a64-instr (encode-movk rd chunk1 1) stream))
    (when (or (not (zerop chunk2)) (not (zerop chunk3)))
      (emit-a64-instr (encode-movk rd chunk2 2) stream))
    (when (not (zerop chunk3))
      (emit-a64-instr (encode-movk rd chunk3 3) stream))))

(defun a64-imm64-size (value)
  "Return the number of 4-byte instructions needed to load VALUE."
  (let ((chunk1 (logand (ash value -16) #xFFFF))
        (chunk2 (logand (ash value -32) #xFFFF))
        (chunk3 (logand (ash value -48) #xFFFF)))
    (cond ((not (zerop chunk3)) 4)
          ((not (zerop chunk2)) 3)
          ((not (zerop chunk1)) 2)
          (t 1))))

;;; Instruction Size Estimation (for two-pass label resolution)

(defun a64-instruction-size (inst)
  "Return size in bytes (multiple of 4) for an AArch64-encoded VM instruction."
  (typecase inst
    (vm-const (* 4 (a64-imm64-size (logand (vm-value inst) #xFFFFFFFFFFFFFFFF))))
    (vm-move 4)          ; encode-mov-rr
    (vm-add 4)           ; encode-add
    (vm-sub 4)           ; encode-sub
    (vm-mul 4)           ; encode-mul
    (vm-label 0)         ; no code
    (vm-jump 4)          ; encode-b
    (vm-jump-zero 4)     ; encode-cbz (one instruction)
    (vm-halt 4)          ; encode-mov-rr (always 1 instruction)
    (vm-ret 28)          ; 6x LDP + RET = 7 instructions (see emit-a64-vm-ret)
    (vm-call 4)          ; encode-blr
    (vm-spill-store 4)   ; STUR
    (vm-spill-load 4)    ; LDUR
    (vm-print 0)
    (t 0)))

;;; Label offset table builder

(defun build-a64-label-offsets (instructions prologue-size)
  "Build hash table mapping label names to byte offsets from function start."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (a64-instruction-size inst)))
    offsets))

;;; VM Instruction Emitters

(defun emit-a64-vm-const (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (value (vm-value inst)))
    ;; Handle negative values via logical masking to 64-bit
    (emit-a64-mov-imm64 rd (logand value #xFFFFFFFFFFFFFFFF) stream)))

(defun emit-a64-vm-move (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (emit-a64-instr (encode-mov-rr rd rn) stream)))

(defun emit-a64-vm-add (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-add rd rn rm) stream)))

(defun emit-a64-vm-sub (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-sub rd rn rm) stream)))

(defun emit-a64-vm-mul (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-mul rd rn rm) stream)))

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
  ;; Restore in reverse push order (X27/X28 pushed last, popped first)
  (emit-a64-instr (encode-ldp-post 27 28 +a64-sp+ 2) stream)
  (emit-a64-instr (encode-ldp-post 25 26 +a64-sp+ 2) stream)
  (emit-a64-instr (encode-ldp-post 23 24 +a64-sp+ 2) stream)
  (emit-a64-instr (encode-ldp-post 21 22 +a64-sp+ 2) stream)
  (emit-a64-instr (encode-ldp-post 19 20 +a64-sp+ 2) stream)
  (emit-a64-instr (encode-ldp-post 29 30 +a64-sp+ 2) stream)
  (emit-a64-instr +a64-ret+ stream))

;;; Main program emitter

(defun emit-a64-instruction (inst stream current-pos label-offsets)
  "Emit AArch64 machine code for one VM instruction."
  (typecase inst
    (vm-const    (emit-a64-vm-const inst stream))
    (vm-move     (emit-a64-vm-move inst stream))
    (vm-add      (emit-a64-vm-add inst stream))
    (vm-sub      (emit-a64-vm-sub inst stream))
    (vm-mul      (emit-a64-vm-mul inst stream))
    (vm-label    nil)
    (vm-jump     (emit-a64-vm-jump inst stream current-pos label-offsets))
    (vm-jump-zero (emit-a64-vm-jump-zero inst stream current-pos label-offsets))
    (vm-halt     (emit-a64-vm-halt inst stream))
    (vm-ret      (emit-a64-vm-ret inst stream))
    (vm-call     (emit-a64-instr (encode-blr (a64-reg (vm-func-reg inst))) stream))
    (vm-spill-store (emit-a64-vm-spill-store inst stream))
    (vm-spill-load  (emit-a64-vm-spill-load inst stream))
    (vm-print    nil)
    (t           (warn "Skipping unsupported AArch64 instruction: ~A" (type-of inst)))))

;;; AArch64 callee-saved registers to save/restore (X19-X28, X29, X30)
;;; X29 = frame pointer (FP), X30 = link register (LR)
;;; Saved as pairs: (X29,X30), (X19,X20), (X21,X22), (X23,X24), (X25,X26), (X27,X28)
;;; Total prologue size: 6 STP x 4 bytes = 24 bytes

(defun emit-a64-prologue (stream)
  "Emit AArch64 function prologue: save FP, LR, and all callee-saved registers X19-X28."
  ;; STP X29, X30, [SP, #-16]!  (imm7 = -16/8 = -2)
  (emit-a64-instr (encode-stp-pre 29 30 +a64-sp+ -2) stream)
  ;; STP X19, X20, [SP, #-16]!
  (emit-a64-instr (encode-stp-pre 19 20 +a64-sp+ -2) stream)
  ;; STP X21, X22, [SP, #-16]!
  (emit-a64-instr (encode-stp-pre 21 22 +a64-sp+ -2) stream)
  ;; STP X23, X24, [SP, #-16]!
  (emit-a64-instr (encode-stp-pre 23 24 +a64-sp+ -2) stream)
  ;; STP X25, X26, [SP, #-16]!
  (emit-a64-instr (encode-stp-pre 25 26 +a64-sp+ -2) stream)
  ;; STP X27, X28, [SP, #-16]!
  (emit-a64-instr (encode-stp-pre 27 28 +a64-sp+ -2) stream))

(defun emit-a64-epilogue (stream)
  "Emit AArch64 function epilogue: restore callee-saved registers and return.
   Restores in reverse push order."
  ;; LDP X27, X28, [SP], #16  (imm7 = 16/8 = 2)
  (emit-a64-instr (encode-ldp-post 27 28 +a64-sp+ 2) stream)
  ;; LDP X25, X26, [SP], #16
  (emit-a64-instr (encode-ldp-post 25 26 +a64-sp+ 2) stream)
  ;; LDP X23, X24, [SP], #16
  (emit-a64-instr (encode-ldp-post 23 24 +a64-sp+ 2) stream)
  ;; LDP X21, X22, [SP], #16
  (emit-a64-instr (encode-ldp-post 21 22 +a64-sp+ 2) stream)
  ;; LDP X19, X20, [SP], #16
  (emit-a64-instr (encode-ldp-post 19 20 +a64-sp+ 2) stream)
  ;; LDP X29, X30, [SP], #16
  (emit-a64-instr (encode-ldp-post 29 30 +a64-sp+ 2) stream)
  ;; RET
  (emit-a64-instr +a64-ret+ stream))

(defun emit-a64-program (program stream)
  "Emit AArch64 machine code for the entire VM program.
   Uses two-pass approach for label resolution."
  (let* ((instructions (vm-program-instructions program))
         ;; Prologue: 6 STP instructions x 4 bytes = 24 bytes
         (prologue-size 24)
         (label-offsets (build-a64-label-offsets instructions prologue-size)))
    ;; Prologue
    (emit-a64-prologue stream)
    ;; Second pass: emit instructions
    (let ((pos prologue-size))
      (dolist (inst instructions)
        (emit-a64-instruction inst stream pos label-offsets)
        (incf pos (a64-instruction-size inst))))
    ;; Epilogue
    (emit-a64-epilogue stream)))

;;; Public API

(defun compile-to-aarch64-bytes (program)
  "Compile VM program to AArch64 machine code bytes.
   Returns: (simple-array (unsigned-byte 8) (*))"
  (let* ((instructions (vm-program-instructions program))
         (ra (allocate-registers instructions *aarch64-calling-convention*))
         (allocated-program (make-vm-program
                             :instructions (regalloc-instructions ra)
                             :result-register (vm-program-result-register program))))
    (let ((*current-a64-regalloc* ra))
      (with-output-to-vector (stream)
        (emit-a64-program allocated-program stream)))))
