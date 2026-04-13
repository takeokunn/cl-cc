;;;; src/emit/x86-64-encoding.lisp - x86-64 Encoding Primitives
;;;
;;; Pure ISA-level encoding layer: functions that only take raw integer
;;; register codes and streams, with no VM type references.

(in-package :cl-cc)

;;; x86-64 Register Encoding

;; Register codes (4-bit values)
;; Low registers (0-7): RAX-RDI, REX.B = 0
;; High registers (8-15): R8-R15, REX.B = 1
(defconstant +rax+ 0)
(defconstant +rcx+ 1)
(defconstant +rdx+ 2)
(defconstant +rbx+ 3)
(defconstant +rsp+ 4)
(defconstant +rbp+ 5)
(defconstant +rsi+ 6)
(defconstant +rdi+ 7)
(defconstant +r8+  8)
(defconstant +r9+  9)
(defconstant +r10+ 10)
(defconstant +r11+ 11)
(defconstant +r12+ 12)
(defconstant +r13+ 13)
(defconstant +r14+ 14)
(defconstant +r15+ 15)

;; XMM register encodings use the same 4-bit register ids.
(defconstant +xmm0+ 0)
(defconstant +xmm1+ 1)
(defconstant +xmm2+ 2)
(defconstant +xmm3+ 3)
(defconstant +xmm4+ 4)
(defconstant +xmm5+ 5)
(defconstant +xmm6+ 6)
(defconstant +xmm7+ 7)
(defconstant +xmm8+ 8)
(defconstant +xmm9+ 9)
(defconstant +xmm10+ 10)
(defconstant +xmm11+ 11)
(defconstant +xmm12+ 12)
(defconstant +xmm13+ 13)
(defconstant +xmm14+ 14)
(defconstant +xmm15+ 15)

;; Calling convention (System V AMD64 ABI)
;; Arguments: RDI, RSI, RDX, RCX, R8, R9
;; Return: RAX
;; Preserved: RBX, RBP, R12-R15
;; Temp: RAX, RCX, RDX, RSI, RDI, R8-R11

;;; Output Stream Utilities

(defmacro with-output-to-vector ((stream-var) &body body)
  "Execute BODY with STREAM-VAR bound to an output stream that collects bytes into a vector."
  `(let ((bytes '()))
      (labels ((stream-write-byte (byte)
                (push byte bytes)))
        (let ((,stream-var #'stream-write-byte))
          ,@body
          (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))))

(defun emit-byte (byte stream)
  "Write single byte to stream (which is a function that takes a byte)."
  (funcall stream (logand byte #xFF)))

(defun emit-dword (dword stream)
  "Write 32-bit value (little-endian)."
  (emit-byte (logand dword #xFF) stream)
  (emit-byte (logand (ash dword -8) #xFF) stream)
  (emit-byte (logand (ash dword -16) #xFF) stream)
  (emit-byte (logand (ash dword -24) #xFF) stream))

(defun emit-qword (qword stream)
  "Write 64-bit value (little-endian)."
  (emit-dword (logand qword #xFFFFFFFF) stream)
  (emit-dword (ash qword -32) stream))

;;; REX Prefix

(defun rex-prefix (&key (w 0) (r 0) (x 0) (b 0))
  "Build REX prefix byte.
   W=1: 64-bit operand, R: ModR/M reg extension, B: ModR/M r/m extension"
  (+ #x40 (ash w 3) (ash r 2) (ash x 1) b))

;;; ModR/M Encoding

(defun modrm (mod reg rm)
  "Build ModR/M byte.
   MOD: 2 bits (00=mem, 01=disp8, 10=disp32, 11=reg)
   REG: 3 bits (register or opcode extension)
   RM: 3 bits (register or addressing mode)"
  (+ (ash (logand mod #x3) 6)
     (ash (logand reg #x7) 3)
     (logand rm #x7)))

(defun sib (scale index base)
  "Build SIB byte.
   SCALE: 2 bits, INDEX: 3 bits, BASE: 3 bits."
  (+ (ash (logand scale #x3) 6)
     (ash (logand index #x7) 3)
     (logand base #x7)))

(defparameter *sib-scale-table*
  '((1 . 0) (2 . 1) (4 . 2) (8 . 3))
  "Alist mapping scale factor {1,2,4,8} to its 2-bit SIB field encoding.")

(defun scale->sib-bits (scale)
  "Return the SIB scale field bits for SCALE ∈ {1,2,4,8}."
  (or (cdr (assoc scale *sib-scale-table*))
      (error "Unsupported x86-64 scale factor ~A" scale)))

(defun %emit-modrm-address (mod reg base offset stream)
  "Emit ModR/M (+ SIB if needed) for [BASE + OFFSET].

   x86-64 requires an explicit SIB byte when BASE is RSP/R12."
  (let ((rm (logand base #x7)))
    (emit-byte (modrm mod reg (if (= rm 4) 4 rm)) stream)
    (when (= rm 4)
      ;; scale=1, no index, base=RSP/R12
      (emit-byte (sib 0 4 4) stream))
    (unless (zerop offset)
      (emit-byte (logand offset #xFF) stream))))

(defun %emit-modrm-indexed-address (mod reg base index scale offset stream)
  "Emit ModR/M+SIB for [BASE + INDEX*SCALE + OFFSET]."
  (let ((base-rm (logand base #x7))
        (index-rm (logand index #x7)))
    (when (= index-rm 4)
      (error "RSP/R12 cannot be used as SIB index register"))
    (emit-byte (modrm mod reg 4) stream)
    (emit-byte (sib (scale->sib-bits scale) index-rm base-rm) stream)
    (unless (zerop offset)
      (emit-byte (logand offset #xFF) stream))))

;;; Instructions

(defun emit-mov-rr64 (dst src stream)
  "MOV dst, src (64-bit register to register).

   Encoding: MOV r/m64, r64 (0x89): REG=src, R/M=dst
             REX.R extends REG (src), REX.B extends R/M (dst)"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x89 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-mov-ri64 (dst imm stream)
  "MOV dst, imm64 (64-bit immediate to register).

   Encoding: REX.W + B8+ rd"
  (emit-byte (rex-prefix :w 1 :b (ash dst -3)) stream)
  (emit-byte (+ #xB8 (logand dst #x7)) stream)
  (emit-qword imm stream))

(defun emit-mov-rm64 (dst base offset stream)
  "MOV dst, [base + offset] (load from memory).

   For offset = 0: REX.W + 8B /r (mod=00)
   For offset fits in byte: REX.W + 8B /r (mod=01)"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (if (zerop offset)
      (%emit-modrm-address 0 dst base offset stream)
      (%emit-modrm-address 1 dst base offset stream)))

(defun emit-mov-mr64 (base offset src stream)
  "MOV [base + offset], src (store to memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (if (zerop offset)
      (%emit-modrm-address 0 src base offset stream)
      (%emit-modrm-address 1 src base offset stream)))

(defun emit-mov-rm64-indexed (dst base index scale offset stream)
  "MOV dst, [base + index*scale + offset] (load from indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (if (zerop offset)
      (%emit-modrm-indexed-address 0 dst base index scale offset stream)
      (%emit-modrm-indexed-address 1 dst base index scale offset stream)))

(defun emit-mov-mr64-indexed (base index scale offset src stream)
  "MOV [base + index*scale + offset], src (store to indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (if (zerop offset)
      (%emit-modrm-indexed-address 0 src base index scale offset stream)
      (%emit-modrm-indexed-address 1 src base index scale offset stream)))

(defun emit-movq-xmm-r64 (dst-xmm src-gpr stream)
  "MOVQ xmm, r64 (copy raw 64 bits from GPR to XMM low lane)."
  (emit-byte #x66 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst-xmm -3) :b (ash src-gpr -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x6E stream)
  (emit-byte (modrm 3 dst-xmm src-gpr) stream))

(defun emit-movsd-xx (dst-xmm src-xmm stream)
  "MOVSD xmm, xmm (scalar double copy)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x10 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-addsd-xx (dst-xmm src-xmm stream)
  "ADDSD xmm, xmm (scalar double add)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x58 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-subsd-xx (dst-xmm src-xmm stream)
  "SUBSD xmm, xmm (scalar double subtract)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x5C stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-mulsd-xx (dst-xmm src-xmm stream)
  "MULSD xmm, xmm (scalar double multiply)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x59 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-divsd-xx (dst-xmm src-xmm stream)
  "DIVSD xmm, xmm (scalar double divide)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x5E stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-add-rr64 (dst src stream)
  "ADD dst, src (64-bit).

   Encoding: REX.W + 01 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x01 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-sub-rr64 (dst src stream)
  "SUB dst, src (64-bit).

   Encoding: REX.W + 29 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x29 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-imul-rr64 (dst src stream)
  "IMUL dst, src (64-bit signed multiply).

   Encoding: REX.W + 0F AF /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xAF stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-cmp-rr64 (op1 op2 stream)
  "CMP op1, op2 (64-bit compare).

   Encoding: REX.W + 39 /r"
  (emit-byte (rex-prefix :w 1 :r (ash op2 -3) :b (ash op1 -3)) stream)
  (emit-byte #x39 stream)
  (emit-byte (modrm 3 op2 op1) stream))

(defun emit-push-r64 (reg stream)
  "PUSH reg (64-bit).

   Encoding: 50+ rd"
  (emit-byte (+ #x50 (logand reg #x7)) stream))

(defun emit-pop-r64 (reg stream)
  "POP reg (64-bit).

   Encoding: 58+ rd"
  (emit-byte (+ #x58 (logand reg #x7)) stream))

(defun emit-ret (stream)
  "RET (return).

   Encoding: C3"
  (emit-byte #xC3 stream))

(defun emit-call-r64 (reg stream)
  "CALL r/m64 (indirect call through register).

   Encoding: REX.W + FF /2"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 2 reg) stream))

(defun emit-jmp-r64 (reg stream)
  "JMP r/m64 (indirect jump through register).

   Encoding: REX.W + FF /4"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 4 reg) stream))

(defun emit-jmp-rel32 (offset stream)
  "JMP rel32 (near jump).

   Encoding: E9 cd"
  (emit-byte #xE9 stream)
  (emit-dword offset stream))

(defun emit-je-rel32 (offset stream)
  "JE rel32 (jump if equal).

   Encoding: 0F 84 cd"
  (emit-byte #x0F stream)
  (emit-byte #x84 stream)
  (emit-dword offset stream))

;;; Compare with Zero

(defun emit-cmp-ri64 (reg imm stream)
  "CMP reg, imm32 (compare register with 32-bit sign-extended immediate).

   Encoding: REX.W + 81 /7 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 7 reg) stream)
  (emit-dword imm stream))

(defun emit-test-rr64 (reg1 reg2 stream)
  "TEST reg1, reg2 (bitwise AND, set flags, discard result).

   Encoding: REX.W + 85 /r"
  (emit-byte (rex-prefix :w 1 :r (ash reg2 -3) :b (ash reg1 -3)) stream)
  (emit-byte #x85 stream)
  (emit-byte (modrm 3 reg2 reg1) stream))

(defun emit-je-short (offset stream)
  "JE rel8 (short conditional jump if equal/zero). 74 cb"
  (emit-byte #x74 stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-popcnt-rr64 (dst src stream)
  "POPCNT dst, src (64-bit population count).

   Encoding: F3 REX.W 0F B8 /r"
  (emit-byte #xF3 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xB8 stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-bsr-rr64 (dst src stream)
  "BSR dst, src (64-bit bit-scan reverse).

   Encoding: REX.W + 0F BD /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xBD stream)
  (emit-byte (modrm 3 dst src) stream))

