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
      (emit-byte (modrm 0 dst base) stream)
      (progn
        (emit-byte (modrm 1 dst base) stream)
        (emit-byte (logand offset #xFF) stream))))

(defun emit-mov-mr64 (base offset src stream)
  "MOV [base + offset], src (store to memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (if (zerop offset)
      (emit-byte (modrm 0 src base) stream)
      (progn
        (emit-byte (modrm 1 src base) stream)
        (emit-byte (logand offset #xFF) stream))))

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

;;; Integer Division via IDIV
;;;
;;; IDIV r64 requires RDX:RAX as the implicit 128-bit dividend, producing:
;;;   quotient  -> RAX
;;;   remainder -> RDX  (truncate semantics)
;;;
;;; We save/restore RAX and RDX around the instruction and use R11 (scratch)
;;; as an intermediate to hold both rhs and the result. Fixed 21-byte layout:
;;;
;;;   [0  +3] MOV  R11, rhs   -- save divisor to scratch
;;;   [3  +1] PUSH RAX        -- save RAX
;;;   [4  +1] PUSH RDX        -- save RDX
;;;   [5  +3] MOV  RAX, lhs   -- set up dividend in RAX
;;;   [8  +2] CQO             -- sign-extend RAX -> RDX:RAX
;;;   [10 +3] IDIV R11        -- quotient->RAX, remainder->RDX
;;;   [13 +3] MOV  R11, RAX   -- save quotient (or R11,RDX for rem)
;;;   [16 +1] POP  RDX        -- restore RDX
;;;   [17 +1] POP  RAX        -- restore RAX
;;;   (caller moves R11 to dst afterward, +3 bytes)
;;;   Total: 21 bytes (sequence only, not including final MOV dst,R11)

(defun emit-idiv-r11 (stream)
  "IDIV R11 -- 64-bit signed division by R11. REX.W.B + F7 /7 + ModRM"
  (emit-byte #x49 stream)   ; REX.W=1, REX.B=1 (R11 is R/M field register)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 7 3) stream)) ; ModRM: mod=11, reg=7(/7=IDIV), rm=3 (R11&7=3)

(defun emit-cqo (stream)
  "CQO -- sign-extend RAX into RDX:RAX. REX.W + 99"
  (emit-byte #x48 stream)
  (emit-byte #x99 stream))

(defun emit-idiv-sequence (lhs rhs result-is-remainder stream)
  "Emit the full IDIV save/setup/divide/restore sequence.
   RESULT-IS-REMAINDER: if true, dst gets RDX (remainder); else RAX (quotient).
   Caller must do (emit-mov-rr64 dst +r11+ stream) afterward."
  (let ((r11 +r11+))
    (emit-mov-rr64 r11 rhs stream)                          ; [0  +3] MOV R11, rhs
    (emit-push-r64 +rax+ stream)                            ; [3  +1] PUSH RAX
    (emit-push-r64 +rdx+ stream)                            ; [4  +1] PUSH RDX
    (emit-mov-rr64 +rax+ lhs stream)                        ; [5  +3] MOV RAX, lhs
    (emit-cqo stream)                                       ; [8  +2] CQO
    (emit-idiv-r11 stream)                                  ; [10 +3] IDIV R11
    ;; Save the desired result to R11 before restoring RAX/RDX
    (if result-is-remainder
        (emit-mov-rr64 r11 +rdx+ stream)                    ; [13 +3] MOV R11, RDX
        (emit-mov-rr64 r11 +rax+ stream))                   ; [13 +3] MOV R11, RAX
    (emit-pop-r64 +rdx+ stream)                             ; [16 +1] POP RDX
    (emit-pop-r64 +rax+ stream)))                           ; [17 +1] POP RAX
    ;; Caller moves R11 to dst afterward

;;; Shift Operations
;;;
;;; x86-64 variable-count shifts (SAL/SAR) require the count in CL (low byte of RCX).
;;; We save/restore RCX around vm-ash, giving a fixed 24-byte instruction sequence.

(defun emit-sal-r64-cl (reg stream)
  "SAL reg, CL (shift arithmetic left by CL). REX.W + D3 /4"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xD3 stream)
  (emit-byte (modrm 3 4 reg) stream))

(defun emit-sar-r64-cl (reg stream)
  "SAR reg, CL (shift arithmetic right by CL). REX.W + D3 /7"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xD3 stream)
  (emit-byte (modrm 3 7 reg) stream))

(defun emit-ror-r64-cl (reg stream)
  "ROR reg, CL (rotate right by CL). REX.W + D3 /1"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xD3 stream)
  (emit-byte (modrm 3 1 reg) stream))

;;; Immediate Arithmetic (ADD/SUB/AND reg, imm8)

(defun emit-add-ri8 (reg imm stream)
  "ADD reg, imm8 (sign-extended to 64-bit). REX.W + 83 /0 ib"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x83 stream)
  (emit-byte (modrm 3 0 reg) stream)
  (emit-byte (logand imm #xFF) stream))

(defun emit-sub-ri8 (reg imm stream)
  "SUB reg, imm8 (sign-extended to 64-bit). REX.W + 83 /5 ib"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x83 stream)
  (emit-byte (modrm 3 5 reg) stream)
  (emit-byte (logand imm #xFF) stream))

(defun emit-and-ri8 (reg imm stream)
  "AND reg, imm8 (sign-extended to 64-bit). REX.W + 83 /4 ib"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x83 stream)
  (emit-byte (modrm 3 4 reg) stream)
  (emit-byte (logand imm #xFF) stream))

(defun emit-jge-short (offset stream)
  "JGE rel8 (short conditional jump if >=). 7D cb
   OFFSET is byte displacement from end of this instruction."
  (emit-byte #x7D stream)
  (emit-byte (logand offset #xFF) stream))

;;; Conditional Move (CMOVcc)

(defun emit-cmovl-rr64 (dst src stream)
  "CMOVL dst, src -- conditional move if less (signed). REX.W + 0F 4C /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x4C stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-cmovg-rr64 (dst src stream)
  "CMOVG dst, src -- conditional move if greater (signed). REX.W + 0F 4F /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x4F stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-cmovne-rr64 (dst src stream)
  "CMOVNE dst, src -- conditional move if not equal / non-zero. REX.W + 0F 45 /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x45 stream)
  (emit-byte (modrm 3 dst src) stream))

;;; Logical / Bitwise Operations

(defun emit-and-rr64 (dst src stream)
  "AND dst, src (64-bit). REX.W + 21 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x21 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-or-rr64 (dst src stream)
  "OR dst, src (64-bit). REX.W + 09 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x09 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-xor-rr64 (dst src stream)
  "XOR dst, src (64-bit). REX.W + 31 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x31 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-not-r64 (reg stream)
  "NOT reg (bitwise complement 64-bit). REX.W + F7 /2"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 2 reg) stream))

(defun emit-bswap-r32 (reg stream)
  "BSWAP reg32 (byte swap low 32 bits). 0F C8+rd with optional REX.B."
  (when (>= reg 8)
    (emit-byte (rex-prefix :b 1) stream))
  (emit-byte #x0F stream)
  (emit-byte (+ #xC8 (logand reg #x7)) stream))

(defun emit-neg-r64 (reg stream)
  "NEG reg (two's complement negate 64-bit). REX.W + F7 /3"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 3 reg) stream))

(defun emit-dec-r64 (reg stream)
  "DEC reg (64-bit decrement). REX.W + FF /1"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 1 reg) stream))

;;; SETcc + MOVZX helpers for comparison results
;;;
;;; SETcc opcode2 values (second byte after 0F):
;;;   #x94 = SETE   (ZF=1)          #x95 = SETNE  (ZF=0)
;;;   #x9C = SETL   (SF/=OF)        #x9D = SETGE  (SF=OF)
;;;   #x9E = SETLE  (ZF=1|SF/=OF)   #x9F = SETG   (ZF=0 & SF=OF)

(defun emit-setcc (opcode2 reg stream)
  "SETcc reg8: set byte register to 0/1 based on condition flags.
   OPCODE2 is the second byte of SETcc (e.g., #x9C for SETL).
   REG is an x86-64 register code (0-15).
   Requires REX prefix for SIL/DIL (codes 6/7) and R8+ (codes >= 8)."
  (when (>= reg 4)
    (emit-byte (rex-prefix :b (if (>= reg 8) 1 0)) stream))
  (emit-byte #x0F stream)
  (emit-byte opcode2 stream)
  (emit-byte (modrm 3 0 reg) stream))

(defun emit-movzx-r64-r8 (dst src stream)
  "MOVZX dst64, src8 -- zero-extend byte register to 64-bit. REX.W + 0F B6 /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xB6 stream)
  (emit-byte (modrm 3 dst src) stream))
