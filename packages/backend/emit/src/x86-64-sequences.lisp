;;;; packages/backend/emit/src/x86-64-sequences.lisp - x86-64 Complex Instruction Sequences
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: emit-idiv-r11, emit-cqo, emit-idiv-sequence (IDIV with
;;; RAX/RDX save-restore), emit-sal-r64-cl, emit-sar-r64-cl, emit-ror-r64-cl,
;;; emit-add-ri8, emit-sub-ri8, emit-and-ri8, emit-jge-short,
;;; emit-cmovl-rr64, emit-cmovg-rr64, emit-cmovne-rr64,
;;; emit-and-rr64, emit-or-rr64, emit-xor-rr64, emit-not-r64,
;;; emit-bswap-r32, emit-neg-r64, emit-dec-r64,
;;; emit-setcc, emit-movzx-r64-r8.
;;;
;;; Basic encoding primitives (register constants, REX/ModR/M helpers,
;;; MOV, arithmetic, PUSH/POP, BSR) are in x86-64-encoding.lisp (loads before).
;;;
;;; Load order: after x86-64-encoding.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/emit)

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
