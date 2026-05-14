;;;; packages/emit/src/x86-64-encoding-instrs.lisp - x86-64 Instruction Emitters
;;;
;;; Individual instruction emit functions: MOV, ADD, SUB, IMUL, MUL, CMP, PUSH,
;;; POP, RET, CALL, JMP/JE, TEST, XMM scalar-double
;;; (MOVSD/ADDSD/SUBSD/MULSD/DIVSD), MOVQ, POPCNT, BSR.
;;;
;;; Depends on x86-64-encoding.lisp (emit-byte, emit-dword, emit-qword,
;;; rex-prefix, modrm, sib, scale->sib-bits, %emit-modrm-address,
;;; %emit-modrm-indexed-address, with-output-to-vector).

(in-package :cl-cc/codegen)

;;; ── MOV instructions ─────────────────────────────────────────────────────────

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
  (%emit-modrm-address (x86-64-memory-mod base offset) dst base offset stream))

(defun emit-mov-mr64 (base offset src stream)
  "MOV [base + offset], src (store to memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src base offset stream))

(defun emit-mov-rm64-indexed (dst base index scale offset stream)
  "MOV dst, [base + index*scale + offset] (load from indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                               dst base index scale offset stream))

(defun emit-mov-mr64-indexed (base index scale offset src stream)
  "MOV [base + index*scale + offset], src (store to indexed memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                               src base index scale offset stream))

;;; ── XMM / scalar-double instructions ─────────────────────────────────────────

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

(defun emit-sqrtsd-xx (dst-xmm src-xmm stream)
  "SQRTSD xmm, xmm (scalar double square root)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= src-xmm 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x51 stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

;;; ── Integer arithmetic and compare ──────────────────────────────────────────

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

(defun emit-add-ri32 (reg imm stream)
  "ADD reg, imm32 (sign-extended to 64-bit). REX.W + 81 /0 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 0 reg) stream)
  (emit-dword imm stream))

(defun emit-sub-ri32 (reg imm stream)
  "SUB reg, imm32 (sign-extended to 64-bit). REX.W + 81 /5 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 5 reg) stream)
  (emit-dword imm stream))

(defun emit-imul-rr64 (dst src stream)
  "IMUL dst, src (64-bit signed multiply).

   Encoding: REX.W + 0F AF /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xAF stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-mul-rm64 (src stream)
  "MUL r/m64 (unsigned 64-bit multiply with implicit RAX and RDX:RAX result).

   Encoding: REX.W + F7 /4"
  (emit-byte (rex-prefix :w 1 :b (ash src -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 4 src) stream))

(defun emit-imul-rm64 (src stream)
  "IMUL r/m64 (signed 64-bit multiply with implicit RAX and RDX:RAX result).

   Encoding: REX.W + F7 /5"
  (emit-byte (rex-prefix :w 1 :b (ash src -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 5 src) stream))

(defun emit-cmp-rr64 (op1 op2 stream)
  "CMP op1, op2 (64-bit compare).

   Encoding: REX.W + 39 /r"
  (emit-byte (rex-prefix :w 1 :r (ash op2 -3) :b (ash op1 -3)) stream)
  (emit-byte #x39 stream)
  (emit-byte (modrm 3 op2 op1) stream))

;;; ── Stack, control flow ───────────────────────────────────────────────────────

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

(defun emit-or-mem-rsp-disp32-imm8 (disp imm stream)
  "OR qword ptr [RSP + DISP32], IMM8.

   Used for stack probing: touching one address per guard page with
   OR [RSP-page], 0 preserves memory contents while faulting early if the
   page is not committed. Encoding: REX.W + 83 /1 id ib with RSP SIB."
  (emit-byte (rex-prefix :w 1) stream)
  (emit-byte #x83 stream)
  (emit-byte (modrm 2 1 4) stream)
  (emit-byte (sib 0 4 4) stream)
  (emit-dword disp stream)
  (emit-byte imm stream))

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

(defun emit-jo-rel32 (offset stream)
  "JO rel32 (jump if overflow).

   Encoding: 0F 80 cd"
  (emit-byte #x0F stream)
  (emit-byte #x80 stream)
  (emit-dword offset stream))

;;; ── Immediate compare and test ───────────────────────────────────────────────

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

;;; ── Bit-manipulation instructions ────────────────────────────────────────────

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
