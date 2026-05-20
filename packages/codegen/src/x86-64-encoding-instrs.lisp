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

(defun emit-mov-rm64-rip (dst disp32 stream)
  "MOV dst, [RIP + disp32] (RIP-relative load).

   Encoding: REX.W + 8B /r with MOD=00, R/M=101, followed by disp32.
   DISP32 is relative to the address immediately after this instruction."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8B stream)
  (%emit-modrm-rip-relative dst disp32 stream))

(defun emit-mov-mr64-rip (disp32 src stream)
  "MOV [RIP + disp32], src (RIP-relative store).

   Encoding: REX.W + 89 /r with MOD=00, R/M=101, followed by disp32.
   DISP32 is relative to the address immediately after this instruction."
  (emit-byte (rex-prefix :w 1 :r (ash src -3)) stream)
  (emit-byte #x89 stream)
  (%emit-modrm-rip-relative src disp32 stream))

(defun emit-mov-rm64-fs-disp32 (dst disp32 stream)
  "MOV dst, FS:[disp32] using absolute disp32 addressing.

   Encoding: 64 REX.W 8B /r (mod=00 r/m=100) SIB(00,100,101) disp32"
  (emit-byte #x64 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8B stream)
  (emit-byte (modrm 0 dst 4) stream)
  (emit-byte (sib 0 4 5) stream)
  (emit-dword disp32 stream))

(defun emit-cmp-rm64-fs-disp32 (reg disp32 stream)
  "CMP reg, FS:[disp32] using absolute disp32 addressing.

   Encoding: 64 REX.W 3B /r (mod=00 r/m=100) SIB(00,100,101) disp32"
  (emit-byte #x64 stream)
  (emit-byte (rex-prefix :w 1 :r (ash reg -3)) stream)
  (emit-byte #x3B stream)
  (emit-byte (modrm 0 reg 4) stream)
  (emit-byte (sib 0 4 5) stream)
  (emit-dword disp32 stream))

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

(defun emit-prefetch-mem (locality base offset stream)
  "Emit PREFETCHT0/PREFETCHNTA [BASE+OFFSET].  LOCALITY is :T0 or :NTA."
  (let ((opcode-ext (ecase locality
                      (:nta 0)
                      (:t0 1))))
    (emit-x86-64-address-rex-if-needed opcode-ext base nil stream)
    (emit-byte #x0F stream)
    (emit-byte #x18 stream)
    (%emit-modrm-address (x86-64-memory-mod base offset)
                         opcode-ext base offset stream)))

(defun emit-prefetch-mem-indexed (locality base index scale offset stream)
  "Emit PREFETCHT0/PREFETCHNTA [BASE+INDEX*SCALE+OFFSET]."
  (let ((opcode-ext (ecase locality
                      (:nta 0)
                      (:t0 1))))
    (emit-x86-64-address-rex-if-needed opcode-ext base index stream)
    (emit-byte #x0F stream)
    (emit-byte #x18 stream)
    (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                                 opcode-ext base index scale offset stream)))

;;; ── LEA instruction (FR-171) ─────────────────────────────────────────────────

(defun emit-lea-rr64 (dst base index stream &optional (scale 1))
  "LEA dst, [base + index*SCALE] — 64-bit load effective address.
SCALE must be 1, 2, 4, or 8.  Uses memory form with SIB byte.
Safety: base=RBP/R13 requires disp8/disp32 with mod=00; index=RSP/R12 is invalid.
The caller (emit-vm-integer-add) ensures base/index come from regalloc which
avoids RBP/R13/RSP/R12 for LEA operands."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8D stream)
  ;; MOD=00, REG=dst, R/M=100 (SIB follows).  Safe because regalloc avoids
  ;; RBP/R13 as base (would need disp8/32) and RSP/R12 as index (invalid).
  (emit-byte (logior (ash dst 3) #x04) stream)
  (emit-byte (sib (scale->sib-bits scale) index base) stream))

(defun emit-lea-rr64-offset (dst base offset stream)
  "LEA dst, [base + OFFSET] — 64-bit load effective address with displacement.
OFFSET must fit in signed 32 bits.  No index register or scale."
  (let ((mod (x86-64-memory-mod base offset)))
    (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash base -3)) stream)
     (emit-byte #x8D stream)
     (%emit-modrm-address mod dst base offset stream)))

(defun emit-lea-rip-relative (dst disp32 stream)
  "LEA dst, [RIP + disp32] — materialize a RIP-relative address."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3)) stream)
  (emit-byte #x8D stream)
  (%emit-modrm-rip-relative dst disp32 stream))

(defun emit-lea-indexed (dst base index scale offset stream)
  "LEA dst, [base + index*SCALE + OFFSET] — full indexed LEA.
SCALE must be 1, 2, 4, or 8.  OFFSET must fit in signed 32 bits."
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :x (ash index -3) :b (ash base -3)) stream)
  (emit-byte #x8D stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                                 dst base index scale offset stream))

(defun emit-lea (dst base index scale offset stream)
  "LEA dst, [base + optional index*SCALE + OFFSET].

Encodes REX.W + 8D /r with ModR/M, optional SIB, and disp8/disp32.
SCALE must be one of 1, 2, 4, or 8 when INDEX is non-NIL."
  (if index
      (emit-lea-indexed dst base index scale offset stream)
      (emit-lea-rr64-offset dst base offset stream)))

;;; ── XMM / scalar-double instructions ─────────────────────────────────────────

(defun emit-sse-prefix-rex-if-needed (reg rm stream &optional index)
  "Emit an SSE REX prefix when REG/RM/INDEX use high architectural registers."
  (let ((r (if (>= reg 8) 1 0))
        (x (if (and index (>= index 8)) 1 0))
        (b (if (>= rm 8) 1 0)))
    (when (or (= r 1) (= x 1) (= b 1))
      (emit-byte (rex-prefix :r r :x x :b b) stream))))

(defun emit-sse66-0f-xx (opcode dst-xmm src-xmm stream)
  "Emit a 66 0F /r XMM register instruction."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm src-xmm stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-sse66-0f-xm (opcode dst-xmm base offset stream)
  "Emit a 66 0F /r XMM load-like instruction with a memory source."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-sse66-0f-mx (opcode base offset src-xmm stream)
  "Emit a 66 0F /r XMM store-like instruction with a memory destination."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed src-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

(defun emit-sse66-0f38-xx (opcode dst-xmm src-xmm stream)
  "Emit a 66 0F 38 /r XMM register instruction."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm src-xmm stream)
  (emit-byte #x0F stream)
  (emit-byte #x38 stream)
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-xmm src-xmm) stream))

(defun emit-sse66-0f38-xm (opcode dst-xmm base offset stream)
  "Emit a 66 0F 38 /r XMM instruction with a memory source."
  (emit-byte #x66 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte #x38 stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defmacro define-sse66-0f-xmm-op (name opcode description)
  "Define register and memory-source forms for a 66 0F XMM operation."
  (let ((rr-name (intern (format nil "EMIT-~A-XX" name)))
        (rm-name (intern (format nil "EMIT-~A-XM" name))))
    `(progn
       (defun ,rr-name (dst-xmm src-xmm stream)
         ,description
         (emit-sse66-0f-xx ,opcode dst-xmm src-xmm stream))
       (defun ,rm-name (dst-xmm base offset stream)
         ,description
         (emit-sse66-0f-xm ,opcode dst-xmm base offset stream)))))

(defmacro define-sse66-0f38-xmm-op (name opcode description)
  "Define register and memory-source forms for a 66 0F 38 XMM operation."
  (let ((rr-name (intern (format nil "EMIT-~A-XX" name)))
        (rm-name (intern (format nil "EMIT-~A-XM" name))))
    `(progn
       (defun ,rr-name (dst-xmm src-xmm stream)
         ,description
         (emit-sse66-0f38-xx ,opcode dst-xmm src-xmm stream))
       (defun ,rm-name (dst-xmm base offset stream)
         ,description
         (emit-sse66-0f38-xm ,opcode dst-xmm base offset stream)))))

(defun emit-movdqa-xx (dst-xmm src-xmm stream)
  "MOVDQA xmm, xmm (aligned packed 128-bit copy)."
  (emit-sse66-0f-xx #x6F dst-xmm src-xmm stream))

(defun emit-movdqa-xm (dst-xmm base offset stream)
  "MOVDQA xmm, [base + offset] (aligned packed 128-bit load)."
  (emit-sse66-0f-xm #x6F dst-xmm base offset stream))

(defun emit-movdqa-mx (base offset src-xmm stream)
  "MOVDQA [base + offset], xmm (aligned packed 128-bit store)."
  (emit-sse66-0f-mx #x7F base offset src-xmm stream))

(defun emit-ssef3-0f-xm (opcode dst-xmm base offset stream)
  "Emit an F3 0F /r XMM load-like instruction with a memory source."
  (emit-byte #xF3 stream)
  (emit-sse-prefix-rex-if-needed dst-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-ssef3-0f-mx (opcode base offset src-xmm stream)
  "Emit an F3 0F /r XMM store-like instruction with a memory destination."
  (emit-byte #xF3 stream)
  (emit-sse-prefix-rex-if-needed src-xmm base stream)
  (emit-byte #x0F stream)
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

(defun emit-movdqu-xm (dst-xmm base offset stream)
  "MOVDQU xmm, [base + offset] (unaligned packed 128-bit load)."
  (emit-ssef3-0f-xm #x6F dst-xmm base offset stream))

(defun emit-movdqu-mx (base offset src-xmm stream)
  "MOVDQU [base + offset], xmm (unaligned packed 128-bit store)."
  (emit-ssef3-0f-mx #x7F base offset src-xmm stream))

(define-sse66-0f-xmm-op paddd #xFE
  "PADDD xmm, xmm/m128 (packed signed/unsigned dword add).")
(define-sse66-0f-xmm-op psubd #xFA
  "PSUBD xmm, xmm/m128 (packed dword subtract).")
(define-sse66-0f-xmm-op pxor #xEF
  "PXOR xmm, xmm/m128 (packed bitwise xor).")
(define-sse66-0f-xmm-op pand #xDB
  "PAND xmm, xmm/m128 (packed bitwise and).")
(define-sse66-0f-xmm-op por #xEB
  "POR xmm, xmm/m128 (packed bitwise or).")

(define-sse66-0f38-xmm-op pmulld #x40
  "PMULLD xmm, xmm/m128 (SSE4.1 packed signed dword multiply).")
(define-sse66-0f38-xmm-op pblendvb #x10
  "PBLENDVB xmm, xmm/m128 (SSE4.1 variable byte blend using XMM0 mask).")
(define-sse66-0f38-xmm-op pminsd #x39
  "PMINSD xmm, xmm/m128 (SSE4.1 packed signed dword minimum).")
(define-sse66-0f38-xmm-op pmaxsd #x3D
  "PMAXSD xmm, xmm/m128 (SSE4.1 packed signed dword maximum).")

(defun emit-avx2-vex-yyy (opcode map dst-ymm src1-ymm src2-ymm stream)
  "Emit an AVX2 VEX.256.66 MAP.W0 register instruction."
  (emit-vex-prefix stream
                   :map map
                   :w 0
                   :vvvv src1-ymm
                   :l 1
                   :pp +vex-pp-66+
                   :r (ash dst-ymm -3)
                   :b (ash src2-ymm -3))
  (emit-byte opcode stream)
  (emit-byte (modrm 3 dst-ymm src2-ymm) stream))

(defun emit-avx2-vex-yym (opcode map dst-ymm src1-ymm base offset stream)
  "Emit an AVX2 VEX.256.66 MAP.W0 instruction with a memory source."
  (emit-vex-prefix stream
                   :map map
                   :w 0
                   :vvvv src1-ymm
                   :l 1
                   :pp +vex-pp-66+
                   :r (ash dst-ymm -3)
                   :b (ash base -3))
  (emit-byte opcode stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-ymm base offset stream))

(defun emit-vpaddd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPADDD ymm, ymm, ymm (AVX2 packed dword add)."
  (emit-avx2-vex-yyy #xFE +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpaddd-yym (dst-ymm src1-ymm base offset stream)
  "VPADDD ymm, ymm, [base + offset] (AVX2 packed dword add)."
  (emit-avx2-vex-yym #xFE +vex-map-0f+ dst-ymm src1-ymm base offset stream))

(defun emit-vpsubd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPSUBD ymm, ymm, ymm (AVX2 packed dword subtract)."
  (emit-avx2-vex-yyy #xFA +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpand-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPAND ymm, ymm, ymm (AVX2 packed bitwise and)."
  (emit-avx2-vex-yyy #xDB +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpor-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPOR ymm, ymm, ymm (AVX2 packed bitwise or)."
  (emit-avx2-vex-yyy #xEB +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpxor-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPXOR ymm, ymm, ymm (AVX2 packed bitwise xor)."
  (emit-avx2-vex-yyy #xEF +vex-map-0f+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpmulld-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPMULLD ymm, ymm, ymm (AVX2 packed signed dword multiply)."
  (emit-avx2-vex-yyy #x40 +vex-map-0f38+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpmulld-yym (dst-ymm src1-ymm base offset stream)
  "VPMULLD ymm, ymm, [base + offset] (AVX2 packed signed dword multiply)."
  (emit-avx2-vex-yym #x40 +vex-map-0f38+ dst-ymm src1-ymm base offset stream))

(defun emit-vmovdqu-ym (dst-ymm base offset stream)
  "VMOVDQU ymm, [base + offset] (AVX unaligned 256-bit load)."
  (emit-vex-prefix stream
                   :map +vex-map-0f+ :w 0 :vvvv #xF :l 1 :pp +vex-pp-f3+
                   :r (ash dst-ymm -3) :b (ash base -3))
  (emit-byte #x6F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-ymm base offset stream))

(defun emit-vmovdqu-my (base offset src-ymm stream)
  "VMOVDQU [base + offset], ymm (AVX unaligned 256-bit store)."
  (emit-vex-prefix stream
                   :map +vex-map-0f+ :w 0 :vvvv #xF :l 1 :pp +vex-pp-f3+
                   :r (ash src-ymm -3) :b (ash base -3))
  (emit-byte #x7F stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-ymm base offset stream))

(defun emit-vpermd-yyy (dst-ymm src1-ymm src2-ymm stream)
  "VPERMD ymm, ymm, ymm (AVX2 permute dwords)."
  (emit-avx2-vex-yyy #x36 +vex-map-0f38+ dst-ymm src1-ymm src2-ymm stream))

(defun emit-vpermd-yym (dst-ymm src1-ymm base offset stream)
  "VPERMD ymm, ymm, [base + offset] (AVX2 permute dwords)."
  (emit-avx2-vex-yym #x36 +vex-map-0f38+ dst-ymm src1-ymm base offset stream))

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

(defun emit-movsd-xm (dst-xmm base offset stream)
  "MOVSD xmm, [base + offset] (scalar double load)."
  (emit-byte #xF2 stream)
  (when (or (>= dst-xmm 8) (>= base 8))
    (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash base -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x10 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) dst-xmm base offset stream))

(defun emit-movsd-mx (base offset src-xmm stream)
  "MOVSD [base + offset], xmm (scalar double store)."
  (emit-byte #xF2 stream)
  (when (or (>= src-xmm 8) (>= base 8))
    (emit-byte (rex-prefix :r (ash src-xmm -3) :b (ash base -3)) stream))
  (emit-byte #x0F stream)
  (emit-byte #x11 stream)
  (%emit-modrm-address (x86-64-memory-mod base offset) src-xmm base offset stream))

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

(defun emit-vfmadd132sd-xxx (dst-xmm add-xmm mul-xmm stream)
  "VFMADD132SD dst, add, mul: dst = dst * mul + add."
  (let* ((r (logand (ash dst-xmm -3) 1))
         (b (logand (ash mul-xmm -3) 1))
         (vvvv (logxor add-xmm #xF)))
    ;; VEX.DDS.LIG.66.0F38.W1 99 /r
    (emit-byte #xC4 stream)
    (emit-byte (logior (ash (logxor r 1) 7)
                       #x40
                       (ash (logxor b 1) 5)
                       #x02)
               stream)
    (emit-byte (logior #x80 (ash vvvv 3) #x01) stream)
    (emit-byte #x99 stream)
    (emit-byte (modrm 3 dst-xmm mul-xmm) stream)))

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

(defun emit-div-rm64 (src stream)
  "DIV r/m64 (unsigned 64-bit divide RDX:RAX by SRC).

   Encoding: REX.W + F7 /6. Quotient is written to RAX, remainder to RDX."
  (emit-byte (rex-prefix :w 1 :b (ash src -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 6 src) stream))

(defun emit-idiv-rm64 (src stream)
  "IDIV r/m64 (signed 64-bit divide RDX:RAX by SRC).

   Encoding: REX.W + F7 /7. Quotient is written to RAX, remainder to RDX."
  (emit-byte (rex-prefix :w 1 :b (ash src -3)) stream)
  (emit-byte #xF7 stream)
  (emit-byte (modrm 3 7 src) stream))

(defun emit-cmp-rr64 (op1 op2 stream)
  "CMP op1, op2 (64-bit compare).

   Encoding: REX.W + 39 /r"
  (emit-byte (rex-prefix :w 1 :r (ash op2 -3) :b (ash op1 -3)) stream)
  (emit-byte #x39 stream)
  (emit-byte (modrm 3 op2 op1) stream))

;;; ── Stack, control flow ───────────────────────────────────────────────────────

(defun emit-push-r64 (reg stream)
  "PUSH reg (64-bit).

   Encoding: [REX.B] 50+ rd. REX.B (#x41) required for R8-R15."
  (when (>= reg 8)
    (emit-byte #x41 stream))
  (emit-byte (+ #x50 (logand reg #x7)) stream))

(defun emit-pop-r64 (reg stream)
  "POP reg (64-bit).

   Encoding: [REX.B] 58+ rd. REX.B (#x41) required for R8-R15."
  (when (>= reg 8)
    (emit-byte #x41 stream))
  (emit-byte (+ #x58 (logand reg #x7)) stream))

(defun emit-leave (stream)
  "LEAVE (restore frame pointer): equivalent to MOV RSP,RBP; POP RBP.

   Encoding: C9"
  (emit-byte #xC9 stream))

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

(defun emit-jno-rel32 (offset stream)
  "JNO rel32 (jump if not overflow).

   Encoding: 0F 81 cd"
  (emit-byte #x0F stream)
  (emit-byte #x81 stream)
  (emit-dword offset stream))

;;; ── Immediate compare and test ───────────────────────────────────────────────

(defun emit-cmp-ri64 (reg imm stream)
  "CMP reg, imm32 (compare register with 32-bit sign-extended immediate).

   Encoding: REX.W + 81 /7 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 7 reg) stream)
  (emit-dword imm stream))

(defun emit-cmp-ri32 (reg imm stream)
  "CMP reg32, imm32.

   Encoding: 81 /7 id (+ optional REX.B for reg >= 8)."
  (when (>= reg 8)
    (emit-byte (rex-prefix :b (ash reg -3)) stream))
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

;; FR-403: Short jump encoders for branch displacement optimization

(defun emit-jmp-rel8 (offset stream)
  "JMP rel8 (short unconditional jump). EB cb"
  (emit-byte #xEB stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jne-rel8 (offset stream)
  "JNE rel8 (short jump if not equal/not zero). 75 cb"
  (emit-byte #x75 stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jns-rel8 (offset stream)
  "JNS rel8 (short jump if not sign / positive). 79 cb"
  (emit-byte #x79 stream)
  (emit-byte (logand offset #xFF) stream))

(defun emit-jge-rel8 (offset stream)
  "JGE rel8 (short jump if greater or equal, signed). 7D cb"
  (emit-byte #x7D stream)
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

(defun emit-lzcnt-rr64 (dst src stream)
  "LZCNT dst, src (64-bit leading-zero count).

   Encoding: F3 REX.W 0F BD /r.  This opcode is BSR with an F3 prefix on
   processors without LZCNT support."
  (emit-byte #xF3 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xBD stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-vex-0f38-rvm64 (opcode pp dst src1 src2 stream)
  "Emit a VEX.NDS.LZ.0F38.W1 three-register GPR instruction.

   DST is encoded in ModR/M.reg, SRC1 in inverted VEX.vvvv, and SRC2 in
   ModR/M.r/m.  PP is the mandatory-prefix field: 0=none, 2=F3, 3=F2."
  (let* ((r (logand (ash dst -3) 1))
         (b (logand (ash src2 -3) 1))
         (vvvv (logxor src1 #xF)))
    (emit-byte #xC4 stream)
    (emit-byte (logior (ash (logxor r 1) 7)
                       #x40
                       (ash (logxor b 1) 5)
                       #x02)
               stream)
    (emit-byte (logior #x80 (ash (logand vvvv #xF) 3) pp) stream)
    (emit-byte opcode stream)
    (emit-byte (modrm 3 dst src2) stream)))

(defun emit-bextr-rrr64 (dst src control stream)
  "BEXTR dst, src, control (64-bit bit-field extract).

   Encoding: VEX.NDS.LZ.0F38.W1 F7 /r.  CONTROL encodes START in bits 7:0 and
   LENGTH in bits 15:8."
  (emit-vex-0f38-rvm64 #xF7 0 dst control src stream))

(defun emit-pext-rrr64 (dst src mask stream)
  "PEXT dst, src, mask (64-bit parallel bit extract).

   Encoding: VEX.NDS.LZ.F3.0F38.W1 F5 /r."
  (emit-vex-0f38-rvm64 #xF5 2 dst src mask stream))

(defun emit-pdep-rrr64 (dst src mask stream)
  "PDEP dst, src, mask (64-bit parallel bit deposit).

   Encoding: VEX.NDS.LZ.F2.0F38.W1 F5 /r."
  (emit-vex-0f38-rvm64 #xF5 3 dst src mask stream))

(defun emit-bsr-rr64 (dst src stream)
  "BSR dst, src (64-bit bit-scan reverse).

   Encoding: REX.W + 0F BD /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xBD stream)
  (emit-byte (modrm 3 dst src) stream))
