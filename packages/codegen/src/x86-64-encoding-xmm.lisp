;;;; packages/codegen/src/x86-64-encoding-xmm.lisp - XMM / SSE / AVX Instruction Emitters
;;;
;;; SSE, SSE2, SSE4.1, AVX2, VEX, EVEX, scalar-double, and FMA instruction
;;; emit functions for the x86-64 backend.
;;;
;;; Depends on x86-64-encoding-instrs (emit-byte, modrm, rex-prefix,
;;; %emit-modrm-address, emit-vex-prefix, emit-evex-prefix, x86-64-memory-mod).

(in-package :cl-cc/codegen)

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

(defmacro define-avx2-vex-yyy-op (name opcode map description)
  "Define emit-NAME-yyy for an AVX2 VEX.256.66 register-register-register operation."
  (let ((fn-name (intern (format nil "EMIT-~A-YYY" name))))
    `(defun ,fn-name (dst-ymm src1-ymm src2-ymm stream)
       ,description
       (emit-avx2-vex-yyy ,opcode ,map dst-ymm src1-ymm src2-ymm stream))))

(defmacro define-avx2-vex-yym-op (name opcode map description)
  "Define emit-NAME-yym for an AVX2 VEX.256.66 register-register-memory operation."
  (let ((fn-name (intern (format nil "EMIT-~A-YYM" name))))
    `(defun ,fn-name (dst-ymm src1-ymm base offset stream)
       ,description
       (emit-avx2-vex-yym ,opcode ,map dst-ymm src1-ymm base offset stream))))

(define-avx2-vex-yyy-op vpaddd  #xFE +vex-map-0f+   "VPADDD ymm, ymm, ymm (AVX2 packed dword add).")
(define-avx2-vex-yyy-op vpsubd  #xFA +vex-map-0f+   "VPSUBD ymm, ymm, ymm (AVX2 packed dword subtract).")
(define-avx2-vex-yyy-op vpand   #xDB +vex-map-0f+   "VPAND ymm, ymm, ymm (AVX2 packed bitwise and).")
(define-avx2-vex-yyy-op vpor    #xEB +vex-map-0f+   "VPOR ymm, ymm, ymm (AVX2 packed bitwise or).")
(define-avx2-vex-yyy-op vpxor   #xEF +vex-map-0f+   "VPXOR ymm, ymm, ymm (AVX2 packed bitwise xor).")
(define-avx2-vex-yyy-op vpmulld #x40 +vex-map-0f38+ "VPMULLD ymm, ymm, ymm (AVX2 packed signed dword multiply).")

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

(defun emit-movq-xmm-r64 (dst-xmm src-gpr stream)
  "MOVQ xmm, r64 (copy raw 64 bits from GPR to XMM low lane)."
  (emit-byte #x66 stream)
  (emit-byte (rex-prefix :w 1 :r (ash dst-xmm -3) :b (ash src-gpr -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #x6E stream)
  (emit-byte (modrm 3 dst-xmm src-gpr) stream))

;;; Scalar-double XMM register-register emitters.
;;; Data: (name opcode description). Logic: F2 + optional REX + 0F + opcode + ModRM.
(defmacro define-scalar-double-xx-op (name opcode description)
  "Generate emit-NAME-xx for an F2-prefixed scalar-double register-register XMM op."
  (let ((fn-name (intern (format nil "EMIT-~A-XX" name))))
    `(defun ,fn-name (dst-xmm src-xmm stream)
       ,description
       (emit-byte #xF2 stream)
       (when (or (>= dst-xmm 8) (>= src-xmm 8))
         (emit-byte (rex-prefix :r (ash dst-xmm -3) :b (ash src-xmm -3)) stream))
       (emit-byte #x0F stream)
       (emit-byte ,opcode stream)
       (emit-byte (modrm 3 dst-xmm src-xmm) stream))))

(define-scalar-double-xx-op movsd  #x10 "MOVSD xmm, xmm (scalar double copy).")
(define-scalar-double-xx-op addsd  #x58 "ADDSD xmm, xmm (scalar double add).")
(define-scalar-double-xx-op subsd  #x5C "SUBSD xmm, xmm (scalar double subtract).")
(define-scalar-double-xx-op mulsd  #x59 "MULSD xmm, xmm (scalar double multiply).")
(define-scalar-double-xx-op divsd  #x5E "DIVSD xmm, xmm (scalar double divide).")
(define-scalar-double-xx-op sqrtsd #x51 "SQRTSD xmm, xmm (scalar double square root).")

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
