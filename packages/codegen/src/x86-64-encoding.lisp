;;;; packages/emit/src/x86-64-encoding.lisp - x86-64 Encoding Primitives
;;;
;;; Pure ISA-level encoding layer: register constants, output stream utilities,
;;; REX prefix, ModR/M, SIB encoding, and modrm-address helpers.
;;;
;;; Individual instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.)
;;; are in x86-64-encoding-instrs.lisp (loaded immediately after this file).

(in-package :cl-cc/codegen)

(defparameter *x86-64-stack-clash-protection-enabled* nil
  "When true, emit destructive x86-64 stack-clash probe steps during frame setup.")

(defparameter *x86-64-safe-stack-enabled* nil
  "When true, emit experimental x86-64 SafeStack TLS shadow-stack-pointer sequences.")

(defparameter *x86-64-avx512-enabled* nil
  "When true, AVX-512 EVEX emitters may be selected by higher-level lowering.")

(defparameter *x86-64-apx-enabled* nil
  "When true, experimental APX NDD/NF encoders may be selected.")

(defparameter *x86-64-atomics-enabled* nil
  "When true, VM atomic operations are lowered to x86-64 locked instructions.")

(defparameter *amx-enabled* nil
  "When true, experimental Intel AMX tile encoders may be selected.")

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

;; APX extended GPR register numbers.  Legacy REX encoders cannot address these;
;; APX helpers below keep them gated behind *X86-64-APX-ENABLED*.
(defconstant +r16+ 16) (defconstant +r17+ 17) (defconstant +r18+ 18) (defconstant +r19+ 19)
(defconstant +r20+ 20) (defconstant +r21+ 21) (defconstant +r22+ 22) (defconstant +r23+ 23)
(defconstant +r24+ 24) (defconstant +r25+ 25) (defconstant +r26+ 26) (defconstant +r27+ 27)
(defconstant +r28+ 28) (defconstant +r29+ 29) (defconstant +r30+ 30) (defconstant +r31+ 31)

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

;; YMM register encodings share the same low 4-bit register ids as XMM.
(defconstant +ymm0+ 0)
(defconstant +ymm1+ 1)
(defconstant +ymm2+ 2)
(defconstant +ymm3+ 3)
(defconstant +ymm4+ 4)
(defconstant +ymm5+ 5)
(defconstant +ymm6+ 6)
(defconstant +ymm7+ 7)
(defconstant +ymm8+ 8)
(defconstant +ymm9+ 9)
(defconstant +ymm10+ 10)
(defconstant +ymm11+ 11)
(defconstant +ymm12+ 12)
(defconstant +ymm13+ 13)
(defconstant +ymm14+ 14)
(defconstant +ymm15+ 15)

;; AVX-512 architectural register ids.
(defconstant +zmm0+ 0) (defconstant +zmm1+ 1) (defconstant +zmm2+ 2) (defconstant +zmm3+ 3)
(defconstant +zmm4+ 4) (defconstant +zmm5+ 5) (defconstant +zmm6+ 6) (defconstant +zmm7+ 7)
(defconstant +zmm8+ 8) (defconstant +zmm9+ 9) (defconstant +zmm10+ 10) (defconstant +zmm11+ 11)
(defconstant +zmm12+ 12) (defconstant +zmm13+ 13) (defconstant +zmm14+ 14) (defconstant +zmm15+ 15)
(defconstant +zmm16+ 16) (defconstant +zmm17+ 17) (defconstant +zmm18+ 18) (defconstant +zmm19+ 19)
(defconstant +zmm20+ 20) (defconstant +zmm21+ 21) (defconstant +zmm22+ 22) (defconstant +zmm23+ 23)
(defconstant +zmm24+ 24) (defconstant +zmm25+ 25) (defconstant +zmm26+ 26) (defconstant +zmm27+ 27)
(defconstant +zmm28+ 28) (defconstant +zmm29+ 29) (defconstant +zmm30+ 30) (defconstant +zmm31+ 31)

(defconstant +k0+ 0) (defconstant +k1+ 1) (defconstant +k2+ 2) (defconstant +k3+ 3)
(defconstant +k4+ 4) (defconstant +k5+ 5) (defconstant +k6+ 6) (defconstant +k7+ 7)

;; Intel AMX tile register ids (TMM0-TMM7).  These are inert unless
;; *AMX-ENABLED* or host feature detection enables AMX lowering.
(defconstant +tmm0+ 0) (defconstant +tmm1+ 1) (defconstant +tmm2+ 2) (defconstant +tmm3+ 3)
(defconstant +tmm4+ 4) (defconstant +tmm5+ 5) (defconstant +tmm6+ 6) (defconstant +tmm7+ 7)

;; Calling convention (System V AMD64 ABI)
;; Arguments: RDI, RSI, RDX, RCX, R8, R9
;; Return: RAX
;; Preserved: RBX, RBP, R12-R15
;; Temp: RAX, RCX, RDX, RSI, RDI, R8-R11

;;; Output Stream Utilities

(defmacro with-output-to-vector ((stream-var) &body body)
  "Execute BODY with STREAM-VAR bound to an output stream that collects bytes into a vector."
  `(let ((bytes '()))
      (flet ((stream-write-byte (byte)
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

(defun encode-sib (scale index-reg base-reg)
  "Return a SIB byte for [BASE-REG + INDEX-REG*SCALE].

SCALE is the addressing scale factor, not the encoded bit field, and must be
one of 1, 2, 4, or 8.  INDEX-REG and BASE-REG are x86-64 register codes; their
low three bits are encoded in the SIB byte while REX.X/REX.B are emitted by the
instruction emitter.  RSP/R12 are not encodable as SIB index registers."
  (let ((index-rm (logand index-reg #x7)))
    (when (= index-rm 4)
      (error "RSP/R12 cannot be used as SIB index register"))
    (sib (scale->sib-bits scale) index-rm (logand base-reg #x7))))

(defun x86-64-memory-mod (base offset)
  "Return the ModR/M MOD field for [BASE + OFFSET].

   MOD=00 cannot encode RBP/R13 as a plain base register, so those bases use
   disp8 even for offset zero. Non-zero offsets use disp8 when they fit in a
   signed byte and disp32 otherwise."
  (let ((base-rm (logand base #x7)))
    (cond
      ((and (zerop offset) (not (= base-rm 5))) 0)
      ((typep offset '(signed-byte 8)) 1)
      (t 2))))

(defun %emit-modrm-address (mod reg base offset stream)
  "Emit ModR/M (+ SIB if needed) for [BASE + OFFSET].

   x86-64 requires an explicit SIB byte when BASE is RSP/R12."
  (let ((rm (logand base #x7)))
    (emit-byte (modrm mod reg (if (= rm 4) 4 rm)) stream)
    (when (= rm 4)
      ;; scale=1, no index, base=RSP/R12
      (emit-byte (sib 0 4 4) stream))
    (case mod
      (1 (emit-byte (logand offset #xFF) stream))
      (2 (emit-dword offset stream)))))

(defun %emit-modrm-rip-relative (reg disp32 stream)
  "Emit ModR/M + disp32 for RIP-relative [RIP + DISP32].

   RIP-relative memory operands use MOD=00 and R/M=101.  The displacement is
   relative to the next instruction, so callers must pass a precomputed disp32."
  (emit-byte (modrm 0 reg 5) stream)
  (emit-dword disp32 stream))

(defun %emit-modrm-indexed-address (mod reg base index scale offset stream)
  "Emit ModR/M+SIB for [BASE + INDEX*SCALE + OFFSET]."
  (let ((base-rm (logand base #x7)))
    (emit-byte (modrm mod reg 4) stream)
    (emit-byte (encode-sib scale index base-rm) stream)
    (case mod
      (1 (emit-byte (logand offset #xFF) stream))
      (2 (emit-dword offset stream)))))

(defun x86-64-memory-address-byte-size (base offset &key index)
  "Return byte count for ModR/M (+ optional SIB/displacement) memory operand."
  (let* ((mod (x86-64-memory-mod base offset))
         (sib-p (or index (= (logand base #x7) 4))))
    (+ 1
       (if sib-p 1 0)
       (ecase mod
         (0 0)
          (1 1)
          (2 4)))))

(defun x86-64-rip-relative-address-byte-size ()
  "Return byte count for RIP-relative ModR/M + disp32 addressing."
  5)

(defun emit-x86-64-address-rex-if-needed (reg base index stream)
  "Emit an address-only REX prefix when high registers appear in REG/BASE/INDEX."
  (let ((r (if (>= reg 8) 1 0))
        (x (if (and index (>= index 8)) 1 0))
        (b (if (>= base 8) 1 0)))
    (when (or (= r 1) (= x 1) (= b 1))
      (emit-byte (rex-prefix :r r :x x :b b) stream))))

;;; VEX Prefix

(defconstant +vex-map-0f+ #b00001
  "VEX opcode-map selector for 0F opcodes.")

(defconstant +vex-map-0f38+ #b00010
  "VEX opcode-map selector for 0F 38 opcodes.")

(defconstant +vex-pp-none+ #b00
  "VEX mandatory-prefix selector: none.")

(defconstant +vex-pp-66+ #b01
  "VEX mandatory-prefix selector: 66.")

(defconstant +vex-pp-f3+ #b10
  "VEX mandatory-prefix selector: F3.")

(defconstant +vex-pp-f2+ #b11
  "VEX mandatory-prefix selector: F2.")

(defun emit-vex-prefix (stream &key (map +vex-map-0f+) (w 0) (vvvv #xF)
                               (l 0) (pp +vex-pp-none+) (r 0) (x 0) (b 0))
  "Emit a shortest valid VEX prefix.

R/X/B are non-inverted extension bits. VVVV is the non-inverted first source
register id; the VEX prefix stores its one's-complement low 4 bits. The 2-byte
form is used for VEX.0F.W0 encodings that do not need X/B extensions."
  (let ((vvvv* (logand (logxor vvvv #xF) #xF)))
    (if (and (= map +vex-map-0f+) (zerop w) (zerop x) (zerop b))
        (progn
          (emit-byte #xC5 stream)
          (emit-byte (logior (ash (logxor (logand r 1) 1) 7)
                             (ash vvvv* 3)
                             (ash (logand l 1) 2)
                             (logand pp #x3))
                     stream))
        (progn
          (emit-byte #xC4 stream)
          (emit-byte (logior (ash (logxor (logand r 1) 1) 7)
                             (ash (logxor (logand x 1) 1) 6)
                             (ash (logxor (logand b 1) 1) 5)
                             (logand map #x1F))
                     stream)
          (emit-byte (logior (ash (logand w 1) 7)
                             (ash vvvv* 3)
                             (ash (logand l 1) 2)
                             (logand pp #x3))
                      stream)))))

;;; EVEX prefix (AVX-512)

(defconstant +evex-map-0f+ #b00001)
(defconstant +evex-map-0f38+ #b00010)
(defconstant +evex-map-0f3a+ #b00011)

(defun emit-evex-prefix (stream &key (map +evex-map-0f+) (w 0) (vvvv #xF)
                                (pp +vex-pp-none+) (r 0) (x 0) (b 0) (r2 0)
                                (aaa 0) (z 0) (ll 2) (bb 0) (v2 1))
  "Emit an EVEX prefix.  R/X/B/R2 are non-inverted extension bits."
  (emit-byte #x62 stream)
  (emit-byte (logior (ash (logxor (logand r 1) 1) 7)
                     (ash (logxor (logand x 1) 1) 6)
                     (ash (logxor (logand b 1) 1) 5)
                     (ash (logxor (logand r2 1) 1) 4)
                     (logand map #x3))
             stream)
  (emit-byte (logior (ash (logand w 1) 7)
                     (ash (logand (logxor vvvv #xF) #xF) 3)
                     4
                     (logand pp #x3))
             stream)
  (emit-byte (logior (ash (logand z 1) 7)
                     (ash (logand ll #x3) 5)
                     (ash (logand bb 1) 4)
                     (ash (logand v2 1) 3)
                      (logand aaa #x7))
              stream))

;;; Intel AMX representative encoders (FR-571)

(defun %x86-64-amx-check-tmm (reg context)
  "Validate an AMX tile register id for CONTEXT."
  (unless (and (integerp reg) (<= 0 reg 7))
    (error "~A requires TMM0-TMM7, got ~S" context reg))
  reg)

(defun emit-tileloadd (dst-tmm base offset stream &key (index +rax+) (scale 1))
  "Emit TILELOADD DST-TMM,[BASE+INDEX*SCALE+OFFSET] representative encoding.

The encoder is present for feature plumbing but is selected only when AMX is
enabled by higher-level code."
  (%x86-64-amx-check-tmm dst-tmm "TILELOADD")
  (emit-vex-prefix stream :map +vex-map-0f38+ :pp +vex-pp-f2+
                   :vvvv index :b (if (>= base 8) 1 0)
                   :x (if (>= index 8) 1 0))
  (emit-byte #x4B stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                               dst-tmm base index scale offset stream))

(defun emit-tilestored (src-tmm base offset stream &key (index +rax+) (scale 1))
  "Emit TILESTORED [BASE+INDEX*SCALE+OFFSET],SRC-TMM representative encoding."
  (%x86-64-amx-check-tmm src-tmm "TILESTORED")
  (emit-vex-prefix stream :map +vex-map-0f38+ :pp +vex-pp-66+
                   :vvvv index :b (if (>= base 8) 1 0)
                   :x (if (>= index 8) 1 0))
  (emit-byte #x4B stream)
  (%emit-modrm-indexed-address (x86-64-memory-mod base offset)
                               src-tmm base index scale offset stream))

(defun emit-tdpbssd (dst-tmm lhs-tmm rhs-tmm stream)
  "Emit TDPBSSD DST-TMM,LHS-TMM,RHS-TMM representative register encoding."
  (%x86-64-amx-check-tmm dst-tmm "TDPBSSD destination")
  (%x86-64-amx-check-tmm lhs-tmm "TDPBSSD left source")
  (%x86-64-amx-check-tmm rhs-tmm "TDPBSSD right source")
  (emit-vex-prefix stream :map +vex-map-0f38+ :pp +vex-pp-f2+ :vvvv lhs-tmm)
  (emit-byte #x5E stream)
  (emit-byte (modrm 3 dst-tmm rhs-tmm) stream))

(defun x86-64-supports-avx512-p ()
  "Return T when AVX-512 support is enabled by flag or visible in host feature text."
  (or *x86-64-avx512-enabled*
      (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_AVX512")))
      (x86-64-host-supports-cpu-feature-p "avx512f")))

(defun x86-64-supports-amx-p ()
  "Return T when Intel AMX support is enabled or detected via host CPUID text."
  (or *amx-enabled*
      (x86-64-env-true-p (ignore-errors (sb-ext:posix-getenv "CLCC_AMX")))
      (x86-64-host-supports-cpu-feature-p "amx_tile")
      (x86-64-host-supports-cpu-feature-p "amx-tile")))

;;; Instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.) are in
;;; x86-64-encoding-instrs.lisp (loaded immediately after this file).
