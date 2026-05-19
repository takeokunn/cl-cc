;;;; packages/emit/src/x86-64-encoding.lisp - x86-64 Encoding Primitives
;;;
;;; Pure ISA-level encoding layer: register constants, output stream utilities,
;;; REX prefix, ModR/M, SIB encoding, and modrm-address helpers.
;;;
;;; Individual instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.)
;;; are in x86-64-encoding-instrs.lisp (loaded immediately after this file).

(in-package :cl-cc/codegen)

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

;;; Instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.) are in
;;; x86-64-encoding-instrs.lisp (loaded immediately after this file).
