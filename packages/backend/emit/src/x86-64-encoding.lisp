;;;; packages/backend/emit/src/x86-64-encoding.lisp - x86-64 Encoding Primitives
;;;
;;; Pure ISA-level encoding layer: register constants, output stream utilities,
;;; REX prefix, ModR/M, SIB encoding, and modrm-address helpers.
;;;
;;; Individual instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.)
;;; are in x86-64-encoding-instrs.lisp (loaded immediately after this file).

(in-package :cl-cc/emit)

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

;;; Instruction emitters (emit-mov-*, emit-add-*, emit-cmp-*, etc.) are in
;;; x86-64-encoding-instrs.lisp (loaded immediately after this file).

