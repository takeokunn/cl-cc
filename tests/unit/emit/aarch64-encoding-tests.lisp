;;;; tests/unit/emit/aarch64-encoding-tests.lisp — AArch64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/aarch64-codegen.lisp encoding functions:
;;;; encode-movz, encode-movk, encode-mov-rr, encode-add, encode-sub,
;;;; encode-mul, encode-cbz, encode-b, encode-blr, encode-stur, encode-ldur,
;;;; encode-stp-pre, encode-ldp-post, +a64-ret+, emit-a64-instr.

(in-package :cl-cc/test)

(defsuite aarch64-encoding-suite :description "AArch64 instruction encoding unit tests")

;;; ─── encode-movz ─────────────────────────────────────────────────────────

(deftest a64-movz-zero-to-x0
  "MOVZ X0, #0 encodes correctly."
  ;; Base = #xD2800000, rd=0, imm16=0, hw=0
  (assert-equal #xD2800000 (cl-cc::encode-movz 0 0)))

(deftest a64-movz-imm-to-x0
  "MOVZ X0, #42 encodes immediate in bits 20:5."
  (let ((word (cl-cc::encode-movz 0 42)))
    ;; imm16 field: bits 20:5 = 42 << 5
    (assert-equal 42 (logand (ash word -5) #xFFFF))
    ;; Rd field: bits 4:0 = 0
    (assert-equal 0 (logand word #x1F))))

(deftest a64-movz-register-x1
  "MOVZ X1, #0 puts register in bits 4:0."
  (assert-equal #xD2800001 (cl-cc::encode-movz 1 0)))

(deftest a64-movz-hw-shift
  "MOVZ X0, #1, LSL #16 encodes hw=1 in bits 22:21."
  (let ((word (cl-cc::encode-movz 0 1 1)))
    ;; hw field: bits 22:21 = 1
    (assert-equal 1 (logand (ash word -21) 3))
    ;; imm16 = 1
    (assert-equal 1 (logand (ash word -5) #xFFFF))))

(deftest a64-movz-max-imm16
  "MOVZ X0, #0xFFFF encodes maximum 16-bit immediate."
  (let ((word (cl-cc::encode-movz 0 #xFFFF)))
    (assert-equal #xFFFF (logand (ash word -5) #xFFFF))))

;;; ─── encode-movk ─────────────────────────────────────────────────────────

(deftest a64-movk-base
  "MOVK X0, #0 has correct base opcode."
  (let ((word (cl-cc::encode-movk 0 0)))
    ;; Base should be #xF2800000
    (assert-equal #xF2800000 (logand word #xFF800000))))

(deftest a64-movk-imm-hw
  "MOVK X0, #0x1234, LSL #32 encodes imm16 and hw=2."
  (let ((word (cl-cc::encode-movk 0 #x1234 2)))
    (assert-equal #x1234 (logand (ash word -5) #xFFFF))
    (assert-equal 2 (logand (ash word -21) 3))))

;;; ─── encode-mov-rr ───────────────────────────────────────────────────────

(deftest a64-mov-rr-x0-x1
  "MOV X0, X1 (ORR X0, XZR, X1)."
  (let ((word (cl-cc::encode-mov-rr 0 1)))
    ;; Rd=0, Rm(Rn)=1 at bits 20:16
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -16) #x1F))))

(deftest a64-mov-rr-same
  "MOV X5, X5 is valid (copy to self)."
  (let ((word (cl-cc::encode-mov-rr 5 5)))
    (assert-equal 5 (logand word #x1F))
    (assert-equal 5 (logand (ash word -16) #x1F))))

;;; ─── encode-add ──────────────────────────────────────────────────────────

(deftest a64-add-encoding
  "ADD X0, X1, X2 encodes all three registers."
  (let ((word (cl-cc::encode-add 0 1 2)))
    (assert-equal 0 (logand word #x1F))           ; Rd
    (assert-equal 1 (logand (ash word -5) #x1F))  ; Rn
    (assert-equal 2 (logand (ash word -16) #x1F)) ; Rm
    ;; Top bits should be ADD opcode
    (assert-equal #x8B000000 (logand word #xFFE00000))))

(deftest a64-add-high-regs
  "ADD X28, X29, X30 uses high register numbers."
  (let ((word (cl-cc::encode-add 28 29 30)))
    (assert-equal 28 (logand word #x1F))
    (assert-equal 29 (logand (ash word -5) #x1F))
    (assert-equal 30 (logand (ash word -16) #x1F))))

;;; ─── encode-sub ──────────────────────────────────────────────────────────

(deftest a64-sub-encoding
  "SUB X0, X1, X2 encodes correctly."
  (let ((word (cl-cc::encode-sub 0 1 2)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -5) #x1F))
    (assert-equal 2 (logand (ash word -16) #x1F))
    (assert-equal #xCB000000 (logand word #xFFE00000))))

(deftest a64-sub-different-from-add
  "SUB and ADD opcodes differ in the top bits."
  (let ((add (cl-cc::encode-add 0 1 2))
        (sub (cl-cc::encode-sub 0 1 2)))
    (assert-false (= add sub))
    ;; Same register fields
    (assert-equal (logand add #x1FFFFF) (logand sub #x1FFFFF))))

;;; ─── encode-mul ──────────────────────────────────────────────────────────

(deftest a64-mul-encoding
  "MUL X0, X1, X2 encodes as MADD with XZR accumulator."
  (let ((word (cl-cc::encode-mul 0 1 2)))
    (assert-equal 0 (logand word #x1F))           ; Rd
    (assert-equal 1 (logand (ash word -5) #x1F))  ; Rn
    (assert-equal 2 (logand (ash word -16) #x1F)) ; Rm
    ;; Ra field (bits 14:10) should be 31 (XZR) = #x7C00
    (assert-equal 31 (logand (ash word -10) #x1F))))

;;; ─── encode-cbz ──────────────────────────────────────────────────────────

(deftest a64-cbz-encoding
  "CBZ X0, +4 (1 instruction forward)."
  (let ((word (cl-cc::encode-cbz 0 1)))
    (assert-equal 0 (logand word #x1F))           ; Rt
    (assert-equal 1 (logand (ash word -5) #x7FFFF)) ; imm19
    (assert-equal #xB4000000 (logand word #xFF000000))))

(deftest a64-cbz-larger-offset
  "CBZ X5, +40 (10 instructions forward)."
  (let ((word (cl-cc::encode-cbz 5 10)))
    (assert-equal 5 (logand word #x1F))
    (assert-equal 10 (logand (ash word -5) #x7FFFF))))

;;; ─── encode-b ────────────────────────────────────────────────────────────

(deftest a64-b-forward
  "B +8 (2 instructions forward)."
  (let ((word (cl-cc::encode-b 2)))
    (assert-equal 2 (logand word #x3FFFFFF))
    (assert-equal #x14000000 (logand word #xFC000000))))

(deftest a64-b-zero
  "B +0 encodes base opcode."
  (assert-equal #x14000000 (cl-cc::encode-b 0)))

;;; ─── encode-blr ──────────────────────────────────────────────────────────

(deftest a64-blr-x0
  "BLR X0 encodes register at bits 9:5."
  (let ((word (cl-cc::encode-blr 0)))
    (assert-equal 0 (logand (ash word -5) #x1F))
    (assert-equal #xD63F0000 (logand word #xFFFFFC00))))

(deftest a64-blr-x30
  "BLR X30 (call through link register)."
  (let ((word (cl-cc::encode-blr 30)))
    (assert-equal 30 (logand (ash word -5) #x1F))))

;;; ─── +a64-ret+ ──────────────────────────────────────────────────────────

(deftest a64-ret-encoding
  "RET has fixed encoding #xD65F03C0."
  (assert-equal #xD65F03C0 cl-cc::+a64-ret+))

;;; ─── encode-stur ─────────────────────────────────────────────────────────

(deftest a64-stur-positive-offset
  "STUR X0, [X29, #8] encodes positive simm9."
  (let ((word (cl-cc::encode-stur 0 29 8)))
    (assert-equal 0 (logand word #x1F))           ; Rt
    (assert-equal 29 (logand (ash word -5) #x1F)) ; Rn
    (assert-equal 8 (logand (ash word -12) #x1FF)))) ; simm9

(deftest a64-stur-zero-offset
  "STUR X1, [SP, #0] with zero offset."
  (let ((word (cl-cc::encode-stur 1 31 0)))
    (assert-equal 1 (logand word #x1F))
    (assert-equal 31 (logand (ash word -5) #x1F))
    (assert-equal 0 (logand (ash word -12) #x1FF))))

;;; ─── encode-ldur ─────────────────────────────────────────────────────────

(deftest a64-ldur-encoding
  "LDUR X0, [X29, #8] encodes load with offset."
  (let ((word (cl-cc::encode-ldur 0 29 8)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 29 (logand (ash word -5) #x1F))
    (assert-equal 8 (logand (ash word -12) #x1FF))
    ;; LDUR base opcode differs from STUR
    (assert-equal #xF8400000 (logand word #xFFE00000))))

(deftest a64-ldur-vs-stur-opcode
  "LDUR and STUR differ only in the opcode bits."
  (let ((ld (cl-cc::encode-ldur 0 1 8))
        (st (cl-cc::encode-stur 0 1 8)))
    (assert-false (= ld st))
    ;; Lower 21 bits (registers + offset) should match
    (assert-equal (logand ld #x1FFFFF) (logand st #x1FFFFF))))

;;; ─── encode-stp-pre ──────────────────────────────────────────────────────

(deftest a64-stp-pre-encoding
  "STP X29, X30, [SP, #-16]! encodes prologue store pair."
  (let ((word (cl-cc::encode-stp-pre 29 30 31 (logand -2 #x7F))))
    ;; Rt1=29, Rt2=30, Rn=31(SP)
    (assert-equal 29 (logand word #x1F))
    (assert-equal 30 (logand (ash word -10) #x1F))
    (assert-equal 31 (logand (ash word -5) #x1F))))

;;; ─── encode-ldp-post ─────────────────────────────────────────────────────

(deftest a64-ldp-post-encoding
  "LDP X29, X30, [SP], #16 encodes epilogue load pair."
  (let ((word (cl-cc::encode-ldp-post 29 30 31 2)))
    (assert-equal 29 (logand word #x1F))           ; Rt1
    (assert-equal 30 (logand (ash word -10) #x1F)) ; Rt2
    (assert-equal 31 (logand (ash word -5) #x1F))  ; Rn=SP
    (assert-equal 2 (logand (ash word -15) #x7F)))) ; imm7

;;; ─── emit-a64-instr ─────────────────────────────────────────────────────

(deftest a64-emit-instr-little-endian
  "emit-a64-instr writes 4 bytes little-endian."
  (let ((bytes nil))
    (cl-cc::emit-a64-instr #xD65F03C0 (lambda (b) (push b bytes)))
    (setf bytes (nreverse bytes))
    (assert-equal 4 (length bytes))
    ;; Little-endian: least significant byte first
    (assert-equal #xC0 (first bytes))
    (assert-equal #x03 (second bytes))
    (assert-equal #x5F (third bytes))
    (assert-equal #xD6 (fourth bytes))))

(deftest a64-emit-instr-zero
  "emit-a64-instr for word 0 writes four zero bytes."
  (let ((bytes nil))
    (cl-cc::emit-a64-instr 0 (lambda (b) (push b bytes)))
    (assert-equal 4 (length (nreverse bytes)))
    (assert-true (every #'zerop bytes))))

;;; ─── Special register constants ──────────────────────────────────────────

(deftest a64-sp-register
  "Stack pointer is register 31."
  (assert-equal 31 cl-cc::+a64-sp+))

(deftest a64-fp-register
  "Frame pointer is X29."
  (assert-equal 29 cl-cc::+a64-fp+))

(deftest a64-lr-register
  "Link register is X30."
  (assert-equal 30 cl-cc::+a64-lr+))

;;; ─── Register mapping ───────────────────────────────────────────────────

(deftest a64-reg-number-table
  "Register mapping table has expected entries."
  (let ((table cl-cc::*aarch64-reg-number*))
    (assert-equal 0 (cdr (assoc :x0 table)))
    (assert-equal 7 (cdr (assoc :x7 table)))
    (assert-equal 30 (cdr (assoc :x30 table)))
    ;; x18 (platform register) should NOT be in the mapping
    (assert-null (assoc :x18 table))))
