;;;; tests/unit/emit/aarch64-encoding-tests.lisp — AArch64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/aarch64-codegen.lisp encoding functions:
;;;; encode-movz, encode-movk, encode-mov-rr, encode-add, encode-sub,
;;;; encode-mul, encode-cbz, encode-b, encode-blr, encode-stur, encode-ldur,
;;;; encode-stp-pre, encode-ldp-post, +a64-ret+, emit-a64-instr.

(in-package :cl-cc/test)

(defsuite aarch64-encoding-suite :description "AArch64 instruction encoding unit tests"
  :parent cl-cc-unit-suite)


(in-suite aarch64-encoding-suite)
;;; ─── encode-movz ─────────────────────────────────────────────────────────

(deftest-each a64-movz-base-cases
  "MOVZ Xd, #0 encodes to its expected fixed value for different destination registers."
  :cases (("x0" #xD2800000 0)
          ("x1" #xD2800001 1))
  (expected rd)
  (assert-equal expected (cl-cc/emit::encode-movz rd 0)))

(deftest a64-movz-field-cases
  "MOVZ field encoding: imm16 at bits 20:5; hw at bits 22:21; max imm16 roundtrips."
  (let ((word (cl-cc/emit::encode-movz 0 42)))
    (assert-equal 42 (logand (ash word -5) #xFFFF))
    (assert-equal 0 (logand word #x1F)))
  (let ((word (cl-cc/emit::encode-movz 0 1 1)))
    (assert-equal 1 (logand (ash word -21) 3))
    (assert-equal 1 (logand (ash word -5) #xFFFF)))
  (let ((word (cl-cc/emit::encode-movz 0 #xFFFF)))
    (assert-equal #xFFFF (logand (ash word -5) #xFFFF))))

;;; ─── encode-movk ─────────────────────────────────────────────────────────

(deftest a64-mov-encoding-cases
  "MOVK: base opcode correct; imm16/hw fields encode correctly. MOV Xd,Xs encodes Rd and Rm in correct bit positions (including self-copy)."
  (let ((base-word (cl-cc/emit::encode-movk 0 0))
        (imm-word  (cl-cc/emit::encode-movk 0 #x1234 2)))
    (assert-equal #xF2800000 (logand base-word #xFF800000))
    (assert-equal #x1234 (logand (ash imm-word -5) #xFFFF))
    (assert-equal 2 (logand (ash imm-word -21) 3)))
  (let ((w01 (cl-cc/emit::encode-mov-rr 0 1))
        (w55 (cl-cc/emit::encode-mov-rr 5 5)))
    (assert-equal 0 (logand w01 #x1F))
    (assert-equal 1 (logand (ash w01 -16) #x1F))
    (assert-equal 5 (logand w55 #x1F))
    (assert-equal 5 (logand (ash w55 -16) #x1F))))

;;; ─── encode-add ──────────────────────────────────────────────────────────

(deftest-each a64-add-register-fields
  "ADD encodes Rd/Rn/Rm in correct bit positions for low and high register numbers."
  :cases (("low-regs"  0  1  2  #x8B000000)
          ("high-regs" 28 29 30 #x8B000000))
  (rd rn rm expected-opcode-bits)
  (let ((word (cl-cc/emit::encode-add rd rn rm)))
    (assert-equal rd (logand word #x1F))
    (assert-equal rn (logand (ash word -5) #x1F))
    (assert-equal rm (logand (ash word -16) #x1F))
    (assert-equal expected-opcode-bits (logand word #xFFE00000))))

;;; ─── encode-sub ──────────────────────────────────────────────────────────

(deftest a64-arithmetic-encoding-cases
  "SUB encodes Rd/Rn/Rm + opcode #xCB000000; opcode differs from ADD; MUL encodes as MADD with XZR accumulator."
  (let ((word (cl-cc/emit::encode-sub 0 1 2)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -5) #x1F))
    (assert-equal 2 (logand (ash word -16) #x1F))
    (assert-equal #xCB000000 (logand word #xFFE00000)))
  (let ((add (cl-cc/emit::encode-add 0 1 2))
        (sub (cl-cc/emit::encode-sub 0 1 2)))
    (assert-false (= add sub))
    (assert-equal (logand add #x1FFFFF) (logand sub #x1FFFFF)))
  (let ((word (cl-cc/emit::encode-mul 0 1 2)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -5) #x1F))
    (assert-equal 2 (logand (ash word -16) #x1F))
    (assert-equal 31 (logand (ash word -10) #x1F))))

;;; ─── encode-cbz ──────────────────────────────────────────────────────────

(deftest a64-branch-encoding-cases
  "CBZ encodes Rt/imm19/opcode; B encodes imm26/opcode; BLR encodes register at bits 9:5."
  (let ((w0 (cl-cc/emit::encode-cbz 0 1))
        (w5 (cl-cc/emit::encode-cbz 5 10)))
    (assert-equal 0 (logand w0 #x1F))
    (assert-equal 1 (logand (ash w0 -5) #x7FFFF))
    (assert-equal #xB4000000 (logand w0 #xFF000000))
    (assert-equal 5 (logand w5 #x1F))
    (assert-equal 10 (logand (ash w5 -5) #x7FFFF)))
  (let ((w2 (cl-cc/emit::encode-b 2)))
    (assert-equal 2 (logand w2 #x3FFFFFF))
    (assert-equal #x14000000 (logand w2 #xFC000000)))
  (assert-equal #x14000000 (cl-cc/emit::encode-b 0))
  (let ((w0  (cl-cc/emit::encode-blr 0))
        (w30 (cl-cc/emit::encode-blr 30)))
    (assert-equal 0  (logand (ash w0  -5) #x1F))
    (assert-equal #xD63F0000 (logand w0 #xFFFFFC00))
    (assert-equal 30 (logand (ash w30 -5) #x1F))))

;;; ─── +a64-ret+ ──────────────────────────────────────────────────────────

;;; ─── encode-stur ─────────────────────────────────────────────────────────

(deftest a64-memory-encoding-cases
  "STUR encodes Rt/Rn/simm9; LDUR encodes correctly and differs from STUR only in opcode bits."
  (let ((w8 (cl-cc/emit::encode-stur 0 29 8))
        (w0 (cl-cc/emit::encode-stur 1 31 0)))
    (assert-equal 0  (logand w8 #x1F))
    (assert-equal 29 (logand (ash w8 -5) #x1F))
    (assert-equal 8  (logand (ash w8 -12) #x1FF))
    (assert-equal 1  (logand w0 #x1F))
    (assert-equal 31 (logand (ash w0 -5) #x1F))
    (assert-equal 0  (logand (ash w0 -12) #x1FF)))
  (let ((word (cl-cc/emit::encode-ldur 0 29 8)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 29 (logand (ash word -5) #x1F))
    (assert-equal 8 (logand (ash word -12) #x1FF))
    (assert-equal #xF8400000 (logand word #xFFE00000)))
  (let ((ld (cl-cc/emit::encode-ldur 0 1 8))
        (st (cl-cc/emit::encode-stur 0 1 8)))
    (assert-false (= ld st))
    (assert-equal (logand ld #x1FFFFF) (logand st #x1FFFFF))))

;;; ─── encode-stp-pre ──────────────────────────────────────────────────────

(deftest a64-stack-pair-encoding
  "STP and LDP encode Rt1/Rt2/Rn correctly in the save/restore pair."
  (let ((stp (cl-cc/emit::encode-stp-pre 29 30 31 (logand -2 #x7F))))
    (assert-equal 29 (logand stp #x1F))
    (assert-equal 30 (logand (ash stp -10) #x1F))
    (assert-equal 31 (logand (ash stp -5) #x1F)))
  (let ((ldp (cl-cc/emit::encode-ldp-post 29 30 31 2)))
    (assert-equal 29 (logand ldp #x1F))
    (assert-equal 30 (logand (ash ldp -10) #x1F))
    (assert-equal 31 (logand (ash ldp -5) #x1F))
    (assert-equal 2  (logand (ash ldp -15) #x7F))))

;;; ─── +a64-ret+ and emit-a64-instr ──────────────────────────────────────

(deftest a64-ret-and-emit-cases
  "RET has fixed encoding #xD65F03C0; emit-a64-instr writes 4 little-endian bytes."
  (assert-equal #xD65F03C0 cl-cc/emit::+a64-ret+)
  (let ((bytes-ret nil)
        (bytes-zero nil))
    (cl-cc/emit::emit-a64-instr #xD65F03C0 (lambda (b) (push b bytes-ret)))
    (setf bytes-ret (nreverse bytes-ret))
    (assert-equal 4 (length bytes-ret))
    (assert-equal #xC0 (first bytes-ret))
    (assert-equal #x03 (second bytes-ret))
    (assert-equal #x5F (third bytes-ret))
    (assert-equal #xD6 (fourth bytes-ret))
    (cl-cc/emit::emit-a64-instr 0 (lambda (b) (push b bytes-zero)))
    (assert-equal 4 (length (nreverse bytes-zero)))
    (assert-true (every #'zerop bytes-zero))))

;;; ─── Special register constants ──────────────────────────────────────────

(deftest-each a64-special-register-constants
  "AArch64 special register numbers: sp=31, fp=X29, lr=X30."
  :cases (("sp" cl-cc/emit::+a64-sp+ 31)
          ("fp" cl-cc/emit::+a64-fp+ 29)
          ("lr" cl-cc/emit::+a64-lr+ 30))
  (constant expected)
  (assert-equal expected constant))

;;; ─── Register mapping ───────────────────────────────────────────────────

(deftest-each a64-reg-number-table
  "Register mapping table has expected entries; x18 (platform register) is absent."
  :cases (("x0"         :x0
           (lambda (entry)
             (assert-equal 0 (cdr entry))))
          ("x7"         :x7
           (lambda (entry)
             (assert-equal 7 (cdr entry))))
          ("x30"        :x30
           (lambda (entry)
             (assert-equal 30 (cdr entry))))
          ("x18-absent" :x18
           (lambda (entry)
             (assert-null entry))))
  (reg verify)
  (let ((entry (assoc reg cl-cc/emit::*aarch64-reg-number*)))
    (funcall verify entry)))
