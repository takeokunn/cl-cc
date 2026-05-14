;;;; tests/unit/emit/aarch64-encoding-tests.lisp — AArch64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/aarch64-codegen.lisp encoding functions:
;;;; encode-movz, encode-movk, encode-mov-rr, encode-add, encode-sub,
;;;; encode-mul, encode-umulh, encode-smulh, encode-cbz, encode-b,
;;;; encode-blr, encode-stur, encode-ldur,
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
  (assert-equal expected (cl-cc/codegen::encode-movz rd 0)))

(deftest a64-movz-imm16-at-bits-20-5
  "MOVZ imm16=42 encodes at bits[20:5]; rd field is zero at bits[4:0]."
  (let ((word (cl-cc/codegen::encode-movz 0 42)))
    (assert-equal 42 (logand (ash word -5) #xFFFF))
    (assert-equal 0 (logand word #x1F))))

(deftest a64-movz-hw-field-at-bits-22-21
  "MOVZ hw=1 places hw=1 at bits[22:21] and imm16=1 at bits[20:5]."
  (let ((word (cl-cc/codegen::encode-movz 0 1 1)))
    (assert-equal 1 (logand (ash word -21) 3))
    (assert-equal 1 (logand (ash word -5) #xFFFF))))

(deftest a64-movz-max-imm16-roundtrips
  "MOVZ max imm16 (#xFFFF) roundtrips correctly."
  (let ((word (cl-cc/codegen::encode-movz 0 #xFFFF)))
    (assert-equal #xFFFF (logand (ash word -5) #xFFFF))))

;;; ─── encode-movk ─────────────────────────────────────────────────────────

(deftest a64-movk-encodes-base-opcode-and-imm16-hw-fields
  "MOVK base opcode is correct; imm16 and hw fields encode at the right bit positions."
  (let ((base-word (cl-cc/codegen::encode-movk 0 0))
        (imm-word  (cl-cc/codegen::encode-movk 0 #x1234 2)))
    (assert-equal #xF2800000 (logand base-word #xFF800000))
    (assert-equal #x1234 (logand (ash imm-word -5) #xFFFF))
    (assert-equal 2 (logand (ash imm-word -21) 3))))

(deftest a64-mov-rr-encodes-rd-and-rm-correctly
  "encode-mov-rr places Rd at bits[4:0] and Rm at bits[20:16], including self-copy."
  (let ((w01 (cl-cc/codegen::encode-mov-rr 0 1))
        (w55 (cl-cc/codegen::encode-mov-rr 5 5)))
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
  (let ((word (cl-cc/codegen::encode-add rd rn rm)))
    (assert-equal rd (logand word #x1F))
    (assert-equal rn (logand (ash word -5) #x1F))
    (assert-equal rm (logand (ash word -16) #x1F))
    (assert-equal expected-opcode-bits (logand word #xFFE00000))))

;;; ─── encode-sub ──────────────────────────────────────────────────────────

(deftest a64-sub-encodes-rd-rn-rm-and-opcode
  "SUB encodes Rd/Rn/Rm at correct bit positions and opcode #xCB000000."
  (let ((word (cl-cc/codegen::encode-sub 0 1 2)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -5) #x1F))
    (assert-equal 2 (logand (ash word -16) #x1F))
    (assert-equal #xCB000000 (logand word #xFFE00000))))

(deftest a64-sub-opcode-differs-from-add-but-registers-match
  "ADD and SUB have different opcodes but identical register bit patterns."
  (let ((add (cl-cc/codegen::encode-add 0 1 2))
        (sub (cl-cc/codegen::encode-sub 0 1 2)))
    (assert-false (= add sub))
    (assert-equal (logand add #x1FFFFF) (logand sub #x1FFFFF))))

(deftest a64-mul-encodes-as-madd-with-xzr-accumulator
  "MUL encodes as MADD with XZR (31) at bits[14:10] as the accumulator."
  (let ((word (cl-cc/codegen::encode-mul 0 1 2)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 1 (logand (ash word -5) #x1F))
    (assert-equal 2 (logand (ash word -16) #x1F))
    (assert-equal 31 (logand (ash word -10) #x1F))))

(deftest a64-mul-high-encoders
  "UMULH/SMULH preserve the Rd/Rn/Rm field layout while selecting the correct opcode family."
  (assert-equal #x9BC27C20 (cl-cc/codegen::encode-umulh 0 1 2))
  (assert-equal #x9B427C20 (cl-cc/codegen::encode-smulh 0 1 2)))

(deftest a64-fsqrt-encoder
  "FSQRT Dd,Dn preserves Rd/Rn fields while selecting the scalar double opcode."
  (assert-equal #x1E61C020 (cl-cc/codegen::encode-fsqrt 0 1)))

;;; ─── encode-cbz ──────────────────────────────────────────────────────────

(deftest a64-cbz-encodes-rt-imm19-and-opcode
  "CBZ encodes Rt at bits[4:0], imm19 at bits[23:5], and opcode #xB4000000."
  (let ((w0 (cl-cc/codegen::encode-cbz 0 1))
        (w5 (cl-cc/codegen::encode-cbz 5 10)))
    (assert-equal 0 (logand w0 #x1F))
    (assert-equal 1 (logand (ash w0 -5) #x7FFFF))
    (assert-equal #xB4000000 (logand w0 #xFF000000))
    (assert-equal 5 (logand w5 #x1F))
    (assert-equal 10 (logand (ash w5 -5) #x7FFFF))))

(deftest a64-b-encodes-imm26-and-opcode
  "B encodes imm26 at bits[25:0] with opcode #x14000000."
  (let ((w2 (cl-cc/codegen::encode-b 2)))
    (assert-equal 2 (logand w2 #x3FFFFFF))
    (assert-equal #x14000000 (logand w2 #xFC000000)))
  (assert-equal #x14000000 (cl-cc/codegen::encode-b 0)))

(deftest a64-blr-encodes-register-at-bits-9-5
  "BLR places the register number at bits[9:5] with opcode #xD63F0000."
  (let ((w0  (cl-cc/codegen::encode-blr 0))
        (w30 (cl-cc/codegen::encode-blr 30)))
    (assert-equal 0  (logand (ash w0  -5) #x1F))
    (assert-equal #xD63F0000 (logand w0 #xFFFFFC00))
    (assert-equal 30 (logand (ash w30 -5) #x1F))))

;;; ─── +a64-ret+ ──────────────────────────────────────────────────────────

;;; ─── encode-stur ─────────────────────────────────────────────────────────

(deftest a64-stur-encodes-rt-rn-and-simm9
  "STUR encodes Rt at bits[4:0], Rn at bits[9:5], and simm9 at bits[20:12]."
  (let ((w8 (cl-cc/codegen::encode-stur 0 29 8))
        (w0 (cl-cc/codegen::encode-stur 1 31 0)))
    (assert-equal 0  (logand w8 #x1F))
    (assert-equal 29 (logand (ash w8 -5) #x1F))
    (assert-equal 8  (logand (ash w8 -12) #x1FF))
    (assert-equal 1  (logand w0 #x1F))
    (assert-equal 31 (logand (ash w0 -5) #x1F))
    (assert-equal 0  (logand (ash w0 -12) #x1FF))))

(deftest a64-ldur-encodes-correctly-with-load-opcode
  "LDUR encodes Rt/Rn/simm9 correctly and has opcode #xF8400000."
  (let ((word (cl-cc/codegen::encode-ldur 0 29 8)))
    (assert-equal 0 (logand word #x1F))
    (assert-equal 29 (logand (ash word -5) #x1F))
    (assert-equal 8 (logand (ash word -12) #x1FF))
    (assert-equal #xF8400000 (logand word #xFFE00000))))

(deftest a64-ldur-and-stur-differ-only-in-opcode
  "LDUR and STUR encodings differ in opcode bits but share the same register/offset pattern."
  (let ((ld (cl-cc/codegen::encode-ldur 0 1 8))
        (st (cl-cc/codegen::encode-stur 0 1 8)))
    (assert-false (= ld st))
    (assert-equal (logand ld #x1FFFFF) (logand st #x1FFFFF))))

;;; ─── encode-stp-pre ──────────────────────────────────────────────────────

(deftest a64-stack-pair-encoding
  "STP and LDP encode Rt1/Rt2/Rn correctly in the save/restore pair."
  (let ((stp (cl-cc/codegen::encode-stp-pre 29 30 31 (logand -2 #x7F))))
    (assert-equal 29 (logand stp #x1F))
    (assert-equal 30 (logand (ash stp -10) #x1F))
    (assert-equal 31 (logand (ash stp -5) #x1F)))
  (let ((ldp (cl-cc/codegen::encode-ldp-post 29 30 31 2)))
    (assert-equal 29 (logand ldp #x1F))
    (assert-equal 30 (logand (ash ldp -10) #x1F))
    (assert-equal 31 (logand (ash ldp -5) #x1F))
    (assert-equal 2  (logand (ash ldp -15) #x7F))))

;;; ─── +a64-ret+ and emit-a64-instr ──────────────────────────────────────

(deftest a64-ret-constant-has-fixed-encoding
  "+a64-ret+ has the fixed AArch64 RET encoding #xD65F03C0."
  (assert-equal #xD65F03C0 cl-cc/codegen::+a64-ret+))

(deftest a64-emit-instr-writes-four-little-endian-bytes
  "emit-a64-instr writes 4 bytes in little-endian order; zero word produces 4 zero bytes."
  (let ((bytes-ret nil)
        (bytes-zero nil))
    (cl-cc/codegen::emit-a64-instr #xD65F03C0 (lambda (b) (push b bytes-ret)))
    (setf bytes-ret (nreverse bytes-ret))
    (assert-equal 4 (length bytes-ret))
    (assert-equal #xC0 (first bytes-ret))
    (assert-equal #x03 (second bytes-ret))
    (assert-equal #x5F (third bytes-ret))
    (assert-equal #xD6 (fourth bytes-ret))
    (cl-cc/codegen::emit-a64-instr 0 (lambda (b) (push b bytes-zero)))
    (assert-equal 4 (length (nreverse bytes-zero)))
    (assert-true (every #'zerop bytes-zero))))

;;; ─── Special register constants ──────────────────────────────────────────

(deftest-each a64-special-register-constants
  "AArch64 special register numbers: sp=31, fp=X29, lr=X30."
  :cases (("sp" cl-cc/codegen::+a64-sp+ 31)
          ("fp" cl-cc/codegen::+a64-fp+ 29)
          ("lr" cl-cc/codegen::+a64-lr+ 30))
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
  (let ((entry (assoc reg cl-cc/codegen::*aarch64-reg-number*)))
    (funcall verify entry)))
