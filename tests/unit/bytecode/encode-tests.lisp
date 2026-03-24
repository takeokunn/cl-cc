;;;; tests/unit/bytecode/encode-tests.lisp - Bytecode ISA v2 Encoder Tests
;;;
;;; Tests for opcode constants, 32-bit instruction word encoding,
;;; bytecode-builder emit/build, and round-trip encoding correctness.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite bytecode-encode-suite
  :description "Bytecode ISA v2 encoder tests"
  :parent cl-cc-suite)

(in-suite bytecode-encode-suite)

;;; ------------------------------------------------------------
;;; Opcode constants
;;; ------------------------------------------------------------

(deftest bytecode-op-nop-is-zero
  "+op-nop+ is #x00."
  (assert-= #x00 cl-cc/bytecode:+op-nop+))

(deftest bytecode-op-load-const
  "+op-load-const+ is #x01."
  (assert-= #x01 cl-cc/bytecode:+op-load-const+))

(deftest bytecode-op-add
  "+op-add+ is #x10."
  (assert-= #x10 cl-cc/bytecode:+op-add+))

(deftest bytecode-op-return
  "+op-return+ is #x35."
  (assert-= #x35 cl-cc/bytecode:+op-return+))

(deftest bytecode-op-wide
  "+op-wide+ is #xFE."
  (assert-= #xFE cl-cc/bytecode:+op-wide+))

;;; ------------------------------------------------------------
;;; encode-nop
;;; ------------------------------------------------------------

(deftest bytecode-encode-nop-is-zero
  "encode-nop produces a zero 32-bit word."
  (assert-= 0 (cl-cc/bytecode:encode-nop)))

;;; ------------------------------------------------------------
;;; encode-3op
;;; ------------------------------------------------------------

(deftest bytecode-encode-3op-basic
  "encode-3op packs opcode, dst, src1, src2 into correct bit positions."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 3)))
    ;; bits[31:24] = op-add (#x20), bits[23:16] = 1, bits[15:8] = 2, bits[7:0] = 3
    (assert-= cl-cc/bytecode:+op-add+ (ldb (byte 8 24) w))
    (assert-= 1 (ldb (byte 8 16) w))
    (assert-= 2 (ldb (byte 8  8) w))
    (assert-= 3 (ldb (byte 8  0) w))))

(deftest bytecode-encode-3op-max-registers
  "encode-3op handles maximum register index (255)."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 255 255 255)))
    (assert-= 255 (ldb (byte 8 16) w))
    (assert-= 255 (ldb (byte 8  8) w))
    (assert-= 255 (ldb (byte 8  0) w))))

(deftest bytecode-encode-3op-zero-registers
  "encode-3op handles register index 0."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-mul+ 0 0 0)))
    (assert-= cl-cc/bytecode:+op-mul+ (ldb (byte 8 24) w))
    (assert-= 0 (ldb (byte 8 16) w))
    (assert-= 0 (ldb (byte 8  8) w))
    (assert-= 0 (ldb (byte 8  0) w))))

;;; ------------------------------------------------------------
;;; encode-2op
;;; ------------------------------------------------------------

(deftest bytecode-encode-2op-basic
  "encode-2op packs opcode, dst, src into correct bit positions."
  (let ((w (cl-cc/bytecode:encode-2op cl-cc/bytecode:+op-neg+ 4 5 0)))
    (assert-= cl-cc/bytecode:+op-neg+ (ldb (byte 8 24) w))
    (assert-= 4 (ldb (byte 8 16) w))
    (assert-= 5 (ldb (byte 8  8) w))))

(deftest bytecode-encode-2op-low-byte-zero
  "encode-2op leaves bits[7:0] as zero (padding byte)."
  (let ((w (cl-cc/bytecode:encode-2op cl-cc/bytecode:+op-move+ 10 20 0)))
    (assert-= 0 (ldb (byte 8 0) w))))

;;; ------------------------------------------------------------
;;; encode-imm
;;; ------------------------------------------------------------

(deftest bytecode-encode-imm-positive
  "encode-imm with positive immediate value."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 3 100)))
    (assert-= cl-cc/bytecode:+op-load-fixnum+ (ldb (byte 8 24) w))
    (assert-= 3 (ldb (byte 8 16) w))
    ;; low 16 bits = 100
    (assert-= 100 (ldb (byte 16 0) w))))

(deftest bytecode-encode-imm-zero
  "encode-imm with imm=0."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 0)))
    (assert-= 0 (ldb (byte 16 0) w))))

(deftest bytecode-encode-imm-negative
  "encode-imm with negative immediate (sign-extended in low 16 bits)."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 1 -1)))
    ;; -1 as 16-bit two's complement = #xFFFF
    (assert-= #xFFFF (ldb (byte 16 0) w))))

(deftest bytecode-encode-imm-min
  "encode-imm with minimum 16-bit signed value (-32768)."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 -32768)))
    (assert-= #x8000 (ldb (byte 16 0) w))))

;;; ------------------------------------------------------------
;;; encode-branch
;;; ------------------------------------------------------------

(deftest bytecode-encode-branch-zero
  "encode-branch with offset 0 produces op in high byte, zeros elsewhere."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ 0)))
    (assert-= cl-cc/bytecode:+op-jump+ (ldb (byte 8 24) w))
    (assert-= 0 (ldb (byte 24 0) w))))

(deftest bytecode-encode-branch-positive
  "encode-branch with positive offset stores in bits[23:0]."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ 50)))
    (assert-= 50 (ldb (byte 24 0) w))))

(deftest bytecode-encode-branch-negative
  "encode-branch with negative offset (-1) stores two's complement in bits[23:0]."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ -1)))
    ;; -1 as 24-bit two's complement = #xFFFFFF
    (assert-= #xFFFFFF (ldb (byte 24 0) w))))

;;; ------------------------------------------------------------
;;; bytecode-builder: emit and build
;;; ------------------------------------------------------------

(deftest bytecode-builder-empty-chunk
  "build-bytecode on empty builder gives zero-length code vector."
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (let ((chunk (cl-cc/bytecode:build-bytecode b)))
      (assert-= 0 (length (cl-cc/bytecode:bytecode-chunk-code chunk))))))

(deftest bytecode-builder-emit-one
  "Emitting one instruction gives a code vector of length 1."
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (cl-cc/bytecode:emit (cl-cc/bytecode:encode-nop) b)
    (let ((chunk (cl-cc/bytecode:build-bytecode b)))
      (assert-= 1 (length (cl-cc/bytecode:bytecode-chunk-code chunk))))))

(deftest bytecode-builder-emit-preserves-word
  "emit stores the exact word; build-bytecode returns it unchanged."
  (let* ((b (cl-cc/bytecode:make-bytecode-builder))
         (w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 5 6 7)))
    (cl-cc/bytecode:emit w b)
    (let* ((chunk (cl-cc/bytecode:build-bytecode b))
           (code  (cl-cc/bytecode:bytecode-chunk-code chunk)))
      (assert-= w (aref code 0)))))

(deftest bytecode-builder-emit-constant
  "emit-constant stores a value in the constant pool."
  (let ((b (cl-cc/bytecode:make-bytecode-builder)))
    (let ((idx (cl-cc/bytecode:emit-constant 'hello b)))
      (assert-= 0 idx)
      (let ((chunk (cl-cc/bytecode:build-bytecode b)))
        (assert-= 1 (length (cl-cc/bytecode:bytecode-chunk-constants chunk)))
        (assert-equal 'hello (aref (cl-cc/bytecode:bytecode-chunk-constants chunk) 0))))))

(deftest bytecode-builder-emit-multiple
  "Emitting multiple instructions preserves order."
  (let ((b  (cl-cc/bytecode:make-bytecode-builder))
        (w0 (cl-cc/bytecode:encode-nop))
        (w1 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 3))
        (w2 (cl-cc/bytecode:encode-return 0)))
    (cl-cc/bytecode:emit w0 b)
    (cl-cc/bytecode:emit w1 b)
    (cl-cc/bytecode:emit w2 b)
    (let* ((chunk (cl-cc/bytecode:build-bytecode b))
           (code  (cl-cc/bytecode:bytecode-chunk-code chunk)))
      (assert-= 3 (length code))
      (assert-= w0 (aref code 0))
      (assert-= w1 (aref code 1))
      (assert-= w2 (aref code 2)))))

;;; ------------------------------------------------------------
;;; Specific instruction encoders
;;; ------------------------------------------------------------

(deftest bytecode-encode-add
  "encode-add produces correct opcode and registers."
  (let ((w (cl-cc/bytecode:encode-add 1 2 3)))
    (assert-= cl-cc/bytecode:+op-add+ (ldb (byte 8 24) w))
    (assert-= 1 (ldb (byte 8 16) w))
    (assert-= 2 (ldb (byte 8  8) w))
    (assert-= 3 (ldb (byte 8  0) w))))

(deftest bytecode-encode-move
  "encode-move produces correct opcode."
  (let ((w (cl-cc/bytecode:encode-move 5 10)))
    (assert-= cl-cc/bytecode:+op-move+ (ldb (byte 8 24) w))))

(deftest bytecode-encode-jump
  "encode-jump produces correct opcode and offset."
  (let ((w (cl-cc/bytecode:encode-jump 10)))
    (assert-= cl-cc/bytecode:+op-jump+ (ldb (byte 8 24) w))
    (assert-= 10 (ldb (byte 24 0) w))))

(deftest bytecode-encode-return
  "encode-return produces correct opcode with src register."
  (let ((w (cl-cc/bytecode:encode-return 3)))
    (assert-= cl-cc/bytecode:+op-return+ (ldb (byte 8 24) w))
    (assert-= 3 (ldb (byte 8 16) w))))

(deftest bytecode-encode-tail-call-is-3op
  "encode-tail-call uses 3-operand format: dst=0(pad), src1=func, src2=nargs."
  ;; encode-tail-call (func nargs) → encode-3op op-tail-call 0 func nargs
  (let ((w (cl-cc/bytecode:encode-tail-call 5 3)))
    (assert-= cl-cc/bytecode:+op-tail-call+ (ldb (byte 8 24) w))
    (assert-= 0 (ldb (byte 8 16) w))   ; dst pad
    (assert-= 5 (ldb (byte 8  8) w))   ; func reg
    (assert-= 3 (ldb (byte 8  0) w)))) ; nargs
