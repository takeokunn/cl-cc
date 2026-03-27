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

(deftest-each bytecode-opcode-constants
  "Opcode constants have the expected integer values."
  ((label constant expected)
   ("nop"        cl-cc/bytecode:+op-nop+        #x00)
   ("load-const" cl-cc/bytecode:+op-load-const+ #x01)
   ("add"        cl-cc/bytecode:+op-add+        #x10)
   ("return"     cl-cc/bytecode:+op-return+     #x35)
   ("wide"       cl-cc/bytecode:+op-wide+       #xFE))
  (declare (ignore label))
  (assert-= expected constant))

;;; ------------------------------------------------------------
;;; encode-nop
;;; ------------------------------------------------------------

(deftest bytecode-encode-nop-is-zero
  "encode-nop produces a zero 32-bit word."
  (assert-= 0 (cl-cc/bytecode:encode-nop)))

;;; ------------------------------------------------------------
;;; encode-3op
;;; ------------------------------------------------------------

(deftest-each bytecode-encode-3op-cases
  "encode-3op packs opcode, dst, src1, src2 into correct bit positions."
  ((label op dst src1 src2)
   ("basic"         cl-cc/bytecode:+op-add+ 1   2   3)
   ("max-registers" cl-cc/bytecode:+op-add+ 255 255 255)
   ("zero-registers" cl-cc/bytecode:+op-mul+ 0  0   0))
  (declare (ignore label))
  (let ((w (cl-cc/bytecode:encode-3op op dst src1 src2)))
    (assert-= op   (ldb (byte 8 24) w))
    (assert-= dst  (ldb (byte 8 16) w))
    (assert-= src1 (ldb (byte 8  8) w))
    (assert-= src2 (ldb (byte 8  0) w))))

;;; ------------------------------------------------------------
;;; encode-2op
;;; ------------------------------------------------------------

(deftest bytecode-encode-2op-cases
  "encode-2op packs opcode, dst, src into correct bit positions and leaves bits[7:0] as zero."
  (let ((w1 (cl-cc/bytecode:encode-2op cl-cc/bytecode:+op-neg+ 4 5 0)))
    (assert-= cl-cc/bytecode:+op-neg+ (ldb (byte 8 24) w1))
    (assert-= 4 (ldb (byte 8 16) w1))
    (assert-= 5 (ldb (byte 8  8) w1)))
  (let ((w2 (cl-cc/bytecode:encode-2op cl-cc/bytecode:+op-move+ 10 20 0)))
    (assert-= 0 (ldb (byte 8 0) w2))))

;;; ------------------------------------------------------------
;;; encode-imm
;;; ------------------------------------------------------------

(deftest-each bytecode-encode-imm-cases
  "encode-imm encodes opcode, register, and immediate into the correct bit fields."
  ((label reg imm expected-low16)
   ("positive" 3 100     100)
   ("zero"     0 0       0)
   ("negative" 1 -1      #xFFFF)
   ("min"      0 -32768  #x8000))
  (declare (ignore label))
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ reg imm)))
    (assert-= cl-cc/bytecode:+op-load-fixnum+ (ldb (byte 8 24) w))
    (assert-= reg (ldb (byte 8 16) w))
    (assert-= expected-low16 (ldb (byte 16 0) w))))

;;; ------------------------------------------------------------
;;; encode-branch
;;; ------------------------------------------------------------

(deftest-each bytecode-encode-branch-cases
  "encode-branch encodes the opcode and offset into the correct bit fields."
  ((label offset expected-low24)
   ("zero"     0   0)
   ("positive" 50  50)
   ("negative" -1  #xFFFFFF))
  (declare (ignore label))
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)))
    (assert-= cl-cc/bytecode:+op-jump+ (ldb (byte 8 24) w))
    (assert-= expected-low24 (ldb (byte 24 0) w))))

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
