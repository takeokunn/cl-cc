;;;; tests/unit/bytecode/decode-tests.lisp - Bytecode ISA v2 Decoder Tests
;;;
;;; Tests for field extraction, instruction-format classifier,
;;; and disassemble-instruction.  Includes regression tests for
;;; previously fixed bugs (tail-call :3op classification).

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite bytecode-decode-suite
  :description "Bytecode ISA v2 decoder tests"
  :parent cl-cc-suite)

(in-suite bytecode-decode-suite)

;;; ------------------------------------------------------------
;;; Field extraction: decode-opcode
;;; ------------------------------------------------------------

(deftest decode-opcode-extracts-bits-31-24
  "decode-opcode extracts bits[31:24]."
  (let ((w (ash #xAB 24)))
    (assert-= #xAB (cl-cc/bytecode:decode-opcode w))))

(deftest decode-opcode-ignores-lower-bits
  "decode-opcode ignores bits[23:0]."
  (let ((w (logior (ash #x20 24) #xFFFFFF)))
    (assert-= #x20 (cl-cc/bytecode:decode-opcode w))))

;;; ------------------------------------------------------------
;;; Field extraction: decode-dst / decode-src1 / decode-src2
;;; ------------------------------------------------------------

(deftest decode-dst-extracts-bits-23-16
  "decode-dst extracts bits[23:16]."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 7 2 3)))
    (assert-= 7 (cl-cc/bytecode:decode-dst w))))

(deftest decode-src1-extracts-bits-15-8
  "decode-src1 extracts bits[15:8]."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 9 3)))
    (assert-= 9 (cl-cc/bytecode:decode-src1 w))))

(deftest decode-src2-extracts-bits-7-0
  "decode-src2 extracts bits[7:0]."
  (let ((w (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 11)))
    (assert-= 11 (cl-cc/bytecode:decode-src2 w))))

;;; ------------------------------------------------------------
;;; Field extraction: decode-imm16
;;; ------------------------------------------------------------

(deftest decode-imm16-positive
  "decode-imm16 returns positive value."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 100)))
    (assert-= 100 (cl-cc/bytecode:decode-imm16 w))))

(deftest decode-imm16-zero
  "decode-imm16 returns 0."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 0)))
    (assert-= 0 (cl-cc/bytecode:decode-imm16 w))))

(deftest decode-imm16-negative-one
  "decode-imm16 sign-extends -1 from 16-bit two's complement."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 -1)))
    (assert-= -1 (cl-cc/bytecode:decode-imm16 w))))

(deftest decode-imm16-min
  "decode-imm16 sign-extends minimum value (-32768)."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 -32768)))
    (assert-= -32768 (cl-cc/bytecode:decode-imm16 w))))

(deftest decode-imm16-max
  "decode-imm16 returns maximum positive value (32767)."
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 32767)))
    (assert-= 32767 (cl-cc/bytecode:decode-imm16 w))))

;;; ------------------------------------------------------------
;;; Field extraction: decode-offset24
;;; ------------------------------------------------------------

(deftest decode-offset24-positive
  "decode-offset24 returns positive offset."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ 200)))
    (assert-= 200 (cl-cc/bytecode:decode-offset24 w))))

(deftest decode-offset24-zero
  "decode-offset24 returns 0."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ 0)))
    (assert-= 0 (cl-cc/bytecode:decode-offset24 w))))

(deftest decode-offset24-negative
  "decode-offset24 sign-extends negative offset."
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ -10)))
    (assert-= -10 (cl-cc/bytecode:decode-offset24 w))))

;;; ------------------------------------------------------------
;;; instruction-format classifier
;;; ------------------------------------------------------------

(deftest decode-format-add-is-3op
  "ADD opcode classifies as :3op."
  (assert-equal :3op (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-add+)))

(deftest decode-format-call-is-3op
  "CALL opcode classifies as :3op."
  (assert-equal :3op (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-call+)))

;;; Regression test: +op-tail-call+ was previously classified as :2op,
;;; causing nargs to be silently dropped during disassembly.
(deftest decode-format-tail-call-is-3op
  "TAIL_CALL opcode classifies as :3op (regression: was incorrectly :2op)."
  (assert-equal :3op (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-tail-call+)))

(deftest decode-format-move-is-2op
  "MOVE opcode classifies as :2op."
  (assert-equal :2op (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-move+)))

(deftest decode-format-neg-is-2op
  "NEG opcode classifies as :2op."
  (assert-equal :2op (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-neg+)))

(deftest decode-format-load-fixnum-is-imm
  "LOAD_FIXNUM opcode classifies as :imm."
  (assert-equal :imm (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-load-fixnum+)))

(deftest decode-format-jump-is-branch
  "JUMP opcode classifies as :branch."
  (assert-equal :branch (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-jump+)))

(deftest decode-format-nop-is-special
  "NOP opcode classifies as :special."
  (assert-equal :special (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-nop+)))

(deftest decode-format-return-nil-is-special
  "RETURN_NIL opcode classifies as :special."
  (assert-equal :special (cl-cc/bytecode:instruction-format cl-cc/bytecode:+op-return-nil+)))

;;; ------------------------------------------------------------
;;; disassemble-instruction round-trip
;;; ------------------------------------------------------------

(deftest decode-disassemble-3op-round-trip
  "disassemble-instruction decodes a 3-op ADD correctly."
  (let* ((w    (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 3))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :3op      (getf info :format))
    (assert-equal "ADD"     (getf info :opcode-name))
    (assert-= 1 (getf info :dst))
    (assert-= 2 (getf info :src1))
    (assert-= 3 (getf info :src2))))

;;; Regression: verify tail-call nargs is preserved through decode.
(deftest decode-disassemble-tail-call-preserves-nargs
  "disassemble-instruction decodes TAIL_CALL nargs correctly (regression fix)."
  (let* ((w    (cl-cc/bytecode:encode-tail-call 5 3))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :3op          (getf info :format))
    (assert-equal "TAIL_CALL"   (getf info :opcode-name))
    ;; func=5 is in src1, nargs=3 is in src2
    (assert-= 5 (getf info :src1))
    (assert-= 3 (getf info :src2))))

(deftest decode-disassemble-2op-round-trip
  "disassemble-instruction decodes a 2-op MOVE correctly."
  (let* ((w    (cl-cc/bytecode:encode-move 4 7))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :2op    (getf info :format))
    (assert-equal "MOVE"  (getf info :opcode-name))
    (assert-= 4 (getf info :dst))
    (assert-= 7 (getf info :src))))

(deftest decode-disassemble-imm-round-trip-positive
  "disassemble-instruction decodes a positive LOAD_FIXNUM correctly."
  (let* ((w    (cl-cc/bytecode:encode-load-fixnum 2 42))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :imm          (getf info :format))
    (assert-equal "LOAD_FIXNUM" (getf info :opcode-name))
    (assert-= 2  (getf info :dst))
    (assert-= 42 (getf info :imm16))))

(deftest decode-disassemble-imm-round-trip-negative
  "disassemble-instruction decodes a negative LOAD_FIXNUM immediate."
  (let* ((w    (cl-cc/bytecode:encode-load-fixnum 0 -5))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-= -5 (getf info :imm16))))

(deftest decode-disassemble-branch-round-trip
  "disassemble-instruction decodes a JUMP offset correctly."
  (let* ((w    (cl-cc/bytecode:encode-jump 99))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :branch (getf info :format))
    (assert-equal "JUMP"  (getf info :opcode-name))
    (assert-= 99 (getf info :offset24))))

(deftest decode-disassemble-special-nop
  "disassemble-instruction decodes NOP as :special."
  (let* ((w    (cl-cc/bytecode:encode-nop))
         (info (cl-cc/bytecode:disassemble-instruction w)))
    (assert-equal :special (getf info :format))
    (assert-equal "NOP"    (getf info :opcode-name))))

;;; ------------------------------------------------------------
;;; Opcode name table
;;; ------------------------------------------------------------

(deftest decode-opcode-name-add
  "ADD opcode maps to string \"ADD\" in *opcode-names*."
  (assert-equal "ADD"
                (gethash cl-cc/bytecode:+op-add+ cl-cc/bytecode:*opcode-names*)))

(deftest decode-opcode-name-tail-call
  "TAIL_CALL opcode maps to string \"TAIL_CALL\" in *opcode-names*."
  (assert-equal "TAIL_CALL"
                (gethash cl-cc/bytecode:+op-tail-call+ cl-cc/bytecode:*opcode-names*)))

(deftest decode-opcode-name-nop
  "NOP opcode maps to string \"NOP\"."
  (assert-equal "NOP"
                (gethash cl-cc/bytecode:+op-nop+ cl-cc/bytecode:*opcode-names*)))
