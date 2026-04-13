;;;; src/bytecode/encode-ops.lisp - CL-CC Bytecode ISA v2 — Encoding Functions
;;;;
;;;; Core encoders (encode-3op / encode-2op / encode-imm / encode-branch),
;;;; all per-opcode encode-* helpers, bytecode-chunk and bytecode-builder
;;;; structs, and the emit / build-bytecode builder API.
;;;;
;;;; Constants, inline field-extraction helpers, and the bytecode-word type
;;;; are in encode.lisp (loads before this file).
;;;;
;;;; Load order: after encode.lisp.

(in-package :cl-cc/bytecode)

(declaim (inline encode-3op encode-2op encode-imm encode-branch encode-nop))

(defun encode-3op (opcode dst src1 src2)
  "Encode a 3-operand instruction: [opcode:8][dst:8][src1:8][src2:8]."
  (declare (type (unsigned-byte 8) opcode dst src1 src2)
           (optimize (speed 3) (safety 1)))
  (the (unsigned-byte 32)
       (logior (ash opcode 24)
               (ash dst    16)
               (ash src1    8)
               src2)))

(defun encode-2op (opcode dst src pad)
  "Encode a 2-operand instruction: [opcode:8][dst:8][src:8][pad:8].
   Use 0 for pad."
  (declare (type (unsigned-byte 8) opcode dst src pad)
           (optimize (speed 3) (safety 1)))
  (the (unsigned-byte 32)
       (logior (ash opcode 24)
               (ash dst    16)
               (ash src     8)
               pad)))

(defun encode-imm (opcode dst imm16)
  "Encode an immediate instruction: [opcode:8][dst:8][imm16:16].
   imm16 is a signed 16-bit integer in [-32768, 32767]."
  (declare (type (unsigned-byte 8) opcode dst)
           (type (integer -32768 32767) imm16)
           (optimize (speed 3) (safety 1)))
  (the (unsigned-byte 32)
       (logior (ash opcode 24)
               (ash dst    16)
               (%sign-extend-16 imm16))))

(defun encode-branch (opcode offset24)
  "Encode a branch instruction: [opcode:8][offset:24].
   offset24 is a signed 24-bit integer in [-8388608, 8388607]."
  (declare (type (unsigned-byte 8) opcode)
           (type (integer -8388608 8388607) offset24)
           (optimize (speed 3) (safety 1)))
  (the (unsigned-byte 32)
       (logior (ash opcode 24)
               (%sign-extend-24 offset24))))

(defun encode-nop ()
  "Encode a NOP instruction."
  (declare (optimize (speed 3) (safety 1)))
  (the (unsigned-byte 32) 0))

;;; ------------------------------------------------------------
;;; Specific Instruction Encoders
;;; ------------------------------------------------------------

;;; Load / Move

(defun encode-load-const (dst const-idx)
  "LOAD_CONST dst, const_idx — load constant pool entry into dst."
  (declare (type (unsigned-byte 8) dst const-idx))
  (encode-3op +op-load-const+ dst const-idx 0))

(defun encode-move (dst src)
  "MOVE dst, src — copy register src to dst."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-move+ dst src 0))

(defun encode-load-nil (dst)
  "LOAD_NIL dst — load NIL into dst."
  (declare (type (unsigned-byte 8) dst))
  (encode-2op +op-load-nil+ dst 0 0))

(defun encode-load-true (dst)
  "LOAD_TRUE dst — load T into dst."
  (declare (type (unsigned-byte 8) dst))
  (encode-2op +op-load-true+ dst 0 0))

(defun encode-load-fixnum (dst imm16)
  "LOAD_FIXNUM dst, imm16 — load small fixnum immediate into dst."
  (declare (type (unsigned-byte 8) dst)
           (type (integer -32768 32767) imm16))
  (encode-imm +op-load-fixnum+ dst imm16))

;;; Arithmetic

(defun encode-add (dst src1 src2)
  "ADD dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-add+ dst src1 src2))

(defun encode-sub (dst src1 src2)
  "SUB dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-sub+ dst src1 src2))

(defun encode-mul (dst src1 src2)
  "MUL dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-mul+ dst src1 src2))

(defun encode-div (dst src1 src2)
  "DIV dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-div+ dst src1 src2))

(defun encode-mod (dst src1 src2)
  "MOD dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-mod+ dst src1 src2))

(defun encode-neg (dst src)
  "NEG dst, src."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-neg+ dst src 0))

(defun encode-inc (dst src)
  "INC dst, src."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-inc+ dst src 0))

(defun encode-dec (dst src)
  "DEC dst, src."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-dec+ dst src 0))

;;; Comparison

(defun encode-eq (dst src1 src2)
  "EQ dst, src1, src2 — pointer equality."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-eq+ dst src1 src2))

(defun encode-eql (dst src1 src2)
  "EQL dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-eql+ dst src1 src2))

(defun encode-equal (dst src1 src2)
  "EQUAL dst, src1, src2 — structural equality."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-equal+ dst src1 src2))

(defun encode-num-lt (dst src1 src2)
  "NUM_LT dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-num-lt+ dst src1 src2))

(defun encode-num-gt (dst src1 src2)
  "NUM_GT dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-num-gt+ dst src1 src2))

(defun encode-num-le (dst src1 src2)
  "NUM_LE dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-num-le+ dst src1 src2))

(defun encode-num-ge (dst src1 src2)
  "NUM_GE dst, src1, src2."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-num-ge+ dst src1 src2))

(defun encode-num-eq (dst src1 src2)
  "NUM_EQ dst, src1, src2 — numeric equality."
  (declare (type (unsigned-byte 8) dst src1 src2))
  (encode-3op +op-num-eq+ dst src1 src2))

;;; Control Flow

(defun encode-jump (offset)
  "JUMP offset — unconditional branch; offset is signed 24-bit."
  (declare (type (integer -8388608 8388607) offset))
  (encode-branch +op-jump+ offset))

(defun encode-jump-if-nil (src offset)
  "JUMP_IF_NIL src, offset — branch if src is NIL.
   Uses [opcode:8][src:8][offset:16] layout (imm format)."
  (declare (type (unsigned-byte 8) src)
           (type (integer -32768 32767) offset))
  (encode-imm +op-jump-if-nil+ src offset))

(defun encode-jump-if-true (src offset)
  "JUMP_IF_TRUE src, offset — branch if src is truthy.
   Uses [opcode:8][src:8][offset:16] layout (imm format)."
  (declare (type (unsigned-byte 8) src)
           (type (integer -32768 32767) offset))
  (encode-imm +op-jump-if-true+ src offset))

(defun encode-call (dst func nargs)
  "CALL dst, func, nargs — call func with nargs args, result in dst."
  (declare (type (unsigned-byte 8) dst func nargs))
  (encode-3op +op-call+ dst func nargs))

(defun encode-tail-call (func nargs)
  "TAIL_CALL func, nargs — tail call func with nargs args."
  (declare (type (unsigned-byte 8) func nargs))
  (encode-3op +op-tail-call+ 0 func nargs))

(defun encode-return (src)
  "RETURN src — return value in src."
  (declare (type (unsigned-byte 8) src))
  (encode-2op +op-return+ src 0 0))

(defun encode-return-nil ()
  "RETURN_NIL — return NIL."
  (encode-2op +op-return-nil+ 0 0 0))

