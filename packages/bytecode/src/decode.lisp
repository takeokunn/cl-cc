;;;; packages/bytecode/src/decode.lisp - CL-CC Bytecode ISA v2 Field Extraction
;;;;
;;;; Contains: field extraction, opcode name table.
;;;; Disassembler (instruction-format, disassemble-instruction, disassemble-chunk)
;;;; is in decode-disasm.lisp (loads after).

(in-package :cl-cc/bytecode)

;;; ------------------------------------------------------------
;;; Field Extraction (hot-path, all inlined)
;;; ------------------------------------------------------------

(declaim (inline decode-opcode decode-dst decode-src1 decode-src2
                 decode-imm16 decode-offset24 %sign-extend))

(defun %sign-extend (raw bits)
  "Sign-extend RAW from BITS-wide two's complement to a signed integer."
  (declare (type fixnum raw bits)
           (optimize (speed 3) (safety 1)))
  (if (logbitp (1- bits) raw)
      (- raw (ash 1 bits))
      raw))

(defun decode-opcode (word)
  "Extract opcode byte from bits [31:24]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 24) word))

(defun decode-dst (word)
  "Extract dst field from bits [23:16]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 16) word))

(defun decode-src1 (word)
  "Extract src1 field from bits [15:8]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 8) word))

(defun decode-src2 (word)
  "Extract src2 field from bits [7:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (ldb (byte 8 0) word))

(defun decode-imm16 (word)
  "Extract signed 16-bit immediate from bits [15:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (%sign-extend (ldb (byte 16 0) word) 16))

(defun decode-offset24 (word)
  "Extract signed 24-bit branch offset from bits [23:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (%sign-extend (ldb (byte 24 0) word) 24))

;;; ------------------------------------------------------------
;;; Opcode Name Table
;;; ------------------------------------------------------------

;;; Declarative alist of (opcode-constant . mnemonic).
;;; #. evaluates each constant at read time; dolist builds the lookup table.
(defparameter *opcode-name-data*
  '(;; Misc
    (#.+op-nop+          . "NOP")
    ;; Load / Move
    (#.+op-load-const+   . "LOAD_CONST")
    (#.+op-move+         . "MOVE")
    (#.+op-load-nil+     . "LOAD_NIL")
    (#.+op-load-true+    . "LOAD_TRUE")
    (#.+op-load-fixnum+  . "LOAD_FIXNUM")
    ;; Arithmetic
    (#.+op-add+          . "ADD")
    (#.+op-sub+          . "SUB")
    (#.+op-mul+          . "MUL")
    (#.+op-div+          . "DIV")
    (#.+op-mod+          . "MOD")
    (#.+op-neg+          . "NEG")
    (#.+op-inc+          . "INC")
    (#.+op-dec+          . "DEC")
    ;; Comparison
    (#.+op-eq+           . "EQ")
    (#.+op-eql+          . "EQL")
    (#.+op-equal+        . "EQUAL")
    (#.+op-num-lt+       . "NUM_LT")
    (#.+op-num-gt+       . "NUM_GT")
    (#.+op-num-le+       . "NUM_LE")
    (#.+op-num-ge+       . "NUM_GE")
    (#.+op-num-eq+       . "NUM_EQ")
    ;; Control Flow
    (#.+op-jump+         . "JUMP")
    (#.+op-jump-if-nil+  . "JUMP_IF_NIL")
    (#.+op-jump-if-true+ . "JUMP_IF_TRUE")
    (#.+op-call+         . "CALL")
    (#.+op-tail-call+    . "TAIL_CALL")
    (#.+op-return+       . "RETURN")
    (#.+op-return-nil+   . "RETURN_NIL")
    ;; Closure & Upvalue
    (#.+op-make-closure+  . "MAKE_CLOSURE")
    (#.+op-get-upvalue+   . "GET_UPVALUE")
    (#.+op-set-upvalue+   . "SET_UPVALUE")
    (#.+op-close-upvalue+ . "CLOSE_UPVALUE")
    ;; Object Access
    (#.+op-get-slot+      . "GET_SLOT")
    (#.+op-set-slot+      . "SET_SLOT")
    (#.+op-get-global+    . "GET_GLOBAL")
    (#.+op-set-global+    . "SET_GLOBAL")
    (#.+op-make-instance+ . "MAKE_INSTANCE")
    ;; Collections
    (#.+op-cons+          . "CONS")
    (#.+op-car+           . "CAR")
    (#.+op-cdr+           . "CDR")
    (#.+op-make-vector+   . "MAKE_VECTOR")
    (#.+op-vector-ref+    . "VECTOR_REF")
    (#.+op-vector-set+    . "VECTOR_SET")
    (#.+op-make-hash+     . "MAKE_HASH")
    (#.+op-hash-ref+      . "HASH_REF")
    (#.+op-hash-set+      . "HASH_SET")
    ;; Type Check
    (#.+op-type-check+    . "TYPE_CHECK")
    (#.+op-fixnump+       . "FIXNUMP")
    (#.+op-consp+         . "CONSP")
    (#.+op-symbolp+       . "SYMBOLP")
    (#.+op-functionp+     . "FUNCTIONP")
    (#.+op-stringp+       . "STRINGP")
    ;; Multiple Values
    (#.+op-values+        . "VALUES")
    (#.+op-recv-values+   . "RECV_VALUES")
    ;; Exception Handling
    (#.+op-push-handler+  . "PUSH_HANDLER")
    (#.+op-pop-handler+   . "POP_HANDLER")
    (#.+op-signal+        . "SIGNAL")
    (#.+op-push-unwind+   . "PUSH_UNWIND")
    (#.+op-pop-unwind+    . "POP_UNWIND")
    ;; Wide prefix
    (#.+op-wide+          . "WIDE"))
  "Alist of (opcode-byte . mnemonic-string) for all known opcodes.")

(defvar *opcode-names*
  (loop with ht = (make-hash-table :test #'eql)
        for (code . name) in *opcode-name-data*
        do (setf (gethash code ht) name)
        finally (return ht))
  "Hash table mapping opcode byte value to its mnemonic string.")

