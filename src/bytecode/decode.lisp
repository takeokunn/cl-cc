;;;; src/bytecode/decode.lisp - CL-CC Bytecode ISA v2 Disassembler
;;;;
;;;; Field extraction, instruction format identification, and
;;;; human-readable disassembly of 32-bit bytecode words.

(in-package :cl-cc/bytecode)

;;; ------------------------------------------------------------
;;; Field Extraction (hot-path, all inlined)
;;; ------------------------------------------------------------

(declaim (inline decode-opcode decode-dst decode-src1 decode-src2
                 decode-imm16 decode-offset24))

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
  (let ((raw (ldb (byte 16 0) word)))
    (declare (type (unsigned-byte 16) raw))
    ;; Sign-extend: if bit 15 set, value is negative
    (if (logbitp 15 raw)
        (- raw 65536)
        raw)))

(defun decode-offset24 (word)
  "Extract signed 24-bit branch offset from bits [23:0]."
  (declare (type (unsigned-byte 32) word)
           (optimize (speed 3) (safety 1)))
  (let ((raw (ldb (byte 24 0) word)))
    (declare (type (unsigned-byte 24) raw))
    ;; Sign-extend: if bit 23 set, value is negative
    (if (logbitp 23 raw)
        (- raw 16777216)
        raw)))

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
  (let ((ht (make-hash-table :test #'eql)))
    (dolist (pair *opcode-name-data* ht)
      (setf (gethash (car pair) ht) (cdr pair))))
  "Hash table mapping opcode byte value to its mnemonic string.")

;;; ------------------------------------------------------------
;;; Instruction Format Classifier
;;; ------------------------------------------------------------

(defun instruction-format (opcode)
  "Return the format keyword for OPCODE.

   Formats:
     :3op     — [opcode:8][dst:8][src1:8][src2:8]
     :2op     — [opcode:8][dst:8][src:8][pad:8]
     :imm     — [opcode:8][dst:8][imm16:16]
     :branch  — [opcode:8][offset:24]
     :special — everything else (NOP, RETURN_NIL, POP_HANDLER, POP_UNWIND, etc.)"
  (declare (type (unsigned-byte 8) opcode))
  (case opcode
    ;; 3-operand: arithmetic, comparisons, call, collections, slots
    ((#x10 #x11 #x12 #x13 #x14
      #x20 #x21 #x22
      #x23 #x24 #x25 #x26 #x27
      #x33 #x34
      #x01
      #x40
      #x50 #x51
      #x54
      #x60
      #x63 #x64 #x65
      #x67 #x68
      #x70)
      :3op)
    ;; 2-operand: unary ops, moves, type predicates
    ((#x02
      #x03 #x04
      #x15 #x16 #x17
      #x35
      #x41 #x42 #x43
      #x52 #x53
      #x61 #x62
      #x66
      #x71 #x72 #x73 #x74 #x75
      #x80 #x81
      #x92
      #x94)
      :2op)
    ;; Immediate: load-fixnum, conditional branches, push-handler
    ((#x05
      #x31 #x32
      #x90)
      :imm)
    ;; Branch: unconditional jump, push-unwind
    ((#x30 #x93)
      :branch)
    ;; Special / zero-operand
    (otherwise
     :special)))

;;; ------------------------------------------------------------
;;; Single-Instruction Disassembler
;;; ------------------------------------------------------------

(defun disassemble-instruction (word)
  "Decode a single 32-bit instruction WORD.
   Returns a plist with keys :opcode-name, :format, and format-specific operand keys."
  (declare (type (unsigned-byte 32) word))
  (let* ((op   (decode-opcode word))
         (name (gethash op *opcode-names* (format nil "UNKNOWN(#x~2,'0X)" op)))
         (fmt  (instruction-format op)))
    (ecase fmt
      (:3op
       (list :opcode-name name
             :format :3op
             :dst  (decode-dst  word)
             :src1 (decode-src1 word)
             :src2 (decode-src2 word)))
      (:2op
       (list :opcode-name name
             :format :2op
             :dst (decode-dst  word)
             :src (decode-src1 word)))
      (:imm
       (list :opcode-name name
             :format :imm
             :dst   (decode-dst   word)
             :imm16 (decode-imm16 word)))
      (:branch
       (list :opcode-name name
             :format  :branch
             :offset24 (decode-offset24 word)))
      (:special
       (list :opcode-name name
             :format :special
             :raw word)))))

;;; ------------------------------------------------------------
;;; Chunk Disassembler
;;; ------------------------------------------------------------

(defun disassemble-chunk (chunk &optional (stream *standard-output*))
  "Print a human-readable disassembly of CHUNK to STREAM.
   Each instruction is printed as:
     <offset>  <hex-word>  <mnemonic> [operands...]"
  (declare (type bytecode-chunk chunk))
  (let ((code      (bytecode-chunk-code chunk))
        (constants (bytecode-chunk-constants chunk)))
    (format stream "~&; Bytecode chunk: ~D instruction(s), ~D constant(s)~%"
            (length code) (length constants))
    ;; Print constant pool
    (when (plusp (length constants))
      (format stream "; Constant pool:~%")
      (loop for i from 0 below (length constants) do
        (format stream ";   [~D] ~S~%" i (aref constants i))))
    (format stream ";~%")
    ;; Print instructions
    (loop for i from 0 below (length code) do
      (let* ((word (aref code i))
             (info (disassemble-instruction word))
             (name (getf info :opcode-name))
             (fmt  (getf info :format)))
        (format stream "~4D  ~8,'0X  ~A" i word name)
        (ecase fmt
          (:3op
           (format stream " r~D, r~D, r~D"
                   (getf info :dst)
                   (getf info :src1)
                   (getf info :src2)))
          (:2op
           (format stream " r~D, r~D"
                   (getf info :dst)
                   (getf info :src)))
          (:imm
           (format stream " r~D, ~D"
                   (getf info :dst)
                   (getf info :imm16)))
          (:branch
           (let ((target (+ i (getf info :offset24))))
             (format stream " ~D  ; -> ~D"
                     (getf info :offset24)
                     target)))
          (:special
           ;; No additional operands for NOP, RETURN_NIL, POP_HANDLER, POP_UNWIND
           (let ((raw (getf info :raw)))
             (unless (zerop (logand raw #x00ffffff))
               (format stream " [raw: #x~6,'0X]" (logand raw #x00ffffff))))))
        (format stream "~%")))
    (values)))
