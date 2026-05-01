;;;; tests/unit/bytecode/decode-tests.lisp - Bytecode ISA v2 Decoder Tests
;;;
;;; Tests for field extraction, instruction-format classifier,
;;; and disassemble-instruction.  Includes regression for
;;; tail-call :3op classification.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite bytecode-decode-suite
  :description "Bytecode ISA v2 decoder tests"
  :parent cl-cc-unit-suite)

(in-suite bytecode-decode-suite)

;;; ------------------------------------------------------------
;;; Field extraction: decode-opcode / decode-dst / decode-src1 / decode-src2
;;; ------------------------------------------------------------

(deftest decode-field-extraction
  "decode-opcode, decode-dst, decode-src1, and decode-src2 extract the correct bit fields."
  (let ((w1 (ash #xAB 24)))
    (assert-= #xAB (cl-cc/bytecode:decode-opcode w1)))
  (let ((w2 (logior (ash #x20 24) #xFFFFFF)))
    (assert-= #x20 (cl-cc/bytecode:decode-opcode w2)))
  (let ((w3 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 7 2 3)))
    (assert-= 7 (cl-cc/bytecode:decode-dst w3)))
  (let ((w4 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 9 3)))
    (assert-= 9 (cl-cc/bytecode:decode-src1 w4)))
  (let ((w5 (cl-cc/bytecode:encode-3op cl-cc/bytecode:+op-add+ 1 2 11)))
    (assert-= 11 (cl-cc/bytecode:decode-src2 w5))))

;;; ------------------------------------------------------------
;;; Field extraction: %sign-extend
;;; ------------------------------------------------------------

(deftest-each sign-extend-cases
  "%%sign-extend converts unsigned raw values to signed integers for any bit width."
  ((label raw bits expected)
   ("8-bit-positive"  127   8   127)
   ("8-bit-negative"  128   8  -128)
   ("8-bit-max"       255   8    -1)
   ("16-bit-positive" 32767 16  32767)
   ("16-bit-negative" 32768 16 -32768)
   ("16-bit-max"      65535 16     -1)
   ("24-bit-zero"     0     24      0))
  (declare (ignore label))
  (assert-= expected (cl-cc/bytecode::%sign-extend raw bits)))

;;; ------------------------------------------------------------
;;; Field extraction: decode-imm16
;;; ------------------------------------------------------------

(deftest-each decode-imm16-cases
  "decode-imm16 sign-extends 16-bit two's complement values correctly."
  ((label encoded-imm expected-value)
   ("positive"     100     100)
   ("zero"         0       0)
   ("negative-one" -1      -1)
   ("min"          -32768  -32768)
   ("max"          32767   32767))
  (declare (ignore label))
  (let ((w (cl-cc/bytecode:encode-imm cl-cc/bytecode:+op-load-fixnum+ 0 encoded-imm)))
    (assert-= expected-value (cl-cc/bytecode:decode-imm16 w))))

;;; ------------------------------------------------------------
;;; Field extraction: decode-offset24
;;; ------------------------------------------------------------

(deftest-each decode-offset24-cases
  "decode-offset24 sign-extends 24-bit branch offsets correctly."
  ((label offset)
   ("positive" 200)
   ("zero"     0)
   ("negative" -10))
  (declare (ignore label))
  (let ((w (cl-cc/bytecode:encode-branch cl-cc/bytecode:+op-jump+ offset)))
    (assert-= offset (cl-cc/bytecode:decode-offset24 w))))

;;; ------------------------------------------------------------
;;; instruction-format classifier
;;; ------------------------------------------------------------

;;; Regression note: +op-tail-call+ was previously classified as :2op,
;;; causing nargs to be silently dropped during disassembly.
(deftest-each decode-format-classification
  "instruction-format classifies each opcode into the correct format keyword."
  ((label opcode expected-format)
   ("add"             cl-cc/bytecode:+op-add+          :3op)
   ("call"            cl-cc/bytecode:+op-call+         :3op)
   ("tail-call"       cl-cc/bytecode:+op-tail-call+    :3op)
   ("move"            cl-cc/bytecode:+op-move+         :2op)
   ("neg"             cl-cc/bytecode:+op-neg+          :2op)
   ("load-fixnum"     cl-cc/bytecode:+op-load-fixnum+  :imm)
   ("jump"            cl-cc/bytecode:+op-jump+         :branch)
   ("nop"             cl-cc/bytecode:+op-nop+          :special)
   ("return-nil"      cl-cc/bytecode:+op-return-nil+   :special))
  (declare (ignore label))
  (assert-equal expected-format (cl-cc/bytecode:instruction-format opcode)))

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

(deftest-each decode-opcode-name-cases
  "Each opcode maps to its expected name string in *opcode-names*."
  ((label opcode expected-name)
   ("add"       cl-cc/bytecode:+op-add+       "ADD")
   ("tail-call" cl-cc/bytecode:+op-tail-call+ "TAIL_CALL")
   ("nop"       cl-cc/bytecode:+op-nop+       "NOP"))
  (declare (ignore label))
  (assert-equal expected-name
                (gethash opcode cl-cc/bytecode:*opcode-names*)))

;;; ------------------------------------------------------------
;;; disassemble-chunk
;;; ------------------------------------------------------------

(deftest disassemble-chunk-empty
  "disassemble-chunk runs without error on a zero-instruction chunk."
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 0 :element-type '(unsigned-byte 32))
                 :constants (vector)))
         (out   (with-output-to-string (s)
                  (cl-cc/bytecode:disassemble-chunk chunk s))))
    (assert-true (search "0 instruction" out))))

(deftest disassemble-chunk-single-instruction
  "disassemble-chunk prints the mnemonic for a single NOP instruction."
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 1
                                        :element-type '(unsigned-byte 32)
                                        :initial-contents (list (cl-cc/bytecode:encode-nop)))
                  :constants (vector)))
         (out   (with-output-to-string (s)
                   (cl-cc/bytecode:disassemble-chunk chunk s))))
    (assert-true (search "NOP" out))))

(deftest disassemble-chunk-with-constants
  "disassemble-chunk prints constant pool entries when present."
  (let* ((chunk (cl-cc/bytecode:make-bytecode-chunk
                 :code      (make-array 1
                                        :element-type '(unsigned-byte 32)
                                        :initial-contents (list (cl-cc/bytecode:encode-nop)))
                  :constants (vector 42 :foo)))
         (out   (with-output-to-string (s)
                   (cl-cc/bytecode:disassemble-chunk chunk s))))
    (assert-true (search "Constant pool" out))
    (assert-true (search "42" out))))
