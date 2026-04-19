;;;; decode-disasm.lisp — CL-CC Bytecode ISA v2 Disassembler
;;;;
;;;; Instruction format classifier and human-readable disassembly.
;;;; Field extraction and opcode name table are in decode.lisp (loads before).
;;;;
;;;; Load order: after decode.lisp.

(in-package :cl-cc/bytecode)

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
    (loop for word across code
          for i from 0
          do (let* ((info (disassemble-instruction word))
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
