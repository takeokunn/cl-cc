;;;; src/bytecode/encode.lisp - CL-CC Bytecode ISA v2 Encoder
;;;;
;;;; 32-bit instruction word encoding.
;;;;
;;;; Instruction formats:
;;;;   [opcode:8][dst:8][src1:8][src2:8]   ; 3-operand (arithmetic, compare)
;;;;   [opcode:8][dst:8][imm16:16]          ; immediate load
;;;;   [opcode:8][offset:24]                ; branch (signed 24-bit offset)
;;;;   [opcode:8][dst:8][src:8][pad:8]      ; 2-operand (unary, move)
;;;;
;;;; Wide operands (>256) use extension prefix 0xFE followed by a full 32-bit word.

(in-package :cl-cc/bytecode)

;;; ------------------------------------------------------------
;;; Bytecode Word Type
;;; ------------------------------------------------------------

(deftype bytecode-word () '(unsigned-byte 32))

;;; ------------------------------------------------------------
;;; Opcode Constants
;;; ------------------------------------------------------------

;; Misc
(defconstant +op-nop+         #x00)

;; Load / Move
(defconstant +op-load-const+  #x01)
(defconstant +op-move+        #x02)
(defconstant +op-load-nil+    #x03)
(defconstant +op-load-true+   #x04)
(defconstant +op-load-fixnum+ #x05)

;; Arithmetic (NaN-boxed fast path + slow path)
(defconstant +op-add+  #x10)
(defconstant +op-sub+  #x11)
(defconstant +op-mul+  #x12)
(defconstant +op-div+  #x13)
(defconstant +op-mod+  #x14)
(defconstant +op-neg+  #x15)
(defconstant +op-inc+  #x16)
(defconstant +op-dec+  #x17)

;; Comparison
(defconstant +op-eq+     #x20)
(defconstant +op-eql+    #x21)
(defconstant +op-equal+  #x22)
(defconstant +op-num-lt+ #x23)
(defconstant +op-num-gt+ #x24)
(defconstant +op-num-le+ #x25)
(defconstant +op-num-ge+ #x26)
(defconstant +op-num-eq+ #x27)

;; Control Flow
(defconstant +op-jump+         #x30)
(defconstant +op-jump-if-nil+  #x31)
(defconstant +op-jump-if-true+ #x32)
(defconstant +op-call+         #x33)
(defconstant +op-tail-call+    #x34)
(defconstant +op-return+       #x35)
(defconstant +op-return-nil+   #x36)

;; Closure & Upvalue
(defconstant +op-make-closure+  #x40)
(defconstant +op-get-upvalue+   #x41)
(defconstant +op-set-upvalue+   #x42)
(defconstant +op-close-upvalue+ #x43)

;; Object Access (with inline cache)
(defconstant +op-get-slot+     #x50)
(defconstant +op-set-slot+     #x51)
(defconstant +op-get-global+   #x52)
(defconstant +op-set-global+   #x53)
(defconstant +op-make-instance+ #x54)

;; Collections
(defconstant +op-cons+        #x60)
(defconstant +op-car+         #x61)
(defconstant +op-cdr+         #x62)
(defconstant +op-make-vector+ #x63)
(defconstant +op-vector-ref+  #x64)
(defconstant +op-vector-set+  #x65)
(defconstant +op-make-hash+   #x66)
(defconstant +op-hash-ref+    #x67)
(defconstant +op-hash-set+    #x68)

;; Type Check (fast path)
(defconstant +op-type-check+ #x70)
(defconstant +op-fixnump+    #x71)
(defconstant +op-consp+      #x72)
(defconstant +op-symbolp+    #x73)
(defconstant +op-functionp+  #x74)
(defconstant +op-stringp+    #x75)

;; Multiple Values
(defconstant +op-values+      #x80)
(defconstant +op-recv-values+ #x81)

;; Exception Handling
(defconstant +op-push-handler+ #x90)
(defconstant +op-pop-handler+  #x91)
(defconstant +op-signal+       #x92)
(defconstant +op-push-unwind+  #x93)
(defconstant +op-pop-unwind+   #x94)

;; Wide prefix for operands > 255
(defconstant +op-wide+ #xfe)

;;; ------------------------------------------------------------
;;; Inline Field Extraction Helpers (for encode use)
;;; ------------------------------------------------------------

(declaim (inline %pack-byte %sign-extend-16 %sign-extend-24))

(defun %pack-byte (x)
  "Clamp x to 8 bits (unsigned)."
  (declare (type (unsigned-byte 8) x))
  (logand x #xff))

(defun %sign-extend-16 (n)
  "Interpret signed 16-bit value n as unsigned 16-bit for encoding.
   Input n must be in [-32768, 32767]."
  (declare (type (integer -32768 32767) n))
  (logand n #xffff))

(defun %sign-extend-24 (n)
  "Interpret signed 24-bit value n as unsigned 24-bit for encoding.
   Input n must be in [-8388608, 8388607]."
  (declare (type (integer -8388608 8388607) n))
  (logand n #xffffff))

;;; ------------------------------------------------------------
;;; Core Encoding Functions
;;; ------------------------------------------------------------

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

;;; Closure & Upvalue

(defun encode-make-closure (dst proto-idx nupvals)
  "MAKE_CLOSURE dst, proto_idx, nupvals — create closure from prototype."
  (declare (type (unsigned-byte 8) dst proto-idx nupvals))
  (encode-3op +op-make-closure+ dst proto-idx nupvals))

(defun encode-get-upvalue (dst upval-idx)
  "GET_UPVALUE dst, upval_idx — load captured upvalue."
  (declare (type (unsigned-byte 8) dst upval-idx))
  (encode-2op +op-get-upvalue+ dst upval-idx 0))

(defun encode-set-upvalue (upval-idx src)
  "SET_UPVALUE upval_idx, src — store into captured upvalue."
  (declare (type (unsigned-byte 8) upval-idx src))
  (encode-2op +op-set-upvalue+ upval-idx src 0))

(defun encode-close-upvalue (reg)
  "CLOSE_UPVALUE reg — close over open upvalue at reg."
  (declare (type (unsigned-byte 8) reg))
  (encode-2op +op-close-upvalue+ reg 0 0))

;;; Object Access

(defun encode-get-slot (dst obj slot-idx)
  "GET_SLOT dst, obj, slot_idx — read object slot."
  (declare (type (unsigned-byte 8) dst obj slot-idx))
  (encode-3op +op-get-slot+ dst obj slot-idx))

(defun encode-set-slot (obj slot-idx src)
  "SET_SLOT obj, slot_idx, src — write object slot."
  (declare (type (unsigned-byte 8) obj slot-idx src))
  (encode-3op +op-set-slot+ obj slot-idx src))

(defun encode-get-global (dst name-idx)
  "GET_GLOBAL dst, name_idx — load global variable by name index."
  (declare (type (unsigned-byte 8) dst name-idx))
  (encode-2op +op-get-global+ dst name-idx 0))

(defun encode-set-global (name-idx src)
  "SET_GLOBAL name_idx, src — store global variable by name index."
  (declare (type (unsigned-byte 8) name-idx src))
  (encode-2op +op-set-global+ name-idx src 0))

(defun encode-make-instance (dst class-idx nargs)
  "MAKE_INSTANCE dst, class_idx, nargs — allocate CLOS instance."
  (declare (type (unsigned-byte 8) dst class-idx nargs))
  (encode-3op +op-make-instance+ dst class-idx nargs))

;;; Collections

(defun encode-cons (dst car-reg cdr-reg)
  "CONS dst, car, cdr — allocate cons cell."
  (declare (type (unsigned-byte 8) dst car-reg cdr-reg))
  (encode-3op +op-cons+ dst car-reg cdr-reg))

(defun encode-car (dst src)
  "CAR dst, src — extract car."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-car+ dst src 0))

(defun encode-cdr (dst src)
  "CDR dst, src — extract cdr."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-cdr+ dst src 0))

(defun encode-make-vector (dst size init)
  "MAKE_VECTOR dst, size, init — allocate vector of size elements initialized to init."
  (declare (type (unsigned-byte 8) dst size init))
  (encode-3op +op-make-vector+ dst size init))

(defun encode-vector-ref (dst vec idx)
  "VECTOR_REF dst, vec, idx — load vector element."
  (declare (type (unsigned-byte 8) dst vec idx))
  (encode-3op +op-vector-ref+ dst vec idx))

(defun encode-vector-set (vec idx src)
  "VECTOR_SET vec, idx, src — store vector element."
  (declare (type (unsigned-byte 8) vec idx src))
  (encode-3op +op-vector-set+ vec idx src))

(defun encode-make-hash (dst size)
  "MAKE_HASH dst, size — allocate hash table with hint size."
  (declare (type (unsigned-byte 8) dst size))
  (encode-2op +op-make-hash+ dst size 0))

(defun encode-hash-ref (dst ht key)
  "HASH_REF dst, ht, key — lookup key in hash table."
  (declare (type (unsigned-byte 8) dst ht key))
  (encode-3op +op-hash-ref+ dst ht key))

(defun encode-hash-set (ht key src)
  "HASH_SET ht, key, src — store key/value in hash table."
  (declare (type (unsigned-byte 8) ht key src))
  (encode-3op +op-hash-set+ ht key src))

;;; Type Check

(defun encode-type-check (dst src type-tag)
  "TYPE_CHECK dst, src, type_tag — general type predicate."
  (declare (type (unsigned-byte 8) dst src type-tag))
  (encode-3op +op-type-check+ dst src type-tag))

(defun encode-fixnump (dst src)
  "FIXNUMP dst, src — test if src is a fixnum."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-fixnump+ dst src 0))

(defun encode-consp (dst src)
  "CONSP dst, src — test if src is a cons."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-consp+ dst src 0))

(defun encode-symbolp (dst src)
  "SYMBOLP dst, src — test if src is a symbol."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-symbolp+ dst src 0))

(defun encode-functionp (dst src)
  "FUNCTIONP dst, src — test if src is a function."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-functionp+ dst src 0))

(defun encode-stringp (dst src)
  "STRINGP dst, src — test if src is a string."
  (declare (type (unsigned-byte 8) dst src))
  (encode-2op +op-stringp+ dst src 0))

;;; Multiple Values

(defun encode-values (nvals)
  "VALUES nvals — push nvals values onto the multiple-values stack."
  (declare (type (unsigned-byte 8) nvals))
  (encode-2op +op-values+ nvals 0 0))

(defun encode-recv-values (nvals)
  "RECV_VALUES nvals — receive nvals multiple values into registers."
  (declare (type (unsigned-byte 8) nvals))
  (encode-2op +op-recv-values+ nvals 0 0))

;;; Exception Handling

(defun encode-push-handler (handler-offset type-idx)
  "PUSH_HANDLER handler_offset, type_idx — push condition handler.
   Uses [opcode:8][type_idx:8][handler_offset:16] layout."
  (declare (type (unsigned-byte 8) type-idx)
           (type (integer -32768 32767) handler-offset))
  (encode-imm +op-push-handler+ type-idx handler-offset))

(defun encode-pop-handler ()
  "POP_HANDLER — pop topmost condition handler."
  (encode-2op +op-pop-handler+ 0 0 0))

(defun encode-signal (src)
  "SIGNAL src — signal condition in src."
  (declare (type (unsigned-byte 8) src))
  (encode-2op +op-signal+ src 0 0))

(defun encode-push-unwind (cleanup-offset)
  "PUSH_UNWIND cleanup_offset — push unwind-protect cleanup.
   offset is signed 24-bit."
  (declare (type (integer -8388608 8388607) cleanup-offset))
  (encode-branch +op-push-unwind+ cleanup-offset))

(defun encode-pop-unwind ()
  "POP_UNWIND — pop topmost unwind-protect frame."
  (encode-2op +op-pop-unwind+ 0 0 0))

;;; ------------------------------------------------------------
;;; Bytecode Chunk
;;; ------------------------------------------------------------

(defstruct (bytecode-chunk (:conc-name bytecode-chunk-))
  "A compiled function's bytecode: instruction array + constant pool."
  (code      (make-array 0 :element-type '(unsigned-byte 32))
             :type (simple-array (unsigned-byte 32) (*)))
  (constants #()
             :type simple-vector))

;;; ------------------------------------------------------------
;;; Assembler / Builder
;;; ------------------------------------------------------------

(defstruct (bytecode-builder (:conc-name bytecode-builder-))
  "Mutable builder for constructing a bytecode-chunk incrementally."
  (code      (make-array 64 :element-type '(unsigned-byte 32)
                         :adjustable t :fill-pointer 0)
             :type (array (unsigned-byte 32) (*)))
  (constants (make-array 16 :adjustable t :fill-pointer 0)
             :type (array t (*))))

(defun emit (instruction builder)
  "Append INSTRUCTION word to BUILDER's code stream.
   Returns the index of the newly appended instruction."
  (declare (type (unsigned-byte 32) instruction)
           (type bytecode-builder builder))
  (vector-push-extend instruction (bytecode-builder-code builder))
  (1- (fill-pointer (bytecode-builder-code builder))))

(defun emit-constant (value builder)
  "Append VALUE to BUILDER's constant pool.
   Returns the index of the newly appended constant."
  (declare (type bytecode-builder builder))
  (vector-push-extend value (bytecode-builder-constants builder))
  (1- (fill-pointer (bytecode-builder-constants builder))))

(defun build-bytecode (builder)
  "Finalize BUILDER into an immutable bytecode-chunk."
  (declare (type bytecode-builder builder))
  (let* ((code-vec  (bytecode-builder-code builder))
         (const-vec (bytecode-builder-constants builder))
         (n-code    (fill-pointer code-vec))
         (n-const   (fill-pointer const-vec))
         (code-arr  (make-array n-code :element-type '(unsigned-byte 32)))
         (const-arr (make-array n-const)))
    (dotimes (i n-code)
      (setf (aref code-arr i) (aref code-vec i)))
    (dotimes (i n-const)
      (setf (aref const-arr i) (aref const-vec i)))
    (make-bytecode-chunk :code code-arr :constants const-arr)))
