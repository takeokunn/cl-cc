;;;; packages/bytecode/src/encode-ops-objects.lisp - CL-CC Bytecode Encoders: Closures, Objects, Builder
;;;;
;;;; Per-opcode encode-* helpers for: closure & upvalue operations,
;;;; object slot access, collections (cons/vector/hash), type checks,
;;;; multiple values, exception handling; plus bytecode-chunk struct,
;;;; bytecode-builder struct, emit, emit-constant, and build-bytecode.
;;;;
;;;; Basic-operation encoders (load/move, arithmetic, comparison, control flow)
;;;; are in encode-ops.lisp (loads before this file).
;;;;
;;;; Load order: after encode-ops.lisp.

(in-package :cl-cc/bytecode)

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
