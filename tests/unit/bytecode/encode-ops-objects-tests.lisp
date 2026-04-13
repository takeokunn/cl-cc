;;;; tests/unit/bytecode/encode-ops-objects-tests.lisp
;;;; Coverage for src/bytecode/encode-ops-objects.lisp:
;;;;   closure/upvalue ops, object/slot ops, collections,
;;;;   type-check predicates, multiple-values, exception ops.

(in-package :cl-cc/test)

(defsuite bytecode-encode-objects-suite
  :description "Bytecode encoder tests for closures, objects, collections, type checks, and exceptions"
  :parent cl-cc-suite)

(in-suite bytecode-encode-objects-suite)

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defmacro assert-opcode= (expected-op word)
  `(assert-= ,expected-op (ldb (byte 8 24) ,word)))

(defmacro assert-dst= (expected word)
  `(assert-= ,expected (ldb (byte 8 16) ,word)))

(defmacro assert-src1= (expected word)
  `(assert-= ,expected (ldb (byte 8  8) ,word)))

(defmacro assert-src2= (expected word)
  `(assert-= ,expected (ldb (byte 8  0) ,word)))

;;; ─── Closure & Upvalue Encoders ──────────────────────────────────────────

(deftest-each closure-upvalue-encoders
  "Closure and upvalue instruction opcodes land in bits[31:24]."
  :cases (("make-closure"   (cl-cc/bytecode:encode-make-closure  1 2 3) cl-cc/bytecode:+op-make-closure+)
          ("get-upvalue"    (cl-cc/bytecode:encode-get-upvalue   3 4)   cl-cc/bytecode:+op-get-upvalue+)
          ("set-upvalue"    (cl-cc/bytecode:encode-set-upvalue   4 5)   cl-cc/bytecode:+op-set-upvalue+)
          ("close-upvalue"  (cl-cc/bytecode:encode-close-upvalue 6)     cl-cc/bytecode:+op-close-upvalue+))
  (word expected-op)
  (assert-opcode= expected-op word))

(deftest encode-make-closure-field-layout
  "encode-make-closure packs dst/proto-idx/nupvals in 3op layout."
  (let ((w (cl-cc/bytecode:encode-make-closure 1 2 3)))
    (assert-dst=  1 w)
    (assert-src1= 2 w)
    (assert-src2= 3 w)))

(deftest encode-get-upvalue-field-layout
  "encode-get-upvalue places register in dst and index in src1."
  (let ((w (cl-cc/bytecode:encode-get-upvalue 5 7)))
    (assert-dst=  5 w)
    (assert-src1= 7 w)))

;;; ─── Object/Slot Encoders ────────────────────────────────────────────────

(deftest-each object-slot-encoders-opcodes
  "Object/slot/global instruction opcodes land in bits[31:24]."
  :cases (("get-slot"     (cl-cc/bytecode:encode-get-slot      1 2 3) cl-cc/bytecode:+op-get-slot+)
          ("set-slot"     (cl-cc/bytecode:encode-set-slot      2 3 4) cl-cc/bytecode:+op-set-slot+)
          ("get-global"   (cl-cc/bytecode:encode-get-global    5 6)   cl-cc/bytecode:+op-get-global+)
          ("set-global"   (cl-cc/bytecode:encode-set-global    6 7)   cl-cc/bytecode:+op-set-global+)
          ("make-instance" (cl-cc/bytecode:encode-make-instance 1 2 3) cl-cc/bytecode:+op-make-instance+))
  (word expected-op)
  (assert-opcode= expected-op word))

;;; ─── Collection Encoders ─────────────────────────────────────────────────

(deftest-each collection-encoders-opcodes
  "Collection instruction opcodes land in bits[31:24]."
  :cases (("cons"        (cl-cc/bytecode:encode-cons        1 2 3) cl-cc/bytecode:+op-cons+)
          ("car"         (cl-cc/bytecode:encode-car         1 2)   cl-cc/bytecode:+op-car+)
          ("cdr"         (cl-cc/bytecode:encode-cdr         1 2)   cl-cc/bytecode:+op-cdr+)
          ("make-vector" (cl-cc/bytecode:encode-make-vector 1 2 3) cl-cc/bytecode:+op-make-vector+)
          ("vector-ref"  (cl-cc/bytecode:encode-vector-ref  1 2 3) cl-cc/bytecode:+op-vector-ref+)
          ("vector-set"  (cl-cc/bytecode:encode-vector-set  1 2 3) cl-cc/bytecode:+op-vector-set+)
          ("make-hash"   (cl-cc/bytecode:encode-make-hash   1 4)   cl-cc/bytecode:+op-make-hash+)
          ("hash-ref"    (cl-cc/bytecode:encode-hash-ref    1 2 3) cl-cc/bytecode:+op-hash-ref+)
          ("hash-set"    (cl-cc/bytecode:encode-hash-set    1 2 3) cl-cc/bytecode:+op-hash-set+))
  (word expected-op)
  (assert-opcode= expected-op word))

(deftest encode-cons-field-layout
  "encode-cons packs dst/car/cdr in 3op layout."
  (let ((w (cl-cc/bytecode:encode-cons 0 1 2)))
    (assert-dst=  0 w)
    (assert-src1= 1 w)
    (assert-src2= 2 w)))

;;; ─── Type Check Predicates ───────────────────────────────────────────────

(deftest-each type-predicate-encoders-opcodes
  "Type predicate instruction opcodes land in bits[31:24]."
  :cases (("type-check" (cl-cc/bytecode:encode-type-check 1 2 3) cl-cc/bytecode:+op-type-check+)
          ("fixnump"    (cl-cc/bytecode:encode-fixnump    1 2)   cl-cc/bytecode:+op-fixnump+)
          ("consp"      (cl-cc/bytecode:encode-consp      1 2)   cl-cc/bytecode:+op-consp+)
          ("symbolp"    (cl-cc/bytecode:encode-symbolp    1 2)   cl-cc/bytecode:+op-symbolp+)
          ("functionp"  (cl-cc/bytecode:encode-functionp  1 2)   cl-cc/bytecode:+op-functionp+)
          ("stringp"    (cl-cc/bytecode:encode-stringp    1 2)   cl-cc/bytecode:+op-stringp+))
  (word expected-op)
  (assert-opcode= expected-op word))

;;; ─── Multiple Values ─────────────────────────────────────────────────────

(deftest encode-values-places-nvals-in-dst
  "encode-values places nvals in the opcode high byte (dst field)."
  ;; encode-values(nvals) → encode-2op op-values nvals 0 0
  ;; so opcode is in bits[31:24] and nvals is in bits[23:16]
  (let ((w (cl-cc/bytecode:encode-values 3)))
    (assert-opcode= cl-cc/bytecode:+op-values+ w)
    (assert-dst= 3 w)))

(deftest encode-recv-values-places-nvals-in-dst
  "encode-recv-values places nvals in the dst field."
  (let ((w (cl-cc/bytecode:encode-recv-values 2)))
    (assert-opcode= cl-cc/bytecode:+op-recv-values+ w)
    (assert-dst= 2 w)))

;;; ─── Exception Handling ──────────────────────────────────────────────────

(deftest encode-push-handler-opcode
  "encode-push-handler places the expected opcode in bits[31:24]."
  (let ((w (cl-cc/bytecode:encode-push-handler 10 0)))
    (assert-opcode= cl-cc/bytecode:+op-push-handler+ w)))

(deftest encode-pop-handler-is-zero-operands
  "encode-pop-handler produces opcode-only instruction (all lower bits zero)."
  (let ((w (cl-cc/bytecode:encode-pop-handler)))
    (assert-opcode= cl-cc/bytecode:+op-pop-handler+ w)
    (assert-= 0 (ldb (byte 24 0) w))))

(deftest encode-signal-opcode
  "encode-signal places op-signal in bits[31:24] and src in bits[23:16]."
  (let ((w (cl-cc/bytecode:encode-signal 5)))
    (assert-opcode= cl-cc/bytecode:+op-signal+ w)
    (assert-dst= 5 w)))

(deftest encode-push-unwind-opcode
  "encode-push-unwind places op-push-unwind in bits[31:24]."
  (let ((w (cl-cc/bytecode:encode-push-unwind 100)))
    (assert-opcode= cl-cc/bytecode:+op-push-unwind+ w)))
