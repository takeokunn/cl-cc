;;;; packages/backend/emit/src/wasm-trampoline-tables.lisp — WASM Trampoline Data Tables
;;;;
;;;; Pure data declarations (alist → hash-table) for WASM emission dispatch.
;;;; Adding a new VM instruction type only requires a single data entry here.
;;;;
;;;; Defines: *wasm-i64-binop-table*, *wasm-i64-cmp-table*, *wasm-unary-fixnum-table*,
;;;;           *wasm-minmax-table*, *wasm-struct-get-table*, *wasm-binop-dispatch*
;;;; Helpers: %make-eq-hash-table, %wasm-const-value-to-wat, %wasm-if-eqref
;;;;
;;;; Load order: after wasm-trampoline.lisp, before wasm-trampoline-emit.lisp.

(in-package :cl-cc/emit)

(defun %make-eq-hash-table (alist)
  "Build an EQ hash-table from ALIST ((key . value) ...) or ((key value) ...)."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht) (if (consp (cdr pair)) (cadr pair) (cdr pair))))))

;;; Binary i64 arithmetic/logic — 12 instruction types
(defparameter *wasm-i64-binop-table*
  (%make-eq-hash-table
   '((vm-add         . "i64.add")
     (vm-integer-add . "i64.add")
     (vm-sub         . "i64.sub")
     (vm-integer-sub . "i64.sub")
     (vm-mul         . "i64.mul")
     (vm-integer-mul . "i64.mul")
     (vm-div         . "i64.div_s")
     (vm-mod         . "i64.rem_s")
     (vm-truncate    . "i64.div_s")
     (vm-logand      . "i64.and")
     (vm-logior      . "i64.or")
     (vm-logxor      . "i64.xor")))
  "Maps binary VM instruction types to WASM i64 opcode strings.")

;;; Comparison → boolean — 6 instruction types
(defparameter *wasm-i64-cmp-table*
  (%make-eq-hash-table
   '((vm-num-eq . "i64.eq")
     (vm-eq     . "i64.eq")
     (vm-lt     . "i64.lt_s")
     (vm-gt     . "i64.gt_s")
     (vm-le     . "i64.le_s")
     (vm-ge     . "i64.ge_s")))
  "Maps comparison VM instruction types to WASM i64 opcode strings.")

;;; Unary fixnum — 5 instruction types; ~A expands to the unboxed source operand
(defparameter *wasm-unary-fixnum-table*
  (%make-eq-hash-table
   '((vm-inc      . "(i64.add ~A (i64.const 1))")
     (vm-dec      . "(i64.sub ~A (i64.const 1))")
     (vm-neg      . "(i64.sub (i64.const 0) ~A)")
     (vm-lognot   . "(i64.xor ~A (i64.const -1))")
     (vm-logcount . "(i64.popcnt ~A)")))
  "Maps unary VM instruction types to WASM i64 format strings (~A = unboxed src).")

;;; Min/max — binary instructions using a conditional select pattern
(defparameter *wasm-minmax-table*
  (%make-eq-hash-table
   '((vm-min . "i64.le_s")
     (vm-max . "i64.ge_s")))
  "Maps min/max VM instruction types to WASM comparison opcodes.")

;;; Struct field access — unary instructions that read from a cons cell field
(defparameter *wasm-struct-get-table*
  (%make-eq-hash-table
   '((vm-car . "(struct.get $cons_t 0 ~A)")
     (vm-cdr . "(struct.get $cons_t 1 ~A)")))
  "Maps cons accessor VM instruction types to WASM struct.get format strings (~A = src).")

;;; Ordered dispatch list: (table . emit-fn) for binary instruction dispatch.
(defparameter *wasm-binop-dispatch*
  (list (cons *wasm-i64-binop-table* #'wasm-i64-binop)
        (cons *wasm-i64-cmp-table*   #'wasm-i64-cmp))
  "Ordered list of (table . emit-fn) for binary instruction dispatch.")

(defun %wasm-const-value-to-wat (val)
  "Return the WASM WAT string for a literal constant value VAL."
  (typecase val
    (integer (wasm-fixnum-box (format nil "(i64.const ~D)" val)))
    (null    "(ref.null eq)")
    ((eql t) "(ref.i31 (i32.const 1))")
    (string  (error "Unsupported WASM trampoline string constant: ~S" val))
    (t       (error "Unsupported WASM trampoline constant: ~S" val))))

(defun %wasm-if-eqref (cond-wat then-wat else-wat)
  "Return a WASM (if (result eqref) cond (then ...) (else ...)) WAT string."
  (format nil "(if (result eqref) ~A (then ~A) (else ~A))"
          cond-wat then-wat else-wat))
