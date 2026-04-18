;;;; packages/backend/emit/src/wasm-trampoline-emit.lisp - WASM Trampoline — Instruction Emitter + Program Builder
;;;;
;;;; Contains: *wasm-i64-binop-table*, *wasm-i64-cmp-table*, *wasm-unary-fixnum-table*
;;;; (data-driven emission tables), emit-trampoline-instruction (dispatch over all
;;;; VM instruction types), build-trampoline-body (Steps 4), collect-registers-from-instructions
;;;; (Step 5), build-wasm-function-wat + build-all-wasm-functions (Steps 6).
;;;;
;;;; wasm-basic-block struct, group-into-basic-blocks, build-label-pc-map, reg helpers,
;;;; and emit-trampoline-jump-to-label are in wasm-trampoline.lisp (loads before).
;;;;
;;;; Load order: after wasm-trampoline.lisp.

(in-package :cl-cc/emit)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Data-Driven WASM Emission Tables
;;;
;;; Each table is a pure data declaration (alist → hash-table) so that adding a
;;; new VM instruction type only requires a single line of data, not code.
;;; This follows the Prolog "fact database" pattern used throughout the compiler.
;;; ─────────────────────────────────────────────────────────────────────────────

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
     (vm-mod         . "i64.rem_s")    ; CL mod ≈ i64.rem_s for same-sign args
     (vm-truncate    . "i64.div_s")
     (vm-logand      . "i64.and")
     (vm-logior      . "i64.or")
     (vm-logxor      . "i64.xor")))
  "Maps binary VM instruction types to WASM i64 opcode strings.
All entries use the unbox-op-box pattern via wasm-i64-binop.")

;;; Comparison → boolean — 6 instruction types
(defparameter *wasm-i64-cmp-table*
  (%make-eq-hash-table
   '((vm-num-eq . "i64.eq")
     (vm-eq     . "i64.eq")
     (vm-lt     . "i64.lt_s")
     (vm-gt     . "i64.gt_s")
     (vm-le     . "i64.le_s")
     (vm-ge     . "i64.ge_s")))
  "Maps comparison VM instruction types to WASM i64 opcode strings.
All entries use the unbox-cmp-bool pattern via wasm-i64-cmp.")

;;; Unary fixnum — 5 instruction types; ~A expands to the unboxed source operand
(defparameter *wasm-unary-fixnum-table*
  (%make-eq-hash-table
   '((vm-inc      . "(i64.add ~A (i64.const 1))")
     (vm-dec      . "(i64.sub ~A (i64.const 1))")
     (vm-neg      . "(i64.sub (i64.const 0) ~A)")
     (vm-lognot   . "(i64.xor ~A (i64.const -1))")
     (vm-logcount . "(i64.popcnt ~A)")))
  "Maps unary VM instruction types to WASM i64 format strings (~A = unboxed src).
All entries use the unbox-op-box pattern.")

;;; Table-dispatch helpers for emit-trampoline-instruction.
;;; Each entry: (table . emit-fn) where emit-fn is called with (reg-map dst lhs rhs op).
(defparameter *wasm-binop-dispatch*
  (list (cons *wasm-i64-binop-table* #'wasm-i64-binop)
        (cons *wasm-i64-cmp-table*   #'wasm-i64-cmp))
  "Ordered list of (table . emit-fn) for binary instruction dispatch.")

(defun emit-trampoline-instruction (inst label-pc-map reg-map num-blocks stream)
  "Emit WAT text for a single VM instruction to STREAM.
   Returns T if instruction was handled, NIL otherwise (emits warn comment)."
  (declare (ignore num-blocks))
  ;; Data-driven dispatch: check tables before falling through to typecase
  (let ((tp (type-of inst)))
    ;; Binary i64 and comparison operations via shared dispatch
    (loop for (table . emit-fn) in *wasm-binop-dispatch*
          for op = (gethash tp table)
          when op
            do (format stream "~%      ~A"
                        (funcall emit-fn reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) op))
               (return-from emit-trampoline-instruction t))
    ;; Unary fixnum operations (4 instruction types)
    (let ((unary-fmt (gethash tp *wasm-unary-fixnum-table*)))
      (when unary-fmt
        (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
          (format stream "~%      ~A"
                  (reg-local-set reg-map (vm-dst inst)
                                 (wasm-fixnum-box (format nil unary-fmt src)))))
        (return-from emit-trampoline-instruction t))))
  ;; Remaining instructions handled by typecase (unique logic per instruction)
  (typecase inst
    ;; ── Constant ──
    (vm-const
     (let* ((val (vm-value inst))
            (dst (vm-dst inst))
            (wat-val
              (typecase val
                (integer
                 (wasm-fixnum-box (format nil "(i64.const ~D)" val)))
                (null "(ref.null eq)")
                ((eql t) "(ref.i31 (i32.const 1))")
                (string
                 ;; String literal: stub with null for now
                 (format nil ";; UNSUPPORTED STRING CONST ~S~%(ref.null eq)" val))
                (t
                 (format nil ";; UNSUPPORTED CONST ~S~%(ref.null eq)" val)))))
       (format stream "~%      ~A"
               (reg-local-set reg-map dst wat-val))
       t))
    ;; ── Move ──
    (vm-move
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (reg-local-ref reg-map (vm-src inst))))
     t)
    ;; ── Unconditional jump ──
    (vm-jump
     (emit-trampoline-jump-to-label (vm-label-name inst) label-pc-map reg-map stream)
     t)
    ;; ── Conditional jump (jump if register is nil/falsy) ──
    (vm-jump-zero
     (let ((pc-idx (gethash (vm-label-name inst) label-pc-map)))
       (unless pc-idx
         (format stream "~%      ;; WARNING: unknown label ~S in jump-zero"
                 (vm-label-name inst)))
       (format stream
               "~%      (if (ref.is_null ~A)~%        (then (local.set ~D (i32.const ~D)) (br $dispatch)))"
               (reg-local-ref reg-map (vm-reg inst))
               (wasm-reg-map-pc-index reg-map)
               (or pc-idx 0)))
     t)
    ;; ── Return ──
    (vm-ret
     (format stream "~%      ~A" (reg-local-ref reg-map (vm-reg inst)))
     (format stream "~%      (br $exit)")
     t)
    ;; ── Halt ──
    (vm-halt
     (format stream "~%      ~A" (reg-local-ref reg-map (vm-reg inst)))
     (format stream "~%      (br $exit)")
     t)
    ;; ── Global variable access ──
    (vm-get-global
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil "(global.get ~A)"
                                    (vm-global-wat-name (vm-global-name inst)))))
     t)
    (vm-set-global
     (format stream "~%      (global.set ~A ~A)"
             (vm-global-wat-name (vm-global-name inst))
             (reg-local-ref reg-map (vm-src inst)))
     t)
    ;; ── Abs (unique: conditional branch) ──
    (vm-abs
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(if (result eqref) (i64.ge_s ~A (i64.const 0)) (then ~A) (else ~A))"
                                      src
                                      (wasm-fixnum-box src)
                                       (wasm-fixnum-box
                                        (format nil "(i64.sub (i64.const 0) ~A)" src)))))
       t))
    ;; ── Integer length (zero-special cased clz lowering) ──
    (vm-integer-length
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil
                                      "(if (result eqref) (i64.eqz ~A) (then ~A) (else ~A))"
                                      src
                                      (wasm-fixnum-box "(i64.const 0)")
                                      (wasm-fixnum-box
                                       (format nil "(i64.sub (i64.const 64) (i64.clz ~A))" src)))))
       t))
    ;; ── Min/max (unique: conditional select) ──
    (vm-min
     (let ((l (wasm-fixnum-unbox reg-map (vm-lhs inst)))
           (r (wasm-fixnum-unbox reg-map (vm-rhs inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(if (result eqref) (i64.le_s ~A ~A) (then ~A) (else ~A))"
                                      l r (wasm-fixnum-box l) (wasm-fixnum-box r))))
       t))
    (vm-max
     (let ((l (wasm-fixnum-unbox reg-map (vm-lhs inst)))
           (r (wasm-fixnum-unbox reg-map (vm-rhs inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(if (result eqref) (i64.ge_s ~A ~A) (then ~A) (else ~A))"
                                      l r (wasm-fixnum-box l) (wasm-fixnum-box r))))
       t))
    ;; ── Ash (unique: conditional shift direction) ──
    (vm-ash
     (let ((lhs (wasm-fixnum-unbox reg-map (vm-lhs inst)))
           (rhs (wasm-fixnum-unbox reg-map (vm-rhs inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(if (result eqref) (i64.ge_s ~A (i64.const 0)) (then ~A) (else ~A))"
                                      rhs
                                      (wasm-fixnum-box (format nil "(i64.shl ~A ~A)" lhs rhs))
                                      (wasm-fixnum-box (format nil "(i64.shr_s ~A (i64.sub (i64.const 0) ~A))" lhs rhs)))))
       t))
    ;; ── Print ──
    ;; Delegate to the $host_print_val import.  The host runtime calls its
    ;; native print/write on the eqref value.
    (vm-print
     (format stream "~%      (call $host_print_val ~A)"
             (reg-local-ref reg-map (vm-reg inst)))
     t)
    ;; ── Cons operations ──
    (vm-cons
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil "(struct.new $cons_t ~A ~A)"
                                    (reg-local-ref reg-map (vm-car-reg inst))
                                    (reg-local-ref reg-map (vm-cdr-reg inst)))))
     t)
    (vm-car
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil "(struct.get $cons_t 0 ~A)"
                                    (reg-local-ref reg-map (vm-src inst)))))
     t)
    (vm-cdr
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil "(struct.get $cons_t 1 ~A)"
                                    (reg-local-ref reg-map (vm-src inst)))))
     t)
    ;; ── Closure creation ──
    ;; Allocate a $closure_t struct with the real funcref table index for this
    ;; entry label.  *wasm-label-to-table-idx* is bound by build-all-wasm-functions.
    (vm-closure
     (let* ((label (vm-label-name inst))
            (table-idx (if (and *wasm-label-to-table-idx* label)
                           (gethash label *wasm-label-to-table-idx* 0)
                           0)))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                                      table-idx)))
       t))
    ;; ── Function call ──
    ;; Calling convention: args are written to globals $cl_arg0..$cl_argN before
    ;; call_indirect, and the callee's prologue loads them into its registers.
    (vm-call
     (let* ((args (vm-args inst))
            (func-local (reg-local-ref reg-map (vm-func-reg inst)))
            (dst-idx (wasm-reg-to-local reg-map (vm-dst inst)))
            ;; Cast the function register to $closure_t and extract the table index
            (entry-idx-wat (format nil
                                   "(struct.get $closure_t 0 (ref.cast (ref $closure_t) ~A))"
                                   func-local)))
       ;; Store each argument in the global arg-passing registers
       (loop for arg in args for i from 0 do
         (format stream "~%      (global.set $cl_arg~D ~A)" i (reg-local-ref reg-map arg)))
       ;; Dispatch to the function via call_indirect on $funcref_table
       (format stream "~%      (local.set ~D (call_indirect (type $main_func_t) (table $funcref_table) ~A))"
               dst-idx entry-idx-wat)
       t))
    ;; ── Null predicate ──
    (vm-null-p
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-bool-to-i31
                             (format nil "(ref.is_null ~A)"
                                     (reg-local-ref reg-map (vm-src inst))))))
     t)
    ;; ── Not ──
    (vm-not
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (format nil
                                    "(if (result eqref) (ref.is_null ~A) (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
                                    (reg-local-ref reg-map (vm-src inst)))))
     t)
    ;; ── Default: unsupported instruction ──
    (t
     (format stream "~%      ;; UNSUPPORTED: ~A" (type-of inst))
     nil)))


;;; (build-trampoline-body, collect-registers-from-instructions,
;;;  build-wasm-function-wat, and build-all-wasm-functions
;;;  are in wasm-trampoline-build.lisp which loads after this file.)

