;;;; src/backend/wasm-trampoline.lisp - PC-Dispatch Trampoline Builder
;;;
;;; Converts a flat list of VM instructions (a function body) into a WAT
;;; body using a PC-dispatch trampoline: loop { block { br_table } }.
;;; This bridges the VM's flat label-jump model to WASM structured control flow.

(in-package :cl-cc)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 1: Group instructions into basic blocks by label
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct (wasm-basic-block (:conc-name wasm-bb-))
  "A basic block in the function: a label and its instructions."
  (label nil :type (or null string))   ; nil for the implicit entry block
  (pc-index nil :type (or null integer)) ; the $pc value that dispatches here
  (instructions nil :type list))       ; the instructions in this block

(defun group-into-basic-blocks (instructions)
  "Split INSTRUCTIONS into basic blocks at vm-label boundaries.
   Returns a list of wasm-basic-block structs in order."
  (let ((blocks nil)
        (current-label nil)
        (current-instrs nil)
        (pc-counter 0))
    (flet ((flush ()
             (when (or current-label current-instrs)
               (push (make-wasm-basic-block
                      :label current-label
                      :pc-index (prog1 pc-counter (incf pc-counter))
                      :instructions (nreverse current-instrs))
                     blocks)
               (setf current-label nil
                     current-instrs nil))))
      (dolist (inst instructions)
        (if (typep inst 'vm-label)
            (progn
              (flush)
              (setf current-label (vm-name inst)))
            (push inst current-instrs)))
      (flush))
    (nreverse blocks)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 2: Build a label -> pc-index map
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-label-pc-map (basic-blocks)
  "Build a hash table from label name (string) -> pc-index (integer)."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (bb basic-blocks map)
      (when (wasm-bb-label bb)
        (setf (gethash (wasm-bb-label bb) map) (wasm-bb-pc-index bb))))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dynamic calling-convention state
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *wasm-label-to-table-idx* nil
  "Dynamic binding: hash table mapping function entry-label name (string) to its
   WASM funcref table index (= wasm-func-index).  Bound in build-all-wasm-functions
   so that emit-trampoline-instruction can emit real table indices for vm-closure.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 3: Emit WAT for a single instruction
;;; ─────────────────────────────────────────────────────────────────────────────
;;; Returns T if instruction was handled, NIL if not supported (emits comment).
;;; REG-MAP is a wasm-reg-map for mapping :R0 etc. to local indices.
;;; LABEL-PC-MAP maps label names to pc-index integers.
;;; NUM-BLOCKS is total number of basic blocks.

(defun reg-local-ref (reg-map reg)
  "Return WAT for getting a register's local variable, e.g. '(local.get 3)'."
  (format nil "(local.get ~D)" (wasm-reg-to-local reg-map reg)))

(defun reg-local-set (reg-map reg value-wat)
  "Return WAT for setting a register's local variable."
  (format nil "(local.set ~D ~A)" (wasm-reg-to-local reg-map reg) value-wat))

(defun wasm-fixnum-unbox (reg-map reg)
  "Unbox a fixnum from i31ref to i64. Assumes reg holds an i31ref fixnum."
  (format nil "(i64.extend_i32_s (i31.get_s ~A))" (reg-local-ref reg-map reg)))

(defun wasm-fixnum-box (i64-wat)
  "Box an i64 as an i31ref fixnum."
  (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat))

(defun wasm-bool-to-i31 (cond-wat)
  "Convert a WASM i32 boolean (0/1) to i31ref (nil/t).
   0 -> ref.null eq (nil), 1 -> (ref.i31 (i32.const 1)) (truthy)."
  (format nil "(if (result eqref) ~A (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
          cond-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Fixnum operation helpers (used by emit-trampoline-instruction)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-binop (reg-map dst lhs rhs op)
  "WAT for: dst = box(op(unbox(lhs), unbox(rhs)))."
  (reg-local-set reg-map dst
                 (wasm-fixnum-box
                  (format nil "(~A ~A ~A)" op
                          (wasm-fixnum-unbox reg-map lhs)
                          (wasm-fixnum-unbox reg-map rhs)))))

(defun wasm-i64-cmp (reg-map dst lhs rhs cmp-op)
  "WAT for: dst = T if cmp-op(unbox(lhs), unbox(rhs)), else NIL."
  (reg-local-set reg-map dst
                 (wasm-bool-to-i31
                  (format nil "(~A ~A ~A)" cmp-op
                          (wasm-fixnum-unbox reg-map lhs)
                          (wasm-fixnum-unbox reg-map rhs)))))

(defun emit-trampoline-jump-to-label (label-name label-pc-map reg-map stream)
  "Emit WAT to set $pc to the index for LABEL-NAME and branch back to $dispatch."
  (let ((pc-idx (gethash label-name label-pc-map)))
    (if pc-idx
        (format stream "~%      (local.set ~D (i32.const ~D))"
                (wasm-reg-map-pc-index reg-map) pc-idx)
        (format stream "~%      ;; WARNING: unknown label ~S" label-name))
    (format stream "~%      (br $dispatch)")))

(defun emit-trampoline-instruction (inst label-pc-map reg-map num-blocks stream)
  "Emit WAT text for a single VM instruction to STREAM.
   Returns T if instruction was handled, NIL otherwise (emits warn comment)."
  (declare (ignore num-blocks))
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
    ;; ── Arithmetic: vm-add, vm-sub, vm-mul (subtypes of vm-binop) ──
    (vm-add
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-fixnum-box
                             (format nil "(i64.add ~A ~A)"
                                     (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                     (wasm-fixnum-unbox reg-map (vm-rhs inst))))))
     t)
    (vm-sub
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-fixnum-box
                             (format nil "(i64.sub ~A ~A)"
                                     (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                     (wasm-fixnum-unbox reg-map (vm-rhs inst))))))
     t)
    (vm-mul
     (format stream "~%      ~A"
             (reg-local-set reg-map (vm-dst inst)
                            (wasm-fixnum-box
                             (format nil "(i64.mul ~A ~A)"
                                     (wasm-fixnum-unbox reg-map (vm-lhs inst))
                                     (wasm-fixnum-unbox reg-map (vm-rhs inst))))))
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
    ;; ── Extended arithmetic ──
    (vm-div
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.div_s"))
     t)
    (vm-mod
     ;; CL mod is floor-based; i64.rem_s is truncate. For same-sign operands they match.
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.rem_s"))
     t)
    (vm-truncate
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.div_s"))
     t)
    (vm-neg
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (wasm-fixnum-box
                               (format nil "(i64.sub (i64.const 0) ~A)" src))))
       t))
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
    (vm-inc
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (wasm-fixnum-box
                               (format nil "(i64.add ~A (i64.const 1))" src))))
       t))
    (vm-dec
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (wasm-fixnum-box
                               (format nil "(i64.sub ~A (i64.const 1))" src))))
       t))
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
    ;; ── Bitwise operations ──
    (vm-logand
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.and"))
     t)
    (vm-logior
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.or"))
     t)
    (vm-logxor
     (format stream "~%      ~A"
             (wasm-i64-binop reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.xor"))
     t)
    (vm-lognot
     (let ((src (wasm-fixnum-unbox reg-map (vm-src inst))))
       (format stream "~%      ~A"
               (reg-local-set reg-map (vm-dst inst)
                              (wasm-fixnum-box
                               (format nil "(i64.xor ~A (i64.const -1))" src))))
       t))
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
    ;; ── Comparisons ──
    (vm-num-eq
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.eq"))
     t)
    (vm-eq
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.eq"))
     t)
    (vm-lt
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.lt_s"))
     t)
    (vm-gt
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.gt_s"))
     t)
    (vm-le
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.le_s"))
     t)
    (vm-ge
     (format stream "~%      ~A"
             (wasm-i64-cmp reg-map (vm-dst inst) (vm-lhs inst) (vm-rhs inst) "i64.ge_s"))
     t)
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

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 4: Build the trampoline body for a function
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-trampoline-body (basic-blocks label-pc-map reg-map param-regs stream)
  "Emit the WAT trampoline body (nested blocks + loop) to STREAM.
   BASIC-BLOCKS is the list of wasm-basic-block. REG-MAP is the register map.
   PARAM-REGS is a list of VM register keywords that are this function's parameters;
   they are loaded from the global $cl_argN calling-convention registers as a prologue."
  (let ((num-blocks (length basic-blocks)))
    (when (zerop num-blocks)
      (format stream "~%      ;; empty function body")
      (return-from build-trampoline-body))

    ;; Prologue: load function arguments from global calling-convention registers.
    ;; The caller stored args in $cl_arg0..$cl_argN before calling call_indirect.
    (loop for reg in param-regs for i from 0 do
      (format stream "~%      (local.set ~D (global.get $cl_arg~D))"
              (wasm-reg-to-local reg-map reg) i))

    ;; Initial pc = 0 (dispatch to block 0 first)
    (format stream "~%      (local.set ~D (i32.const 0))"
            (wasm-reg-map-pc-index reg-map))

    ;; Outer block: vm-ret/vm-halt break to $exit
    (format stream "~%      (block $exit (result eqref)")

    ;; Loop for re-dispatching after a jump
    (format stream "~%        (loop $dispatch (result eqref)")

    ;; Nested blocks in REVERSE order so block 0 is innermost.
    ;; WAT nesting: (block $blk_N-1 (block $blk_N-2 ... (block $blk_0 br_table)))
    ;; br_table targets are in ascending order: $blk_0, $blk_1, ..., $exit
    (dotimes (i num-blocks)
      (format stream "~%          (block $blk_~D (result eqref)" i))

    ;; The br_table dispatch instruction
    (format stream "~%            (br_table")
    (dotimes (i num-blocks)
      (format stream " $blk_~D" i))
    (format stream " $exit")   ; default target: exit (should not occur)
    (format stream "~%             (local.get ~D))" (wasm-reg-map-pc-index reg-map))

    ;; Close each nested block and emit its instructions.
    ;; Block i's closing paren comes after the br_table, then the instructions
    ;; for basic block i follow, then a branch back to $dispatch (except the last).
    ;;
    ;; The WAT layout after br_table resolution for block i is:
    ;;   ) ;; end block $blk_i
    ;;   <instructions for block i>
    ;;   (br $dispatch)   ;; unless last block
    (dotimes (i num-blocks)
      (format stream "~%          ) ;; end block $blk_~D" i)
      (let ((bb (nth i basic-blocks)))
        (dolist (inst (wasm-bb-instructions bb))
          (emit-trampoline-instruction inst label-pc-map reg-map num-blocks stream)))
      (unless (= i (1- num-blocks))
        (format stream "~%          (br $dispatch)")))

    ;; Close loop and outer block
    (format stream "~%        ) ;; end loop $dispatch")
    (format stream "~%      ) ;; end block $exit")

    ;; Fall-through value: return nil if no explicit vm-ret/vm-halt
    (format stream "~%      (ref.null eq)")))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 5: Collect all virtual registers used in an instruction list
;;; ─────────────────────────────────────────────────────────────────────────────

(defun collect-registers-from-instructions (instructions reg-map)
  "Pre-allocate local indices for all virtual registers used in INSTRUCTIONS."
  (dolist (inst instructions)
    (flet ((touch (reg)
             (when (and reg (keywordp reg))
               (wasm-reg-to-local reg-map reg))))
      (typecase inst
        (vm-const    (touch (vm-dst inst)))
        (vm-move     (touch (vm-dst inst)) (touch (vm-src inst)))
        (vm-binop    (touch (vm-dst inst)) (touch (vm-lhs inst)) (touch (vm-rhs inst)))
        (vm-jump-zero (touch (vm-reg inst)))
        (vm-ret      (touch (vm-reg inst)))
        (vm-halt     (touch (vm-reg inst)))
        (vm-print    (touch (vm-reg inst)))
        (vm-call     (touch (vm-dst inst)) (touch (vm-func-reg inst))
                     (dolist (a (vm-args inst)) (touch a)))
        (vm-closure  (touch (vm-dst inst)))
        (vm-cons     (touch (vm-dst inst)) (touch (vm-car-reg inst)) (touch (vm-cdr-reg inst)))
        (vm-car      (touch (vm-dst inst)) (touch (vm-src inst)))
        (vm-cdr      (touch (vm-dst inst)) (touch (vm-src inst)))
        (vm-null-p   (touch (vm-dst inst)) (touch (vm-src inst)))
        (vm-not      (touch (vm-dst inst)) (touch (vm-src inst)))
        (vm-get-global (touch (vm-dst inst)))
        (vm-set-global (touch (vm-src inst)))
        ;; Unary arithmetic (vm-neg, vm-abs, vm-inc, vm-dec, vm-lognot)
        ((or vm-neg vm-abs vm-inc vm-dec vm-lognot)
         (touch (vm-dst inst)) (touch (vm-src inst)))
        ;; Binary arithmetic not subclassed from vm-binop
        ((or vm-div vm-mod vm-truncate vm-floor-inst vm-ceiling-inst
             vm-min vm-max vm-logand vm-logior vm-logxor vm-ash)
         (touch (vm-dst inst)) (touch (vm-lhs inst)) (touch (vm-rhs inst)))
        ;; Comparisons
        ((or vm-num-eq vm-eq vm-lt vm-gt vm-le vm-ge)
         (touch (vm-dst inst)) (touch (vm-lhs inst)) (touch (vm-rhs inst)))
        (t nil)))))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 6: Build complete WAT body for a single wasm-function-def
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-wasm-function-wat (func-def)
  "Build the WAT trampoline body for FUNC-DEF.
   Populates the :body slot with a list containing a single WAT string.
   Returns the WAT string."
  (let* ((instructions (wasm-func-source-instructions func-def))
         (basic-blocks (group-into-basic-blocks instructions))
         (label-pc-map (build-label-pc-map basic-blocks))
         (num-params 0)  ; TODO: infer from closure captures
         (reg-map (make-wasm-reg-map-for-function num-params))
         (body-stream (make-string-output-stream)))
    ;; Pre-allocate locals for all registers that appear in this function
    (collect-registers-from-instructions instructions reg-map)
    ;; Emit the trampoline body (pass param-regs for the arg-load prologue)
    (build-trampoline-body basic-blocks label-pc-map reg-map
                           (wasm-func-params func-def) body-stream)
    (let ((body-str (get-output-stream-string body-stream)))
      (setf (wasm-func-body func-def) (list body-str))
      body-str)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main: build-all-wasm-functions
;;; ─────────────────────────────────────────────────────────────────────────────

(defun build-all-wasm-functions (module)
  "Build WAT bodies for all functions in MODULE (mutates each wasm-function-def).
   Sets up the label-to-table-index map used by vm-closure codegen and updates the
   module's table size so the funcref_table is large enough for all functions.
   Returns MODULE."
  ;; Build a label-name -> table-index hash table.
  ;; Each function's table index equals its wasm-func-index (sequential from 0).
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (func (wasm-module-functions module))
      (let ((wat-name (wasm-func-wat-name func)))
        (when (and wat-name (> (length wat-name) 1))
          ;; Strip the leading $ from the WAT name to get the label name
          (setf (gethash (subseq wat-name 1) tbl) (wasm-func-index func)))))
    ;; Update the module's table size so emit-wat-table declares enough entries
    (setf (wasm-module-table-size module) (length (wasm-module-functions module)))
    ;; Bind the table for use by emit-trampoline-instruction during body building
    (let ((*wasm-label-to-table-idx* tbl))
      (dolist (func (wasm-module-functions module) module)
        (build-wasm-function-wat func)))))
