;;;; packages/emit/src/wasm-trampoline.lisp - PC-Dispatch Trampoline Builder
;;;
;;; Converts a flat list of VM instructions (a function body) into a WAT
;;; body using a PC-dispatch trampoline: loop { block { br_table } }.
;;; This bridges the VM's flat label-jump model to WASM structured control flow.

(in-package :cl-cc/codegen)

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
  (let ((dst (wasm-reg-to-local reg-map reg)))
    (if (and (stringp value-wat)
             (search "(local.get " value-wat)
             (= (position #\( value-wat) 0))
        (format nil "(local.tee ~D ~A)" dst value-wat)
        (format nil "(local.set ~D ~A)" dst value-wat))))

(defun wasm-fixnum-unbox (reg-map reg)
  "Unbox a fixnum from i31ref to i64. Assumes reg holds an i31ref fixnum.

FR-145: When the register is known to hold an already-unboxed integer value
(via wasm-fixnum-unboxed-reg-p), return the raw local.get directly without
the i31.get_s → i64.extend_i32_s unboxing sequence."
  (if (wasm-fixnum-unboxed-reg-p reg-map reg)
      (reg-local-ref reg-map reg)
      (format nil "(i64.extend_i32_s (i31.get_s ~A))" (reg-local-ref reg-map reg))))

(defun wasm-fixnum-box (i64-wat)
  "Box an i64 as an i31ref fixnum."
  (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-145: Integer Range Annotation — fixnum unboxed register tracking
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *wasm-fixnum-unboxed-regs* nil
  "Dynamic binding: hash table mapping VM register keyword → integer constant value
   when the register holds a known fixnum constant that can be used as raw i64.
   Set by vm-const emit when the const value is an integer (FR-145).")

(defun wasm-fixnum-unboxed-reg-p (reg-map reg)
  "Return the integer constant REG holds, or NIL if not a known fixnum constant.
FR-145: Checks *wasm-fixnum-unboxed-regs* table for the register."
  (declare (ignore reg-map))
  (and *wasm-fixnum-unboxed-regs*
       (gethash reg *wasm-fixnum-unboxed-regs*)))

(defun wasm-mark-reg-unboxed-fixnum (reg value)
  "Mark REG as holding an unboxed i64 constant VALUE (FR-145)."
  (when *wasm-fixnum-unboxed-regs*
    (setf (gethash reg *wasm-fixnum-unboxed-regs*) value)))

(defun wasm-clear-reg-unboxed-fixnum (reg)
  "Clear the unboxed-fixnum mark for REG (FR-145)."
  (when *wasm-fixnum-unboxed-regs*
    (remhash reg *wasm-fixnum-unboxed-regs*)))

(defun wasm-bool-to-i31 (cond-wat)
  "Convert a WASM i32 boolean (0/1) to i31ref (nil/t).
   0 -> ref.null eq (nil), 1 -> (ref.i31 (i32.const 1)) (truthy)."
  (format nil "(if (result eqref) ~A (then (ref.i31 (i32.const 1))) (else (ref.null eq)))"
          cond-wat))

(defun %wasm-captured-value-reg (capture)
  "Return the VM register carrying CAPTURE's value."
  (if (consp capture) (cdr capture) capture))

(defun emit-wasm-closure-allocation (reg-map dst entry-index captured stream indent)
  "Emit closure allocation using $closure_t and $env_t GC structs.

Captured values are materialized into a mutable $eqref_array_t with array.new and
array.set before the closure struct is created.

FR-142: Eliminate redundant ref.cast by using local.tee pattern.  When array.new
returns (ref $eqref_array_t), we use local.tee to keep the typed ref available on
the stack for immediate use, avoiding one ref.cast per closure construction.

FR-144: Use typed closure environment array via array.new_fixed eqref for direct
index access instead of the intermediate $env_t struct wrapper."
  (let ((prefix (make-string indent :initial-element #\Space)))
    (if captured
        (let ((tmp (wasm-reg-map-tmp-index reg-map))
              (first-reg (%wasm-captured-value-reg (first captured))))
          ;; FR-142+FR-144: Build env array inline, then create closure struct.
          ;; Single capture: array.new_fixed $eqref_array_t 1 val inline.
          ;; Multiple captures: local.set + array.set loop (must use ref.cast for
          ;; eqref→typed downcast on each array.set, per Wasm GC spec).
          (if (= (length captured) 1)
              (format stream "~%~A~A"
                      prefix
                      (reg-local-set reg-map dst
                                     (format nil "(struct.new $closure_t (i32.const ~D) (struct.new $env_t (array.new_fixed $eqref_array_t 1 ~A) (ref.null $env_t)))"
                                             entry-index (reg-local-ref reg-map first-reg))))
              (progn
                ;; FR-142: Use local.tee to avoid ref.cast on first array.set.
                ;; The array.new result, typed as (ref $eqref_array_t), stays on
                ;; the stack after local.tee for immediate consumption.
                (format stream "~%~A(array.set $eqref_array_t (local.tee ~D (array.new $eqref_array_t (ref.null eq) (i32.const ~D))) (i32.const 0) ~A)"
                        prefix tmp (length captured)
                        (reg-local-ref reg-map first-reg))
                (loop for capture in (cdr captured)
                      for idx from 1
                      for reg = (%wasm-captured-value-reg capture)
                      do (format stream "~%~A(array.set $eqref_array_t (ref.cast (ref $eqref_array_t) (local.get ~D)) (i32.const ~D) ~A)"
                                 prefix tmp idx (reg-local-ref reg-map reg)))
                (format stream "~%~A~A"
                        prefix
                        (reg-local-set reg-map dst
                                       (format nil "(struct.new $closure_t (i32.const ~D) (struct.new $env_t (local.get ~D) (ref.null $env_t)))"
                                               entry-index tmp))))))
        (format stream "~%~A~A"
                prefix
                (reg-local-set
                 reg-map dst
                 (format nil "(struct.new $closure_t (i32.const ~D) (ref.null $env_t))"
                         entry-index))))))

(defun wasm-closure-ref-wat (reg-map closure-reg index)
  "Return WAT for reading captured INDEX from CLOSURE-REG.

FR-144: Typed closure environment array access.  The closure struct's env field
is now a typed (ref $eqref_array_t) through the $env_t wrapper.  We use
struct.get to read the array, then array.get for direct indexed access — no
hash-table lookup needed for closure environment reads."
  (format nil "(array.get $eqref_array_t (struct.get $env_t 0 (ref.cast (ref $env_t) (struct.get $closure_t 1 (ref.cast (ref $closure_t) ~A)))) (i32.const ~D))"
          (reg-local-ref reg-map closure-reg)
          index))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Fixnum operation helpers (used by emit-trampoline-instruction)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun wasm-i64-binop (reg-map dst lhs rhs op)
  "WAT for: dst = box(op(unbox(lhs), unbox(rhs))).

FR-145: When lhs and/or rhs are known to hold fixnum-constant values (tracked via
*wasm-fixnum-unboxed-regs*), emit i64.const directly instead of going through
the local variable, avoiding unnecessary boxing/unboxing cycles."
  (flet ((unbox-reg (reg)
           (let ((const-val (wasm-fixnum-unboxed-reg-p reg-map reg)))
             (if const-val
                 (format nil "(i64.const ~D)" const-val)
                 (wasm-fixnum-unbox reg-map reg)))))
    (reg-local-set reg-map dst
                   (wasm-fixnum-box
                    (format nil "(~A ~A ~A)" op
                            (unbox-reg lhs)
                            (unbox-reg rhs))))))

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
