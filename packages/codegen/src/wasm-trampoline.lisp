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

