;;;; src/emit/wasm-trampoline-build.lisp — WASM Trampoline: body builder + module assembler
;;;
;;; Contains:
;;;   - build-trampoline-body (Step 4: nested blocks + loop + br_table WAT emission)
;;;   - collect-registers-from-instructions (Step 5: register discovery)
;;;   - build-wasm-function-wat (Step 6: WAT body for one function)
;;;   - build-all-wasm-functions (assemble all functions in a module)
;;;
;;; Data tables (*wasm-i64-binop-table*, etc.) and emit-trampoline-instruction
;;; are in wasm-trampoline-emit.lisp (loads before).
;;;
;;; Load order: after wasm-trampoline-emit.lisp.
(in-package :cl-cc)

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
  "Pre-allocate local indices for all virtual registers used in INSTRUCTIONS.
   Uses the optimizer's opt-inst-dst and opt-inst-read-regs for register discovery,
   avoiding duplication of per-instruction-type register access knowledge."
  (dolist (inst instructions)
    (flet ((touch (reg)
             (when (and reg (keywordp reg))
               (wasm-reg-to-local reg-map reg))))
      ;; Touch destination register
      (let ((dst (opt-inst-dst inst)))
        (when dst (touch dst)))
      ;; Touch all source registers
      (dolist (reg (opt-inst-read-regs inst))
        (touch reg)))))

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
