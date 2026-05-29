;;;; packages/emit/src/wasm-trampoline-build.lisp — WASM Trampoline: body builder + module assembler
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
(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Step 4: Build the trampoline body for a function
;;; ─────────────────────────────────────────────────────────────────────────────

(defstruct wasm-case-dispatch-env
  (const-values (make-hash-table :test #'eq))
  (closure-labels (make-hash-table :test #'eq))
  (list-labels (make-hash-table :test #'eq))
  (vector-labels (make-hash-table :test #'eq))
  (index-info (make-hash-table :test #'eq)))

(defun %wasm-note-case-value (table dst value)
  (when dst
    (setf (gethash dst table) value)))

(defun %wasm-build-case-dispatch-env (instructions)
  "Collect the dense CASE table shape emitted by the user-level CASE expander."
  (let ((env (make-wasm-case-dispatch-env)))
    (dolist (inst instructions env)
      (cond
        ((typep inst 'vm-const)
         (%wasm-note-case-value (wasm-case-dispatch-env-const-values env)
                                (vm-dst inst)
                                (vm-value inst)))
        ((typep inst 'vm-closure)
         (%wasm-note-case-value (wasm-case-dispatch-env-closure-labels env)
                                (vm-dst inst)
                                (vm-label-name inst)))
        ((typep inst 'vm-cons)
         (let* ((car-label (gethash (vm-car-reg inst)
                                    (wasm-case-dispatch-env-closure-labels env)))
                (cdr-labels (gethash (vm-cdr-reg inst)
                                     (wasm-case-dispatch-env-list-labels env))))
           (multiple-value-bind (cdr-value cdr-const-p)
               (gethash (vm-cdr-reg inst)
                        (wasm-case-dispatch-env-const-values env))
             (when (and car-label (or (and cdr-const-p (null cdr-value)) cdr-labels))
               (%wasm-note-case-value (wasm-case-dispatch-env-list-labels env)
                                      (vm-dst inst)
                                      (cons car-label cdr-labels))))))
        ((typep inst 'cl-cc/vm::vm-coerce-to-vector)
         (let ((labels (gethash (vm-src inst)
                                (wasm-case-dispatch-env-list-labels env))))
           (when labels
             (%wasm-note-case-value (wasm-case-dispatch-env-vector-labels env)
                                    (vm-dst inst)
                                    labels))))
        ((typep inst 'vm-sub)
         (let ((min-key (gethash (vm-rhs inst)
                                 (wasm-case-dispatch-env-const-values env))))
           (when (integerp min-key)
             (%wasm-note-case-value (wasm-case-dispatch-env-index-info env)
                                    (vm-dst inst)
                                    (cons (vm-lhs inst) min-key)))))
        ((typep inst 'vm-move)
         (dolist (accessor (list #'wasm-case-dispatch-env-vector-labels
                                 #'wasm-case-dispatch-env-index-info))
           (let ((value (gethash (vm-src inst) (funcall accessor env))))
             (when value
               (%wasm-note-case-value (funcall accessor env)
                                      (vm-dst inst)
                                      value)))))))))

(defun %wasm-next-label-after (instructions start-index)
  (loop for inst in (nthcdr start-index instructions)
        when (typep inst 'vm-label)
          return (vm-name inst)))

(defun %maybe-emit-wasm-case-dispatch-pattern
    (instructions index case-env label-pc-map reg-map stream fallback-default-label)
  "Emit br_table for the CASE expander's SVREF/MOVE/CALL dispatch pattern.

Returns the number of VM instructions consumed, or 0 when the pattern does not
match or the br_table helper declines the dispatch."
  (let ((svref (nth index instructions))
        (move (nth (1+ index) instructions))
        (call (nth (+ index 2) instructions)))
    (when (and (typep svref 'cl-cc/vm::vm-svref)
               (typep move 'vm-move)
               (typep call 'vm-call)
               (null (vm-args call))
               (eq (vm-src move) (vm-dst svref))
               (eq (vm-func-reg call) (vm-dst move)))
      (let* ((targets (gethash (vm-lhs svref)
                               (wasm-case-dispatch-env-vector-labels case-env)))
             (index-info (gethash (vm-rhs svref)
                                  (wasm-case-dispatch-env-index-info case-env)))
             (default-label (or (%wasm-next-label-after instructions (+ index 3))
                                fallback-default-label)))
        (when (and targets index-info default-label)
          (let ((case-targets (loop for label in targets
                                    for key from (cdr index-info)
                                    collect (cons key label))))
             (when (maybe-emit-wasm-dense-case-br-table
                    (car index-info) case-targets default-label
                    label-pc-map reg-map stream)
              3))))))
    0)

(defun %wasm-local-pc-block-map (basic-blocks)
  "Return an EQL hash table mapping local instruction PC to trampoline block PC."
  (let ((map (make-hash-table :test #'eql))
        (pc 0))
    (loop for bb in basic-blocks
          for block-pc from 0
          do (dolist (_ (wasm-bb-instructions bb))
               (declare (ignore _))
               (setf (gethash pc map) block-pc)
               (incf pc)))
    map))

(defun %wasm-dynamic-eh-entries (instructions)
  "Recover catch/handler dynamic extents still represented by VM instructions."
  (let ((stack nil)
        (entries nil))
    (loop for inst in instructions
          for pc from 0
          do (cond
               ((typep inst 'vm-establish-catch)
                (push (list :kind :catch
                            :start-pc (1+ pc)
                            :handler-label (vm-catch-handler-label inst)
                            :result-reg (vm-catch-result-reg inst)
                            :tag-reg (vm-catch-tag-reg inst))
                      stack))
               ((typep inst 'vm-establish-handler)
                (push (list :kind :handler
                            :start-pc (1+ pc)
                            :handler-label (vm-handler-label inst)
                            :result-reg (vm-handler-result-reg inst)
                            :condition-type (vm-error-type inst))
                      stack))
               ((typep inst 'vm-remove-handler)
                (let ((entry (pop stack)))
                  (when entry
                    (setf (getf entry :end-pc) pc)
                    (push entry entries))))))
    (nreverse entries)))

(defun %wasm-eh-entry->block-entry (entry pc->block label-pc-map)
  "Convert a local instruction-PC EH ENTRY to trampoline block-PC coordinates."
  (let* ((start-pc (getf entry :start-pc))
         (end-pc (getf entry :end-pc))
         (start-block (gethash start-pc pc->block))
         (end-block (gethash (max start-pc (1- end-pc)) pc->block))
         (handler-block (gethash (getf entry :handler-label) label-pc-map)))
    (when (and start-block end-block handler-block)
      (append entry
              (list :start-block start-block
                    :end-block (1+ end-block)
                    :handler-block handler-block)))))

(defun %wasm-effective-eh-entries (basic-blocks label-pc-map reg-map static-entries)
  "Return native Wasm EH dispatch entries for this trampoline body."
  (let* ((all-instructions nil))
    (dolist (bb basic-blocks)
      (dolist (inst (wasm-bb-instructions bb))
        (push inst all-instructions)))
    (let* ((instructions (nreverse all-instructions))
           (pc->block (%wasm-local-pc-block-map basic-blocks))
           (entries (append static-entries (%wasm-dynamic-eh-entries instructions))))
      (loop for entry in entries
            for block-entry = (%wasm-eh-entry->block-entry entry pc->block label-pc-map)
            when block-entry
              collect (progn
                        (when (getf block-entry :result-reg)
                          (wasm-reg-to-local reg-map (getf block-entry :result-reg)))
                        (when (getf block-entry :tag-reg)
                          (wasm-reg-to-local reg-map (getf block-entry :tag-reg)))
                        block-entry)))))

(defun %wasm-eh-pc-range-test (reg-map entry)
  (format nil "(i32.and (i32.ge_u (local.get ~D) (i32.const ~D)) (i32.lt_u (local.get ~D) (i32.const ~D)))"
          (wasm-reg-map-pc-index reg-map)
          (getf entry :start-block)
          (wasm-reg-map-pc-index reg-map)
          (getf entry :end-block)))

(defun %emit-wasm-dispatch-catch (entries reg-map stream)
  "Emit the native Wasm catch side of the PC-dispatch trampoline."
  (format stream "~%            (catch $cl_condition_tag")
  (format stream "~%              (local.set ~D) ;; condition/value payload"
          (wasm-reg-map-tmp-index reg-map))
  (format stream "~%              (local.set ~D) ;; condition/catch tag payload"
          (wasm-reg-map-eh-tag-index reg-map))
  (dolist (entry entries)
    (let ((range-test (%wasm-eh-pc-range-test reg-map entry)))
      (when (eq (getf entry :kind) :catch)
        (setf range-test
              (format nil "(i32.and ~A (ref.eq (local.get ~D) ~A))"
                      range-test
                      (wasm-reg-map-eh-tag-index reg-map)
                      (reg-local-ref reg-map (getf entry :tag-reg)))))
      (format stream "~%              (if ~A" range-test)
      (format stream "~%                (then")
      (format stream "~%                  (local.set ~D (local.get ~D))"
              (wasm-reg-to-local reg-map (getf entry :result-reg))
              (wasm-reg-map-tmp-index reg-map))
      (format stream "~%                  (local.set ~D (i32.const ~D))"
              (wasm-reg-map-pc-index reg-map)
              (getf entry :handler-block))
      (format stream "~%                  (br $dispatch)))"))
  (format stream "~%              (throw $cl_condition_tag (local.get ~D) (local.get ~D))"
          (wasm-reg-map-eh-tag-index reg-map)
          (wasm-reg-map-tmp-index reg-map))
  (format stream "~%            ) ;; end catch $cl_condition_tag")))

(defun emit-wasm-try-table-wat (entries reg-map stream)
  "Emit EH v2 try_table catch clauses for ENTRIES.

The PC-dispatch trampoline keeps the existing EH v1 try/catch wrapper for
portable control flow; this function is the EH v2 codegen hook used by both the
trampoline comments/metadata and direct structured emission.  It materializes
the exact catch table shape expected by EH v2 backends: CL tag catches plus an
optional catch_all_ref clause that captures an exnref for inspection."
  (format stream "~%            ;; FR-252: try_table catch table")
  (format stream "~%            ;; (try_table")
  (dolist (entry entries)
    (format stream "~%            ;;   (catch_ref $cl_condition_tag $blk_~D) ;; handler-case ~S"
            (getf entry :handler-block)
            (or (getf entry :condition-type) (getf entry :tag-reg))))
  (when (wasm-catch-all-ref-feature-enabled-p)
    (format stream "~%            ;;   (catch_all_ref $dispatch) ;; handler-case (t (e) ...), captured in local ~D"
            (wasm-reg-map-exnref-index reg-map)))
  (format stream "~%            ;; )"))

(defun %emit-wasm-dispatch-catch-v2 (entries reg-map stream)
  "Emit EH v2-aware catch side for the PC-dispatch trampoline.

Native try_table branch targets are recorded through emit-wasm-try-table-wat;
the catch body captures exnref through imported helpers so CL handler-case can
compare tags/payloads without changing the trampoline architecture."
  (emit-wasm-try-table-wat entries reg-map stream)
  (format stream "~%            (catch $cl_condition_tag")
  (format stream "~%              (local.set ~D) ;; condition/value payload" (wasm-reg-map-tmp-index reg-map))
  (format stream "~%              (local.set ~D) ;; condition/catch tag payload" (wasm-reg-map-eh-tag-index reg-map))
  (format stream "~%              ;; FR-271/FR-289: exnref-compatible handler dispatch; payload may be a GC struct")
  (dolist (entry entries)
    (let ((range-test (%wasm-eh-pc-range-test reg-map entry)))
      (when (eq (getf entry :kind) :catch)
        (setf range-test
              (format nil "(i32.and ~A (ref.eq (local.get ~D) ~A))"
                      range-test
                      (wasm-reg-map-eh-tag-index reg-map)
                      (reg-local-ref reg-map (getf entry :tag-reg)))))
      (when (and (eq (getf entry :kind) :handler)
                 (not (member (getf entry :condition-type) '(t condition) :test #'eq)))
        (format stream "~%              ;; FR-289: handler ~S may ref.cast/br_on_cast payload local ~D to $condition_t"
                (getf entry :condition-type)
                (wasm-reg-map-tmp-index reg-map)))
      (format stream "~%              (if ~A" range-test)
      (format stream "~%                (then")
      (format stream "~%                  (local.set ~D (local.get ~D))"
              (wasm-reg-to-local reg-map (getf entry :result-reg))
              (wasm-reg-map-tmp-index reg-map))
      (format stream "~%                  (local.set ~D (i32.const ~D))"
              (wasm-reg-map-pc-index reg-map)
              (getf entry :handler-block))
      (format stream "~%                  (br $dispatch)))")))
  (format stream "~%              (throw_ref (call $cl_condition_to_exnref (local.get ~D)))"
          (wasm-reg-map-tmp-index reg-map))
  (format stream "~%            ) ;; end EH v2 catch $cl_condition_tag"))

(defun build-trampoline-body (basic-blocks label-pc-map reg-map param-regs stream
                              &optional static-exception-entries)
  "Emit the WAT trampoline body (nested blocks + loop) to STREAM.
   BASIC-BLOCKS is the list of wasm-basic-block. REG-MAP is the register map.
   PARAM-REGS is a list of VM register keywords that are this function's parameters;
   they are loaded from the global $cl_argN calling-convention registers as a prologue."
  (let* ((num-blocks (length basic-blocks))
         (eh-entries (%wasm-effective-eh-entries
                      basic-blocks label-pc-map reg-map static-exception-entries)))
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

    ;; FR-204: Exception Handling — wrap entire dispatch in try/catch.
    ;; The Wasm exception handling proposal enables native try/catch/throw
    ;; instead of PC-dispatch-based exception simulation.  On catch, the
    ;; exception payload is recovered and the dispatch loop continues with
    ;; the appropriate handler block's PC index.
    (format stream "~%        (try $try_body (result eqref)")
    (format stream "~%          (do")

    ;; Loop for re-dispatching after a jump
    (format stream "~%            (loop $dispatch (result eqref)")

    (when (and eh-entries (wasm-eh-v2-feature-enabled-p) (wasm-exnref-feature-enabled-p))
      (wasm-reg-map-exnref-index reg-map))

    (when eh-entries
      (format stream "~%          (try (result eqref)")
      (format stream "~%            (do"))

    ;; Nested blocks in REVERSE order so block 0 is innermost.
    ;; WAT nesting: (block $blk_N-1 (block $blk_N-2 ... (block $blk_0 br_table)))
    ;; br_table targets are in ascending order: $blk_0, $blk_1, ..., $exit
    (dotimes (i num-blocks)
      (format stream "~%              (block $blk_~D (result eqref)" i))

    ;; The br_table dispatch instruction
    (format stream "~%                (br_table")
    (dotimes (i num-blocks)
      (format stream " $blk_~D" i))
    (format stream " $exit")   ; default target: exit (should not occur)
    (format stream "~%                 (local.get ~D))" (wasm-reg-map-pc-index reg-map))

    ;; Close each nested block and emit its instructions.
    ;; Block i's closing paren comes after the br_table, then the instructions
    ;; for basic block i follow, then a branch back to $dispatch (except the last).
    ;;
    ;; The WAT layout after br_table resolution for block i is:
    ;;   ) ;; end block $blk_i
    ;;   <instructions for block i>
    ;;   (br $dispatch)   ;; unless last block
    (let ((case-env (let ((all-instructions nil))
                      (dolist (bb basic-blocks
                               (%wasm-build-case-dispatch-env (nreverse all-instructions)))
                        (dolist (inst (wasm-bb-instructions bb))
                          (push inst all-instructions)))))
          ;; FR-145: Bind fixnum unboxed register tracking table during emission
          (*wasm-fixnum-unboxed-regs* (make-hash-table :test #'eq)))
      (dotimes (i num-blocks)
      (format stream "~%              ) ;; end block $blk_~D" i)
      (let ((bb (nth i basic-blocks)))
        (loop with instructions = (wasm-bb-instructions bb)
              with fallback-default-label = (and (< (1+ i) num-blocks)
                                                 (wasm-bb-label (nth (1+ i) basic-blocks)))
              for rest on instructions
              for index from 0
              for consumed = 0 then (if (plusp consumed) (1- consumed) 0)
              when (zerop consumed)
                do (let ((skip (%maybe-emit-wasm-case-dispatch-pattern
                                instructions index case-env label-pc-map reg-map stream
                                fallback-default-label)))
                      (if (plusp skip)
                          (setf consumed skip)
                          (emit-trampoline-instruction (car rest) label-pc-map reg-map
                                                       num-blocks stream)))))
      ;; FR-143: Tail-call dispatch.  When enabled, replace (br $dispatch) with
      ;; return_call_indirect to eliminate the self-loop overhead.  Each block
      ;; tail-calls back into this same function through the funcref table,
      ;; allowing the WASM engine to optimize the call chain.
      (unless (= i (1- num-blocks))
        ;; Fall-through remains an intra-function trampoline dispatch.  FR-143
        ;; return_call_indirect is only valid for jumps to labels that are known
        ;; function-table entries; a basic-block PC is not a funcref table index.
        (format stream "~%          (br $dispatch)")))

    (when eh-entries
      (format stream "~%            ) ;; end do")
      (if (wasm-eh-v2-feature-enabled-p)
          (%emit-wasm-dispatch-catch-v2 eh-entries reg-map stream)
          (%emit-wasm-dispatch-catch eh-entries reg-map stream))
      (format stream "~%          ) ;; end try/catch $dispatch"))

    ;; Close loop and outer block
    (format stream "~%            ) ;; end loop $dispatch")
    (format stream "~%          ) ;; end do $try_body")
    ;; FR-204: catch handler for CL conditions
    (format stream "~%          (catch $cl_condition_tag")
    (format stream "~%            ;; Recover exception payload: tag-value in local $tmp, handler-pc from tag")
    (format stream "~%            (drop)  ;; pop tag value")
    (format stream "~%            (drop)  ;; pop exception value")
    (format stream "~%            ;; Re-enter dispatch loop at block 0 (exception recovery)")
    (format stream "~%            (local.set ~D (i32.const 0))" (wasm-reg-map-pc-index reg-map))
    (format stream "~%            (br $dispatch)")
    (format stream "~%          ) ;; end catch")
    (format stream "~%        ) ;; end try $try_body")
    (format stream "~%      ) ;; end block $exit")

    ;; Fall-through value: return nil if no explicit vm-ret/vm-halt
    (format stream "~%      (ref.null eq)"))))

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
          (param-regs (wasm-function-param-regs func-def))
          (num-params (length param-regs))
          (reg-map (make-wasm-reg-map-for-function num-params))
          (body-stream (make-string-output-stream)))
    (initialize-wasm-param-locals reg-map param-regs)
    ;; Pre-allocate locals for all registers that appear in this function
    (collect-registers-from-instructions instructions reg-map)
    ;; Emit the trampoline body with FR-143 tail-call parameters
    (build-trampoline-body basic-blocks label-pc-map reg-map
                           param-regs body-stream
                           (wasm-func-exception-table func-def))
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
