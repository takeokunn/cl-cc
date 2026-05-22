(in-package :cl-cc/optimize)

;;; ─── FR-516 Escape Analysis ───────────────────────────────────────────────

(defvar *opt-escape-analysis-metadata* (make-hash-table :test #'eq)
  "EQ side table recording FR-516 escape/stack-allocation decisions.")

(defun opt-escape-analysis-metadata (inst)
  "Return FR-516 escape metadata for allocation INST, or NIL."
  (gethash inst *opt-escape-analysis-metadata*))

(defun opt-escape-stack-allocation-marked-p (inst)
  "Return T when INST is proven non-escaping by FR-516."
  (getf (opt-escape-analysis-metadata inst) :stack-allocation))

(defun %opt-escape-allocation-p (inst)
  "Return T when INST allocates a heap object tracked by FR-516."
  (opt-heap-root-inst-p inst))

(defun %opt-escape-root-for-reg (reg alias-roots)
  (or (and reg (gethash reg alias-roots)) reg))

(defun %opt-escape-add-edge (from to graph)
  "Record that TO is reachable through FROM; if FROM escapes, TO escapes too."
  (when (and from to (not (eq from to)))
    (pushnew to (gethash from graph) :test #'eq)))

(defun %opt-escape-mark-root (root escaped reason escaped-reasons)
  (when root
    (setf (gethash root escaped) t)
    (pushnew reason (gethash root escaped-reasons) :test #'eq)))

(defun %opt-escape-call-inst-p (inst)
  (typep inst '(or vm-call vm-tail-call vm-apply vm-generic-call)))

(defun %opt-escape-return-inst-p (inst)
  (typep inst '(or vm-ret vm-halt)))

(defun %opt-escape-store-inst-p (inst)
  (typep inst '(or vm-set-global vm-slot-write vm-aset vm-register-function
                   vm-rplaca vm-rplacd)))

(defun %opt-escape-value-regs-stored-by-inst (inst)
  "Return registers whose values are stored by INST."
  (cond
    ((typep inst 'vm-set-global) (list (cl-cc/vm::vm-set-global-src inst)))
    ((typep inst 'vm-slot-write) (list (cl-cc/vm::vm-slot-write-value-reg inst)))
    ((typep inst 'vm-aset) (list (vm-val-reg inst)))
    ((typep inst 'vm-register-function) (opt-inst-read-regs inst))
    ((typep inst '(or vm-rplaca vm-rplacd)) (list (vm-val-reg inst)))
    (t nil)))

(defun %opt-escape-container-reg (inst)
  "Return the heap container register written by INST, or NIL for global stores."
  (cond
    ((typep inst 'vm-slot-write) (cl-cc/vm::vm-slot-write-obj-reg inst))
    ((typep inst 'vm-aset) (vm-array-reg inst))
    ((typep inst '(or vm-rplaca vm-rplacd)) (vm-cons-reg inst))
    (t nil)))

(defun %opt-escape-direct-uses-mark-escapes (instructions alias-roots alloc-roots graph escaped reasons)
  "Mark direct escaping uses and object connectivity edges."
  (dolist (inst instructions)
    (cond
      ;; Returning a tracked object makes it live after function return.
      ((%opt-escape-return-inst-p inst)
       (dolist (reg (opt-inst-read-regs inst))
         (let ((root (%opt-escape-root-for-reg reg alias-roots)))
           (when (gethash root alloc-roots)
             (%opt-escape-mark-root root escaped :returned reasons)))))

      ;; Unknown calls may retain any passed object or closure.
      ((%opt-escape-call-inst-p inst)
       (dolist (reg (opt-inst-read-regs inst))
         (let ((root (%opt-escape-root-for-reg reg alias-roots)))
           (when (gethash root alloc-roots)
             (%opt-escape-mark-root root escaped :unknown-call reasons)))))

      ;; Stores to globals/unknown heap make the value escape. Stores into another
      ;; tracked local object create a connectivity edge instead.
      ((%opt-escape-store-inst-p inst)
       (let* ((container-reg (%opt-escape-container-reg inst))
              (container-root (%opt-escape-root-for-reg container-reg alias-roots))
              (local-container-p (and container-root (gethash container-root alloc-roots))))
         (dolist (value-reg (%opt-escape-value-regs-stored-by-inst inst))
           (let ((value-root (%opt-escape-root-for-reg value-reg alias-roots)))
             (when (gethash value-root alloc-roots)
               (if local-container-p
                   (%opt-escape-add-edge container-root value-root graph)
                   (%opt-escape-mark-root value-root escaped :stored-to-heap reasons)))))))

      ;; A closure carries captured roots. If the closure later escapes, captured
      ;; roots escape through this connectivity chain.
      ((typep inst 'vm-make-closure)
       (let ((closure-root (%opt-escape-root-for-reg (vm-dst inst) alias-roots)))
         (dolist (captured (vm-env-regs inst))
           (let ((captured-root (%opt-escape-root-for-reg captured alias-roots)))
             (when (and (gethash closure-root alloc-roots)
                        (gethash captured-root alloc-roots))
               (%opt-escape-add-edge closure-root captured-root graph)))))))))

(defun %opt-escape-propagate (graph escaped reasons)
  "Propagate escape marks along captured/stored-object connectivity edges."
  (let ((work (loop for root being the hash-keys of escaped collect root)))
    (loop while work
          do (let ((root (pop work)))
               (dolist (child (gethash root graph))
                 (unless (gethash child escaped)
                   (%opt-escape-mark-root child escaped :reachable-from-escaping-object reasons)
                   (push child work)))))))

(defun opt-analyze-escapes (instructions)
  "Analyze FR-516 escaping heap objects in INSTRUCTIONS.

Returns four values: allocation table root->inst, escaped root table,
connectivity graph, and escape reason table.  The analysis is conservative:
returns, global/unknown heap stores, and unknown calls are treated as escapes;
stores into locally-tracked objects become graph edges so closure/object capture
chains are handled transitively."
  (let ((alias-roots (opt-compute-heap-aliases instructions))
        (alloc-roots (make-hash-table :test #'eq))
        (graph (make-hash-table :test #'eq))
        (escaped (make-hash-table :test #'eq))
        (reasons (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (when (and (%opt-escape-allocation-p inst) (opt-inst-dst inst))
        (setf (gethash (%opt-escape-root-for-reg (opt-inst-dst inst) alias-roots)
                       alloc-roots)
              inst)))
    (%opt-escape-direct-uses-mark-escapes instructions alias-roots alloc-roots graph escaped reasons)
    (%opt-escape-propagate graph escaped reasons)
    (values alloc-roots escaped graph reasons)))

(defun %opt-mark-stack-allocation-compatible (inst root kind)
  "Record stack allocation intent in both FR-516 and legacy FR-18 side tables."
  (let ((metadata (list :stack-allocation t
                        :replacement-op :vm-stack-alloc
                        :allocation-kind kind
                        :root-reg root
                        :reason :non-escaping-fr-516)))
    (setf (gethash inst *opt-escape-analysis-metadata*) metadata)
    (let ((legacy-symbol (find-symbol "*OPT-STACK-ALLOCATION-METADATA*" :cl-cc/optimize)))
      (when (and legacy-symbol (boundp legacy-symbol))
        (setf (gethash inst (symbol-value legacy-symbol)) metadata)))))

(defun opt-pass-escape-analysis (instructions)
  "FR-516: mark heap allocations that do not escape function scope.

Non-escaping allocations are annotated for downstream lowering as
`:replacement-op :vm-stack-alloc'.  The pass leaves bytecode unchanged unless a
future VM stack-allocation instruction is present, preserving correctness for
all unproven or potentially escaping objects."
  (clrhash *opt-escape-analysis-metadata*)
  (multiple-value-bind (alloc-roots escaped _graph reasons)
      (opt-analyze-escapes instructions)
    (declare (ignore _graph reasons))
    (maphash (lambda (root inst)
               (unless (gethash root escaped)
                 (%opt-mark-stack-allocation-compatible inst root (opt-heap-root-kind inst))))
             alloc-roots))
  instructions)
