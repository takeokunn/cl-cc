;;;; optimizer-inline-pass.lisp — Function Inlining Pass Execution
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: opt-make-pure-function-memo-table, opt-pure-function-memo-key,
;;; opt-pure-function-memo-get, opt-pure-function-memo-put,
;;; opt-function-body-instruction-tables,
;;; opt-top-level-function-roots, opt-reachable-function-labels,
;;; opt-pass-global-dce, opt-inline-inst-cost, opt-inline-body-cost,
;;; opt-adaptive-inline-threshold, opt-inline-eligible-p, opt-pass-inline.
;;;
;;; Analysis helpers (opt-collect-function-defs, opt-build-function-name-map,
;;; opt-build-call-graph, opt-call-graph-recursive-labels, opt-make-renaming,
;;; opt-rename-regs-in-inst, opt-can-safely-rename-p,
;;; opt-body-has-global-refs-p, opt-max-reg-index) are in
;;; optimizer-inline.lisp (loads before this file).
;;;
;;; Load order: after optimizer-inline.lisp, before optimizer-dataflow.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/optimize)

(defun opt-make-pure-function-memo-table ()
  "Create a memo table for pure-function result caching."
  (make-hash-table :test #'equal))

(defun opt-pure-function-memo-key (label args)
  "Build the memoization key for LABEL applied to ARGS."
  (list label args))

(defun opt-pure-function-memo-get (memo-table pure-labels label args)
  "Return cached result for pure LABEL/ARGS, or NIL with a miss flag."
  (if (gethash label pure-labels)
      (multiple-value-bind (value found-p)
          (gethash (opt-pure-function-memo-key label args) memo-table)
        (values value found-p))
      (values nil nil)))

(defun opt-pure-function-memo-put (memo-table pure-labels label args result)
  "Store RESULT for pure LABEL/ARGS in MEMO-TABLE.
Impure or unknown labels are ignored conservatively."
  (when (gethash label pure-labels)
    (setf (gethash (opt-pure-function-memo-key label args) memo-table) result))
  result)

(defun opt-function-body-instruction-tables (func-defs)
  "Single-pass scan of FUNC-DEFS; returns (values body-inst-set inst->label).
body-inst-set: EQ hash-table, every instruction that belongs to a function body.
inst->label:   EQ hash-table, maps each such instruction to its owning label."
  (let ((body-inst-set (make-hash-table :test #'eq))
        (inst->label   (make-hash-table :test #'eq)))
    (maphash (lambda (label def)
               (dolist (inst (getf def :body))
                 (setf (gethash inst body-inst-set) t
                       (gethash inst inst->label)   label)))
             func-defs)
    (values body-inst-set inst->label)))

(defun opt-top-level-function-roots (instructions func-defs name-to-label body-inst-set)
  "Return an EQUAL hash-table of function labels reachable from top-level code.
Only instructions outside collected function bodies are scanned. This keeps the
analysis conservative and avoids treating nested function internals as roots."
  (let ((roots (make-hash-table :test #'equal))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (unless (gethash inst body-inst-set)
        (typecase inst
          ((or vm-closure vm-func-ref)
           (let ((label (vm-label-name inst)))
             (when (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label))))
          (vm-const
           (let ((label (and (symbolp (vm-value inst))
                             (gethash (vm-value inst) name-to-label))))
             (when (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label))))
          (vm-move
           (multiple-value-bind (label present-p)
               (gethash (vm-src inst) reg-track)
             (if present-p
                 (setf (gethash (vm-dst inst) reg-track) label)
                 (remhash (vm-dst inst) reg-track))))
          ((or vm-call vm-tail-call)
           (let ((label (gethash (vm-func-reg inst) reg-track)))
             (when label
               (setf (gethash label roots) t)))
           (let ((dst (opt-inst-dst inst)))
             (when dst (remhash dst reg-track))))
          (vm-set-global
           (let ((label (gethash (vm-src inst) reg-track)))
             (when label
               (setf (gethash label roots) t))))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst (remhash dst reg-track)))))))))

(defun opt-reachable-function-labels (graph roots)
  "Return an EQUAL hash-table of function labels reachable in GRAPH from ROOTS."
  (let ((reachable (make-hash-table :test #'equal))
        (worklist nil))
    (maphash (lambda (label _)
               (push label worklist))
             roots)
    (loop while worklist
          do (let ((label (pop worklist)))
               (unless (gethash label reachable)
                 (setf (gethash label reachable) t)
                 (dolist (callee (gethash label graph))
                   (push callee worklist)))))
    reachable))

(defun opt-pass-global-dce (instructions)
  "Remove unreachable linear function definitions conservatively.
The pass only removes whole vm-closure/vm-register-function/function-body groups
for labels that are unreachable from top-level function roots. Top-level code is
preserved verbatim. Non-linear or otherwise uncollected functions are left in
place by construction."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions)))
    (when (= 0 (hash-table-count func-defs))
      (return-from opt-pass-global-dce instructions))
    (multiple-value-bind (body-inst-set body-inst-labels)
        (opt-function-body-instruction-tables func-defs)
      (let* ((graph (opt-build-call-graph instructions func-defs name-to-label))
           (roots (opt-top-level-function-roots instructions func-defs name-to-label body-inst-set))
           (reachable (opt-reachable-function-labels graph roots))
           (closure-reg->label (make-hash-table :test #'eq))
           (labels-to-drop (make-hash-table :test #'equal)))
      (maphash (lambda (label _def)
                 (unless (gethash label reachable)
                   (setf (gethash label labels-to-drop) t)))
               func-defs)
      (when (= 0 (hash-table-count labels-to-drop))
        (return-from opt-pass-global-dce instructions))
      (dolist (inst instructions)
        (when (vm-closure-p inst)
          (setf (gethash (vm-dst inst) closure-reg->label) (vm-label-name inst))))
      (remove-if (lambda (inst)
                   (cond
                     ((and (vm-closure-p inst)
                           (gethash (vm-label-name inst) labels-to-drop))
                      t)
                     ((and (typep inst 'vm-label)
                           (gethash (vm-name inst) labels-to-drop))
                      t)
                     ((and (gethash inst body-inst-set)
                           (not (typep inst 'vm-label))
                           (gethash (gethash inst body-inst-labels) labels-to-drop))
                      t)
                     ((and (typep inst 'vm-register-function)
                           (let ((label (gethash (vm-src inst) closure-reg->label)))
                             (and label (gethash label labels-to-drop))))
                      t)
                     (t nil)))
                 instructions)))))

(defun opt-inline-inst-cost (inst)
  "Return the inline cost of INST using the shared e-graph opcode table.
This keeps inlining policy aligned with the optimizer's existing cost model
instead of relying on raw instruction count."
  (egraph-default-cost (vm-inst-to-enode-op inst) nil))

(defun opt-inline-body-cost (body)
  "Return the total inline cost of BODY, excluding the final vm-ret."
  (reduce #'+ (mapcar #'opt-inline-inst-cost (butlast body)) :initial-value 0))

(defun opt-adaptive-inline-threshold (def &key (base-threshold 15) (max-threshold 50))
  "Compute a conservative adaptive inline threshold for DEF.
Cheap bodies dominated by low-cost instructions get a larger threshold, while
call-heavy bodies are kept near the base threshold. This is a small FR-150
style heuristic without runtime profile counters."
  (let* ((body (butlast (getf def :body)))
         (inst-count (length body))
         (cheap-count (count-if (lambda (inst)
                                  (<= (opt-inline-inst-cost inst) 1))
                                body))
         (call-heavy-p (some (lambda (inst)
                               (typep inst '(or vm-call vm-generic-call vm-apply)))
                             body))
         (cheap-ratio (if (zerop inst-count) 1.0 (/ cheap-count inst-count))))
    (min max-threshold
         (max 8
              (+ base-threshold
                 (if call-heavy-p -5 0)
                 (cond
                   ((>= cheap-ratio 0.90) 35)
                   ((>= cheap-ratio 0.75) 20)
                   ((>= cheap-ratio 0.50) 8)
                   (t 0)))))))

(defun opt-inline-eligible-p (def threshold)
  "Return T if the function definition DEF (a plist from opt-collect-function-defs)
satisfies all inlining preconditions:
1. Has a vm-closure with zero captured variables
2. Has only required params (no &optional/&rest/&key)
3. Body cost ≤ THRESHOLD (excluding the final vm-ret)
4. All body instructions support lossless register renaming
5. Body reads no 'global' registers (ones not defined by params or body)"
  (let ((ci   (getf def :closure))
        (body (getf def :body)))
    (and (vm-closure-p ci)
         (null (vm-captured-vars ci))
         (null (vm-closure-optional-params ci))
         (null (vm-closure-rest-param ci))
         (null (vm-closure-key-params ci))
         (<= (opt-inline-body-cost body) threshold)
         (opt-can-safely-rename-p body)
         (not (opt-body-has-global-refs-p body (vm-closure-params ci))))))

(defun opt-pass-inline (instructions &key (threshold 15))
  "Replace vm-call of a known small function with the inlined body.
The function must be: captured-var-free, linear, and have body cost
≤ THRESHOLD (not counting the final vm-ret)."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (recursive-labels (opt-call-graph-recursive-labels
                            (opt-build-call-graph instructions func-defs name-to-label)))
         (base-idx  (1+ (opt-max-reg-index instructions)))
         (reg-track (make-hash-table :test #'eq))   ; reg → label of known function
         (const-track (make-hash-table :test #'eq)) ; reg → constant value
         (result nil))
    (dolist (inst instructions)
      (typecase inst
        ;; Record which register now holds which function
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when (gethash label func-defs)
             (setf (gethash (vm-dst inst) reg-track) label)))
         (let ((dst (opt-inst-dst inst)))
           (when dst (remhash dst const-track)))
         (push inst result))
        ;; Track vm-const loading a function name symbol
        (vm-const
         (let* ((val (vm-value inst))
                (label (when (symbolp val) (gethash val name-to-label))))
           (when (and label (gethash label func-defs))
             (setf (gethash (vm-dst inst) reg-track) label)))
          (setf (gethash (vm-dst inst) const-track) (vm-value inst))
          (push inst result))
        (vm-move
         (multiple-value-bind (src-const present-p)
             (gethash (vm-src inst) const-track)
           (if present-p
               (setf (gethash (vm-dst inst) const-track) src-const)
               (remhash (vm-dst inst) const-track)))
         (push inst result))
        ;; Attempt inlining
        (vm-call
         (let* ((label (gethash (vm-func-reg inst) reg-track))
                (def   (and label (gethash label func-defs)))
                (effective-threshold (if (and def (eq threshold :adaptive))
                                         (opt-adaptive-inline-threshold def)
                                         threshold)))
           (if (and def
                    (not (gethash label recursive-labels))
                    (opt-inline-eligible-p def effective-threshold))
               ;; ── Inline ──
               (let* ((body   (getf def :body))
                      (ci     (getf def :closure))
                      (params (vm-closure-params ci))
                      (rename (opt-make-renaming body base-idx)))
                 (incf base-idx (hash-table-count rename))
                 ;; 1. Move call arguments into renamed parameter registers
                 (loop for param in params
                       for arg in (vm-args inst)
                       do (let ((dst (or (gethash param rename) param))
                                (const-present nil)
                                (const-value nil))
                            (multiple-value-setq (const-value const-present)
                              (gethash arg const-track))
                            (push (if const-present
                                      (make-vm-const :dst dst :value const-value)
                                      (make-vm-move :dst dst :src arg))
                                  result)))
                 ;; 2. Emit renamed body (all but the last vm-ret)
                 (dolist (bi (butlast body))
                   (push (opt-rename-regs-in-inst bi rename) result))
                 ;; 3. Move renamed return register to call's dst
                 (let* ((ret-inst (car (last body)))
                        (ret-src (or (gethash (vm-reg ret-inst) rename)
                                     (vm-reg ret-inst))))
                   (push (make-vm-move :dst (vm-dst inst) :src ret-src) result)))
               ;; ── Keep call as-is ──
               (push inst result))))
        ;; Invalidate tracking for any overwritten register
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track)
             (remhash dst const-track)))
         (push inst result))))
    (nreverse result)))
