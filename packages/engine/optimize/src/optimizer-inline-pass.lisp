;;;; optimizer-inline-pass.lisp — Inlining Infrastructure (memo, DCE)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: opt-make-pure-function-memo-table,
;;; opt-pure-function-memo-get, opt-pure-function-memo-put,
;;; opt-function-body-instruction-tables,
;;; opt-top-level-function-roots, opt-reachable-function-labels,
;;; opt-pass-global-dce.
;;;
;;; Load order: after optimizer-purity.lisp, before optimizer-inline-cost.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/optimize)

(defun opt-make-pure-function-memo-table ()
  "Create a memo table for pure-function result caching."
  (make-hash-table :test #'equal))

(defun opt-pure-function-memo-get (memo-table pure-labels label args)
  "Return cached result for pure LABEL/ARGS, or NIL with a miss flag."
  (if (gethash label pure-labels)
      (multiple-value-bind (value found-p)
          (gethash (list label args) memo-table)
        (values value found-p))
      (values nil nil)))

(defun opt-pure-function-memo-put (memo-table pure-labels label args result)
  "Store RESULT for pure LABEL/ARGS in MEMO-TABLE.
Impure or unknown labels are ignored conservatively."
  (when (gethash label pure-labels)
    (setf (gethash (list label args) memo-table) result))
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

;;; Cost model + main inline pass → see optimizer-inline-cost.lisp
