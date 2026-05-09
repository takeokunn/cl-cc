;;;; optimizer-purity.lisp — Transitive Function Purity Analysis
;;;;
;;;; Call graph construction and recursive SCC detection for inlining eligibility.
;;;; These functions are called by optimizer-inline-pass.lisp to determine which
;;;; functions can be safely inlined (non-recursive, transitively pure).
;;;;
;;;; Load order: after optimizer-inline.lisp.

(in-package :cl-cc/optimize)

(defun opt-build-call-graph (instructions func-defs name-to-label)
  "Return label → direct callees graph for the linear function bodies in FUNC-DEFS."
  (let ((graph (make-hash-table :test #'equal)))
    (maphash (lambda (label def)
               (let ((body (getf def :body))
                     (reg-track (make-hash-table :test #'eq))
                     (callees nil))
                 (dolist (inst body)
                   (typecase inst
                     ((or vm-closure vm-func-ref)
                      (let ((callee (vm-label-name inst)))
                        (when (gethash callee func-defs)
                          (setf (gethash (vm-dst inst) reg-track) callee))))
                     (vm-const
                      (let ((callee (and (symbolp (vm-value inst))
                                         (gethash (vm-value inst) name-to-label))))
                        (when callee
                          (setf (gethash (vm-dst inst) reg-track) callee))))
                     (vm-call
                      (let ((callee (gethash (vm-func-reg inst) reg-track)))
                        (when callee
                          (pushnew callee callees :test #'equal))))
                     (t
                      (let ((dst (opt-inst-dst inst)))
                        (when dst (remhash dst reg-track))))))
                 (setf (gethash label graph) callees)))
             func-defs)
    graph))

(defun %opt-call-graph-reaches-self-p (graph start node seen)
  "Return T if NODE can reach START via GRAPH edges, using SEEN to avoid cycles."
  (dolist (next (gethash node graph) nil)
    (cond
      ((equal next start) (return-from %opt-call-graph-reaches-self-p t))
      ((not (gethash next seen))
       (setf (gethash next seen) t)
       (when (%opt-call-graph-reaches-self-p graph start next seen)
         (return-from %opt-call-graph-reaches-self-p t))))))

(defun opt-call-graph-recursive-labels (graph)
  "Return the set of labels that are in a recursive SCC of GRAPH."
  (let ((recursive (make-hash-table :test #'equal)))
    (maphash (lambda (label _callees)
               (when (%opt-call-graph-reaches-self-p graph label label (make-hash-table :test #'equal))
                 (setf (gethash label recursive) t)))
             graph)
    recursive))

(defun opt-function-body-transitively-pure-p (body func-defs name-to-label pure-labels)
  "Return T if BODY is transitively pure under PURE-LABELS.

Known direct calls are allowed only when their callee label is already marked
pure in PURE-LABELS. Unknown calls remain conservative and therefore impure."
  (let ((reg-track (make-hash-table :test #'eq)))
    (dolist (inst body t)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (if (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label present-p)
             (gethash (vm-src inst) reg-track)
           (if present-p
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        ((or vm-call vm-tail-call)
         (let ((callee (gethash (vm-func-reg inst) reg-track)))
           (unless (and callee (gethash callee pure-labels))
             (return-from opt-function-body-transitively-pure-p nil))
           (when (opt-inst-dst inst)
             (remhash (opt-inst-dst inst) reg-track))))
        (t
         (unless (or (typep inst 'vm-ret)
                     (typep inst 'vm-label)
                     (opt-inst-pure-p inst))
           (return-from opt-function-body-transitively-pure-p nil))
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track))))))))

(defun opt-infer-transitive-function-purity (instructions)
  "Infer a conservative set of transitively pure function labels.

The inference covers known direct-call edges between collected linear function
bodies. Recursive SCCs remain conservative and are not marked pure."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (graph (opt-build-call-graph instructions func-defs name-to-label))
         (recursive-labels (opt-call-graph-recursive-labels graph))
         (pure-labels (make-hash-table :test #'equal))
         (changed t))
    (loop while changed
          do (setf changed nil)
             (maphash (lambda (label def)
                        (unless (or (gethash label pure-labels)
                                    (gethash label recursive-labels))
                          (when (opt-function-body-transitively-pure-p
                                 (getf def :body) func-defs name-to-label pure-labels)
                            (setf (gethash label pure-labels) t
                                  changed t))))
                      func-defs))
     pure-labels))

(defun %opt-pure-call-boundary-p (inst)
  "Return T when INST ends a straight-line pure-call optimization region."
  (or (typep inst 'vm-label)
      (typep inst 'vm-jump)
      (typep inst 'vm-jump-zero)
      (typep inst 'vm-ret)
      (typep inst 'vm-halt)
      (typep inst 'vm-tail-call)))

(defun %opt-pure-call-key (label args)
  "Return the CSE memo key for a pure direct call to LABEL with ARGS."
  (cons label (copy-list args)))

(defun %opt-pure-call-entry-uses-reg-p (key result-reg reg)
  "Return T when KEY/RESULT-REG depends on REG."
  (or (eq result-reg reg)
      (member reg (cdr key) :test #'eq)))

(defun %opt-pure-call-kill-reg (call-cse reg)
  "Drop memoized pure-call entries invalidated by overwriting REG."
  (when reg
    (let ((doomed nil))
      (maphash (lambda (key result-reg)
                 (when (%opt-pure-call-entry-uses-reg-p key result-reg reg)
                   (push key doomed)))
               call-cse)
      (dolist (key doomed)
        (remhash key call-cse)))))

(defun %opt-pure-call-record-if-safe (call-cse key dst)
  "Record KEY -> DST only when DST does not overwrite a key argument register."
  (when (and key
             dst
             (not (member dst (cdr key) :test #'eq)))
    (setf (gethash key call-cse) dst)))

(defun %opt-resolved-pure-direct-call-label (inst reg-track pure-labels)
  "Return INST's direct callee label when it is known and transitively pure."
  (when (typep inst 'vm-call)
    (let ((label (gethash (vm-func-reg inst) reg-track)))
      (and label
           (gethash label pure-labels)
           label))))

(defun %opt-rewrite-pure-direct-calls (instructions name-to-label pure-labels)
  "Rewrite repeated pure direct calls inside straight-line regions.

The pass only reuses known direct `vm-call` sites whose callee label is marked
pure in PURE-LABELS.  CSE state is flushed at labels and control-flow
boundaries, and memo entries are killed whenever one of their argument/result
registers is overwritten."
  (let ((reg-track (make-hash-table :test #'eq))
        (call-cse (make-hash-table :test #'equal))
        (result nil))
    (labels ((flush-state ()
               (clrhash reg-track)
               (clrhash call-cse)))
      (dolist (inst instructions (nreverse result))
        (cond
          ((%opt-pure-call-boundary-p inst)
           (flush-state)
           (push inst result))
          ((typep inst 'vm-call)
           (let* ((dst (opt-inst-dst inst))
                  (label (%opt-resolved-pure-direct-call-label inst reg-track pure-labels))
                  (key (and label dst (%opt-pure-call-key label (vm-args inst))))
                  (existing (and key (gethash key call-cse))))
             (cond
               ((and existing (eq existing dst))
                ;; The same register still holds the same pure result.
                nil)
               (existing
                (%opt-pure-call-kill-reg call-cse dst)
                (remhash dst reg-track)
                (push (make-vm-move :dst dst :src existing) result))
               (t
                (%opt-pure-call-kill-reg call-cse dst)
                (push inst result)
                (remhash dst reg-track)
                (%opt-pure-call-record-if-safe call-cse key dst)))))
          (t
           (let ((dst (opt-inst-dst inst)))
             (%opt-pure-call-kill-reg call-cse dst)
             (%opt-devirt-track-designator inst name-to-label reg-track)
             (push inst result))))))))

(defun %opt-remove-dead-pure-direct-calls (instructions name-to-label pure-labels)
  "Remove unused known-pure direct calls conservatively.

Only direct `vm-call` sites with a known pure callee label and an unused
destination register are removed.  Calls with unresolved callees, unknown side
effects, or no destination are preserved."
  (let ((used (make-hash-table :test #'eq))
        (reg-track (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    (labels ((flush-state ()
               (clrhash reg-track)))
      (dolist (inst instructions (nreverse result))
        (cond
          ((%opt-pure-call-boundary-p inst)
           (flush-state)
           (push inst result))
          ((typep inst 'vm-call)
           (let* ((dst (opt-inst-dst inst))
                  (label (%opt-resolved-pure-direct-call-label inst reg-track pure-labels)))
             (if (and dst label (not (gethash dst used)))
                 nil
                 (progn
                   (remhash dst reg-track)
                   (push inst result)))))
          (t
           (%opt-devirt-track-designator inst name-to-label reg-track)
           (push inst result)))))))

(defun opt-pass-pure-call-optimization (instructions)
  "Conservatively optimize repeated and dead known-pure direct calls.

The pass first infers a transitive pure-label set with
`opt-infer-transitive-function-purity`, then applies two safe rewrites:

1. Reuse repeated straight-line `vm-call` sites that resolve to the same pure
   callee label and the same argument registers.
2. Remove known-pure direct calls whose destination register is never used.

Unknown calls, `vm-apply`, `vm-generic-call`, unresolved indirect calls, and
recursive SCCs remain untouched by construction."
  ;; Fast bailout: skip the whole call-graph fixed-point when the input has
  ;; no VM-CALL instructions at all (the dominant case for tiny test inputs).
  (unless (some (lambda (inst) (typep inst 'vm-call)) instructions)
    (return-from opt-pass-pure-call-optimization instructions))
  (let ((pure-labels (opt-infer-transitive-function-purity instructions)))
    (when (= 0 (hash-table-count pure-labels))
      (return-from opt-pass-pure-call-optimization instructions))
    (let* ((name-to-label (opt-build-function-name-map instructions))
           (cse-rewritten (%opt-rewrite-pure-direct-calls instructions name-to-label pure-labels)))
      (%opt-remove-dead-pure-direct-calls cse-rewritten name-to-label pure-labels))))
