(in-package :cl-cc/optimize)

(defun %opt-pass-store-to-load-forward-linear (instructions)
  "Forward pending vm-set-global values into matching vm-get-global loads.

This is the original straight-line implementation. It remains the fast path for
single-block instruction streams so existing behavior stays intact."
  (let ((state       (make-mem-pass-state))
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (%mps-flush-all state)
         (%mps-emit state inst))
        ((or vm-jump vm-jump-zero vm-ret vm-halt)
         (%mps-flush-all state)
         (%mps-emit state inst))
        (vm-get-global
         (let* ((key   (list :global (cl-cc/vm::vm-get-global-name inst)))
                (store (%mps-pending-store state key))
                (dst   (cl-cc/vm::vm-get-global-dst inst)))
           (if store
               (progn (%mps-emit state (make-vm-move :dst dst :src (cl-cc/vm::vm-set-global-src store)))
                      (when dst
                        (%mps-flush-dependent-on-reg state dst :exclude-key key)))
               (progn (when dst
                        (%mps-flush-dependent-on-reg state dst))
                      (%mps-flush-one state key)
                      (%mps-emit state inst)))))
        (vm-slot-read
         (let* ((key   (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                           (cl-cc/vm::vm-slot-read-slot-name inst)
                                           alias-roots))
                (store (%mps-pending-store state key))
                (dst   (cl-cc/vm::vm-slot-read-dst inst)))
           (if store
               (progn (%mps-emit state (make-vm-move :dst dst :src (cl-cc/vm::vm-slot-write-value-reg store)))
                      (when dst
                        (%mps-flush-dependent-on-reg state dst :exclude-key key)))
               (progn (when dst
                        (%mps-flush-dependent-on-reg state dst))
                      (%mps-flush-one state key)
                      (%mps-emit state inst)))))
        (vm-set-global
         (%mps-remember-store state (list :global (cl-cc/vm::vm-set-global-name inst)) inst))
        (vm-slot-write
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                                         (cl-cc/vm::vm-slot-write-slot-name inst)
                                                         alias-roots)
                              inst))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (%mps-flush-dependent-on-reg state dst)))
         (unless (opt-inst-pure-p inst) (%mps-flush-all state))
          (%mps-emit state inst))))
    (%mps-flush-all state)
    (nreverse (mps-result state))))

(defun %available-store-copy-state (state)
  "Return a deep-enough copy of available-store STATE."
  (let ((copy (make-hash-table :test #'equal)))
    (when state
      (maphash (lambda (key value)
                 (setf (gethash key copy) value))
               state))
    copy))

(defun %available-store-state-equal (a b)
  "Return T when available-store states A and B map the same keys to the same stores."
  (and (= (hash-table-count a) (hash-table-count b))
       (let ((same t))
         (maphash (lambda (key value)
                    (multiple-value-bind (other found-p)
                        (gethash key b)
                      (unless (and found-p (eq value other))
                        (setf same nil))))
                  a)
         same)))

(defun %available-store-merge-states (states)
  "Keep only store facts that every predecessor agrees on."
  (cond
    ((null states)
     (make-hash-table :test #'equal))
    ((null (cdr states))
     (%available-store-copy-state (car states)))
    (t
     (let* ((first  (car states))
            (merged (%available-store-copy-state first))
            (dead   nil))
       (maphash (lambda (key value)
                  (unless (every (lambda (state)
                                   (multiple-value-bind (other found-p)
                                       (gethash key state)
                                     (and found-p (eq value other))))
                                 (cdr states))
                    (push key dead)))
                first)
       (dolist (key dead)
         (remhash key merged))
       merged))))

(defun %available-store-location-key (inst alias-roots)
  "Return the modeled memory key for store/load INST, or NIL when unsupported.

CFG-aware forwarding models globals and slot accesses keyed by alias roots."
  (typecase inst
    ((or vm-set-global vm-get-global)
     (list :global (typecase inst
                     (vm-set-global (cl-cc/vm::vm-set-global-name inst))
                     (vm-get-global (cl-cc/vm::vm-get-global-name inst)))))
    ((or vm-slot-write vm-slot-read)
     (typecase inst
       (vm-slot-write
        (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                            (cl-cc/vm::vm-slot-write-slot-name inst)
                            alias-roots))
       (vm-slot-read
        (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                            (cl-cc/vm::vm-slot-read-slot-name inst)
                            alias-roots))))
    (t nil)))

(defun %available-store-remember-store (state inst alias-roots)
  "Record INST as the latest available store in STATE when modeled."
  (let ((key (%available-store-location-key inst alias-roots)))
    (when key
      (setf (gethash key state) inst))
    key))

(defun %available-store-kill-dependent-on-reg (state reg &key exclude-key)
  "Drop store facts whose saved source register is overwritten by REG."
  (when reg
    (let ((dead nil))
      (maphash (lambda (key store)
                 (when (and store
                            (not (equal key exclude-key))
                            (%mps-pending-uses-reg-p store reg))
                   (push key dead)))
               state)
      (dolist (key dead)
        (remhash key state)))))

(defun %available-store-control-inst-p (inst)
  "Return T when INST is a CFG terminator handled by explicit graph edges."
  (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))

(defun %available-store-clobber-inst-p (inst)
  "Return T when INST conservatively invalidates all available-store facts."
  (and (not (typep inst '(or vm-set-global)))
       (not (%available-store-control-inst-p inst))
       (not (opt-inst-pure-p inst))))

(defun %available-store-transfer (block in-state alias-roots)
  "Transfer available-store facts through BLOCK."
  (let ((state (%available-store-copy-state in-state)))
    (dolist (inst (bb-instructions block) state)
      (typecase inst
        (vm-get-global
         (let* ((key   (%available-store-location-key inst alias-roots))
                (store (and key (gethash key state)))
                (dst   (cl-cc/vm::vm-get-global-dst inst)))
           (if store
               (%available-store-kill-dependent-on-reg state dst :exclude-key key)
               (%available-store-kill-dependent-on-reg state dst))))
        (vm-slot-read
         (let* ((key   (%available-store-location-key inst alias-roots))
                (store (and key (gethash key state)))
                (dst   (cl-cc/vm::vm-slot-read-dst inst)))
           (if store
               (%available-store-kill-dependent-on-reg state dst :exclude-key key)
               (%available-store-kill-dependent-on-reg state dst))))
        ((or vm-set-global vm-slot-write)
         (%available-store-remember-store state inst alias-roots))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (%available-store-kill-dependent-on-reg state dst)))
         (when (%available-store-clobber-inst-p inst)
           (clrhash state)))))))

(defun %available-store-forwarded-load (inst store)
  "Return a vm-move replacement for INST using STORE, or NIL when unsupported."
  (typecase inst
    (vm-get-global
     (when (typep store 'vm-set-global)
       (make-vm-move :dst (cl-cc/vm::vm-get-global-dst inst)
                     :src (cl-cc/vm::vm-set-global-src store))))
    (vm-slot-read
     (when (typep store 'vm-slot-write)
       (make-vm-move :dst (cl-cc/vm::vm-slot-read-dst inst)
                     :src (cl-cc/vm::vm-slot-write-value-reg store))))))

(defun %available-store-const-value-before-terminator (block reg)
  "Return constant integer value of REG at BLOCK terminator when provable, else NIL.

Conservative straight-line scan: tracks vm-const and vm-move aliases within BLOCK."
  (let ((env (make-hash-table :test #'eq)))
    (dolist (inst (bb-instructions block))
      (typecase inst
        (vm-const
         (let ((dst (vm-dst inst))
               (value (vm-value inst)))
           (when (and dst (integerp value))
             (setf (gethash dst env) value))))
        (vm-move
         (let ((dst (vm-dst inst))
               (src (vm-src inst)))
           (when dst
             (if src
                 (multiple-value-bind (value found-p) (gethash src env)
                   (if found-p
                       (setf (gethash dst env) value)
                       (remhash dst env)))
                 (remhash dst env)))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst env))))))
    (gethash reg env)))

(defun %available-store-edge-feasible-p (pred succ)
  "Return T when edge PRED -> SUCC is feasible under local constant branch facts.

Only handles vm-jump-zero with block-local constant condition values.
Unknown conditions remain feasible (conservative)."
  (let ((term (car (last (bb-instructions pred)))))
    (cond
      ((typep term 'vm-jump-zero)
       (let* ((cond-reg (vm-reg term))
              (target-label (vm-label-name term))
              (succ-label (and (bb-label succ) (vm-name (bb-label succ))))
              (const-value (%available-store-const-value-before-terminator pred cond-reg))
              (zero-edge-p (and succ-label (equal succ-label target-label))))
         (if const-value
             (if (= const-value 0)
                 zero-edge-p
                 (not zero-edge-p))
             t)))
      (t t))))

(defun %available-store-refined-entry-state (block flow reachable)
  "Compute BLOCK entry state by merging only feasible predecessor OUT states."
  (let* ((preds (bb-predecessors block))
         (feasible-preds (remove-if-not (lambda (pred)
                                          (and (gethash pred reachable)
                                               (%available-store-edge-feasible-p pred block)))
                                        preds))
         (states (mapcar (lambda (pred)
                           (gethash pred (opt-dataflow-result-out flow)))
                         feasible-preds)))
    (%available-store-merge-states states)))

(defun %available-store-reachable-blocks (cfg)
  "Return EQ hash-table of blocks reachable under local constant branch pruning."
  (let ((reachable (make-hash-table :test #'eq))
        (work nil))
    (when (cfg-entry cfg)
      (setf (gethash (cfg-entry cfg) reachable) t)
      (push (cfg-entry cfg) work))
    (loop while work
          do (let ((block (pop work)))
               (dolist (succ (bb-successors block))
                 (when (and (%available-store-edge-feasible-p block succ)
                            (not (gethash succ reachable)))
                   (setf (gethash succ reachable) t)
                   (push succ work)))))
    reachable))

(defun %available-store-build-rewrite-map (block entry-state alias-roots)
  "Return hash-table mapping original instructions to rewritten instructions for BLOCK.

The mapping preserves instruction count and order by replacing only supported
loads in-place."
  (let ((state (%available-store-copy-state entry-state))
        (rewrites (make-hash-table :test #'eq)))
    (dolist (inst (bb-instructions block) rewrites)
      (typecase inst
        (vm-get-global
         (let* ((key         (%available-store-location-key inst alias-roots))
                (store       (and key (gethash key state)))
                (replacement (and store (%available-store-forwarded-load inst store)))
                (dst         (cl-cc/vm::vm-get-global-dst inst)))
           (if replacement
               (progn
                 (setf (gethash inst rewrites) replacement)
                 (%available-store-kill-dependent-on-reg state dst :exclude-key key))
               (%available-store-kill-dependent-on-reg state dst))))
        (vm-slot-read
         (let* ((key         (%available-store-location-key inst alias-roots))
                (store       (and key (gethash key state)))
                (replacement (and store (%available-store-forwarded-load inst store)))
                (dst         (cl-cc/vm::vm-slot-read-dst inst)))
           (if replacement
               (progn
                 (setf (gethash inst rewrites) replacement)
                 (%available-store-kill-dependent-on-reg state dst :exclude-key key))
               (%available-store-kill-dependent-on-reg state dst))))
        ((or vm-set-global vm-slot-write)
         (%available-store-remember-store state inst alias-roots))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (%available-store-kill-dependent-on-reg state dst)))
         (when (%available-store-clobber-inst-p inst)
           (clrhash state)))))))

(defun %opt-pass-store-to-load-forward-cfg (instructions)
  "Forward stores to loads across basic blocks when the CFG proves the store available."
  (let* ((cfg         (cfg-build instructions))
         (alias-roots (opt-compute-heap-aliases instructions))
         (initial     (make-hash-table :test #'equal))
         (flow        (opt-run-dataflow cfg
                                        :direction :forward
                                        :meet #'%available-store-merge-states
                                        :transfer (lambda (block in-state)
                                                    (%available-store-transfer block in-state alias-roots))
                                        :state-equal #'%available-store-state-equal
                                        :initial-state initial
                                        :boundary-state initial
                                        :copy-state #'%available-store-copy-state)))
    (let ((rewrites (make-hash-table :test #'eq))
          (reachable (%available-store-reachable-blocks cfg)))
      (loop for block across (cfg-blocks cfg)
            when (and block (gethash block reachable))
            do (let* ((entry-state (%available-store-refined-entry-state block flow reachable))
                      (per-block
                       (%available-store-build-rewrite-map
                        block
                        entry-state
                        alias-roots)))
                 (maphash (lambda (old new)
                            (setf (gethash old rewrites) new))
                          per-block)))
      (mapcar (lambda (inst)
                (or (gethash inst rewrites) inst))
              instructions))))

(defun opt-pass-store-to-load-forward (instructions)
  "Forward available store values into matching loads.

Single-block streams keep the original straight-line implementation. Multi-block
streams use CFG/dataflow facts and only forward stores proven available on every
incoming path to the load's basic block."
  (let ((cfg (cfg-build instructions)))
    (if (= (length (cfg-blocks cfg)) 1)
        (%opt-pass-store-to-load-forward-linear instructions)
        (%opt-pass-store-to-load-forward-cfg instructions))))
