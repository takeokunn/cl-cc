(in-package :cl-cc/optimize)
;;; ─── Memory Optimization Passes ─────────────────────────────────────────────
;;;
;;; Extracted from optimizer-memory.lisp.
;;; Depends on: optimizer-memory.lisp (alias analysis: opt-compute-heap-aliases,
;;;   opt-slot-alias-key, opt-rewrite-inst-regs, opt-inst-dst, opt-inst-pure-p).
;;; Load order: immediately after optimizer-memory.lisp.

;;; ─── Shared mem-pass-state struct and helpers ────────────────────────────────

(defvar *opt-bounds-check-eliminable-metadata* (make-hash-table :test #'eq)
  "EQ side table for BCE annotations on existing VM instructions.")

(defun opt-mark-bounds-check-eliminable (inst &key array-reg index-reg length-reg block)
  "Annotate INST as having a proven redundant array bounds check.

The VM has no unchecked array instruction yet, so BCE records conservative
side-table metadata and returns INST unchanged. Downstream codegen can query this
metadata without requiring cross-package instruction-shape changes."
  (setf (gethash inst *opt-bounds-check-eliminable-metadata*)
        (list :bounds-check-eliminable t
              :array-reg array-reg
              :index-reg index-reg
              :length-reg length-reg
              :block block))
  inst)

(defun opt-bounds-check-eliminable-metadata (inst)
  "Return BCE metadata for INST, or NIL when INST is not annotated."
  (gethash inst *opt-bounds-check-eliminable-metadata*))

(defun opt-bounds-check-eliminable-marked-p (inst)
  "Return T when INST has been annotated by the BCE pass."
  (getf (opt-bounds-check-eliminable-metadata inst) :bounds-check-eliminable))

(defstruct (mem-pass-state (:conc-name mps-))
  "Mutable state shared by the DSE and store-to-load-forward passes."
  (result        nil                           :type list)
  (pending-order nil                           :type list)
  (pending-by-name (make-hash-table :test #'equal) :type hash-table))

(defun %mps-emit (state inst)
  (push inst (mps-result state)))

(defun %mps-pending-store (state key)
  (gethash key (mps-pending-by-name state)))

(defun %mps-remember-store (state key inst)
  (unless (gethash key (mps-pending-by-name state))
    (push key (mps-pending-order state)))
  (setf (gethash key (mps-pending-by-name state)) inst))

(defun %mps-drop-pending (state key)
  (remhash key (mps-pending-by-name state))
  (setf (mps-pending-order state)
        (remove key (mps-pending-order state) :test #'equal)))

(defun %mps-flush-one (state key)
  (let ((inst (%mps-pending-store state key)))
    (when inst
      (%mps-emit state inst)
      (%mps-drop-pending state key))))

(defun %mps-flush-all (state)
  (dolist (key (reverse (mps-pending-order state)))
    (%mps-flush-one state key)))

(defun %mps-pending-uses-reg-p (pending reg)
  "Return T if PENDING store instruction reads REG."
  (typecase pending
    (vm-set-global (eq (cl-cc/vm::vm-set-global-src pending) reg))
    (vm-slot-write (or (eq (cl-cc/vm::vm-slot-write-obj-reg pending) reg)
                       (eq (cl-cc/vm::vm-slot-write-value-reg pending) reg)))))

(defun %mps-flush-if-src-overwritten (state dst)
  "Flush pending stores that read DST (i.e., their source register is DST)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending (%mps-pending-uses-reg-p pending dst))
        (%mps-flush-one state key)))))

(defun %mps-flush-dependent-on-reg (state reg &key exclude-key)
  "Flush pending stores that reference REG (but not EXCLUDE-KEY)."
  (dolist (key (copy-list (mps-pending-order state)))
    (let ((pending (%mps-pending-store state key)))
      (when (and pending
                  (not (equal key exclude-key))
                  (%mps-pending-uses-reg-p pending reg))
        (%mps-flush-one state key)))))

;;; ─── Cons slot forwarding ─────────────────────────────────────────────────

(defun %opt-cons-slot-kill-dependent-on-reg (facts reg)
  "Remove cons-slot facts made stale when REG is overwritten."
  (when reg
    (let ((dead nil))
      (maphash (lambda (cell-reg entry)
                 (when (or (eq cell-reg reg)
                           (eq (car entry) reg)
                           (eq (cdr entry) reg))
                   (push cell-reg dead)))
               facts)
      (dolist (cell-reg dead)
        (remhash cell-reg facts)))))

(defun %opt-cons-slot-safe-source-p (dst car-reg cdr-reg)
  "Return T when a vm-cons DST write does not destroy either slot source."
  (and (not (eq dst car-reg))
       (not (eq dst cdr-reg))))

(defun %opt-cons-slot-mutation-boundary-p (inst)
  "Return T when INST may mutate heap/global state or call arbitrary code."
  (member (vm-inst-effect-kind inst)
          '(:write-global :io :control :unknown)
          :test #'eq))

(defun opt-pass-cons-slot-forward (instructions)
  "Forward car/cdr of a fresh vm-cons to moves from the original slot registers.

The pass is intentionally straight-line and conservative: labels/control-flow,
calls, unknown effects, and heap mutations clear all facts, while overwrites of a
tracked cell register or slot source kill only the dependent facts."
  (let ((facts (make-hash-table :test #'eq))
        (result nil))
    (labels ((emit (inst) (push inst result))
             (remember-cons (dst car-reg cdr-reg)
               (if (%opt-cons-slot-safe-source-p dst car-reg cdr-reg)
                   (setf (gethash dst facts) (cons car-reg cdr-reg))
                   (remhash dst facts)))
             (copy-fact (dst src)
               (multiple-value-bind (entry found-p) (gethash src facts)
                 (if found-p
                     (setf (gethash dst facts) entry)
                     (remhash dst facts)))))
      (dolist (inst instructions (nreverse result))
        (typecase inst
          (vm-cons
           (let ((dst (vm-dst inst)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (remember-cons dst (vm-car-reg inst) (vm-cdr-reg inst))
             (emit inst)))
          (vm-move
           (let ((dst (vm-dst inst))
                 (src (vm-src inst)))
             (unless (eq dst src)
               (%opt-cons-slot-kill-dependent-on-reg facts dst)
               (copy-fact dst src))
             (emit inst)))
          (vm-car
           (let* ((dst (vm-dst inst))
                  (entry (gethash (vm-src inst) facts)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (if entry
                 (emit (make-vm-move :dst dst :src (car entry)))
                 (emit inst))))
          (vm-cdr
           (let* ((dst (vm-dst inst))
                  (entry (gethash (vm-src inst) facts)))
             (%opt-cons-slot-kill-dependent-on-reg facts dst)
             (if entry
                 (emit (make-vm-move :dst dst :src (cdr entry)))
                 (emit inst))))
          ((or vm-label vm-jump vm-jump-zero vm-ret vm-halt)
           (clrhash facts)
           (emit inst))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst
               (%opt-cons-slot-kill-dependent-on-reg facts dst)))
           (when (%opt-cons-slot-mutation-boundary-p inst)
             (clrhash facts))
           (emit inst)))))))

(defun opt-pass-dead-store-elim (instructions)
  "Eliminate overwritten stores in straight-line code.

   Conservative rules:
   - a later store to the same global/slot kills the earlier pending store
   - vm-get-global / vm-slot-read force the matching pending store to be emitted first
   - labels and non-pure instructions flush all pending stores"
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
         (let ((dst (cl-cc/vm::vm-get-global-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (%mps-flush-one state (cl-cc/vm::vm-get-global-name inst))
         (%mps-emit state inst))
        (vm-slot-read
         (let ((dst (cl-cc/vm::vm-slot-read-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (%mps-flush-one state (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                                    (cl-cc/vm::vm-slot-read-slot-name inst)
                                                    alias-roots))
         (%mps-emit state inst))
        (vm-set-global
         (%mps-remember-store state (cl-cc/vm::vm-set-global-name inst) inst))
        (vm-slot-write
         (%mps-remember-store state (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                                         (cl-cc/vm::vm-slot-write-slot-name inst)
                                                         alias-roots)
                              inst))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst (%mps-flush-if-src-overwritten state dst)))
         (unless (opt-inst-pure-p inst) (%mps-flush-all state))
         (%mps-emit state inst))))
    (%mps-flush-all state)
    (nreverse (mps-result state))))

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

(defun %available-store-rewrite-block (block entry-state alias-roots)
  "Rewrite store-to-load pairs inside BLOCK using ENTRY-STATE as the incoming facts."
  (let ((state  (%available-store-copy-state entry-state))
        (result nil))
    (labels ((emit (inst)
               (push inst result)))
      (dolist (inst (bb-instructions block) (nreverse result))
        (typecase inst
          (vm-get-global
           (let* ((key         (%available-store-location-key inst alias-roots))
                  (store       (and key (gethash key state)))
                  (replacement (and store (%available-store-forwarded-load inst store)))
                  (dst         (cl-cc/vm::vm-get-global-dst inst)))
             (if replacement
                 (progn
                   (emit replacement)
                   (%available-store-kill-dependent-on-reg state dst :exclude-key key))
                 (progn
                   (%available-store-kill-dependent-on-reg state dst)
                   (emit inst)))))
          (vm-slot-read
           (let* ((key         (%available-store-location-key inst alias-roots))
                  (store       (and key (gethash key state)))
                  (replacement (and store (%available-store-forwarded-load inst store)))
                  (dst         (cl-cc/vm::vm-slot-read-dst inst)))
             (if replacement
                 (progn
                   (emit replacement)
                   (%available-store-kill-dependent-on-reg state dst :exclude-key key))
                 (progn
                   (%available-store-kill-dependent-on-reg state dst)
                   (emit inst)))))
          ((or vm-set-global vm-slot-write)
           (%available-store-remember-store state inst alias-roots)
           (emit inst))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst
               (%available-store-kill-dependent-on-reg state dst)))
           (when (%available-store-clobber-inst-p inst)
             (clrhash state))
           (emit inst)))))))

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

;;; ─── Bounds check elimination annotation ──────────────────────────────────

(defun %opt-bce-array-length-regs (instructions)
  "Return a conservative array-reg -> length-reg table for BCE.

Tracks lengths produced by vm-make-array and explicit vm-array-length
instructions. Unknown overwrites kill stale array-length facts."
  (let ((array-lengths (make-hash-table :test #'eq)))
    (dolist (inst instructions array-lengths)
      (typecase inst
        (vm-make-array
         (let ((dst (vm-dst inst)))
           (when dst
             (setf (gethash dst array-lengths) (vm-size-reg inst)))))
        (vm-array-length
         (let ((known (gethash (vm-src inst) array-lengths)))
           ;; Preserve a known allocation size when available; otherwise record
           ;; the explicit length register as the best available fact.
           (setf (gethash (vm-src inst) array-lengths)
                 (or known (vm-dst inst)))))
        (vm-move
         (let ((dst (vm-dst inst)))
           (when dst
             (multiple-value-bind (length-reg found-p)
                 (gethash (vm-src inst) array-lengths)
               (if found-p
                   (setf (gethash dst array-lengths) length-reg)
                   (remhash dst array-lengths))))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when (and dst (not (typep inst 'vm-array-length)))
             (remhash dst array-lengths))))))))

(defun %opt-bce-inst-array-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-array-reg inst))
    (t nil)))

(defun %opt-bce-inst-index-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-index-reg inst))
    (t nil)))

(defun %opt-bce-instruction-blocks (cfg)
  "Return an EQ table mapping instructions to their containing basic block."
  (let ((inst->block (make-hash-table :test #'eq)))
    (loop for block across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions block))
               (setf (gethash inst inst->block) block)))
    inst->block))

(defun opt-pass-bounds-check-elimination (instructions)
  "Annotate array accesses whose bounds checks are provably redundant.

This FR-039 pass is intentionally non-rewriting until unchecked VM array
instructions exist. It builds CFG facts, computes dominators and loop depths,
uses path-sensitive integer ranges, and marks proven `vm-aref` / `vm-aset`
instructions with BCE metadata. The returned instruction list is valid and keeps
the original instruction objects/order."
  (let* ((cfg (cfg-build instructions))
         (length-regs (%opt-bce-array-length-regs instructions))
         (inst->block (%opt-bce-instruction-blocks cfg)))
    ;; Clear stale BCE metadata from previous passes.
    (clrhash *opt-bounds-check-eliminable-metadata*)
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (let ((ranges (opt-compute-path-sensitive-ranges cfg)))
      (dolist (inst instructions instructions)
        (when (typep inst '(or vm-aref vm-aset))
          (let* ((array-reg (%opt-bce-inst-array-reg inst))
                 (index-reg (%opt-bce-inst-index-reg inst))
                 (length-reg (and array-reg (gethash array-reg length-regs)))
                 (block (gethash inst inst->block)))
            (when (and index-reg length-reg
                       (opt-array-bounds-check-eliminable-p
                        index-reg length-reg ranges block))
              (opt-mark-bounds-check-eliminable inst
                                                :array-reg array-reg
                                                :index-reg index-reg
                                                :length-reg length-reg
                                                :block block))))))))
