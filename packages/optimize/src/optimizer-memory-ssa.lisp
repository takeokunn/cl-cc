(in-package :cl-cc/optimize)

(defun opt-compute-memory-ssa-snapshot (instructions)
  "Compute a straight-line Memory-SSA snapshot table for INSTRUCTIONS.

Returns an EQ hash-table mapping each modeled instruction to a plist:
  :kind     one of :def or :use
  :location canonical location key
  :in       incoming memory version
  :out      outgoing memory version

This intentionally models only straight-line versioning (no MemoryPhi)."
  (let ((annotations (make-hash-table :test #'eq))
        (version 0)
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions annotations)
      (let ((loc (%opt-memory-location-key inst alias-roots)))
        (cond
          ((and loc (opt-memory-def-inst-p inst))
           (let ((vin version))
             (incf version)
             (setf (gethash inst annotations)
                   (list :kind :def :location loc :in vin :out version))))
          ((and loc (opt-memory-use-inst-p inst))
            (setf (gethash inst annotations)
                  (list :kind :use :location loc :in version :out version))))))))

(defun %opt-memory-ssa-copy-state (state)
  (let ((copy (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (setf (gethash k copy) v))
             state)
    copy))

(defun %opt-memory-ssa-const-value-before-terminator (block reg)
  "Resolve REG to a block-local integer constant before BLOCK terminator, if known." 
  (let ((env (make-hash-table :test #'eq)))
    (dolist (inst (bb-instructions block))
      (when (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt))
        (return))
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

(defun %opt-memory-ssa-edge-feasible-p (pred succ)
  "Return T when edge PRED->SUCC is feasible under local constant branch facts." 
  (let ((term (car (last (bb-instructions pred)))))
    (cond
      ((typep term 'vm-jump-zero)
       (let* ((cond-reg (vm-reg term))
              (target-label (vm-label-name term))
              (succ-label (and (bb-label succ) (vm-name (bb-label succ))))
              (const-value (%opt-memory-ssa-const-value-before-terminator pred cond-reg))
              (zero-edge-p (and succ-label (equal succ-label target-label))))
         (if const-value
             (if (= const-value 0)
                 zero-edge-p
                 (not zero-edge-p))
             t)))
      (t t))))

(defun %opt-memory-ssa-reachable-blocks (cfg)
  "Return EQ hash-table of blocks reachable under local constant branch pruning." 
  (let ((reachable (make-hash-table :test #'eq))
        (work nil))
    (when (cfg-entry cfg)
      (setf (gethash (cfg-entry cfg) reachable) t)
      (push (cfg-entry cfg) work))
    (loop while work
          do (let ((block (pop work)))
               (dolist (succ (bb-successors block))
                 (when (and (%opt-memory-ssa-edge-feasible-p block succ)
                            (not (gethash succ reachable)))
                   (setf (gethash succ reachable) t)
                   (push succ work)))))
    reachable))

(defun %opt-memory-ssa-merge-states (states)
  "Keep location->version facts only when all incoming states agree."
  (cond
    ((null states)
     (make-hash-table :test #'equal))
    ((null (cdr states))
     (%opt-memory-ssa-copy-state (car states)))
    (t
     (let* ((first (car states))
            (merged (%opt-memory-ssa-copy-state first))
            (dead nil))
       (maphash (lambda (key value)
                  (unless (every (lambda (state)
                                   (multiple-value-bind (other found-p)
                                       (gethash key state)
                                     (and found-p (eql value other))))
                                 (cdr states))
                    (push key dead)))
                first)
       (dolist (key dead)
         (remhash key merged))
       merged))))

(defun %opt-memory-ssa-state-equal (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for key being the hash-keys of a
             always (multiple-value-bind (bv found-p)
                        (gethash key b)
                      (and found-p (eql (gethash key a) bv))))))

(defstruct (opt-memory-phi-node (:conc-name opt-memory-phi-))
  "Explicit MemoryPhi node metadata for a block-entry location join.

LOCATION is the canonical memory location key.
VERSION is the synthetic joined version assigned at block entry.
INCOMING is an alist of (pred-block . pred-version)."
  location
  version
  incoming)

(defun %opt-memory-ssa-synthesize-entry-phis (block predecessor-states phi-version-table next-version)
  "Return synthetic MemoryPhi versions for BLOCK entry.

When BLOCK has multiple predecessors and all predecessor states define a
location but disagree on its version, synthesize (or reuse) a fresh version id
for that BLOCK/location pair. Returns two values:
  1) hash-table location -> synthesized version
  2) updated NEXT-VERSION"
  (let ((phis (make-hash-table :test #'equal)))
    (when (and predecessor-states (cdr predecessor-states))
      (let* ((first-state (car predecessor-states))
             (rest-states (cdr predecessor-states)))
        (when first-state
          (maphash (lambda (loc first-version)
                     (when (every (lambda (state)
                                    (multiple-value-bind (version found-p)
                                        (gethash loc state)
                                      (and found-p (integerp version))))
                                   rest-states)
                       (unless (every (lambda (state)
                                        (eql first-version (gethash loc state)))
                                      rest-states)
                         (let* ((phi-key (list block loc))
                                (version (gethash phi-key phi-version-table)))
                           (unless version
                             (incf next-version)
                             (setf version next-version
                                   (gethash phi-key phi-version-table) version))
                           (setf (gethash loc phis) version)))))
                   first-state))))
    (values phis next-version)))

(defun opt-memory-ssa-phi-nodes (annotations &optional block)
  "Return explicit MemoryPhi nodes recorded in ANNOTATIONS.

When BLOCK is provided, return only that block's phi node list.
Otherwise return an EQ hash-table block -> list of `opt-memory-phi-node'."
  (let ((phi-table (gethash :memory-phi-nodes annotations)))
    (cond
      (block (copy-list (or (and phi-table (gethash block phi-table)) nil)))
      (phi-table
       (let ((copy (make-hash-table :test #'eq)))
         (maphash (lambda (b nodes)
                    (setf (gethash b copy) (copy-list nodes)))
                  phi-table)
         copy))
      (t (make-hash-table :test #'eq)))))

(defun opt-compute-memory-ssa-cfg-snapshot (instructions)
  "Compute a CFG-aware conservative Memory-SSA snapshot for INSTRUCTIONS.

This helper assigns each memory-def instruction a stable monotonically
increasing version id (by instruction order), then propagates location->version
facts over CFG with a strict predecessor-agreement meet. At join points where
all predecessor states define a location but disagree on its version, this
helper synthesizes a fresh MemoryPhi-like version id for that location."
  (let* ((cfg (cfg-build instructions))
         (reachable (%opt-memory-ssa-reachable-blocks cfg))
         (alias-roots (opt-compute-heap-aliases instructions))
         (annotations (make-hash-table :test #'eq))
         (phi-node-table (make-hash-table :test #'eq))
         (def-version (make-hash-table :test #'eq))
         (phi-version-table (make-hash-table :test #'equal))
         (phi-aware-in (make-hash-table :test #'eq))
         (phi-aware-out (make-hash-table :test #'eq))
         (next-version 0)
         (rpo (cfg-compute-rpo cfg)))
    (dolist (inst instructions)
      (let ((loc (%opt-memory-location-key inst alias-roots)))
        (when (and loc (opt-memory-def-inst-p inst))
          (incf next-version)
          (setf (gethash inst def-version) next-version))))

    (dolist (block rpo)
      (setf (gethash block phi-aware-in) (make-hash-table :test #'equal)
            (gethash block phi-aware-out) (make-hash-table :test #'equal)))

    (loop with changed = t
          while changed
          do (setf changed nil)
             (dolist (block rpo)
               (let* ((preds (bb-predecessors block))
                      (feasible-preds (remove-if-not (lambda (pred)
                                                       (and (gethash pred reachable)
                                                            (%opt-memory-ssa-edge-feasible-p pred block)))
                                                     preds))
                      (pred-states (mapcar (lambda (pred)
                                             (or (gethash pred phi-aware-out)
                                                 (make-hash-table :test #'equal)))
                                           feasible-preds))
                       (entry-base (if (eq block (cfg-entry cfg))
                                       (make-hash-table :test #'equal)
                                       (%opt-memory-ssa-merge-states pred-states)))
                       (entry-state (%opt-memory-ssa-copy-state entry-base)))
                 (multiple-value-bind (entry-phis updated-version)
                     (%opt-memory-ssa-synthesize-entry-phis
                      block pred-states phi-version-table next-version)
                   (setf next-version updated-version)
                   (when (> (hash-table-count entry-phis) 0)
                      (let ((nodes nil))
                        (maphash (lambda (loc version)
                                   (let ((incoming nil))
                                     (dolist (pred feasible-preds)
                                       (let* ((pred-state (or (gethash pred phi-aware-out)
                                                              (make-hash-table :test #'equal)))
                                              (pred-version (or (gethash loc pred-state) 0)))
                                         (push (cons pred pred-version) incoming)))
                                    (push (make-opt-memory-phi-node :location loc
                                                                    :version version
                                                                    :incoming (nreverse incoming))
                                          nodes)))
                                entry-phis)
                       (setf (gethash block phi-node-table) (nreverse nodes))))
                   (maphash (lambda (loc version)
                              (setf (gethash loc entry-state) version))
                            entry-phis))
                 (let ((out-state (%opt-memory-ssa-copy-state entry-state)))
                   (dolist (inst (bb-instructions block))
                     (let ((loc (%opt-memory-location-key inst alias-roots)))
                       (when (and loc (opt-memory-def-inst-p inst))
                         (setf (gethash loc out-state)
                               (gethash inst def-version)))))
                   (unless (and (%opt-memory-ssa-state-equal
                                 (gethash block phi-aware-in) entry-state)
                                (%opt-memory-ssa-state-equal
                                 (gethash block phi-aware-out) out-state))
                     (setf changed t
                           (gethash block phi-aware-in) entry-state
                           (gethash block phi-aware-out) out-state))))))

      (loop for block across (cfg-blocks cfg)
          when block
          do (let* ((base-state (or (gethash block phi-aware-in)
                                    (make-hash-table :test #'equal)))
                    (state (%opt-memory-ssa-copy-state base-state))
                    (entry-phis (make-hash-table :test #'equal)))
               (maphash (lambda (phi-key version)
                          (when (eq (first phi-key) block)
                            (setf (gethash (second phi-key) entry-phis) version)))
                        phi-version-table)
               (dolist (inst (bb-instructions block))
                 (let* ((loc (%opt-memory-location-key inst alias-roots))
                        (incoming (and loc (gethash loc state)))
                        (entry-phi-version (and loc (gethash loc entry-phis)))
                        (incoming-phi-p (and entry-phi-version
                                             (eql incoming entry-phi-version))))
                   (cond
                     ((and loc (opt-memory-def-inst-p inst))
                      (let ((outgoing (gethash inst def-version)))
                        (setf (gethash inst annotations)
                              (list :kind :def :location loc
                                    :in (or incoming 0)
                                    :out outgoing
                                    :incoming-from (if incoming-phi-p :phi :state)))
                        (setf (gethash loc state) outgoing)))
                      ((and loc (opt-memory-use-inst-p inst))
                      (setf (gethash inst annotations)
                            (list :kind :use :location loc
                                  :in (or incoming 0)
                                  :out (or incoming 0)
                                  :incoming-from (if incoming-phi-p :phi :state)))))))))
    (setf (gethash :memory-phi-nodes annotations) phi-node-table)
    annotations))

(defun opt-memory-ssa-version-at (inst annotations &key (point :in))
  "Return memory version for INST in ANNOTATIONS at POINT (:in or :out)."
  (let ((entry (gethash inst annotations)))
    (when entry
      (ecase point
        (:in (getf entry :in))
        (:out (getf entry :out))))))
