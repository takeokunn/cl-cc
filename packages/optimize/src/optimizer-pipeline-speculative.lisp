;;;; optimizer-pipeline-speculative.lisp — IC/speculation/profiling/lattice helpers
(in-package :cl-cc/optimize)

(defun opt-ic-transition (site receiver-key target)
  "Record RECEIVER-KEY → TARGET in SITE and return SITE.
State transitions follow uninitialized → monomorphic → polymorphic → megamorphic."
  (incf (opt-ic-site-misses site))
  (let ((existing (assoc receiver-key (opt-ic-site-entries site) :test #'equal)))
    (if existing
        (setf (cdr existing) target)
        (push (cons receiver-key target) (opt-ic-site-entries site))))
  (let ((n (length (opt-ic-site-entries site))))
    (setf (opt-ic-site-state site)
          (cond ((= n 0) :uninitialized)
                ((= n 1) :monomorphic)
                ((<= n (opt-ic-site-max-polymorphic-entries site)) :polymorphic)
                (t :megamorphic)))
    (when (eq (opt-ic-site-state site) :megamorphic)
      (setf (opt-ic-site-megamorphic-fallback site)
            (copy-list (opt-ic-site-entries site)))))
  site)

(defstruct (opt-speculation-log (:conc-name opt-spec-log-))
  "Failure log preventing repeated harmful speculative optimizations."
  (failures (make-hash-table :test #'equal))
  (threshold 1 :type integer))

(defun opt-record-speculation-failure (log site-id reason)
  "Record a failed speculation for SITE-ID and REASON."
  (let* ((key (list site-id reason))
         (count (1+ (gethash key (opt-spec-log-failures log) 0))))
    (setf (gethash key (opt-spec-log-failures log)) count)
    count))

(defun opt-speculation-failed-p (log site-id reason)
  "Return T when SITE-ID/REASON has crossed LOG's failure threshold."
  (>= (gethash (list site-id reason) (opt-spec-log-failures log) 0)
      (opt-spec-log-threshold log)))

(defstruct (opt-profile-data (:conc-name opt-profile-))
  "Small profile container for edge, value, call-chain, and allocation data."
  (edges (make-hash-table :test #'equal))
  (values (make-hash-table :test #'equal))
  (call-chains (make-hash-table :test #'equal))
  (alloc-sites (make-hash-table :test #'equal)))

(defun %opt-profile-incf (table key &optional (delta 1))
  (let ((next (+ delta (gethash key table 0))))
    (setf (gethash key table) next)
    next))

(defun opt-profile-record-edge (profile from to &optional (delta 1))
  "Increment the execution count for CFG edge FROM → TO."
  (%opt-profile-incf (opt-profile-edges profile) (cons from to) delta))

(defun opt-profile-record-value (profile site-id value &optional (delta 1))
  "Increment a top-k style value counter for SITE-ID."
  (%opt-profile-incf (opt-profile-values profile) (list site-id value) delta))

(defun opt-profile-record-call-chain (profile chain &optional (delta 1))
  "Increment a context-sensitive call-chain sample."
  (%opt-profile-incf (opt-profile-call-chains profile) (copy-list chain) delta))

(defun opt-profile-record-allocation (profile site-id bytes &optional (count 1))
  "Record allocation COUNT and BYTES for SITE-ID."
  (let* ((table (opt-profile-alloc-sites profile))
         (current (or (gethash site-id table) (cons 0 0))))
    (incf (car current) count)
    (incf (cdr current) bytes)
    (setf (gethash site-id table) current)
    current))

(defstruct (opt-lattice-value (:conc-name opt-lattice-value-))
  "Three-point SCCP/IPSCCP lattice value."
  (kind :bottom :type keyword)
  value)

(defun opt-lattice-bottom ()
  "Return the unknown-bottom lattice element."
  (make-opt-lattice-value :kind :bottom))

(defun opt-lattice-constant (value)
  "Return a constant lattice element for VALUE."
  (make-opt-lattice-value :kind :constant :value value))

(defun opt-lattice-overdefined ()
  "Return the overdefined lattice element."
  (make-opt-lattice-value :kind :overdefined))

(defun opt-lattice-meet (left right)
  "Conservatively meet LEFT and RIGHT in the SCCP lattice."
  (let ((left-kind (opt-lattice-value-kind left))
        (right-kind (opt-lattice-value-kind right)))
    (cond
      ((eq left-kind :bottom) right)
      ((eq right-kind :bottom) left)
      ((or (eq left-kind :overdefined)
           (eq right-kind :overdefined))
       (opt-lattice-overdefined))
      ((and (eq left-kind :constant)
            (eq right-kind :constant)
            (equal (opt-lattice-value-value left)
                   (opt-lattice-value-value right)))
       left)
      (t (opt-lattice-overdefined)))))

(defstruct (opt-function-summary (:conc-name opt-function-summary-))
  "Small interprocedural summary used by conservative IPO helpers."
  name
  (pure-p nil :type boolean)
  (effects nil :type list)
  (constants nil :type list)
  (callees nil :type list)
  (return-lattice (opt-lattice-bottom)))

(defun opt-function-summary-safe-to-inline-p (summary &key (max-effects 0))
  "Return T when SUMMARY is pure enough for conservative inlining/IPSCCP use."
  (and (opt-function-summary-pure-p summary)
       (<= (length (opt-function-summary-effects summary)) max-effects)))

(defstruct (opt-slab-pool (:conc-name opt-slab-pool-))
  "Fixed-size object pool for cons/slab allocation modelling."
  (object-size 1 :type integer)
  (free-list nil :type list)
  (next-id 0 :type integer)
  (allocated-count 0 :type integer))

(defun opt-slab-allocate (pool)
  "Allocate one fixed-size object id from POOL."
  (if (opt-slab-pool-free-list pool)
      (pop (opt-slab-pool-free-list pool))
      (progn
        (incf (opt-slab-pool-next-id pool))
        (incf (opt-slab-pool-allocated-count pool))
        (list :slab-object (opt-slab-pool-object-size pool)
              (opt-slab-pool-next-id pool)))))

(defun opt-slab-free (pool object)
  "Return OBJECT to POOL's freelist."
  (push object (opt-slab-pool-free-list pool))
  pool)

(defstruct (opt-bump-region (:conc-name opt-bump-region-))
  "Bump-pointer allocation region used by allocation planning helpers."
  (cursor 0 :type integer)
  (limit 0 :type integer)
  (marks nil :type list))

(defun %opt-align-up (value alignment)
  (* alignment (ceiling value alignment)))

(defun opt-bump-allocate (region words &key (alignment 1))
  "Allocate WORDS from REGION and return the start cursor, or NIL on overflow."
  (let* ((start (%opt-align-up (opt-bump-region-cursor region) alignment))
         (end (+ start words)))
    (when (<= end (opt-bump-region-limit region))
      (setf (opt-bump-region-cursor region) end)
      start)))

(defun opt-bump-mark (region)
  "Record REGION's current cursor and return it."
  (push (opt-bump-region-cursor region) (opt-bump-region-marks region))
  (opt-bump-region-cursor region))

(defun opt-bump-reset (region &optional mark)
  "Reset REGION to MARK, or to the most recent saved mark."
  (let ((target (or mark (pop (opt-bump-region-marks region)) 0)))
    (setf (opt-bump-region-cursor region) target)))

(defstruct (opt-stack-map (:conc-name opt-stack-map-))
  "Safepoint stack-map metadata: VM PC and live roots."
  (pc 0 :type integer)
  (roots nil :type list))

(defun opt-stack-map-live-root-p (stack-map root)
  "Return T when ROOT is live at STACK-MAP's safepoint."
  (not (null (member root (opt-stack-map-roots stack-map) :test #'equal))))

(defstruct (opt-guard-state (:conc-name opt-guard-state-))
  "Speculative guard state for guard weakening and deopt accounting."
  (kind :type-check :type keyword)
  (strength :full-type-check :type keyword)
  (executions 0 :type integer)
  (failures 0 :type integer))

(defun opt-weaken-guard (guard &key (execution-threshold 10))
  "Weaken GUARD when it has enough successful executions and no failures."
  (when (and (zerop (opt-guard-state-failures guard))
             (>= (opt-guard-state-executions guard) execution-threshold))
    (setf (opt-guard-state-strength guard)
          (case (opt-guard-state-strength guard)
            (:full-type-check :tag-bit-test)
            (:tag-bit-test :shape-id-compare)
            (:shape-id-compare :nop)
            (otherwise (opt-guard-state-strength guard)))))
  (opt-guard-state-strength guard))

(defun opt-guard-record (guard success-p)
  "Record one GUARD execution and return its current strength."
  (incf (opt-guard-state-executions guard))
  (unless success-p
    (incf (opt-guard-state-failures guard))
    (setf (opt-guard-state-strength guard) :full-type-check))
  (opt-weaken-guard guard))

(defstruct (opt-jit-cache-entry (:conc-name opt-jit-cache-entry-))
  "One JIT code-cache entry for conservative eviction decisions."
  id
  (size 0 :type integer)
  (warmth 0 :type integer)
  (active-p t :type boolean))

(defun opt-jit-cache-select-eviction (entries &key (pressure-threshold 0.8) current-size max-size)
  "Select the coldest active entry when cache pressure exceeds PRESSURE-THRESHOLD."
  (when (and max-size
             (plusp max-size)
             current-size
             (> (/ (float current-size) (float max-size)) pressure-threshold))
    (loop for entry in entries
          when (opt-jit-cache-entry-active-p entry)
            minimize (opt-jit-cache-entry-warmth entry) into min-warmth
            and collect entry into active
          finally (return (find min-warmth active
                                :key #'opt-jit-cache-entry-warmth
                                :test #'=)))))

(defstruct (opt-module-summary (:conc-name opt-module-summary-))
  "ThinLTO-style module summary for parallel whole-program planning."
  module
  (exports nil :type list)
  (function-count 0 :type integer)
  (type-summaries nil :type list))

(defun opt-merge-module-summaries (summaries)
  "Merge SUMMARIES into a small global summary plist."
  (list :modules (mapcar #'opt-module-summary-module summaries)
        :exports (remove-duplicates
                  (mapcan (lambda (summary)
                            (copy-list (opt-module-summary-exports summary)))
                          summaries)
                  :test #'equal)
        :function-count (reduce #'+ summaries
                                :key #'opt-module-summary-function-count
                                :initial-value 0)))

(defstruct (opt-sea-node (:conc-name opt-sea-node-))
  "Schedule-free Sea-of-Nodes placeholder used by MIR/SSA bridge planning."
  id
  op
  (inputs nil :type list)
  (controls nil :type list))

(defun opt-sea-node-schedulable-p (node)
  "Return T when NODE has an operator and explicit control dependencies."
  (and (opt-sea-node-op node)
       (listp (opt-sea-node-controls node))))

(defstruct (opt-deopt-frame (:conc-name opt-deopt-frame-))
  "VM materialization metadata for a deoptimization point."
  (vm-pc 0 :type integer)
  (register-map nil :type list)
  (inlined-frames nil :type list))

(defun opt-materialize-deopt-state (frame machine-registers)
  "Return a VM register alist reconstructed from MACHINE-REGISTERS using FRAME."
  (loop for (machine-reg . vm-reg) in (opt-deopt-frame-register-map frame)
        collect (cons vm-reg (cdr (assoc machine-reg machine-registers :test #'equal)))))

(defstruct (opt-shape-descriptor (:conc-name opt-shape-))
  "Hidden-class style slot layout descriptor."
  (shape-id 0 :type integer)
  (slots nil :type list)
  (offsets nil :type list))

(defun make-opt-shape-descriptor-for-slots (shape-id slots)
  "Create a shape descriptor whose slot offsets follow SLOTS order."
  (make-opt-shape-descriptor
   :shape-id shape-id
   :slots (copy-list slots)
   :offsets (loop for slot in slots for i from 0 collect (cons slot i))))

(defun opt-shape-slot-offset (shape slot)
  "Return SLOT's fixed offset in SHAPE, or NIL when absent."
  (cdr (assoc slot (opt-shape-offsets shape) :test #'equal)))

(defun opt-adaptive-compilation-threshold (&key (base 1000) warmup-p cache-pressure failures)
  "Return an adaptive tiering/recompilation threshold.
Warmup lowers the threshold, cache pressure raises it, and failures suppress
speculative recompilation."
  (let ((threshold base))
    (when warmup-p
      (setf threshold (max 1 (floor threshold 3))))
    (when (and cache-pressure (> cache-pressure 0.6))
      (setf threshold (* threshold 2)))
    (when (and failures (plusp failures))
      (setf threshold (* threshold (1+ failures))))
    threshold))
