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

(defstruct (opt-megamorphic-cache (:conc-name opt-mega-cache-))
  "Shared megamorphic dispatch cache used after IC state reaches :megamorphic."
  (entries (make-hash-table :test #'equal))
  (order nil :type list)
  (max-size 64 :type integer))

(defun %opt-mega-cache-touch (cache key)
  (setf (opt-mega-cache-order cache)
        (cons key (remove key (opt-mega-cache-order cache) :test #'equal))))

(defun %opt-mega-cache-evict-if-needed (cache)
  (let ((limit (opt-mega-cache-max-size cache)))
    (when (and (plusp limit)
               (> (length (opt-mega-cache-order cache)) limit))
      (let ((victim (car (last (opt-mega-cache-order cache)))))
        (setf (opt-mega-cache-order cache)
              (butlast (opt-mega-cache-order cache) 1))
        (remhash victim (opt-mega-cache-entries cache))))))

(defun opt-mega-cache-put (cache receiver-key target)
  "Insert RECEIVER-KEY -> TARGET into CACHE with simple LRU eviction."
  (setf (gethash receiver-key (opt-mega-cache-entries cache)) target)
  (%opt-mega-cache-touch cache receiver-key)
  (%opt-mega-cache-evict-if-needed cache)
  target)

(defun opt-mega-cache-get (cache receiver-key)
  "Lookup RECEIVER-KEY in CACHE. Returns (values target found-p)."
  (multiple-value-bind (target found-p)
      (gethash receiver-key (opt-mega-cache-entries cache))
    (when found-p
      (%opt-mega-cache-touch cache receiver-key))
    (values target found-p)))

(defun opt-ic-resolve-target (site receiver-key &optional megamorphic-cache)
  "Resolve dispatch target for RECEIVER-KEY from SITE and optional shared cache.

Lookup order:
  1) site-local IC entries
  2) shared megamorphic cache (only when SITE is :megamorphic)

Returns (values target source-keyword), where source is one of
  :site-local, :megamorphic-shared, or :miss."
  (let ((local (assoc receiver-key (opt-ic-site-entries site) :test #'equal)))
    (when local
      (return-from opt-ic-resolve-target (values (cdr local) :site-local))))
  (when (and megamorphic-cache
             (eq (opt-ic-site-state site) :megamorphic))
    (multiple-value-bind (target found-p)
        (opt-mega-cache-get megamorphic-cache receiver-key)
      (when found-p
        (return-from opt-ic-resolve-target (values target :megamorphic-shared)))))
  (values nil :miss))

(defstruct (opt-ic-patch-plan (:conc-name opt-ic-patch-))
  "Plan for patching one IC call site at runtime.

This helper is backend-agnostic and only models *what* to patch, not machine
encoding details."
  site-id
  old-state
  new-state
  patch-kind
  target)

(defun opt-ic-make-patch-plan (site-id old-state new-state target)
  "Build a conservative IC patch plan from OLD-STATE to NEW-STATE.

PATCH-KIND is one of:
  :install-monomorphic
  :promote-polymorphic
  :promote-megamorphic
  :no-op"
  (make-opt-ic-patch-plan
   :site-id site-id
   :old-state old-state
   :new-state new-state
   :patch-kind (cond
                 ((and (eq old-state :uninitialized)
                       (eq new-state :monomorphic)) :install-monomorphic)
                 ((and (member old-state '(:uninitialized :monomorphic))
                       (eq new-state :polymorphic)) :promote-polymorphic)
                 ((eq new-state :megamorphic) :promote-megamorphic)
                 (t :no-op))
   :target target))

(defun opt-build-inline-polymorphic-dispatch (entries receiver-reg)
  "Build a simple PIC-style dispatch chain from ENTRIES.

ENTRIES is an alist of (shape-key . target). Returns a list of plists with
fields :shape, :receiver, :target representing sequential guards."
  (loop for (shape . target) in entries
        collect (list :shape shape :receiver receiver-reg :target target)))

(defstruct (opt-speculation-log (:conc-name opt-spec-log-))
  "Failure log preventing repeated harmful speculative optimizations."
  (failures (make-hash-table :test #'equal))
  (threshold 1 :type integer))

(defparameter *opt-speculation-log* (make-opt-speculation-log)
  "Process-global speculation failure log used by conservative roadmap helpers.")

(defun %opt-stable-print-string (object)
  (with-standard-io-syntax
    (prin1-to-string object)))

(defun %opt-effective-speculation-log (log)
  (or log *opt-speculation-log*))

(defun opt-clear-speculation-log (&optional (log *opt-speculation-log*))
  "Clear LOG's recorded failures and return LOG."
  (let ((target (%opt-effective-speculation-log log)))
    (clrhash (opt-spec-log-failures target))
    target))

(defun opt-record-speculation-failure (log site-id reason)
  "Record a failed speculation for SITE-ID and REASON."
  (let* ((target (%opt-effective-speculation-log log))
         (key (list site-id reason))
         (count (1+ (gethash key (opt-spec-log-failures target) 0))))
    (setf (gethash key (opt-spec-log-failures target)) count)
    count))

(defun opt-speculation-failed-p (log site-id reason)
  "Return T when SITE-ID/REASON has crossed LOG's failure threshold."
  (let ((target (%opt-effective-speculation-log log)))
    (>= (gethash (list site-id reason) (opt-spec-log-failures target) 0)
        (opt-spec-log-threshold target))))

(defun opt-speculation-allowed-p (site-id reason &optional (log *opt-speculation-log*))
  "Return T when SITE-ID/REASON is still allowed under LOG."
  (not (opt-speculation-failed-p log site-id reason)))

(defun %opt-speculation-log-snapshot (log)
  (let ((entries nil))
    (maphash (lambda (key count)
               (push (cons key count) entries))
             (opt-spec-log-failures log))
    (sort entries
          (lambda (left right)
            (string< (%opt-stable-print-string (car left))
                     (%opt-stable-print-string (car right)))))))

(defun opt-save-speculation-log (pathname &optional (log *opt-speculation-log*))
  "Persist LOG to PATHNAME as a simple S-expression and return PATHNAME."
  (let ((target (%opt-effective-speculation-log log)))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (with-standard-io-syntax
        (prin1 (list :threshold (opt-spec-log-threshold target)
                     :failures (%opt-speculation-log-snapshot target))
               stream)))
    pathname))

(defun opt-load-speculation-log (pathname &optional (log *opt-speculation-log*))
  "Load a persisted speculation log from PATHNAME into LOG and return LOG."
  (let ((target (%opt-effective-speculation-log log)))
    (with-open-file (stream pathname :direction :input)
      (with-standard-io-syntax
        (let* ((payload (read stream nil nil))
               (threshold (or (getf payload :threshold) 1))
               (failures (getf payload :failures)))
          (setf (opt-spec-log-threshold target) threshold)
          (opt-clear-speculation-log target)
          (dolist (entry failures)
            (when (and (consp entry) (integerp (cdr entry)))
              (setf (gethash (car entry) (opt-spec-log-failures target))
                    (cdr entry)))))))
    target))

(defstruct (opt-profile-data (:conc-name opt-profile-))
  "Small profile container for edge, value, call-chain, and allocation data."
  (edges (make-hash-table :test #'equal))
  (values (make-hash-table :test #'equal))
  (call-chains (make-hash-table :test #'equal))
  (alloc-sites (make-hash-table :test #'equal))
  (value-limit 4 :type integer)
  (value-ranges (make-hash-table :test #'equal)))

(defun %opt-profile-incf (table key &optional (delta 1))
  (let ((next (+ delta (gethash key table 0))))
    (setf (gethash key table) next)
    next))

(defun %opt-profile-site-value-entries (profile site-id)
  (let ((entries nil))
    (maphash (lambda (key count)
               (when (equal site-id (first key))
                 (push (cons key count) entries)))
             (opt-profile-values profile))
    entries))

(defun %opt-profile-value-entry-preferred-p (left right)
  (let ((left-count (cdr left))
        (right-count (cdr right)))
    (or (> left-count right-count)
        (and (= left-count right-count)
             (string< (%opt-stable-print-string (second (car left)))
                      (%opt-stable-print-string (second (car right))))))))

(defun %opt-profile-prune-site-values (profile site-id)
  (let ((limit (opt-profile-value-limit profile)))
    (when (plusp limit)
      (let ((entries (%opt-profile-site-value-entries profile site-id)))
        (when (> (length entries) limit)
          (dolist (entry (nthcdr limit
                                 (sort entries #'%opt-profile-value-entry-preferred-p)))
            (remhash (car entry) (opt-profile-values profile))))))))

(defun %opt-profile-record-range (profile site-id value)
  (when (numberp value)
    (let ((current (gethash site-id (opt-profile-value-ranges profile))))
      (if current
          (progn
            (setf (car current) (min (car current) value))
            (setf (cdr current) (max (cdr current) value))
            current)
          (setf (gethash site-id (opt-profile-value-ranges profile))
                (cons value value))))))

(defun opt-profile-record-edge (profile from to &optional (delta 1))
  "Increment the execution count for CFG edge FROM → TO."
  (%opt-profile-incf (opt-profile-edges profile) (cons from to) delta))

(defun opt-profile-record-value (profile site-id value &optional (delta 1))
  "Increment a top-k style value counter for SITE-ID."
  (let ((count (%opt-profile-incf (opt-profile-values profile) (list site-id value) delta)))
    (%opt-profile-record-range profile site-id value)
    (%opt-profile-prune-site-values profile site-id)
    count))

(defun opt-profile-top-values (profile site-id &optional limit)
  "Return SITE-ID's retained top values as sorted (VALUE . COUNT) pairs."
  (let* ((entries (sort (%opt-profile-site-value-entries profile site-id)
                        #'%opt-profile-value-entry-preferred-p))
         (selected (if limit
                       (subseq entries 0 (min limit (length entries)))
                       entries)))
    (mapcar (lambda (entry)
              (cons (second (car entry)) (cdr entry)))
            selected)))

(defun opt-profile-value-range (profile site-id)
  "Return SITE-ID's numeric value range as (MIN . MAX), or NIL when absent."
  (let ((range (gethash site-id (opt-profile-value-ranges profile))))
    (and range (cons (car range) (cdr range)))))

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

(defun %opt-pgo-edge-count (edge-counts from to)
  (let ((key (cons from to)))
    (cond
      ((typep edge-counts 'opt-profile-data)
       (gethash key (opt-profile-edges edge-counts) 0))
      ((hash-table-p edge-counts)
       (gethash key edge-counts 0))
      (t (or (cdr (assoc key edge-counts :test #'equal)) 0)))))

(defun opt-pgo-best-successor (block successors edge-counts &optional visited)
  "Return BLOCK's hottest unvisited successor according to EDGE-COUNTS."
  (let ((candidates (remove-if (lambda (successor)
                                 (and visited (gethash successor visited)))
                               successors)))
    (car (sort (copy-list candidates)
               (lambda (left right)
                 (let ((left-count (%opt-pgo-edge-count edge-counts block left))
                       (right-count (%opt-pgo-edge-count edge-counts block right)))
                   (or (> left-count right-count)
                       (and (= left-count right-count)
                            (string< (%opt-stable-print-string left)
                                     (%opt-stable-print-string right))))))))))

(defun opt-pgo-build-hot-chain (entry successors-alist edge-counts)
  "Build a greedy Pettis-Hansen-style hot block chain from ENTRY."
  (let ((visited (make-hash-table :test #'equal))
        (chain nil)
        (current entry))
    (loop while current
          do (push current chain)
             (setf (gethash current visited) t)
             (let* ((successors (copy-list (cdr (assoc current successors-alist :test #'equal))))
                    (next (opt-pgo-best-successor current successors edge-counts visited)))
               (setf current next)))
    (nreverse chain)))

(defun opt-pgo-rotate-loop (loop-chain preferred-exit)
  "Rotate LOOP-CHAIN so PREFERRED-EXIT becomes the loop bottom."
  (let ((index (position preferred-exit loop-chain :test #'equal)))
    (if index
        (append (subseq loop-chain (1+ index))
                (subseq loop-chain 0 (1+ index)))
        (copy-list loop-chain))))

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
  (inst-count 0 :type integer)
  (exported-p nil :type boolean)
  (importable-p t :type boolean)
  (pure-p nil :type boolean)
  (effects nil :type list)
  (constants nil :type list)
  (callees nil :type list)
  (return-lattice (opt-lattice-bottom)))

(defun opt-function-summary-safe-to-inline-p (summary &key (max-effects 0))
  "Return T when SUMMARY is pure enough for conservative inlining/IPSCCP use."
  (and (opt-function-summary-pure-p summary)
       (<= (length (opt-function-summary-effects summary)) max-effects)))

(defun opt-thinlto-should-import-p (summary caller-callees &key (budget 50))
  "Return T when SUMMARY is a conservative ThinLTO import candidate."
  (and (opt-function-summary-importable-p summary)
       (not (opt-function-summary-exported-p summary))
       (<= (opt-function-summary-inst-count summary) budget)
       (not (member (opt-function-summary-name summary)
                    caller-callees
                    :test #'equal))))

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

(defstruct (opt-cow-object (:conc-name opt-cow-object-))
  "Copy-on-write wrapper for planner/runtime-independent optimization metadata.

PAYLOAD is treated as immutable by readers; writers must call OPT-COW-WRITE to
ensure uniqueness before mutation.  REFCOUNT models shared aliases."
  payload
  (refcount 1 :type integer))

(defun opt-cow-copy (cow)
  "Return a logical copy of COW by incrementing shared refcount.

This operation is O(1): no payload duplication occurs here."
  (incf (opt-cow-object-refcount cow))
  cow)

(defun opt-cow-write (cow write-fn)
  "Apply WRITE-FN under copy-on-write semantics and return the resulting object.

When COW is uniquely owned (REFCOUNT <= 1), WRITE-FN mutates its payload in
place.  When shared, this function decrements the old object's REFCOUNT and
materializes a detached copy using COPY-TREE before applying WRITE-FN."
  (if (<= (opt-cow-object-refcount cow) 1)
      (progn
        (funcall write-fn (opt-cow-object-payload cow))
        cow)
      (let* ((new-payload (copy-tree (opt-cow-object-payload cow)))
             (fresh (make-opt-cow-object :payload new-payload :refcount 1)))
        (decf (opt-cow-object-refcount cow))
        (funcall write-fn (opt-cow-object-payload fresh))
        fresh)))

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

(defstruct (opt-osr-point (:conc-name opt-osr-point-))
  "On-Stack Replacement metadata at loop back-edge safe points."
  loop-id
  (vm-pc 0 :type integer)
  (live-registers nil :type list)
  (hotness 0 :type integer))

(defun opt-materialize-deopt-state (frame machine-registers)
  "Return a VM register alist reconstructed from MACHINE-REGISTERS using FRAME."
  (loop for (machine-reg . vm-reg) in (opt-deopt-frame-register-map frame)
        collect (cons vm-reg (cdr (assoc machine-reg machine-registers :test #'equal)))))

(defun opt-osr-trigger-p (osr-point &key (threshold 1000))
  "Return T when OSR-POINT hotness reaches THRESHOLD."
  (>= (opt-osr-point-hotness osr-point) threshold))

(defun opt-osr-materialize-entry (osr-point machine-registers)
  "Materialize live VM register state for OSR entry from MACHINE-REGISTERS.

Each entry in OPT-OSR-POINT-LIVE-REGISTERS is (machine-reg . vm-reg)."
  (loop for (machine-reg . vm-reg) in (opt-osr-point-live-registers osr-point)
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

(defstruct (opt-shape-transition-cache (:conc-name opt-shape-trans-))
  "Forward-only shape transition cache (parent-shape, slot) -> child-shape."
  (table (make-hash-table :test #'equal))
  (order nil :type list)
  (max-size 256 :type integer))

(defun %opt-shape-trans-key (parent-shape-id slot)
  (list parent-shape-id slot))

(defun opt-shape-transition-put (cache parent-shape-id slot child-shape-id)
  "Register one forward shape transition and return CHILD-SHAPE-ID."
  (let ((key (%opt-shape-trans-key parent-shape-id slot)))
    (setf (gethash key (opt-shape-trans-table cache)) child-shape-id
          (opt-shape-trans-order cache)
          (cons key (remove key (opt-shape-trans-order cache) :test #'equal)))
    (when (> (length (opt-shape-trans-order cache))
             (opt-shape-trans-max-size cache))
      (let ((victim (car (last (opt-shape-trans-order cache)))))
        (setf (opt-shape-trans-order cache)
              (butlast (opt-shape-trans-order cache) 1))
        (remhash victim (opt-shape-trans-table cache))))
    child-shape-id))

(defun opt-shape-transition-get (cache parent-shape-id slot)
  "Lookup forward shape transition. Returns (values child-shape-id found-p)."
  (gethash (%opt-shape-trans-key parent-shape-id slot)
           (opt-shape-trans-table cache)))

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

(defun opt-tier-transition (current-tier hotness
                            &key (baseline-threshold 100)
                                 (optimized-threshold 1000))
  "Return the next tier for CURRENT-TIER and HOTNESS, or CURRENT-TIER."
  (ecase current-tier
    (:interpreter
     (if (>= hotness baseline-threshold) :baseline :interpreter))
    (:baseline
     (if (>= hotness optimized-threshold) :optimized :baseline))
    (:optimized :optimized)))

(defstruct (opt-async-state-machine (:conc-name opt-async-sm-))
  "Minimal async/await lowering plan as an explicit state machine graph."
  entry-state
  (states nil :type list)
  (await-points nil :type list)
  (transitions nil :type list))

(defun opt-build-async-state-machine (await-labels)
  "Build a linear async state machine skeleton from AWAIT-LABELS.

Each await label creates one state and a transition to the next state.
Returns an OPT-ASYNC-STATE-MACHINE descriptor for planning/testing."
  (let* ((states (loop for i from 0 below (1+ (length await-labels)) collect i))
         (transitions
           (loop for i from 0 below (length await-labels)
                 for label in await-labels
                 collect (list :from i :await label :to (1+ i)))))
    (make-opt-async-state-machine
     :entry-state 0
     :states states
     :await-points (copy-list await-labels)
     :transitions transitions)))

(defun opt-choose-coroutine-lowering-strategy (&key supports-call/cc deep-yield-p)
  "Choose coroutine lowering strategy.

Returns :stackful when deep yield or call/cc compatibility is required,
otherwise returns :stackless for state-machine lowering."
  (if (or supports-call/cc deep-yield-p)
      :stackful
      :stackless))

(defstruct (opt-channel-site (:conc-name opt-chan-site-))
  "Channel/CSP optimization metadata for one send/recv site."
  (buffer-size 0 :type integer)
  (queue-depth 0 :type integer)
  (contention 0 :type integer)
  (select-arity 1 :type integer))

(defun opt-channel-select-path (site)
  "Select channel fast/sync path based on SITE characteristics.

Returns one of:
  :fast-buffered      buffered channel with available queue slots/items
  :synchronous-rendezvous  unbuffered channel hand-off path
  :contended-fallback heavy contention -> conservative fallback"
  (cond
    ((> (opt-chan-site-contention site) 3) :contended-fallback)
    ((and (> (opt-chan-site-buffer-size site) 0)
          (< (opt-chan-site-queue-depth site) (opt-chan-site-buffer-size site)))
     :fast-buffered)
    (t :synchronous-rendezvous)))

(defun opt-channel-should-jump-table-select-p (site &key (threshold 4))
  "Return T when select-arity is large enough to prefer jump-table lowering."
  (>= (opt-chan-site-select-arity site) threshold))

(defstruct (opt-stm-plan (:conc-name opt-stm-plan-))
  "STM lowering plan for one `(atomically ...)` region."
  (reads nil :type list)
  (writes nil :type list)
  (pure-p nil :type boolean)
  (inline-log-p t :type boolean))

(defun opt-stm-build-plan (&key reads writes pure-p)
  "Build a conservative STM plan from read/write sets and purity.

When PURE-P is true, transaction logging can be skipped."
  (make-opt-stm-plan
   :reads (copy-list reads)
   :writes (copy-list writes)
   :pure-p pure-p
   :inline-log-p (not pure-p)))

(defun opt-stm-needs-log-p (plan)
  "Return T when PLAN requires transactional logging."
  (and (not (opt-stm-plan-pure-p plan))
       (or (opt-stm-plan-reads plan)
           (opt-stm-plan-writes plan))))

(defstruct (opt-lockfree-plan (:conc-name opt-lockfree-plan-))
  "Lock-free lowering support plan for CAS-based data structures."
  (operation :cas :type keyword)
  (aba-risk-p nil :type boolean)
  (reclamation :none :type keyword))

(defun opt-lockfree-select-reclamation (&key aba-risk-p contention)
  "Choose memory reclamation strategy for lock-free lowering.

Rules:
  - no ABA risk -> :none
  - high contention (>= 4) -> :epoch
  - otherwise -> :hazard-pointer"
  (cond
    ((not aba-risk-p) :none)
    ((and contention (>= contention 4)) :epoch)
    (t :hazard-pointer)))

(defun opt-lockfree-build-plan (&key operation aba-risk-p contention)
  "Build a lock-free support plan with reclamation strategy."
  (make-opt-lockfree-plan
   :operation (or operation :cas)
   :aba-risk-p aba-risk-p
   :reclamation (opt-lockfree-select-reclamation
                 :aba-risk-p aba-risk-p
                 :contention contention)))

(defstruct (opt-cfi-plan (:conc-name opt-cfi-plan-))
  "Control-Flow Integrity planning record."
  (insert-endbr64-p nil :type boolean)
  (insert-bti-p nil :type boolean)
  (protect-indirect-calls-p t :type boolean))

(defun opt-build-cfi-plan (&key target has-indirect-calls-p)
  "Build a conservative CFI insertion plan for TARGET (:x86-64 or :aarch64)."
  (make-opt-cfi-plan
   :insert-endbr64-p (and has-indirect-calls-p (eq target :x86-64))
   :insert-bti-p (and has-indirect-calls-p (eq target :aarch64))
   :protect-indirect-calls-p has-indirect-calls-p))

(defun opt-cfi-entry-opcode (plan)
  "Return the function-entry CFI marker opcode selected by PLAN."
  (cond
    ((opt-cfi-plan-insert-endbr64-p plan) :endbr64)
    ((opt-cfi-plan-insert-bti-p plan) :bti-c)
    (t :none)))

(defun opt-should-use-retpoline-p (&key target has-indirect-branch-p supports-ibrs-p)
  "Return T when retpoline mitigation should be enabled.

Retpoline is x86-64 specific and unnecessary when IBRS/eIBRS is available."
  (and (eq target :x86-64)
       has-indirect-branch-p
       (not supports-ibrs-p)))

(defun opt-retpoline-thunk-name (target-reg)
  "Return the module-local retpoline thunk name for TARGET-REG."
  (format nil "__clcc_retpoline_~(~a~)" target-reg))

(defun opt-needs-stack-canary-p (&key has-stack-buffer-p)
  "Return T when stack protector instrumentation should be inserted."
  (not (null has-stack-buffer-p)))

(defun opt-stack-canary-emit-plan (&key has-stack-buffer-p guard-slot failure-target)
  "Return a backend-neutral stack canary prologue/epilogue emission plan."
  (let ((enabled-p (opt-needs-stack-canary-p :has-stack-buffer-p has-stack-buffer-p)))
    (list :enabled-p enabled-p
          :guard-slot (and enabled-p (or guard-slot :stack-canary))
          :load-source (and enabled-p :tls-canary)
          :failure-target (and enabled-p (or failure-target '__stack_chk_fail)))))

(defun opt-stack-canary-prologue-seq (plan &key (temp-reg :stack-canary-temp))
  "Return abstract prologue operations for PLAN's stack canary setup."
  (when (getf plan :enabled-p)
    `((:op :load-canary
       :source ,(getf plan :load-source)
       :dst ,temp-reg)
      (:op :store-canary
       :src ,temp-reg
       :slot ,(getf plan :guard-slot)))))

(defun opt-stack-canary-epilogue-seq (plan &key (temp-reg :stack-canary-temp))
  "Return abstract epilogue operations for PLAN's stack canary verification."
  (when (getf plan :enabled-p)
    `((:op :load-canary
       :source ,(getf plan :guard-slot)
       :dst ,temp-reg)
      (:op :compare-canary
       :left ,temp-reg
       :right ,(getf plan :load-source))
      (:op :branch-if-canary-mismatch
       :target ,(getf plan :failure-target)))))

(defstruct (opt-shadow-stack-plan (:conc-name opt-shadow-stack-plan-))
  "Shadow Stack (CET SS) planning record for return-address verification."
  (enabled-p nil :type boolean)
  target
  (needs-incsssp-p nil :type boolean)
  (needs-save-restore-p nil :type boolean))

(defun opt-build-shadow-stack-plan (&key target supports-cet-ss-p
                                      has-nonlocal-control-p
                                      has-setjmp-longjmp-p)
  "Build a conservative Shadow Stack plan for TARGET.

CET Shadow Stack is x86-64 specific. Non-local control transfers require
explicit save/restore planning before backend instruction emission is safe."
  (let* ((enabled-p (and (eq target :x86-64) supports-cet-ss-p))
         (save-restore-p (and enabled-p
                              (or has-nonlocal-control-p
                                  has-setjmp-longjmp-p))))
    (make-opt-shadow-stack-plan
     :enabled-p enabled-p
     :target (and enabled-p target)
     :needs-incsssp-p enabled-p
     :needs-save-restore-p save-restore-p)))

(defstruct (opt-wasm-tailcall-plan (:conc-name opt-wasm-tail-))
  "Wasm tail-call lowering decision for one call site."
  (tail-position-p nil :type boolean)
  (indirect-p nil :type boolean)
  (enabled-p t :type boolean)
  (opcode :call :type keyword))

(defun opt-wasm-select-tailcall-opcode (&key tail-position-p indirect-p enabled-p)
  "Select wasm call opcode with tail-call proposal support.

Returns one of :call, :call-indirect, :return-call, :return-call-indirect."
  (if (and enabled-p tail-position-p)
      (if indirect-p :return-call-indirect :return-call)
      (if indirect-p :call-indirect :call)))

(defun opt-build-wasm-tailcall-plan (&key tail-position-p indirect-p (enabled-p t))
  "Build tail-call lowering plan and chosen opcode for one wasm call site."
  (make-opt-wasm-tailcall-plan
   :tail-position-p tail-position-p
   :indirect-p indirect-p
   :enabled-p enabled-p
   :opcode (opt-wasm-select-tailcall-opcode
            :tail-position-p tail-position-p
            :indirect-p indirect-p
            :enabled-p enabled-p)))

(defstruct (opt-wasm-gc-layout (:conc-name opt-wasm-gc-))
  "Wasm GC layout descriptor for struct/array-backed CL objects."
  (kind :struct :type keyword)
  (fields nil :type list)
  (nullable-p t :type boolean))

(defun opt-build-wasm-gc-layout (&key kind fields nullable-p)
  "Build a wasm-gc layout descriptor for planning/testing purposes."
  (make-opt-wasm-gc-layout
   :kind (or kind :struct)
   :fields (copy-list fields)
   :nullable-p (if (null nullable-p) nil t)))

(defstruct (opt-debug-loc (:conc-name opt-debug-loc-))
  "Source-level location record for debug info planning."
  file
  (line 0 :type integer)
  (column 0 :type integer)
  (symbol nil :type t))

(defun opt-build-dwarf-line-row (address debug-loc)
  "Build a minimal DWARF-like line row plist from ADDRESS and DEBUG-LOC."
  (list :address address
        :file (opt-debug-loc-file debug-loc)
        :line (opt-debug-loc-line debug-loc)
        :column (opt-debug-loc-column debug-loc)))

(defun opt-build-wasm-source-map-entry (offset debug-loc)
  "Build a source-map entry plist for wasm OFFSET and DEBUG-LOC."
  (list :offset offset
        :source (opt-debug-loc-file debug-loc)
        :line (opt-debug-loc-line debug-loc)
        :column (opt-debug-loc-column debug-loc)))

(defun opt-format-diagnostic-reason (pass outcome reason)
  "Format optimization diagnostic reason in Rpass-like style."
  (format nil "~a: ~a (~a)" pass outcome reason))

(defstruct (opt-tls-plan (:conc-name opt-tls-plan-))
  "Thread-local access lowering plan."
  target
  (uses-inline-tls-p nil :type boolean)
  (base-register nil :type (or null keyword)))

(defun opt-build-tls-plan (&key target hot-access-p)
  "Build TLS access plan for TARGET architecture.

When HOT-ACCESS-P is true, choose inline segment/thread-pointer based access."
  (let ((base (case target
                (:x86-64 :fs)
                (:aarch64 :tpidr_el0)
                (otherwise nil))))
    (make-opt-tls-plan
     :target target
     :uses-inline-tls-p (and hot-access-p (not (null base)))
     :base-register (and hot-access-p base))))

(defstruct (opt-atomic-plan (:conc-name opt-atomic-plan-))
  "Atomic lowering plan for architecture + memory ordering."
  target
  operation
  memory-order
  opcode)

(defun opt-select-atomic-opcode (&key target operation memory-order)
  "Select a representative atomic opcode for OPERATION and MEMORY-ORDER.

This helper captures lowering intent only; exact instruction encoding remains
backend responsibility."
  (declare (ignore memory-order))
  (case target
    (:x86-64
     (case operation
       (:incf :lock-xadd)
       (:cas :lock-cmpxchg)
       (otherwise :lock-op)))
    (:aarch64
     (case operation
       (:incf :ldadd)
       (:cas :ldxr-stxr)
       (otherwise :atomic-op)))
    (otherwise :atomic-op)))

(defun opt-build-atomic-plan (&key target operation memory-order)
  "Build atomic lowering plan with selected opcode."
  (make-opt-atomic-plan
   :target target
   :operation operation
   :memory-order memory-order
   :opcode (opt-select-atomic-opcode
            :target target
            :operation operation
            :memory-order memory-order)))

(defstruct (opt-htm-plan (:conc-name opt-htm-plan-))
  "Hardware Transactional Memory lock-elision plan."
  target
  (uses-htm-p nil :type boolean)
  (begin-opcode nil :type (or null keyword))
  (end-opcode nil :type (or null keyword))
  (abort-opcode nil :type (or null keyword))
  (fallback-lock-p t :type boolean))

(defun opt-build-htm-plan (&key target supports-htm-p low-contention-p)
  "Build an HTM lock-elision plan.

HTM path is enabled only when hardware support exists and contention is low.
Fallback lock path remains enabled conservatively."
  (let* ((enable-htm-p (and supports-htm-p low-contention-p))
         (begin (case target
                  (:x86-64 :xbegin)
                  (:power10 :tbegin)
                  (:power :tbegin)
                  (otherwise nil)))
         (end (case target
                (:x86-64 :xend)
                ((:power10 :power) :tend)
                (otherwise nil)))
         (abort (case target
                  (:x86-64 :xabort)
                  ((:power10 :power) :tabort)
                  (otherwise nil)))
         (usable (not (null (and enable-htm-p begin end abort)))))
    (make-opt-htm-plan
     :target target
     :uses-htm-p usable
     :begin-opcode (and usable begin)
     :end-opcode (and usable end)
     :abort-opcode (and usable abort)
     :fallback-lock-p t)))

(defstruct (opt-concurrent-gc-plan (:conc-name opt-conc-gc-plan-))
  "Concurrent GC planning record."
  (concurrent-mark-p t :type boolean)
  (write-barrier :satb :type keyword)
  (mutator-assist-p t :type boolean)
  (stw-phases '(:initial-mark :final-remark) :type list))

(defun opt-build-concurrent-gc-plan (&key latency-sensitive-p heap-size)
  "Build conservative concurrent-GC plan for tri-color marking.

LATENCY-SENSITIVE-P keeps concurrent marking + SATB barrier.
Small heaps may disable mutator assist to avoid overhead."
  (let* ((small-heap-p (and heap-size (< heap-size (* 32 1024 1024))))
         (concurrent-mark-p (not (null latency-sensitive-p)))
         (barrier (if concurrent-mark-p :satb :incremental-update))
         (mutator-assist-p (and concurrent-mark-p (not small-heap-p))))
    (make-opt-concurrent-gc-plan
     :concurrent-mark-p concurrent-mark-p
     :write-barrier barrier
     :mutator-assist-p mutator-assist-p
     :stw-phases (if concurrent-mark-p
                      '(:initial-mark :final-remark)
                      '(:full-mark-sweep)))))

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(defstruct (opt-partial-specialization (:conc-name opt-partial-spec-))
  "Residual helper result for conservative constant-argument specialization."
  original-name
  specialized-name
  (signature nil :type list)
  (static-args nil :type list)
  (dynamic-args nil :type list)
  (residual-body nil :type list))

(defstruct (opt-binding-time (:conc-name opt-binding-time-))
  "Binding-time classification for one parameter under the SCCP lattice."
  parameter
  (kind :dynamic :type keyword)
  value
  lattice)

(defstruct (opt-specialization-plan (:conc-name opt-specialization-plan-))
  "Known-callee specialization plan keyed by a constant-argument signature."
  callee-label
  specialized-name
  (signature nil :type list)
  (static-args nil :type list)
  (dynamic-args nil :type list)
  (clone-needed-p nil :type boolean)
  (cache-hit-p nil :type boolean))

(defun %opt-normalize-binding-cell-value (cell)
  (let ((tail (cdr cell)))
    (if (and (consp tail) (null (cdr tail)))
        (car tail)
        tail)))

(defun %opt-binding-value (key bindings)
  (cond
    ((hash-table-p bindings)
     (gethash key bindings))
    (bindings
     (let ((cell (assoc key bindings :test #'equal)))
       (if cell
           (values (%opt-normalize-binding-cell-value cell) t)
           (values nil nil))))
    (t (values nil nil))))

(defun %opt-parameter-constant (parameter index constant-bindings)
  (multiple-value-bind (value present-p)
      (%opt-binding-value parameter constant-bindings)
    (if present-p
        (values value t)
        (%opt-binding-value index constant-bindings))))

(defun %opt-constant-binding-signature (parameters constant-bindings)
  (loop for parameter in parameters
        for index from 0
        append (multiple-value-bind (value present-p)
                   (%opt-parameter-constant parameter index constant-bindings)
                 (when present-p
                   (list (cons parameter value))))))

(defun %opt-remove-shadowed-bindings (signature shadowed)
  (remove-if (lambda (cell)
               (member (car cell) shadowed :test #'equal))
             signature))

(defun %opt-lambda-binding-symbol (item)
  (cond
    ((and (symbolp item)
          (not (member item '(&optional &rest &key &aux &body &whole
                              &environment &allow-other-keys)
                       :test #'eq)))
     item)
    ((and (consp item) (symbolp (car item)))
     (car item))
    (t nil)))

(defun %opt-let-binding-symbol (binding)
  (cond
    ((symbolp binding) binding)
    ((and (consp binding) (symbolp (car binding))) (car binding))
    (t nil)))

(defun %opt-substitute-let-bindings (bindings signature sequential-p substitute-fn)
  (let ((current-signature signature)
        (shadowed nil)
        (result nil))
    (dolist (binding bindings)
      (let ((var (%opt-let-binding-symbol binding)))
        (push (if (and (consp binding) (cdr binding))
                  (list var (funcall substitute-fn (second binding) current-signature))
                  binding)
              result)
        (when var
          (push var shadowed)
          (when sequential-p
            (setf current-signature
                  (%opt-remove-shadowed-bindings current-signature (list var)))))))
    (values (nreverse result)
            (if sequential-p
                current-signature
                (%opt-remove-shadowed-bindings signature shadowed)))))

(defun %opt-tree-substitute-constants (form signature)
  (labels ((substitute-body (forms active-signature)
             (let ((current-signature active-signature)
                   (result nil))
               (dolist (body-form forms)
                 (multiple-value-bind (new-form new-signature)
                     (if (and (consp body-form) (eq (car body-form) 'setq))
                         (substitute-setq (cdr body-form) current-signature)
                         (values (substitute-node body-form current-signature)
                                 current-signature))
                   (push new-form result)
                   (setf current-signature new-signature)))
               (values (nreverse result) current-signature)))
           (substitute-setq (pairs active-signature)
             (let ((current-signature active-signature)
                   (result nil))
               (loop for (place value) on pairs by #'cddr
                     do (push place result)
                        (push (substitute-node value current-signature) result)
                        (when (symbolp place)
                          (setf current-signature
                                (%opt-remove-shadowed-bindings current-signature
                                                               (list place)))))
               (values (cons 'setq (nreverse result)) current-signature)))
           (substitute-node (node active-signature)
             (cond
               ((symbolp node)
                (let ((cell (assoc node active-signature :test #'equal)))
                  (if cell (cdr cell) node)))
               ((atom node) node)
               ((eq (car node) 'quote) node)
               ((eq (car node) 'function) node)
               ((eq (car node) 'lambda)
                (let* ((lambda-list (second node))
                       (shadowed (remove nil
                                         (mapcar #'%opt-lambda-binding-symbol
                                                 lambda-list)))
                        (body-signature
                          (%opt-remove-shadowed-bindings active-signature shadowed)))
                  (list* 'lambda
                         lambda-list
                         (substitute-body (cddr node) body-signature))))
               ((eq (car node) 'progn)
                (cons 'progn (substitute-body (cdr node) active-signature)))
               ((member (car node) '(let let*) :test #'eq)
                (multiple-value-bind (bindings body-signature)
                    (%opt-substitute-let-bindings
                     (second node)
                     active-signature
                     (eq (car node) 'let*)
                      #'substitute-node)
                  (list* (car node)
                         bindings
                         (substitute-body (cddr node) body-signature))))
               ((eq (car node) 'setq)
                (substitute-setq (cdr node) active-signature))
               ((symbolp (car node))
                (cons (car node)
                      (substitute-body (cdr node) active-signature)))
               (t
                (substitute-body node active-signature)))))
    (substitute-node form signature)))

(defun %opt-dynamic-parameters (parameters signature)
  (remove-if (lambda (parameter)
               (assoc parameter signature :test #'equal))
             parameters))

(defun %opt-specialized-name (function-name signature)
  (format nil "~A__spec__~A"
          (%opt-stable-print-string function-name)
          (%opt-stable-print-string signature)))

(defun opt-specialize-constant-args (function-name parameters body constant-bindings
                                     &key specialized-name)
  "Build a residual helper copy of BODY with constant PARAMETERS substituted.

This is a helper-level partial-evaluation primitive: it does not execute code or
install a clone in the function registry. It records the static signature and
returns a residual body that later passes can fold safely."
  (let* ((signature (%opt-constant-binding-signature parameters constant-bindings))
         (name (or specialized-name
                   (%opt-specialized-name function-name signature))))
    (make-opt-partial-specialization
     :original-name function-name
     :specialized-name name
     :signature signature
     :static-args signature
     :dynamic-args (%opt-dynamic-parameters parameters signature)
     :residual-body (cdr (%opt-tree-substitute-constants
                          (cons 'progn body)
                          signature)))))

(defun %opt-lattice-binding-time-kind (lattice)
  (if (and (opt-lattice-value-p lattice)
           (eq (opt-lattice-value-kind lattice) :constant))
      :static
      :dynamic))

(defun opt-sccp-analyze-binding-times (parameters lattice-bindings)
  "Classify PARAMETERS as :STATIC or :DYNAMIC using SCCP lattice bindings."
  (loop for parameter in parameters
        for index from 0
        collect (multiple-value-bind (lattice present-p)
                    (%opt-parameter-constant parameter index lattice-bindings)
                  (let ((kind (if present-p
                                  (%opt-lattice-binding-time-kind lattice)
                                  :dynamic)))
                    (make-opt-binding-time
                     :parameter parameter
                     :kind kind
                     :value (and (eq kind :static)
                                 (opt-lattice-value-value lattice))
                     :lattice (and present-p lattice))))))

(defun opt-build-specialization-plan (callee-label arguments constant-bindings
                                      &key cache)
  "Build a conservative clone/call-redirection plan for known constant arguments.

Returns NIL when ARGUMENTS have no known constants. When CACHE is supplied, the
same `(callee . signature)` pair reuses the earlier specialized name and marks
the plan as a cache hit instead of requesting a new clone."
  (let ((signature (%opt-constant-binding-signature arguments constant-bindings)))
    (when signature
      (let* ((cache-key (list callee-label signature))
             (cached-name nil)
             (cache-hit-p nil))
        (when cache
          (multiple-value-setq (cached-name cache-hit-p)
            (gethash cache-key cache)))
        (let ((specialized-name (or cached-name
                                    (%opt-specialized-name callee-label signature))))
          (when (and cache (not cache-hit-p))
            (setf (gethash cache-key cache) specialized-name))
          (make-opt-specialization-plan
           :callee-label callee-label
           :specialized-name specialized-name
           :signature signature
           :static-args signature
           :dynamic-args (%opt-dynamic-parameters arguments signature)
           :clone-needed-p (not cache-hit-p)
           :cache-hit-p cache-hit-p))))))
