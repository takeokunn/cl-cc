(in-package :cl-cc/optimize)

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

(defun opt-stack-map-live-root-p (stack-map root)
  "Return T when ROOT is live at STACK-MAP's safepoint."
  (not (null (member root (opt-stack-map-roots stack-map) :test #'equal))))

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

(defun opt-sea-node-schedulable-p (node)
  "Return T when NODE has an operator and explicit control dependencies."
  (and (opt-sea-node-op node)
       (listp (opt-sea-node-controls node))))

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

(defun make-opt-shape-descriptor-for-slots (shape-id slots)
  "Create a shape descriptor whose slot offsets follow SLOTS order."
  (make-opt-shape-descriptor
   :shape-id shape-id
   :slots (copy-list slots)
   :offsets (loop for slot in slots for i from 0 collect (cons slot i))))

(defun opt-shape-slot-offset (shape slot)
  "Return SLOT's fixed offset in SHAPE, or NIL when absent."
  (cdr (assoc slot (opt-shape-offsets shape) :test #'equal)))

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
