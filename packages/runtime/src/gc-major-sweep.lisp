(in-package :cl-cc/runtime)

(defun %gc-sweep-old-space (heap)
  "Sweep the old generation: reclaim all unmarked (dead) objects by adding them
   to the free-list, and clear the mark bit on all live (marked) objects."
  (let ((addr     (rt-heap-old-base heap))
        (old-free (rt-heap-old-free heap))
        (freed    0))
    (%rt-free-list-rebuild-bins heap nil)
    (loop while (< addr old-free) do
      (let ((h (rt-heap-object-header heap addr)))
        (cond
          ((header-forwarding-p h)
           ;; Forwarding pointer should not appear in old space — skip 1 word
           (incf addr 1))
          ((not (integerp h))
           ;; Non-integer, non-cons header — stop
           (return))
          ((zerop (header-size h))
           ;; Zero size header (uninitialized region) — stop
           (return))
          ((header-marked-p h)
           ;; Live object: clear mark bit and advance
           (rt-heap-set-header heap addr (header-clear-mark h))
           (incf addr (header-size h)))
          (t
             ;; Dead object: reclaim to segregated free-list
             (let ((size (header-size h)))
               (dolist (hook *rt-gc-death-hooks*)
                 (funcall hook heap addr size))
               (rt-free-list-insert heap size addr)
              (incf freed size)
              (incf addr size))))))
    ;; Coalesce adjacent free blocks (FR-398)
    (%rt-free-list-rebuild-bins
     heap (%gc-coalesce-free-list (rt-heap-free-list-blocks heap)))
    (when *gc-profile-guided-placement*
      (%rt-free-list-rebuild-bins
       heap (%rt-gc-profile-guide-free-list heap (rt-heap-free-list-blocks heap))))
    (setf (rt-heap-lazy-sweep-cursor heap) old-free
          (rt-heap-lazy-sweep-limit heap) old-free)
    (incf (rt-heap-words-collected heap) freed)))

;;; FR-340: Concurrent Sweeping — sweeps old space on-demand during allocation; lazy sweep with page-level granularity
(defun rt-gc-lazy-sweep-step (heap region-start &key (page-words +gc-card-size-words+))
  "Sweep one old-space page starting at REGION-START and enqueue dead blocks."
  (check-type heap rt-heap)
  (let* ((old-free (rt-heap-old-free heap))
         (limit (min old-free (+ region-start page-words)))
         (addr region-start)
         (freed 0))
    (loop while (< addr limit) do
      (let ((h (rt-heap-object-header heap addr)))
        (cond
          ((header-forwarding-p h) (incf addr 1))
          ((or (not (integerp h)) (zerop (header-size h)))
           (setf addr limit))
          ((header-marked-p h)
           (rt-heap-set-header heap addr (header-clear-mark h))
           (incf addr (header-size h)))
          (t
           (let ((size (header-size h)))
             (dolist (hook *rt-gc-death-hooks*)
               (funcall hook heap addr size))
             (rt-free-list-insert heap size addr)
             (incf freed size)
             (incf addr size))))))
    (setf (rt-heap-lazy-sweep-cursor heap)
          (max (rt-heap-lazy-sweep-cursor heap) addr))
    (when (>= (rt-heap-lazy-sweep-cursor heap) (rt-heap-lazy-sweep-limit heap))
      (%rt-free-list-rebuild-bins
       heap (%gc-coalesce-free-list (rt-heap-free-list-blocks heap))))
    (incf (rt-heap-words-collected heap) freed)
    freed))

(defun %gc-coalesce-free-list (free-list)
  "Coalesce adjacent free blocks in the free-list.
   Sorts by address, then merges blocks where (addr1 + size1) = addr2."
  (if (null free-list)
      nil
      (let* ((sorted (sort (copy-list free-list) #'< :key #'cdr))
             (result nil)
             (current (car sorted)))
        (dolist (block (cdr sorted))
          (let ((cur-size (car current))
                (cur-addr (cdr current))
                (next-size (car block))
                (next-addr (cdr block)))
            (if (= (+ cur-addr cur-size) next-addr)
                ;; Adjacent blocks — merge
                (setf current (cons (+ cur-size next-size) cur-addr))
                ;; Non-adjacent — push current and start new
                (progn
                  (push current result)
                  (setf current block)))))
        (push current result)
        (nreverse result))))

(defun %rt-gc-block-hot-neighbor-score (heap block)
  "Return a small priority score for free-list BLOCK based on adjacent objects."
  (destructuring-bind (size . addr) block
    (let* ((before (max (rt-heap-old-base heap) (1- addr)))
           (after (+ addr size))
           (score 0))
      (declare (ignore before))
      ;; We cannot cheaply find the previous object start in an unindexed
      ;; mark-sweep heap, so this FR-468 hook scores the next object.  The hook is
      ;; deliberately isolated for the future compactor/object index to refine.
      (when (and (< after (rt-heap-old-free heap))
                 (integerp (rt-heap-object-header heap after)))
        (case (rt-gc-classify-hotness heap after)
          (:hot (incf score 2))
          (:warm (incf score 1))))
      score)))

(defun %rt-gc-profile-guide-free-list (heap free-list)
  "Order FREE-LIST so allocations prefer holes adjacent to hot/warm objects."
  (stable-sort (copy-list free-list) #'>
               :key (lambda (block)
                      (%rt-gc-block-hot-neighbor-score heap block))))

(defun rt-gc-compact-old-space (heap)
  "Sliding compaction of old space (FR-089/FR-213).

Algorithm:
  1. Walk old space, build forwarding table: live objects are assigned
     contiguous addresses starting from RT-HEAP-OLD-BASE.  Pinned objects
     stay in place (they are relocation barriers).
  2. Update all pointer slots (roots, young, old, large-object spaces) to
     use forwarded addresses.
  3. Copy each moved object to its new location (bottom-up traversal).
  4. Update old-free, rebuild free-list, reset card table.

Returns a plist describing the compaction result."
  (check-type heap rt-heap)
  (let* ((old-base (rt-heap-old-base heap))
         (old-free (rt-heap-old-free heap))
         (forwarding (make-hash-table :test #'eql))
         (compact-cursor old-base)
         (live-count 0)
         (moved-count 0)
         (dead-words 0))
    ;; Phase 1: Compute new addresses + build forwarding table
    (loop with addr = old-base
          while (< addr old-free) do
            (let ((h (rt-heap-object-header heap addr)))
              (cond
                ((header-forwarding-p h)
                 (incf addr 1))
                ((or (not (integerp h)) (zerop (header-size h)))
                 (setf addr old-free))
                ((header-marked-p h)
                 (let* ((size (header-size h))
                        (pinned (rt-object-pinned-p heap addr))
                        (new-addr (if pinned addr compact-cursor)))
                   (unless (= new-addr addr)
                     (setf (gethash addr forwarding) new-addr)
                     (incf moved-count))
                   (incf live-count)
                    (setf compact-cursor
                          (if pinned
                              ;; Pinned FFI objects are relocation barriers for
                              ;; the Lisp2 slide: subsequent objects must not be
                              ;; assigned into the pinned object's address range.
                              (max compact-cursor (+ addr size))
                              (+ new-addr size)))
                   (incf addr size)))
                (t
                 (let ((size (header-size h)))
                   (incf dead-words size)
                   (incf addr size))))))
    (let ((new-old-free compact-cursor))
      ;; Phase 2: Update pointer slots throughout the heap using forwarding table
      (labels ((maybe-forward (value)
                 (if (and (integerp value) (val-pointer-p value))
                     (let* ((a (decode-pointer value))
                            (na (gethash a forwarding)))
                       (if na (encode-pointer na (pointer-tag value)) value))
                     value))
               (update-range (start end)
                 (loop with a = start
                       while (< a end) do
                         (let ((h (rt-heap-object-header heap a)))
                           (cond
                             ((header-forwarding-p h) (incf a 1))
                             ((and (integerp h) (> (header-size h) 0))
                              (let ((size (header-size h)))
                                (dolist (offset (rt-object-pointer-slots heap a))
                                  (let ((slot (+ a offset)))
                                    (rt-heap-set heap slot
                                                 (maybe-forward (rt-heap-ref heap slot)))))
                                (incf a size)))
                             (t (return)))))))
        ;; 2a. Roots
        (dolist (root-cell (rt-heap-roots heap))
          (setf (cdr root-cell) (maybe-forward (cdr root-cell))))
        ;; 2b. Binding stacks and globals
        (dolist (thread-state *gc-threads*)
          (dolist (binding (%rt-gc-thread-binding-stack thread-state))
            (%rt-gc-set-binding-value binding
             (maybe-forward (%rt-gc-binding-value binding)))))
        (when (boundp '*rt-global-var-registry*)
          (maphash (lambda (sym val)
                     (setf (gethash sym *rt-global-var-registry*)
                           (maybe-forward val)))
                   *rt-global-var-registry*))
        ;; 2c-2e. Update pointer slots in each space
        (update-range (rt-heap-young-from-base heap) (rt-heap-young-free heap))
        (update-range old-base old-free)
        (update-range (rt-heap-large-obj-base heap) (rt-heap-large-obj-free heap))
        ;; 2f-2g. Update SATB and barrier queues
        (setf (rt-heap-satb-queue heap)
              (mapcar (lambda (x) (maybe-forward x)) (rt-heap-satb-queue heap)))
        (setf (rt-heap-barrier-buffer heap)
              (mapcar (lambda (x) (maybe-forward x)) (rt-heap-barrier-buffer heap))))
      ;; Phase 3: Move objects (bottom-up, safe because destinations ≤ sources)
      (loop with addr = old-base
            while (< addr old-free) do
              (let ((h (rt-heap-object-header heap addr)))
                (cond
                  ((header-forwarding-p h)
                   (incf addr 1))
                  ((or (not (integerp h)) (zerop (header-size h)))
                   (setf addr old-free))
                  ((header-marked-p h)
                   (let ((size (header-size h))
                         (new-addr (gethash addr forwarding addr)))
                     (unless (= new-addr addr)
                       ;; Copy words from old location to new location
                       (loop for i from 0 below size do
                         (rt-heap-set heap (+ new-addr i)
                                      (rt-heap-ref heap (+ addr i))))
                       ;; Install forwarding pointer so future scans react correctly
                       (rt-heap-set-header heap addr
                                           (header-make-forwarding-ptr new-addr))
                       ;; Clear mark bit at new location
                       (let ((new-h (rt-heap-object-header heap new-addr)))
                         (rt-heap-set-header heap new-addr
                                             (header-clear-mark new-h))))
                     ;; Clear mark bit at original location (may not match addr if moved)
                     (unless (= new-addr addr)
                       (let ((hdr (rt-heap-object-header heap addr)))
                         (when (integerp hdr)
                           (rt-heap-set-header heap addr (header-clear-mark hdr)))))
                     (incf addr size)))
                  (t
                   ;; Dead — skip
                   (incf addr (header-size h))))))
      ;; Phase 4: Update metadata
      (setf (rt-heap-old-free heap) new-old-free)
      ;; Clear card table (all old→young relationships must be re-recorded)
      (rt-card-clear-all heap)
      ;; Rebuild free-list for whatever dead space remains after compaction
      (%rt-free-list-rebuild-bins heap nil)
      ;; Record stats
      (when (plusp moved-count)
        (incf (rt-heap-words-collected heap) dead-words))
      ;; Return result plist
      (list :status :compact-done
            :algorithm :sliding-compaction
            :pinned-objects-preserved t
            :live-count live-count
            :moved-count moved-count
            :dead-words dead-words
            :old-free-before old-free
            :old-free-after new-old-free
            :fragmentation-before (rt-heap-fragmentation-pct heap)
            :fragmentation-after (if (plusp (- new-old-free old-base))
                                     (/ (float dead-words 1.0d0)
                                        (- new-old-free old-base))
                                     0.0d0)))))

(defun rt-gc-major-collect (heap)
  "Perform a major GC of the old generation using tri-color mark-and-sweep.

   Algorithm:
   1. Set gc-state to :major-gc.
   2. Initial mark: grey all old-space objects directly reachable from roots
      and from young-space roots (scan young objects reachable from roots).
   3. Drain the SATB queue into the grey set (SATB invariant).
   4. Marking loop: repeatedly pop a grey object, mark it black, and grey all
      unvisited old-space children.
   5. Sweep: scan old space linearly, reclaiming unmarked objects to free-list
      and clearing mark bits on survivors.
   6. Reset gc-state to :normal and increment major-gc-count."
  (let ((pause-start (get-internal-real-time)))
  (setf (rt-heap-gc-state heap)
        (if *rt-concurrent-gc-enabled-p*
            :major-gc-concurrent
            :major-gc))
  (unwind-protect
      (let ((queue-cell (cons nil nil)))
        ;; Phase 1: initial mark (STW root snapshot).
        (%rt-gc-seed-major-roots heap queue-cell)
        ;; Phase 2: mark. In concurrent mode a host worker drains the grey queue
        ;; while mutators preserve the initial snapshot through SATB queues.
        (if *rt-concurrent-gc-enabled-p*
            (%rt-gc-run-concurrent-mark heap queue-cell)
            (if *gc-incremental-mark-enabled*
                (%rt-gc-drain-incremental-mark heap queue-cell)
                (%rt-gc-drain-major-mark-work heap queue-cell)))
        ;; Phase 3: final remark (STW SATB drain) and drain any newly grey work.
        (%rt-gc-drain-satb-to-grey heap queue-cell)
        (%rt-gc-drain-major-mark-work heap queue-cell)
        (when (fboundp '%rt-gc-sweep-hash-consing)
          (%rt-gc-sweep-hash-consing))
        ;; Weak refs / weak hash tables / finalizers are processed after the
        ;; strong graph is completely marked and before old-space sweep consumes
        ;; mark bits.  They never seed roots; ephemeron values are the only
        ;; conditional marking path and are drained before weak clearing.
        (let ((marked-set (and (fboundp '%rt-gc-build-marked-set)
                               (%rt-gc-build-marked-set heap))))
          (when marked-set
            (when (fboundp 'rt-gc-process-references)
              (rt-gc-process-references heap marked-set))
            (when (fboundp '%rt-gc-process-finalizers)
              (%rt-gc-process-finalizers heap marked-set))
            (when (fboundp '%rt-gc-process-phantom-references)
              (%rt-gc-process-phantom-references heap marked-set))))
        ;; FR-339: verify no black old-space object still points to a white
        ;; old-space object before marks are consumed by sweeping.
        (multiple-value-bind (ok violating-addr)
            (rt-gc-verify-tri-color-invariant heap)
          (unless ok
            (error "FR-339: tri-color invariant violated at old object ~D"
                   violating-addr)))
        ;; Phase 4: sweep old space.  Concurrent mode uses the same sweep worker
        ;; entry point as the background/lazy path; the portable Pure CL runtime
        ;; joins before returning so the public heap invariants remain unchanged.
        (if *rt-concurrent-gc-enabled-p*
            (rt-gc-concurrent-sweep heap)
            (if *gc-lazy-sweep-enabled*
                (progn
                  (%rt-free-list-rebuild-bins heap nil)
                  (setf (rt-heap-lazy-sweep-cursor heap) (rt-heap-old-base heap)
                        (rt-heap-lazy-sweep-limit heap) (rt-heap-old-free heap))
                  (rt-gc-lazy-sweep-step heap (rt-heap-lazy-sweep-cursor heap)))
                (if (plusp *gc-worker-count*)
                    (rt-gc-parallel-sweep heap *gc-worker-count*)
                    (%gc-sweep-old-space heap))))
        ;; FR-438: integrated class/code unload pass-through after sweeping.
        (rt-gc-run-unload-pass heap)
        (when (fboundp '%rt-gc-clean-adjustable-array-registry)
          (%rt-gc-clean-adjustable-array-registry heap)))
    ;; Always reset gc-state, even on error
    (with-gc-mark-queue-locked ()
      (remhash heap *rt-gc-incremental-mark-queues*))
    (setf (rt-heap-gc-state heap) :normal))
  (incf (rt-heap-major-gc-count heap))
  (%rt-gc-check-pressure heap)
  (%rt-gc-note-pause heap pause-start)
  ;; FR-391: Heap Growth Policy / FR-392: Heap Shrink Policy — resize only
  ;; after a full old-generation collection has had a chance to reclaim garbage.
  ;; Growth wins over shrink if occupancy remains critically high after sweeping.
  (when (rt-gc-should-run-compaction-p heap)
    (rt-gc-compact-old-space heap))
  (unless (rt-heap-maybe-grow heap)
    (rt-heap-maybe-shrink heap))
  (when *gc-verify-after-collect*
    (rt-gc-verify-heap heap))
  heap))

(defun rt-gc-concurrent-sweep-worker (heap)
  "Sweep old space for FR-620 concurrent sweeping infrastructure."
  (check-type heap rt-heap)
  (if *gc-lazy-sweep-enabled*
      (progn
        (%rt-free-list-rebuild-bins heap nil)
        (setf (rt-heap-lazy-sweep-cursor heap) (rt-heap-old-base heap)
              (rt-heap-lazy-sweep-limit heap) (rt-heap-old-free heap))
        (loop while (< (rt-heap-lazy-sweep-cursor heap)
                       (rt-heap-lazy-sweep-limit heap)) do
          (rt-gc-lazy-sweep-step heap (rt-heap-lazy-sweep-cursor heap))))
      (%gc-sweep-old-space heap))
  heap)

(defun rt-gc-concurrent-sweep (heap)
  "Run the old-generation sweep worker on a host thread when available.

The worker function is separate so native runtimes can schedule it truly
concurrently with mutators.  The SBCL-hosted infrastructure joins before the
collector returns, preserving existing test-suite and allocation invariants while
still exercising the same concurrent sweep entry point."
  (check-type heap rt-heap)
  (let ((make-thread (%rt-gc-mark-sb-thread-function "MAKE-THREAD"))
        (join-thread (%rt-gc-mark-sb-thread-function "JOIN-THREAD")))
    (if (and make-thread join-thread)
        (let ((thread (ignore-errors
                        (funcall make-thread
                                 (lambda () (rt-gc-concurrent-sweep-worker heap))
                                 :name "cl-cc concurrent sweep"))))
          (if thread
              (unwind-protect
                   (progn
                     (setf *rt-concurrent-sweep-thread* thread)
                     (funcall join-thread thread))
                (setf *rt-concurrent-sweep-thread* nil))
              (rt-gc-concurrent-sweep-worker heap)))
        (rt-gc-concurrent-sweep-worker heap))))

(defun rt-gc-should-run-compaction-p (heap)
  "Return true when FR-621 old-generation mark-compact should run."
  (and *compacting-gc-enabled*
       (or (rt-heap-should-compact-p heap)
           (and (plusp *gc-compact-after-major-cycles*)
                (plusp (rt-heap-major-gc-count heap))
                (zerop (mod (rt-heap-major-gc-count heap)
                            *gc-compact-after-major-cycles*))))))

;;; Section 6: GC Statistics

(defun rt-gc-stats (heap)
  "Return a plist of current GC statistics for HEAP.

   Keys:
     :minor-gc-count  - number of minor GCs performed
     :major-gc-count  - number of major GCs performed
     :words-collected - total words reclaimed across all GC cycles
     :words-promoted  - total words promoted from young to old generation
     :young-used      - words currently live in young from-space
      :young-total     - total capacity of one young semi-space
      :old-used        - words currently allocated in old space
      :old-total       - total capacity of old space
      :heap-occupancy-pct - young+old occupancy percentage
      :free-list-count - number of free-list entries in old space"
  (append
   (list :minor-gc-count  (rt-heap-minor-gc-count heap)
        :major-gc-count  (rt-heap-major-gc-count heap)
        :words-collected (rt-heap-words-collected heap)
        :words-promoted  (rt-heap-words-promoted heap)
        :young-used      (- (rt-heap-young-free heap)
                            (rt-heap-young-from-base heap))
        :young-total     (rt-heap-young-semi-size heap)
        :old-used        (- (rt-heap-old-free heap)
                            (rt-heap-old-base heap))
        :old-total       (rt-heap-old-size heap)
         :heap-occupancy-pct (rt-heap-occupancy-pct heap)
          :free-list-count (length (rt-heap-free-list-blocks heap))
         :total-alloc-words (rt-heap-total-alloc-words heap)
         :age-hist (coerce (rt-heap-age-hist heap) 'list)
         :age-distribution (%rt-gc-age-distribution-plist heap)
         :large-object-used (- (rt-heap-large-obj-free heap)
                               (rt-heap-large-obj-base heap))
         :large-object-total (rt-heap-large-obj-size heap)
          :gc-pause-total (rt-heap-gc-pause-total heap)
          :gc-pause-max (rt-heap-gc-pause-max heap)
          :gc-throughput-ratio (rt-gc-throughput-ratio heap)
          :gc-throughput-target *gc-throughput-target*
          :gc-max-pause-ms *gc-max-pause-ms*
          :pause-exceeded-count (rt-heap-pause-exceeded-count heap)
          :incremental-work-budget (rt-heap-incremental-work-budget heap)
          :allocation-rate-words-per-sec (rt-heap-allocation-rate-words-per-sec heap)
          :fragmentation-pct (rt-heap-fragmentation-pct heap)
          :should-compact-p (rt-heap-should-compact-p heap)
          :gc-compaction-enabled-p *gc-compaction-enabled*
          :gc-incremental-mark-enabled-p *gc-incremental-mark-enabled*
          :gc-lazy-sweep-enabled-p *gc-lazy-sweep-enabled*
          :lazy-sweep-cursor (rt-heap-lazy-sweep-cursor heap)
          :lazy-sweep-limit (rt-heap-lazy-sweep-limit heap)
          :incremental-mark-queue-length
          (length (with-gc-mark-queue-locked ()
                    (gethash heap *rt-gc-incremental-mark-queues*)))
          :concurrent-gc-enabled-p *rt-concurrent-gc-enabled-p*
          :concurrent-gc-write-barrier *rt-concurrent-gc-write-barrier-mode*
          :concurrent-gc-stw-phases (copy-list *rt-concurrent-gc-stw-phases*)
          :concurrent-gc-mutator-assist-p *rt-concurrent-gc-mutator-assist-p*
          :concurrent-mark-thread-active-p (and (boundp '*rt-concurrent-mark-thread*)
                                                (not (null *rt-concurrent-mark-thread*)))
          :numa-enabled-p (and (boundp '*rt-numa-enabled*) *rt-numa-enabled*))
   (%rt-gc-age-distribution-plist heap)))

;;; ------------------------------------------------------------
;;; Periodic GC (FR-441) & Idle-Time GC (FR-439)
;;; ------------------------------------------------------------

(defparameter *gc-periodic-interval-ms* 0
  "If positive, trigger a minor GC every N milliseconds of wall-clock time.
   Default 0 disables periodic GC.")

(defparameter *gc-last-periodic-gc-time* 0
  "Internal real time of the last periodic GC trigger.")

(defparameter *gc-idle-work-fraction* 0.5d0
  "Fraction of idle time to dedicate to background GC work (FR-439).
   Default 0.5 = 50% of idle time. Set to 0 to disable.")

(defun rt-gc-maybe-periodic-collect (heap)
  "Trigger a minor GC if the periodic interval has elapsed since the last one.
    Call this from the main loop or from rt-gc-alloc periodically."
  (when (and (plusp *gc-periodic-interval-ms*)
             (not (rt-gc-defer-non-critical-work-p heap)))
    (let* ((now (get-internal-real-time))
           (elapsed-ms (* (/ (- now *gc-last-periodic-gc-time*)
                              internal-time-units-per-second)
                          1000)))
      (when (>= elapsed-ms *gc-periodic-interval-ms*)
        (setf *gc-last-periodic-gc-time* now)
        (rt-gc-minor-collect heap)))))

(defun rt-gc-idle-work (heap &key (budget 64))
  "Perform background GC work during idle time (FR-439).
   Processes up to BUDGET SATB queue entries and sweeps a portion
   of old space if major GC is in concurrent mode.
   Returns the number of work units completed."
  (if (rt-gc-defer-non-critical-work-p heap)
      0
      (let ((work-done 0))
        (when (eq (rt-heap-gc-state heap) :major-gc-concurrent)
          (incf work-done (rt-gc-concurrent-assist heap :budget budget)))
        work-done)))

;;; ------------------------------------------------------------
;;; FR-369: Prometheus Metrics Export
;;; ------------------------------------------------------------

(defun rt-gc-prometheus-metrics (heap)
  "Return a string containing GC metrics in Prometheus exposition format.

Output lines follow the Prometheus text-based format:
  # HELP <name> <description>
  # TYPE <name> <type>
  <name>{<label>=\"<value>\"} <value>

Metrics exported:
  gc_pause_seconds{type=\"minor\"}
  gc_pause_seconds{type=\"major\"}
  gc_collections_total{type=\"minor\"}
  gc_collections_total{type=\"major\"}
  heap_used_bytes
  heap_available_bytes
  gc_promoted_bytes"
  (check-type heap rt-heap)
  (let* ((stats (rt-gc-stats heap))
         (minor-pause (/ (float (getf stats :gc-pause-total) 1.0d0) 2.0d0)) ; rough split
         (major-pause (max 0.0d0 (- (getf stats :gc-pause-total) minor-pause)))
         (young-used (- (rt-heap-young-free heap) (rt-heap-young-from-base heap)))
         (old-used (- (rt-heap-old-free heap) (rt-heap-old-base heap)))
         (large-used (- (rt-heap-large-obj-free heap) (rt-heap-large-obj-base heap)))
         (total-used (* (+ young-used old-used large-used) 8))
         (young-capacity (rt-heap-young-semi-size heap))
         (old-capacity (rt-heap-old-size heap))
         (large-capacity (rt-heap-large-obj-size heap))
         (total-capacity (* (+ young-capacity old-capacity large-capacity) 8)))
    (format nil "~{~a~%~}"
            (list
             "# HELP gc_pause_seconds Time spent in GC pauses (cumulative)"
             "# TYPE gc_pause_seconds gauge"
             (format nil "gc_pause_seconds{type=\"minor\"} ~,6f" minor-pause)
             (format nil "gc_pause_seconds{type=\"major\"} ~,6f" major-pause)
             ""
             "# HELP gc_collections_total Total number of GC collections"
             "# TYPE gc_collections_total counter"
             (format nil "gc_collections_total{type=\"minor\"} ~D"
                     (getf stats :minor-gc-count))
             (format nil "gc_collections_total{type=\"major\"} ~D"
                     (getf stats :major-gc-count))
             ""
             "# HELP heap_used_bytes Currently used heap memory in bytes"
             "# TYPE heap_used_bytes gauge"
             (format nil "heap_used_bytes ~D" total-used)
             ""
             "# HELP heap_available_bytes Total managed heap capacity in bytes"
             "# TYPE heap_available_bytes gauge"
             (format nil "heap_available_bytes ~D" total-capacity)
             ""
             "# HELP gc_promoted_bytes Bytes promoted from young to old generation"
             "# TYPE gc_promoted_bytes counter"
             (format nil "gc_promoted_bytes ~D"
                     (* (getf stats :words-promoted) 8))))))

;;; ------------------------------------------------------------
;;; FR-367: DTrace/eBPF Tracing Stubs
;;; ------------------------------------------------------------

(defparameter *gc-probes-enabled* nil
  "When true, GC probe points print to *TRACE-OUTPUT*.

In a native codegen backend this flag would gate DTrace SDT probes (Linux
USDT via SystemTap) or eBPF uprobes.  The Pure CL runtime implements probes
as conditional *TRACE-OUTPUT* logging — a documented stub for future native
integration.

Probe points:
  rt-gc-probe-alloc(size-words)        — called on each allocation
  rt-gc-probe-gc-start(phase)          — called at GC phase start
  rt-gc-probe-gc-end(phase)            — called at GC phase end

Native backends can replace each body with a platform SDT probe:
  DTrace:    dtrace -n 'gc-probe-alloc { printf(...) }'
  eBPF/USDT: bpftrace -e 'usdt:/proc/self/exe:gc_probe_alloc { ... }'")

(defun rt-gc-probe-alloc (size-words)
  "Probe: allocation of SIZE-WORDS words.
When *GC-PROBES-ENABLED* is true, prints to *TRACE-OUTPUT*.
Native replacement: DTrace SDT probe gc-probe-alloc / eBPF uprobe."
  (when *gc-probes-enabled*
    (let ((*print-pretty* nil))
      (format *trace-output* "GC-PROBE-ALLOC ~D~%" size-words)
      (force-output *trace-output*)))
  size-words)

(defun rt-gc-probe-gc-start (phase)
  "Probe: GC phase start (PHASE is :MINOR, :MAJOR, :COMPACT, etc.).
Native replacement: DTrace SDT probe gc-probe-gc-start / eBPF uprobe."
  (when *gc-probes-enabled*
    (let ((*print-pretty* nil))
      (format *trace-output* "GC-PROBE-GC-START ~S~%" phase)
      (force-output *trace-output*)))
  phase)

(defun rt-gc-probe-gc-end (phase)
  "Probe: GC phase end (PHASE is :MINOR, :MAJOR, :COMPACT, etc.).
Native replacement: DTrace SDT probe gc-probe-gc-end / eBPF uprobe."
  (when *gc-probes-enabled*
    (let ((*print-pretty* nil))
      (format *trace-output* "GC-PROBE-GC-END ~S~%" phase)
      (force-output *trace-output*)))
  phase)
