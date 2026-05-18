;;;; packages/runtime/src/gc-major.lisp — Major GC (tri-color mark-sweep) + GC Statistics
;;;
;;; Contains:
;;;   - %gc-sweep-old-space — sweep dead objects from old generation
;;;   - rt-gc-major-collect — tri-color mark-and-sweep full GC
;;;   - rt-gc-stats — GC statistics plist
;;;
;;; Minor GC (Cheney copying), write barrier, allocation, and root registration
;;; are in gc.lisp (loads before).
;;;
;;; Load order: after gc.lisp.
(in-package :cl-cc/runtime)

(defparameter *rt-concurrent-gc-enabled-p* nil
  "When true, major collection enters :major-gc-concurrent state.")

(defparameter *rt-concurrent-gc-write-barrier-mode* :satb
  "Concurrent-GC barrier mode (:satb or :incremental-update).")

(defparameter *rt-concurrent-gc-stw-phases* '(:initial-mark :final-remark)
  "STW phase model used when concurrent GC mode is enabled.")

(defparameter *rt-concurrent-gc-mutator-assist-p* t
  "When true, mutators are expected to assist concurrent marking progress.")

(defparameter *rt-concurrent-mark-thread* nil
  "Host thread object for the active concurrent mark worker, or NIL.

FR-339 foundation: SBCL hosts may run phase-2 marking on SB-THREAD while mutators
continue through SATB pre-write barriers. Implementations without threads drain
the same grey queue synchronously while preserving the four-phase protocol.")

;;; FR-088: Incremental GC Marking — bounded mark steps interleaved with mutator work; 4-phase protocol (initial-mark, incremental-mark, final-remark, sweep)
(defparameter *gc-incremental-mark-enabled* nil
  "When true, major GC exposes its mark phase as bounded incremental steps.

RT-GC-MAJOR-COLLECT is organized as:
  Phase 1: initial mark (STW root snapshot)
  Phase 2: incremental mark (RT-GC-INCREMENTAL-MARK-STEP drains grey work)
  Phase 3: final remark (STW SATB drain)
   Phase 4: sweep (STW in this foundation implementation).")

;;; FR-468: GC Copy Order as Cache Warm-Up — biases free-list toward blocks adjacent to hot objects for cache-friendly placement
(defparameter *gc-profile-guided-placement* nil
  "When true, bias old-space free-list order toward blocks adjacent to hot objects.

This is the FR-468 foundation hook.  The current mark-sweep collector does not
move live objects; it reorders reclaimed blocks so future old-space allocation
prefers holes near hot survivors, preserving a cache-friendly placement path for
the future compacting collector.")

;;; FR-340: Concurrent Sweeping — sweeps old space on-demand during allocation; lazy sweep with page-level granularity
(defparameter *gc-lazy-sweep-enabled* nil
  "When true, major GC defers old-space sweeping and allocation sweeps pages on demand.")

(defparameter *gc-max-pause-ms* 200
  "Maximum target stop-the-world GC pause in milliseconds before SLO accounting fires.")

(defvar *rt-gc-incremental-mark-queues* (make-hash-table :test #'eq)
  "Heap -> grey queue for incremental major-GC marking.")

(defun rt-gc-configure-concurrent-mode (&key enabled-p write-barrier stw-phases mutator-assist-p)
  "Configure concurrent-GC runtime mode and return current settings plist."
  (when (not (null enabled-p))
    (setf *rt-concurrent-gc-enabled-p* enabled-p))
  (when write-barrier
    (setf *rt-concurrent-gc-write-barrier-mode* write-barrier))
  (when stw-phases
    (setf *rt-concurrent-gc-stw-phases* (copy-list stw-phases)))
  (when (not (null mutator-assist-p))
    (setf *rt-concurrent-gc-mutator-assist-p* mutator-assist-p))
  (list :enabled-p *rt-concurrent-gc-enabled-p*
        :write-barrier *rt-concurrent-gc-write-barrier-mode*
        :stw-phases (copy-list *rt-concurrent-gc-stw-phases*)
        :mutator-assist-p *rt-concurrent-gc-mutator-assist-p*))

(defun rt-gc-concurrent-assist (heap &key (budget 16))
  "Perform a bounded mutator-assist step during concurrent major GC.

Drains current SATB entries into the incremental grey queue, marks up to BUDGET
grey objects, and returns the number of grey objects processed."
  (if (or (not *rt-concurrent-gc-enabled-p*)
          (not *rt-concurrent-gc-mutator-assist-p*)
          (not (eq (rt-heap-gc-state heap) :major-gc-concurrent)))
      0
      (let ((queue-cell (cons (gethash heap *rt-gc-incremental-mark-queues*) nil)))
        (%rt-gc-drain-satb-to-grey heap queue-cell)
        (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell))
        (destructuring-bind (status &optional processed)
            (rt-gc-incremental-mark-step heap budget)
          (declare (ignore status))
          (or processed 0)))))

;;; Section 5: Major GC — Tri-color Mark-Sweep

(defun %rt-gc-grey-object (heap queue-cell addr)
  "Push old-space ADDR into QUEUE-CELL when it has not yet been marked/greyed."
  (when (and (integerp addr) (rt-old-addr-p heap addr))
    (let ((h (rt-heap-object-header heap addr)))
      (when (and (integerp h)
                 (not (header-marked-p h))
                 (not (header-gray-p h)))
        (rt-heap-set-header heap addr (header-set-gray h))
        (push addr (car queue-cell))
        addr))))

(defun %rt-gc-package-symbol-address (heap symbol)
  "Return SYMBOL's heap address when it is represented in HEAP."
  (%rt-gc-pointer-address heap symbol :legacy-raw-p t))

(defun rt-gc-mark-package-symbols (heap package)
  "Mark/grey interned symbols reachable from PACKAGE's runtime symbol table.

PACKAGE may be a runtime package descriptor or any designator accepted by the
runtime registry.  Only symbols present in the package's intern table are
considered package roots.  Uninterned host symbols are removed from the runtime
symbol table during this pass so subsequent sweeps do not retain them through
stale package metadata.  Returns the list of heap symbol addresses submitted to
the major-GC mark queue."
  (check-type heap rt-heap)
  (let* ((pkg (and package
                   (if (hash-table-p package)
                       package
                       (and (fboundp '%rt-package-metadata)
                            (%rt-package-metadata package)))))
         (table (and pkg (gethash :symbols pkg)))
         (host-package (and pkg (gethash :host-package pkg)))
         (queue-cell (cons nil nil))
         (marked nil))
    (when table
      (maphash
       (lambda (name symbol)
         (cond
           ((and (symbolp symbol)
                 (null (symbol-package symbol)))
            (remhash name table))
           ((and (symbolp symbol) host-package
                 (symbol-package symbol)
                 (not (eq (symbol-package symbol) host-package)))
            ;; The symbol is interned elsewhere; it is not a root of this package.
            nil)
           (t
            (let ((addr (%rt-gc-package-symbol-address heap symbol)))
              (when (and addr (%rt-gc-grey-object heap queue-cell addr))
                (push addr marked))))))
       table))
    (when pkg
      (setf (gethash :gc-marked-symbols pkg) (nreverse marked)))
    (car queue-cell)))

(defun %rt-gc-mark-package-registry (heap queue-cell)
  "Grey symbols reachable from all runtime package descriptors."
  (when (boundp '*rt-package-registry*)
    (maphash (lambda (name package)
               (declare (ignore name))
               (dolist (addr (rt-gc-mark-package-symbols heap package))
                 (pushnew addr (car queue-cell) :test #'eql)))
             *rt-package-registry*))
  queue-cell)

(defun %rt-gc-seed-major-roots (heap queue-cell)
  "Phase 1 initial mark: grey old-space objects reachable from roots."
  (labels ((maybe-gray (addr)
             (%rt-gc-grey-object heap queue-cell addr))
           (scan-young-object (addr)
             (let ((h (rt-heap-object-header heap addr)))
               (when (and (integerp h) (> (header-size h) 0))
                 (dolist (offset (rt-object-pointer-slots heap addr))
                   (let ((child (%rt-gc-pointer-address
                                 heap (rt-heap-ref heap (+ addr offset))
                                 :legacy-raw-p t)))
                     (when child (maybe-gray child))))))))
    (dolist (root-cell (rt-heap-roots heap))
      (let ((val (%rt-gc-root-pointer-address heap root-cell)))
        (cond
          ((and val (rt-old-addr-p heap val)) (maybe-gray val))
          ((and val (rt-young-addr-p heap val)) (scan-young-object val)))))
    (dolist (addr (%rt-gc-scan-binding-stacks heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    (dolist (addr (%rt-gc-scan-global-variables heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    (%rt-gc-mark-package-registry heap queue-cell)
    (dolist (addr (%rt-gc-conservative-root-addresses heap))
      (cond
        ((rt-old-addr-p heap addr) (maybe-gray addr))
        ((rt-young-addr-p heap addr) (scan-young-object addr))))
    queue-cell))

(defun %rt-gc-drain-satb-to-grey (heap queue-cell)
  "Phase 3 final remark helper: drain SATB entries into the major-GC grey queue."
  (dolist (ptr (nconc (rt-heap-satb-queue heap)
                      (when (fboundp 'rt-gc-drain-satb-thread-queues)
                        (rt-gc-drain-satb-thread-queues heap))))
    (let ((addr (%rt-gc-pointer-address heap ptr :legacy-raw-p t)))
      (when addr
        (%rt-gc-grey-object heap queue-cell addr))))
  (setf (rt-heap-satb-queue heap) nil)
  queue-cell)

(defun %rt-gc-mark-one-grey (heap queue-cell addr)
  "Blacken ADDR and grey each unvisited old-space child."
  (let ((header (rt-heap-object-header heap addr)))
    (when (integerp header)
      (rt-heap-set-header heap addr
                          (header-clear-gray (header-set-mark header)))
      (dolist (offset (rt-object-pointer-slots heap addr))
        (let ((child-addr (%rt-gc-pointer-address
                           heap (rt-heap-ref heap (+ addr offset))
                           :legacy-raw-p t)))
          (when child-addr
            (%rt-gc-grey-object heap queue-cell child-addr)))))))

;;; FR-088: Incremental GC Marking — bounded mark steps interleaved with mutator work; 4-phase protocol (initial-mark, incremental-mark, final-remark, sweep)
(defun rt-gc-incremental-mark-step (heap budget)
  "Process up to BUDGET grey old-space objects for an incremental major GC.

Returns (:MORE-WORK N) when grey work remains after processing N objects, or
(:DONE) when the queue is empty.  The queue is initialized by
RT-GC-MAJOR-COLLECT phase 1 and stored in *RT-GC-INCREMENTAL-MARK-QUEUES* so
mutator assist and idle work can make bounded progress between STW phases."
  (check-type heap rt-heap)
  (check-type budget (integer 0 *))
  (let ((queue-cell (cons (gethash heap *rt-gc-incremental-mark-queues*) nil))
        (processed 0))
    (loop while (and (< processed budget) (car queue-cell)) do
      (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell)))
      (incf processed))
    (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell))
    (if (car queue-cell)
        (list :more-work processed)
        (progn
          (remhash heap *rt-gc-incremental-mark-queues*)
          (list :done)))))

(defun %rt-gc-drain-incremental-mark (heap queue-cell)
  "Drain QUEUE-CELL to completion using the same primitive as incremental steps."
  (setf (gethash heap *rt-gc-incremental-mark-queues*) (car queue-cell))
  (loop for result = (rt-gc-incremental-mark-step heap 64)
        until (eq (first result) :done))
  (setf (car queue-cell) (gethash heap *rt-gc-incremental-mark-queues*))
  queue-cell)

(defun %rt-gc-run-concurrent-mark (heap queue-cell)
  "Phase 2 concurrent mark worker for FR-339.

The worker owns the grey queue between initial mark and final remark. On SBCL it
is launched with SB-THREAD; otherwise the Pure CL fallback executes the same work
synchronously. SATB queues are drained only by final remark to close the snapshot
created by the initial STW root scan."
  (labels ((mark-work ()
             (if *gc-incremental-mark-enabled*
                 (%rt-gc-drain-incremental-mark heap queue-cell)
                 (loop while (car queue-cell) do
                   (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell)))))))
    (if (and (find-package :sb-thread)
             (fboundp (find-symbol "MAKE-THREAD" :sb-thread))
             (fboundp (find-symbol "JOIN-THREAD" :sb-thread)))
        (let* ((make-thread (symbol-function (find-symbol "MAKE-THREAD" :sb-thread)))
               (join-thread (symbol-function (find-symbol "JOIN-THREAD" :sb-thread)))
               (thread (funcall make-thread #'mark-work :name "cl-cc concurrent mark")))
          (setf *rt-concurrent-mark-thread* thread)
          (funcall join-thread thread)
          (setf *rt-concurrent-mark-thread* nil))
        (mark-work)))
  queue-cell)

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
                   (when (not pinned)
                     (setf compact-cursor (+ new-addr size)))
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
                (loop while (car queue-cell) do
                  (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell))))))
        ;; Phase 3: final remark (STW SATB drain) and drain any newly grey work.
        (%rt-gc-drain-satb-to-grey heap queue-cell)
        (loop while (car queue-cell) do
          (%rt-gc-mark-one-grey heap queue-cell (pop (car queue-cell))))
        (when (fboundp '%rt-gc-sweep-hash-consing)
          (%rt-gc-sweep-hash-consing))
        ;; Phase 4: sweep old space.
        (if *gc-lazy-sweep-enabled*
            (progn
              (%rt-free-list-rebuild-bins heap nil)
              (setf (rt-heap-lazy-sweep-cursor heap) (rt-heap-old-base heap)
                    (rt-heap-lazy-sweep-limit heap) (rt-heap-old-free heap))
              (rt-gc-lazy-sweep-step heap (rt-heap-lazy-sweep-cursor heap)))
            (if (plusp *gc-worker-count*)
                (rt-gc-parallel-sweep heap *gc-worker-count*)
                (%gc-sweep-old-space heap)))
        ;; FR-438: integrated class/code unload pass-through after sweeping.
        (rt-gc-run-unload-pass heap)
        (when (fboundp '%rt-gc-clean-adjustable-array-registry)
          (%rt-gc-clean-adjustable-array-registry heap)))
    ;; Always reset gc-state, even on error
    (remhash heap *rt-gc-incremental-mark-queues*)
    (setf (rt-heap-gc-state heap) :normal))
  (incf (rt-heap-major-gc-count heap))
  (%rt-gc-check-pressure heap)
  (%rt-gc-note-pause heap pause-start)
  ;; FR-391/392: resize only after a full old-generation collection has had a
  ;; chance to reclaim garbage.  Growth wins over shrink if occupancy remains
  ;; critically high after sweeping.
  (when (and *gc-compaction-enabled* (rt-heap-should-compact-p heap))
    (rt-gc-compact-old-space heap))
  (unless (rt-heap-maybe-grow heap)
    (rt-heap-maybe-shrink heap))
  (when *gc-verify-after-collect*
    (rt-gc-verify-heap heap))
  heap))

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
          (length (gethash heap *rt-gc-incremental-mark-queues*))
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
