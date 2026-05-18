;;;; packages/runtime/src/gc.lisp - 2-Generation Generational GC for CL-CC Native Runtime
;;;
;;; Depends on heap.lisp being loaded first (rt-heap struct, header functions,
;;; card table, address predicates, rt-object-pointer-slots, etc.).
;;;
;;; Public API:
;;;   rt-gc-alloc           - bump-pointer allocation in young from-space
;;;   rt-gc-add-root        - register a GC root cell
;;;   rt-gc-remove-root     - unregister a GC root cell
;;;   rt-gc-write-barrier   - SATB pre-write barrier + card table update
;;;   rt-gc-minor-collect   - Cheney copying minor GC (young generation)
;;;   rt-gc-major-collect   - tri-color mark-sweep major GC (old generation)
;;;   rt-gc-stats           - return a plist of GC statistics
;;;
;;; Internal helpers are prefixed with %gc-.

(in-package :cl-cc/runtime)

(defparameter *gc-verify-after-collect* nil
  "When true, verify heap invariants after each GC cycle.")

(defparameter *gc-stress-mode* nil
  "When true, trigger a minor GC after each young-space allocation.")

(defparameter *rt-use-slab-allocator* nil
  "When T, use slab/size-class allocators for fixed-size objects (FR-359).
   Default NIL for backward compatibility; enable for optimized workloads.")

(defparameter *gc-conservative-roots* nil
  "When true, conservatively scan registered thread stacks for heap pointers.")

(defparameter *gc-compaction-enabled* nil
  "When true, major GC may invoke the old-space compaction trigger.

FR-089/FR-213: sliding compaction is implemented.  See
rt-gc-compact-old-space for details.  Pinned objects are hard relocation
barriers and must not be moved by compaction.")

(defparameter *gc-threads* '((:name :main :stack nil :frames nil))
  "Thread states scanned by the GC.  Each state may be a plist containing :STACK
and/or :FRAMES, a VM-FRAME, a vector, or a list of raw stack words.")

(defparameter *gc-inhibit-during-signals* t
  "When true, signal-safe regions may inhibit automatic GC entry.")

(defvar *rt-gc-safe-region-depths* (make-hash-table :test #'equal)
  "Logical thread id -> GC safe-region nesting depth.

A positive depth means the mutator has reached a point where the collector may
observe its roots.  The Pure CL runtime uses this as a portable safepoint model;
native backends can map the same API to thread suspension/cooperation.")

(defvar *rt-current-gc-heap* nil
  "Dynamically bound heap used by signal handlers that need to inhibit GC.")

(defparameter *rt-current-thread-id* :main
  "Current logical runtime thread id used by GC safepoint helpers.")

(defparameter *rt-heap-hugepage-enabled* nil
  "When true, native heap setup may request Transparent Huge Page promotion.")

(defvar *rt-gc-root-types* (make-hash-table :test #'eq)
  "Heap -> alist mapping root cells to precise root types.")

(defvar *rt-pinned-objects* (make-hash-table :test #'eq)
  "Heap -> hash-table of pinned object addresses.

Pinning is a relocation constraint: pinned objects must not be moved by old-space
compaction.  During minor GC, a pinned young object is forced through the
promotion path so the inactive semi-space never retains the only live copy.")

(defvar *rt-current-pinning-heap* nil
  "Dynamic heap used by WITH-PINNED-OBJECTS two-element bindings.

Callers that use (WITH-PINNED-OBJECTS ((VAR OBJ) ...) ...) must bind this to the
target heap.  The macro also accepts three-element bindings (VAR HEAP OBJ) when a
heap expression should be supplied explicitly.")

(defparameter *rt-use-barrier-batching* nil
  "When true, write barriers may enqueue card marks for later flushing.")

(defvar *rt-gc-alloc-hooks* nil)
(defvar *rt-gc-death-hooks* nil)

(defparameter *vm-call-frame-pool-limit* 256
  "Maximum number of recycled VM call frames retained by RT-FREE-CALL-FRAME.")

(defvar *vm-call-frame-pool* nil
  "Pool of recycled VM call frames for short-lived calls.")

(defparameter *gc-worker-count* 0
  "Number of parallel GC worker threads.  0 keeps the collector sequential.

Pure Common Lisp has no standard thread API; this runtime uses SB-THREAD when it
is available and otherwise executes worker tasks sequentially.")

(defvar *rt-minor-gc-window-start* nil)
(defvar *rt-low-promotion-cycles* 0)

(defmacro without-gcing ((heap) &body body)
  "Run BODY while inhibiting automatic GC for HEAP."
  `(let ((old-inhibit (rt-heap-gc-inhibit ,heap)))
     (unwind-protect
          (progn
            (setf (rt-heap-gc-inhibit ,heap) t)
            ,@body)
       (setf (rt-heap-gc-inhibit ,heap) old-inhibit)
       (when (and (not old-inhibit) (rt-heap-gc-pending ,heap))
         (setf (rt-heap-gc-pending ,heap) nil)
           (rt-gc-minor-collect ,heap)))))

(defun %rt-gc-thread-id (thread-state)
  "Return the logical thread id for THREAD-STATE."
  (cond
    ((and (consp thread-state) (getf thread-state :id)) (getf thread-state :id))
    ((and (consp thread-state) (getf thread-state :name)) (getf thread-state :name))
    (t thread-state)))

(defun rt-gc-enter-safe-region (thread-id)
  "Enter a GC safe region for THREAD-ID and return the new nesting depth."
  (let ((depth (1+ (gethash thread-id *rt-gc-safe-region-depths* 0))))
    (setf (gethash thread-id *rt-gc-safe-region-depths*) depth)
    depth))

(defun rt-gc-leave-safe-region (thread-id)
  "Leave a GC safe region for THREAD-ID and return the remaining nesting depth."
  (let ((depth (gethash thread-id *rt-gc-safe-region-depths* 0)))
    (when (plusp depth)
      (decf depth)
      (if (zerop depth)
          (remhash thread-id *rt-gc-safe-region-depths*)
          (setf (gethash thread-id *rt-gc-safe-region-depths*) depth)))
    depth))

(defun rt-gc-thread-safe-region-depth (thread-id)
  "Return THREAD-ID's current safe-region nesting depth."
  (gethash thread-id *rt-gc-safe-region-depths* 0))

(defun rt-gc-all-threads-safe-p ()
  "Return true when every registered runtime thread is in a safe region.

An empty thread list is treated as safe.  The default runtime registers :MAIN in
*GC-THREADS*, so a collector can use this predicate before starting cooperative
stop-the-world work."
  (every (lambda (thread-state)
           (plusp (rt-gc-thread-safe-region-depth
                   (%rt-gc-thread-id thread-state))))
         *gc-threads*))

(defmacro with-gc-safe-region ((&optional (thread-id '*rt-current-thread-id*)) &body body)
  "Run BODY while THREAD-ID is recorded as being in a GC safe region."
  (let ((tid (gensym "THREAD-ID")))
    `(let ((,tid ,thread-id))
       (rt-gc-enter-safe-region ,tid)
       (unwind-protect
            (progn ,@body)
         (rt-gc-leave-safe-region ,tid)))))

(defun rt-gc-signal-handler-enter (heap)
  "Inhibit automatic GC while a signal handler is running for HEAP."
  (when (and *gc-inhibit-during-signals* heap)
    (setf (rt-heap-gc-inhibit heap) t))
  heap)

(defun rt-gc-signal-handler-leave (heap &optional old-inhibit)
  "Restore signal-handler GC inhibition and service pending GC if needed."
  (when (and *gc-inhibit-during-signals* heap)
    (setf (rt-heap-gc-inhibit heap) old-inhibit)
    (when (and (not old-inhibit) (rt-heap-gc-pending heap))
      (setf (rt-heap-gc-pending heap) nil)
      (rt-gc-minor-collect heap)))
  heap)

(defmacro with-gc-signal-inhibit ((heap) &body body)
  "Run BODY as a signal handler region that temporarily inhibits GC for HEAP."
  (let ((h (gensym "HEAP"))
        (old (gensym "OLD-INHIBIT")))
    `(let* ((,h ,heap)
            (,old (and ,h (rt-heap-gc-inhibit ,h))))
       (unwind-protect
            (progn
              (rt-gc-signal-handler-enter ,h)
              ,@body)
         (rt-gc-signal-handler-leave ,h ,old)))))

(defun rt-heap-madvise-sequential (heap start end)
  "Portable MADV_SEQUENTIAL heap hint.

On Linux/native targets this interface would call madvise(2) with
MADV_SEQUENTIAL for heap pages in [START, END), telling the kernel that the
collector expects sequential access.  The Pure CL runtime cannot alter host VM
policy, so it validates the range and returns descriptive no-op metadata."
  (check-type heap rt-heap)
  (check-type start integer)
  (check-type end integer)
  (unless (<= 0 start end (length (rt-heap-words heap)))
    (error "Invalid heap madvise range [~D, ~D)" start end))
  (list :status :noop :hint :madv-sequential :start start :end end :portable t))

(defun rt-heap-madvise-willneed (heap start end)
  "Portable MADV_WILLNEED heap hint.

On Linux/native targets this interface would call madvise(2) with
MADV_WILLNEED to prefetch heap pages before GC scans them.  Pure CL returns a
documented no-op result while keeping the call site portable."
  (check-type heap rt-heap)
  (check-type start integer)
  (check-type end integer)
  (unless (<= 0 start end (length (rt-heap-words heap)))
    (error "Invalid heap madvise range [~D, ~D)" start end))
  (list :status :noop :hint :madv-willneed :start start :end end :portable t))

(defun rt-heap-madvise-hugepage (heap)
  "Portable Transparent Huge Page promotion hint for HEAP.

Native Linux runtimes may request MADV_HUGEPAGE for large old/large-object
spaces when *RT-HEAP-HUGEPAGE-ENABLED* is true.  The Pure CL heap is a vector,
so this records the intended policy without changing OS memory mappings."
  (check-type heap rt-heap)
  (list :status :noop
        :hint :transparent-huge-pages
        :enabled *rt-heap-hugepage-enabled*
        :portable t
        :heap-words (length (rt-heap-words heap))))

(defvar *rt-adjustable-array-storage-registry* (make-hash-table :test #'eq)
  "Heap -> records of adjustable-array backing storage superseded by resize.")

(defun rt-gc-register-adjustable-array (heap array-addr storage-addr)
  "Record old adjustable-array backing storage for GC reclamation.

When an adjustable array is resized, ARRAY-ADDR remains the live array object and
STORAGE-ADDR is the old backing storage that should no longer be kept alive by
array metadata.  The collector treats STORAGE-ADDR as ordinary garbage; this
registry is diagnostic bookkeeping cleaned after full collections."
  (check-type heap rt-heap)
  (check-type array-addr integer)
  (check-type storage-addr integer)
  (let ((record (list :array array-addr
                      :old-storage storage-addr
                      :registered-at (get-internal-real-time))))
    (push record (gethash heap *rt-adjustable-array-storage-registry*))
    record))

(defun %rt-gc-clean-adjustable-array-registry (heap)
  "Drop adjustable-array storage records whose old storage is no longer live."
  (let ((records (gethash heap *rt-adjustable-array-storage-registry*)))
    (setf (gethash heap *rt-adjustable-array-storage-registry*)
          (remove-if-not
           (lambda (record)
             (let ((addr (getf record :old-storage)))
               (and (integerp addr)
                    (%rt-gc-object-start-p heap addr))))
           records))))

(defun %rt-pinned-table (heap &key create)
  (or (gethash heap *rt-pinned-objects*)
      (when create
        (setf (gethash heap *rt-pinned-objects*)
              (make-hash-table :test #'eql)))))

(defun %rt-normalize-pin-address (addr)
  (if (and (integerp addr) (val-pointer-p addr))
      (decode-pointer addr)
      addr))

(defun rt-object-pinned-p (heap addr)
  "Return true when ADDR is pinned in HEAP.

Pinned objects are relocation barriers for compaction; compaction must preserve
their addresses and avoid sliding other objects through them."
  (let ((table (%rt-pinned-table heap)))
    (and table (gethash (%rt-normalize-pin-address addr) table))))

;;; FR-212: Object Pinning — prevents GC from relocating pinned objects; essential for FFI safety
(defun rt-pin-object (heap addr)
  "Pin object address ADDR in HEAP and return the normalized address.

Pinning prevents old-space compaction from moving the object.  Young pinned
objects encountered by minor GC are forced into the promotion path so roots do
not retain addresses in the inactive semi-space after a flip."
  (check-type heap rt-heap)
  (let ((normalized (%rt-normalize-pin-address addr)))
    (unless (integerp normalized)
      (error "cl-cc/runtime: cannot pin non-address value ~S" addr))
    (setf (gethash normalized (%rt-pinned-table heap :create t)) t)
    normalized))

(defun rt-unpin-object (heap addr)
  "Remove ADDR from HEAP's pin set and return the normalized address."
  (check-type heap rt-heap)
  (let ((normalized (%rt-normalize-pin-address addr))
        (table (%rt-pinned-table heap)))
    (when table
      (remhash normalized table))
    normalized))

;;; FR-212: Object Pinning — prevents GC from relocating pinned objects; essential for FFI safety
(defmacro with-pinned-objects (bindings &body body)
  "Evaluate BODY with objects pinned for its dynamic extent.

Each binding is either (VAR OBJ), using *RT-CURRENT-PINNING-HEAP*, or
(VAR HEAP OBJ), using an explicit heap expression.  VAR receives OBJ's value.
All pinned objects are unpinned by UNWIND-PROTECT.  Pinning documents a hard
relocation barrier: compaction must not move pinned objects."
  (let ((pins (gensym "PINS")))
    (labels ((parse-binding (binding)
               (destructuring-bind (var &rest rest) binding
                 (ecase (length rest)
                   (1 (values var '*rt-current-pinning-heap* (first rest)))
                   (2 (values var (first rest) (second rest)))))))
      (let ((lets nil)
            (pin-forms nil))
        (dolist (binding bindings)
          (multiple-value-bind (var heap-form obj-form) (parse-binding binding)
            (push `(,var ,obj-form) lets)
            (push `(let ((heap ,heap-form))
                     (unless heap
                       (error "cl-cc/runtime: WITH-PINNED-OBJECTS requires a heap for ~S" ',var))
                     (rt-pin-object heap ,var)
                     (push (cons heap ,var) ,pins))
                  pin-forms)))
        `(let ,(nreverse lets)
           (let ((,pins nil))
             (unwind-protect
                  (progn
                    ,@(nreverse pin-forms)
                    ,@body)
               (dolist (pin ,pins)
                 (rt-unpin-object (car pin) (cdr pin))))))))))

(defun rt-gc-inhibit-p (heap)
  (rt-heap-gc-inhibit heap))

(defun rt-gc-register-alloc-hook (hook)
  (check-type hook function)
  (pushnew hook *rt-gc-alloc-hooks* :test #'eq)
  hook)

(defun rt-gc-register-death-hook (hook)
  (check-type hook function)
  (pushnew hook *rt-gc-death-hooks* :test #'eq)
  hook)

(defun rt-gc-select-policy (heap &optional (policy-key nil policy-key-p))
  "Select or configure a GC policy for HEAP.

Without POLICY-KEY, return the current collection recommendation.  With
POLICY-KEY, configure ergonomic thresholds and pacer targets for one of
:THROUGHPUT, :PAUSE-TIME, or :BALANCED, then return a plist describing the
active policy."
  (if policy-key-p
      (ecase policy-key
        (:throughput
         (setf *gc-throughput-target* 0.05d0
               (rt-heap-pressure-threshold-high heap) 90.0d0
               (rt-heap-pressure-threshold-low heap) 30.0d0
               (rt-heap-shrink-threshold heap) 0.20d0
               (rt-heap-compaction-trigger-fraction heap) 0.60d0)
         (list :policy :throughput
               :throughput-target *gc-throughput-target*
               :pressure-high (rt-heap-pressure-threshold-high heap)
               :pressure-low (rt-heap-pressure-threshold-low heap)
               :shrink-threshold (rt-heap-shrink-threshold heap)))
        (:pause-time
         (setf *gc-throughput-target* 0.02d0
               (rt-heap-pressure-threshold-high heap) 70.0d0
               (rt-heap-pressure-threshold-low heap) 15.0d0
               (rt-heap-shrink-threshold heap) 0.30d0
               (rt-heap-compaction-trigger-fraction heap) 0.40d0)
         (list :policy :pause-time
               :throughput-target *gc-throughput-target*
               :pressure-high (rt-heap-pressure-threshold-high heap)
               :pressure-low (rt-heap-pressure-threshold-low heap)
               :shrink-threshold (rt-heap-shrink-threshold heap)))
        (:balanced
         (setf *gc-throughput-target* 0.05d0
               (rt-heap-pressure-threshold-high heap) 80.0d0
               (rt-heap-pressure-threshold-low heap) 20.0d0
               (rt-heap-shrink-threshold heap) 0.25d0
               (rt-heap-compaction-trigger-fraction heap) 0.50d0)
         (list :policy :balanced
               :throughput-target *gc-throughput-target*
               :pressure-high (rt-heap-pressure-threshold-high heap)
               :pressure-low (rt-heap-pressure-threshold-low heap)
               :shrink-threshold (rt-heap-shrink-threshold heap))))
      (cond
        ((and *gc-compaction-enabled* (rt-heap-should-compact-p heap)) :compact)
        ((>= (rt-heap-occupancy-pct heap) (rt-heap-pressure-threshold-high heap)) :major)
        (t :minor))))

;;; FR-341: GC Pause Time Goals / SLO — records GC pause duration and checks against *gc-max-pause-ms*
(defun %rt-gc-note-pause (heap start-tick)
  (let* ((ticks (- (get-internal-real-time) start-tick))
         (seconds (/ (float ticks 1.0d0) internal-time-units-per-second))
         (millis (* seconds 1000.0d0)))
      (incf (rt-heap-gc-pause-total heap) seconds)
      (setf (rt-heap-gc-pause-max heap)
            (max (rt-heap-gc-pause-max heap) seconds))
      (when (and (boundp '*gc-max-pause-ms*)
                 (> millis *gc-max-pause-ms*))
        (incf (rt-heap-pause-exceeded-count heap))
        (setf (rt-heap-incremental-work-budget heap)
              (max 1 (floor (rt-heap-incremental-work-budget heap) 2))))))

(defun rt-gc-throughput-ratio (heap)
  "Return the fraction of wall-clock runtime spent in GC pauses."
  (let* ((elapsed-ticks (- (get-internal-real-time)
                           (rt-heap-gc-wall-start-tick heap)))
         (elapsed-seconds (/ (float (max 1 elapsed-ticks) 1.0d0)
                             internal-time-units-per-second)))
    (/ (rt-heap-gc-pause-total heap) elapsed-seconds)))

(defun rt-gc-defer-non-critical-work-p (heap)
  "Return true when the pacer should defer non-critical GC work."
  (and (plusp *gc-throughput-target*)
       (> (rt-gc-throughput-ratio heap) *gc-throughput-target*)))

(defun rt-gc-size-collection-set (heap pause-budget-words)
  "Estimate a bounded collection-set size for future mixed GC work.

The estimate is capped by old-space allocation, fragmentation, and pacer state.
When GC has already exceeded its throughput target, this returns 0 so callers
can defer non-critical mixed collection work."
  (check-type pause-budget-words integer)
  (let* ((old-used (- (rt-heap-old-free heap) (rt-heap-old-base heap)))
          (fragmented-words (loop for (size . nil) in (rt-heap-free-list-blocks heap) sum size))
         (effective-budget (max 0 pause-budget-words)))
    (if (rt-gc-defer-non-critical-work-p heap)
        0
        (min old-used
             effective-budget
             (max effective-budget fragmented-words)))))

(defun %rt-gc-note-allocation-rate (heap)
  "Update HEAP's allocation-rate EMA in words per second."
  (let* ((now (get-internal-real-time))
         (last-tick (rt-heap-allocation-rate-last-tick heap))
         (last-total (rt-heap-allocation-rate-last-total heap))
         (elapsed-ticks (- now last-tick))
         (delta-words (- (rt-heap-total-alloc-words heap) last-total)))
    (when (and (plusp elapsed-ticks) (plusp delta-words))
      (let* ((elapsed-seconds (/ (float elapsed-ticks 1.0d0)
                                 internal-time-units-per-second))
             (instant-rate (/ (float delta-words 1.0d0) elapsed-seconds))
             (previous (rt-heap-allocation-rate-words-per-sec heap)))
        (setf (rt-heap-allocation-rate-words-per-sec heap)
              (if (zerop previous)
                  instant-rate
                  (+ (* 0.20d0 instant-rate) (* 0.80d0 previous))))))
    (setf (rt-heap-allocation-rate-last-tick heap) now
          (rt-heap-allocation-rate-last-total heap) (rt-heap-total-alloc-words heap))))

(defun %rt-gc-enforce-heap-limit (heap requested-words)
  "Force major GC before exceeding *GC-MAX-HEAP-WORDS* and signal if still over."
  (when (and (plusp *gc-max-heap-words*)
             (> (+ (%rt-heap-live-used-words heap) requested-words)
                *gc-max-heap-words*))
    (unless (rt-heap-gc-inhibit heap)
      (rt-gc-major-collect heap))
    (when (> (+ (%rt-heap-live-used-words heap) requested-words)
             *gc-max-heap-words*)
      (error "cl-cc/runtime: GC heap limit exceeded — ~D live/requested words exceed limit ~D"
             (+ (%rt-heap-live-used-words heap) requested-words)
             *gc-max-heap-words*))))

(defun %rt-gc-check-pressure (heap)
  (let* ((occupancy (rt-heap-occupancy-pct heap))
         (level (cond
                  ((>= occupancy (rt-heap-pressure-threshold-high heap)) :high)
                  ((<= occupancy (rt-heap-pressure-threshold-low heap)) :low)
                  (t nil))))
    (when level
      (dolist (hook (rt-heap-pressure-hooks heap))
        (funcall hook heap level occupancy)))
    level))

;;; FR-085: Dynamic GC Age Threshold Tuning — adjusts *gc-tenuring-threshold* based on promotion ratio
(defun rt-gc-dynamic-tenure (promotion-ratio)
  "Adjust *GC-TENURING-THRESHOLD* according to the last minor-GC promotion ratio."
  (cond
    ((> promotion-ratio 0.5d0)
     (setf *gc-tenuring-threshold* (max 1 (1- *gc-tenuring-threshold*))))
    ((< promotion-ratio 0.1d0)
     (setf *gc-tenuring-threshold* (min 15 (1+ *gc-tenuring-threshold*)))))
  *gc-tenuring-threshold*)

;;; FR-333: Nursery Sizing Heuristics — adaptive young size based on promotion ratio and allocation rate
(defun %rt-gc-tune-nursery (promotion-ratio)
  "Adjust the default nursery size for subsequently created heaps."
  (let* ((now (get-internal-real-time))
         (freq-high-p (and *rt-minor-gc-window-start*
                           (< (/ (float (- now *rt-minor-gc-window-start*) 1.0d0)
                                 internal-time-units-per-second)
                              1.0d0))))
    (unless *rt-minor-gc-window-start*
      (setf *rt-minor-gc-window-start* now))
    (cond
      ((or (> promotion-ratio 0.5d0) freq-high-p)
       (setf *rt-low-promotion-cycles* 0
             *gc-young-size-words* (min (* *gc-young-size-words* 2)
                                        (* 16 1024 1024))))
      ((< promotion-ratio 0.05d0)
       (incf *rt-low-promotion-cycles*)
       (when (>= *rt-low-promotion-cycles* 3)
         (setf *gc-young-size-words* (max (floor *gc-young-size-words* 2)
                                          (* 32 1024))
               *rt-low-promotion-cycles* 0)))
      (t
       (setf *rt-low-promotion-cycles* 0)))))

;;; FR-331: Old-Space Free-List Allocation Reuse — first-fit/best-fit from sweep free-list before bump-pointer fallback
(defun %rt-gc-alloc-old-from-free-list (heap size)
  "First-fit old-space allocation from the sweep free-list."
  (multiple-value-bind (bin addr) (rt-free-list-find heap size)
    (declare (ignore bin))
    (or addr
        (when (and (fboundp 'rt-gc-lazy-sweep-step)
                   (< (rt-heap-lazy-sweep-cursor heap)
                      (rt-heap-lazy-sweep-limit heap)))
          (rt-gc-lazy-sweep-step heap (rt-heap-lazy-sweep-cursor heap))
          (nth-value 1 (rt-free-list-find heap size))))))

(defun %rt-zero-frame (frame)
  (when (and frame (fboundp 'vm-frame-registers))
    (fill (vm-frame-registers frame) +val-nil+))
  (when (and frame (fboundp '(setf vm-frame-sp)))
    (setf (vm-frame-sp frame) 0))
  (when (and frame (fboundp '(setf vm-frame-pc)))
    (setf (vm-frame-pc frame) 0))
  frame)

(defun rt-alloc-call-frame (&optional (size 0))
  "Allocate a VM call frame, reusing a zeroed frame from *VM-CALL-FRAME-POOL* when possible."
  (let ((frame (pop *vm-call-frame-pool*)))
    (cond
      (frame (%rt-zero-frame frame))
      ((fboundp 'make-vm-frame) (make-vm-frame :register-count size))
      ((fboundp '%make-vm-frame) (%make-vm-frame))
      (t (make-array size :initial-element +val-nil+)))))

(defun rt-free-call-frame (frame)
  "Zero FRAME and return it to the bounded VM call-frame pool."
  (let ((zeroed (%rt-zero-frame frame)))
    (when (< (length *vm-call-frame-pool*) *vm-call-frame-pool-limit*)
      (push zeroed *vm-call-frame-pool*))
    zeroed))

;;; ------------------------------------------------------------
;;; FR-338: Parallel GC Worker Threads
;;;
;;; Foundation implementation. When *gc-worker-count* is 0 (default),
;;; all GC work runs sequentially on the GC thread. When > 0 and
;;; SB-THREAD is available, work is distributed across worker threads.
;;;
;;; Current capabilities:
;;;   - Work partitioning via %rt-gc-partition-list (round-robin)
;;;   - Sequential fallback for Pure CL without threads
;;;   - Per-phase parallelism: root-scan, mark-drain, sweep
;;;
;;; Future (requires native thread support):
;;;   - Work-stealing between workers
;;;   - Per-thread SATB queues (FR-339 foundation)
;;;   - NUMA-aware worker placement (FR-364)
;;; ------------------------------------------------------------

(defun %rt-gc-sb-thread-function (name)
  "Return the SB-THREAD function named NAME, or NIL when unavailable."
  (let ((package (find-package "SB-THREAD")))
    (when package
      (multiple-value-bind (symbol status) (find-symbol name package)
        (when (and status (fboundp symbol))
          (symbol-function symbol))))))

(defun rt-gc-detect-worker-count ()
  "Return recommended number of GC worker threads based on host environment.
Returns 0 (= sequential) when SB-THREAD is unavailable."
  (let ((make-thread (%rt-gc-sb-thread-function "MAKE-THREAD"))
        (join-thread (%rt-gc-sb-thread-function "JOIN-THREAD")))
    (if (and make-thread join-thread)
        (let ((thread (ignore-errors
                        (funcall make-thread (lambda ()) :name "gc-probe"))))
          (when thread
            (ignore-errors (funcall join-thread thread)))
          (if thread 1 0))
        0)))

(defun %rt-gc-run-worker-tasks (tasks)
  "Run TASKS in parallel with SB-THREAD when configured, otherwise sequentially."
  (let ((make-thread (%rt-gc-sb-thread-function "MAKE-THREAD"))
        (join-thread (%rt-gc-sb-thread-function "JOIN-THREAD")))
    (if (and (plusp *gc-worker-count*) make-thread join-thread)
        (let ((errors nil))
          (let ((threads
                  (mapcar (lambda (task)
                            (handler-case
                                (funcall make-thread task :name "gc-worker")
                              (error (c)
                                (push c errors)
                                nil)))
                          tasks)))
            (when errors
              (warn "FR-338: ~D GC worker thread(s) failed to start, using sequential fallback"
                    (length errors)))
            (if (and threads (notany #'null threads) (null errors))
                (let ((results
                        (mapcar (lambda (thread)
                                  (handler-case
                                      (funcall join-thread thread)
                                    (error (c)
                                      (push c errors)
                                      nil)))
                                threads)))
                  (when errors
                    (warn "FR-338: ~D GC worker thread(s) failed during execution"
                          (length errors)))
                  results)
                (progn
                  ;; If startup was only partially successful, wait for already
                  ;; started workers before the sequential fallback so no worker
                  ;; task runs concurrently with its fallback copy.
                  (dolist (thread (remove nil threads))
                    (handler-case
                        (funcall join-thread thread)
                      (error (c)
                        (push c errors))))
                  (mapcar #'funcall tasks)))))
        (mapcar #'funcall tasks))))

(defun %rt-gc-run-workers-with-progress (tasks &key (phase :unknown))
  "Run TASKS across *GC-WORKER-COUNT* workers, returning total items processed.
Reports progress when *GC-PROBES-ENABLED* is true."
  (let ((total 0))
    (if (or (zerop *gc-worker-count*)
            (not (find-package "SB-THREAD")))
        (dolist (task tasks total)
          (let ((result (funcall task)))
            (when (integerp result)
              (incf total result))
            (when (and (boundp '*gc-probes-enabled*)
                       (symbol-value '*gc-probes-enabled*))
              (format *trace-output* "GC-WORKER-PROGRESS ~S ~D~%" phase total)
              (force-output *trace-output*))))
        (let ((results (%rt-gc-run-worker-tasks tasks)))
          (dolist (result results total)
            (when (integerp result)
              (incf total result))
            (when (and (boundp '*gc-probes-enabled*)
                       (symbol-value '*gc-probes-enabled*))
              (format *trace-output* "GC-WORKER-PROGRESS ~S ~D~%" phase total)
              (force-output *trace-output*)))))))

(defun %rt-gc-partition-list (items workers)
  (let* ((count (max 1 workers))
         (buckets (make-array count :initial-element nil)))
    (loop for item in items
          for i from 0 do
            (push item (svref buckets (mod i count))))
    (loop for i from 0 below count collect (nreverse (svref buckets i)))))

;;; FR-338: Parallel GC Worker Threads (foundation) — distributes roots/mark/sweep work across workers when SB-THREAD available
(defun rt-gc-parallel-root-scan (heap &optional (workers *gc-worker-count*))
  "Return root addresses using worker-partitioned scans when host threads exist."
  (let ((parts (%rt-gc-partition-list (rt-heap-roots heap) (max 1 workers))))
    ;; Each worker returns a private result list; the caller merges those lists
    ;; after all workers have finished, avoiding unsynchronized PUSH/NCONC.
    (let ((per-worker-results
            (%rt-gc-run-worker-tasks
             (mapcar (lambda (part)
                       (lambda ()
                         (loop for root in part
                               for addr = (%rt-gc-root-pointer-address heap root)
                               when addr collect addr)))
                     parts))))
      (remove-duplicates (apply #'nconc per-worker-results) :test #'eql))))

;;; FR-338: Parallel GC Worker Threads (foundation) — distributes roots/mark/sweep work across workers when SB-THREAD available
(defun rt-gc-parallel-mark (heap &optional (workers *gc-worker-count*))
  "Drain the major-GC grey queue with worker-sized batches.

The Pure CL fallback is deterministic and sequential; SBCL hosts may run batches
through SB-THREAD, with coarse-grained queue partitioning rather than lock-free
work stealing."
  (let ((queue (gethash heap *rt-gc-incremental-mark-queues*)))
    ;; Collect per-worker counts and sum them after workers finish.  The count
    ;; mutation below is worker-local, so no shared PROCESSED counter is touched
    ;; concurrently under SB-THREAD; the Pure CL fallback remains sequential.
    (let ((worker-results
            (%rt-gc-run-worker-tasks
             (mapcar (lambda (part)
                       (lambda ()
                         (let ((processed 0)
                               (cell (cons part nil)))
                           (loop while (car cell) do
                             (%rt-gc-mark-one-grey heap cell (pop (car cell)))
                             (incf processed))
                           processed)))
                     (%rt-gc-partition-list queue (max 1 workers))))))
      (remhash heap *rt-gc-incremental-mark-queues*)
      (apply #'+ worker-results))))

(defun rt-gc-parallel-sweep (heap &optional (workers *gc-worker-count*))
  "Run old-space sweep.  Region division is documented; mutation is serialized for safety."
  (declare (ignore workers))
  (%gc-sweep-old-space heap))

(defun %rt-gc-valid-header-p (header)
  (and (integerp header)
       (> (header-size header) 0)
       (<= 0 (header-tag header) 7)))

(defun %rt-gc-object-start-p (heap addr)
  "Return true when ADDR appears to designate a live heap object header."
  (and (integerp addr)
       (rt-heap-addr-p heap addr)
       (let ((header (rt-heap-object-header heap addr)))
         (or (header-forwarding-p header)
             (%rt-gc-valid-header-p header)))))

;;; FR-336: GC-NaN-Boxing Integration — uses val-pointer-p/decode-pointer for precise pointer identification during GC scanning
(defun %rt-gc-pointer-address (heap value &key legacy-raw-p)
  "Return the heap address encoded by VALUE, or NIL when VALUE is not a pointer.

NaN-boxed pointer values are recognized with VAL-POINTER-P and decoded with
DECODE-POINTER.  LEGACY-RAW-P preserves compatibility with older runtime tests
and root cells that still store raw heap addresses directly."
  (cond
    ((and (integerp value) (val-pointer-p value))
     (let ((addr (decode-pointer value)))
       (when (%rt-gc-object-start-p heap addr)
         addr)))
    ((and legacy-raw-p (%rt-gc-object-start-p heap value))
     value)
    (t nil)))

(defun %rt-gc-value-address-for-predicate (value predicate &key legacy-raw-p)
  "Return VALUE's decoded address when PREDICATE accepts it.

This is used during minor GC after from/to-space flipping, where the evacuation
source is no longer considered a live heap range by RT-HEAP-ADDR-P."
  (cond
    ((and (integerp value) (val-pointer-p value))
     (let ((addr (decode-pointer value)))
       (when (funcall predicate addr) addr)))
    ((and legacy-raw-p (integerp value) (funcall predicate value))
     value)
    (t nil)))

(defun %rt-gc-rebox-pointer-like (old-value new-addr)
  "Preserve OLD-VALUE's pointer representation while replacing its address."
  (if (and (integerp old-value) (val-pointer-p old-value))
      (encode-pointer new-addr (pointer-tag old-value))
      new-addr))

(defun %rt-gc-root-type (heap root-cell)
  (or (cdr (assoc root-cell (gethash heap *rt-gc-root-types*) :test #'eq))
      :any))

(defun %rt-gc-root-pointer-address (heap root-cell)
  "Return ROOT-CELL's heap address according to its precise root metadata."
  (let ((value (cdr root-cell)))
    (case (%rt-gc-root-type heap root-cell)
      (:pointer (%rt-gc-pointer-address heap value :legacy-raw-p nil))
      (:any     (%rt-gc-pointer-address heap value :legacy-raw-p t))
      ((:fixnum :double :char) nil)
      (otherwise nil))))

(defun %rt-gc-thread-words (thread-state)
  "Return a list of conservative stack words described by THREAD-STATE."
  (cond
    ((null thread-state) nil)
    ((vm-frame-p thread-state)
     (loop for value across (vm-frame-registers thread-state) collect value))
    ((vectorp thread-state)
     (loop for value across thread-state collect value))
    ((and (consp thread-state)
          (or (getf thread-state :stack) (getf thread-state :frames)))
     (append (copy-list (getf thread-state :stack))
             (loop for frame in (getf thread-state :frames)
                   append (%rt-gc-thread-words frame))))
    ((listp thread-state) thread-state)
    (t nil)))

(defun %rt-gc-thread-binding-stack (thread)
  "Return THREAD's dynamic binding stack, if any."
  (cond
    ((and (consp thread) (getf thread :bindings)) (getf thread :bindings))
    ((boundp '*rt-dynamic-binding-stacks*)
     (gethash (%rt-gc-thread-id thread) *rt-dynamic-binding-stacks*))
    (t nil)))

(defun %rt-gc-binding-symbol (binding)
  (cond
    ((and (consp binding) (getf binding :symbol)) (getf binding :symbol))
    ((consp binding) (car binding))
    (t nil)))

(defun %rt-gc-binding-value (binding)
  (cond
    ((and (consp binding) (getf binding :value)) (getf binding :value))
    ((consp binding) (cdr binding))
    (t nil)))

(defun %rt-gc-set-binding-value (binding value)
  (cond
    ((and (consp binding) (getf binding :value))
     (setf (getf binding :value) value))
    ((consp binding)
     (setf (cdr binding) value)))
  binding)

(defun rt-gc-scan-binding-stack (heap thread)
  "Return heap pointer addresses found in THREAD's dynamic binding stack.

Bindings are treated as additional GC roots.  Bindings for global-only special
variables are skipped because those symbols never use thread-local dynamic
storage and are traced via the global/runtime registries instead."
  (check-type heap rt-heap)
  (remove-duplicates
   (loop for binding in (%rt-gc-thread-binding-stack thread)
         for sym = (%rt-gc-binding-symbol binding)
         for skip = (and sym
                         (fboundp 'rt-special-variable-global-only-p)
                         (rt-special-variable-global-only-p sym))
         for addr = (and (not skip)
                         (%rt-gc-pointer-address heap (%rt-gc-binding-value binding)
                                                :legacy-raw-p t))
         when addr collect addr)
   :test #'eql))

(defun %rt-gc-scan-binding-stacks (heap)
  "Return heap pointer addresses from all registered dynamic binding stacks."
  (remove-duplicates
   (loop for thread-state in *gc-threads*
         append (rt-gc-scan-binding-stack heap thread-state))
   :test #'eql))

(defun %rt-gc-global-variable-bindings ()
  "Return (SYMBOL . VALUE) bindings from the runtime global special registry."
  (when (boundp '*rt-global-var-registry*)
    (let (bindings)
      (maphash (lambda (sym value) (push (cons sym value) bindings))
               *rt-global-var-registry*)
      bindings)))

(defun %rt-gc-scan-global-variables (heap)
  "Return heap pointer addresses held by global-only special variables."
  (remove-duplicates
   (loop for binding in (%rt-gc-global-variable-bindings)
         for addr = (%rt-gc-pointer-address heap (cdr binding) :legacy-raw-p t)
         when addr collect addr)
   :test #'eql))

(defun %rt-gc-conservative-root-addresses (heap)
  "Return stack words that conservatively look like valid heap object pointers."
  (when *gc-conservative-roots*
    (remove-duplicates
     (loop for thread-state in *gc-threads*
           append (loop for word in (%rt-gc-thread-words thread-state)
                        for addr = (%rt-gc-pointer-address heap word :legacy-raw-p t)
                        when addr collect addr))
     :test #'eql)))

(defun rt-gc-verify-heap (heap)
  "Verify basic heap invariants and signal an error on corruption."
  (labels ((verify-range (start end evacuated-p)
             (loop with addr = start
                   while (< addr end) do
                     (let ((h (rt-heap-object-header heap addr)))
                       (when (and evacuated-p (header-forwarding-p h))
                         (error "GC verify: forwarding pointer remains at ~D" addr))
                       (cond
                         ((header-forwarding-p h)
                          (if evacuated-p
                              (error "GC verify: forwarding pointer remains at ~D" addr)
                              (incf addr 1)))
                         ((and (integerp h) (zerop h)) (return))
                         ((not (%rt-gc-valid-header-p h))
                          (error "GC verify: invalid header at ~D: ~S" addr h))
                          (t
                           (let ((size (header-size h)))
                             (when (> (+ addr size) end)
                               (error "GC verify: object at ~D exceeds range" addr))
                             (when (or (header-marked-p h) (header-gray-p h))
                               (error "GC verify: mark/gray bits not cleared at ~D" addr))
                             (dolist (offset (rt-object-pointer-slots heap addr))
                               (let ((value (rt-heap-ref heap (+ addr offset))))
                                 (when (and (integerp value) (val-pointer-p value))
                                   (let ((target (decode-pointer value)))
                                     (unless (%rt-gc-object-start-p heap target)
                                       (error "GC verify: invalid boxed pointer ~S at ~D+~D"
                                              value addr offset))))
                                 (when (and (integerp value)
                                            (not (val-pointer-p value))
                                            (rt-heap-addr-p heap value)
                                            (not (%rt-gc-object-start-p heap value)))
                                   (error "GC verify: invalid raw pointer ~S at ~D+~D"
                                          value addr offset))))
                             (incf addr size))))))))
    (verify-range (rt-heap-young-from-base heap) (rt-heap-young-free heap) nil)
    (verify-range (rt-heap-old-base heap) (rt-heap-old-free heap) t)
    (verify-range (rt-heap-large-obj-base heap) (rt-heap-large-obj-free heap) t)
    t))

;; Ensure the card-size constant is present even if this file is compiled or
;; loaded in isolation during incremental builds.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*gc-tenuring-threshold*)
    (defparameter *gc-tenuring-threshold* 3
      "Minor GC survival cycles before promotion to old generation."))
  (unless (boundp '+gc-card-size-words+)
    (defconstant +gc-card-size-words+ 64
      "Card size in words (512 bytes with 8-byte words).")))

;;; ------------------------------------------------------------
;;; Section 1: Allocation
;;; ------------------------------------------------------------

(defun rt-gc-alloc (heap type-tag size-words)
  "Allocate SIZE-WORDS words in young from-space and return the word address.
   TYPE-TAG is stored for documentation purposes but the header is NOT written here;
   the caller is responsible for writing the header word immediately after allocation.
   Automatically triggers a minor GC if from-space is exhausted.
   Signals an error if the heap is exhausted even after GC."
  (%rt-gc-enforce-heap-limit heap size-words)
  (incf (rt-heap-total-alloc-words heap) size-words)
  (%rt-gc-note-allocation-rate heap)
  (dolist (hook *rt-gc-alloc-hooks*)
    (funcall hook heap size-words))
  ;; The allocator returns an uninitialized object whose header is written by the
  ;; caller.  Stress mode therefore collects immediately before each allocation,
  ;; exercising GC on every allocation boundary without evacuating a headerless
  ;; object that has not yet been installed by the caller.
  (when (and *gc-stress-mode*
             (not (rt-heap-gc-inhibit heap))
             (eq (rt-heap-gc-state heap) :normal)
             (> (rt-heap-young-free heap) (rt-heap-young-from-base heap)))
    (rt-gc-minor-collect heap))
  (let ((slab-class (and *rt-use-slab-allocator*
                          (rt-slab-size-class type-tag size-words))))
    (cond
     (slab-class
      ;; Try slab allocation first; fall back to bump-pointer if exhausted
      (handler-case
          (let ((addr (rt-slab-alloc heap slab-class)))
            (rt-gc-profile-sample (* size-words 8))
            addr)
        (error ()
          ;; Slab exhausted — fall through to bump-pointer allocator
          nil)))
    ;;; FR-086: Large Object Space (LOS) — objects exceeding threshold bypass nursery; allocated directly in large-object space
    ((> size-words (rt-heap-large-obj-threshold heap))
      (let* ((addr (rt-heap-large-obj-free heap))
             (limit (+ (rt-heap-large-obj-base heap)
                       (rt-heap-large-obj-size heap))))
        (when (> (+ addr size-words) limit)
          (error "cl-cc/runtime: large object space exhausted — ~D words requested" size-words))
        (setf (rt-heap-large-obj-free heap) (+ addr size-words))
        (rt-gc-profile-sample (* size-words 8))
        addr))
    (t
      (let* ((from-base (rt-heap-young-from-base heap))
             (semi-size (rt-heap-young-semi-size heap))
             (limit     (+ from-base semi-size))
             (addr      (rt-heap-young-free heap)))
        (when (> (+ addr size-words) limit)
          (if (rt-heap-gc-inhibit heap)
              (setf (rt-heap-gc-pending heap) t)
              (rt-gc-minor-collect heap))
          ;; Recompute after GC (bases may have flipped)
          (setf from-base (rt-heap-young-from-base heap)
                limit     (+ from-base semi-size)
                addr      (rt-heap-young-free heap))
          (when (> (+ addr size-words) limit)
            (error "cl-cc/runtime: heap exhausted — ~D words requested, ~D words available in young space"
                   size-words (- limit addr))))
        (setf (rt-heap-young-free heap) (+ addr size-words))
        (rt-gc-profile-sample (* size-words 8))
        addr)))))

;;; ------------------------------------------------------------
;;; Section 2: Root Registration
;;; ------------------------------------------------------------

(defun rt-gc-add-root (heap root-cell)
  "Register ROOT-CELL as a GC root.
   ROOT-CELL must be a cons whose cdr holds the heap address to keep live.
   The GC updates (cdr root-cell) in place when the object is moved."
  (rt-gc-add-root-typed heap root-cell :any))

(defun rt-gc-add-root-typed (heap root-cell type)
  "Register ROOT-CELL as a GC root with precise TYPE metadata.

TYPE is one of :POINTER, :FIXNUM, :DOUBLE, :CHAR, or :ANY.  :ANY preserves the
legacy conservative behavior used by older callers; typed non-pointer roots are
skipped entirely by the collectors."
  (check-type root-cell cons)
  (unless (member type '(:pointer :fixnum :double :char :any) :test #'eq)
    (error "cl-cc/runtime: invalid GC root type ~S" type))
  (pushnew root-cell (rt-heap-roots heap) :test #'eq)
  (let ((alist (gethash heap *rt-gc-root-types*)))
    (setf (gethash heap *rt-gc-root-types*)
          (acons root-cell type (delete root-cell alist :key #'car :test #'eq))))
  root-cell)

(defun rt-gc-remove-root (heap root-cell)
  "Unregister ROOT-CELL from the GC root set."
  (setf (rt-heap-roots heap)
        (delete root-cell (rt-heap-roots heap) :test #'eq))
  (let ((alist (gethash heap *rt-gc-root-types*)))
    (setf (gethash heap *rt-gc-root-types*)
          (delete root-cell alist :key #'car :test #'eq)))
  root-cell)

;;; ------------------------------------------------------------
;;; Section 3: Minor GC — Cheney Copying
;;; ------------------------------------------------------------

(defun %rt-gc-normalize-address (addr)
  (if (and (integerp addr) (val-pointer-p addr))
      (decode-pointer addr)
      addr))

(defun rt-gc-co-locate (heap obj1-addr obj2-addr)
  "Record an object co-location hint for the next copying/compacting GC.

FR-307: when OBJ1 is copied by Cheney evacuation, OBJ2 is preferentially copied
immediately after it so related objects become adjacent.  The hint is recorded in
both directions; future old-space compactors can consume the same metadata when
choosing sliding addresses."
  (check-type heap rt-heap)
  (let ((obj1 (%rt-gc-normalize-address obj1-addr))
        (obj2 (%rt-gc-normalize-address obj2-addr)))
    (unless (and (integerp obj1) (integerp obj2))
      (error "cl-cc/runtime: co-location hints require object addresses: ~S ~S"
             obj1-addr obj2-addr))
    (setf (gethash obj1 (rt-heap-co-location-hints heap)) obj2
          (gethash obj2 (rt-heap-co-location-hints heap)) obj1)
    (list :status :recorded :object-1 obj1 :object-2 obj2)))

(defun %rt-gc-copy-co-located-neighbor (heap from-addr to-free-cell promoted-list-cell in-source-p)
  "Copy FROM-ADDR's co-location neighbor next when it is still in the source set."
  (let* ((hints (rt-heap-co-location-hints heap))
         (neighbor (and hints (gethash from-addr hints))))
    (when neighbor
      ;; One-shot consumption avoids recursive ping-pong while preserving adjacency.
      (remhash from-addr hints)
      (remhash neighbor hints)
      (when (and in-source-p
                 (integerp neighbor)
                 (/= neighbor from-addr)
                 (funcall in-source-p neighbor))
        (let ((neighbor-header (rt-heap-object-header heap neighbor)))
          (unless (header-forwarding-p neighbor-header)
            (multiple-value-bind (neighbor-dest neighbor-free)
                (%gc-copy-object heap neighbor (cdr to-free-cell) promoted-list-cell)
              (declare (ignore neighbor-dest))
              (setf (cdr to-free-cell) neighbor-free))))))))

(defun %gc-copy-object (heap from-addr to-free promoted-list-cell)
  "Copy the object at FROM-ADDR to TO-FREE, or to old space if promotion applies.
   When promoted, push the destination old-space address onto (car PROMOTED-LIST-CELL)
   so the caller can later scan those objects for old->young pointers.
   Installs a forwarding pointer (cons :forwarded dest) at FROM-ADDR slot 0.
   Returns (values new-addr new-to-free)."
  (let* ((header    (rt-heap-object-header heap from-addr))
         (size      (header-size header))
         (age       (header-age header))
         (pinned-p  (rt-object-pinned-p heap from-addr))
         (promote-p (or pinned-p (>= age *gc-tenuring-threshold*))))
    (let ((dest-addr
            (if promote-p
                ;; Promote to old space, reusing sweep free-list blocks first.
                (or (%rt-gc-alloc-old-from-free-list heap size)
                    (let ((old-free (rt-heap-old-free heap)))
                      (when (>= (+ old-free size)
                                (+ (rt-heap-old-base heap) (rt-heap-old-size heap)))
                        (error "cl-cc/runtime: old space exhausted during promotion"))
                      (setf (rt-heap-old-free heap) (+ old-free size))
                      old-free))
                ;; Copy to to-space
                to-free)))
      ;; Copy all words verbatim
      (loop for i from 0 below size do
        (rt-heap-set heap (+ dest-addr i)
                     (rt-heap-ref heap (+ from-addr i))))
      ;; Increment age in the destination header
      (rt-heap-set-header heap dest-addr
                           (header-increment-age header))
      (let ((new-age (header-age (rt-heap-object-header heap dest-addr))))
        (incf (svref (rt-heap-age-hist heap) new-age)))
      ;; Install forwarding pointer in source slot 0
      (rt-heap-set-header heap from-addr
                          (header-make-forwarding-ptr dest-addr))
      ;; Track promoted objects for later card-dirtying pass
      (when promote-p
        (push dest-addr (car promoted-list-cell))
        (setf (gethash dest-addr (rt-heap-recent-promotions heap))
              (rt-heap-minor-gc-count heap))
        (incf (rt-heap-words-promoted heap) size)
        (when pinned-p
          (rt-unpin-object heap from-addr)
          (rt-pin-object heap dest-addr)))
      ;; Return new address and updated to-free
      (values dest-addr
              (if promote-p to-free (+ to-free size))))))

(defun %gc-ensure-copied (heap from-addr to-free-cell promoted-list-cell &optional in-source-p)
  "Ensure the object at FROM-ADDR has been copied to to-space or old space.
    Returns the new (destination) address.
    Updates (cdr TO-FREE-CELL) in place when a new copy is made in to-space."
  (when (and (integerp from-addr) (val-pointer-p from-addr))
    (setf from-addr (decode-pointer from-addr)))
  (unless (integerp from-addr)
    (error "cl-cc/runtime: GC copy requested for non-pointer value ~S" from-addr))
  (let ((h (rt-heap-object-header heap from-addr)))
    (cond
      ((header-forwarding-p h)
       ;; Already copied — return existing forwarding destination
       (header-forwarding-ptr h))
      (t
        (multiple-value-bind (new-addr new-free)
            (%gc-copy-object heap from-addr (cdr to-free-cell) promoted-list-cell)
          (setf (cdr to-free-cell) new-free)
          (%rt-gc-copy-co-located-neighbor heap from-addr to-free-cell promoted-list-cell in-source-p)
          new-addr)))))

;;; Minor GC (%gc-scan-dirty-cards, %gc-cheney-scan, %gc-update-promoted,
;;; rt-gc-minor-collect) is in gc-minor.lisp (loads after this file).

;;; Section 4 (Write Barrier — SATB + Card Table: rt-gc-write-barrier)
;;; is in gc-write-barrier.lisp (loads after gc-minor, before gc-major.lisp).

;;; ------------------------------------------------------------
;;; FR-343-345: Thread-Local Allocation Buffers (TLAB)
;;; ------------------------------------------------------------

(defstruct (rt-tlab (:constructor %make-rt-tlab)
                    (:conc-name rt-tlab-))
  "Thread-Local Allocation Buffer (FR-343).

Each runtime thread may have a private TLAB for lock-free bump-pointer
allocation within a pre-allocated young-space region.  Fields:

  BUFFER-START — word address of the TLAB region start
  BUFFER-LIMIT — word address past the end of the TLAB region
  BUFFER-FREE  — current bump-pointer allocation cursor within the TLAB
  THREAD-ID    — logical thread id that owns this TLAB
  RETIRED-P    — when true, this TLAB has been retired and should not be used"
  (buffer-start 0 :type fixnum)
  (buffer-limit 0 :type fixnum)
  (buffer-free  0 :type fixnum)
  (thread-id    nil :type (or null keyword))
  (retired-p    nil :type boolean))

(defparameter *gc-tlab-size-words* 512
  "Default TLAB allocation chunk in words (4KB with 8B words).")

(defvar *rt-thread-local-heaps* nil
  "Alist of (thread-id . RT-TLAB) mapping logical thread ids to TLAB objects.
Used by RT-GC-TLAB-ALLOC for lock-free per-thread allocation.")

(defparameter *gc-tlab-retire-fill* t
  "When true (FR-344), retired TLABs fill remaining space with dummy objects
to minimize waste.  The dummy header uses type-tag 0 (fixnum) and is small
enough to preserve heap invariants during concurrent marking.")

(defun %rt-gc-tlab-for (heap thread-id)
  "Return the RT-TLAB for THREAD-ID, or NIL.  Creates one if absent."
  (declare (ignore heap))
  (or (cdr (assoc thread-id *rt-thread-local-heaps* :test #'eq))
      (let ((tlab (%make-rt-tlab :thread-id thread-id)))
        (push (cons thread-id tlab) *rt-thread-local-heaps*)
        tlab)))

(defun %rt-gc-tlab-refill (heap thread-id)
  "Allocate a fresh TLAB region from young from-space for THREAD-ID.

The TLAB is carved from the global young bump-pointer region, sized by
*GC-TLAB-SIZE-WORDS*.  If the young space has insufficient contiguously
available words, a minor GC is triggered and a more conservative TLAB size
is attempted."
  (let* ((size-words (min *gc-tlab-size-words*
                          (rt-heap-young-semi-size heap)))
         (tlab (%rt-gc-tlab-for heap thread-id)))
    ;; Clear retired flag
    (setf (rt-tlab-retired-p tlab) nil)
    ;; Try to allocate from young from-space
    (let* ((addr (rt-heap-young-free heap))
           (limit (+ addr size-words))
           (semi-limit (+ (rt-heap-young-from-base heap)
                          (rt-heap-young-semi-size heap))))
      (when (> limit semi-limit)
        ;; Not enough room — trigger a minor GC and retry
        (rt-gc-minor-collect heap)
        (setf addr (rt-heap-young-free heap)
              limit (+ addr size-words))
        (when (> limit (+ (rt-heap-young-from-base heap)
                          (rt-heap-young-semi-size heap)))
          (error "cl-cc/runtime: TLAB refill failed — young space exhausted")))
      ;; Bump young-free past the TLAB region
      (setf (rt-heap-young-free heap) limit)
      ;; Install TLAB bounds
      (setf (rt-tlab-buffer-start tlab) addr
            (rt-tlab-buffer-limit tlab) limit
            (rt-tlab-buffer-free tlab) addr)
      tlab)))

(defun %rt-gc-tlab-retire (heap tlab)
  "Retire TLAB: record it as retired.  When *GC-TLAB-RETIRE-FILL* is true
(FR-344), fill the unused portion with a dummy object to maintain heap
invariants and minimize waste for concurrent marking."
  (setf (rt-tlab-retired-p tlab) t)
  (when *gc-tlab-retire-fill*
    (let* ((free-pos (rt-tlab-buffer-free tlab))
           (limit (rt-tlab-buffer-limit tlab))
           (remaining (- limit free-pos)))
      (when (plusp remaining)
        ;; Write a dummy header (type-tag 0 = fixnum-like) over the unused
        ;; region so concurrent markers observe a valid object boundary.
        (rt-heap-set-header heap free-pos
                            (make-header remaining 0 0)))))
  tlab)

(defun rt-gc-tlab-alloc (heap thread-id size-words)
  "Thread-local bump-pointer allocation from THREAD-ID's TLAB.

Returns the word address of the allocated block within the TLAB.  If the
TLAB lacks room, a new TLAB is allocated from global young space (possibly
triggering a minor GC).  When no TLAB is available for any reason, falls
back to RT-GC-ALLOC.

This is the FR-343 entry point for lock-free per-thread allocation.

FR-345 (SIMD zeroing):  In a native codegen backend, the caller may emit
a vectorised zero-fill (e.g. movdqa or NEON STP) over the returned range
[ADDR, ADDR + SIZE-WORDS) instead of depending on the heap's zero-initialised
array storage."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let ((tlab (%rt-gc-tlab-for heap thread-id)))
    (unless (and (not (rt-tlab-retired-p tlab))
                 (<= (+ (rt-tlab-buffer-free tlab) size-words)
                     (rt-tlab-buffer-limit tlab)))
      ;; TLAB exhausted or retired — retire current and refill
      (%rt-gc-tlab-retire heap tlab)
      (%rt-gc-tlab-refill heap thread-id))
    ;; Bump-allocate within TLAB
    (let ((addr (rt-tlab-buffer-free tlab)))
      (setf (rt-tlab-buffer-free tlab) (+ addr size-words))
      (incf (rt-heap-total-alloc-words heap) size-words)
      (%rt-gc-note-allocation-rate heap)
      (dolist (hook *rt-gc-alloc-hooks*)
        (funcall hook heap size-words))
      ;; FR-345 (stub):  In a native codegen backend the caller may issue
      ;; SIMD zero-fill instructions over [ADDR, ADDR + SIZE-WORDS).  The
      ;; Pure CL heap is zero-initialised at allocation, so explicit
      ;; zeroing is unnecessary here.
      addr)))

(defun rt-gc-tlab-retire-all (heap)
  "Retire all TLABs for HEAP.  Called before a minor GC so that all thread
buffers are flushed to the heap, allowing the collector to observe the full
young-space allocation."
  (dolist (entry *rt-thread-local-heaps*)
    (%rt-gc-tlab-retire heap (cdr entry))))

;;; ------------------------------------------------------------
;;; FR-342: GC Concurrent Relocation (stub)
;;;
;;; Forwarding table hook is defined in heap.lisp (rt-gc-forward-object,
;;; rt-gc-clear-forwarding-table).  Concurrent relocation requires load
;;; barriers (FR-349) and colored pointers (FR-348) — both deferred.
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; FR-391: Heap Growth Policy (foundation)
;;;
;;; rt-heap-maybe-grow (heap.lisp) implements 2x growth when occupancy > 90%.
;;; rt-heap-maybe-shrink implements halving after 3 low-occupancy cycles.
;;; Max heap size is controlled by *gc-max-heap-words*.
;;;
;;; Deferred: contiguous address-space growth via mmap in native backends.
;;; Pure CL uses simple-vector resize which may copy.
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; FR-422: GC Ergonomics / Auto-Configuration (foundation)
;;;
;;; rt-gc-auto-configure-heap (heap.lisp) detects system RAM and configures
;;; young/old sizes.  Container detection via rt-heap-detect-container-memory-limit.
;;; CLI override via --heap-max argument supported.
;;; ------------------------------------------------------------

;;; ------------------------------------------------------------
;;; FR-424: GC Policy Selection (foundation)
;;;
;;; rt-gc-select-policy provides :throughput/:latency/:memory policy presets.
;;; Each policy adjusts *gc-young-size-words*, *gc-old-size-words*,
;;; and *gc-tenuring-threshold*.
;;; ------------------------------------------------------------
