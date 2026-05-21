(in-package :cl-cc/runtime)

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
      (if (fboundp 'rt-signal-oom)
          (rt-signal-oom requested-words
                         :heap heap
                         :limit-words *gc-max-heap-words*
                         :used-words (%rt-heap-live-used-words heap))
          (error "cl-cc/runtime: GC heap limit exceeded — ~D live/requested words exceed limit ~D"
                 (+ (%rt-heap-live-used-words heap) requested-words)
                 *gc-max-heap-words*)))))

(defun %rt-gc-check-pressure (heap)
  (let* ((occupancy (rt-heap-occupancy-pct heap))
         (level (cond
                  ((>= occupancy (rt-heap-pressure-threshold-high heap)) :high)
                  ((<= occupancy (rt-heap-pressure-threshold-low heap)) :low)
                  (t nil))))
    (when level
      (when (and (fboundp 'rt-check-heap-pressure-thresholds)
                 (member level '(:high) :test #'eq))
        (rt-check-heap-pressure-thresholds heap))
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
     (setf *gc-tenuring-threshold* (min 3 (1+ *gc-tenuring-threshold*)))))
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
;;; FR-425: GC Pacer / Allocation Pacing
;;; FR-427: Allocation Back-Pressure
;;; ------------------------------------------------------------

(defparameter *gc-pacer-enabled* nil
  "When true, allocation rate is throttled to let GC catch up (FR-425).")

(defparameter *gc-allocation-rate-limit-words-per-sec* 0
  "Target allocation rate ceiling in words/sec for GC pacer (FR-425).
When 0, no allocation pacing is applied.")

(defparameter *gc-back-pressure-threshold* 0.85d0
  "Heap occupancy fraction above which allocation back-pressure triggers (FR-427).
When occupancy exceeds this threshold, GC is requested more aggressively and
allocation hooks may throttle or trigger synchronous collection.")

(defun rt-gc-pacer-maybe-throttle (heap size-words)
  "FR-425/427: Optionally delay allocation to allow GC to catch up.

When *gc-pacer-enabled* and allocation-rate-limit is set, this function
waits proportionally to SIZE-WORDS.  When heap occupancy exceeds
*gc-back-pressure-threshold*, synchronous GC is triggered before
returning to reduce fragmentation risk."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (when (and *gc-pacer-enabled*
             (plusp *gc-allocation-rate-limit-words-per-sec*))
    (sleep (/ (float size-words) (float *gc-allocation-rate-limit-words-per-sec*))))
  (when (> (rt-heap-occupancy-pct heap) (* 100.0d0 *gc-back-pressure-threshold*))
    (rt-gc-minor-collect heap))
  heap)
