(in-package :cl-cc/runtime)

;;; Precise safepoint and stack-map notes (FR-1115)
;;;
;;; The runtime treats a safepoint as a compiler- or VM-emitted poll where a
;;; mutator can safely service deferred work: pending GC, cooperative
;;; preemption, and async OS signals.  Native backends should emit polls at
;;; function entry, loop back-edges, allocation slow paths, and explicit foreign
;;; calls.  Each safepoint that can expose live heap references must have a
;;; stack map in *RT-GC-STACKMAP-TABLE* keyed by frame id.
;;;
;;; Stack maps are precise: every live object slot is listed as
;;; (FRAME-OFFSET . :OBJECT), while scalar slots (:FIXNUM, :DOUBLE, :CHAR, ...)
;;; are ignored by the collector.  JIT/AOT object files may store the same data
;;; in a read-only `.gc_map` section containing records:
;;;
;;;   u32 frame-id-delta | u16 slot-count | repeated sleb128 offset-delta + u8 kind
;;;
;;; Frame ids and offsets are delta-encoded to reduce code-size impact.  The
;;; decoder reconstructs monotonically increasing frame ids and slot offsets
;;; before registering RT-STACKMAP objects with RT-GC-REGISTER-STACKMAP.

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

(defun rt-gc-register-thread (&key (id *rt-current-thread-id*) stack frames state)
  "Register a logical mutator thread for cooperative safepoint coordination.

STATE may be an existing plist/thread descriptor.  When omitted, a descriptor
compatible with the existing GC root scanner is created from ID, STACK, and
FRAMES.  The descriptor is returned and retained in *GC-THREADS*."
  (let ((thread-state (or state (list :id id :name id :stack stack :frames frames))))
    (pushnew thread-state *gc-threads* :test #'equal)
    thread-state))

(defun rt-gc-unregister-thread (thread-or-id)
  "Remove THREAD-OR-ID from *GC-THREADS* and clear its safe-region depth."
  (let ((id (%rt-gc-thread-id thread-or-id)))
    (setf *gc-threads*
          (remove-if (lambda (thread-state)
                       (or (equal thread-state thread-or-id)
                           (equal (%rt-gc-thread-id thread-state) id)))
                     *gc-threads*))
    (remhash id *rt-gc-safe-region-depths*)
    id))

(defun rt-gc-request (heap &key (reason :explicit))
  "Request GC at the next safepoint for HEAP and return REASON."
  (declare (ignore reason))
  (setf *gc-pending* t)
  (when heap
    (setf (rt-heap-gc-pending heap) t))
  reason)

(defun rt-gc-preemption-request (&optional (pending t))
  "Set cooperative preemption request state observed by safepoints."
  (setf *rt-preemption-pending* pending))

(defun rt-gc-safepoint-check (heap &key (kind :poll) (thread-id *rt-current-thread-id*))
  "Poll a GC/preemption safepoint.

KIND documents the compiler/VM position: :FUNCTION-ENTRY, :LOOP-BACK-EDGE,
:ALLOCATION, or :POLL.  If a GC is pending while HEAP is inhibited (for example
inside WITHOUT-GCING), the request stays pending.  Otherwise a minor collection
is serviced after briefly marking THREAD-ID as safe."
  (declare (ignore kind))
  (when (fboundp 'rt-process-pending-signals)
    (rt-process-pending-signals))
  (when *rt-preemption-pending*
    (when *rt-preemption-yield-hook*
      (funcall *rt-preemption-yield-hook* thread-id))
    (setf *rt-preemption-pending* nil))
  (when (and heap (or *gc-pending* (rt-heap-gc-pending heap)))
    (if (rt-heap-gc-inhibit heap)
        (setf (rt-heap-gc-pending heap) t)
        (progn
          (rt-gc-enter-safe-region thread-id)
          (unwind-protect
               (progn
                 (setf *gc-pending* nil
                       (rt-heap-gc-pending heap) nil)
                 (rt-gc-minor-collect heap))
            (rt-gc-leave-safe-region thread-id)))))
  heap)

(defun rt-emit-gc-safepoint (&key heap (kind :jit) frame-id live-slots thread-id)
  "Register optional stack-map metadata and poll a JIT/native safepoint.

HEAP defaults to *RT-CURRENT-GC-HEAP*.  FRAME-ID and LIVE-SLOTS are optional
compiler metadata; when supplied, LIVE-SLOTS should be an alist of
(frame-offset . kind).  Returns HEAP after servicing pending GC/signals."
  (let ((target-heap (or heap *rt-current-gc-heap*)))
    (when frame-id
      (rt-gc-register-stackmap frame-id live-slots :source kind))
    (rt-gc-safepoint-check target-heap
                           :kind kind
                           :thread-id (or thread-id *rt-current-thread-id*))))

(defun rt-compress-stackmap-slots (slots)
  "Return delta-encoded stack-map SLOTS for compact `.gc_map` emission.

Input is an alist of (absolute-offset . kind).  Output is an alist of
(offset-delta . kind) sorted by absolute offset."
  (let ((previous 0)
        (sorted (sort (copy-list slots) #'< :key #'car)))
    (loop for (offset . kind) in sorted
          for delta = (- offset previous)
          do (setf previous offset)
          collect (cons delta kind))))

(defun rt-decompress-stackmap-slots (compressed)
  "Inflate delta-encoded stack-map slots produced by RT-COMPRESS-STACKMAP-SLOTS."
  (let ((offset 0))
    (loop for (delta . kind) in compressed
          do (incf offset delta)
          collect (cons offset kind))))

(defun rt-gc-map-section-documentation ()
  "Return documentation for the native `.gc_map` section format."
  "`.gc_map` records are delta encoded: u32 frame-id-delta, u16 slot-count, then slot-count repetitions of sleb128 frame-offset-delta plus u8 slot-kind. Slot kind 1 denotes :OBJECT; scalar kinds are ignored by GC. The loader expands records and calls RT-GC-REGISTER-STACKMAP before code execution.")

(defmacro with-gc-function-entry-safepoint ((heap &optional (thread-id '*rt-current-thread-id*)) &body body)
  "Run BODY after polling a function-entry safepoint."
  `(progn
     (rt-gc-safepoint-check ,heap :kind :function-entry :thread-id ,thread-id)
     ,@body))

(defmacro with-gc-loop-backedge-safepoint ((heap &optional (thread-id '*rt-current-thread-id*)) &body body)
  "Run BODY after polling a loop-back-edge safepoint."
  `(progn
     (rt-gc-safepoint-check ,heap :kind :loop-back-edge :thread-id ,thread-id)
     ,@body))

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
