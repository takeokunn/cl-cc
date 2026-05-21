(in-package :cl-cc/runtime)

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
          ;; Header age is a 2-bit saturating field (0..3).  Dynamic tenure may
          ;; temporarily raise the threshold above that representable maximum; in
          ;; that case an age-saturated object must still be promotable.
          (effective-threshold (min 3 *gc-tenuring-threshold*))
          (promote-p (or pinned-p (>= age effective-threshold))))
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

;;; FR-343: Thread-Local Allocation Buffers (TLAB)
(defstruct (rt-tlab (:constructor %make-rt-tlab)
                    (:conc-name rt-tlab-))
  "Per-thread allocation buffer for lock-free young-space allocation.

BASE and LIMIT delimit the reserved nursery slice, FREE is the current bump
cursor, THREAD-ID is a stable fixnum derived from the logical runtime thread id,
and WASTE-BYTES accumulates retired unused capacity for FR-344 accounting."
  (base 0 :type fixnum)
  (free 0 :type fixnum)
  (limit 0 :type fixnum)
  (thread-id 0 :type fixnum)
  (waste-bytes 0 :type fixnum)
  (retired-p nil :type boolean))

(defun make-rt-tlab (&key (base 0) (free base) (limit base) (thread-id 0) (waste-bytes 0) retired-p)
  "Construct an RT-TLAB using the public FR-343 field names."
  (%make-rt-tlab :base base
                 :free free
                 :limit limit
                 :thread-id thread-id
                 :waste-bytes waste-bytes
                 :retired-p retired-p))

(defparameter *gc-tlab-size-words* 512
  "Default TLAB allocation chunk in words (4KB with 8B words).")

(defvar *rt-thread-local-heaps* nil
  "List of (HEAP THREAD-ID . RT-TLAB) entries mapping heaps/threads to TLABs.
Used by RT-GC-TLAB-ALLOC for lock-free per-thread allocation.")

(defun %rt-gc-tlab-sb-thread-function (name)
  "Return the SB-THREAD function named NAME, or NIL when unavailable."
  (ignore-errors
    (let ((package (find-package "SB-THREAD")))
      (when package
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (fboundp symbol))
            (symbol-function symbol)))))))

(defun %rt-gc-tlab-make-mutex (name)
  "Create an optional host mutex without reader-time SB-THREAD dependency."
  (let ((make-mutex (%rt-gc-tlab-sb-thread-function "MAKE-MUTEX")))
    (and make-mutex (ignore-errors (funcall make-mutex :name name)))))

(defvar *rt-gc-tlab-refill-lock* (%rt-gc-tlab-make-mutex "gc-tlab-refill")
  "Optional mutex serializing global nursery bump-pointer reservation for TLAB refill.")

(defmacro %rt-gc-with-tlab-refill-lock (() &body body)
  (let ((fn (gensym "CALL-WITH-MUTEX"))
        (lock (gensym "LOCK")))
    `(let ((,lock *rt-gc-tlab-refill-lock*))
       (if ,lock
           (let ((,fn (%rt-gc-tlab-sb-thread-function "CALL-WITH-MUTEX")))
             (if ,fn
                 (funcall ,fn (lambda () ,@body) ,lock)
                 (progn ,@body)))
           (progn ,@body)))))

(defparameter *gc-tlab-retire-fill* t
  "When true (FR-344), retired TLABs fill remaining space with dummy objects
to minimize waste.  The dummy header uses type-tag 0 (fixnum) and is small
enough to preserve heap invariants during concurrent marking.")

(defun %rt-gc-tlab-thread-id (thread-id)
  "Return a portable fixnum id for THREAD-ID suitable for RT-TLAB metadata."
  (logand (sxhash thread-id) most-positive-fixnum))

(defun %rt-gc-tlab-for (heap thread-id)
  "Return the RT-TLAB for THREAD-ID, or NIL.  Creates one if absent."
  (or (loop for entry in *rt-thread-local-heaps*
            when (and (eq (first entry) heap)
                      (eql (second entry) thread-id))
              return (cddr entry))
      (let ((tlab (%make-rt-tlab :thread-id (%rt-gc-tlab-thread-id thread-id))))
        (push (cons heap (cons thread-id tlab)) *rt-thread-local-heaps*)
        tlab)))

(defun %rt-gc-tlab-refill (heap thread-id &optional (minimum-words 1))
  "Allocate a fresh TLAB region from young from-space for THREAD-ID.

The TLAB is carved from the global young bump-pointer region, sized by
*GC-TLAB-SIZE-WORDS*.  If the young space has insufficient contiguously
available words, a minor GC is triggered and a more conservative TLAB size
is attempted."
  (let* ((size-words (min (max *gc-tlab-size-words* minimum-words)
                          (rt-heap-young-semi-size heap)))
         (tlab (%rt-gc-tlab-for heap thread-id)))
    (%rt-gc-with-tlab-refill-lock ()
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
        (setf (rt-tlab-base tlab) addr
              (rt-tlab-limit tlab) limit
              (rt-tlab-free tlab) addr)
        tlab))))

(defun %rt-gc-tlab-retire (heap tlab)
  "Retire TLAB: record it as retired.  When *GC-TLAB-RETIRE-FILL* is true
(FR-344), fill the unused portion with a dummy object to maintain heap
invariants and minimize waste for concurrent marking."
  (setf (rt-tlab-retired-p tlab) t)
  (when *gc-tlab-retire-fill*
    (let* ((free-pos (rt-tlab-free tlab))
           (limit (rt-tlab-limit tlab))
           (remaining (- limit free-pos)))
      (when (plusp remaining)
        (incf (rt-tlab-waste-bytes tlab) (* remaining 8))
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
                 (<= (+ (rt-tlab-free tlab) size-words)
                     (rt-tlab-limit tlab)))
      ;; TLAB exhausted or retired — retire current and refill
      (%rt-gc-tlab-retire heap tlab)
      (%rt-gc-tlab-refill heap thread-id size-words))
    ;; Bump-allocate within TLAB
    (let ((addr (rt-tlab-free tlab)))
      (setf (rt-tlab-free tlab) (+ addr size-words))
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
    (when (eq (first entry) heap)
      (%rt-gc-tlab-retire heap (cddr entry)))))

;;; ------------------------------------------------------------
;;; TODO(roadmap, FR-342): GC Concurrent Relocation (not implemented)
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

;;; ------------------------------------------------------------
;;; TLAB Public API aliases (FR-550)
;;; ------------------------------------------------------------
(defun rt-tlab-alloc (heap thread-id size-words)
  "Public alias for rt-gc-tlab-alloc (FR-550)."
  (rt-gc-tlab-alloc heap thread-id size-words))

(defun rt-tlab-retire (heap thread-id)
  "Public alias: retire the TLAB for THREAD-ID (FR-550)."
  (let ((tlab (rt-tlab-retire-all heap)))
    (declare (ignore thread-id))
    tlab))
