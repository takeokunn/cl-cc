;;;; packages/runtime/src/heap.lisp - CL-CC Managed Heap for Generational GC
;;;
;;; This file provides the managed heap data structure for a 2-generation
;;; Generational GC:
;;;   - Young (nursery) split into 2 semi-spaces (from-space + to-space)
;;;     for Cheney copying collection.
;;;   - Old generation for tenured objects with a card table for tracking
;;;     old-to-young pointer references.
;;;
;;; Memory layout (flat simple-vector):
;;;   [0 .. semi-size-1]                  = from-space (young)
;;;   [semi-size .. 2*semi-size-1]         = to-space (young)
;;;   [2*semi-size .. 2*semi-size+old-1]   = old space
;;;
;;; Object header word bit layout (non-negative integer):
;;;   bit 63..32 = size in words (including header)
;;;   bit 31..24 = type tag (0..7, matching +tag-* constants in runtime.lisp)
;;;   bit 23..20 = age (0-15, minor GC survival cycles)
;;;   bit 19     = mark bit (set during major GC marking)
;;;   bit 18     = gray bit (SATB marking queue membership)
;;;   bit 17     = forwarding flag (Cheney copy; when set, header encodes dest addr)
;;;   bit 16..0  = reserved / forwarding address (when forwarding flag is set)

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Configuration Parameters
;;; ------------------------------------------------------------

(defparameter *gc-young-size-words* (* 128 1024)
  "Young space in 64-bit words (1MB total, 512KB per semi-space).")

(defparameter *gc-old-size-words* (* 512 1024)
  "Old space in 64-bit words (4MB).")

(defparameter *gc-max-heap-words* 0
  "Maximum live GC heap words before allocation must force a major GC.
   A value of 0 means unlimited.")

(defparameter *gc-throughput-target* 0.05d0
  "Target fraction of wall-clock time spent in GC pauses.
   The default 0.05 means GC should consume at most 5% of runtime.")

(defparameter *rt-numa-enabled* nil
  "When true, route allocation/GC scheduling through portable NUMA hooks.

The Pure CL runtime exposes the interface but has no OS NUMA bindings; every
thread maps to node 0. Native Linux builds can back this with libnuma
numa_alloc_local(3), mbind(2), and scheduler affinity. Windows builds can use
VirtualAllocExNuma plus processor-group affinity APIs.")

(defparameter *rt-stack-guard-enabled* t
  "When true, native VM stack setup should reserve a guard page at stack end.

In the Pure CL runtime this is a documented portable interface: host-specific
launchers may use mmap/mprotect (or equivalent) to mark the final stack page as
inaccessible and install a SIGSEGV/SIGBUS handler that reports stack overflow
instead of allowing adjacent memory corruption.  Without platform support the
runtime falls back to returning guard metadata from RT-INSTALL-STACK-GUARD.")

(defconstant +rt-default-system-memory-bytes+ (* 512 1024 1024)
  "Portable fallback for host memory detection (512MiB).")

(defun rt-heap-detect-container-memory-limit ()
  "Return the cgroup memory limit in bytes if running in a container,
    or NIL if not containerized or limit cannot be determined.
   
    Checks cgroup v2 first (/sys/fs/cgroup/memory.max), then v1
    (/sys/fs/cgroup/memory/memory.limit_in_bytes).
    Returns NIL on failure (non-container environment or permission error)."
  ;; FR-423: Container-Aware Heap Sizing — reads cgroup v1/v2 memory limits for Docker/Kubernetes compatibility
  (or (ignore-errors
        (with-open-file (f "/sys/fs/cgroup/memory.max"
                           :direction :input
                           :if-does-not-exist nil)
          (when f
            (let ((line (read-line f nil nil)))
              (when line
                (let ((limit (parse-integer line :junk-allowed t)))
                  (unless (string= line "max")
                    limit)))))))
      (ignore-errors
        (with-open-file (f "/sys/fs/cgroup/memory/memory.limit_in_bytes"
                           :direction :input
                           :if-does-not-exist nil)
          (when f
            (let ((line (read-line f nil nil)))
              (when line
                (let ((limit (parse-integer line :junk-allowed t)))
                  ;; A very large value means "no limit"
                  (unless (> limit (expt 2 50))
                    limit)))))))))

(defun %rt-detect-system-memory-bytes ()
  "Return best-effort system memory in bytes using portable Common Lisp only."
  +rt-default-system-memory-bytes+)

(defun %rt-clamp (value min-value max-value)
  (min max-value (max min-value value)))

(defun rt-gc-auto-configure-heap (&key memory-bytes)
  "Auto-configure *GC-YOUNG-SIZE-WORDS* and *GC-OLD-SIZE-WORDS*.

When a container memory limit is detected, the ergonomic budget is capped at
75% of that limit so the runtime leaves headroom for code, stacks, native
allocations, and host implementation overhead.  Explicit MEMORY-BYTES remains
an upper input but is also bounded by the container cap when present."
  (let* ((container-limit (rt-heap-detect-container-memory-limit))
         (container-budget (and container-limit (floor (* container-limit 3) 4)))
         (detected-bytes (or memory-bytes (%rt-detect-system-memory-bytes)))
         (available-bytes (if container-budget
                              (min detected-bytes container-budget)
                              detected-bytes))
         (young-bytes (%rt-clamp (floor available-bytes 128)
                                  (* 1 1024 1024)
                                  (* 4 1024 1024)))
         (old-bytes (%rt-clamp (floor available-bytes 32)
                               (* 4 1024 1024)
                               (* 16 1024 1024)))
         (young-words (max 2 (floor young-bytes 8)))
         (old-words (max 1 (floor old-bytes 8))))
    (when (oddp young-words)
      (incf young-words))
    (setf *gc-young-size-words* young-words
          *gc-old-size-words* old-words)
    (list :memory-bytes available-bytes
           :container-limit-bytes container-limit
           :container-heap-cap-bytes container-budget
            :young-size-words young-words
            :old-size-words old-words)))

(defparameter *rt-heap-randomize* nil
  "When T, apply logical address-space randomization to heap bases (FR-373).
   Default NIL for deterministic test behaviour; set to T for production.")

(defun rt-heap-randomize-base ()
  "Return a portable 20-bit randomized heap base offset in words, or 0 when disabled.
FR-373: The managed heap is a simple-vector in Pure CL, so the offset is applied
to logical heap addresses rather than to an OS virtual-memory mapping.  Native
launchers can combine this logical ASLR with mmap-based placement."
  (if *rt-heap-randomize*
      (random (ash 1 20))
      0))

(defun rt-install-stack-guard (stack-base stack-size)
  "Document and configure the VM stack guard-page interface.

STACK-BASE and STACK-SIZE describe the VM stack address range.  On native
targets the VM should place an inaccessible guard page at the stack end using a
platform mechanism such as mmap/mprotect and install a SIGSEGV handler that
recognizes faults within that page as stack overflows.  This Pure CL fallback
does not change host memory protection; it returns a plist describing the guard
region so platform-specific startup code can enforce it."
  (check-type stack-base integer)
  (check-type stack-size integer)
  (let* ((page-size 4096)
         (guard-size (if *rt-stack-guard-enabled* page-size 0))
         (guard-base (max stack-base (- (+ stack-base stack-size) guard-size))))
    (list :enabled *rt-stack-guard-enabled*
          :stack-base stack-base
          :stack-size stack-size
          :guard-base guard-base
          :guard-size guard-size
          :fault-signal :sigsegv
          :portable-fallback t)))

;;; ------------------------------------------------------------
;;; Sanitizer runtime toggles (FR-489..493 minimal runtime path)
;;; ------------------------------------------------------------

(defparameter *rt-asan-enabled* nil
  "When true, rt-heap-ref/set perform ASan-like poisoned-address checks.")

(defparameter *rt-msan-enabled* nil
  "When true, rt-heap-ref reports reads from uninitialized words.")

(defparameter *rt-tsan-enabled* nil
  "When true, rt-heap-set/ref perform a conservative race check using access history.")

(defparameter *rt-hwasan-enabled* nil
  "When true, rt-heap-set/ref validate heap word tags.")

(defparameter *rt-ubsan-enabled* nil
  "When true, rt-heap-set/ref perform basic undefined-behavior contract checks.")

(defparameter *rt-heap-poison-map* (make-hash-table :test #'eql)
  "Address -> T when poisoned (ASan/HWASan-like redzone marker).")

(defparameter *rt-heap-init-map* (make-hash-table :test #'eql)
  "Address -> T when initialized (MSan-like state).")

(defparameter *rt-heap-tag-map* (make-hash-table :test #'eql)
  "Address -> 4-bit heap tag (HWASan-like metadata).")

(defparameter *rt-heap-access-map* (make-hash-table :test #'eql)
  "Address -> (thread-id . mode) access history used by conservative TSan checks.")

(defparameter *rt-tsan-thread-id* 0
  "Current logical thread id used by TSan checks in this runtime simulation.")

(defun rt-sanitizer-reset-state ()
  "Reset sanitizer bookkeeping maps to empty state."
  (clrhash *rt-heap-poison-map*)
  (clrhash *rt-heap-init-map*)
  (clrhash *rt-heap-tag-map*)
  (clrhash *rt-heap-access-map*)
  t)

(defun rt-sanitizer-poison-address (index)
  "Mark INDEX as poisoned for ASan/HWASan checks."
  (setf (gethash index *rt-heap-poison-map*) t)
  t)

(defun rt-sanitizer-unpoison-address (index)
  "Clear poison marker from INDEX."
  (remhash index *rt-heap-poison-map*)
  t)

(defun rt-sanitizer-set-address-tag (index tag)
  "Set 4-bit HWASan tag for INDEX." 
  (setf (gethash index *rt-heap-tag-map*) (logand tag #xF))
  t)

(defun %rt-asan-check-address (heap index op)
  (let ((limit (length (rt-heap-words heap))))
    (when (or (< index 0) (>= index limit))
      (error "ASan: ~A out-of-bounds heap access at ~D (limit ~D)" op index limit))
    (when (gethash index *rt-heap-poison-map*)
      (error "ASan: ~A on poisoned heap address ~D" op index))))

(defun %rt-msan-check-read (index)
  (when (and *rt-msan-enabled* (null (gethash index *rt-heap-init-map*)))
    (error "MSan: read from uninitialized heap address ~D" index)))

(defun %rt-tsan-check-access (index mode)
  (when *rt-tsan-enabled*
    (let ((prev (gethash index *rt-heap-access-map*)))
      (when (and prev
                 (/= (car prev) *rt-tsan-thread-id*)
                 (or (eq mode :write) (eq (cdr prev) :write)))
        (error "TSan: data race at heap address ~D between thread ~D (~A) and ~D (~A)"
               index (car prev) (cdr prev) *rt-tsan-thread-id* mode))
      (setf (gethash index *rt-heap-access-map*) (cons *rt-tsan-thread-id* mode)))))

(defun %rt-hwasan-check-address (index expected-tag)
  (when *rt-hwasan-enabled*
    (let ((actual-tag (gethash index *rt-heap-tag-map* 0)))
      (unless (= (logand expected-tag #xF) actual-tag)
        (error "HWASan: tag mismatch at heap address ~D (expected ~D, actual ~D)"
               index (logand expected-tag #xF) actual-tag)))))

(defun %rt-ubsan-check-access (heap index op)
  (when *rt-ubsan-enabled*
    (unless (integerp index)
      (error "UBSan: ~A requires integer heap index, got ~S" op index))
    (when (< index 0)
      (error "UBSan: ~A uses negative heap index ~D" op index))
    (when (>= index (length (rt-heap-words heap)))
      (error "UBSan: ~A out-of-bounds heap index ~D" op index))))

(defparameter *gc-tenuring-threshold* 3
  "Minor GC survival cycles before promotion to old generation.")

(defconstant +gc-card-summary-block-size+ 64
  "Number of cards represented by one card-summary entry.")

(defconstant +gc-card-size-words+ 64
  "Card size in words (512 bytes with 8-byte words).")

(defconstant +rt-free-list-bin-count+ 16
  "Number of segregated old-space free-list bins.")

(defconstant +rt-slab-page-words+ 256
  "Default slab page size in heap words for fixed-size object classes.")

(defparameter *rt-slab-size-classes*
  '((:cons . 3)
    (:symbol . 4)
    (:closure-small . 2)
    (:closure-4 . 4)
    (:array-min . 4))
  "Named fixed-size slab classes used by the runtime allocator.")

(defstruct (rt-slab (:constructor %make-rt-slab)
                    (:conc-name rt-slab-))
  "Fixed-size slab page metadata.  FREE-LIST contains available object addresses."
  (class-size 0 :type fixnum)
  (free-list nil :type list)
  (slab-base 0 :type fixnum)
  (slab-limit 0 :type fixnum))

;;; ------------------------------------------------------------
;;; Object Header Bit Layout Constants
;;; ------------------------------------------------------------

;;; Size field: bits 63..32
(defconstant +header-size-shift+    32)
(defconstant +header-size-mask+     #xFFFFFFFF00000000)

;;; Type tag field: bits 31..24
(defconstant +header-tag-shift+     24)
(defconstant +header-tag-mask+      #x00000000FF000000)

;;; Age field: bits 23..20
(defconstant +header-age-shift+     20)
(defconstant +header-age-mask+      #x0000000000F00000)

;;; Flag bits (bits 19 and 18 for mark/gray; used only on integer headers)
(defconstant +header-mark-bit+      #x0000000000080000)  ; bit 19
(defconstant +header-gray-bit+      #x0000000000040000)  ; bit 18

;;; Forwarding pointers are represented as (cons :forwarded dest-addr)
;;; rather than bit-packed integers.  This avoids address-size limitations
;;; and is idiomatic for a Pure CL heap simulation.

;;; ------------------------------------------------------------
;;; Header Construction and Accessors
;;; ------------------------------------------------------------

(defun make-header (size type-tag &optional (age 0))
  "Construct a header word encoding SIZE, TYPE-TAG, and AGE.
   SIZE is the object size in words (including header).
   TYPE-TAG is 0-7 matching +tag-* constants.
   AGE is 0-15 (minor GC survival count)."
  (logior (ash size +header-size-shift+)
          (ash (logand type-tag #xFF) +header-tag-shift+)
          (ash (logand age #xF) +header-age-shift+)))

(defun header-size (h)
  "Extract the object size in words from header word H."
  (ash h (- +header-size-shift+)))

(defun header-tag (h)
  "Extract the type tag (0-7) from header word H."
  (logand (ash h (- +header-tag-shift+)) #xFF))

(defun header-age (h)
  "Extract the age (0-15) from header word H."
  (logand (ash h (- +header-age-shift+)) #xF))

(defun header-marked-p (h)
  "Return true if the mark bit is set in header word H."
  (not (zerop (logand h +header-mark-bit+))))

(defun header-gray-p (h)
  "Return true if the gray bit is set in header word H."
  (not (zerop (logand h +header-gray-bit+))))

(defun header-forwarding-p (h)
  "Return true if H represents a forwarding pointer (i.e. is a cons :forwarded)."
  (and (consp h) (eq (car h) :forwarded)))

(defun header-set-mark (h)
  "Return a new header with the mark bit set."
  (logior h +header-mark-bit+))

(defun header-clear-mark (h)
  "Return a new header with the mark bit cleared."
  (logand h (lognot +header-mark-bit+)))

(defun header-set-gray (h)
  "Return a new header with the gray bit set."
  (logior h +header-gray-bit+))

(defun header-clear-gray (h)
  "Return a new header with the gray bit cleared."
  (logand h (lognot +header-gray-bit+)))

(defun header-make-forwarding-ptr (dest-addr)
  "Create a forwarding pointer value for DEST-ADDR.
   This value is stored in slot 0 of the from-space object.
   Use header-forwarding-p to detect it and header-forwarding-ptr to extract."
  (cons :forwarded dest-addr))

(defun header-forwarding-ptr (h)
  "Extract the forwarding destination address from a forwarding pointer H.
   Only valid when (header-forwarding-p h) is true."
  (cdr h))

(defun header-increment-age (h)
  "Return a new header with age incremented by 1, capped at 15."
  (let* ((current-age (header-age h))
         (new-age (min 15 (1+ current-age))))
    (logior (logand h (lognot +header-age-mask+))
            (ash new-age +header-age-shift+))))

;;; ------------------------------------------------------------
;;; rt-heap Structure
;;; ------------------------------------------------------------

(defstruct (rt-heap (:constructor %make-rt-heap)
                    (:conc-name rt-heap-))
  (words           nil  :type simple-vector)
  ;;; FR-087: rt-heap Field Reordering — young-free co-located with young-from-base/young-to-base/young-semi-size in same cache line
  (young-from-base 0    :type fixnum)
  (young-to-base   0    :type fixnum)
  (young-semi-size 0    :type fixnum)
  (young-free      0    :type fixnum)
  (old-base        0    :type fixnum)
  (old-size        0    :type fixnum)
  (old-free        0    :type fixnum)
  (minor-gc-count  0    :type fixnum)
  (major-gc-count  0    :type fixnum)
  (words-collected 0    :type fixnum)
  (words-promoted  0    :type fixnum)
  (card-table      nil  :type (simple-array (unsigned-byte 8) (*)))
  (card-summary    nil  :type simple-vector)
  (num-cards       0    :type fixnum)
  (roots           nil  :type list)
  (satb-queue      nil  :type list)
  (barrier-buffer  nil  :type list)
  (free-list       nil  :type list)
  (free-bins       nil  :type simple-vector)
  (slab-pools      nil  :type hash-table)
  (lazy-sweep-cursor 0  :type fixnum)
  (lazy-sweep-limit 0   :type fixnum)
  (incremental-work-budget 64 :type fixnum)
  (pause-exceeded-count 0 :type fixnum)
  (access-bits     nil  :type hash-table)
  (recent-promotions nil :type hash-table)
  (gc-state        :normal :type keyword)
  (total-alloc-words 0  :type fixnum)
  (age-hist        nil  :type simple-vector)
  ;;; FR-086: Large Object Space (LOS) — objects exceeding threshold bypass nursery; allocated directly in large-object space
  (large-obj-threshold 8192 :type fixnum)
  (large-obj-base  0    :type fixnum)
  (large-obj-size  0    :type fixnum)
  (large-obj-free  0    :type fixnum)
  (gc-pause-total  0.0d0 :type double-float)
  (gc-pause-max    0.0d0 :type double-float)
  (gc-wall-start-tick 0 :type integer)
  (allocation-rate-words-per-sec 0.0d0 :type double-float)
  (allocation-rate-last-tick 0 :type integer)
  (allocation-rate-last-total 0 :type integer)
  (pressure-hooks  nil  :type list)
  (pressure-threshold-high 80.0d0 :type double-float)
  (pressure-threshold-low 20.0d0 :type double-float)
  (max-heap-words  0    :type fixnum)
  (initial-heap-words 0 :type fixnum)
  (initial-old-size 0 :type fixnum)
  (initial-large-obj-size 0 :type fixnum)
  (shrink-threshold 0.25d0 :type double-float)
  (shrink-counter 0 :type fixnum)
  (compaction-trigger-fraction 0.5d0 :type double-float)
  (forwarding-table nil :type hash-table)
  (numa-node-map nil :type hash-table)
  (numa-gc-schedule nil :type list)
  (interleaved-regions nil :type list)
  (co-location-hints nil :type hash-table)
  (gc-inhibit      nil  :type boolean)
  (gc-pending      nil  :type boolean))

;;; ------------------------------------------------------------
;;; make-rt-heap Factory
;;; ------------------------------------------------------------

(defun make-rt-heap (&key (young-size *gc-young-size-words* young-size-p)
                          (old-size   *gc-old-size-words* old-size-p))
  "Allocate and initialize a fresh managed heap.

   YOUNG-SIZE is the total young generation size in words; it is split
   evenly into two semi-spaces of YOUNG-SIZE/2 words each.
   OLD-SIZE is the old generation size in words.

   Layout of the flat word vector:
     [0 .. semi-size-1]                from-space
     [semi-size .. 2*semi-size-1]      to-space
     [2*semi-size .. 2*semi-size+old-1] old space"
  (unless (or young-size-p old-size-p)
    (rt-gc-auto-configure-heap)
    (setf young-size *gc-young-size-words*
          old-size *gc-old-size-words*))
   (let* ((semi-size  (floor young-size 2))
          (large-obj-size old-size)
          (heap-base (rt-heap-randomize-base))
          (young-from-base heap-base)
          (young-to-base (+ young-from-base semi-size))
          (old-base   (+ young-to-base semi-size))
          (large-obj-base (+ old-base old-size))
          (total-size (+ heap-base (* 2 semi-size) old-size large-obj-size))
          (words      (make-array total-size :initial-element 0))
          (num-cards  (ceiling old-size +gc-card-size-words+))
         (card-table (make-array num-cards
                                  :element-type '(unsigned-byte 8)
                                  :initial-element 0))
         (card-summary (make-array (ceiling num-cards +gc-card-summary-block-size+)
                                   :initial-element nil))
           (age-hist (make-array 16 :initial-element 0))
           (free-bins (make-array +rt-free-list-bin-count+ :initial-element nil))
           (slab-pools (make-hash-table :test #'eql))
           (now (get-internal-real-time)))
    (%make-rt-heap
      :words           words
      :young-from-base young-from-base
      :young-to-base   young-to-base
      :young-semi-size semi-size
      :young-free      young-from-base
      :old-base        old-base
     :old-size        old-size
     :old-free        old-base
     :minor-gc-count  0
     :major-gc-count  0
     :words-collected 0
      :words-promoted  0
      :card-table      card-table
      :card-summary    card-summary
      :num-cards       num-cards
      :roots           nil
      :satb-queue      nil
      :barrier-buffer  nil
      :free-list       nil
      :free-bins       free-bins
      :slab-pools      slab-pools
      :lazy-sweep-cursor old-base
      :lazy-sweep-limit old-base
      :incremental-work-budget 64
      :pause-exceeded-count 0
        :access-bits     (make-hash-table :test #'eql)
       :recent-promotions (make-hash-table :test #'eql)
       :gc-state        :normal
      :total-alloc-words 0
      :age-hist        age-hist
      :large-obj-threshold 8192
      :large-obj-base  large-obj-base
      :large-obj-size  large-obj-size
       :large-obj-free  large-obj-base
       :gc-pause-total  0.0d0
       :gc-pause-max    0.0d0
       :gc-wall-start-tick now
       :allocation-rate-words-per-sec 0.0d0
       :allocation-rate-last-tick now
       :allocation-rate-last-total 0
       :pressure-hooks  nil
       :pressure-threshold-high 80.0d0
       :pressure-threshold-low 20.0d0
       :max-heap-words  total-size
       :initial-heap-words total-size
       :initial-old-size old-size
       :initial-large-obj-size large-obj-size
        :shrink-threshold 0.25d0
        :shrink-counter 0
        :compaction-trigger-fraction 0.5d0
        :forwarding-table (make-hash-table :test #'eql)
        :numa-node-map (make-hash-table :test #'eql)
        :numa-gc-schedule nil
        :interleaved-regions nil
        :co-location-hints (make-hash-table :test #'eql)
        :gc-inhibit      nil
        :gc-pending      nil)))

;;; ------------------------------------------------------------
;;; Heap Word Access
;;; ------------------------------------------------------------

(defun rt-gc-forward-object (heap old-addr new-addr)
  "Record an OLD-ADDR -> NEW-ADDR forwarding entry for concurrent relocation.

This is the public FR-342 forwarding table hook used by future compactors.  The
Pure CL implementation does not move payload words concurrently yet; it records
the relocation mapping and relies on RT-HEAP-REF's relocation barrier to heal
stale slots lazily as mutators read them."
  (check-type heap rt-heap)
  (check-type old-addr integer)
  (check-type new-addr integer)
  (setf (gethash old-addr (rt-heap-forwarding-table heap)) new-addr)
  new-addr)

(defun rt-gc-clear-forwarding-table (heap)
  "Clear all transient concurrent-relocation forwarding entries for HEAP."
  (check-type heap rt-heap)
  (clrhash (rt-heap-forwarding-table heap))
  heap)

;;; ------------------------------------------------------------
;;; FR-288: Large Page Support (⏸️ deferred)
;;;   Requires OS-specific mmap flags (MAP_HUGETLB, MADV_HUGEPAGE).
;;;   Pure CL stub: *rt-heap-hugepage-enabled* toggle exists but
;;;   has no effect without native backend support.
;;;
;;; FR-363: NUMA-Aware Heap Allocation (⚠️ stub)
;;;   Interface: *rt-numa-enabled*, rt-numa-local-alloc.
;;;   Requires libnuma on Linux or VirtualAllocExNuma on Windows.
;;;
;;; FR-364: NUMA-Local GC (⚠️ stub)
;;;   Interface: rt-gc-numa-affinity.
;;;   Requires sched_getcpu and CPU topology access.
;;;
;;; FR-365: Memory Interleaving (⚠️ stub)
;;;   Interface: rt-heap-interleave.
;;;   Requires mbind(MPOL_INTERLEAVE) on Linux.
;;; ------------------------------------------------------------

(defun rt-numa-node-of-thread (thread-id)
  "Return the NUMA node for THREAD-ID.

Pure CL has no portable NUMA discovery, so every thread maps to node 0. Native
Linux runtimes should query sched_getcpu/get_mempolicy or libnuma; Windows
runtimes can use GetNumaProcessorNodeEx."
  (declare (ignore thread-id))
  0)

(defun rt-numa-local-alloc (heap thread-id size-words)
  "Allocate SIZE-WORDS words from THREAD-ID's local NUMA node.

This portable stub records node metadata and delegates to the existing GC bump
allocator. Linux integrations should use numa_alloc_local/mbind for backing VM
pages; Windows integrations should use VirtualAllocExNuma."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let* ((node (if *rt-numa-enabled* (rt-numa-node-of-thread thread-id) 0))
         (addr (rt-gc-alloc heap 0 size-words)))
    (setf (gethash addr (rt-heap-numa-node-map heap)) node)
    addr))

(defun rt-gc-numa-affinity (heap node)
  "Return a portable NUMA-local GC worker schedule for NODE.

GC workers should be scheduled on the same NUMA node as the objects they scan.
Pure CL cannot set affinity, so this records a deterministic round-robin worker
assignment that native runtimes can translate to OS scheduler calls."
  (check-type heap rt-heap)
  (check-type node integer)
  (let* ((workers (max 1 *gc-worker-count*))
         (schedule (loop for worker from 0 below workers
                         collect (list :worker worker :node (mod (+ node worker) workers)))))
    (setf (rt-heap-numa-gc-schedule heap) schedule)
    schedule))

(defun rt-heap-interleave (heap addr size)
  "Mark [ADDR, ADDR+SIZE) as interleaved shared data.

FR-365 interface stub: native Linux should use mbind(MPOL_INTERLEAVE) for global
shared data pages. The Pure CL heap records region metadata only."
  (check-type heap rt-heap)
  (check-type addr integer)
  (check-type size (integer 0 *))
  (let ((region (list :start addr :end (+ addr size) :policy :interleave :portable t)))
    (push region (rt-heap-interleaved-regions heap))
    region))

(defun rt-heap-ref (heap index)
  "Read word at absolute INDEX from HEAP.

FR-342 relocation barrier: during concurrent relocation, pointer-like values are
checked against RT-HEAP-FORWARDING-TABLE. A forwarded slot is self-healed in
place and the updated value is returned, amortizing stale-pointer repair."
  (%rt-ubsan-check-access heap index :read)
  (when *rt-asan-enabled*
    (%rt-asan-check-address heap index :read))
  (%rt-hwasan-check-address index 0)
  (%rt-tsan-check-access index :read)
  (%rt-msan-check-read index)
  (when (and (rt-heap-access-bits heap)
             (integerp index)
             (or (and (>= index (rt-heap-young-from-base heap))
                      (< index (+ (rt-heap-young-from-base heap)
                                  (rt-heap-young-semi-size heap))))
                 (and (>= index (rt-heap-old-base heap))
                      (< index (+ (rt-heap-old-base heap)
                                  (rt-heap-old-size heap))))
                 (and (>= index (rt-heap-large-obj-base heap))
                      (< index (+ (rt-heap-large-obj-base heap)
                                  (rt-heap-large-obj-size heap))))))
    (setf (gethash index (rt-heap-access-bits heap))
          (rt-heap-minor-gc-count heap)))
  (let* ((value (svref (rt-heap-words heap) index))
         (table (rt-heap-forwarding-table heap))
         (old-addr (cond
                     ((and (integerp value) (val-pointer-p value))
                      (decode-pointer value))
                     ((integerp value) value)
                     (t nil)))
         (new-addr (and table old-addr (gethash old-addr table))))
    (if new-addr
        (let ((healed (if (and (integerp value) (val-pointer-p value))
                          (encode-pointer new-addr (pointer-tag value))
                          new-addr)))
          (setf (svref (rt-heap-words heap) index) healed)
          healed)
        value)))

(defun rt-heap-set (heap index value)
  "Write VALUE at absolute INDEX in HEAP."
  (%rt-ubsan-check-access heap index :write)
  (when *rt-asan-enabled*
    (%rt-asan-check-address heap index :write))
  (%rt-hwasan-check-address index 0)
  (%rt-tsan-check-access index :write)
  (setf (gethash index *rt-heap-init-map*) t)
  (setf (svref (rt-heap-words heap) index) value))

(defun rt-heap-object-header (heap addr)
  "Read the header word of the object at absolute word address ADDR."
  (rt-heap-ref heap addr))

(defun rt-heap-set-header (heap addr new-header)
  "Write NEW-HEADER as the header of the object at absolute word address ADDR."
  (rt-heap-set heap addr new-header))

(defun rt-heap-object-size (heap addr)
  "Return the size in words of the object at absolute word address ADDR."
  (header-size (rt-heap-object-header heap addr)))

(defun rt-heap-occupancy-pct (heap)
  "Return total young+old heap occupancy as a percentage.

The numerator is the active young from-space cursor plus the old-generation
allocation cursor.  The denominator is one young semi-space plus the old-space
capacity, matching the capacities reported by RT-GC-STATS."
  (let* ((young-used (- (rt-heap-young-free heap)
                        (rt-heap-young-from-base heap)))
         (old-used (- (rt-heap-old-free heap)
                      (rt-heap-old-base heap)))
         (used (+ young-used old-used))
         (total (+ (rt-heap-young-semi-size heap)
                   (rt-heap-old-size heap))))
    (if (plusp total)
        (* 100.0d0 (/ used total))
        0.0d0)))

(defun rt-heap-register-pressure-hook (heap hook)
  "Register HOOK to be called with (HEAP LEVEL OCCUPANCY-PCT) after GC pressure checks."
  (check-type hook function)
  (pushnew hook (rt-heap-pressure-hooks heap) :test #'eq)
  hook)

(defun rt-heap-fragmentation-pct (heap)
  "Return old-space fragmentation as free-list words divided by old-space size."
  (let ((free-words (loop for (size . nil) in (rt-heap-free-list-blocks heap) sum size))
        (old-size (rt-heap-old-size heap)))
    (if (plusp old-size)
        (/ (float free-words 1.0d0) old-size)
        0.0d0)))

(defun rt-heap-should-compact-p (heap)
  "Return true when fragmentation exceeds the configured compaction trigger."
  (> (rt-heap-fragmentation-pct heap)
     (rt-heap-compaction-trigger-fraction heap)))

(defun %rt-heap-live-used-words (heap)
  "Return the current used words in young, old, and large-object spaces."
  (+ (- (rt-heap-young-free heap) (rt-heap-young-from-base heap))
     (- (rt-heap-old-free heap) (rt-heap-old-base heap))
     (- (rt-heap-large-obj-free heap) (rt-heap-large-obj-base heap))))

;;; ------------------------------------------------------------
;;; Segregated free lists and slabs (FR-359, FR-362)
;;; ------------------------------------------------------------

(defun rt-free-list-bin-index (size-words)
  "Return the segregated free-list bin index for SIZE-WORDS."
  (check-type size-words (integer 1 *))
  (let ((bytes (* size-words 8)))
    (cond
      ((<= bytes 8) 0)
      ((<= bytes 16) 1)
      ((<= bytes 32) 2)
      ((<= bytes 64) 3)
      ((<= bytes 128) 4)
      ((<= bytes 256) 5)
      ((<= bytes 512) 6)
      ((<= bytes 1024) 7)
      ((<= bytes 2048) 8)
      ((<= bytes 4096) 9)
      ((<= bytes 8192) 10)
      ((<= bytes 16384) 11)
      ((<= bytes 32768) 12)
      ((<= bytes 65536) 13)
      ((<= bytes 131072) 14)
      (t 15))))

(defun rt-heap-free-list-blocks (heap)
  "Return all old-space free blocks, preserving legacy RT-HEAP-FREE-LIST view."
  (let ((bins (rt-heap-free-bins heap)))
    (if bins
        (loop for i from 0 below (length bins) append (copy-list (svref bins i)))
        (copy-list (rt-heap-free-list heap)))))

(defun %rt-free-list-sync-legacy (heap)
  (setf (rt-heap-free-list heap) (rt-heap-free-list-blocks heap)))

(defun %rt-free-list-rebuild-bins (heap blocks)
  (let ((bins (or (rt-heap-free-bins heap)
                  (setf (rt-heap-free-bins heap)
                        (make-array +rt-free-list-bin-count+ :initial-element nil)))))
    (fill bins nil)
    (dolist (block blocks)
      (let ((index (rt-free-list-bin-index (car block))))
        (push block (svref bins index))))
    (%rt-free-list-sync-legacy heap)
    bins))

(defun rt-free-list-insert (heap size-words addr)
  "Insert free block (SIZE-WORDS . ADDR) into the appropriate segregated bin."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (check-type addr integer)
  (let* ((bins (or (rt-heap-free-bins heap)
                   (setf (rt-heap-free-bins heap)
                         (make-array +rt-free-list-bin-count+ :initial-element nil))))
         (index (rt-free-list-bin-index size-words)))
    (push (cons size-words addr) (svref bins index))
    (%rt-free-list-sync-legacy heap)
    index))

(defun rt-free-list-find (heap size-words)
  "Find and remove a block at least SIZE-WORDS words; return BIN-INDEX and ADDR."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let ((bins (rt-heap-free-bins heap)))
    (when bins
      (loop for i from (rt-free-list-bin-index size-words) below (length bins) do
        (loop with prev = nil
              for cell on (svref bins i)
              for block = (car cell)
              for block-size = (car block)
              for block-addr = (cdr block)
              when (>= block-size size-words) do
                (let ((remainder (- block-size size-words)))
                  (if prev
                      (setf (cdr prev) (cdr cell))
                      (setf (svref bins i) (cdr cell)))
                  (when (plusp remainder)
                    (rt-free-list-insert heap remainder (+ block-addr size-words)))
                  (%rt-free-list-sync-legacy heap)
                  (return-from rt-free-list-find (values i block-addr)))
              do (setf prev cell)))))
  (values nil nil))

(defun rt-slab-size-class (type-tag size-words)
  "Return the slab class key for a known fixed-size runtime allocation."
  (declare (ignore type-tag))
  (car (find size-words *rt-slab-size-classes* :key #'cdr :test #'=)))

(defun %rt-slab-allocate-page (heap class-size)
  (let* ((slots (max 1 (floor +rt-slab-page-words+ class-size)))
         (page-words (* slots class-size))
         (addr (or (nth-value 1 (rt-free-list-find heap page-words))
                   (let ((old-free (rt-heap-old-free heap)))
                     (when (> (+ old-free page-words)
                              (+ (rt-heap-old-base heap) (rt-heap-old-size heap)))
                       (error "cl-cc/runtime: slab page allocation exhausted old space — ~D words requested"
                              page-words))
                     (setf (rt-heap-old-free heap) (+ old-free page-words))
                     old-free)))
         (limit (+ addr page-words))
         (free nil))
    (loop for slot from addr below limit by class-size do
      (rt-heap-set-header heap slot (make-header class-size 0 0))
      (push slot free))
    (%make-rt-slab :class-size class-size
                   :free-list free
                   :slab-base addr
                   :slab-limit limit)))

(defun rt-slab-alloc (heap size-class)
  "Allocate one fixed-size object slot from SIZE-CLASS slab pool."
  (check-type heap rt-heap)
  (let* ((class-size (or (cdr (assoc size-class *rt-slab-size-classes*))
                         (and (integerp size-class) size-class))))
    (unless (and (integerp class-size) (plusp class-size))
      (error "cl-cc/runtime: unknown slab size class ~S" size-class))
    (let* ((pools (rt-heap-slab-pools heap))
           (slabs (gethash size-class pools))
           (slab (or (find-if #'rt-slab-free-list slabs)
                     (let ((new-slab (%rt-slab-allocate-page heap class-size)))
                       (push new-slab (gethash size-class pools))
                       new-slab)))
           (addr (pop (rt-slab-free-list slab))))
      addr)))

(defun rt-slab-free (heap size-class addr)
  "Return ADDR to SIZE-CLASS slab free list and clear its words."
  (check-type heap rt-heap)
  (check-type addr integer)
  (let* ((class-size (or (cdr (assoc size-class *rt-slab-size-classes*))
                         (and (integerp size-class) size-class)))
         (slab (find-if (lambda (s)
                          (and (= (rt-slab-class-size s) class-size)
                               (<= (rt-slab-slab-base s) addr)
                               (< addr (rt-slab-slab-limit s))))
                        (gethash size-class (rt-heap-slab-pools heap)))))
    (unless slab
      (error "cl-cc/runtime: address ~D is not in slab class ~S" addr size-class))
    (loop for i from addr below (+ addr class-size) do
      (rt-heap-set heap i 0))
    (rt-heap-set-header heap addr (make-header class-size 0 0))
    (pushnew addr (rt-slab-free-list slab) :test #'eql)
    addr))

(defun %rt-heap-rebox-shifted-address (heap value old-large-base old-large-free delta)
  "Shift VALUE when it points into the old large-object range."
  (let ((addr (cond
                ((and (integerp value) (val-pointer-p value))
                 (decode-pointer value))
                ((integerp value) value)
                (t nil))))
    (if (and addr (>= addr old-large-base) (< addr old-large-free))
        (%rt-gc-rebox-pointer-like value (+ addr delta))
        value)))

(defun %rt-heap-adjust-large-object-pointers (heap old-large-base old-large-free delta)
  "Adjust roots and traced pointer slots after the large-object range moves."
  (unless (zerop delta)
    (dolist (root-cell (rt-heap-roots heap))
      (setf (cdr root-cell)
            (%rt-heap-rebox-shifted-address heap (cdr root-cell)
                                            old-large-base old-large-free delta)))
    (labels ((adjust-range (start end)
               (loop with addr = start
                     while (< addr end) do
                       (let ((h (rt-heap-object-header heap addr)))
                         (cond
                           ((header-forwarding-p h) (incf addr 1))
                           ((and (integerp h) (> (header-size h) 0))
                            (let ((size (header-size h)))
                              (dolist (offset (rt-object-pointer-slots heap addr))
                                (let ((slot (+ addr offset)))
                                  (rt-heap-set heap slot
                                               (%rt-heap-rebox-shifted-address
                                                heap (rt-heap-ref heap slot)
                                                old-large-base old-large-free delta))))
                              (incf addr size)))
                           (t (return)))))))
      (adjust-range (rt-heap-young-from-base heap) (rt-heap-young-free heap))
      (adjust-range (rt-heap-old-base heap) (rt-heap-old-free heap))
      (adjust-range (rt-heap-large-obj-base heap) (rt-heap-large-obj-free heap))))
  heap)

(defun %rt-heap-resize-old-space (heap new-old-size)
  "Resize old-space capacity while preserving object addresses where possible.

The old-space base is stable.  Large-object space moves by the old-size delta;
roots and traced pointer slots that target large objects are rewritten."
  (let* ((old-words (rt-heap-words heap))
         (old-total (length old-words))
         (old-size (rt-heap-old-size heap))
         (old-base (rt-heap-old-base heap))
         (old-free (rt-heap-old-free heap))
         (old-large-base (rt-heap-large-obj-base heap))
         (old-large-free (rt-heap-large-obj-free heap))
         (large-used (- old-large-free old-large-base))
         (delta (- new-old-size old-size))
         (new-large-base (+ old-large-base delta))
         (new-large-size (max (rt-heap-initial-large-obj-size heap)
                              (+ (rt-heap-large-obj-size heap) delta)
                              large-used))
         (new-total (+ new-large-base new-large-size))
         (new-words (make-array new-total :initial-element 0))
         (new-num-cards (ceiling new-old-size +gc-card-size-words+)))
    (declare (ignore old-total))
    ;; Young semispaces and old objects keep their absolute addresses.
    (loop for i from 0 below (min old-free (length old-words)) do
      (setf (svref new-words i) (svref old-words i)))
    ;; Large-object payload moves with the large-object base.
    (loop for i from 0 below large-used do
      (setf (svref new-words (+ new-large-base i))
            (svref old-words (+ old-large-base i))))
    (setf (rt-heap-words heap) new-words
          (rt-heap-old-size heap) new-old-size
          (rt-heap-large-obj-base heap) new-large-base
          (rt-heap-large-obj-size heap) new-large-size
          (rt-heap-large-obj-free heap) (+ new-large-base large-used)
          (rt-heap-num-cards heap) new-num-cards
          (rt-heap-card-table heap) (make-array new-num-cards
                                                :element-type '(unsigned-byte 8)
                                                :initial-element 0)
          (rt-heap-card-summary heap) (make-array (ceiling new-num-cards
                                                           +gc-card-summary-block-size+)
                                                  :initial-element nil))
    (%rt-heap-adjust-large-object-pointers heap old-large-base old-large-free delta)
    heap))

(defun rt-heap-maybe-grow (heap)
  "Double old-space capacity when post-GC occupancy remains above 90%.

Growth is capped by RT-HEAP-MAX-HEAP-WORDS.  Returns true when the heap was
resized, NIL otherwise."
  (let ((current-total (length (rt-heap-words heap)))
        (max-total (rt-heap-max-heap-words heap)))
    (when (and (> (rt-heap-occupancy-pct heap) 90.0d0)
               (or (zerop max-total) (< current-total max-total)))
      (let* ((target-total (if (zerop max-total)
                               (* current-total 2)
                               (min (* current-total 2) max-total)))
             (delta-total (- target-total current-total))
             (new-old-size (+ (rt-heap-old-size heap) delta-total)))
        (when (plusp delta-total)
          (%rt-heap-resize-old-space heap new-old-size)
          (setf (rt-heap-shrink-counter heap) 0)
          t)))))

(defun rt-heap-maybe-shrink (heap)
  "Halve heap capacity after three consecutive low-occupancy major GCs.

Occupancy must remain below RT-HEAP-SHRINK-THRESHOLD (default 25%).  The heap
never shrinks below its initial size or below currently used old/large words."
  (let ((threshold-pct (* 100.0d0 (rt-heap-shrink-threshold heap))))
    (if (< (rt-heap-occupancy-pct heap) threshold-pct)
        (incf (rt-heap-shrink-counter heap))
        (setf (rt-heap-shrink-counter heap) 0))
    (when (>= (rt-heap-shrink-counter heap) 3)
      (let* ((current-total (length (rt-heap-words heap)))
             (minimum-total (rt-heap-initial-heap-words heap))
             (target-total (max minimum-total (floor current-total 2)))
             (old-used (- (rt-heap-old-free heap) (rt-heap-old-base heap)))
             (large-used (- (rt-heap-large-obj-free heap) (rt-heap-large-obj-base heap)))
             (minimum-old-size (max (rt-heap-initial-old-size heap) old-used))
             (new-old-size (max minimum-old-size
                                (- target-total
                                   (rt-heap-large-obj-base heap)
                                   large-used))))
        (if (< target-total current-total)
            (progn
              (%rt-heap-resize-old-space heap new-old-size)
              (setf (rt-heap-shrink-counter heap) 0)
              t)
            (progn
              (setf (rt-heap-shrink-counter heap) 0)
              nil))))))

;;; Card table helpers, address predicates, and rt-object-pointer-slots
;;; are in heap-trace.lisp (loads next).

;;; ------------------------------------------------------------
;;; Immortal / Permanent Objects (FR-377)
;;; FR-377: Immortal / Permanent Objects — never GC'd objects in dedicated region; reduces root scan volume
;;; ------------------------------------------------------------

(defparameter *rt-immortal-registry* (make-hash-table :test #'eql)
  "Set of heap addresses that are immortal (never GC'd).
   Objects in this set are skipped during GC root scanning and sweeping.")

(defparameter *rt-immortal-space-base* nil
  "Base address of the immortal object space, or NIL if not yet allocated.")

(defun rt-make-immortal (type-tag size-words)
  "Allocate an immortal object that will never be collected.
   Immortal objects are placed in a separate permanent space and are
   excluded from GC root scanning. Use for built-in symbols, built-in
   functions, and code objects that should persist for the process lifetime.
   Returns the address of the allocated object."
  (let* ((total-words (+ size-words 1)) ; +1 for header
         (ptr (if *rt-immortal-space-base*
                  (prog1 *rt-immortal-space-base*
                    (incf *rt-immortal-space-base* total-words))
                  (let ((base 0)) ; placeholder — in real impl, mmap at fixed addr
                    (setf *rt-immortal-space-base* (+ base total-words))
                    base))))
    (setf (gethash ptr *rt-immortal-registry*) t)
    ;; Header is written by the caller via rt-heap-set-header
    ptr))

(defun rt-immortal-p (addr)
  "Return T if ADDR is an immortal object."
  (gethash addr *rt-immortal-registry*))

;;; ------------------------------------------------------------
;;; Off-Heap Native Memory (FR-378)
;;; FR-378: Off-Heap Native Memory Management — FFI buffers allocated outside GC heap with allocation tracking
;;; ------------------------------------------------------------

(defparameter *rt-native-allocations* (make-hash-table :test #'eql)
  "Registry of native memory allocations: ptr → size-in-bytes.")

(defparameter *rt-total-native-bytes* 0
  "Total bytes allocated through rt-native-alloc (for GC pressure tracking).")

(defun rt-native-alloc (size-bytes)
  "Allocate SIZE-BYTES of native (off-heap) memory.
   Returns an integer handle representing the allocation.
   This memory is NOT managed by the GC and must be freed with rt-native-free.
   However, large native allocations increase GC pressure tracking."
  (let ((handle (hash-table-count *rt-native-allocations*)))
    (setf (gethash handle *rt-native-allocations*) size-bytes)
    (incf *rt-total-native-bytes* size-bytes)
    handle))

(defun rt-native-free (handle)
  "Free a native memory allocation identified by HANDLE."
  (let ((size (gethash handle *rt-native-allocations*)))
    (when size
      (decf *rt-total-native-bytes* size)
      (remhash handle *rt-native-allocations*)
      t)))

(defun rt-gc-register-external-memory (bytes)
  "Notify the GC that BYTES of external (non-GC-heap) memory is in use.
   This increases GC pressure tracking, causing more frequent collections
   when large amounts of external memory are held."
  (incf *rt-total-native-bytes* bytes))

;;; ------------------------------------------------------------
;;; Container-Aware Heap Sizing (FR-423) — implemented above at line 48
;;; ------------------------------------------------------------
