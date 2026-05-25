(in-package :cl-cc/runtime)

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

;;; FR-373: Address Space Layout Randomization for Heap
(defun rt-heap-randomize-base (&optional (size #x1000000 size-supplied-p))
  "Return a randomized base offset for heap allocation.

Pure CL implements heap ASLR as a logical word offset into the managed
SIMPLE-VECTOR.  Native backends can map the same interface to
mmap(MAP_FIXED_NOREPLACE).  For backwards-compatible deterministic heap
construction, implicit calls return 0 unless *RT-HEAP-RANDOMIZE* is true;
explicit SIZE calls return a fresh Pure CL random offset in [0, SIZE)."
  (let ((max-offset (max 1 (min size #x1000000))))
    (if (or size-supplied-p *rt-heap-randomize*)
        (random max-offset)
        0)))

(defvar *rt-stack-guard-registry* (make-hash-table :test #'equal)
  "Pure CL FR-376 registry of installed logical stack guard pages.")

;;; FR-376: Guard Pages for Stack Overflow Detection
(defun rt-install-stack-guard (stack-base stack-size)
  "Install a guard page at the end of the stack.

Pure CL cannot change host page protection, so it records guard metadata in a
plist registry. Native backends should implement the same interface with
mprotect(PROT_NONE) plus sigaltstack/signal handling."
  (check-type stack-base integer)
  (check-type stack-size integer)
  (let* ((page-size 4096)
          (guard-size (if *rt-stack-guard-enabled* page-size 0))
          (guard-base (max stack-base (- (+ stack-base stack-size) guard-size)))
          (descriptor (list :installed *rt-stack-guard-enabled*
                            :enabled *rt-stack-guard-enabled*
                            :guard-size guard-size
                            :stack-base stack-base
                            :stack-size stack-size
                            :guard-base guard-base
                            :fault-signal :sigsegv
                            :portable-fallback t)))
    (setf (gethash (list stack-base stack-size) *rt-stack-guard-registry*) descriptor)
    descriptor))

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

(defun %rt-sanitizer-sb-thread-function (name)
  "Return the SB-THREAD function named NAME, or NIL when unavailable."
  (ignore-errors
    (let ((package (find-package "SB-THREAD")))
      (when package
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and status (fboundp symbol))
            (symbol-function symbol)))))))

(defun %rt-sanitizer-make-mutex ()
  "Create an optional mutex for sanitizer bookkeeping maps."
  (let ((make-mutex (%rt-sanitizer-sb-thread-function "MAKE-MUTEX")))
    (and make-mutex (ignore-errors (funcall make-mutex :name "rt-sanitizer-maps")))))

(defvar *rt-sanitizer-map-lock* (%rt-sanitizer-make-mutex)
  "Optional lock protecting global sanitizer hash tables during parallel tests.")

(defmacro %rt-with-sanitizer-map-lock (() &body body)
  (let ((fn (gensym "CALL-WITH-MUTEX"))
        (lock (gensym "LOCK")))
    `(let ((,lock *rt-sanitizer-map-lock*))
       (if ,lock
           (let ((,fn (%rt-sanitizer-sb-thread-function "CALL-WITH-MUTEX")))
             (if ,fn
                 (funcall ,fn (lambda () ,@body) ,lock)
                 (progn ,@body)))
           (progn ,@body)))))

(defun rt-sanitizer-reset-state ()
  "Reset sanitizer bookkeeping maps to empty state."
  (%rt-with-sanitizer-map-lock ()
    (clrhash *rt-heap-poison-map*)
    (clrhash *rt-heap-init-map*)
    (clrhash *rt-heap-tag-map*)
    (clrhash *rt-heap-access-map*))
  t)

(defun rt-sanitizer-poison-address (index)
  "Mark INDEX as poisoned for ASan/HWASan checks."
  (%rt-with-sanitizer-map-lock ()
    (setf (gethash index *rt-heap-poison-map*) t))
  t)

(defun rt-sanitizer-unpoison-address (index)
  "Clear poison marker from INDEX."
  (%rt-with-sanitizer-map-lock ()
    (remhash index *rt-heap-poison-map*))
  t)

(defun rt-sanitizer-set-address-tag (index tag)
  "Set 4-bit HWASan tag for INDEX."
  (%rt-with-sanitizer-map-lock ()
    (setf (gethash index *rt-heap-tag-map*) (logand tag #xF)))
  t)

(defun %rt-asan-check-address (heap index op)
  (let ((limit (length (rt-heap-words heap))))
    (when (or (< index 0) (>= index limit))
      (error "ASan: ~A out-of-bounds heap access at ~D (limit ~D)" op index limit))
    (%rt-with-sanitizer-map-lock ()
      (when (gethash index *rt-heap-poison-map*)
        (error "ASan: ~A on poisoned heap address ~D" op index)))))

(defun %rt-msan-check-read (index)
  (when *rt-msan-enabled*
    (%rt-with-sanitizer-map-lock ()
      (when (null (gethash index *rt-heap-init-map*))
        (error "MSan: read from uninitialized heap address ~D" index)))))

(defun %rt-tsan-check-access (index mode)
  (when *rt-tsan-enabled*
    (%rt-with-sanitizer-map-lock ()
      (let ((prev (gethash index *rt-heap-access-map*)))
        (when (and prev
                   (/= (car prev) *rt-tsan-thread-id*)
                   (or (eq mode :write) (eq (cdr prev) :write)))
          (error "TSan: data race at heap address ~D between thread ~D (~A) and ~D (~A)"
                 index (car prev) (cdr prev) *rt-tsan-thread-id* mode))
        (setf (gethash index *rt-heap-access-map*) (cons *rt-tsan-thread-id* mode))))))

(defun %rt-hwasan-check-address (index expected-tag)
  (when *rt-hwasan-enabled*
    (%rt-with-sanitizer-map-lock ()
      (let ((actual-tag (gethash index *rt-heap-tag-map* 0)))
        (unless (= (logand expected-tag #xF) actual-tag)
          (error "HWASan: tag mismatch at heap address ~D (expected ~D, actual ~D)"
                 index (logand expected-tag #xF) actual-tag))))))

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

(defparameter *rt-free-list-size-class-bytes* #(8 16 32 64 128 256 512 1024)
  "FR-156 old-space segregated free-list size classes, in bytes.")

(defparameter *rt-free-list-size-class-words* #(1 2 4 8 16 32 64 128)
  "FR-156 old-space segregated free-list size classes, in heap words.")

(defparameter *rt-free-list-fallback-class-words* #(256 512 1024 2048 4096 8192 16384 32768)
  "Compatibility fallback classes for blocks larger than the FR-156 buckets.")

(defconstant +rt-free-list-bin-count+ 16
  "Number of old-space free-list bins: 8 FR-156 buckets plus 8 oversized fallbacks.")

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

;;; FR-266 compressed object header (one 64-bit word):
;;;   [ type-tag:8 | gc-bits:4 | shape-id:20 | size:32 ]
;;;
;;; The public RT-HEADER-* accessors expose those bitfields directly.  The older
;;; HEADER-* API remains as a compatibility alias: HEADER-TAG maps to TYPE-TAG,
;;; HEADER-AGE maps to GC-BITS, and HEADER-SIZE maps to SIZE.
;;;
;;; MARK/GRAY are transient collector flags used by the existing mark-sweep
;;; implementation.  They are kept as compatibility bits outside the compressed
;;; payload so the FR-266 fields retain their exact widths; native runtimes can
;;; lower them to side metadata or use the 4 GC bits according to their collector
;;; encoding.

;;; Size field: bits 31..0
(defconstant +header-size-shift+    0)
(defconstant +header-size-mask+     #x00000000FFFFFFFF)

;;; Shape-id field: bits 51..32
(defconstant +header-shape-id-shift+ 32)
(defconstant +header-shape-field-mask+ #x000FFFFF00000000)
(defconstant +header-shape-id-mask+ #x000FFFFF00000000)
(defconstant +header-shape-id-max+ #xFFFFF)

;;; GC-bits field: bits 55..52
(defconstant +header-gc-bits-shift+ 52)
(defconstant +header-gc-bits-mask+  #x00F0000000000000)

;;; Type tag field: bits 63..56
(defconstant +header-tag-shift+     56)
(defconstant +header-tag-mask+      #xFF00000000000000)

;;; Age occupies bits 53..52 (2 bits, 0-3) within the GC-bits field.
;;; Mark (bit 55) and gray (bit 54) are outside the age sub-field.
(defconstant +header-age-shift+     +header-gc-bits-shift+)
(defconstant +header-age-mask+      #x0030000000000000)

;;; Mark/Gray bits live inside the GC-bits field (bits 55..52) so the entire
;;; header word always fits in (unsigned-byte 64).  Bit 55 = mark, bit 54 = gray,
;;; leaving bits 53..52 for the 2-bit age/survival counter.
(defconstant +header-mark-bit+      #x0080000000000000)
(defconstant +header-gray-bit+      #x0040000000000000)

;;; Forwarding pointers are represented as (cons :forwarded dest-addr)
;;; rather than bit-packed integers.  This avoids address-size limitations
;;; and is idiomatic for a Pure CL heap simulation.

;;; ------------------------------------------------------------
;;; Header Construction and Accessors
;;; ------------------------------------------------------------

(defun make-rt-header (size type-tag &key (gc-bits 0) (shape-id 0))
  "Construct a FR-266 compressed runtime object header.

SIZE is the object size in heap words, including the header word.  TYPE-TAG is
an 8-bit runtime type tag.  GC-BITS is the 4-bit collector field used by the
compatibility HEADER-AGE accessor.  SHAPE-ID embeds the FR-214 object-shape id in
the header, replacing the need for a per-object class pointer in compact object
layouts."
  (check-type size (integer 0 #xffffffff))
  (check-type type-tag (integer 0 #xff))
  (check-type gc-bits (integer 0 #xf))
  (check-type shape-id (integer 0 #xfffff))
  (logior (ash (logand type-tag #xFF) +header-tag-shift+)
          (ash (logand gc-bits #xF) +header-gc-bits-shift+)
          (ash (logand shape-id +header-shape-id-max+) +header-shape-id-shift+)
          (logand size #xFFFFFFFF)))

(defun make-header (size type-tag &optional (age 0) (shape-id 0))
  "Compatibility constructor for a compressed FR-266 header.

AGE maps to the 4-bit GC-BITS field.  Optional SHAPE-ID embeds the FR-214 shape
id when available; callers that do not know a shape retain shape id 0."
  (make-rt-header size type-tag :gc-bits age :shape-id shape-id))

(defun rt-header-size (h)
  "Extract the 32-bit object size field from compressed header H."
  (logand h #xFFFFFFFF))

(defun rt-header-type-tag (h)
  "Extract the 8-bit runtime type tag from compressed header H."
  (logand (ash h (- +header-tag-shift+)) #xFF))

(defun rt-header-gc-bits (h)
  "Extract the 4-bit GC field from compressed header H."
  (logand (ash h (- +header-gc-bits-shift+)) #xF))

(defun rt-header-shape-id (h)
  "Extract the embedded 20-bit FR-214 shape id from compressed header H."
  (logand (ash (logand h +header-shape-id-mask+) (- +header-shape-id-shift+))
          +header-shape-id-max+))

(defun header-size (h)
  "Extract the object size in words from header word H."
  (rt-header-size h))

(defun header-tag (h)
  "Extract the type tag (0-7) from header word H."
  (rt-header-type-tag h))

(defun header-age (h)
  "Extract the age (0-3) from header word H.
   Age occupies bits 53..52; mark (bit 55) and gray (bit 54) are masked out."
  (logand (rt-header-gc-bits h) #x3))

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
  "Return a new header with age incremented by 1, capped at 3."
  (let* ((current-age (header-age h))
         (new-age (min 3 (1+ current-age))))
    (logior (logand h (lognot +header-age-mask+))
            (ash new-age +header-age-shift+))))

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
           (managed-words (+ (* 2 semi-size) old-size large-obj-size))
            (heap-base (if *rt-heap-randomize*
                           (rt-heap-randomize-base managed-words)
                           (rt-heap-randomize-base)))
           (young-from-base heap-base)
           (young-to-base (+ young-from-base semi-size))
           (old-base   (+ young-to-base semi-size))
           (large-obj-base (+ old-base old-size))
           (total-size (+ heap-base managed-words))
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
     (when (and *compressed-pointers-enabled*
                (> managed-words +compressed-heap-region-words+))
       (error "cl-cc/runtime: compressed pointers require heap <= 4GB (~D words requested, max ~D)"
              managed-words +compressed-heap-region-words+))
     (when *compressed-pointers-enabled*
       (setf *heap-base-address* young-from-base))
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
        ;; FR-391: 0 means unlimited dynamic growth; positive
        ;; *GC-MAX-HEAP-WORDS* caps RT-HEAP-MAYBE-GROW.
        :max-heap-words  (if (plusp *gc-max-heap-words*)
                             *gc-max-heap-words*
                             0)
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
;;; FR-347: Compressed Object References (portable offset codec)
;;; ------------------------------------------------------------

(defun rt-compress-object-ref (heap addr)
  "Return ADDR as a 32-bit heap-relative offset for FR-347.

The Pure CL heap stores ordinary word addresses, but this helper exposes the
same contract a native backend would use for compressed object references:
OFFSET = ADDR - HEAP-BASE.  The offset is validated to fit in 32 bits so callers
can test density-sensitive paths without native pointer compression."
  (check-type heap rt-heap)
  (check-type addr integer)
  (let* ((base (rt-heap-young-from-base heap))
          (offset (- addr base)))
    (unless (<= 0 offset #xffffffff)
      (error "cl-cc/runtime: address ~D cannot be compressed relative to heap base ~D"
             addr base))
    offset))

(defun rt-decompress-object-ref (heap offset)
  "Expand a 32-bit heap-relative OFFSET back into an absolute heap word address."
  (check-type heap rt-heap)
  (check-type offset (integer 0 #xffffffff))
  (+ (rt-heap-young-from-base heap) offset))

(defun %rt-ensure-compressed-pointer-range (addr size-words)
  "Signal if [ADDR, ADDR+SIZE-WORDS) cannot be represented as a compressed pointer."
  (when *compressed-pointers-enabled*
    (let ((end-offset (- (+ addr size-words) *heap-base-address*)))
      (unless (<= 0 end-offset +compressed-heap-region-words+)
        (error "cl-cc/runtime: allocation [~D, ~D) escapes compressed 4GB heap region based at ~D"
               addr (+ addr size-words) *heap-base-address*)))))

;;; ------------------------------------------------------------
;;; FR-288: Large Page Support (portable implementation)
;;;   On Linux with MAP_HUGETLB, native runtime can enable huge pages.
;;;   Portable CL implementation: tracks *rt-heap-hugepage-enabled* toggle
;;;   and provides os-advise-hugepage for transparent hugepage hinting.
;;;   Native backends should wire mmap(MAP_HUGETLB) or MADV_HUGEPAGE.
;;;
;;; FR-363: NUMA-Aware Heap Allocation (portable implementation)
;;;   Interface: *rt-numa-enabled*, rt-numa-local-alloc.
;;;   Portable implementation records per-thread NUMA node mappings
;;;   and delegates to GC bump allocator. Linux libnuma integration
;;;   available via the topology module (detect-numa-topology).
;;;
;;; FR-364: NUMA-Local GC (portable implementation)
;;;   Interface: rt-gc-numa-affinity.
;;;   Portable implementation generates deterministic round-robin
;;;   worker schedules. Native runtimes translate to sched_setaffinity.
;;;
;;; FR-365: Memory Interleaving (portable implementation)
;;;   Interface: rt-heap-interleave.
;;;   Portable implementation records interleave policy metadata.
;;;   Linux backends use mbind(MPOL_INTERLEAVE) when available.
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

FR-365: Pure CL implementation records interleaved region metadata in
RT-HEAP-INTERLEAVED-REGIONS.  The NUMA-aware GC worker scheduler
(rt-gc-numa-affinity) uses this metadata for deterministic work distribution.

Deferred to Tier 6 (memory-gc.md): native Linux mbind(MPOL_INTERLEAVE) call
for global shared data pages requires OS-level mmap/mbind integration."
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
  (%rt-with-sanitizer-map-lock ()
    (setf (gethash index *rt-heap-init-map*) t))
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
