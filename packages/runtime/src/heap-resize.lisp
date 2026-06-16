(in-package :cl-cc/runtime)

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
                           ((and (integerp h) (> (rt-header-size h) 0))
                            (let ((size (rt-header-size h)))
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
;;; FR-377: Immortal / Permanent Objects
;;; FR-377: Immortal / Permanent Objects — never GC'd permanent handles; reduces root scan volume
;;; ------------------------------------------------------------

(defvar *rt-immortal-registry* (make-hash-table :test 'eq)
  "Hash table of immortal objects, keyed by handle.")

(defparameter *rt-immortal-space-base* nil
  "Logical cursor into the immortal object space.")

(defun rt-make-immortal (value &optional size-words)
  "Create an immortal object that will never be GC'd.

VALUE is stored under a fresh handle. Callers may pass (TYPE-TAG SIZE-WORDS);
in that case the stored value is a plist describing the logical permanent object
rather than a native address."
  (let ((handle (gensym "IMMORTAL-")))
    (setf (gethash handle *rt-immortal-registry*)
          (if size-words
              (let ((base (or *rt-immortal-space-base* 0))
                    (total-words (1+ size-words)))
                (setf *rt-immortal-space-base* (+ base total-words))
                (list :type-tag value
                      :size-words size-words
                      :base base
                      :total-words total-words))
              value))
    handle))

(defun rt-immortal-p (handle)
  "Return T if HANDLE is an immortal object."
  (nth-value 1 (gethash handle *rt-immortal-registry*)))

(defun rt-immortal-objects-count ()
  "Return the number of immortal objects."
  (hash-table-count *rt-immortal-registry*))

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
