;;;; packages/runtime/src/heap-los.lisp — Large Object Space (FR-622)

(in-package :cl-cc/runtime)

(defparameter *large-object-threshold* 8192
  "Large Object Space threshold. Objects larger than this word count bypass nursery allocation.")

(defstruct rt-los-allocation
  (address 0 :type integer)
  (size-words 0 :type integer)
  (mapped-bytes 0 :type integer)
  (region nil)
  (movable-p nil :type boolean))

(defvar *rt-los-allocations* (make-hash-table :test #'eq)
  "Heap -> list of LOS allocation descriptors. LOS objects are never compacted.")

(defvar *rt-los-card-tables* (make-hash-table :test #'eq)
  "Heap -> card table tracking old-space cards that contain old->LOS pointers.")

(defun rt-los-threshold-words (heap)
  "Return the effective LOS threshold for HEAP, synchronizing the FR-622 flag."
  (check-type heap rt-heap)
  (let ((threshold (or (rt-heap-large-obj-threshold heap)
                       *large-object-threshold*)))
    (setf (rt-heap-large-obj-threshold heap) threshold)
    threshold))

(defun rt-large-object-size-p (heap size-words)
  "Return true when SIZE-WORDS should be routed to Large Object Space."
  (and (integerp size-words)
       (> size-words (rt-los-threshold-words heap))))

(defun %rt-los-page-align-words (words)
  "Round WORDS up to the native page size expressed in 64-bit words."
  (ceiling (rt-page-align (* words 8)) 8))

(defun %rt-los-ensure-card-table (heap)
  "Return the old->LOS card table for HEAP, creating it on demand."
  (or (gethash heap *rt-los-card-tables*)
      (setf (gethash heap *rt-los-card-tables*)
            (make-array (max 1 (rt-heap-num-cards heap))
                        :element-type '(unsigned-byte 8)
                        :initial-element 0))))

(defun rt-los-card-dirty-p (heap old-addr)
  "Return true if OLD-ADDR's card contains an old->LOS pointer."
  (not (zerop (aref (%rt-los-ensure-card-table heap)
                    (rt-card-index heap old-addr)))))

(defun rt-los-card-mark-dirty (heap old-addr)
  "Mark OLD-ADDR's card as containing an old->LOS pointer."
  (setf (aref (%rt-los-ensure-card-table heap)
              (rt-card-index heap old-addr))
        1)
  old-addr)

(defun rt-los-card-clear-all (heap)
  "Clear all old->LOS card marks for HEAP."
  (fill (%rt-los-ensure-card-table heap) 0)
  t)

(defun rt-los-alloc (heap size-words)
  "Allocate SIZE-WORDS words in Large Object Space and return a stable address.

The logical heap remains a SIMPLE-VECTOR for the portable runtime, while each LOS
object records a page-aligned mmap descriptor.  The returned object address is
never moved by copying or compacting collectors."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let* ((addr (rt-heap-large-obj-free heap))
         (limit (+ (rt-heap-large-obj-base heap)
                   (rt-heap-large-obj-size heap)))
         (mapped-words (%rt-los-page-align-words size-words))
         (mapped-bytes (* mapped-words 8)))
    (when (> (+ addr size-words) limit)
      (if (fboundp 'rt-signal-oom)
          (rt-signal-oom size-words :heap heap :limit-words limit :used-words addr)
          (error "cl-cc/runtime: large object space exhausted — ~D words requested" size-words)))
    (%rt-ensure-compressed-pointer-range addr size-words)
    (let ((region (if (and (boundp '*use-huge-pages*) *use-huge-pages*)
                      (rt-huge-page-mmap nil mapped-bytes
                                         (logior +rt-prot-read+ +rt-prot-write+)
                                         +rt-map-anonymous+ nil 0)
                      (rt-mmap nil mapped-bytes
                               (logior +rt-prot-read+ +rt-prot-write+)
                               +rt-map-anonymous+ nil 0))))
      ;; Keep the GC-visible LOS range dense.  Page granularity is tracked in the
      ;; mapping descriptor; advancing by page padding would make existing heap
      ;; scanners stop at zero-filled gaps before later LOS objects.
      (setf (rt-heap-large-obj-free heap) (+ addr size-words))
      (push (make-rt-los-allocation :address addr
                                    :size-words size-words
                                    :mapped-bytes mapped-bytes
                                    :region region
                                    :movable-p nil)
            (gethash heap *rt-los-allocations*))
      (rt-gc-profile-sample (* size-words 8))
      addr)))

(defun rt-los-note-write (heap obj-addr new-val)
  "Record an old->LOS pointer in the LOS card table when NEW-VAL targets LOS."
  (when (and (integerp obj-addr)
             (rt-old-addr-p heap obj-addr))
    (let ((target (cond
                    ((and (integerp new-val) (val-pointer-p new-val))
                     (decode-pointer new-val))
                    ((integerp new-val) new-val)
                    (t nil))))
      (when (and target (rt-large-obj-addr-p heap target))
        (rt-los-card-mark-dirty heap obj-addr)))))

(defun rt-los-allocation-descriptors (heap)
  "Return LOS allocation descriptors for HEAP."
  (copy-list (gethash heap *rt-los-allocations*)))
