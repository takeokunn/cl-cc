;;;; packages/runtime/src/gc-write-barrier.lisp — SATB + Card Table Write Barrier
;;;
;;; Extracted from gc.lisp (Section 4).
;;; Contains: rt-gc-write-barrier — the pre-write barrier combining SATB
;;; snapshot (for major GC tri-color invariant) and card-table update
;;; (for minor GC old->young tracking).
;;;
;;; Depends on:
;;;   heap-trace.lisp — rt-old-addr-p, rt-young-addr-p, rt-card-mark-dirty
;;;   gc.lisp         — rt-heap-gc-state, rt-heap-satb-queue, rt-heap-ref,
;;;                     rt-heap-set, rt-heap-object-header, header-marked-p
;;;
;;; Load order: after gc.lisp, before gc-major.lisp.
(in-package :cl-cc/runtime)

(defvar *rt-gc-satb-thread-queues* (make-hash-table :test #'eq)
  "Heap -> per-thread SATB queues used by concurrent major GC.

Each nested table maps logical thread ids to a private LIFO queue.  In native
backends these queues can be thread-local lock-free buffers; the Pure CL runtime
uses ordinary lists because mutation is cooperative and serialized by the host.")

;;; ------------------------------------------------------------
;;; Section 4: Write Barrier — SATB + Card Table
;;; ------------------------------------------------------------

(defun rt-gc-flush-barrier-buffer (heap)
  "Apply pending batched card marks and clear the barrier buffer."
  (dolist (old-addr (rt-heap-barrier-buffer heap))
    (when (and (integerp old-addr) (rt-old-addr-p heap old-addr))
      (rt-card-mark-dirty heap old-addr)))
  (setf (rt-heap-barrier-buffer heap) nil)
  t)

(defun %rt-gc-satb-thread-table (heap &key create)
  (or (gethash heap *rt-gc-satb-thread-queues*)
      (when create
        (setf (gethash heap *rt-gc-satb-thread-queues*)
              (make-hash-table :test #'equal)))))

(defun rt-gc-satb-enqueue (heap value &optional (thread-id *rt-current-thread-id*))
  "Append VALUE to THREAD-ID's SATB queue for HEAP and return VALUE.

This is the FR-339 pre-write barrier hook preserving the SATB invariant: the
snapshot observed at initial mark remains traceable because overwritten pointer
values are retained until final remark drains all per-thread queues."
  (let ((table (%rt-gc-satb-thread-table heap :create t)))
    (push value (gethash thread-id table)))
  value)

(defun rt-gc-drain-satb-thread-queues (heap)
  "Return and clear all per-thread SATB entries for HEAP."
  (let ((table (%rt-gc-satb-thread-table heap))
        (entries nil))
    (when table
      (maphash (lambda (thread-id queue)
                 (declare (ignore thread-id))
                 (setf entries (nconc queue entries)))
               table)
      (clrhash table))
    entries))

(defun %rt-gc-write-barrier-addr-p (heap value)
  "Conservative heap-address predicate for write-barrier checks.

Returns the heap address encoded in VALUE, or NIL when VALUE is not a heap pointer.
Unlike %RT-GC-POINTER-ADDRESS this does NOT require a valid object header at the
target address — it is safe for write-barrier use where being slightly conservative
(treating a non-pointer word as a potential heap address) is harmless.

Both NaN-boxed pointer values and legacy raw heap addresses are recognized."
  (cond
    ((and (integerp value) (val-pointer-p value))
     (let ((addr (decode-pointer value)))
       (when (rt-heap-addr-p heap addr) addr)))
    ((and (integerp value) (rt-heap-addr-p heap value))
     value)
    (t nil)))

(defun rt-gc-write-barrier (heap obj-addr slot-offset new-val)
  "SATB pre-write barrier combined with card table update.

   Before writing NEW-VAL into slot SLOT-OFFSET of the object at OBJ-ADDR:
   - During :major-gc: if the object is black (marked), snapshot the OLD value
     into the SATB queue so the tri-color invariant is maintained.
   - Always: if OBJ-ADDR is in old space and NEW-VAL is a young address,
     mark the card dirty so the minor GC will scan this card for old->young roots.

   Then performs the actual write."
  (let ((slot-addr (+ obj-addr slot-offset))
        (state     (rt-heap-gc-state heap)))
    ;; Young-to-young writes cannot violate SATB or old->young remembered-set
    ;; invariants, so bypass all barrier bookkeeping.
    ;;; FR-335: Write Barrier Young-to-Young Elision — skips SATB and card marking when both old and new values are in young space
    (when (and (integerp obj-addr) (rt-young-addr-p heap obj-addr)
               (let ((target (%rt-gc-write-barrier-addr-p heap new-val)))
                 (and target (rt-young-addr-p heap target))))
      (return-from rt-gc-write-barrier
        (rt-heap-set heap slot-addr new-val)))
    ;; SATB snapshot: during major GC, preserve old pointer before overwrite
    (when (member state '(:major-gc :major-gc-concurrent) :test #'eq)
      (let ((old-val (rt-heap-ref heap slot-addr))
            (h       (rt-heap-object-header heap obj-addr)))
        (let ((old-addr (%rt-gc-write-barrier-addr-p heap old-val)))
          (when (and (integerp h)
                     (header-marked-p h)
                     old-addr)
             (if (eq state :major-gc-concurrent)
                 (rt-gc-satb-enqueue heap old-val)
                 (push old-val (rt-heap-satb-queue heap)))))))
    ;; Card table: record old->young pointer
    (when (and (integerp obj-addr) (rt-old-addr-p heap obj-addr))
      (when (let ((target (%rt-gc-write-barrier-addr-p heap new-val)))
              (and target (rt-young-addr-p heap target)))
        (if *rt-use-barrier-batching*
            (push obj-addr (rt-heap-barrier-buffer heap))
            (rt-card-mark-dirty heap obj-addr))))
    ;; Perform the write
    (rt-heap-set heap slot-addr new-val)))

;;; (%gc-sweep-old-space, rt-gc-major-collect, and rt-gc-stats
;;;  are in gc-major.lisp which loads after this file.)
