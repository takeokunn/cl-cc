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

;;; ------------------------------------------------------------
;;; Section 4: Write Barrier — SATB + Card Table
;;; ------------------------------------------------------------

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
    ;; SATB snapshot: during major GC, preserve old pointer before overwrite
    (when (member state '(:major-gc :major-gc-concurrent) :test #'eq)
      (let ((old-val (rt-heap-ref heap slot-addr))
            (h       (rt-heap-object-header heap obj-addr)))
        (when (and (integerp h)
                   (header-marked-p h)
                   (integerp old-val)
                   (rt-heap-addr-p heap old-val))
          (push old-val (rt-heap-satb-queue heap)))))
    ;; Card table: record old->young pointer
    (when (and (integerp obj-addr) (rt-old-addr-p heap obj-addr))
      (when (and (integerp new-val) (rt-young-addr-p heap new-val))
        (rt-card-mark-dirty heap obj-addr)))
    ;; Perform the write
    (rt-heap-set heap slot-addr new-val)))

;;; (%gc-sweep-old-space, rt-gc-major-collect, and rt-gc-stats
;;;  are in gc-major.lisp which loads after this file.)
