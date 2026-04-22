;;;; packages/backend/runtime/src/gc.lisp - 2-Generation Generational GC for CL-CC Native Runtime
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
  (declare (ignore type-tag))
  (let* ((from-base (rt-heap-young-from-base heap))
         (semi-size (rt-heap-young-semi-size heap))
         (limit     (+ from-base semi-size))
         (addr      (rt-heap-young-free heap)))
    (when (> (+ addr size-words) limit)
      (rt-gc-minor-collect heap)
      ;; Recompute after GC (bases may have flipped)
      (setf from-base (rt-heap-young-from-base heap)
            limit     (+ from-base semi-size)
            addr      (rt-heap-young-free heap))
      (when (> (+ addr size-words) limit)
        (error "cl-cc/runtime: heap exhausted — ~D words requested, ~D words available in young space"
               size-words (- limit addr))))
    (setf (rt-heap-young-free heap) (+ addr size-words))
    addr))

;;; ------------------------------------------------------------
;;; Section 2: Root Registration
;;; ------------------------------------------------------------

(defun rt-gc-add-root (heap root-cell)
  "Register ROOT-CELL as a GC root.
   ROOT-CELL must be a cons whose cdr holds the heap address to keep live.
   The GC updates (cdr root-cell) in place when the object is moved."
  (push root-cell (rt-heap-roots heap)))

(defun rt-gc-remove-root (heap root-cell)
  "Unregister ROOT-CELL from the GC root set."
  (setf (rt-heap-roots heap)
        (delete root-cell (rt-heap-roots heap) :test #'eq)))

;;; ------------------------------------------------------------
;;; Section 3: Minor GC — Cheney Copying
;;; ------------------------------------------------------------

(defun %gc-copy-object (heap from-addr to-free promoted-list-cell)
  "Copy the object at FROM-ADDR to TO-FREE, or to old space if promotion applies.
   When promoted, push the destination old-space address onto (car PROMOTED-LIST-CELL)
   so the caller can later scan those objects for old->young pointers.
   Installs a forwarding pointer (cons :forwarded dest) at FROM-ADDR slot 0.
   Returns (values new-addr new-to-free)."
  (let* ((header    (rt-heap-object-header heap from-addr))
         (size      (header-size header))
         (age       (header-age header))
         (promote-p (>= age *gc-tenuring-threshold*)))
    (let ((dest-addr
            (if promote-p
                ;; Promote to old space via old-free bump pointer
                (let ((old-free (rt-heap-old-free heap)))
                  (when (>= (+ old-free size)
                             (+ (rt-heap-old-base heap) (rt-heap-old-size heap)))
                    (error "cl-cc/runtime: old space exhausted during promotion"))
                  (setf (rt-heap-old-free heap) (+ old-free size))
                  old-free)
                ;; Copy to to-space
                to-free)))
      ;; Copy all words verbatim
      (loop for i from 0 below size do
        (rt-heap-set heap (+ dest-addr i)
                     (rt-heap-ref heap (+ from-addr i))))
      ;; Increment age in the destination header
      (rt-heap-set-header heap dest-addr
                          (header-increment-age header))
      ;; Install forwarding pointer in source slot 0
      (rt-heap-set-header heap from-addr
                          (header-make-forwarding-ptr dest-addr))
      ;; Track promoted objects for later card-dirtying pass
      (when promote-p
        (push dest-addr (car promoted-list-cell))
        (incf (rt-heap-words-promoted heap) size))
      ;; Return new address and updated to-free
      (values dest-addr
              (if promote-p to-free (+ to-free size))))))

(defun %gc-ensure-copied (heap from-addr to-free-cell promoted-list-cell)
  "Ensure the object at FROM-ADDR has been copied to to-space or old space.
   Returns the new (destination) address.
   Updates (cdr TO-FREE-CELL) in place when a new copy is made in to-space."
  (let ((h (rt-heap-object-header heap from-addr)))
    (cond
      ((header-forwarding-p h)
       ;; Already copied — return existing forwarding destination
       (header-forwarding-ptr h))
      (t
       (multiple-value-bind (new-addr new-free)
           (%gc-copy-object heap from-addr (cdr to-free-cell) promoted-list-cell)
         (setf (cdr to-free-cell) new-free)
         new-addr)))))

;;; Minor GC (%gc-scan-dirty-cards, %gc-cheney-scan, %gc-update-promoted,
;;; rt-gc-minor-collect) is in gc-minor.lisp (loads after this file).

;;; Section 4 (Write Barrier — SATB + Card Table: rt-gc-write-barrier)
;;; is in gc-write-barrier.lisp (loads after gc-minor, before gc-major.lisp).

