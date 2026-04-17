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

(defun %gc-scan-dirty-cards (heap to-free-cell promoted-list-cell in-source-p)
  "Scan dirty cards in old space and copy any young (source-space) pointer values found.
   IN-SOURCE-P is a predicate that returns true for addresses in the evacuation source."
  (let* ((card-table (rt-heap-card-table heap))
         (old-base   (rt-heap-old-base heap))
         (old-free   (rt-heap-old-free heap))
         (num-cards  (length card-table)))
    (loop for card-idx from 0 below num-cards do
      (when (not (zerop (aref card-table card-idx)))
        (let* ((card-start (+ old-base (* card-idx +gc-card-size-words+)))
               (card-end   (min (+ card-start +gc-card-size-words+) old-free))
               (addr       card-start))
          (loop while (< addr card-end) do
            (let ((h (rt-heap-object-header heap addr)))
              (cond
                ((header-forwarding-p h)
                 ;; Forwarding pointer in old space — skip the entire object size
                 ;; using the forwarded object's header in to-space
                 (let* ((fwd-addr  (header-forwarding-ptr h))
                        (fwd-h     (rt-heap-object-header heap fwd-addr))
                        (fwd-size  (if (integerp fwd-h) (header-size fwd-h) 1)))
                   (incf addr (max 1 fwd-size))))
                ((and (integerp h) (> (header-size h) 0))
                 (let ((size (header-size h)))
                   (dolist (offset (rt-object-pointer-slots heap addr))
                     (let ((val (rt-heap-ref heap (+ addr offset))))
                       (when (and (integerp val) (funcall in-source-p val))
                         (rt-heap-set heap (+ addr offset)
                                      (%gc-ensure-copied heap val to-free-cell
                                                         promoted-list-cell)))))
                   (incf addr size)))
                (t
                 ;; Zero or unrecognized header — stop scanning this card
                 (return))))))))))

(defun %gc-cheney-scan (heap evac-target to-free-cell promoted-list-cell in-source-p)
  "Cheney scan loop: iterate over objects in [evac-target .. to-free-cell) in to-space.
   For each object, update all pointer slots that still point into the evacuation source.
   The upper bound (cdr to-free-cell) grows as new objects are copied, so newly
   copied objects are automatically scanned — this implements breadth-first traversal."
  (let ((scan evac-target))
    (loop while (< scan (cdr to-free-cell)) do
      (let ((h (rt-heap-object-header heap scan)))
        (cond
          ((header-forwarding-p h)
           ;; A forwarding cons at this position — should not appear in to-space,
           ;; but skip 1 word defensively.
           (incf scan 1))
          ((and (integerp h) (> (header-size h) 0))
           (let ((size (header-size h)))
             (dolist (offset (rt-object-pointer-slots heap scan))
               (let ((val (rt-heap-ref heap (+ scan offset))))
                 (when (and (integerp val) (funcall in-source-p val))
                   (rt-heap-set heap (+ scan offset)
                                (%gc-ensure-copied heap val to-free-cell
                                                   promoted-list-cell)))))
             (incf scan size)))
          (t
           ;; Zero header — stop scan
           (return)))))))

(defun %gc-update-promoted (heap promoted-addrs)
  "Post-minor-GC pass: scan all promoted objects (now in old space) and dirty
   their cards for any slots that contain young-space (new from-space) addresses.
   Also update any remaining stale source-space pointers via the forwarding header."
  (dolist (old-addr promoted-addrs)
    (let ((h (rt-heap-object-header heap old-addr)))
      (when (and (integerp h) (> (header-size h) 0))
        (dolist (offset (rt-object-pointer-slots heap old-addr))
          (let ((val (rt-heap-ref heap (+ old-addr offset))))
            (cond
              ;; Slot still holds a forwarding pointer from source space — update it
              ((header-forwarding-p val)
               (let ((new-val (header-forwarding-ptr val)))
                 (rt-heap-set heap (+ old-addr offset) new-val)
                 (when (rt-young-addr-p heap new-val)
                   (rt-card-mark-dirty heap old-addr))))
              ;; Slot holds a live young address — dirty the card
              ((and (integerp val) (rt-young-addr-p heap val))
               (rt-card-mark-dirty heap old-addr)))))))))

(defun rt-gc-minor-collect (heap)
  "Perform a minor (young generation) GC using Cheney semi-space copying.

   Algorithm:
   1. Save the old from-space base (evacuation source).
   2. Flip from/to spaces: new from-space = old to-space.
   3. Copy all root-reachable young objects from the old from-space.
   4. Drain the SATB queue, treating its entries as additional roots.
   5. Scan dirty cards in old space for old->young pointers.
   6. Run the Cheney scan loop until to-free converges.
   7. Scan promoted objects: update stale pointers and dirty their cards.
   8. Update young-free, accumulate stats, clear SATB queue and card table."
  (setf (rt-heap-gc-state heap) :minor-gc)
  (unwind-protect
      (let* (;; Save old from-space range (evacuation source)
             (evac-source         (rt-heap-young-from-base heap))
             (semi-size           (rt-heap-young-semi-size heap))
             ;; Evacuation target = old to-space = will become new from-space
             (evac-target         (rt-heap-young-to-base heap))
             ;; Mutable cell tracking the fill pointer in to-space
             (to-free-cell        (cons nil evac-target))
             ;; Mutable cell holding the list of promoted old-space addresses
             (promoted-list-cell  (cons nil nil)))
        ;; Flip from/to spaces
        (setf (rt-heap-young-from-base heap) evac-target
              (rt-heap-young-to-base   heap) evac-source
              (rt-heap-young-free      heap) evac-target)
        ;; Predicate: is this address in the evacuation source (old from-space)?
        (flet ((in-source-p (addr)
                 (and (>= addr evac-source)
                      (< addr (+ evac-source semi-size)))))
          ;; Step 1: Copy roots
          (dolist (root-cell (rt-heap-roots heap))
            (let ((val (cdr root-cell)))
              (when (and (integerp val) (in-source-p val))
                (setf (cdr root-cell)
                      (%gc-ensure-copied heap val to-free-cell promoted-list-cell)))))
          ;; Step 2: Drain SATB queue — treat entries as additional roots
          (let ((new-satb nil))
            (dolist (ptr (rt-heap-satb-queue heap))
              (cond
                ((and (integerp ptr) (in-source-p ptr))
                 ;; Young pointer in SATB — evacuate it
                 (%gc-ensure-copied heap ptr to-free-cell promoted-list-cell))
                (t
                 ;; Non-young pointer (old space ref) — keep for major GC
                 (push ptr new-satb))))
            (setf (rt-heap-satb-queue heap) new-satb))
          ;; Step 3: Scan dirty cards in old space for old->young pointers
          (%gc-scan-dirty-cards heap to-free-cell promoted-list-cell #'in-source-p)
          ;; Step 4: Cheney scan loop — breadth-first traversal of to-space
          (%gc-cheney-scan heap evac-target to-free-cell promoted-list-cell #'in-source-p)
          ;; Step 5: Scan promoted objects — update stale pointers, dirty cards
          (%gc-update-promoted heap (car promoted-list-cell))
          ;; Step 6: Commit new young-free
          (setf (rt-heap-young-free heap) (cdr to-free-cell))
          ;; Step 7: Statistics — words collected = semi-size - live words in new from-space
          (let ((live-words (- (cdr to-free-cell) evac-target)))
            (incf (rt-heap-words-collected heap) (- semi-size live-words)))
          ;; Step 8: Clear card table (old->young references re-recorded via write barrier)
          (rt-card-clear-all heap))
        (incf (rt-heap-minor-gc-count heap)))
    ;; Always reset gc-state, even on error
    (setf (rt-heap-gc-state heap) :normal)))

;;; Section 4 (Write Barrier — SATB + Card Table: rt-gc-write-barrier)
;;; is in gc-write-barrier.lisp (loads after this file, before gc-major.lisp).

