;;;; packages/runtime/src/gc-minor.lisp - Minor GC (Cheney Semi-Space Copying)
;;;
;;; Depends on gc.lisp being loaded first (%gc-copy-object, %gc-ensure-copied).
;;;
;;; The minor collector evacuates young-generation objects from the current
;;; from-space into the to-space (or promotes them to old space when they have
;;; survived enough collection cycles).
;;;
;;; Public API:
;;;   rt-gc-minor-collect   - Cheney copying minor GC (young generation)
;;;
;;; Internal helpers are prefixed with %gc-.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Section 3a: Minor GC Helpers
;;; ------------------------------------------------------------

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

;;; ------------------------------------------------------------
;;; Section 3b: Minor GC Entry Point
;;; ------------------------------------------------------------

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
