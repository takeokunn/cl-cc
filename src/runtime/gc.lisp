;;;; src/runtime/gc.lisp - 2-Generation Generational GC for CL-CC Native Runtime
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
    (when (eq state :major-gc)
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

;;; ------------------------------------------------------------
;;; Section 5: Major GC — Tri-color Mark-Sweep
;;; ------------------------------------------------------------

(defun %gc-sweep-old-space (heap)
  "Sweep the old generation: reclaim all unmarked (dead) objects by adding them
   to the free-list, and clear the mark bit on all live (marked) objects."
  (let ((addr     (rt-heap-old-base heap))
        (old-free (rt-heap-old-free heap))
        (freed    0))
    (loop while (< addr old-free) do
      (let ((h (rt-heap-object-header heap addr)))
        (cond
          ((header-forwarding-p h)
           ;; Forwarding pointer should not appear in old space — skip 1 word
           (incf addr 1))
          ((not (integerp h))
           ;; Non-integer, non-cons header — stop
           (return))
          ((zerop (header-size h))
           ;; Zero size header (uninitialized region) — stop
           (return))
          ((header-marked-p h)
           ;; Live object: clear mark bit and advance
           (rt-heap-set-header heap addr (header-clear-mark h))
           (incf addr (header-size h)))
          (t
           ;; Dead object: reclaim to free-list
           (let ((size (header-size h)))
             (push (cons size addr) (rt-heap-free-list heap))
             (incf freed size)
             (incf addr size))))))
    (incf (rt-heap-words-collected heap) freed)))

(defun rt-gc-major-collect (heap)
  "Perform a major GC of the old generation using tri-color mark-and-sweep.

   Algorithm:
   1. Set gc-state to :major-gc.
   2. Initial mark: grey all old-space objects directly reachable from roots
      and from young-space roots (scan young objects reachable from roots).
   3. Drain the SATB queue into the grey set (SATB invariant).
   4. Marking loop: repeatedly pop a grey object, mark it black, and grey all
      unvisited old-space children.
   5. Sweep: scan old space linearly, reclaiming unmarked objects to free-list
      and clearing mark bits on survivors.
   6. Reset gc-state to :normal and increment major-gc-count."
  (setf (rt-heap-gc-state heap) :major-gc)
  (unwind-protect
      (let ((gray-queue nil))
        (flet ((maybe-gray (addr)
                 "Push ADDR onto gray-queue if it is an unmarked, ungray old-space object."
                 (when (and (integerp addr) (rt-old-addr-p heap addr))
                   (let ((h (rt-heap-object-header heap addr)))
                     (when (and (integerp h)
                                (not (header-marked-p h))
                                (not (header-gray-p h)))
                       (rt-heap-set-header heap addr (header-set-gray h))
                       (push addr gray-queue))))))
          ;; Step 1: Initial mark from roots
          ;; Grey old-space objects directly referenced by roots; also trace
          ;; through young objects so old-space objects reachable via young
          ;; generation are not silently missed.
          (dolist (root-cell (rt-heap-roots heap))
            (let ((val (cdr root-cell)))
              (cond
                ;; Root directly references old space
                ((and (integerp val) (rt-old-addr-p heap val))
                 (maybe-gray val))
                ;; Root references young space — trace its pointer slots into old space
                ((and (integerp val) (rt-young-addr-p heap val))
                 (let ((h (rt-heap-object-header heap val)))
                   (when (and (integerp h) (> (header-size h) 0))
                     (dolist (offset (rt-object-pointer-slots heap val))
                       (maybe-gray (rt-heap-ref heap (+ val offset))))))))))
          ;; Step 2: Drain SATB queue
          (dolist (ptr (rt-heap-satb-queue heap))
            (maybe-gray ptr))
          (setf (rt-heap-satb-queue heap) nil)
          ;; Step 3: Marking loop — grey -> black, push unvisited children
          (loop while gray-queue do
            (let* ((addr   (pop gray-queue))
                   (header (rt-heap-object-header heap addr)))
              ;; Mark black: set mark bit, clear gray bit
              (when (integerp header)
                (rt-heap-set-header heap addr
                                    (header-clear-gray (header-set-mark header)))
                ;; Trace pointer slots
                (dolist (offset (rt-object-pointer-slots heap addr))
                  (let* ((child-addr (rt-heap-ref heap (+ addr offset))))
                    (maybe-gray child-addr))))))
          ;; Step 4: Sweep old space
          (%gc-sweep-old-space heap)))
    ;; Always reset gc-state, even on error
    (setf (rt-heap-gc-state heap) :normal))
  (incf (rt-heap-major-gc-count heap)))

;;; ------------------------------------------------------------
;;; Section 6: GC Statistics
;;; ------------------------------------------------------------

(defun rt-gc-stats (heap)
  "Return a plist of current GC statistics for HEAP.

   Keys:
     :minor-gc-count  - number of minor GCs performed
     :major-gc-count  - number of major GCs performed
     :words-collected - total words reclaimed across all GC cycles
     :words-promoted  - total words promoted from young to old generation
     :young-used      - words currently live in young from-space
     :young-total     - total capacity of one young semi-space
     :old-used        - words currently allocated in old space
     :old-total       - total capacity of old space
     :free-list-count - number of free-list entries in old space"
  (list :minor-gc-count  (rt-heap-minor-gc-count heap)
        :major-gc-count  (rt-heap-major-gc-count heap)
        :words-collected (rt-heap-words-collected heap)
        :words-promoted  (rt-heap-words-promoted heap)
        :young-used      (- (rt-heap-young-free heap)
                            (rt-heap-young-from-base heap))
        :young-total     (rt-heap-young-semi-size heap)
        :old-used        (- (rt-heap-old-free heap)
                            (rt-heap-old-base heap))
        :old-total       (rt-heap-old-size heap)
        :free-list-count (length (rt-heap-free-list heap))))
