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
    ;;; FR-084: Card Table Summarization — 1-level bitmask over card table blocks skips clean cards during minor GC
    (loop with block = 0
          while (< (* block +gc-card-summary-block-size+) num-cards) do
            (if (rt-card-summary-clean-block-p heap block)
                (incf block)
                (let ((block-end (min num-cards
                                      (+ (* block +gc-card-summary-block-size+)
                                         +gc-card-summary-block-size+))))
                  (loop for card-idx from (* block +gc-card-summary-block-size+)
                        below block-end do
                    (when (not (zerop (aref card-table card-idx)))
                      (let* ((card-start (+ old-base (* card-idx +gc-card-size-words+)))
                             (card-end   (min (+ card-start +gc-card-size-words+) old-free))
                             (addr       card-start))
                        (loop while (< addr card-end) do
                          (let ((h (rt-heap-object-header heap addr)))
                            (cond
                              ((header-forwarding-p h)
                               (let* ((fwd-addr  (header-forwarding-ptr h))
                                      (fwd-h     (rt-heap-object-header heap fwd-addr))
                                      (fwd-size  (if (integerp fwd-h) (header-size fwd-h) 1)))
                                 (incf addr (max 1 fwd-size))))
                              ((and (integerp h) (> (header-size h) 0))
                               (let ((size (header-size h)))
                                  (dolist (offset (rt-object-pointer-slots heap addr))
                                    (let* ((slot-addr (+ addr offset))
                                           (val (rt-heap-ref heap slot-addr))
                                            (from-addr (%rt-gc-value-address-for-predicate
                                                        val in-source-p)))
                                      (when from-addr
                                        (rt-heap-set heap slot-addr
                                                     (%rt-gc-rebox-pointer-like
                                                      val
                                                       (%gc-ensure-copied heap from-addr to-free-cell
                                                                          promoted-list-cell
                                                                          in-source-p))))))
                                 (incf addr size)))
                              (t
                               (return))))))))
                  (incf block))))))

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
              ;; Pinned objects are not expected in the active Cheney to-space:
              ;; pinned young objects are promoted by %GC-COPY-OBJECT.  If a
              ;; caller pins an already-copied object defensively skip tracing it
              ;; in this pass; pinning is a relocation barrier.
              (unless (rt-object-pinned-p heap scan)
                (dolist (offset (rt-object-pointer-slots heap scan))
                  (let* ((slot-addr (+ scan offset))
                         (val (rt-heap-ref heap slot-addr))
                          (from-addr (%rt-gc-value-address-for-predicate
                                      val in-source-p)))
                    (when from-addr
                      (rt-heap-set heap slot-addr
                                   (%rt-gc-rebox-pointer-like
                                    val
                                     (%gc-ensure-copied heap from-addr to-free-cell
                                                        promoted-list-cell
                                                        in-source-p)))))))
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
              ((let ((target (%rt-gc-pointer-address heap val)))
                 (and target (rt-young-addr-p heap target)))
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
  (let ((pause-start (get-internal-real-time))
        (promoted-before (rt-heap-words-promoted heap))
        (live-words 0))
  (setf (rt-heap-gc-state heap) :minor-gc)
  ;; FR-343: retire all TLABs before semispace flip so no thread-local buffer
  ;; continues allocating into the evacuation source after this collection.
  (rt-gc-tlab-retire-all heap)
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
        (rt-gc-flush-barrier-buffer heap)
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
            (let* ((val (cdr root-cell))
                   (from-addr
                     (case (%rt-gc-root-type heap root-cell)
                        ((:pointer :any) (%rt-gc-value-address-for-predicate val #'in-source-p))
                       (otherwise nil))))
              (when from-addr
                (setf (cdr root-cell)
                      (%rt-gc-rebox-pointer-like
                        val
                         (%gc-ensure-copied heap from-addr to-free-cell
                                            promoted-list-cell #'in-source-p))))))
          ;; Dynamic binding stacks are thread-local special-variable roots.
          ;; Update each binding in place when its value is evacuated.
          (dolist (thread-state *gc-threads*)
            (dolist (binding (%rt-gc-thread-binding-stack thread-state))
              (let* ((sym (%rt-gc-binding-symbol binding))
                     (skip (and sym
                                (fboundp 'rt-special-variable-global-only-p)
                                (rt-special-variable-global-only-p sym)))
                     (val (%rt-gc-binding-value binding))
                     (from-addr (and (not skip)
                                      (%rt-gc-value-address-for-predicate
                                       val #'in-source-p))))
                (when from-addr
                  (%rt-gc-set-binding-value
                   binding
                    (%rt-gc-rebox-pointer-like
                     val
                     (%gc-ensure-copied heap from-addr to-free-cell
                                        promoted-list-cell #'in-source-p)))))))
          ;; Global special variables are roots too, but they do not require
          ;; thread-local binding-stack scans when marked global-only.
          (when (boundp '*rt-global-var-registry*)
            (maphash
             (lambda (sym val)
                (let ((from-addr (%rt-gc-value-address-for-predicate
                                  val #'in-source-p)))
                 (when from-addr
                   (setf (gethash sym *rt-global-var-registry*)
                          (%rt-gc-rebox-pointer-like
                           val
                           (%gc-ensure-copied heap from-addr to-free-cell
                                              promoted-list-cell #'in-source-p))))))
             *rt-global-var-registry*))
          (when *gc-conservative-roots*
            (dolist (thread-state *gc-threads*)
              (dolist (word (%rt-gc-thread-words thread-state))
                (let ((from-addr (%rt-gc-value-address-for-predicate
                                   word #'in-source-p)))
                  (when from-addr
                    (%gc-ensure-copied heap from-addr to-free-cell
                                       promoted-list-cell #'in-source-p))))))
          ;; Step 2: Drain SATB queue — treat entries as additional roots
          (let ((new-satb nil))
            (dolist (ptr (rt-heap-satb-queue heap))
              (let ((from-addr (%rt-gc-value-address-for-predicate ptr #'in-source-p)))
                (cond
                (from-addr
                  ;; Young pointer in SATB — evacuate it
                  (%gc-ensure-copied heap from-addr to-free-cell
                                     promoted-list-cell #'in-source-p))
                (t
                  ;; Non-young pointer (old space ref) — keep for major GC
                  (push ptr new-satb)))))
            (setf (rt-heap-satb-queue heap) new-satb))
          ;; Step 3: Scan dirty cards in old space for old->young pointers
          (%gc-scan-dirty-cards heap to-free-cell promoted-list-cell #'in-source-p)
          ;; Step 4: Cheney scan loop — breadth-first traversal of to-space
          (%gc-cheney-scan heap evac-target to-free-cell promoted-list-cell #'in-source-p)
          ;; Step 5: Scan promoted objects — update stale pointers, dirty cards
          (%gc-update-promoted heap (car promoted-list-cell))
          ;; Weak references are deliberately not evacuation roots.  Once the
          ;; copying graph is closed, either retarget them to the forwarding
          ;; address of a live referent or clear/drop them when the referent was
          ;; left behind in the evacuation source.
          (when (fboundp 'rt-gc-process-weak-after-minor)
            (rt-gc-process-weak-after-minor heap #'in-source-p))
          ;; Step 6: Commit new young-free
          (setf (rt-heap-young-free heap) (cdr to-free-cell))
          ;; Step 7: Statistics — words collected = semi-size - live words in new from-space
          (setf live-words (- (cdr to-free-cell) evac-target))
          (incf (rt-heap-words-collected heap) (- semi-size live-words))
          ;; Step 8: Clear card table (old->young references re-recorded via write barrier)
          (rt-card-clear-all heap))
        (incf (rt-heap-minor-gc-count heap)))
    ;; Always reset gc-state, even on error
    (setf (rt-heap-gc-state heap) :normal))
  (let* ((promoted-delta (- (rt-heap-words-promoted heap) promoted-before))
         (promotion-ratio (if (plusp live-words)
                              (/ (float promoted-delta 1.0d0) live-words)
                              0.0d0)))
    (rt-gc-dynamic-tenure promotion-ratio)
    (%rt-gc-tune-nursery promotion-ratio))
  (%rt-gc-check-pressure heap)
  (%rt-gc-note-pause heap pause-start)
  (when *gc-verify-after-collect*
    (rt-gc-verify-heap heap))
  heap))
