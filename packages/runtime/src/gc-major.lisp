;;;; packages/runtime/src/gc-major.lisp — Major GC (tri-color mark-sweep) + GC Statistics
;;;
;;; Contains:
;;;   - %gc-sweep-old-space — sweep dead objects from old generation
;;;   - rt-gc-major-collect — tri-color mark-and-sweep full GC
;;;   - rt-gc-stats — GC statistics plist
;;;
;;; Minor GC (Cheney copying), write barrier, allocation, and root registration
;;; are in gc.lisp (loads before).
;;;
;;; Load order: after gc.lisp.
(in-package :cl-cc/runtime)

(defparameter *rt-concurrent-gc-enabled-p* nil
  "When true, major collection enters :major-gc-concurrent state.")

(defparameter *rt-concurrent-gc-write-barrier-mode* :satb
  "Concurrent-GC barrier mode (:satb or :incremental-update).")

(defparameter *rt-concurrent-gc-stw-phases* '(:initial-mark :final-remark)
  "STW phase model used when concurrent GC mode is enabled.")

(defparameter *rt-concurrent-gc-mutator-assist-p* t
  "When true, mutators are expected to assist concurrent marking progress.")

(defun rt-gc-configure-concurrent-mode (&key enabled-p write-barrier stw-phases mutator-assist-p)
  "Configure concurrent-GC runtime mode and return current settings plist."
  (when (not (null enabled-p))
    (setf *rt-concurrent-gc-enabled-p* enabled-p))
  (when write-barrier
    (setf *rt-concurrent-gc-write-barrier-mode* write-barrier))
  (when stw-phases
    (setf *rt-concurrent-gc-stw-phases* (copy-list stw-phases)))
  (when (not (null mutator-assist-p))
    (setf *rt-concurrent-gc-mutator-assist-p* mutator-assist-p))
  (list :enabled-p *rt-concurrent-gc-enabled-p*
        :write-barrier *rt-concurrent-gc-write-barrier-mode*
        :stw-phases (copy-list *rt-concurrent-gc-stw-phases*)
        :mutator-assist-p *rt-concurrent-gc-mutator-assist-p*))

(defun rt-gc-concurrent-assist (heap &key (budget 16))
  "Perform a bounded mutator-assist step during concurrent major GC.

Marks up to BUDGET old-space addresses currently queued in SATB and returns
the number of addresses processed."
  (if (or (not *rt-concurrent-gc-enabled-p*)
          (not *rt-concurrent-gc-mutator-assist-p*)
          (not (eq (rt-heap-gc-state heap) :major-gc-concurrent)))
      0
      (let ((processed 0)
            (remaining nil))
        (dolist (ptr (rt-heap-satb-queue heap))
          (if (and (< processed budget)
                   (integerp ptr)
                   (rt-old-addr-p heap ptr))
              (let ((h (rt-heap-object-header heap ptr)))
                (when (and (integerp h) (not (header-marked-p h)))
                  (rt-heap-set-header heap ptr (header-set-mark h)))
                (incf processed))
              (push ptr remaining)))
        (setf (rt-heap-satb-queue heap) (nreverse remaining))
        processed)))

;;; Section 5: Major GC — Tri-color Mark-Sweep

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
  (setf (rt-heap-gc-state heap)
        (if *rt-concurrent-gc-enabled-p*
            :major-gc-concurrent
            :major-gc))
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

;;; Section 6: GC Statistics

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
        :free-list-count (length (rt-heap-free-list heap))
        :concurrent-gc-enabled-p *rt-concurrent-gc-enabled-p*
        :concurrent-gc-write-barrier *rt-concurrent-gc-write-barrier-mode*
        :concurrent-gc-stw-phases (copy-list *rt-concurrent-gc-stw-phases*)
        :concurrent-gc-mutator-assist-p *rt-concurrent-gc-mutator-assist-p*))
