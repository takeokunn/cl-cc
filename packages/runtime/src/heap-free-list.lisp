(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Segregated free lists and slabs (FR-359, FR-362)
;;; ------------------------------------------------------------

(defun rt-free-list-size-class-words (size-words)
  "Round SIZE-WORDS up to the next FR-156 size class, or return NIL.

The segregated classes cover 8..1024 bytes (1..128 heap words).  Larger blocks
remain in the canonical free-list fallback so non-class sizes keep working."
  (check-type size-words (integer 1 *))
  (loop for class across *rt-free-list-size-class-words*
        when (<= size-words class)
          return class))

(defun rt-free-list-bin-index (size-words)
  "Return the FR-156 size-class bin index for SIZE-WORDS.

The first eight indices are the required 8..1024 byte buckets. Larger sizes map
to compatibility fallback buckets so pre-existing oversized free-list reuse keeps
working."
  (check-type size-words (integer 1 *))
  (or (loop for class across *rt-free-list-size-class-words*
            for index from 0
            when (<= size-words class)
              return index)
      (loop for class across *rt-free-list-fallback-class-words*
            for index from (length *rt-free-list-size-class-words*)
            when (<= size-words class)
              return index)
      (1- +rt-free-list-bin-count+)))

(defun rt-heap-free-list-blocks (heap)
  "Return all old-space free blocks from the canonical free-list mirror."
  (copy-list (rt-heap-free-list heap)))

(defun %rt-free-list-sync-mirror (heap)
  "Compatibility hook: the heap free-list is the canonical mirror in FR-156."
  (rt-heap-free-list heap))

(defun %rt-free-list-ensure-bins (heap)
  (or (rt-heap-free-bins heap)
      (setf (rt-heap-free-bins heap)
            (make-array +rt-free-list-bin-count+ :initial-element nil))))

(defun %rt-free-list-remove-block (heap block)
  "Remove BLOCK from the canonical mirror and its size-class bin."
  (setf (rt-heap-free-list heap)
        (delete block (rt-heap-free-list heap) :test #'eq))
  (let ((index (rt-free-list-bin-index (car block)))
        (bins (rt-heap-free-bins heap)))
    (when (and index bins)
      (setf (svref bins index)
            (delete block (svref bins index) :test #'eq))))
  block)

(defun %rt-free-list-insert-block (heap block)
  "Insert BLOCK into the canonical mirror and, when class-sized, its bucket."
  (push block (rt-heap-free-list heap))
  (let ((index (rt-free-list-bin-index (car block))))
    (when index
      (push block (svref (%rt-free-list-ensure-bins heap) index)))
    index))

(defun %rt-free-list-record-remainder (heap addr size-words)
  "Record a split free-list remainder and install a scannable free header."
  (when (plusp size-words)
    (rt-heap-set-header heap addr (make-header size-words 0 0))
    (%rt-free-list-insert-block heap (cons size-words addr))))

(defun %rt-free-list-rebuild-bins (heap blocks)
  (let ((bins (%rt-free-list-ensure-bins heap)))
    (fill bins nil)
    (setf (rt-heap-free-list heap) (copy-list blocks))
    (dolist (block blocks)
      (let ((index (rt-free-list-bin-index (car block))))
        (when index
          (push block (svref bins index)))))
    bins))

(defun rt-free-list-insert (heap size-words addr)
  "Insert free block (SIZE-WORDS . ADDR) into the appropriate segregated bin."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (check-type addr integer)
  (%rt-free-list-insert-block heap (cons size-words addr)))

(defun rt-free-list-find (heap size-words)
  "Find and remove an old-space free block for SIZE-WORDS.

For FR-156 class-sized requests (8..1024 bytes), round to the next size class
and check that bucket first for O(1) reuse.  Larger compatible buckets are only
consulted if the target bucket has no suitable block, preserving existing
free-list semantics before callers fall back to bump-pointer allocation."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let ((index (rt-free-list-bin-index size-words)))
    (if index
        (let ((bins (rt-heap-free-bins heap)))
          (when bins
            (loop for bin-index from index below (length bins) do
              (loop with prev = nil
                    for cell on (svref bins bin-index)
                    for block = (car cell)
                    for block-size = (car block)
                    for block-addr = (cdr block)
                    when (>= block-size size-words) do
                      (let ((remainder (- block-size size-words)))
                        (if prev
                            (setf (cdr prev) (cdr cell))
                            (setf (svref bins bin-index) (cdr cell)))
                        (setf (rt-heap-free-list heap)
                              (delete block (rt-heap-free-list heap) :test #'eq))
                        (%rt-free-list-record-remainder heap (+ block-addr size-words) remainder)
                        (return-from rt-free-list-find (values bin-index block-addr)))
                    do (setf prev cell)))))
        (loop for cell on (rt-heap-free-list heap)
              for block = (car cell)
              for block-size = (car block)
              for block-addr = (cdr block)
              when (>= block-size size-words) do
                (let ((remainder (- block-size size-words)))
                  (%rt-free-list-remove-block heap block)
                  (%rt-free-list-record-remainder heap (+ block-addr size-words) remainder)
                  (return-from rt-free-list-find (values nil block-addr))))))
  (values nil nil))

(defun rt-free-list-alloc-from-bin (heap size-words)
  "Pop a block from SIZE-WORDS' FR-156 size-class bin.

SIZE-WORDS is rounded to the next 8..1024 byte size class before lookup.  Only
the matching segregated bin is consulted; callers that receive NIL can fall back
to old-space bump allocation without scanning larger bins.  Returns
  (values CLASS-SIZE ADDR), or (values NIL NIL) when the request has no FR-156
  class or the target bin is empty.  The returned size is the actual allocated
  size (which may be SIZE-WORDS, not necessarily CLASS-SIZE)."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let ((class-size (rt-free-list-size-class-words size-words)))
    (unless class-size
      (return-from rt-free-list-alloc-from-bin (values nil nil)))
    (let* ((index (rt-free-list-bin-index class-size))
           (bins (rt-heap-free-bins heap))
           (block (and bins (pop (svref bins index)))))
      (unless block
        (return-from rt-free-list-alloc-from-bin (values nil nil)))
      (let ((block-size (car block))
            (block-addr (cdr block)))
        ;; Safety: reject blocks smaller than the actual request
        (when (< block-size size-words)
          (setf (rt-heap-free-list heap)
                (delete block (rt-heap-free-list heap) :test #'eq))
          (return-from rt-free-list-alloc-from-bin (values nil nil)))
        (setf (rt-heap-free-list heap)
              (delete block (rt-heap-free-list heap) :test #'eq))
        ;; Split at actual request size, not class-size
        (when (> block-size size-words)
          (%rt-free-list-record-remainder heap
                                          (+ block-addr size-words)
                                          (- block-size size-words)))
        (values size-words block-addr)))))

(defun rt-slab-size-class (type-tag size-words)
  "Return the slab class key for a known fixed-size runtime allocation."
  (declare (ignore type-tag))
  (car (find size-words *rt-slab-size-classes* :key #'cdr :test #'=)))

(defun %rt-slab-allocate-page (heap class-size)
  (let* ((slots (max 1 (floor +rt-slab-page-words+ class-size)))
         (page-words (* slots class-size))
         (addr (or (nth-value 1 (rt-free-list-find heap page-words))
                   (let ((old-free (rt-heap-old-free heap)))
                     (when (> (+ old-free page-words)
                              (+ (rt-heap-old-base heap) (rt-heap-old-size heap)))
                       (error "cl-cc/runtime: slab page allocation exhausted old space — ~D words requested"
                              page-words))
                     (setf (rt-heap-old-free heap) (+ old-free page-words))
                     old-free)))
         (limit (+ addr page-words))
         (free nil))
    (loop for slot from addr below limit by class-size do
      (rt-heap-set-header heap slot (make-header class-size 0 0))
      (push slot free))
    (%make-rt-slab :class-size class-size
                   :free-list free
                   :slab-base addr
                   :slab-limit limit)))

(defun rt-slab-alloc (heap size-class)
  "Allocate one fixed-size object slot from SIZE-CLASS slab pool."
  (check-type heap rt-heap)
  (let* ((class-size (or (cdr (assoc size-class *rt-slab-size-classes*))
                         (and (integerp size-class) size-class))))
    (unless (and (integerp class-size) (plusp class-size))
      (error "cl-cc/runtime: unknown slab size class ~S" size-class))
    (let* ((pools (rt-heap-slab-pools heap))
           (slabs (gethash size-class pools))
           (slab (or (find-if #'rt-slab-free-list slabs)
                     (let ((new-slab (%rt-slab-allocate-page heap class-size)))
                       (push new-slab (gethash size-class pools))
                       new-slab)))
           (addr (pop (rt-slab-free-list slab))))
      addr)))

(defun rt-slab-free (heap size-class addr)
  "Return ADDR to SIZE-CLASS slab free list and clear its words."
  (check-type heap rt-heap)
  (check-type addr integer)
  (let* ((class-size (or (cdr (assoc size-class *rt-slab-size-classes*))
                         (and (integerp size-class) size-class)))
         (slab (find-if (lambda (s)
                          (and (= (rt-slab-class-size s) class-size)
                               (<= (rt-slab-slab-base s) addr)
                               (< addr (rt-slab-slab-limit s))))
                        (gethash size-class (rt-heap-slab-pools heap)))))
    (unless slab
      (error "cl-cc/runtime: address ~D is not in slab class ~S" addr size-class))
    (loop for i from addr below (+ addr class-size) do
      (rt-heap-set heap i 0))
    (rt-heap-set-header heap addr (make-header class-size 0 0))
    (pushnew addr (rt-slab-free-list slab) :test #'eql)
    addr))
