(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Segregated free lists and slabs (FR-359, FR-362)
;;; ------------------------------------------------------------

(defun rt-free-list-bin-index (size-words)
  "Return the segregated free-list bin index for SIZE-WORDS."
  (check-type size-words (integer 1 *))
  (let ((bytes (* size-words 8)))
    (cond
      ((<= bytes 8) 0)
      ((<= bytes 16) 1)
      ((<= bytes 32) 2)
      ((<= bytes 64) 3)
      ((<= bytes 128) 4)
      ((<= bytes 256) 5)
      ((<= bytes 512) 6)
      ((<= bytes 1024) 7)
      ((<= bytes 2048) 8)
      ((<= bytes 4096) 9)
      ((<= bytes 8192) 10)
      ((<= bytes 16384) 11)
      ((<= bytes 32768) 12)
      ((<= bytes 65536) 13)
      ((<= bytes 131072) 14)
      (t 15))))

(defun rt-heap-free-list-blocks (heap)
  "Return all old-space free blocks from segregated bins or the heap mirror."
  (let ((bins (rt-heap-free-bins heap)))
    (if bins
        (loop for i from 0 below (length bins) append (copy-list (svref bins i)))
        (copy-list (rt-heap-free-list heap)))))

(defun %rt-free-list-sync-mirror (heap)
  (setf (rt-heap-free-list heap) (rt-heap-free-list-blocks heap)))

(defun %rt-free-list-rebuild-bins (heap blocks)
  (let ((bins (or (rt-heap-free-bins heap)
                  (setf (rt-heap-free-bins heap)
                        (make-array +rt-free-list-bin-count+ :initial-element nil)))))
    (fill bins nil)
    (dolist (block blocks)
      (let ((index (rt-free-list-bin-index (car block))))
        (push block (svref bins index))))
    (%rt-free-list-sync-mirror heap)
    bins))

(defun rt-free-list-insert (heap size-words addr)
  "Insert free block (SIZE-WORDS . ADDR) into the appropriate segregated bin."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (check-type addr integer)
  (let* ((bins (or (rt-heap-free-bins heap)
                   (setf (rt-heap-free-bins heap)
                         (make-array +rt-free-list-bin-count+ :initial-element nil))))
         (index (rt-free-list-bin-index size-words)))
    (push (cons size-words addr) (svref bins index))
    (%rt-free-list-sync-mirror heap)
    index))

(defun rt-free-list-find (heap size-words)
  "Find and remove a block at least SIZE-WORDS words; return BIN-INDEX and ADDR."
  (check-type heap rt-heap)
  (check-type size-words (integer 1 *))
  (let ((bins (rt-heap-free-bins heap)))
    (when bins
      (loop for i from (rt-free-list-bin-index size-words) below (length bins) do
        (loop with prev = nil
              for cell on (svref bins i)
              for block = (car cell)
              for block-size = (car block)
              for block-addr = (cdr block)
              when (>= block-size size-words) do
                (let ((remainder (- block-size size-words)))
                  (if prev
                      (setf (cdr prev) (cdr cell))
                      (setf (svref bins i) (cdr cell)))
                  (when (plusp remainder)
                    (rt-free-list-insert heap remainder (+ block-addr size-words)))
                  (%rt-free-list-sync-mirror heap)
                  (return-from rt-free-list-find (values i block-addr)))
              do (setf prev cell)))))
  (values nil nil))

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
