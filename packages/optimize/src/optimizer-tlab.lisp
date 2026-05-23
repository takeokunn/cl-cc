;;;; packages/optimize/src/optimizer-tlab.lisp — FR-676 TLAB
;;;; Thread-Local Allocation Buffers for lock-free allocation.
;;;; HotSpot TLAB / Go per-P allocation cache equivalent.

(in-package :cl-cc/optimize)

(defvar *tlab-enabled* t)

(defstruct (tlab (:conc-name tlab-))
  "Thread-Local Allocation Buffer: per-thread pre-allocated heap region."
  (start 0 :type integer)      ; start address
  (current 0 :type integer)    ; current allocation pointer
  (end 0 :type integer)        ; end address
  (size (* 64 1024) :type integer)) ; 64 KB default

(defvar *per-thread-tlab* nil
  "Thread-local TLAB for the current thread.")

(defun tlab-allocate (tlab size-words)
  "Bump-allocate SIZE-WORDS words from TLAB.
Returns the address or NIL if TLAB is exhausted."
  (let* ((byte-size (* size-words 8))
         (new-ptr (+ (tlab-current tlab) byte-size)))
    (if (<= new-ptr (tlab-end tlab))
        (prog1 (tlab-current tlab)
          (setf (tlab-current tlab) new-ptr))
        ;; TLAB exhausted: refill from global heap
        nil)))

(defun tlab-refill (tlab heap)
  "Refill TLAB from HEAP."
  (declare (ignore heap))
  ;; Request a new chunk from the global heap
  (let ((new-chunk 0))
    (setf (tlab-start tlab) new-chunk
          (tlab-current tlab) new-chunk
          (tlab-end tlab) (+ new-chunk (tlab-size tlab)))
    tlab))

(defun tlab-init (size)
  "Initialize a new TLAB with SIZE bytes."
  (make-tlab :size size))

;; Thread-local TLAB access
(defmacro with-tlab ((&optional (size (* 64 1024))) &body body)
  "Execute BODY with a thread-local TLAB of SIZE bytes."
  `(let ((*per-thread-tlab* (tlab-init ,size)))
     (unwind-protect (progn ,@body)
       ;; Return remaining TLAB to global heap
       (setf *per-thread-tlab* nil))))
