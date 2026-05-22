;;;; packages/pipeline/src/arena.lisp — Per-compilation arena allocator (FR-625)

(in-package :cl-cc/pipeline)

(defparameter *default-compilation-arena-size* (* 1024 1024)
  "Default per-compilation arena size in bytes.")

(defvar *current-compilation-arena* nil
  "Dynamically bound arena used for temporary compiler objects.")

(defstruct (compilation-arena (:constructor %make-compilation-arena))
  (buffer (make-array 0 :element-type '(unsigned-byte 8) :fill-pointer 0)
          :type vector)
  (size 0 :type fixnum))

(defun make-compilation-arena (&optional (arena-size *default-compilation-arena-size*))
  "Create a byte-vector-backed bump-pointer arena of ARENA-SIZE bytes."
  (check-type arena-size (integer 1 *))
  (%make-compilation-arena
   :buffer (make-array arena-size
                       :element-type '(unsigned-byte 8)
                       :fill-pointer 0
                       :initial-element 0)
   :size arena-size))

(defun arena-reset (&optional (arena *current-compilation-arena*))
  "Reset ARENA instantly by setting its fill pointer to zero."
  (check-type arena compilation-arena)
  (setf (fill-pointer (compilation-arena-buffer arena)) 0)
  arena)

(defun arena-alloc (size &optional (arena *current-compilation-arena*))
  "Allocate SIZE bytes from ARENA and return the byte offset."
  (check-type size (integer 0 *))
  (check-type arena compilation-arena)
  (let* ((buffer (compilation-arena-buffer arena))
         (offset (fill-pointer buffer))
         (next (+ offset size)))
    (when (> next (compilation-arena-size arena))
      (error "cl-cc/pipeline: compilation arena exhausted — ~D bytes requested, ~D bytes available"
             size (- (compilation-arena-size arena) offset)))
    (setf (fill-pointer buffer) next)
    offset))

(defmacro with-compilation-arena ((&key (size '*default-compilation-arena-size*) arena) &body body)
  "Bind *CURRENT-COMPILATION-ARENA* while compiling temporary AST/MIR data.

The arena is reset in UNWIND-PROTECT so temporary allocations are freed at the
end of the compilation scope without affecting existing allocation behavior."
  (let ((arena-var (gensym "ARENA")))
    `(let* ((,arena-var (or ,arena (make-compilation-arena ,size)))
            (*current-compilation-arena* ,arena-var))
       (unwind-protect
            (progn ,@body)
         (arena-reset ,arena-var)))))
