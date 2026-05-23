(in-package :cl-cc/runtime)

(defconstant +alloc-large-threshold+ 32768)
(defparameter *alloc-size-classes* '(8 16 32 64 128 256 512 1024 2048 4096))
(defun rt-alloc (size) (make-array (max size 1) :element-type '(unsigned-byte 8) :initial-element 0))
(defun rt-free (obj size) (declare (ignore obj size)) nil)
(defun rt-size-class-for (size) (or (find size *alloc-size-classes* :test #'>=) +alloc-large-threshold+))
(defun rt-allocator-init () t)

;;; ── FR-816 Arena allocator ─────────────────────────────────────────────

(defstruct (rt-arena (:constructor %make-rt-arena))
  buffer
  (cursor 0 :type fixnum))

(defstruct (rt-arena-block (:constructor %make-rt-arena-block))
  (offset 0 :type fixnum)
  size)

(defun make-arena (&key (size-hint 4096))
  (%make-rt-arena :buffer (make-array (max size-hint 1) :initial-element nil)
                  :cursor 0))

(defun arena-alloc (arena size-words)
  (let* ((buf (rt-arena-buffer arena))
         (cursor (rt-arena-cursor arena))
         (needed (+ cursor size-words)))
    (when (> needed (length buf))
      (let* ((new-len (* 2 (max needed (length buf))))
             (new-buf (make-array new-len :initial-element nil)))
        (loop for i from 0 below (length buf) do
          (setf (aref new-buf i) (aref buf i)))
        (setf (rt-arena-buffer arena) new-buf)))
    (setf (rt-arena-cursor arena) (+ cursor size-words))
    (%make-rt-arena-block :offset cursor :size size-words)))

(defun arena-reset (arena)
  (setf (rt-arena-cursor arena) 0))

(defmacro with-arena ((name &key (size-hint 4096)) &body body)
  `(let ((,name (make-arena :size-hint ,size-hint)))
     ,@body))

;;; ── FR-817 Object pool ─────────────────────────────────────────────────

(defstruct (rt-object-pool (:constructor %make-rt-object-pool))
  name
  constructor
  (min-size 0 :type fixnum)
  (max-size 16 :type fixnum)
  (pool nil))

(defun make-object-pool (name &key (min-size 0) (max-size 16) (constructor (lambda () nil)))
  (let ((pool (%make-rt-object-pool :name name :constructor constructor
                                    :min-size min-size :max-size max-size)))
    (dotimes (i min-size)
      (push (funcall constructor) (rt-object-pool-pool pool)))
    pool))

(defun pool-acquire (pool)
  (or (pop (rt-object-pool-pool pool))
      (funcall (rt-object-pool-constructor pool))))

(defun pool-release (pool obj)
  (when (< (length (rt-object-pool-pool pool)) (rt-object-pool-max-size pool))
    (push obj (rt-object-pool-pool pool)))
  (values))
