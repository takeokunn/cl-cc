;;;; GPU Compute Offloading Runtime API (FR-400)
(in-package :cl-cc/runtime)

(defparameter +rt-gpu-devices+ '(:cpu :metal :vulkan))

(defstruct rt-gpu-buffer
  (data (make-array 0 :element-type '(unsigned-byte 8)) :type vector)
  (size 0 :type integer)
  (device :cpu :type keyword))

(defstruct rt-gpu-kernel
  (name nil)
  (source "" :type string)
  (grid-dims '(1 1 1) :type list)
  (block-dims '(1 1 1) :type list))

(defun %rt-gpu-check-device (device)
  (unless (member device +rt-gpu-devices+)
    (error "Unsupported runtime GPU device: ~a" device))
  device)

(defun %rt-gpu-check-size (size)
  (unless (and (integerp size) (not (minusp size)))
    (error "GPU buffer size must be a non-negative integer: ~a" size))
  size)

(defun %rt-gpu-check-buffer (buffer)
  (unless (rt-gpu-buffer-p buffer)
    (error "Expected rt-gpu-buffer, got: ~s" buffer))
  buffer)

(defun %rt-gpu-check-dims (dims name)
  (unless (and (listp dims)
               (= (length dims) 3)
               (every (lambda (dim)
                        (and (integerp dim) (plusp dim)))
                      dims))
    (error "~a must be a list of three positive integers: ~s" name dims))
  dims)

(defun rt-gpu-buffer-alloc (size &key (device :cpu))
  "Allocate an RT-GPU-BUFFER of SIZE bytes on DEVICE.

DEVICE is a runtime backend keyword: :CPU, :METAL, or :VULKAN. The current
portable implementation stores all buffer bytes in a host byte vector while
preserving the requested device tag for VM/runtime dispatch."
  (%rt-gpu-check-size size)
  (%rt-gpu-check-device device)
  (make-rt-gpu-buffer
   :data (make-array size :element-type '(unsigned-byte 8) :initial-element 0)
   :size size
   :device device))

(defun rt-gpu-buffer-copy (src dst &key (src-offset 0) (dst-offset 0) size)
  "Copy bytes from SRC to DST and return DST.

SRC and DST must be RT-GPU-BUFFER objects. SRC-OFFSET and DST-OFFSET are byte
offsets into each buffer. SIZE defaults to the largest byte count that fits in
both buffers from the supplied offsets."
  (%rt-gpu-check-buffer src)
  (%rt-gpu-check-buffer dst)
  (%rt-gpu-check-size src-offset)
  (%rt-gpu-check-size dst-offset)
  (let* ((src-available (- (rt-gpu-buffer-size src) src-offset))
         (dst-available (- (rt-gpu-buffer-size dst) dst-offset))
         (count (or size (min src-available dst-available))))
    (%rt-gpu-check-size count)
    (when (or (minusp src-available)
              (minusp dst-available)
              (> count src-available)
              (> count dst-available))
      (error "GPU buffer copy out of bounds: src-offset=~a dst-offset=~a size=~a"
             src-offset dst-offset count))
    (replace (rt-gpu-buffer-data dst)
             (rt-gpu-buffer-data src)
             :start1 dst-offset
             :end1 (+ dst-offset count)
             :start2 src-offset
             :end2 (+ src-offset count))
    dst))

(defun rt-gpu-buffer-free (buffer)
  "Release BUFFER's runtime storage and return T.

The portable runtime has no native device allocation to release, so this marks
the buffer as an empty CPU buffer and drops its byte-vector reference."
  (%rt-gpu-check-buffer buffer)
  (setf (rt-gpu-buffer-data buffer)
        (make-array 0 :element-type '(unsigned-byte 8))
        (rt-gpu-buffer-size buffer) 0
        (rt-gpu-buffer-device buffer) :cpu)
  t)

(defun rt-gpu-kernel-compile (source &key (backend :cpu))
  "Compile shader SOURCE for BACKEND and return the compiled representation.

FR-400 currently provides a portable stub compiler. It validates the requested
backend keyword and returns SOURCE unchanged; real Metal/SPIR-V/Vulkan lowering
belongs in emitter backends rather than the runtime."
  (%rt-gpu-check-device backend)
  source)

(defun %rt-gpu-kernel-callback (kernel)
  (let ((name (rt-gpu-kernel-name kernel)))
    (cond
      ((functionp name) name)
      ((and (symbolp name) (fboundp name)) (symbol-function name))
      (t nil))))

(defun %rt-gpu-dispatch-cpu (kernel buffers)
  (let* ((grid-dims (%rt-gpu-check-dims (rt-gpu-kernel-grid-dims kernel) "GPU grid dimensions"))
         (block-dims (%rt-gpu-check-dims (rt-gpu-kernel-block-dims kernel) "GPU block dimensions"))
         (callback (%rt-gpu-kernel-callback kernel))
         (dispatch-count 0))
    (destructuring-bind (grid-x grid-y grid-z) grid-dims
      (destructuring-bind (block-x block-y block-z) block-dims
        (loop for gz below grid-z do
          (loop for gy below grid-y do
            (loop for gx below grid-x do
              (loop for tz below block-z do
                (loop for ty below block-y do
                  (loop for tx below block-x do
                    (incf dispatch-count)
                    (when callback
                      (apply callback
                             gx gy gz
                             block-x block-y block-z
                             tx ty tz
                             buffers))))))))))
    dispatch-count))

(defun rt-gpu-launch-kernel (kernel &rest buffers)
  "Launch KERNEL with BUFFERS and return the simulated dispatch count.

The FR-400 portable runtime executes a CPU fallback: it iterates over every
grid/block/thread coordinate. If KERNEL's name is a function designator, that
function is invoked once per simulated work item with arguments
GX GY GZ BLOCK-X BLOCK-Y BLOCK-Z TX TY TZ followed by BUFFERS. If no callback
is available, launch still validates inputs and simulates completion."
  (unless (rt-gpu-kernel-p kernel)
    (error "Expected rt-gpu-kernel, got: ~s" kernel))
  (dolist (buffer buffers)
    (%rt-gpu-check-buffer buffer))
  (%rt-gpu-dispatch-cpu kernel buffers))

(defun rt-gpu-launch-async (kernel &rest buffers)
  "Launch KERNEL asynchronously with BUFFERS and return an RT-FUTURE.

The returned future resolves to the same value as RT-GPU-LAUNCH-KERNEL. When
the runtime scheduler is available the launch is spawned cooperatively;
otherwise the portable fallback resolves the future immediately after running
the CPU executor."
  (let ((future (rt-make-future)))
    (flet ((complete ()
             (rt-future-resolve future
               (apply #'rt-gpu-launch-kernel kernel buffers))))
      (if (fboundp 'rt-spawn)
          (funcall (symbol-function 'rt-spawn) #'complete)
          (complete)))
    future))
