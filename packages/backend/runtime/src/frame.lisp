;;;; packages/backend/runtime/src/frame.lisp - Fixed-Size Register Array and Stack Frame
;;;
;;; Implements the vm-frame structure (256 integer-indexed registers) and
;;; a pre-allocated frame pool to reduce GC pressure during function calls.
;;;
;;; Calling convention:
;;;   R0-R15   : caller-save (scratch)
;;;   R16-R31  : callee-save (preserved across calls)
;;;   R0-R7    : argument registers (R0 = first arg)
;;;   R0       : return value register
;;;   R32+     : spill slots (managed by register allocator)
;;;
;;; Register indices are plain fixnums 0..255 (not keywords).

(in-package :cl-cc/runtime)

(declaim (optimize (speed 3) (safety 1)))

;;; ------------------------------------------------------------
;;; Register count constants
;;; ------------------------------------------------------------

(defconstant +frame-register-count+ 256
  "Total number of register slots per frame.")

(defconstant +frame-caller-save-start+  0
  "First caller-save register index.")
(defconstant +frame-caller-save-end+   15
  "Last caller-save register index (R0-R15).")

(defconstant +frame-callee-save-start+ 16
  "First callee-save register index.")
(defconstant +frame-callee-save-end+   31
  "Last callee-save register index (R16-R31).")

(defconstant +frame-arg-start+  0
  "First argument register (R0).")
(defconstant +frame-arg-end+    7
  "Last argument register (R7); R0-R7 pass up to 8 arguments.")

(defconstant +frame-return-reg+ 0
  "Return value register (R0).")

(defconstant +frame-spill-start+ 32
  "First spill-slot register (R32+).")

;;; ------------------------------------------------------------
;;; vm-frame defstruct
;;; ------------------------------------------------------------

(defstruct (vm-frame
            (:constructor %make-vm-frame ())
            (:conc-name vm-frame-))
  "A single activation frame: fixed 256-slot register file plus meta-fields.

   Fields:
     registers    — simple-vector of +frame-register-count+ NaN-boxed values
     sp           — stack pointer (index into a separate value stack, if used)
     pc           — program counter (instruction index in the current code vector)
     closure      — the currently executing closure object (or NIL)
     return-frame — the caller's vm-frame (NIL at top level)"
  ;; Specialized (unsigned-byte 64) array: SBCL stores values unboxed,
  ;; eliminating heap allocation on every register read/write.
  (registers    (make-array +frame-register-count+
                            :element-type '(unsigned-byte 64)
                            :initial-element +val-nil+)
                :type (simple-array (unsigned-byte 64) (*)))
  (sp           0   :type fixnum)
  (pc           0   :type fixnum)
  (closure      nil)
  (return-frame nil :type (or null vm-frame)))

;;; ------------------------------------------------------------
;;; Frame pool
;;; ------------------------------------------------------------

(defconstant +frame-pool-size+ 1024
  "Number of vm-frames pre-allocated in the pool.")

(defvar *frame-pool*
  (make-array +frame-pool-size+ :initial-element nil)
  "Pre-allocated pool of vm-frame objects.
   Treated as a stack; *frame-pool-top* tracks the next free slot.")

(defvar *frame-pool-top* 0
  "Index of the next frame to hand out from *frame-pool*.
   0 means the pool is empty; +frame-pool-size+ means fully stocked.")

(declaim (type simple-vector *frame-pool*))
(declaim (type fixnum *frame-pool-top*))

(defun initialize-frame-pool ()
  "Pre-allocate all frames in *frame-pool* and reset the pool top.
   Call once at VM startup (or after a GC that clears the pool)."
  (dotimes (i +frame-pool-size+)
    (setf (svref *frame-pool* i) (%make-vm-frame)))
  (setf *frame-pool-top* +frame-pool-size+)
  (values))

;;; ------------------------------------------------------------
;;; Pool acquire / release
;;; ------------------------------------------------------------

(declaim (ftype (function () vm-frame) frame-pool-acquire))
(declaim (ftype (function (vm-frame) (values)) frame-pool-release))
(declaim (inline frame-pool-acquire frame-pool-release))

(defun frame-pool-acquire ()
  "Return a clean vm-frame from the pool.
   If the pool is exhausted, allocate a fresh frame on the heap."
  (declare (type fixnum *frame-pool-top*))
  (if (> *frame-pool-top* 0)
      (let ((idx (decf *frame-pool-top*)))
        (declare (type fixnum idx))
        (the vm-frame (svref *frame-pool* idx)))
      ;; Pool exhausted: allocate fresh (will be GC'd normally).
      (%make-vm-frame)))

(defun frame-pool-release (frame)
  "Zero-fill FRAME's registers and return it to the pool (if not full)."
  (declare (type vm-frame frame))
  ;; Reset meta-fields.
  (setf (vm-frame-sp frame)           0
        (vm-frame-pc frame)           0
        (vm-frame-closure frame)      nil
        (vm-frame-return-frame frame) nil)
  ;; Zero-fill register array. Uses fill for potential SIMD vectorization by SBCL.
  (fill (vm-frame-registers frame) +val-nil+)
  ;; Return to pool if there is room.
  (when (< *frame-pool-top* +frame-pool-size+)
    (let ((idx *frame-pool-top*))
      (declare (type fixnum idx))
      (setf (svref *frame-pool* idx) frame)
      (incf *frame-pool-top*)))
  (values))

;;; ------------------------------------------------------------
;;; Register access
;;; ------------------------------------------------------------

(declaim (ftype (function (vm-frame fixnum) (unsigned-byte 64)) frame-reg-get))
(declaim (ftype (function (vm-frame fixnum (unsigned-byte 64)) (unsigned-byte 64))
                frame-reg-set))
(declaim (inline frame-reg-get frame-reg-set))

(defun frame-reg-get (frame idx)
  "Read the NaN-boxed value stored in register IDX (0..255) of FRAME."
  (declare (type vm-frame frame)
           (type fixnum idx))
  (the (unsigned-byte 64)
       (aref (vm-frame-registers frame) idx)))

(defun frame-reg-set (frame idx val)
  "Write the NaN-boxed value VAL into register IDX (0..255) of FRAME.
   Returns VAL."
  (declare (type vm-frame frame)
           (type fixnum idx)
           (type (unsigned-byte 64) val))
  (setf (aref (vm-frame-registers frame) idx) val)
  val)

;;; ------------------------------------------------------------
;;; Convenience: reset a frame for reuse without releasing to pool
;;; ------------------------------------------------------------

(defun frame-reset (frame)
  "Clear all registers and meta-fields of FRAME in place.
   Useful when a frame is reused for a tail call."
  (declare (type vm-frame frame))
  (setf (vm-frame-sp frame) 0
        (vm-frame-pc frame) 0
        (vm-frame-closure frame) nil
        (vm-frame-return-frame frame) nil)
  (fill (vm-frame-registers frame) +val-nil+)
  frame)
