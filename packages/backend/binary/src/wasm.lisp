;;;; packages/backend/binary/src/wasm.lisp - WASM Binary Format Utilities
;;;
;;; LEB128 encoding, byte buffer helpers, and IEEE754 double-float bit
;;; manipulation used by the WASM emit pipeline and tests.
;;; The emit pipeline uses WAT text format (see wasm-trampoline-emit.lisp).
;;;
;;; Reference: https://webassembly.github.io/spec/core/binary/index.html

(in-package :cl-cc/binary)

;;; ------------------------------------------------------------
;;; Section 1: LEB128 Encoding
;;; ------------------------------------------------------------

(defun encode-uleb128 (value)
  "Encode VALUE as an unsigned LEB128 byte sequence. Returns a list of bytes."
  (let ((bytes nil))
    (loop
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (zerop value)
            (progn (push byte bytes) (return))
            (push (logior byte #x80) bytes))))
    (nreverse bytes)))

(defun encode-sleb128 (value)
  "Encode VALUE as a signed LEB128 byte sequence. Returns a list of bytes."
  (let ((bytes nil)
        (more t))
    (loop while more do
      (let ((byte (logand value #x7f)))
        (setf value (ash value -7))
        (if (or (and (zerop value) (zerop (logand byte #x40)))
                (and (= value -1) (not (zerop (logand byte #x40)))))
            (setf more nil)
            (setf byte (logior byte #x80)))
        (push byte bytes)))
    (nreverse bytes)))

;;; ------------------------------------------------------------
;;; Section 2: Byte Buffer Helpers
;;; ------------------------------------------------------------

(defun make-wasm-buffer ()
  "Create a fresh WASM byte buffer (adjustable byte array with fill pointer)."
  (make-binary-buffer 0))

(defun wasm-buf-write-uleb128 (buf value)
  "Write VALUE as unsigned LEB128 into BUF."
  (binary-buffer-write-bytes buf (encode-uleb128 value)))

(defun wasm-buf-write-sleb128 (buf value)
  "Write VALUE as signed LEB128 into BUF."
  (binary-buffer-write-bytes buf (encode-sleb128 value)))


(defun portable-double-float-bits (value)
  "Return VALUE as an IEEE754 double bit pattern using portable CL operations."
  (let* ((x (float value 1.0d0))
         (sign-bit (if (minusp (float-sign x 1.0d0)) 1 0)))
    (cond
      ((zerop x)
       (ash sign-bit 63))
      ((not (= x x))
       ;; Quiet NaN with a minimal payload.
       (logior (ash sign-bit 63)
               (ash #x7ff 52)
               #x0008000000000000))
      ((and (= x (* x 2.0d0)) (not (zerop x)))
       ;; Infinity.
       (logior (ash sign-bit 63)
               (ash #x7ff 52)))
      (t
       (multiple-value-bind (significand exponent sign) (integer-decode-float x)
         (declare (ignore sign))
         (let* ((abs-significand (abs significand))
                (unbiased-exp (+ exponent 52))
                (fraction
                  (if (>= unbiased-exp -1022)
                      (- abs-significand (ash 1 52))
                      (ash abs-significand (+ exponent 1074))))
                (exp-field
                  (if (>= unbiased-exp -1022)
                      (+ unbiased-exp 1023)
                      0)))
           (logior (ash sign-bit 63)
                   (ash exp-field 52)
                   (logand fraction #x000fffffffffffff))))))))

(defun wasm-buf-write-f64 (buf value)
  "Write a 64-bit IEEE754 float VALUE into BUF (little-endian)."
  (let* ((bits (portable-double-float-bits value))
         (lo (logand bits #xffffffff))
         (hi (ash bits -32)))
    (loop for shift from 0 to 24 by 8
          do (binary-buffer-write-u8 buf (logand (ash lo (- shift)) #xff)))
    (loop for shift from 0 to 24 by 8
          do (binary-buffer-write-u8 buf (logand (ash hi (- shift)) #xff)))))

