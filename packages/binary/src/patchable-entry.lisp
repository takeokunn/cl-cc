;;;; packages/binary/src/patchable-entry.lisp — FR-584 Patchable Function Entry
;;;; Reserve NOP bytes before function entries for runtime patching.
;;;; GCC/Clang -fpatchable-function-entry equivalent.

(in-package :cl-cc/binary)

;;; ──── Configuration ────
(defvar *patchable-entry-before* 0
  "Number of NOP bytes to insert before each function entry.")

(defvar *patchable-entry-after* 0
  "Number of NOP bytes to insert after the function entry (before body).")

;;; ──── NOP emission ────
(defun emit-nop-sequence (stream count)
  "Emit COUNT bytes of NOP instructions into STREAM.
Uses multi-byte NOPs for efficiency:
  1 byte:  90 (NOP)
  2 bytes: 66 90 (NOP)
  3 bytes: 0F 1F 00
  4 bytes: 0F 1F 40 00
  5 bytes: 0F 1F 44 00 00
  ...
  9 bytes: 66 0F 1F 84 00 00 00 00 00"
  (loop while (> count 0)
        for n = (min count 9)
        do (case n
             (9 (progn
                  (write-byte #x66 stream)
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x84 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)))
             (8 (progn
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x84 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)))
             (7 (progn
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x80 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)))
             (6 (progn
                  (write-byte #x66 stream)
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x44 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)))
             (5 (progn
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x44 stream)
                  (write-byte #x00 stream)
                  (write-byte #x00 stream)))
             (4 (progn
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x40 stream)
                  (write-byte #x00 stream)))
             (3 (progn
                  (write-byte #x0F stream)
                  (write-byte #x1F stream)
                  (write-byte #x00 stream)))
             (2 (progn
                  (write-byte #x66 stream)
                  (write-byte #x90 stream)))
             (1 (write-byte #x90 stream)))
        do (decf count n)))

;;; ──── Function entry patching ────
(defun emit-patchable-function-entry (stream)
  "Emit NOP padding before and after the function entry point.
BEFORE: N NOPs for hot-patching (e.g., ftrace, SystemTap, eBPF uprobes).
AFTER: M NOPs for entry instrumentation."
  (when (> *patchable-entry-before* 0)
    (emit-nop-sequence stream *patchable-entry-before*))
  ;; The actual function entry point is here
  ;; (caller sets a label or marks the current position)
  (when (> *patchable-entry-after* 0)
    (emit-nop-sequence stream *patchable-entry-after*)))

;;; ──── Hot patching support ────
(defun patch-function-entry (func-addr new-code-bytes)
  "Overwrite the NOP bytes at FUNC-ADDR with NEW-CODE-BYTES.
Used for runtime hot-patching of functions.
The patched bytes must not exceed *PATCHABLE-ENTRY-BEFORE*."
  (let ((patch-size (length new-code-bytes)))
    (when (> patch-size *patchable-entry-before*)
      (error "Patch size ~D exceeds reserved ~D bytes"
             patch-size *patchable-entry-before*))
    ;; Write new code bytes over the NOPs
    (loop for i from 0 below patch-size
          for byte across new-code-bytes
          do (setf #+sbcl (sb-sys:sap-ref-8 (sb-sys:int-sap func-addr) i)
                 #-sbcl (aref new-code-bytes i) ; selfhost: no in-place patching
                 byte))
    t))

;;; ──── Integration with codegen ────
(defmacro with-patchable-entries ((&key (before 0) (after 0)) &body body)
  "Execute BODY with patchable function entry NOP padding."
  `(let ((*patchable-entry-before* ,before)
         (*patchable-entry-after* ,after))
     ,@body))
