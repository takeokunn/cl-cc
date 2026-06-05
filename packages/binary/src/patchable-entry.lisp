;;;; packages/binary/src/patchable-entry.lisp — FR-584 Patchable Function Entry
;;;; Reserve NOP bytes before function entries for runtime patching.
;;;; GCC/Clang -fpatchable-function-entry equivalent.

(in-package :cl-cc/binary)

;;; ──── Configuration ────
(defvar *patchable-entry-before* 0
  "Number of NOP bytes to insert before each function entry.")

(defvar *patchable-entry-after* 0
  "Number of NOP bytes to insert after the function entry (before body).")

;;; ──── NOP byte sequences (Intel x86-64 multi-byte NOPs, SDM Vol. 2B table 4-12) ────
;;; Keys are byte counts 1-9; values are the canonical NOP encodings.
(defparameter *nop-sequences*
  '((1 . #(#x90))
    (2 . #(#x66 #x90))
    (3 . #(#x0F #x1F #x00))
    (4 . #(#x0F #x1F #x40 #x00))
    (5 . #(#x0F #x1F #x44 #x00 #x00))
    (6 . #(#x66 #x0F #x1F #x44 #x00 #x00))
    (7 . #(#x0F #x1F #x80 #x00 #x00 #x00 #x00))
    (8 . #(#x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00))
    (9 . #(#x66 #x0F #x1F #x84 #x00 #x00 #x00 #x00 #x00)))
  "Alist mapping NOP byte-count (1-9) to the corresponding x86-64 multi-byte NOP vector.")

;;; ──── NOP emission ────
(defun emit-nop-sequence (stream count)
  "Emit COUNT bytes of NOP instructions into STREAM.
Uses multi-byte NOPs for efficiency (up to 9 bytes per NOP instruction)."
  (loop while (> count 0)
        for n = (min count 9)
        for bytes = (cdr (assoc n *nop-sequences*))
        do (loop for byte across bytes do (write-byte byte stream))
        do (decf count n)))

;;; ──── Function entry patching ────
(defun emit-patchable-function-entry (stream)
  "Emit NOP padding before and after the function entry point.

Emits *PATCHABLE-ENTRY-BEFORE* NOP bytes (for hot-patching: ftrace,
SystemTap, eBPF uprobes) followed by *PATCHABLE-ENTRY-AFTER* NOP bytes
(for entry instrumentation).  The caller is responsible for recording
the current stream position as the function entry label between these two
NOP regions."
  (when (> *patchable-entry-before* 0)
    (emit-nop-sequence stream *patchable-entry-before*))
  ;; The actual function entry point is here;
  ;; caller sets a label or marks the current position.
  (when (> *patchable-entry-after* 0)
    (emit-nop-sequence stream *patchable-entry-after*)))

;;; ──── Hot patching support ────
(defun patch-function-entry (func-addr new-code-bytes)
  "Overwrite the NOP bytes at FUNC-ADDR with NEW-CODE-BYTES.
Used for runtime hot-patching of functions.
The patched bytes must not exceed *PATCHABLE-ENTRY-BEFORE*.
Uses sb-sys:sap-ref-8 for direct memory write."
  (let ((patch-size (length new-code-bytes)))
    (when (> patch-size *patchable-entry-before*)
      (error "Patch size ~D exceeds reserved ~D bytes"
             patch-size *patchable-entry-before*))
    (loop for i from 0 below patch-size
          for byte across new-code-bytes
          do (setf (sb-sys:sap-ref-8 (sb-sys:int-sap func-addr) i) byte))
    t))

;;; ──── Integration with codegen ────
(defmacro with-patchable-entries ((&key (before 0) (after 0)) &body body)
  "Execute BODY with patchable function entry NOP padding."
  `(let ((*patchable-entry-before* ,before)
         (*patchable-entry-after* ,after))
     ,@body))
