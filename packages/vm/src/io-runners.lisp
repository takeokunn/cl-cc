(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O Runner Convenience Functions
;;;
;;; Extracted from io-execute.lisp.
;;; Contains:
;;;   - run-compiled-with-io  — run a compiled VM program with redirectable I/O
;;;   - run-string-with-io    — parse+compile+run a source string with I/O
;;;
;;; All execute-instruction methods for I/O instructions are in
;;; io-execute.lisp (loads before this file).
;;;
;;; Load order: after io-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun run-compiled-with-io (program &key
                                       (output-stream *standard-output*)
                                       (input-stream *standard-input*))
  "Run a compiled VM program with I/O support.
OUTPUT-STREAM and INPUT-STREAM can be specified to redirect I/O."
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (flat (coerce instructions 'vector))
         (state (make-vm-state :output-stream output-stream)))
    ;; Set up standard streams
    (setf (vm-standard-input state) input-stream)
    (setf (vm-standard-output state) output-stream)
    ;; Bind execution context for sub-invocations (custom method combination)
    (let ((*vm-exec-flat* flat)
          (*vm-exec-labels* labels))
      ;; Run the program
      (loop with pc = 0
            while (< pc (length instructions))
            do (multiple-value-bind (next-pc halted result)
                   (execute-instruction (aref flat pc) state pc labels)
                 (when halted
                   (return result))
                 (setf pc next-pc))
            finally (return nil)))))

(defun run-string-with-io (source &key
                                   (output-stream *standard-output*)
                                   (input-stream *standard-input*))
  "Parse, compile, and run a source string with I/O support."
  (let ((program (funcall *vm-compile-string-hook* source)))
    (run-compiled-with-io program
                          :output-stream output-stream
                          :input-stream input-stream)))
