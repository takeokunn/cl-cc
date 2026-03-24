;;;; compile/ir/printer.lisp — Human-Readable Compile IR Dump
;;;;
;;;; Provides text-format output for debugging and testing.
;;;; Format is inspired by LLVM IR / MLIR text format:
;;;;
;;;;   define my-fn(%0, %1) -> integer {
;;;;   entry:
;;;;     ; preds: (none)
;;;;     %2 = <inst ir-add-inst>
;;;;   then(%3):
;;;;     ; preds: entry
;;;;     ...
;;;;   }

(in-package :cl-cc)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Value Formatting
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-format-value (v)
  "Return a short string representation of an ir-value V.
   Produces '%<id>' for ir-values, or the Lisp-printed form for anything else."
  (if (ir-value-p v)
      (format nil "%~D" (irv-id v))
      (format nil "~S" v)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Instruction Printing
;;;; ─────────────────────────────────────────────────────────────────────────

(defgeneric ir-print-inst (inst stream)
  (:documentation "Print a human-readable form of INST to STREAM.
   Specialise for each concrete instruction type to show operands.")
  (:method ((inst ir-inst) stream)
    "Default: show struct type and result value (or void)."
    (let ((result (iri-result inst)))
      (if result
          (format stream "  ~A = <~A>"
                  (ir-format-value result)
                  (type-of inst))
          (format stream "  <~A>" (type-of inst))))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Block Dump
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-print-block (blk &optional (stream *standard-output*))
  "Print block BLK in human-readable IR form to STREAM."
  ;; Header: label + optional block parameters
  (format stream "~A" (irb-label blk))
  (when (irb-params blk)
    (format stream "(~{~A~^, ~})"
            (mapcar #'ir-format-value (irb-params blk))))
  (format stream ":~%")
  ;; Predecessor annotation
  (if (irb-predecessors blk)
      (format stream "  ; preds: ~{~A~^, ~}~%"
              (mapcar (lambda (p) (format nil "~A" (irb-label p)))
                      (irb-predecessors blk)))
      (format stream "  ; preds: (none)~%"))
  ;; Body instructions
  (dolist (inst (irb-insts blk))
    (ir-print-inst inst stream)
    (terpri stream))
  ;; Terminator
  (when (irb-terminator blk)
    (ir-print-inst (irb-terminator blk) stream)
    (terpri stream)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Function Dump
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-print-function (fn &optional (stream *standard-output*))
  "Print function FN in human-readable IR form to STREAM.
   Blocks are printed in RPO order (entry first)."
  (format stream "define ~A(~{~A~^, ~}) -> ~A {~%"
          (or (irf-name fn) "<anonymous>")
          (mapcar #'ir-format-value (irf-params fn))
          (or (irf-return-type fn) "any"))
  (dolist (blk (ir-rpo fn))
    (ir-print-block blk stream)
    (terpri stream))
  (format stream "}~%"))

(defun ir-function-to-string (fn)
  "Return the human-readable IR dump of function FN as a string."
  (with-output-to-string (s)
    (ir-print-function fn s)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Module Dump
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-print-module (mod &optional (stream *standard-output*))
  "Print all functions in IR module MOD to STREAM."
  (format stream "; IR Module (~D function~:P)~%~%"
          (length (irm-functions mod)))
  (dolist (fn (irm-functions mod))
    (ir-print-function fn stream)
    (terpri stream)))
