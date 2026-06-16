;;;; optimizer-remarks.lisp — YAML optimization remarks

(in-package :cl-cc/optimize)

(defstruct (opt-remark (:constructor make-opt-remark
                         (&key type pass-name function-name description source-location)))
  "One optimization remark entry. TYPE is :APPLIED, :MISSED, or :ANALYSIS."
  (type :analysis :type keyword)
  (pass-name "unknown")
  (function-name "<toplevel>")
  (description "")
  (source-location nil))

(defun opt-remark-type-applies-p (type mode)
  "Return T when remark TYPE should be emitted for MODE."
  (case mode
    (:all t)
    (:changed (eq type :applied))
    (:applied (eq type :applied))
    (:missed (eq type :missed))
    (:analysis (eq type :analysis))
    (otherwise nil)))

(defun %opt-yaml-string (value)
  "Return VALUE rendered as a conservative double-quoted YAML scalar."
  (let ((text (format nil "~A" (or value ""))))
    (with-output-to-string (out)
      (write-char #\" out)
      (loop for ch across text do
        (case ch
          (#\\ (write-string "\\\\" out))
          (#\" (write-string "\\\"" out))
          (#\Newline (write-string "\\n" out))
          (otherwise (write-char ch out))))
      (write-char #\" out))))

(defun write-opt-remark-yaml (remark stream)
  "Write REMARK to STREAM as one YAML sequence item."
  (format stream "- type: ~A~%" (string-downcase (symbol-name (opt-remark-type remark))))
  (format stream "  pass: ~A~%" (%opt-yaml-string (opt-remark-pass-name remark)))
  (format stream "  function: ~A~%" (%opt-yaml-string (opt-remark-function-name remark)))
  (format stream "  description: ~A~%" (%opt-yaml-string (opt-remark-description remark)))
  (format stream "  source_location: ~A~%"
          (if (opt-remark-source-location remark)
              (%opt-yaml-string (opt-remark-source-location remark))
              "null"))
  remark)

(defun emit-opt-remark (stream &key type pass-name function-name description source-location)
  "Create and write one YAML optimization remark to STREAM."
  (write-opt-remark-yaml
   (make-opt-remark :type type
                    :pass-name pass-name
                    :function-name (or function-name "<toplevel>")
                    :description description
                    :source-location source-location)
   stream))

(defun opt-pass-optimization-remarks (instructions)
  "Emit an analysis optimization remark when remark reporting is enabled.

This framework pass intentionally leaves INSTRUCTIONS unchanged.  The main
pipeline owns per-pass changed/missed remarks; this pass provides a stable pass
entry so users can request a final optimization-remarks stage explicitly."
  (when (and (boundp '*optimization-report-stream*)
             *optimization-report-stream*)
    (emit-opt-remark *optimization-report-stream*
                     :type :analysis
                     :pass-name "optimization-remarks"
                     :description "optimizer remarks pass completed"))
  instructions)
