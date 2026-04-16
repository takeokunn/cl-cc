;;;; diagnostics.lisp — Rust-style diagnostic messages for the parser

(in-package :cl-cc/parse)

;;; Diagnostic struct

(defstruct diagnostic
  "A diagnostic message with source location."
  (severity    :error :type keyword)  ; :error :warning :note
  (span        (cons 0 0))           ; (start-byte . end-byte)
  (source-file nil)
  (message     "" :type string)
  (hints       nil :type list)       ; ((label . (start . end)) ...)
  (notes       nil :type list))      ; list of strings

(defstruct diag-source-location
  "A resolved source location for diagnostics."
  (file nil)
  (line 1 :type integer)
  (column 1 :type integer)
  (byte-offset 0 :type integer))

;;; Location helpers

(defun byte-offset-to-line-col (source byte-offset)
  "Convert a byte offset to (values line column) in SOURCE. Both 1-based."
  (let ((line 1) (col 1))
    (loop for i from 0 below (min byte-offset (length source))
          for ch = (char source i)
          do (if (char= ch #\Newline)
                 (progn (incf line) (setf col 1))
                 (incf col)))
    (values line col)))

(defun source-line-at (source byte-offset)
  "Return the full source line containing BYTE-OFFSET."
  (let* ((start (let ((pos (position #\Newline source :end (min byte-offset (length source)) :from-end t)))
                  (if pos (1+ pos) 0)))
         (end (or (position #\Newline source :start (min byte-offset (length source)))
                  (length source))))
    (subseq source start end)))

;;; Formatting

(defun format-diagnostic (diag source &optional (stream *standard-output*))
  "Format DIAG in Rust-style diagnostic format."
  (let* ((sev (diagnostic-severity diag))
         (msg (diagnostic-message diag))
         (span (diagnostic-span diag))
         (file (or (diagnostic-source-file diag) "<input>"))
         (start-byte (car span)))
    (multiple-value-bind (line col) (byte-offset-to-line-col source start-byte)
      ;; severity: message
      (format stream "~&~(~A~): ~A~%" sev msg)
      ;; --> file:line:col
      (format stream "  --> ~A:~D:~D~%" file line col)
      ;; source line
      (let ((src-line (source-line-at source start-byte)))
        (format stream "   |~%")
        (format stream "~2D | ~A~%" line src-line)
        ;; caret underline
        (format stream "   | ~A^~%" (make-string (max 0 (1- col)) :initial-element #\Space))))
    ;; hints
    (dolist (h (diagnostic-hints diag))
      (format stream "   = hint: ~A~%" (car h)))
    ;; notes
    (dolist (n (diagnostic-notes diag))
      (format stream "   = note: ~A~%" n))))

(defun format-diagnostic-list (diags source &optional (stream *standard-output*))
  "Format a list of diagnostics with summary."
  (dolist (d diags)
    (format-diagnostic d source stream)
    (terpri stream))
  (let ((nerr (count :error diags :key #'diagnostic-severity))
        (nwarn (count :warning diags :key #'diagnostic-severity)))
    (format stream "~&~D error~:P, ~D warning~:P~%" nerr nwarn)))

;;; Convenience constructors

(defun make-parse-error (message span &key source-file hints notes)
  (make-diagnostic :severity :error :message message :span span
                   :source-file source-file :hints hints :notes notes))

(defun make-parse-warning (message span &key source-file hints notes)
  (make-diagnostic :severity :warning :message message :span span
                   :source-file source-file :hints hints :notes notes))

;;; Condition

(define-condition parse-failure (error)
  ((diagnostics :initarg :diagnostics :reader parse-failure-diagnostics :type list))
  (:report (lambda (condition stream)
             (format stream "Parse failed with ~D error~:P"
                     (length (parse-failure-diagnostics condition))))))
