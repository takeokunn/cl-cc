;;;; reader.lisp - Enhanced S-Expression Reader with Source Location Tracking
;;;;
;;;; This module provides:
;;;; - Source location (file, line, column) tracking during reading
;;;; - Integration with SBCL's reader for line/column extraction
;;;; - Location-annotated S-expressions for better error messages

(in-package :cl-cc)

;;; Source Location Data Structure

(defstruct source-location
  "Structure to hold source location information."
  file
  (line 1 :type fixnum)
  (column 0 :type fixnum)
  (position 0 :type fixnum))

(defstruct (source-annotated-sexp (:conc-name sa-sexp-))
  "S-expression with source location annotation (composition, not inheritance)."
  sexp
  (file nil)
  (line 1 :type fixnum)
  (column 0 :type fixnum)
  (position 0 :type fixnum))

;;; Source Location Context

(defvar *source-file* nil
  "Current source file being read (bound dynamically).")

(defvar *source-input* nil
  "Current input stream being read (bound dynamically).")

(defvar *source-locations* nil
  "Hash table mapping cons cells to source locations.")

;;; SBCL-Specific Line/Column Tracking

(defun get-stream-position (stream)
  "Get current position in stream."
  (file-position stream))

(defun get-line-number (stream)
  "Get current line number from stream (approximation using count-newlines)."
  ;; SBCL provides sb-impl::stream-line-column for some streams
  ;; This is a fallback implementation
  (let ((position (get-stream-position stream)))
    (when (and *source-input* (eq stream *source-input*))
      ;; Simple approximation: count newlines from beginning
      ;; For accurate line tracking, we'd need to track as we read
      1)))

(defun get-column-number (stream)
  "Get current column number from stream."
  (declare (ignore stream))
  ;; Without low-level stream access, we approximate
  0)

;;; Enhanced Reader with Location Tracking

(defun %make-source-location (stream &optional (file *source-file*))
  "Create a source-location structure for current position in STREAM."
  (make-source-location
   :file file
   :line (or (ignore-errors (sb-impl::stream-line-column stream)) 1)
   :column (or (ignore-errors (sb-impl::stream-line-column stream)) 0)
   :position (or (get-stream-position stream) 0)))

(defun read-with-location (stream &optional eof-error-p eof-value)
  "Read an S-expression from STREAM, returning (values sexp location).
   If EOF-ERROR-P is true, signal error on EOF, otherwise return EOF-VALUE."
  (let* ((start-pos (get-stream-position stream))
         (result (read stream eof-error-p :eof)))
    (if (eq result :eof)
        (values eof-value nil)
        (let ((location (%make-source-location stream *source-file*)))
          (declare (ignore start-pos))
          (values result location)))))

(defun read-from-string-with-location (string &key (file nil) (start 0) end)
  "Read an S-expression from STRING with source location information.
   Returns (values sexp location)."
  (let ((sub (if end
                 (subseq string start end)
                 (subseq string start)))
        (*source-file* file))
    (with-input-from-string (stream sub)
      (let ((*source-input* stream))
        (read-with-location stream t nil)))))

;;; Location-Preserving AST Creation

(defun lower-sexp-with-location-to-ast (sexp-location)
  "Convert a source-annotated-sexp to an AST with source location."
  (if (source-annotated-sexp-p sexp-location)
      (let ((sexp (sa-sexp-sexp sexp-location)))
        (lower-sexp-to-ast sexp
                           :source-file (sa-sexp-file sexp-location)
                           :source-line (sa-sexp-line sexp-location)
                           :source-column (sa-sexp-column sexp-location)))
      ;; Fallback for non-annotated sexp
      (lower-sexp-to-ast sexp-location)))

(defun parse-source-with-location (source &key (file nil))
  "Parse SOURCE string into an AST with source location information.
   FILE is the optional source file name for error messages."
  (multiple-value-bind (sexp location)
      (read-from-string-with-location source :file file)
    (if (source-location-p location)
        (lower-sexp-to-ast sexp
                           :source-file (source-location-file location)
                           :source-line (source-location-line location)
                           :source-column (source-location-column location))
        (lower-sexp-to-ast sexp))))

;;; Location-Aware Error Reporting

(define-condition reader-error-with-location (error)
  ((message :initarg :message :reader reader-error-message)
   (location :initarg :location :reader reader-error-location))
  (:report (lambda (condition stream)
             (let ((loc (reader-error-location condition)))
               (format stream "Reader error at ~A:~D:~D: ~A"
                       (or (source-location-file loc) "<unknown>")
                       (source-location-line loc)
                       (source-location-column loc)
                       (reader-error-message condition))))))

(defun reader-error-with-location (location format-control &rest format-args)
  "Signal a reader error with source location information."
  (error 'reader-error-with-location
         :location location
         :message (apply #'format nil format-control format-args)))

;;; Batch Reading with Locations

(defun read-all-with-locations (stream &key (file nil))
  "Read all S-expressions from STREAM, returning a list of source-annotated-sexp objects."
  (let ((*source-file* file)
        (*source-input* stream)
        (results nil))
    (handler-case
        (loop
          (multiple-value-bind (sexp location)
              (read-with-location stream nil :eof)
            (when (eq sexp :eof)
              (return (nreverse results)))
            (push (make-source-annotated-sexp
                   :sexp sexp
                   :file (or file (and location (source-location-file location)))
                   :line (if location (source-location-line location) 1)
                   :column (if location (source-location-column location) 0)
                   :position (if location (source-location-position location) 0))
                  results)))
      (end-of-file ()
        (nreverse results)))))

(defun read-file-with-locations (pathname)
  "Read all S-expressions from FILE with source locations."
  (with-open-file (stream pathname :direction :input)
    (read-all-with-locations stream :file (pathname pathname))))

;;; Integration with parse-source

(defun parse-source-from-file (pathname)
  "Parse the first S-expression from PATHNAME into an AST with locations."
  (with-open-file (stream pathname :direction :input)
    (let* ((*source-file* (pathname pathname))
           (*source-input* stream))
      (multiple-value-bind (sexp location)
          (read-with-location stream t nil)
        (if (source-location-p location)
            (lower-sexp-to-ast sexp
                               :source-file (source-location-file location)
                               :source-line (source-location-line location)
                               :source-column (source-location-column location))
            (lower-sexp-to-ast sexp))))))

;;; Debugging Utilities

(defun print-location (location &optional (stream *standard-output*))
  "Print a human-readable location string to STREAM."
  (format stream "~A:~D:~D"
          (or (source-location-file location) "<unknown>")
          (source-location-line location)
          (source-location-column location)))

(defun format-with-location (location format-control &rest format-args)
  "Format a message with source location prefix."
  (format nil "[~A] ~?"
          (print-location location nil)
          format-control
          format-args))
