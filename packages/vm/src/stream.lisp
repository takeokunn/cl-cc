;;;; stream.lisp -- VM instructions for host-backed ANSI streams

(in-package :cl-cc/vm)

;; FR-600: Stream types
(define-vm-instruction vm-make-broadcast-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-broadcast-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-broadcast-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-broadcast-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-concatenated-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-concatenated-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-concatenated-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-concatenated-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-echo-stream (vm-instruction)
  (dst nil :reader vm-dst) (src1 nil :reader vm-src1) (src2 nil :reader vm-src2)
  (:sexp-tag :make-echo-stream) (:sexp-slots dst src1 src2))

(defmethod execute-instruction ((inst vm-make-echo-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:make-echo-stream (vm-reg-get state (vm-src1 inst))
                                   (vm-reg-get state (vm-src2 inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-synonym-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-synonym-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-synonym-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-synonym-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-two-way-stream (vm-instruction)
  (dst nil :reader vm-dst) (src1 nil :reader vm-src1) (src2 nil :reader vm-src2)
  (:sexp-tag :make-two-way-stream) (:sexp-slots dst src1 src2))

(defmethod execute-instruction ((inst vm-make-two-way-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:make-two-way-stream (vm-reg-get state (vm-src1 inst))
                                      (vm-reg-get state (vm-src2 inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-string-input-stream (vm-instruction)
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-string-input-stream) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-string-input-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-string-input-stream (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-make-string-output-stream (vm-instruction)
  (dst nil :reader vm-dst)
  (:sexp-tag :make-string-output-stream) (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-make-string-output-stream) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (cl:make-string-output-stream))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-get-output-stream-string (vm-instruction)
  (dst nil :reader vm-dst) (stream-reg nil :reader vm-goss-stream-reg))

(defmethod execute-instruction ((inst vm-get-output-stream-string) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (cl:get-output-stream-string (vm-reg-get state (vm-goss-stream-reg inst))))
  (values (1+ pc) nil nil))

;; FR-601: Gray streams
;;
;; Define the user-visible Gray Streams protocol in the VM package.  On SBCL the
;; fundamental classes also inherit from SB-GRAY classes and SB-GRAY protocol
;; methods delegate into the cl-cc/vm generic functions below, so ordinary host
;; CL stream operators continue to work for VM fundamental streams.

(defclass fundamental-stream (sb-gray:fundamental-stream) ())

(defclass fundamental-character-input-stream
    (fundamental-stream sb-gray:fundamental-character-input-stream)
  ())

(defclass fundamental-character-output-stream
    (fundamental-stream sb-gray:fundamental-character-output-stream)
  ())

(defclass fundamental-binary-input-stream
    (fundamental-stream sb-gray:fundamental-binary-input-stream)
  ())

(defclass fundamental-binary-output-stream
    (fundamental-stream sb-gray:fundamental-binary-output-stream)
  ())

(defclass fundamental-character-stream
    (fundamental-character-input-stream fundamental-character-output-stream)
  ())

(defclass fundamental-binary-stream
    (fundamental-binary-input-stream fundamental-binary-output-stream)
  ())

(defgeneric stream-read-char (stream)
  (:documentation "Read one character from STREAM, returning +EOF-VALUE+ at EOF."))
(defgeneric stream-unread-char (stream character)
  (:documentation "Push CHARACTER back onto STREAM."))
(defgeneric stream-read-char-no-hang (stream)
  (:documentation "Read one character from STREAM without blocking when possible."))
(defgeneric stream-peek-char (stream)
  (:documentation "Return the next character from STREAM without consuming it."))
(defgeneric stream-listen (stream)
  (:documentation "Return true when character input is available on STREAM."))
(defgeneric stream-read-line (stream)
  (:documentation "Read one line from STREAM and return line and missing-newline-p."))
(defgeneric stream-clear-input (stream)
  (:documentation "Clear buffered input from STREAM."))
(defgeneric stream-write-char (stream character)
  (:documentation "Write CHARACTER to STREAM."))
(defgeneric stream-write-string (stream string &optional start end)
  (:documentation "Write STRING subsequence START..END to STREAM."))
(defgeneric stream-terpri (stream)
  (:documentation "Write a newline to STREAM."))
(defgeneric stream-fresh-line (stream)
  (:documentation "Start a new line on STREAM if not already at line start."))
(defgeneric stream-finish-output (stream)
  (:documentation "Finish pending output on STREAM."))
(defgeneric stream-force-output (stream)
  (:documentation "Force buffered output on STREAM."))
(defgeneric stream-clear-output (stream)
  (:documentation "Clear buffered output from STREAM."))
(defgeneric stream-advance-to-column (stream column)
  (:documentation "Advance STREAM output to COLUMN when possible."))
(defgeneric stream-line-column (stream)
  (:documentation "Return STREAM's current output column, or NIL if unknown."))
(defgeneric stream-start-line-p (stream)
  (:documentation "Return true when STREAM is at the start of an output line."))
(defgeneric stream-read-byte (stream)
  (:documentation "Read one byte from STREAM, returning +EOF-VALUE+ at EOF."))
(defgeneric stream-write-byte (stream byte)
  (:documentation "Write BYTE to STREAM."))

(defun %no-applicable-stream-method (generic-function &rest args)
  "Signal the standard CLOS no-applicable-method condition for stream protocol."
  (apply #'no-applicable-method generic-function args))

(defmethod stream-read-char ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-read-char stream))

(defmethod stream-unread-char ((stream fundamental-stream) character)
  (%no-applicable-stream-method #'stream-unread-char stream character))

(defmethod stream-read-char-no-hang ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-read-char-no-hang stream))

(defmethod stream-peek-char ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-peek-char stream))

(defmethod stream-listen ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-listen stream))

(defmethod stream-read-line ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-read-line stream))

(defmethod stream-clear-input ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-clear-input stream))

(defmethod stream-write-char ((stream fundamental-stream) character)
  (%no-applicable-stream-method #'stream-write-char stream character))

(defmethod stream-write-string ((stream fundamental-stream) string &optional start end)
  (%no-applicable-stream-method #'stream-write-string stream string start end))

(defmethod stream-terpri ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-terpri stream))

(defmethod stream-fresh-line ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-fresh-line stream))

(defmethod stream-finish-output ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-finish-output stream))

(defmethod stream-force-output ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-force-output stream))

(defmethod stream-clear-output ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-clear-output stream))

(defmethod stream-advance-to-column ((stream fundamental-stream) column)
  (%no-applicable-stream-method #'stream-advance-to-column stream column))

(defmethod stream-line-column ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-line-column stream))

(defmethod stream-start-line-p ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-start-line-p stream))

(defmethod stream-read-byte ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-read-byte stream))

(defmethod stream-write-byte ((stream fundamental-stream) byte)
  (%no-applicable-stream-method #'stream-write-byte stream byte))

(defmethod stream-read-char-no-hang ((stream fundamental-character-input-stream))
  (stream-read-char stream))

(defmethod stream-peek-char ((stream fundamental-character-input-stream))
  (let ((character (stream-read-char stream)))
    (unless (eq character +eof-value+)
      (stream-unread-char stream character))
    character))

(defmethod stream-listen ((stream fundamental-character-input-stream))
  (not (eq (stream-peek-char stream) +eof-value+)))

(defmethod stream-read-line ((stream fundamental-character-input-stream))
  (let ((chars '()))
    (loop for character = (stream-read-char stream)
          do (cond
               ((eq character +eof-value+)
                (if chars
                    (return (values (coerce (nreverse chars) 'string) t))
                    (return (values +eof-value+ t))))
               ((char= character #\Newline)
                (return (values (coerce (nreverse chars) 'string) nil)))
               (t
                (push character chars))))))

(defmethod stream-clear-input ((stream fundamental-character-input-stream))
  nil)

(defmethod stream-write-string ((stream fundamental-character-output-stream) string &optional start end)
  (let ((start (or start 0))
        (end (or end (length string))))
    (loop for index from start below end
          do (stream-write-char stream (char string index))))
  string)

(defmethod stream-terpri ((stream fundamental-character-output-stream))
  (stream-write-char stream #\Newline)
  nil)

(defmethod stream-fresh-line ((stream fundamental-character-output-stream))
  (unless (stream-start-line-p stream)
    (stream-terpri stream)
    t))

(defmethod stream-finish-output ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-force-output ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-clear-output ((stream fundamental-character-output-stream))
  nil)

(defmethod stream-advance-to-column ((stream fundamental-character-output-stream) column)
  (let ((current-column (stream-line-column stream)))
    (when current-column
      (loop repeat (max 0 (- column current-column))
            do (stream-write-char stream #\Space))
      t)))

(defmethod stream-start-line-p ((stream fundamental-character-output-stream))
  (eql (stream-line-column stream) 0))

(defmethod close ((stream fundamental-stream) &key abort)
  (declare (ignore abort))
  (%no-applicable-stream-method #'close stream))

(defmethod open-stream-p ((stream fundamental-stream))
  (%no-applicable-stream-method #'open-stream-p stream))

(defmethod input-stream-p ((stream fundamental-stream))
  (%no-applicable-stream-method #'input-stream-p stream))

(defmethod output-stream-p ((stream fundamental-stream))
  (%no-applicable-stream-method #'output-stream-p stream))

(defmethod stream-element-type ((stream fundamental-stream))
  (%no-applicable-stream-method #'stream-element-type stream))

(defmethod input-stream-p ((stream fundamental-character-input-stream)) t)
(defmethod input-stream-p ((stream fundamental-binary-input-stream)) t)
(defmethod output-stream-p ((stream fundamental-character-output-stream)) t)
(defmethod output-stream-p ((stream fundamental-binary-output-stream)) t)

(progn
  (defmethod sb-gray:stream-read-char ((stream fundamental-character-input-stream))
    (stream-read-char stream))

  (defmethod sb-gray:stream-unread-char ((stream fundamental-character-input-stream) character)
    (stream-unread-char stream character))

  (defmethod sb-gray:stream-read-char-no-hang ((stream fundamental-character-input-stream))
    (stream-read-char-no-hang stream))

  (defmethod sb-gray:stream-peek-char ((stream fundamental-character-input-stream))
    (stream-peek-char stream))

  (defmethod sb-gray:stream-listen ((stream fundamental-character-input-stream))
    (stream-listen stream))

  (defmethod sb-gray:stream-read-line ((stream fundamental-character-input-stream))
    (stream-read-line stream))

  (defmethod sb-gray:stream-clear-input ((stream fundamental-character-input-stream))
    (stream-clear-input stream))

  (defmethod sb-gray:stream-write-char ((stream fundamental-character-output-stream) character)
    (stream-write-char stream character))

  (defmethod sb-gray:stream-write-string ((stream fundamental-character-output-stream) string &optional start end)
    (stream-write-string stream string start end))

  (defmethod sb-gray:stream-terpri ((stream fundamental-character-output-stream))
    (stream-terpri stream))

  (defmethod sb-gray:stream-fresh-line ((stream fundamental-character-output-stream))
    (stream-fresh-line stream))

  (defmethod sb-gray:stream-finish-output ((stream fundamental-character-output-stream))
    (stream-finish-output stream))

  (defmethod sb-gray:stream-force-output ((stream fundamental-character-output-stream))
    (stream-force-output stream))

  (defmethod sb-gray:stream-clear-output ((stream fundamental-character-output-stream))
    (stream-clear-output stream))

  (defmethod sb-gray:stream-advance-to-column ((stream fundamental-character-output-stream) column)
    (stream-advance-to-column stream column))

  (defmethod sb-gray:stream-line-column ((stream fundamental-character-output-stream))
    (stream-line-column stream))

  (defmethod sb-gray:stream-start-line-p ((stream fundamental-character-output-stream))
    (stream-start-line-p stream))

  (defmethod sb-gray:stream-read-byte ((stream fundamental-binary-input-stream))
    (stream-read-byte stream))

  (defmethod sb-gray:stream-write-byte ((stream fundamental-binary-output-stream) byte)
    (stream-write-byte stream byte)))

(defun %stream-read-char (stream)
  "Read from either a VM Gray stream or a host CL stream."
  (if (typep stream 'fundamental-character-input-stream)
      (stream-read-char stream)
      (cl:read-char stream)))

(defun %stream-write-char (character stream)
  "Write to either a VM Gray stream or a host CL stream."
  (if (typep stream 'fundamental-character-output-stream)
      (stream-write-char stream character)
      (cl:write-char character stream)))

(define-vm-unary-instruction vm-stream-read-char :stream-read-char "Read char from stream.")
(define-simple-instruction vm-stream-read-char :unary %stream-read-char)

(define-vm-instruction vm-stream-write-char (vm-instruction)
  (stream-reg nil :reader vm-swc-stream-reg) (char-reg nil :reader vm-swc-char-reg)
  (:sexp-tag :stream-write-char) (:sexp-slots stream-reg char-reg))

(defmethod execute-instruction ((inst vm-stream-write-char) state pc labels)
  (declare (ignore labels))
  (%stream-write-char (vm-reg-get state (vm-swc-char-reg inst))
                      (vm-reg-get state (vm-swc-stream-reg inst)))
  (values (1+ pc) nil nil))

;; FR-602: Bivalent streams are provided by io-instructions/io-predicates as
;; vm-read-byte and vm-write-byte.  Keep this file focused on stream-object
;; instructions to avoid redefining those existing instruction structures.
