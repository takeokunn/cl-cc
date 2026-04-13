(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O State, Constants, Instruction Classes, and Stream Helpers
;;;
;;; Contains: vm-io-state CLOS class, +stdin-handle+/+stdout-handle+/+eof-value+
;;; constants, all define-vm-instruction forms for I/O operations, and stream
;;; helper functions (vm-get-stream, vm-allocate-file-handle, vm-stream-open-p).
;;;
;;; execute-instruction methods, run-compiled-with-io, run-string-with-io are
;;; in io-execute.lisp, which loads immediately after this file.
;;;
;;; Load order: after vm-run.lisp, before io-execute.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; VM I/O State - Extended VM State for File I/O Operations

(defclass vm-io-state (vm-state)
  ((open-files :initform (make-hash-table :test #'eql)
               :reader vm-open-files
               :documentation "Hash table mapping file handles to streams")
   (file-counter :initform 0 :accessor vm-file-counter
                 :documentation "Counter for generating unique file handles")
   (standard-input :initform *standard-input* :accessor vm-standard-input
                   :documentation "Standard input stream")
   (standard-output :initform *standard-output* :accessor vm-standard-output
                    :documentation "Standard output stream")
   (string-streams :initform (make-hash-table :test #'eql)
                   :reader vm-string-streams
                   :documentation "Hash table for in-memory string streams"))
  (:documentation "Extended VM state with file I/O capabilities."))

;;; Special file handle constants
(defconstant +stdin-handle+ 0 "File handle for standard input")
(defconstant +stdout-handle+ 1 "File handle for standard output")
(defconstant +eof-value+ :eof "Special value returned at end of file")

;;; I/O Instruction Classes

(define-vm-instruction vm-open-file (vm-instruction)
  "Open a file for reading or writing. Stores file handle in DST."
  (dst nil :reader vm-dst)
  (path nil :reader vm-path)
  (direction nil :reader vm-file-direction)
  (if-exists :supersede :reader vm-if-exists)
  (if-not-exists nil :reader vm-if-not-exists)
  (:sexp-tag :open-file)
  (:sexp-slots dst path direction))

(define-vm-instruction vm-close-file (vm-instruction)
  "Close a file handle."
  (handle nil :reader vm-file-handle)
  (:sexp-tag :close-file)
  (:sexp-slots handle))

(define-vm-instruction vm-read-char (vm-instruction)
  "Read a single character from a file."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :read-char)
  (:sexp-slots dst handle))

(define-vm-instruction vm-read-line (vm-instruction)
  "Read a line from a file as a string."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :read-line)
  (:sexp-slots dst handle))

(define-vm-instruction vm-write-char (vm-instruction)
  "Write a single character to a file."
  (handle nil :reader vm-file-handle)
  (char nil :reader vm-char-reg)
  (:sexp-tag :write-char)
  (:sexp-slots handle char))

(define-vm-instruction vm-write-string (vm-instruction)
  "Write a string to a file."
  (handle nil :reader vm-file-handle)
  (str nil :reader vm-str-reg)
  (:sexp-tag :write-string)
  (:sexp-slots handle str))

(define-vm-instruction vm-peek-char (vm-instruction)
  "Peek at the next character without consuming it."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :peek-char)
  (:sexp-slots dst handle))

(define-vm-instruction vm-unread-char (vm-instruction)
  "Put a character back onto the input stream."
  (handle nil :reader vm-file-handle)
  (char nil :reader vm-char-reg)
  (:sexp-tag :unread-char)
  (:sexp-slots handle char))

;; Custom sexp: conditional on optional position slot
(define-vm-instruction vm-file-position (vm-instruction)
  "Get or set the file position."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (position nil :reader vm-position-reg))

(defmethod instruction->sexp ((inst vm-file-position))
  (if (vm-position-reg inst)
      (list :file-position (vm-dst inst) (vm-file-handle inst) (vm-position-reg inst))
      (list :file-position (vm-dst inst) (vm-file-handle inst))))

(setf (gethash :file-position *instruction-constructors*)
      (lambda (sexp)
        (if (fourth sexp)
            (make-vm-file-position :dst (second sexp)
                                   :handle (third sexp)
                                   :position (fourth sexp))
            (make-vm-file-position :dst (second sexp)
                                   :handle (third sexp)))))

(define-vm-instruction vm-file-length (vm-instruction)
  "Get the length of a file in bytes."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :file-length)
  (:sexp-slots dst handle))

(define-vm-instruction vm-eof-p (vm-instruction)
  "Check if a value is the EOF marker."
  (dst nil :reader vm-dst)
  (value nil :reader vm-value)
  (:sexp-tag :eof-p)
  (:sexp-slots dst value))

;; Custom sexp: conditional on optional initial-string slot
(define-vm-instruction vm-make-string-stream (vm-instruction)
  "Create an in-memory string stream."
  (dst nil :reader vm-dst)
  (direction nil :reader vm-stream-direction)
  (initial-string nil :reader vm-initial-string))

(defmethod instruction->sexp ((inst vm-make-string-stream))
  (if (vm-initial-string inst)
      (list :make-string-stream (vm-dst inst) (vm-stream-direction inst) (vm-initial-string inst))
      (list :make-string-stream (vm-dst inst) (vm-stream-direction inst))))

(setf (gethash :make-string-stream *instruction-constructors*)
      (lambda (sexp)
        (if (fourth sexp)
            (make-vm-make-string-stream :dst (second sexp)
                                        :direction (third sexp)
                                        :initial-string (fourth sexp))
            (make-vm-make-string-stream :dst (second sexp)
                                        :direction (third sexp)))))

(define-vm-instruction vm-get-string-from-stream (vm-instruction)
  "Get accumulated string from an output string stream."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :get-string-from-stream)
  (:sexp-slots dst handle))

;;; Stream Type Predicates

(define-vm-instruction vm-streamp (vm-instruction)
  "Check if a value is a stream."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :streamp)
  (:sexp-slots dst src))

(define-vm-instruction vm-input-stream-p (vm-instruction)
  "Check if a value is an input stream."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :input-stream-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-output-stream-p (vm-instruction)
  "Check if a value is an output stream."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :output-stream-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-open-stream-p (vm-instruction)
  "Check if a stream is open."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :open-stream-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-interactive-stream-p (vm-instruction)
  "Check if a stream is interactive."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :interactive-stream-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-stream-element-type-inst (vm-instruction)
  "Get the element type of a stream."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :stream-element-type)
  (:sexp-slots dst src))

;;; Binary I/O Instructions

(define-vm-instruction vm-read-byte (vm-instruction)
  "Read a byte from a binary stream."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :read-byte)
  (:sexp-slots dst handle))

(define-vm-instruction vm-write-byte (vm-instruction)
  "Write a byte to a binary stream."
  (handle nil :reader vm-file-handle)
  (byte-val nil :reader vm-byte-val)
  (:sexp-tag :write-byte)
  (:sexp-slots handle byte-val))

;;; Stream Control Instructions

(define-vm-instruction vm-force-output (vm-instruction)
  "Force buffered output to be sent."
  (handle nil :reader vm-file-handle)
  (:sexp-tag :force-output)
  (:sexp-slots handle))

(define-vm-instruction vm-finish-output (vm-instruction)
  "Ensure all output is completed."
  (handle nil :reader vm-file-handle)
  (:sexp-tag :finish-output)
  (:sexp-slots handle))

(define-vm-instruction vm-clear-input (vm-instruction)
  "Clear any buffered input."
  (handle nil :reader vm-file-handle)
  (:sexp-tag :clear-input)
  (:sexp-slots handle))

(define-vm-instruction vm-clear-output (vm-instruction)
  "Abort any pending buffered output."
  (handle nil :reader vm-file-handle)
  (:sexp-tag :clear-output)
  (:sexp-slots handle))

(define-vm-instruction vm-listen-inst (vm-instruction)
  "Check if input is available on a stream."
  (dst nil :reader vm-dst)
  (handle nil :reader vm-file-handle)
  (:sexp-tag :listen)
  (:sexp-slots dst handle))

;;; write-line Instruction

(define-vm-instruction vm-write-line (vm-instruction)
  "Write a string followed by a newline to a stream."
  (handle nil :reader vm-file-handle)
  (str nil :reader vm-str-reg)
  (:sexp-tag :write-line)
  (:sexp-slots handle str))

;;; Load File Instruction

(define-vm-instruction vm-load-file (vm-instruction)
  "Load and execute a Lisp source file."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :load-file)
  (:sexp-slots dst src))

;;; I/O Helper Functions

(defun vm-get-stream (state handle)
  "Get the stream associated with HANDLE from STATE.
Handles special cases for stdin (0), stdout (1), and direct CL stream objects."
  (cond
    ;; Direct CL stream object (from make-string-input-stream etc.)
    ((streamp handle) handle)
    ((eql handle +stdin-handle+)
     (vm-standard-input state))
    ((eql handle +stdout-handle+)
     (vm-standard-output state))
    (t
     (or (gethash handle (vm-open-files state))
         (gethash handle (vm-string-streams state))
         (error "Invalid file handle: ~A" handle)))))

(defun vm-allocate-file-handle (state)
  "Allocate and return a new unique file handle."
  ;; Start from 2 to avoid conflict with stdin/stdout
  (let ((handle (max 2 (1+ (vm-file-counter state)))))
    (setf (vm-file-counter state) handle)
    handle))

(defun vm-stream-open-p (state handle)
  "Check if a file handle is currently open."
  (or (streamp handle)
      (and (eql handle +stdin-handle+) (vm-standard-input state))
      (and (eql handle +stdout-handle+) (vm-standard-output state))
      (gethash handle (vm-open-files state))
      (gethash handle (vm-string-streams state))))

