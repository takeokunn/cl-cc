(in-package :cl-cc)

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

;;; Instruction Execution for I/O Operations

(defmethod execute-instruction ((inst vm-open-file) state pc labels)
  (declare (ignore labels))
  (handler-case
      (let* ((path-str (vm-reg-get state (vm-path inst)))
             (direction (vm-file-direction inst))
             (handle (vm-allocate-file-handle state))
             (stream (open path-str
                          :direction direction
                          :if-exists :supersede
                          :if-does-not-exist (if (eq direction :output)
                                                 :create
                                                 :error))))
        (setf (gethash handle (vm-open-files state)) stream)
        (vm-reg-set state (vm-dst inst) handle)
        (values (1+ pc) nil nil))
    (file-error (e)
      (error "vm-open-file: Failed to open file: ~A" e))))

(defmethod execute-instruction ((inst vm-close-file) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (gethash handle (vm-open-files state))))
    (cond
      ;; Don't close stdin/stdout
      ((or (eql handle +stdin-handle+) (eql handle +stdout-handle+))
       (values (1+ pc) nil nil))
      ;; Close regular file stream
      (stream
       (close stream)
       (remhash handle (vm-open-files state))
       (values (1+ pc) nil nil))
      ;; Check string streams
      ((gethash handle (vm-string-streams state))
       (remhash handle (vm-string-streams state))
       (values (1+ pc) nil nil))
      (t
       (error "vm-close-file: Invalid file handle: ~A" handle)))))

(defmethod execute-instruction ((inst vm-read-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (char (read-char stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) char)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-line) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (multiple-value-bind (line missing-newline-p)
        (read-line stream nil +eof-value+)
      (declare (ignore missing-newline-p))
      ;; read-line returns the line even when EOF terminates it.
      ;; Only return :eof when the stream was already at EOF (line = +eof-value+).
      (vm-reg-set state (vm-dst inst) line)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-write-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (char (vm-reg-get state (vm-char-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-char char stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-write-string) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (str (vm-reg-get state (vm-str-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-string str stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-peek-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (char (peek-char nil stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) char)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-unread-char) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (char (vm-reg-get state (vm-char-reg inst)))
         (stream (vm-get-stream state handle)))
    (unread-char char stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-file-position) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (position-reg (vm-position-reg inst)))
    (if position-reg
        ;; Set position
        (let ((new-pos (vm-reg-get state position-reg)))
          (file-position stream new-pos)
          (vm-reg-set state (vm-dst inst) new-pos)
          (values (1+ pc) nil nil))
        ;; Get position
        (let ((current-pos (file-position stream)))
          (vm-reg-set state (vm-dst inst) current-pos)
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-file-length) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (length (file-length stream)))
    (vm-reg-set state (vm-dst inst) length)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-eof-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-value inst)))
         (is-eof (if (eq value +eof-value+) 1 0)))
    (vm-reg-set state (vm-dst inst) is-eof)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-string-stream) state pc labels)
  (declare (ignore labels))
  (let* ((direction (vm-stream-direction inst))
         (initial-str (when (vm-initial-string inst)
                        (vm-reg-get state (vm-initial-string inst))))
         (handle (vm-allocate-file-handle state))
         (stream (if (eq direction :input)
                     (make-string-input-stream (or initial-str ""))
                     (make-string-output-stream))))
    (setf (gethash handle (vm-string-streams state)) stream)
    (vm-reg-set state (vm-dst inst) handle)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-get-string-from-stream) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (gethash handle (vm-string-streams state))))
     (if stream
        (progn
          (vm-reg-set state (vm-dst inst) (get-output-stream-string stream))
          (values (1+ pc) nil nil))
        (error "vm-get-string-from-stream: Handle ~A is not an output string stream" handle))))

;;; Stream Predicate Execution
;;;
;;; Values can be either direct CL stream objects or integer handles.
;;; We resolve handles to streams before applying predicates.

(defun %resolve-stream-val (state val)
  "Resolve VAL to a CL stream: if it's already a stream, return it;
if it's an integer handle, look it up in STATE's stream tables."
  (cond
    ((streamp val) val)
    ((integerp val)
     (cond
       ((eql val +stdin-handle+) (vm-standard-input state))
       ((eql val +stdout-handle+) (vm-standard-output state))
       ((typep state 'vm-io-state)
        (or (gethash val (vm-open-files state))
            (gethash val (vm-string-streams state))))
       (t nil)))
    (t nil)))

(defmethod execute-instruction ((inst vm-streamp) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if stream t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-input-stream-p) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if (and stream (input-stream-p stream)) t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-output-stream-p) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if (and stream (output-stream-p stream)) t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-open-stream-p) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if (and stream (open-stream-p stream)) t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-interactive-stream-p) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if (and stream (interactive-stream-p stream)) t nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-stream-element-type-inst) state pc labels)
  (declare (ignore labels))
  (let* ((val (vm-reg-get state (vm-src inst)))
         (stream (%resolve-stream-val state val)))
    (vm-reg-set state (vm-dst inst) (if stream (stream-element-type stream) nil))
    (values (1+ pc) nil nil)))

;;; Binary I/O Execution

(defmethod execute-instruction ((inst vm-read-byte) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle))
         (byte (read-byte stream nil +eof-value+)))
    (vm-reg-set state (vm-dst inst) byte)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-write-byte) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (byte (vm-reg-get state (vm-byte-val inst)))
         (stream (vm-get-stream state handle)))
    (write-byte byte stream)
    (values (1+ pc) nil nil)))

;;; Stream Control Execution

(defmethod execute-instruction ((inst vm-force-output) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (force-output stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-finish-output) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (finish-output stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clear-input) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (clear-input stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-listen-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (stream (vm-get-stream state handle)))
    (vm-reg-set state (vm-dst inst) (if (listen stream) t nil))
    (values (1+ pc) nil nil)))

;;; write-line Execution

(defmethod execute-instruction ((inst vm-write-line) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-file-handle inst)))
         (str (vm-reg-get state (vm-str-reg inst)))
         (stream (vm-get-stream state handle)))
    (write-line str stream)
    (values (1+ pc) nil nil)))

;;; Load File Execution
;;; our-load is defined in pipeline.lisp (loaded after this file).
;;; Use funcall + symbol-function to avoid forward reference.

(defmethod execute-instruction ((inst vm-load-file) state pc labels)
  (declare (ignore labels))
  (let* ((path (vm-reg-get state (vm-src inst)))
         (result (funcall (symbol-function 'our-load) path)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Convenience Functions for Running I/O Programs

(defun run-compiled-with-io (program &key
                                       (output-stream *standard-output*)
                                       (input-stream *standard-input*))
  "Run a compiled VM program with I/O support.
OUTPUT-STREAM and INPUT-STREAM can be specified to redirect I/O."
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (state (make-instance 'vm-io-state :output-stream output-stream)))
    ;; Set up standard streams
    (setf (vm-standard-input state) input-stream)
    (setf (vm-standard-output state) output-stream)
    ;; Run the program
    (loop with pc = 0
          while (< pc (length instructions))
          do (multiple-value-bind (next-pc halted result)
                 (execute-instruction (nth pc instructions) state pc labels)
               (when halted
                 (return result))
               (setf pc next-pc))
          finally (return nil))))

(defun run-string-with-io (source &key
                                   (output-stream *standard-output*)
                                   (input-stream *standard-input*))
  "Parse, compile, and run a source string with I/O support."
  (let ((program (compile-string source)))
    (run-compiled-with-io program
                          :output-stream output-stream
                          :input-stream input-stream)))

;;; Simple I/O Instructions (work with any vm-state, use *standard-output*)

(define-vm-instruction vm-princ (vm-instruction)
  "Print object readably (no escaping) to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :princ)
  (:sexp-slots src))

(define-vm-instruction vm-prin1 (vm-instruction)
  "Print object with escaping to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :prin1)
  (:sexp-slots src))

(define-vm-instruction vm-print-inst (vm-instruction)
  "Print object with newline prefix and space suffix to *standard-output*."
  (src nil :reader vm-src)
  (:sexp-tag :print)
  (:sexp-slots src))

(define-vm-instruction vm-terpri-inst (vm-instruction)
  "Output a newline to *standard-output*."
  (:sexp-tag :terpri))

(define-vm-instruction vm-fresh-line-inst (vm-instruction)
  "Output a newline if not at start of line to *standard-output*."
  (:sexp-tag :fresh-line))

(define-vm-instruction vm-write-to-string-inst (vm-instruction)
  "Convert object to its printed representation as a string."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :write-to-string)
  (:sexp-slots dst src))

;; Custom sexp: uses list* with variadic arg-regs
(define-vm-instruction vm-format-inst (vm-instruction)
  "Format string with arguments. Result string stored in DST."
  (dst nil :reader vm-dst)
  (fmt nil :reader vm-fmt)
  (arg-regs nil :reader vm-arg-regs))

(defmethod instruction->sexp ((inst vm-format-inst))
  (list* :format (vm-dst inst) (vm-fmt inst) (vm-arg-regs inst)))

(setf (gethash :format *instruction-constructors*)
      (lambda (sexp)
        (make-vm-format-inst :dst (second sexp)
                             :fmt (third sexp)
                             :arg-regs (cdddr sexp))))

(define-vm-instruction vm-make-string-output-stream-inst (vm-instruction)
  "Create a string output stream, store in DST."
  (dst nil :reader vm-dst)
  (:sexp-tag :make-string-output-stream)
  (:sexp-slots dst))

(define-vm-instruction vm-get-output-stream-string-inst (vm-instruction)
  "Extract accumulated string from string output stream in SRC, store in DST."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :get-output-stream-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-stream-write-string-inst (vm-instruction)
  "Write string in SRC to stream in STREAM-REG."
  (stream-reg nil :reader vm-stream-reg)
  (src nil :reader vm-src)
  (:sexp-tag :stream-write-string)
  (:sexp-slots stream-reg src))

;;; Execute simple I/O instructions

(defmethod execute-instruction ((inst vm-princ) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (princ val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (prin1 val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (print val (vm-output-stream state))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-terpri-inst) state pc labels)
  (declare (ignore labels))
  (terpri (vm-output-stream state))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-fresh-line-inst) state pc labels)
  (declare (ignore labels))
  (fresh-line (vm-output-stream state))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-write-to-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (write-to-string val))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-format-inst) state pc labels)
  (declare (ignore labels))
  (let* ((fmt-str (vm-reg-get state (vm-fmt inst)))
         (arg-vals (mapcar (lambda (r) (vm-reg-get state r)) (vm-arg-regs inst)))
         (result (apply #'format nil fmt-str arg-vals)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-string-output-stream-inst) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (make-string-output-stream))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-get-output-stream-string-inst) state pc labels)
  (declare (ignore labels))
  (let ((stream (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst) (get-output-stream-string stream))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-stream-write-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((stream-val (vm-reg-get state (vm-stream-reg inst)))
         (stream (if (streamp stream-val)
                     stream-val
                     (vm-get-stream state stream-val)))
         (str (vm-reg-get state (vm-src inst))))
    (write-string str stream)
    (values (1+ pc) nil nil)))

;;; Reader Instructions (use cl-cc's own lexer/parser)

(define-vm-instruction vm-read-from-string-inst (vm-instruction)
  "Read an S-expression from a string using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-from-string)
  (:sexp-slots dst src))

(define-vm-instruction vm-read-sexp-inst (vm-instruction)
  "Read an S-expression from a stream handle using cl-cc's own parser."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :read-sexp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-read-from-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((str (vm-reg-get state (vm-src inst)))
         (forms (parse-all-forms str))
         (value (if forms (first forms) nil)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-sexp-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-src inst)))
         (stream (vm-get-stream state handle))
         (line (read-line stream nil nil))
         (value (when line (first (parse-all-forms line)))))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))
