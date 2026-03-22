(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM I/O State - Extended VM State for File I/O Operations
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; I/O Instruction Classes
;;; ----------------------------------------------------------------------------

(defclass vm-open-file (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the file handle")
   (path :initarg :path :reader vm-path
         :documentation "Register containing the file path string")
   (direction :initarg :direction :reader vm-file-direction
              :documentation "Direction keyword: :input or :output"))
  (:documentation "Open a file for reading or writing. Stores file handle in DST."))

(defclass vm-close-file (vm-instruction)
  ((handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle to close"))
  (:documentation "Close a file handle."))

(defclass vm-read-char (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the character read")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle"))
  (:documentation "Read a single character from a file."))

(defclass vm-read-line (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the line read (string)")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle"))
  (:documentation "Read a line from a file as a string."))

(defclass vm-write-char (vm-instruction)
  ((handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle")
   (char :initarg :char :reader vm-char-reg
         :documentation "Register containing the character to write"))
  (:documentation "Write a single character to a file."))

(defclass vm-write-string (vm-instruction)
  ((handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle")
   (str :initarg :str :reader vm-str-reg
        :documentation "Register containing the string to write"))
  (:documentation "Write a string to a file."))

(defclass vm-peek-char (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the peeked character")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle"))
  (:documentation "Peek at the next character without consuming it."))

(defclass vm-unread-char (vm-instruction)
  ((handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle")
   (char :initarg :char :reader vm-char-reg
         :documentation "Register containing the character to put back"))
  (:documentation "Put a character back onto the input stream."))

(defclass vm-file-position (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store current position (when getting)")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle")
   (position :initarg :position :reader vm-position-reg :initform nil
             :documentation "Register containing new position (when setting)"))
  (:documentation "Get or set the file position."))

(defclass vm-file-length (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the file length")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the file handle"))
  (:documentation "Get the length of a file in bytes."))

(defclass vm-eof-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store boolean result (1 if EOF, 0 otherwise)")
   (value :initarg :value :reader vm-value
          :documentation "Register containing value to check for EOF"))
  (:documentation "Check if a value is the EOF marker."))

(defclass vm-make-string-stream (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the stream handle")
   (direction :initarg :direction :reader vm-stream-direction
              :documentation "Direction: :input or :output")
   (initial-string :initarg :initial-string :reader vm-initial-string :initform nil
                   :documentation "Optional register containing initial string for input streams"))
  (:documentation "Create an in-memory string stream."))

(defclass vm-get-string-from-stream (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the accumulated string")
   (handle :initarg :handle :reader vm-file-handle
           :documentation "Register containing the output string stream handle"))
  (:documentation "Get accumulated string from an output string stream."))

;;; ----------------------------------------------------------------------------
;;; I/O Helper Functions
;;; ----------------------------------------------------------------------------

(defun vm-get-stream (state handle)
  "Get the stream associated with HANDLE from STATE.
Handles special cases for stdin (0) and stdout (1)."
  (cond
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
  (or (and (eql handle +stdin-handle+) (vm-standard-input state))
      (and (eql handle +stdout-handle+) (vm-standard-output state))
      (gethash handle (vm-open-files state))
      (gethash handle (vm-string-streams state))))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

(defmethod instruction->sexp ((inst vm-open-file))
  (list :open-file (vm-dst inst) (vm-path inst) (vm-file-direction inst)))

(defmethod instruction->sexp ((inst vm-close-file))
  (list :close-file (vm-file-handle inst)))

(defmethod instruction->sexp ((inst vm-read-char))
  (list :read-char (vm-dst inst) (vm-file-handle inst)))

(defmethod instruction->sexp ((inst vm-read-line))
  (list :read-line (vm-dst inst) (vm-file-handle inst)))

(defmethod instruction->sexp ((inst vm-write-char))
  (list :write-char (vm-file-handle inst) (vm-char-reg inst)))

(defmethod instruction->sexp ((inst vm-write-string))
  (list :write-string (vm-file-handle inst) (vm-str-reg inst)))

(defmethod instruction->sexp ((inst vm-peek-char))
  (list :peek-char (vm-dst inst) (vm-file-handle inst)))

(defmethod instruction->sexp ((inst vm-unread-char))
  (list :unread-char (vm-file-handle inst) (vm-char-reg inst)))

(defmethod instruction->sexp ((inst vm-file-position))
  (if (vm-position-reg inst)
      (list :file-position (vm-dst inst) (vm-file-handle inst) (vm-position-reg inst))
      (list :file-position (vm-dst inst) (vm-file-handle inst))))

(defmethod instruction->sexp ((inst vm-file-length))
  (list :file-length (vm-dst inst) (vm-file-handle inst)))

(defmethod instruction->sexp ((inst vm-eof-p))
  (list :eof-p (vm-dst inst) (vm-value inst)))

(defmethod instruction->sexp ((inst vm-make-string-stream))
  (if (vm-initial-string inst)
      (list :make-string-stream (vm-dst inst) (vm-stream-direction inst) (vm-initial-string inst))
      (list :make-string-stream (vm-dst inst) (vm-stream-direction inst))))

(defmethod instruction->sexp ((inst vm-get-string-from-stream))
  (list :get-string-from-stream (vm-dst inst) (vm-file-handle inst)))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion
;;; ----------------------------------------------------------------------------

;; Extend the existing sexp->instruction method by adding cases
;; Note: This requires modifying the primary method or using :around
;; For now, we provide a helper function that can be integrated

(defun io-sexp->instruction (sexp)
  "Convert I/O-related S-expressions to instruction objects."
  (case (car sexp)
    (:open-file (make-instance 'vm-open-file
                               :dst (second sexp)
                               :path (third sexp)
                               :direction (or (fourth sexp) :input)))
    (:close-file (make-instance 'vm-close-file
                                :handle (second sexp)))
    (:read-char (make-instance 'vm-read-char
                               :dst (second sexp)
                               :handle (third sexp)))
    (:read-line (make-instance 'vm-read-line
                               :dst (second sexp)
                               :handle (third sexp)))
    (:write-char (make-instance 'vm-write-char
                                :handle (second sexp)
                                :char (third sexp)))
    (:write-string (make-instance 'vm-write-string
                                  :handle (second sexp)
                                  :str (third sexp)))
    (:peek-char (make-instance 'vm-peek-char
                               :dst (second sexp)
                               :handle (third sexp)))
    (:unread-char (make-instance 'vm-unread-char
                                 :handle (second sexp)
                                 :char (third sexp)))
    (:file-position (if (fourth sexp)
                        (make-instance 'vm-file-position
                                       :dst (second sexp)
                                       :handle (third sexp)
                                       :position (fourth sexp))
                        (make-instance 'vm-file-position
                                       :dst (second sexp)
                                       :handle (third sexp))))
    (:file-length (make-instance 'vm-file-length
                                 :dst (second sexp)
                                 :handle (third sexp)))
    (:eof-p (make-instance 'vm-eof-p
                           :dst (second sexp)
                           :value (third sexp)))
    (:make-string-stream (if (fourth sexp)
                             (make-instance 'vm-make-string-stream
                                            :dst (second sexp)
                                            :direction (third sexp)
                                            :initial-string (fourth sexp))
                             (make-instance 'vm-make-string-stream
                                            :dst (second sexp)
                                            :direction (third sexp))))
    (:get-string-from-stream (make-instance 'vm-get-string-from-stream
                                            :dst (second sexp)
                                            :handle (third sexp)))
    (otherwise nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution for I/O Operations
;;; ----------------------------------------------------------------------------

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
    (multiple-value-bind (line eof-p)
        (read-line stream nil +eof-value+)
      (if eof-p
          (vm-reg-set state (vm-dst inst) +eof-value+)
          (vm-reg-set state (vm-dst inst) line))
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

;;; ----------------------------------------------------------------------------
;;; Convenience Functions for Running I/O Programs
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Simple I/O Instructions (work with any vm-state, use *standard-output*)
;;; ----------------------------------------------------------------------------

(defclass vm-princ (vm-instruction)
  ((src :initarg :src :reader vm-src))
  (:documentation "Print object readably (no escaping) to *standard-output*."))

(defclass vm-prin1 (vm-instruction)
  ((src :initarg :src :reader vm-src))
  (:documentation "Print object with escaping to *standard-output*."))

(defclass vm-print-inst (vm-instruction)
  ((src :initarg :src :reader vm-src))
  (:documentation "Print object with newline prefix and space suffix to *standard-output*."))

(defclass vm-terpri-inst (vm-instruction)
  ()
  (:documentation "Output a newline to *standard-output*."))

(defclass vm-fresh-line-inst (vm-instruction)
  ()
  (:documentation "Output a newline if not at start of line to *standard-output*."))

(defclass vm-write-to-string-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Convert object to its printed representation as a string."))

(defclass vm-format-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (fmt :initarg :fmt :reader vm-fmt)
   (arg-regs :initarg :arg-regs :reader vm-arg-regs))
  (:documentation "Format string with arguments. Result string stored in DST."))

(defclass vm-make-string-output-stream-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst))
  (:documentation "Create a string output stream, store in DST."))

(defclass vm-get-output-stream-string-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Extract accumulated string from string output stream in SRC, store in DST."))

(defclass vm-stream-write-string-inst (vm-instruction)
  ((stream-reg :initarg :stream :reader vm-stream-reg)
   (src :initarg :src :reader vm-src))
  (:documentation "Write string in SRC to stream in STREAM-REG."))

;;; Instruction -> S-expression for simple I/O

(defmethod instruction->sexp ((inst vm-princ))
  (list :princ (vm-src inst)))

(defmethod instruction->sexp ((inst vm-prin1))
  (list :prin1 (vm-src inst)))

(defmethod instruction->sexp ((inst vm-print-inst))
  (list :print (vm-src inst)))

(defmethod instruction->sexp ((inst vm-terpri-inst))
  (list :terpri))

(defmethod instruction->sexp ((inst vm-fresh-line-inst))
  (list :fresh-line))

(defmethod instruction->sexp ((inst vm-write-to-string-inst))
  (list :write-to-string (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-format-inst))
  (list* :format (vm-dst inst) (vm-fmt inst) (vm-arg-regs inst)))

(defmethod instruction->sexp ((inst vm-make-string-output-stream-inst))
  (list :make-string-output-stream (vm-dst inst)))

(defmethod instruction->sexp ((inst vm-get-output-stream-string-inst))
  (list :get-output-stream-string (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-stream-write-string-inst))
  (list :stream-write-string (vm-stream-reg inst) (vm-src inst)))

;;; Execute simple I/O instructions

(defmethod execute-instruction ((inst vm-princ) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (princ val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (prin1 val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-inst) state pc labels)
  (declare (ignore labels))
  (let ((val (vm-reg-get state (vm-src inst))))
    (print val)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-terpri-inst) state pc labels)
  (declare (ignore labels))
  (terpri)
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-fresh-line-inst) state pc labels)
  (declare (ignore labels))
  (fresh-line)
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
  (let ((stream (vm-reg-get state (vm-stream-reg inst)))
        (str (vm-reg-get state (vm-src inst))))
    (write-string str stream)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Reader Instructions (use host CL reader for bootstrap)
;;; ----------------------------------------------------------------------------

(defclass vm-read-from-string-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Read an S-expression from a string using host CL reader."))

(defclass vm-read-sexp-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Read an S-expression from a stream handle using host CL reader."))

(defmethod instruction->sexp ((inst vm-read-from-string-inst))
  (list :read-from-string (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-read-sexp-inst))
  (list :read-sexp (vm-dst inst) (vm-src inst)))

(defmethod execute-instruction ((inst vm-read-from-string-inst) state pc labels)
  (declare (ignore labels))
  (let* ((str (vm-reg-get state (vm-src inst)))
         (value (read-from-string str)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-sexp-inst) state pc labels)
  (declare (ignore labels))
  (let* ((handle (vm-reg-get state (vm-src inst)))
         (stream (vm-get-stream state handle))
         (value (read stream nil nil)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))
