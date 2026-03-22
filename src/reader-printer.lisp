;;;; reader-printer.lisp - Custom S-Expression Reader and Printer for VM
;;;;
;;;; This module provides:
;;;; - A custom reader that does NOT use CL's built-in READ
;;;; - A printer for outputting values as readable S-expressions
;;;; - VM-callable reader/printer functions
;;;; - VM instructions for reading and printing

(in-package :cl-cc)

;;; Reader State

(defstruct (vm-reader-state
             (:conc-name vrs-)
             (:constructor %make-vm-reader-state))
  "State for incremental S-expression reading."
  (stream nil :type (or stream null))
  (char nil :type (or character null))
  (line 1 :type fixnum)
  (column 0 :type fixnum)
  (eof-reached nil :type boolean)
  (readtable nil :type (or hash-table null)))

(defun make-vm-reader-state (stream)
  "Create a new reader state for STREAM."
  (let ((state (%make-vm-reader-state :stream stream)))
    (vm-reader-advance state)
    state))

(defun vm-reader-advance (state)
  "Advance to the next character in the reader state.
   Returns the new character or NIL on EOF."
  (let ((stream (vrs-stream state)))
    (handler-case
        (let ((ch (read-char stream nil nil)))
          (cond
            ((null ch)
             (setf (vrs-char state) nil
                   (vrs-eof-reached state) t))
            ((char= ch #\Newline)
             (incf (vrs-line state))
             (setf (vrs-column state) 0
                   (vrs-char state) ch))
            (t
             (incf (vrs-column state))
             (setf (vrs-char state) ch)))
          (vrs-char state))
      (error ()
        (setf (vrs-char state) nil
              (vrs-eof-reached state) t)
        nil))))

(defun vm-reader-peek (state)
  "Peek at current character without advancing."
  (vrs-char state))

(defun vm-reader-skip-whitespace (state)
  "Skip whitespace and comments, returning first non-whitespace char."
  (loop
    (let ((ch (vm-reader-peek state)))
      (cond
        ((null ch) (return nil))
        ((whitespace-char-p ch) (vm-reader-advance state))
        ((char= ch #\;)
         ;; Skip comment to end of line
         (loop
           (let ((c (vm-reader-peek state)))
             (when (or (null c) (char= c #\Newline))
               (return))
             (vm-reader-advance state)))
         ;; After comment, continue checking whitespace
         (vm-reader-advance state))
        (t (return ch))))))

(defun whitespace-char-p (ch)
  "Check if CH is a whitespace character."
  (and ch (or (char= ch #\Space)
              (char= ch #\Tab)
              (char= ch #\Newline)
              (char= ch #\Return)
              (char= ch #\Page))))

;;; Reader Conditions

(define-condition vm-reader-error (error)
  ((message :initarg :message :reader vm-reader-error-message)
   (line :initarg :line :reader vm-reader-error-line)
   (column :initarg :column :reader vm-reader-error-column))
  (:report (lambda (condition stream)
             (format stream "Reader error at line ~D, column ~D: ~A"
                     (vm-reader-error-line condition)
                     (vm-reader-error-column condition)
                     (vm-reader-error-message condition)))))

(defun vm-reader-signal-error (state format-control &rest format-args)
  "Signal a reader error with current position."
  (error 'vm-reader-error
         :line (vrs-line state)
         :column (vrs-column state)
         :message (apply #'format nil format-control format-args)))

;;; Core Reader - Integer

(defun vm-read-integer (state &optional (sign 1))
  "Read an integer from STATE. SIGN is 1 or -1 for already-read sign."
  (let ((digits nil))
    ;; Collect digits
    (loop
      (let ((ch (vm-reader-peek state)))
        (when (or (null ch) (not (digit-char-p ch)))
          (return))
        (push ch digits)
        (vm-reader-advance state)))
    (if digits
        (* sign (parse-integer (coerce (nreverse digits) 'string)))
        (vm-reader-signal-error state "Expected integer"))))

(defun vm-read-number (state &optional first-char)
  "Read a number (integer) starting with optional FIRST-CHAR."
  (let ((ch (or first-char (vm-reader-peek state))))
    (cond
      ((char= ch #\-)
       (vm-reader-advance state)
       (let ((next (vm-reader-peek state)))
         (if (digit-char-p next)
             (vm-read-integer state -1)
             ;; It's a symbol starting with -
             (vm-read-symbol state ch))))
      ((char= ch #\+)
       (vm-reader-advance state)
       (let ((next (vm-reader-peek state)))
         (if (digit-char-p next)
             (vm-read-integer state 1)
             ;; It's a symbol starting with +
             (vm-read-symbol state ch))))
      ((digit-char-p ch)
       (unless first-char (vm-reader-advance state))
       (vm-read-integer state 1))
      (t
       (vm-reader-signal-error state "Expected number, got ~S" ch)))))

;;; Core Reader - Symbol

(defun vm-symbol-char-p (ch)
  "Check if CH is valid in a symbol (not including escape chars)."
  (and ch
       (not (whitespace-char-p ch))
       (not (member ch '(#\( #\) #\" #\' #\` #\, #\;)))))

(defun vm-read-symbol (state &optional first-char)
  "Read a symbol from STATE, optionally starting with FIRST-CHAR."
  (let ((chars (if first-char (list first-char) nil))
        (escaped nil))
    (loop
      (let ((ch (vm-reader-peek state)))
        (cond
          ((null ch) (return))
          ((char= ch #\\)
           ;; Escape character - read next char literally
           (vm-reader-advance state)
           (let ((next (vm-reader-peek state)))
             (when next
               (push next chars)
               (vm-reader-advance state))))
          ((char= ch #\|)
           ;; Pipe escape - read until closing pipe
           (vm-reader-advance state)
           (setf escaped t)
           (loop
             (let ((c (vm-reader-peek state)))
               (cond
                 ((null c)
                  (vm-reader-signal-error state "Unterminated |...| escape"))
                 ((char= c #\|)
                  (vm-reader-advance state)
                  (return))
                 ((char= c #\\)
                  (vm-reader-advance state)
                  (let ((next (vm-reader-peek state)))
                    (when next
                      (push next chars)
                      (vm-reader-advance state))))
                 (t
                  (push c chars)
                  (vm-reader-advance state))))))
          ((vm-symbol-char-p ch)
           (push ch chars)
           (vm-reader-advance state))
          (t (return)))))
    (let ((name (coerce (nreverse chars) 'string)))
      ;; Handle special symbols
      (cond
        ((string= name "nil") nil)
        ((string= name "t") t)
        (t (if escaped
               (intern name :keyword)  ;; Escaped symbols become keywords for simplicity
               (intern name :cl-cc-user)))))))

;;; Core Reader - String

(defun vm-read-string (state)
  "Read a string literal from STATE."
  (vm-reader-advance state)  ; Skip opening quote
  (let ((chars nil))
    (loop
      (let ((ch (vm-reader-peek state)))
        (cond
          ((null ch)
           (vm-reader-signal-error state "Unterminated string"))
          ((char= ch #\")
           (vm-reader-advance state)  ; Skip closing quote
           (return))
          ((char= ch #\\)
           ;; Escape sequence
           (vm-reader-advance state)
           (let ((next (vm-reader-peek state)))
             (cond
               ((null next)
                (vm-reader-signal-error state "Unterminated escape sequence"))
               ((char= next #\n)
                (push #\Newline chars)
                (vm-reader-advance state))
               ((char= next #\t)
                (push #\Tab chars)
                (vm-reader-advance state))
               ((char= next #\r)
                (push #\Return chars)
                (vm-reader-advance state))
               ((char= next #\\)
                (push #\\ chars)
                (vm-reader-advance state))
               ((char= next #\")
                (push #\" chars)
                (vm-reader-advance state))
               (t
                (push next chars)
                (vm-reader-advance state)))))
          (t
           (push ch chars)
           (vm-reader-advance state)))))
    (coerce (nreverse chars) 'string)))

;;; Core Reader - List

(defun vm-read-list (state)
  "Read a list from STATE."
  (vm-reader-advance state)  ; Skip opening paren
  (vm-reader-skip-whitespace state)
  (let ((elements nil))
    (loop
      (vm-reader-skip-whitespace state)
      (let ((ch (vm-reader-peek state)))
        (cond
          ((null ch)
           (vm-reader-signal-error state "Unterminated list"))
          ((char= ch #\))
           (vm-reader-advance state)  ; Skip closing paren
           (return))
          ((char= ch #\.)
           ;; Possible dotted pair
           (vm-reader-advance state)
           (let ((next (vm-reader-peek state)))
             (if (whitespace-char-p next)
                 ;; Dotted pair - read cdr
                 (progn
                   (vm-reader-skip-whitespace state)
                   (let ((cdr (vm-read-expr state)))
                     (vm-reader-skip-whitespace state)
                     (let ((close (vm-reader-peek state)))
                       (unless (and close (char= close #\)))
                         (vm-reader-signal-error
                          state "Expected ) after dotted pair, got ~S" close))
                       (vm-reader-advance state)
                       ;; Build dotted list
                       (let ((result (nreverse elements)))
                         (when result
                           (setf (cdr (last result)) cdr))
                         (return result)))))
                 ;; It's a symbol starting with .
                 (progn
                   ;; Put back the . and read as symbol
                   (let ((sym-chars (list #\.)))
                     (loop
                       (let ((c (vm-reader-peek state)))
                         (when (or (null c) (not (vm-symbol-char-p c)))
                           (return))
                         (push c sym-chars)
                         (vm-reader-advance state)))
                     (push (intern (coerce (nreverse sym-chars) 'string) :cl-cc-user)
                           elements))))))
          (t
           (push (vm-read-expr state) elements)))))
    (nreverse elements)))

;;; Core Reader - Quote Forms

(defun vm-read-quote (state)
  "Read a quoted expression: 'expr"
  (vm-reader-advance state)  ; Skip quote
  (list 'quote (vm-read-expr state)))

(defun vm-read-backquote (state)
  "Read a backquoted expression: `expr"
  (vm-reader-advance state)  ; Skip backquote
  (list 'backquote (vm-read-expr state)))

(defun vm-read-comma (state)
  "Read a comma expression: ,expr or ,@expr"
  (vm-reader-advance state)  ; Skip comma
  (let ((ch (vm-reader-peek state)))
    (if (and ch (char= ch #\@))
        (progn
          (vm-reader-advance state)  ; Skip @
          (list 'comma-at (vm-read-expr state)))
        (list 'comma (vm-read-expr state)))))

;;; Core Reader - Dispatch Macro (#)

(defun vm-read-dispatch (state)
  "Read a dispatch macro character starting with #."
  (vm-reader-advance state)  ; Skip #
  (let ((ch (vm-reader-peek state)))
    (cond
      ((null ch)
       (vm-reader-signal-error state "Unexpected end after #"))
      ((char= ch #\\)
       ;; Character literal: #\x
       (vm-reader-advance state)
       (let ((char-ch (vm-reader-peek state)))
         (if char-ch
             (progn
               (vm-reader-advance state)
               ;; Check for named characters
               (let ((name-start (vm-reader-peek state)))
                 (if (and name-start (alpha-char-p name-start))
                     ;; Could be #\Space, #\Newline, etc.
                     (let ((name-chars (list char-ch)))
                       (loop
                         (let ((c (vm-reader-peek state)))
                           (when (or (null c) (not (alpha-char-p c)))
                             (return))
                           (push c name-chars)
                           (vm-reader-advance state)))
                       (let ((name (coerce (nreverse name-chars) 'string)))
                         (cond
                           ((string-equal name "Space") #\Space)
                           ((string-equal name "Newline") #\Newline)
                           ((string-equal name "Tab") #\Tab)
                           ((string-equal name "Return") #\Return)
                           ((string-equal name "Page") #\Page)
                           ((string-equal name "Backspace") #\Backspace)
                           (t (schar name 0)))))
                     char-ch)))
             (vm-reader-signal-error state "Unexpected end in character literal"))))
      ((char= ch #\()
       ;; Vector: #(1 2 3)
       (vm-reader-advance state)
       (let ((elements (vm-read-delimited-list-internal state #\))))
         (make-array (length elements) :initial-contents elements)))
      ((char= ch #\:)
       ;; Uninterned symbol: #:name
       (vm-reader-advance state)
       (make-symbol (symbol-name (vm-read-symbol state))))
      ((char= ch #\|)
       ;; Block comment: #| ... |#
       (vm-reader-advance state)
       (vm-read-block-comment state)
       ;; After skipping comment, read next expression
       (vm-read-expr state))
      ((char= ch #\;)
       ;; S-expression comment: #;expr
       (vm-reader-advance state)
       (vm-read-expr state)  ; Skip the expression
       ;; Return next expression
       (vm-read-expr state))
      ((or (char= ch #\t) (char= ch #\T))
       ;; Boolean true: #t or #T
       (vm-reader-advance state)
       t)
      ((or (char= ch #\f) (char= ch #\F))
       ;; Boolean false: #f or #F (for compatibility, though CL uses nil)
       (vm-reader-advance state)
       nil)
      ((digit-char-p ch)
       ;; Possible array or complex number with reader macro
       ;; For now, just signal error for unsupported features
       (vm-reader-signal-error state "Unsupported # dispatch: #~A" ch))
      (t
       (vm-reader-signal-error state "Unknown # dispatch character: ~S" ch)))))

(defun vm-read-block-comment (state)
  "Skip a block comment #| ... |#, handling nesting."
  (let ((depth 1))
    (loop
      (let ((ch (vm-reader-peek state)))
        (cond
          ((null ch)
           (vm-reader-signal-error state "Unterminated block comment"))
          ((char= ch #\#)
           (vm-reader-advance state)
           (let ((next (vm-reader-peek state)))
             (when (and next (char= next #\|))
               (vm-reader-advance state)
               (incf depth))))
          ((char= ch #\|)
           (vm-reader-advance state)
           (let ((next (vm-reader-peek state)))
             (when (and next (char= next #\#))
               (vm-reader-advance state)
               (decf depth)
               (when (zerop depth)
                 (return)))))
          (t (vm-reader-advance state)))))))

;;; Main Reader Entry Points

(defun vm-read-expr (state)
  "Read a single S-expression from reader state."
  (vm-reader-skip-whitespace state)
  (let ((ch (vm-reader-peek state)))
    (cond
      ((null ch) nil)  ; EOF
      ((digit-char-p ch)
       (vm-reader-advance state)
       (vm-read-integer state 1))
      ((char= ch #\-)
       (vm-reader-advance state)
       (let ((next (vm-reader-peek state)))
         (if (digit-char-p next)
             (vm-read-integer state -1)
             (vm-read-symbol state #\-))))
      ((char= ch #\+)
       (vm-reader-advance state)
       (let ((next (vm-reader-peek state)))
         (if (digit-char-p next)
             (vm-read-integer state 1)
             (vm-read-symbol state #\+))))
      ((alpha-char-p ch)
       (vm-reader-advance state)
       (vm-read-symbol state ch))
      ((char= ch #\")
       (vm-read-string state))
      ((char= ch #\()
       (vm-read-list state))
      ((char= ch #\')
       (vm-read-quote state))
      ((char= ch #\`)
       (vm-read-backquote state))
      ((char= ch #\,)
       (vm-read-comma state))
      ((char= ch #\#)
       (vm-read-dispatch state))
      ((member ch '(#\: #\! #\@ #\$ #\% #\^ #\& #\* #\_ #\= #\< #\> #\? #\/ #\~))
       ;; Symbol constituents
       (vm-reader-advance state)
       (vm-read-symbol state ch))
      ((char= ch #\) )
       (vm-reader-signal-error state "Unexpected close parenthesis"))
      (t
       (vm-reader-signal-error state "Unexpected character: ~S" ch)))))

(defun vm-read (state &optional (eof-error-p t) (eof-value nil))
  "Read an S-expression from reader state.
   Returns EOF-VALUE on EOF if EOF-ERROR-P is nil, otherwise signals error."
  (vm-reader-skip-whitespace state)
  (let ((ch (vm-reader-peek state)))
    (cond
      ((null ch)
       (if eof-error-p
           (vm-reader-signal-error state "Unexpected end of input")
           eof-value))
      (t (vm-read-expr state)))))

(defun vm-read-preserving-whitespace (state &optional (eof-error-p t) (eof-value nil))
  "Read an S-expression, preserving trailing whitespace.
   Does not consume trailing whitespace after reading."
  (vm-reader-skip-whitespace state)
  (let ((ch (vm-reader-peek state)))
    (cond
      ((null ch)
       (if eof-error-p
           (vm-reader-signal-error state "Unexpected end of input")
           eof-value))
      (t (vm-read-expr state)))))

(defun vm-read-from-string (string &key (start 0) end (eof-error-p t) (eof-value nil))
  "Read an S-expression from STRING."
  (let* ((sub (if end (subseq string start end) (subseq string start)))
         (stream (make-string-input-stream sub))
         (state (make-vm-reader-state stream)))
    (vm-read state eof-error-p eof-value)))

(defun vm-read-delimited-list-internal (state delimiter)
  "Read a list of expressions until DELIMITER is encountered."
  (let ((elements nil))
    (loop
      (vm-reader-skip-whitespace state)
      (let ((ch (vm-reader-peek state)))
        (cond
          ((null ch)
           (vm-reader-signal-error state "Unexpected end of list"))
          ((char= ch delimiter)
           (vm-reader-advance state)
           (return (nreverse elements)))
          (t
           (push (vm-read-expr state) elements)))))))

(defun vm-read-delimited-list (delimiter state &optional (eof-error-p t) (eof-value nil))
  "Read a list of S-expressions until DELIMITER is read.
   DELIMITER is consumed but not returned."
  (declare (ignore eof-error-p eof-value))
  (vm-read-delimited-list-internal state delimiter))

;;; Printer Functions

(defun vm-prin1-to-string (object)
  "Convert OBJECT to a machine-readable string representation."
  (typecase object
    (null "NIL")
    ((eql t) "T")
    (integer (prin1-to-string object))
    (string (with-output-to-string (s)
              (write-char #\" s)
              (loop for ch across object
                    do (cond
                         ((char= ch #\\) (write-string "\\\\" s))
                         ((char= ch #\") (write-string "\\\"" s))
                         ((char= ch #\Newline) (write-string "\\n" s))
                         ((char= ch #\Tab) (write-string "\\t" s))
                         ((char= ch #\Return) (write-string "\\r" s))
                         (t (write-char ch s))))
              (write-char #\" s)))
    (character (with-output-to-string (s)
                 (write-string "#\\" s)
                 (case object
                   (#\Space (write-string "Space" s))
                   (#\Newline (write-string "Newline" s))
                   (#\Tab (write-string "Tab" s))
                   (#\Return (write-string "Return" s))
                   (t (write-char object s)))))
    (symbol (symbol-name object))
    (cons (vm-print-list-to-string object))
    (vector (vm-print-vector-to-string object))
     (t (format nil "#<~A>" (type-of object)))))

(defun vm-print-list-to-string (list)
  "Convert LIST to its string representation."
  (with-output-to-string (s)
    (write-char #\( s)
    (loop
      (cond
        ((null list) (return))
        ((consp list)
         (write-string (vm-prin1-to-string (car list)) s)
         (cond
           ((null (cdr list)))  ; do nothing, proper list end
           ((consp (cdr list))
            (write-char #\Space s))
           (t
            ;; Improper list - atom as cdr
            (write-string " . " s)
            (write-string (vm-prin1-to-string (cdr list)) s)
            (return)))
         (setf list (cdr list)))
        (t
         ;; Improper list - atom as cdr
         (write-string " . " s)
         (write-string (vm-prin1-to-string list) s)
         (return)))))
    (write-char #\) s)))

(defun vm-print-vector-to-string (vec)
  "Convert VECTOR to its string representation."
  (with-output-to-string (s)
    (write-string "#(" s)
    (loop for i from 0 below (length vec)
          do (progn
               (when (> i 0) (write-char #\Space s))
               (write-string (vm-prin1-to-string (aref vec i)) s)))
    (write-char #\) s)))

(defun vm-princ-to-string (object)
  "Convert OBJECT to a human-readable string (no escape characters)."
  (typecase object
    (null "")
    (string object)
    (character (string object))
    (symbol (symbol-name object))
    (t (vm-prin1-to-string object))))

(defun vm-prin1 (object &optional (stream *standard-output*))
  "Print OBJECT to STREAM in machine-readable format.
   Returns OBJECT."
  (write-string (vm-prin1-to-string object) stream)
  object)

(defun vm-princ (object &optional (stream *standard-output*))
  "Print OBJECT to STREAM in human-readable format.
   Returns OBJECT."
  (write-string (vm-princ-to-string object) stream)
  object)

(defun vm-print (object &optional (stream *standard-output*))
  "Print OBJECT to STREAM with trailing newline.
   Returns OBJECT."
  (vm-prin1 object stream)
  (terpri stream)
  object)

(defun vm-write-to-string (object)
  "Convert OBJECT to its string representation."
  (vm-prin1-to-string object))

;;; VM Reader/Printer Instructions

(define-vm-instruction vm-read-sexp (vm-instruction)
  "Read an S-expression from stream register to dst register."
  (dst nil :reader vm-dst)
  (stream-reg nil :reader vm-stream-reg)
  (:sexp-tag :read-sexp)
  (:sexp-slots dst stream-reg))

(define-vm-instruction vm-read-from-string-rp (vm-instruction)
  "Read an S-expression from string register to dst register."
  (dst nil :reader vm-dst)
  (string-reg nil :reader vm-string-reg)
  (:sexp-tag :read-from-string)
  (:sexp-slots dst string-reg))

(define-vm-instruction vm-print-sexp (vm-instruction)
  "Print S-expression from register with newline."
  (src nil :reader vm-src)
  (:sexp-tag :print-sexp)
  (:sexp-slots src))

(define-vm-instruction vm-prin1-sexp (vm-instruction)
  "Print S-expression in machine-readable format."
  (src nil :reader vm-src)
  (:sexp-tag :prin1-sexp)
  (:sexp-slots src))

(define-vm-instruction vm-princ-sexp (vm-instruction)
  "Print S-expression in human-readable format."
  (src nil :reader vm-src)
  (:sexp-tag :princ-sexp)
  (:sexp-slots src))

(define-vm-instruction vm-write-to-string-rp (vm-instruction)
  "Convert S-expression to string and store in dst."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :write-to-string)
  (:sexp-slots dst src))

;;; Instruction Execution for Reader/Printer

(defmethod execute-instruction ((inst vm-read-sexp) state pc labels)
  (declare (ignore labels))
  (let* ((stream-or-nil (vm-reg-get state (vm-stream-reg inst)))
         (stream (or stream-or-nil (vm-output-stream state)))
         (reader-state (make-vm-reader-state stream))
         (value (vm-read reader-state nil nil)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-read-from-string-rp) state pc labels)
  (declare (ignore labels))
  (let* ((string (vm-reg-get state (vm-string-reg inst)))
         (value (vm-read-from-string string :eof-error-p nil :eof-value nil)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print-sexp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst)))
        (stream (vm-output-stream state)))
    (vm-print value stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-prin1-sexp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst)))
        (stream (vm-output-stream state)))
    (vm-prin1 value stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-princ-sexp) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst)))
        (stream (vm-output-stream state)))
    (vm-princ value stream)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-write-to-string-rp) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (string (vm-write-to-string value)))
    (vm-reg-set state (vm-dst inst) string)
    (values (1+ pc) nil nil)))

;;; VM Reader State Heap Object

;; For storing reader states on the heap
(defclass vm-reader-state-object (vm-heap-object)
  ((stream :initarg :stream :reader vm-rso-stream)
   (line :initarg :line :accessor vm-rso-line)
   (column :initarg :column :accessor vm-rso-column)
   (char :initarg :char :accessor vm-rso-char)
   (eof-reached :initarg :eof-reached :accessor vm-rso-eof-reached))
  (:documentation "Reader state stored on the VM heap."))

(define-vm-instruction vm-make-reader (vm-instruction)
  "Create a new reader state from stream and store in dst."
  (dst nil :reader vm-dst)
  (stream-reg nil :reader vm-stream-reg)
  (:sexp-tag :make-reader)
  (:sexp-slots dst stream-reg))

(define-vm-instruction vm-reader-read (vm-instruction)
  "Read next S-expression from reader state."
  (dst nil :reader vm-dst)
  (reader-reg nil :reader vm-reader-reg)
  (:sexp-tag :reader-read)
  (:sexp-slots dst reader-reg))

(define-vm-instruction vm-reader-advance-rp (vm-instruction)
  "Advance reader to next character."
  (reader-reg nil :reader vm-reader-reg)
  (:sexp-tag :reader-advance)
  (:sexp-slots reader-reg))

(define-vm-instruction vm-reader-peek-rp (vm-instruction)
  "Peek at current character in reader."
  (dst nil :reader vm-dst)
  (reader-reg nil :reader vm-reader-reg)
  (:sexp-tag :reader-peek)
  (:sexp-slots dst reader-reg))

(defmethod execute-instruction ((inst vm-make-reader) state pc labels)
  (declare (ignore labels))
  (let* ((stream (vm-reg-get state (vm-stream-reg inst)))
         (reader-state (if (typep stream 'vm-heap-object)
                           (let ((s (vm-heap-get state (vm-heap-address stream))))
                             (make-vm-reader-state s))
                           (make-vm-reader-state stream)))
         (rso (make-instance 'vm-reader-state-object
                             :stream (vrs-stream reader-state)
                             :line (vrs-line reader-state)
                             :column (vrs-column reader-state)
                             :char (vrs-char reader-state)
                             :eof-reached (vrs-eof-reached reader-state)))
         (addr (vm-heap-alloc state rso)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-reader-read) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-reader-reg inst)))
         (rso (vm-heap-get state addr)))
    (if (typep rso 'vm-reader-state-object)
        (let* ((stream (vm-rso-stream rso))
               (reader-state (make-vm-reader-state stream)))
          ;; Sync state
          (setf (vrs-line reader-state) (vm-rso-line rso)
                (vrs-column reader-state) (vm-rso-column rso)
                (vrs-char reader-state) (vm-rso-char rso)
                (vrs-eof-reached reader-state) (vm-rso-eof-reached rso))
          (let ((value (vm-read reader-state nil nil)))
            ;; Update heap object
            (setf (vm-rso-line rso) (vrs-line reader-state)
                  (vm-rso-column rso) (vrs-column reader-state)
                  (vm-rso-char rso) (vrs-char reader-state)
                  (vm-rso-eof-reached rso) (vrs-eof-reached reader-state))
            (vm-reg-set state (vm-dst inst) value)
            (values (1+ pc) nil nil)))
        (error "vm-reader-read: Expected reader state at address ~A" addr))))

(defmethod execute-instruction ((inst vm-reader-advance-rp) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-reader-reg inst)))
         (rso (vm-heap-get state addr)))
    (if (typep rso 'vm-reader-state-object)
        (let* ((stream (vm-rso-stream rso))
               (reader-state (make-vm-reader-state stream)))
          ;; Sync and advance
          (setf (vrs-line reader-state) (vm-rso-line rso)
                (vrs-column reader-state) (vm-rso-column rso)
                (vrs-char reader-state) (vm-rso-char rso)
                (vrs-eof-reached reader-state) (vm-rso-eof-reached rso))
          (vm-reader-advance reader-state)
          ;; Update heap object
          (setf (vm-rso-line rso) (vrs-line reader-state)
                (vm-rso-column rso) (vrs-column reader-state)
                (vm-rso-char rso) (vrs-char reader-state)
                (vm-rso-eof-reached rso) (vrs-eof-reached reader-state))
          (values (1+ pc) nil nil))
        (error "vm-reader-advance: Expected reader state at address ~A" addr))))

(defmethod execute-instruction ((inst vm-reader-peek-rp) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-reader-reg inst)))
         (rso (vm-heap-get state addr)))
    (if (typep rso 'vm-reader-state-object)
        (let ((ch (vm-rso-char rso)))
          (vm-reg-set state (vm-dst inst) ch)
          (values (1+ pc) nil nil))
        (error "vm-reader-peek: Expected reader state at address ~A" addr))))

;;; Utility Functions

(defun vm-read-all-from-string (string)
  "Read all S-expressions from STRING, returning a list."
  (let* ((stream (make-string-input-stream string))
         (state (make-vm-reader-state stream))
         (results nil))
    (handler-case
        (loop
          (vm-reader-skip-whitespace state)
          (when (vrs-eof-reached state)
            (return (nreverse results)))
          (let ((expr (vm-read-expr state)))
            (push expr results)))
      (error () (nreverse results)))))
