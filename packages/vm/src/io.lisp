(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O State, Constants, Stream Helpers, and Encoding
;;;
;;; Contains:
;;;   - *print-* and *default-external-format* parameters
;;;   - *vm-stream-external-formats* side table
;;;   - External format normalization and encoding/decoding utilities:
;;;       %vm-normalize-external-format, %vm-host-external-format,
;;;       vm-encode-string, vm-decode-bytes, vm-stream-external-format,
;;;       vm-set-stream-external-format
;;;   - vm-open — file open with external-format tracking
;;;   - with-standard-io-syntax macro
;;;   - vm-io-state CLOS class (open-files, file-counter, std I/O, string-streams)
;;;   - clone-vm-state — copy runtime state for test isolation
;;;   - +stdin-handle+, +stdout-handle+, +eof-value+ constants
;;;   - vm-string-input-stream struct and helpers
;;;   - vm-get-stream      — resolve handle → CL stream
;;;   - vm-allocate-file-handle — allocate a fresh file handle integer
;;;   - vm-stream-open-p   — check if a handle is currently open
;;;   - VM stream bridge helpers (%vm-bridge-*)
;;;   - with-binary-file macro
;;;   - *vm-io-host-bridge-table* registration
;;;
;;; Networking (sockets, DNS, TLS, mmap) is in io-network.lisp (loads next).
;;; All define-vm-instruction forms for I/O operations are in io-instructions.lisp.
;;; execute-instruction methods are in io-execute.lisp.
;;; run-compiled-with-io / run-string-with-io are in io-runners.lisp.
;;;
;;; Load order: after vm-run.lisp, before io-network.lisp and io-instructions.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── VM I/O State ────────────────────────────────────────────────────────────

(defparameter *print-level* nil
  "Maximum depth to print, or NIL for unlimited depth.")

(defparameter *print-length* nil
  "Maximum number of elements to print at each level, or NIL for unlimited length.")

(declaim (special *print-circle*))

(defparameter *print-case* :upcase
  "Case conversion used when printing symbols: :UPCASE, :DOWNCASE, or :CAPITALIZE.")

(defparameter *print-escape* t
  "When true, print escape characters needed for READ to recover the object.")

(defparameter *print-gensym* t
  "When true, print uninterned symbols with the #: prefix.")

(defparameter *print-array* t
  "When true, print array contents when possible.")

(defparameter *print-lines* nil
  "Maximum pretty-printer lines to emit, or NIL for unlimited lines.")

(defparameter *print-right-margin* nil
  "Pretty-printer target line width, or NIL for the implementation default.")

(defparameter *default-external-format* :utf-8
  "Default external format for VM character streams.")

(defvar *vm-stream-external-formats* (make-hash-table :test #'eq)
  "Side table mapping host stream objects to VM external-format designators.")

(defparameter *print-readably* nil
  "When true, signal if an object cannot be printed readably.")

(defparameter *print-base* 10
  "Radix used when printing integers.")

(defparameter *print-radix* nil
  "When true, print radix markers for numbers.")

(defparameter *print-pretty* nil
  "When true, use the pretty printer for structured output.")

(declaim (special *print-pprint-dispatch* *readtable*))

(defun %vm-normalize-external-format (format)
  "Normalize VM external format designators to canonical keywords."
  (case (or format *default-external-format*)
    ((:utf8 :utf-8) :utf-8)
    ((:utf16 :utf-16 :utf-16le) :utf-16le)
    (:utf-16be :utf-16be)
    ((:utf32 :utf-32 :utf-32le) :utf-32le)
    (:utf-32be :utf-32be)
    ((:latin1 :latin-1 :iso-8859-1) :latin-1)
    (:ascii :ascii)
    (otherwise (error "Unsupported external format: ~S" format))))

(defun %vm-host-external-format (format)
  "Return the closest host external-format designator for FORMAT."
  (case (%vm-normalize-external-format format)
    (:latin-1 :latin-1)
    (otherwise (%vm-normalize-external-format format))))

(defun %vm-signal-or-substitute-encoding-error (mode replacement)
  (ecase mode
    (:error (error "Character cannot be represented in requested external format"))
    (:replace replacement)
    (:ignore nil)))

(defun %vm-replacement-character ()
  (or (code-char #xfffd) #\?))

(defun %vm-encode-latin/ascii (string limit error-mode)
  (let ((bytes (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for ch across string
          for code = (char-code ch)
          do (cond
               ((<= code limit) (vector-push-extend code bytes))
               (t (let ((sub (%vm-signal-or-substitute-encoding-error error-mode 63)))
                    (when sub (vector-push-extend sub bytes))))))
    bytes))

(defun vm-encode-string (string format &key (error-mode :error))
  "Encode STRING into an (UNSIGNED-BYTE 8) vector using FORMAT.
ERROR-MODE is :ERROR, :REPLACE, or :IGNORE."
  (check-type string string)
  (let ((format (%vm-normalize-external-format format)))
    (case format
      (:ascii (%vm-encode-latin/ascii string 127 error-mode))
      (:latin-1 (%vm-encode-latin/ascii string 255 error-mode))
      (otherwise
       (handler-case
           (sb-ext:string-to-octets string :external-format (%vm-host-external-format format))
         (error (e)
           (ecase error-mode
             (:error (error e))
             ((:replace :ignore)
              ;; Re-encode after replacing non-Latin BMP surrogates defensively.
              (sb-ext:string-to-octets
               (with-output-to-string (out)
                 (loop for ch across string
                       do (if (code-char (char-code ch))
                              (write-char ch out)
                              (unless (eq error-mode :ignore) (write-char (%vm-replacement-character) out)))))
               :external-format (%vm-host-external-format format))))))))))

(defun %vm-coerce-octets (bytes)
  (cond
    ((typep bytes '(array (unsigned-byte 8) (*))) bytes)
    ((vectorp bytes)
     (let ((out (make-array (length bytes) :element-type '(unsigned-byte 8))))
       (dotimes (i (length bytes) out) (setf (aref out i) (aref bytes i)))))
    ((listp bytes)
     (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))
    (t (error "Expected octet vector/list: ~S" bytes))))

(defun %vm-detect-bom (bytes requested-format)
  "Return canonical format and starting offset after recognizing a Unicode BOM."
  (let ((n (length bytes))
        (fmt (%vm-normalize-external-format requested-format)))
    (cond
      ((and (>= n 4) (= (aref bytes 0) #xFF) (= (aref bytes 1) #xFE)
            (= (aref bytes 2) 0) (= (aref bytes 3) 0))
       (values :utf-32le 4))
      ((and (>= n 4) (= (aref bytes 0) 0) (= (aref bytes 1) 0)
            (= (aref bytes 2) #xFE) (= (aref bytes 3) #xFF))
       (values :utf-32be 4))
      ((and (>= n 3) (= (aref bytes 0) #xEF) (= (aref bytes 1) #xBB) (= (aref bytes 2) #xBF))
       (values :utf-8 3))
      ((and (>= n 2) (= (aref bytes 0) #xFF) (= (aref bytes 1) #xFE))
       (values :utf-16le 2))
      ((and (>= n 2) (= (aref bytes 0) #xFE) (= (aref bytes 1) #xFF))
       (values :utf-16be 2))
      (t (values fmt 0)))))

(defun %vm-decode-latin/ascii (bytes limit error-mode)
  (with-output-to-string (out)
    (loop for byte across bytes
          do (cond
               ((<= byte limit) (write-char (code-char byte) out))
               (t (ecase error-mode
                    (:error (error "Invalid byte ~D for requested external format" byte))
                    (:replace (write-char (%vm-replacement-character) out))
                    (:ignore nil)))))))

(defun vm-decode-bytes (bytes format &key (error-mode :error))
  "Decode BYTES into a string using FORMAT, auto-removing UTF BOMs."
  (let ((octets (%vm-coerce-octets bytes)))
    (multiple-value-bind (format start) (%vm-detect-bom octets format)
      (let ((payload (if (zerop start) octets (subseq octets start))))
        (case format
          (:ascii (%vm-decode-latin/ascii payload 127 error-mode))
          (:latin-1 (%vm-decode-latin/ascii payload 255 error-mode))
          (otherwise
           (handler-case
               (sb-ext:octets-to-string payload :external-format (%vm-host-external-format format))
             (error (e)
               (ecase error-mode
                 (:error (error e))
                 (:replace (string (%vm-replacement-character)))
                 (:ignore ""))))))))))
(defun vm-stream-external-format (stream)
  "Return STREAM's VM external format, defaulting to *DEFAULT-EXTERNAL-FORMAT*."
  (gethash stream *vm-stream-external-formats* *default-external-format*))

(defun vm-set-stream-external-format (stream fmt)
  "Associate STREAM with external format FMT and return FMT's canonical form."
  (setf (gethash stream *vm-stream-external-formats*)
        (%vm-normalize-external-format fmt)))

(defun vm-open (file &key (direction :input) (if-exists :supersede)
                       if-does-not-exist if-not-exists
                       (external-format *default-external-format*) element-type)
  "Open FILE with VM external-format normalization and stream metadata tracking."
  (let* ((fmt (%vm-normalize-external-format external-format))
         (args (append (list file :direction direction
                             :if-exists if-exists
                             :if-does-not-exist (or if-does-not-exist if-not-exists
                                                   (case direction
                                                     ((:output :io) :create)
                                                     (:probe nil)
                                                     (otherwise :error))))
                       (when element-type (list :element-type element-type))
                       (list :external-format (%vm-host-external-format fmt))))
         (stream (apply #'open args)))
    (when stream (vm-set-stream-external-format stream fmt))
    stream))

(defmacro with-standard-io-syntax (&body body)
  "Execute BODY with ANSI standard I/O control variables bound to defaults."
  `(let ((*print-base* 10)
          (*print-radix* nil)
          (*print-circle* nil)
          (*print-pretty* nil)
          (*print-level* nil)
          (*print-length* nil)
          (*print-case* :upcase)
          (*print-escape* t)
          (*print-gensym* t)
          (*print-array* t)
          (*print-lines* nil)
          (*print-right-margin* nil)
          (*print-readably* nil)
          (*print-pprint-dispatch* (copy-pprint-dispatch nil))
          (*readtable* (copy-readtable nil))
          (cl:*print-base* 10)
          (cl:*print-radix* nil)
          (cl:*print-circle* nil)
          (cl:*print-pretty* nil)
          (cl:*print-level* nil)
          (cl:*print-length* nil)
          (cl:*print-case* :upcase)
          (cl:*print-escape* t)
          (cl:*print-gensym* t)
          (cl:*print-array* t)
          (cl:*print-lines* nil)
          (cl:*print-right-margin* nil)
          (cl:*print-readably* nil)
          (cl:*readtable* (cl:copy-readtable nil)))
     ,@body))

(defclass vm-io-state (vm-state)
  ((open-files :initform (make-hash-table :test #'eql)
               :reader vm-open-files
               :documentation "Hash table mapping file handles to streams")
   (file-counter :initform 0 :accessor vm-file-counter
                 :documentation "Counter for generating unique file handles")
   (standard-input :initform *standard-input* :accessor vm-standard-input
                   :documentation "Standard input stream")
   (standard-output :initform *standard-output* :initarg :output-stream
                    :accessor vm-standard-output
                    :documentation "Standard output stream")
   (string-streams :initform (make-hash-table :test #'eql)
                   :reader vm-string-streams
                   :documentation "Hash table for in-memory string streams"))
  (:documentation "Extended VM state with file I/O capabilities."))

;;; ─── VM State Clone ──────────────────────────────────────────────────────────

(defun %copy-ht-into (src dst)
  "Replace DST's contents with a shallow copy of SRC."
  (clrhash dst)
  (maphash (lambda (k v) (setf (gethash k dst) v)) src))

(defun %copy-vm-symbol-property-table-into (src dst)
  "Replace DST's contents with a copy of SRC that preserves plist isolation."
  (clrhash dst)
  (maphash (lambda (symbol entry)
             (setf (gethash symbol dst)
                   (cl-cc/vm::%vm-copy-symbol-property-entry entry)))
           src))

(defun clone-vm-state (source &key (output-stream *standard-output*))
  "Create a new vm-io-state seeded with the runtime state from SOURCE.
 Copies function-registry, class-registry, global-vars, heap, heap-counter,
 user/system symbol property state, and related metadata so user code can call
 stdlib functions and access stdlib globals without recompiling the stdlib.
Registers, call-stack, handler-stack, method-call-stack start fresh so
each test begins with a clean execution context."
  (let ((clone (make-vm-state :output-stream output-stream)))
    (%copy-ht-into (vm-function-registry source) (vm-function-registry clone))
    (%copy-ht-into (vm-class-registry    source) (vm-class-registry    clone))
    (%copy-ht-into (vm-global-vars       source) (vm-global-vars       clone))
    (%copy-ht-into (vm-state-heap        source) (vm-state-heap        clone))
    (%copy-vm-symbol-property-table-into (vm-symbol-plists source)
                                         (vm-symbol-plists clone))
    (%copy-vm-symbol-property-table-into (vm-system-symbol-plists source)
                                         (vm-system-symbol-plists clone))
    (setf (vm-heap-counter clone) (vm-heap-counter source))
    (setf (vm-symbol-plist-read-barrier clone)
          (vm-symbol-plist-read-barrier source))
    (setf (vm-standard-output clone) output-stream)
    clone))

;;; ─── File Handle Constants ───────────────────────────────────────────────────

(defconstant +stdin-handle+  0    "File handle for standard input")
(defconstant +stdout-handle+ 1    "File handle for standard output")
(defconstant +eof-value+     :eof "Special value returned at end of file")

;;; ─── Stream Helper Functions ─────────────────────────────────────────────────

(defstruct (vm-string-input-stream
            (:constructor %make-vm-string-input-stream (contents)))
  "VM-owned string input stream with explicit position and one-character pushback."
  (contents "" :type string)
  (position 0 :type fixnum)
  (unread-character nil))

(defun make-vm-string-input-stream (contents)
  "Create a VM-owned string input stream from CONTENTS."
  (%make-vm-string-input-stream (or contents "")))

(defun vm-string-input-stream-read-char (stream)
  "Read one character from VM string input STREAM, respecting unread pushback."
  (let ((unread (vm-string-input-stream-unread-character stream)))
    (if unread
        (progn
          (setf (vm-string-input-stream-unread-character stream) nil)
          unread)
        (let ((position (vm-string-input-stream-position stream))
              (contents (vm-string-input-stream-contents stream)))
          (if (< position (length contents))
              (prog1 (char contents position)
                (setf (vm-string-input-stream-position stream) (1+ position)))
              +eof-value+)))))

(defun vm-string-input-stream-unread-char (character stream)
  "Push CHARACTER back onto VM string input STREAM."
  (when (vm-string-input-stream-unread-character stream)
    (error "vm-unread-char: String input stream already has unread character"))
  (setf (vm-string-input-stream-unread-character stream) character)
  nil)

(defun vm-string-input-stream-peek-char (stream)
  "Return next character from VM string input STREAM without consuming it."
  (let ((character (vm-string-input-stream-read-char stream)))
    (unless (eq character +eof-value+)
      (vm-string-input-stream-unread-char character stream))
    character))

(defun vm-string-input-stream-listen (stream)
  "Return true when VM string input STREAM has available input."
  (or (vm-string-input-stream-unread-character stream)
      (< (vm-string-input-stream-position stream)
         (length (vm-string-input-stream-contents stream)))))

(defun vm-string-input-stream-read-line (stream)
  "Read one line from VM string input STREAM."
  (let ((chars '()))
    (loop for character = (vm-string-input-stream-read-char stream)
          do (cond
               ((eq character +eof-value+)
                (if chars
                    (return (values (coerce (nreverse chars) 'string) t))
                    (return (values +eof-value+ t))))
               ((char= character #\Newline)
                (return (values (coerce (nreverse chars) 'string) nil)))
               (t
                (push character chars))))))

(defun vm-string-input-stream-file-position (stream &optional new-position-p new-position)
  "Get or set VM string input STREAM position."
  (if new-position-p
      (let ((contents (vm-string-input-stream-contents stream)))
        (if (and (integerp new-position)
                 (<= 0 new-position (length contents)))
            (progn
              (setf (vm-string-input-stream-position stream) new-position
                    (vm-string-input-stream-unread-character stream) nil)
              t)
            nil))
      (vm-string-input-stream-position stream)))

(defun vm-string-input-stream-file-length (stream)
  "Return length of VM string input STREAM contents."
  (length (vm-string-input-stream-contents stream)))

(defun vm-string-input-stream-clear-input (stream)
  "Clear unread input from VM string input STREAM."
  (setf (vm-string-input-stream-unread-character stream) nil)
  nil)

(defun vm-get-stream (state handle)
  "Resolve HANDLE to a CL stream from STATE.
Accepts: direct CL stream objects, +stdin-handle+ (0), +stdout-handle+ (1),
or any handle allocated by vm-allocate-file-handle."
  (cond
    ((streamp handle)              handle)
    ((eql handle +stdin-handle+)   (vm-standard-input state))
    ((eql handle +stdout-handle+)  (vm-standard-output state))
    (t (or (gethash handle (vm-open-files state))
           (gethash handle (vm-string-streams state))
           (error "Invalid file handle: ~A" handle)))))

(defun vm-allocate-file-handle (state)
  "Allocate and return a new unique integer file handle (>= 2 to avoid stdin/stdout)."
  (let ((handle (max 2 (1+ (vm-file-counter state)))))
    (setf (vm-file-counter state) handle)
    handle))

(defun vm-stream-open-p (state handle)
  "Return true if HANDLE refers to a currently open stream in STATE."
  (or (streamp handle)
      (and (eql handle +stdin-handle+)  (vm-standard-input state))
      (and (eql handle +stdout-handle+) (vm-standard-output state))
      (gethash handle (vm-open-files state))
      (gethash handle (vm-string-streams state))))

(defun %vm-bridge-stream-arg (value)
  "Resolve a VM stream handle passed through a host bridge callable."
  (if *vm-current-state*
      (vm-get-stream *vm-current-state* value)
      value))

(defun %vm-stream-handle-for-stream (state stream)
  "Return the VM handle for STREAM in STATE, or NIL when STREAM is direct."
  (let ((found nil))
    (maphash (lambda (handle candidate)
               (when (eq candidate stream)
                 (setf found handle)))
             (vm-open-files state))
    (unless found
      (maphash (lambda (handle candidate)
                 (when (eq candidate stream)
                   (setf found handle)))
               (vm-string-streams state)))
    found))

(defun %vm-bridge-stream-result (stream)
  "Convert host STREAM back to its VM handle when one exists."
  (if *vm-current-state*
      (or (%vm-stream-handle-for-stream *vm-current-state* stream) stream)
      stream))

(defun %vm-bridge-symbol-stream-value (symbol)
  "Return SYMBOL's current stream value, preferring the active VM global store."
  (if *vm-current-state*
      (multiple-value-bind (value found-p)
          (gethash symbol (vm-global-vars *vm-current-state*))
        (if found-p
            (%vm-bridge-stream-arg value)
            (symbol-value symbol)))
      (symbol-value symbol)))

(defun %vm-bridge-make-synonym-stream (symbol)
  "Construct a usable stream for VM synonym-stream calls.

Host synonym streams follow host dynamic bindings, not VM global bindings, so
the VM bridge resolves the current VM symbol value to its underlying stream."
  (if *vm-current-state*
      (%vm-bridge-symbol-stream-value symbol)
      (make-synonym-stream symbol)))

(defun %vm-bridge-make-broadcast-stream (&rest streams)
  (apply #'make-broadcast-stream (mapcar #'%vm-bridge-stream-arg streams)))

(defun %vm-bridge-make-two-way-stream (input-stream output-stream)
  (make-two-way-stream (%vm-bridge-stream-arg input-stream)
                       (%vm-bridge-stream-arg output-stream)))

(defun %vm-bridge-make-echo-stream (input-stream output-stream)
  (make-echo-stream (%vm-bridge-stream-arg input-stream)
                    (%vm-bridge-stream-arg output-stream)))

(defun %vm-input-stream-to-host (value)
  "Coerce a VM string-input-stream to a host string-input-stream (reading from
its current position) so host stream constructors accept it. Other values pass
through after the usual bridge handle resolution."
  (let ((resolved (%vm-bridge-stream-arg value)))
    (if (vm-string-input-stream-p resolved)
        (make-string-input-stream
         (subseq (vm-string-input-stream-contents resolved)
                 (vm-string-input-stream-position resolved)))
        resolved)))

(defun %vm-bridge-make-concatenated-stream (&rest streams)
  ;; make-concatenated-stream is read-only, so converting each VM string-input
  ;; stream to an equivalent host string-input-stream is sufficient.
  (apply #'make-concatenated-stream (mapcar #'%vm-input-stream-to-host streams)))

(defun %vm-bridge-file-position (stream &optional (position nil position-p))
  "Host bridge for ANSI FILE-POSITION that accepts VM stream handles."
  (let ((resolved (%vm-bridge-stream-arg stream)))
    (if position-p
        (if (file-position resolved position) t nil)
        (file-position resolved))))

(defun %vm-bridge-file-length (stream)
  "Host bridge for ANSI FILE-LENGTH that accepts VM stream handles."
  (file-length (%vm-bridge-stream-arg stream)))

(defun make-buffered-stream (stream &key (buffer-size 4096) (strategy :full))
  "Return STREAM with host buffering semantics.

CL-CC delegates concrete stream buffering to the host Common Lisp stream.  This
helper validates the ANSI-facing buffering options used by the VM surface and
returns the underlying stream, so FINISH-OUTPUT, FORCE-OUTPUT, CLEAR-INPUT, and
CLEAR-OUTPUT operate on the same host buffer.  STRATEGY is one of :FULL, :LINE,
or :NONE."
  (check-type buffer-size (integer 1 *))
  (unless (member strategy '(:full :line :none))
    (error "Invalid buffering strategy: ~S" strategy))
  (%vm-bridge-stream-arg stream))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %remove-binary-file-default-keys (plist)
    "Return PLIST without :DIRECTION and :ELEMENT-TYPE for WITH-BINARY-FILE."
    (loop for (key value) on plist by #'cddr
          unless (member key '(:direction :element-type))
            append (list key value))))

(defmacro with-binary-file ((stream filespec &rest options
                                    &key (direction :io) (element-type ''(unsigned-byte 8))
                                    &allow-other-keys)
                            &body body)
  "Open FILESPEC as a binary file and execute BODY with STREAM bound.

Defaults to :DIRECTION :IO and ELEMENT-TYPE '(UNSIGNED-BYTE 8), matching the VM
random-access binary I/O use case."
  (declare (ignore direction element-type))
  `(with-open-file (,stream ,filespec
                    :direction ,(or (getf options :direction) :io)
                    :element-type ,(or (getf options :element-type) ''(unsigned-byte 8))
                    ,@(%remove-binary-file-default-keys options))
     ,@body))

(defun %vm-bridge-get-output-stream-string (stream)
  (get-output-stream-string (%vm-bridge-stream-arg stream)))

(defun %vm-bridge-stream-control (function stream)
  (funcall function (%vm-bridge-stream-arg stream)))

(defun %vm-bridge-broadcast-stream-streams (stream)
  (mapcar #'%vm-bridge-stream-result (broadcast-stream-streams stream)))

(defun %vm-bridge-two-way-stream-input-stream (stream)
  (%vm-bridge-stream-result (two-way-stream-input-stream stream)))

(defun %vm-bridge-two-way-stream-output-stream (stream)
  (%vm-bridge-stream-result (two-way-stream-output-stream stream)))

(defun %vm-bridge-echo-stream-input-stream (stream)
  (%vm-bridge-stream-result (echo-stream-input-stream stream)))

(defun %vm-bridge-echo-stream-output-stream (stream)
  (%vm-bridge-stream-result (echo-stream-output-stream stream)))

(defun %vm-bridge-concatenated-stream-streams (stream)
  (mapcar #'%vm-bridge-stream-result (concatenated-stream-streams stream)))

(eval-when (:load-toplevel :execute)
  (defparameter *vm-io-host-bridge-table*
    `((with-standard-io-syntax . ,(lambda (thunk)
                                    (with-standard-io-syntax (funcall thunk))))
      (make-readtable              . ,#'make-readtable)
      (copy-readtable              . ,#'copy-readtable)
      (set-macro-character         . ,#'set-macro-character)
      (get-macro-character         . ,#'get-macro-character)
      (set-dispatch-macro-character . ,#'set-dispatch-macro-character)
      (get-dispatch-macro-character . ,#'get-dispatch-macro-character)
      (readtable-case              . ,#'readtable-case)
      (file-position               . ,#'%vm-bridge-file-position)
      (file-length                 . ,#'%vm-bridge-file-length)
      (make-buffered-stream        . ,#'make-buffered-stream)
      (finish-output               . ,(lambda (&optional (stream *standard-output*))
                                        (%vm-bridge-stream-control #'finish-output stream)))
      (force-output                . ,(lambda (&optional (stream *standard-output*))
                                        (%vm-bridge-stream-control #'force-output stream)))
      (clear-input                 . ,(lambda (&optional (stream *standard-input*))
                                        (%vm-bridge-stream-control #'clear-input stream)))
      (clear-output                . ,(lambda (&optional (stream *standard-output*))
                                        (%vm-bridge-stream-control #'clear-output stream)))
      (stream-element-type         . ,(lambda (stream)
                                        (stream-element-type (%vm-bridge-stream-arg stream))))
      (open-stream-p               . ,(lambda (stream)
                                        (open-stream-p (%vm-bridge-stream-arg stream))))
      (interactive-stream-p        . ,(lambda (stream)
                                        (interactive-stream-p (%vm-bridge-stream-arg stream))))
      (make-string-input-stream    . ,#'make-string-input-stream)
      (make-string-output-stream   . ,#'make-string-output-stream)
      (get-output-stream-string    . ,#'%vm-bridge-get-output-stream-string)
      (make-synonym-stream         . ,#'%vm-bridge-make-synonym-stream)
      (make-broadcast-stream       . ,#'%vm-bridge-make-broadcast-stream)
      (make-two-way-stream         . ,#'%vm-bridge-make-two-way-stream)
      (make-echo-stream            . ,#'%vm-bridge-make-echo-stream)
      (make-concatenated-stream    . ,#'%vm-bridge-make-concatenated-stream)
      (broadcast-stream-streams    . ,#'%vm-bridge-broadcast-stream-streams)
      (two-way-stream-input-stream  . ,#'%vm-bridge-two-way-stream-input-stream)
      (two-way-stream-output-stream . ,#'%vm-bridge-two-way-stream-output-stream)
      (echo-stream-input-stream    . ,#'%vm-bridge-echo-stream-input-stream)
      (echo-stream-output-stream   . ,#'%vm-bridge-echo-stream-output-stream)
      (concatenated-stream-streams . ,#'%vm-bridge-concatenated-stream-streams))
    "Declarative host bridge registration table for I/O-related functions.")

  (dolist (entry *vm-io-host-bridge-table*)
    (vm-register-host-bridge (car entry) (cdr entry))))

;;; I/O instruction class declarations are in io-instructions.lisp (loads next).
