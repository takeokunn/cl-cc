(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — I/O State, Constants, and Stream Helpers
;;;
;;; Contains:
;;;   - vm-io-state CLOS class (open-files, file-counter, std I/O, string-streams)
;;;   - +stdin-handle+, +stdout-handle+, +eof-value+ constants
;;;   - vm-get-stream      — resolve handle → CL stream
;;;   - vm-allocate-file-handle — allocate a fresh file handle integer
;;;   - vm-stream-open-p   — check if a handle is currently open
;;;
;;; All define-vm-instruction forms for I/O operations are in io-instructions.lisp
;;; (loads immediately after this file).
;;; execute-instruction methods are in io-execute.lisp.
;;; run-compiled-with-io / run-string-with-io are in io-runners.lisp.
;;;
;;; Load order: after vm-run.lisp, before io-instructions.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── VM I/O State ────────────────────────────────────────────────────────────

(defparameter *print-level* nil
  "Maximum depth to print, or NIL for unlimited depth.")

(defparameter *print-length* nil
  "Maximum number of elements to print at each level, or NIL for unlimited length.")

(defparameter *print-circle* nil
  "When true, detect and mark circular/shared printed structure.")

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

(defstruct vm-circle-print-context
  (counts (make-hash-table :test #'eq))
  (labels (make-hash-table :test #'eq))
  (printed (make-hash-table :test #'eq))
  (next-label 0 :type fixnum))

(defun %vm-circle-trackable-p (object)
  (or (consp object) (vectorp object) (hash-table-p object)
      (typep object 'standard-object)))

(defun %vm-circle-object-slots (object)
  #+sbcl
  (handler-case
      (loop for slot in (class-slots (class-of object))
            for name = (slot-definition-name slot)
            when (slot-boundp object name) collect (cons name (slot-value object name)))
    (error () nil))
  #-sbcl (declare (ignore object))
  #-sbcl nil)

(defun %vm-circle-labeling-visit (object ctx seen)
  "Phase 1: DFS through compound objects and count repeated visits."
  (when (%vm-circle-trackable-p object)
    (incf (gethash object (vm-circle-print-context-counts ctx) 0))
    (unless (gethash object seen)
      (setf (gethash object seen) t)
      (cond
        ((consp object)
         ;; Traverse the CDR chain iteratively, stopping when we revisit a cons
         ;; (circular back-reference) so the DFS terminates.
         (loop for tail = object then (cdr tail)
               while (consp tail)
               do (let ((next (cdr tail)))
                    (%vm-circle-labeling-visit (car tail) ctx seen)
                    ;; If next cell is already seen, count the back-reference and stop
                    (when (and (consp next) (gethash next seen))
                      (%vm-circle-labeling-visit next ctx seen)
                      (return)))
               finally (when tail (%vm-circle-labeling-visit tail ctx seen))))
        ((vectorp object)
         (loop for element across object do (%vm-circle-labeling-visit element ctx seen)))
        ((hash-table-p object)
         (maphash (lambda (key value)
                    (%vm-circle-labeling-visit key ctx seen)
                    (%vm-circle-labeling-visit value ctx seen))
                  object))
        ((typep object 'standard-object)
         (dolist (slot (%vm-circle-object-slots object))
           (%vm-circle-labeling-visit (cdr slot) ctx seen))))))
  object)

(defun %vm-circle-assign-labels (ctx)
  (maphash (lambda (object count)
             (when (> count 1)
               (setf (gethash object (vm-circle-print-context-labels ctx))
                     (prog1 (vm-circle-print-context-next-label ctx)
                       (incf (vm-circle-print-context-next-label ctx))))))
           (vm-circle-print-context-counts ctx))
  ctx)

(defun %vm-circle-write-atom (object stream escape-p)
  (let ((cl:*print-escape* escape-p)
        (cl:*print-case* *print-case*)
        (cl:*print-base* *print-base*)
        (cl:*print-radix* *print-radix*)
        (cl:*print-gensym* *print-gensym*)
        (cl:*print-array* *print-array*)
        (cl:*print-lines* *print-lines*)
        (cl:*print-right-margin* *print-right-margin*))
    (write object :stream stream)))

(defun %vm-circle-write-object (object stream ctx escape-p)
  "Phase 2: emit #n= at first occurrence and #n# for later occurrences."
  (let ((label (and (%vm-circle-trackable-p object)
                    (gethash object (vm-circle-print-context-labels ctx)))))
    (when label
      (if (gethash object (vm-circle-print-context-printed ctx))
          (progn (format stream "#~D#" label)
                 (return-from %vm-circle-write-object object))
          (progn (setf (gethash object (vm-circle-print-context-printed ctx)) t)
                 (format stream "#~D=" label))))
    (cond
      ((consp object)
       (write-char #\( stream)
       (loop for tail = object then (cdr tail)
             for firstp = t then nil
             while (consp tail)
             do (unless firstp (write-char #\Space stream))
                (%vm-circle-write-object (car tail) stream ctx escape-p)
                (let* ((next (cdr tail))
                       (next-label (and (%vm-circle-trackable-p next)
                                        (gethash next (vm-circle-print-context-labels ctx)))))
                  (when (and next-label
                             (gethash next (vm-circle-print-context-printed ctx)))
                    (write-string " . " stream)
                    (%vm-circle-write-object next stream ctx escape-p)
                    (return)))
             finally (when tail
                       (write-string " . " stream)
                       (%vm-circle-write-object tail stream ctx escape-p)))
       (write-char #\) stream))
      ((vectorp object)
       (if *print-array*
           (progn
             (write-string "#(" stream)
             (loop for i from 0 below (length object)
                   do (when (plusp i) (write-char #\Space stream))
                      (%vm-circle-write-object (aref object i) stream ctx escape-p))
             (write-char #\) stream))
           (format stream "#<~A ~D>" (type-of object) (length object))))
      ((hash-table-p object)
       (write-string "#<HASH-TABLE" stream)
       (maphash (lambda (key value)
                  (write-char #\Space stream)
                  (%vm-circle-write-object key stream ctx escape-p)
                  (write-string " => " stream)
                  (%vm-circle-write-object value stream ctx escape-p))
                object)
       (write-char #\> stream))
      ((typep object 'standard-object)
       (format stream "#<~A" (class-name (class-of object)))
       (dolist (slot (%vm-circle-object-slots object))
         (format stream " ~S " (car slot))
         (%vm-circle-write-object (cdr slot) stream ctx escape-p))
       (write-char #\> stream))
      (t (%vm-circle-write-atom object stream escape-p))))
  object)

(defun vm-write-object-to-string (object &key (escape t) (circle *print-circle*))
  "Return OBJECT's printed representation, using the host fast path unless CIRCLE is true."
  (if (not circle)
      (let ((cl:*print-escape* escape)
            (cl:*print-case* *print-case*)
            (cl:*print-base* *print-base*)
            (cl:*print-radix* *print-radix*)
            (cl:*print-gensym* *print-gensym*)
            (cl:*print-array* *print-array*)
            (cl:*print-lines* *print-lines*)
            (cl:*print-right-margin* *print-right-margin*)
            (cl:*print-level* *print-level*)
            (cl:*print-length* *print-length*)
            (cl:*print-pretty* *print-pretty*)
            (cl:*print-readably* *print-readably*))
        (write-to-string object))
      (let ((ctx (make-vm-circle-print-context)))
        (%vm-circle-labeling-visit object ctx (make-hash-table :test #'eq))
        (%vm-circle-assign-labels ctx)
        (with-output-to-string (out)
          (%vm-circle-write-object object out ctx escape)))))

(defparameter *print-readably* nil
  "When true, signal if an object cannot be printed readably.")

(defparameter *print-base* 10
  "Radix used when printing integers.")

(defparameter *print-radix* nil
  "When true, print radix markers for numbers.")

(defparameter *print-pretty* nil
  "When true, use the pretty printer for structured output.")

(declaim (special *print-pprint-dispatch* *readtable*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-bsd-sockets))
  #+sbcl (ignore-errors (require :sb-posix)))

;;; ─── Host-backed sockets, DNS, TLS, and mmap FR helpers ─────────────────────

(define-condition tls-unsupported (error)
  ((reason :initarg :reason :reader tls-unsupported-reason))
  (:report (lambda (condition stream)
             (format stream "TLS backend unsupported: ~a"
                     (tls-unsupported-reason condition)))))

(defstruct vm-socket
  "Host-backed socket descriptor for FR-851."
  backend
  (family :ipv4)
  (type :tcp)
  stream)

(defstruct dns-cache-entry value expires-at)
(defstruct dns-async-result thread result error done-p)
(defstruct (tls-context (:constructor %make-tls-context))
  verify-peer ca-bundle cert-file key-file server-p)

(defstruct vm-mmap-region
  path protection flags length buffer array stream dirty-p closed-p)

(defvar *dns-cache* (make-hash-table :test #'equal)
  "DNS cache keyed by operation and input, with per-entry TTLs.")

(defparameter *dns-default-ttl* 60
  "Default DNS cache TTL in seconds.")

(defun %unsupported (feature)
  (error "~a is unsupported on this host Lisp" feature))

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
       #+sbcl
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
               :external-format (%vm-host-external-format format))))))
       #-sbcl
       (declare (ignore error-mode))
       #-sbcl
       (error "VM external-format encoding requires SBCL for ~S" format)))))

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
           #+sbcl
           (handler-case
               (sb-ext:octets-to-string payload :external-format (%vm-host-external-format format))
             (error (e)
               (ecase error-mode
                 (:error (error e))
                 (:replace (string (%vm-replacement-character)))
                 (:ignore ""))))
           #-sbcl
           (declare (ignore error-mode))
           #-sbcl
           (error "VM external-format decoding requires SBCL for ~S" format)))))))

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

(defun %socket-class (family)
  #+sbcl
  (ecase family
    ((:ipv4 :inet 4) 'sb-bsd-sockets:inet-socket)
    ((:ipv6 :inet6 6) 'sb-bsd-sockets:inet6-socket))
  #-sbcl (declare (ignore family))
  #-sbcl (%unsupported "sockets"))

(defun %socket-type (type)
  (ecase type
    ((:tcp :stream) :stream)
    ((:udp :datagram :dgram) :datagram)))

(defun %socket-protocol (type)
  (ecase type
    ((:tcp :stream) :tcp)
    ((:udp :datagram :dgram) :udp)))

(defun %parse-ipv4 (host)
  (let ((parts nil) (start 0) (text (string host)))
    (loop for pos = (position #\. text :start start)
          do (push (parse-integer text :start start :end pos) parts)
          if pos do (setf start (1+ pos)) else do (return))
    (let ((octets (nreverse parts)))
      (unless (= (length octets) 4)
        (error "Invalid IPv4 address: ~a" host))
      (make-array 4 :element-type '(unsigned-byte 8)
                    :initial-contents octets))))

(defun %host-address (host family)
  #+sbcl
  (cond
    ((and (vectorp host) (not (stringp host))) host)
    ((member family '(:ipv6 :inet6 6))
     (sb-bsd-sockets:make-inet6-address (string host)))
    (t
     (handler-case
         (sb-bsd-sockets:make-inet-address (string host))
        (error ()
          (let ((entry (sb-bsd-sockets:get-host-by-name (string host))))
            (or (first (sb-bsd-sockets:host-ent-addresses entry))
                (sb-bsd-sockets:host-ent-address entry)))))))
  #-sbcl (declare (ignore host family))
  #-sbcl (%unsupported "socket address conversion"))

(defun %ip-string (address)
  (cond
    ((and (vectorp address) (= (length address) 4))
     (format nil "~{~d~^.~}" (coerce address 'list)))
    ((vectorp address)
     (format nil "~{~x~^:~}"
             (loop for i from 0 below (length address) by 2
                   collect (+ (ash (aref address i) 8)
                              (aref address (1+ i))))))
    (t (string address))))

(defun make-tcp-socket (&key (family :ipv4)
                             reuse-address tcp-nodelay keepalive dual-stack)
  "Create a TCP socket object backed by SBCL sb-bsd-sockets."
  #+sbcl
  (let ((socket (make-instance (%socket-class family)
                               :type :stream :protocol :tcp)))
    (declare (ignore dual-stack))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (when tcp-nodelay
      (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)))
    (when keepalive
      (ignore-errors (setf (sb-bsd-sockets:sockopt-keep-alive socket) t)))
    (make-vm-socket :backend socket :family family :type :tcp))
  #-sbcl (declare (ignore family reuse-address tcp-nodelay keepalive dual-stack))
  #-sbcl (%unsupported "TCP sockets"))

(defun make-udp-socket (&key (family :ipv4) reuse-address dual-stack)
  "Create a UDP socket object backed by SBCL sb-bsd-sockets."
  #+sbcl
  (let ((socket (make-instance (%socket-class family)
                               :type :datagram :protocol :udp)))
    (declare (ignore dual-stack))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (make-vm-socket :backend socket :family family :type :udp))
  #-sbcl (declare (ignore family reuse-address dual-stack))
  #-sbcl (%unsupported "UDP sockets"))

(defun socket-bind (socket host port)
  "Bind SOCKET to HOST and PORT. Use port 0 for an ephemeral localhost port."
  #+sbcl
  (progn
    (sb-bsd-sockets:socket-bind (vm-socket-backend socket)
                                (%host-address host (vm-socket-family socket))
                                port)
    socket)
  #-sbcl (declare (ignore socket host port))
  #-sbcl (%unsupported "socket-bind"))

(defun socket-listen (socket &optional (backlog 5))
  "Mark SOCKET as a TCP listener."
  #+sbcl (progn (sb-bsd-sockets:socket-listen (vm-socket-backend socket) backlog) socket)
  #-sbcl (declare (ignore socket backlog))
  #-sbcl (%unsupported "socket-listen"))

(defun socket-connect (socket host port)
  "Connect SOCKET to HOST and PORT."
  #+sbcl
  (progn
    (sb-bsd-sockets:socket-connect (vm-socket-backend socket)
                                   (%host-address host (vm-socket-family socket))
                                   port)
    socket)
  #-sbcl (declare (ignore socket host port))
  #-sbcl (%unsupported "socket-connect"))

(defun socket-accept (socket)
  "Accept one TCP connection from listener SOCKET and return a socket object."
  #+sbcl
  (make-vm-socket :backend (sb-bsd-sockets:socket-accept (vm-socket-backend socket))
                  :family (vm-socket-family socket)
                  :type :tcp)
  #-sbcl (declare (ignore socket))
  #-sbcl (%unsupported "socket-accept"))

(defun make-socket-stream (socket &key (element-type '(unsigned-byte 8))
                                    (input t) (output t) buffering)
  "Create a bidirectional Gray/binary stream from SOCKET."
  #+sbcl
  (or (vm-socket-stream socket)
      (setf (vm-socket-stream socket)
            (sb-bsd-sockets:socket-make-stream
             (vm-socket-backend socket)
             :element-type element-type
             :input input :output output
             :buffering (or buffering :full))))
  #-sbcl (declare (ignore socket element-type input output buffering))
  #-sbcl (%unsupported "make-socket-stream"))

(defun socket-send (socket data &key (start 0) end host port)
  "Send octets or a string over SOCKET. UDP may pass HOST and PORT."
  #+sbcl
  (let* ((end (or end (length data)))
         (payload (make-array (- end start) :element-type '(unsigned-byte 8))))
    (loop for src from start below end
          for dst from 0
          do (setf (aref payload dst)
                   (etypecase data
                     (string (char-code (char data src)))
                     (vector (aref data src)))))
    (if (and host port)
        (sb-bsd-sockets:socket-send (vm-socket-backend socket) payload (length payload)
                                    :address (list (%host-address host (vm-socket-family socket)) port))
        (sb-bsd-sockets:socket-send (vm-socket-backend socket) payload (length payload))))
  #-sbcl (declare (ignore socket data start end host port))
  #-sbcl (%unsupported "socket-send"))

(defun socket-receive (socket &key (size 4096) stringp)
  "Receive up to SIZE bytes from SOCKET. Return data, count, peer address, and peer port."
  #+sbcl
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (multiple-value-bind (data count address port)
        (sb-bsd-sockets:socket-receive (vm-socket-backend socket) buffer size)
      (let* ((payload (subseq data 0 count))
             (peer-address (if (consp address) (first address) address))
             (peer-port (if (consp address) (second address) port)))
        (values (if stringp (map 'string #'code-char payload) payload)
                count
                (and peer-address (%ip-string peer-address))
                peer-port))))
  #-sbcl (declare (ignore socket size stringp))
  #-sbcl (%unsupported "socket-receive"))

(defun socket-close (socket)
  "Close SOCKET and any stream created for it."
  #+sbcl
  (progn
    (when (vm-socket-stream socket)
      (ignore-errors (close (vm-socket-stream socket))))
    (ignore-errors (sb-bsd-sockets:socket-close (vm-socket-backend socket)))
    t)
  #-sbcl (declare (ignore socket))
  #-sbcl (%unsupported "socket-close"))

(defmacro with-socket ((var socket-form) &body body)
  "Bind VAR to SOCKET-FORM and always close it after BODY."
  `(let ((,var ,socket-form))
     (unwind-protect (progn ,@body)
       (when ,var (socket-close ,var)))))

(defun socket-local-address (socket)
  "Return SOCKET's local address and port as two values."
  #+sbcl
  (multiple-value-bind (address port)
      (sb-bsd-sockets:socket-name (vm-socket-backend socket))
    (values (%ip-string address) port))
  #-sbcl (declare (ignore socket))
  #-sbcl (%unsupported "socket-local-address"))

(defun %dns-cache-lookup (key)
  (let ((entry (gethash key *dns-cache*)))
    (when (and entry (> (dns-cache-entry-expires-at entry) (get-universal-time)))
      (dns-cache-entry-value entry))))

(defun %dns-cache-store (key value ttl)
  (setf (gethash key *dns-cache*)
        (make-dns-cache-entry :value value
                              :expires-at (+ (get-universal-time) ttl)))
  value)

(defun dns-resolve (host &key (ttl *dns-default-ttl*))
  "Resolve HOST to a cached list of A/AAAA textual addresses."
  #+sbcl
  (let ((key (list :resolve (string host))))
    (or (%dns-cache-lookup key)
        (%dns-cache-store
         key
         (delete-duplicates
          (mapcar #'%ip-string
                  (sb-bsd-sockets:host-ent-addresses
                   (sb-bsd-sockets:get-host-by-name (string host))))
          :test #'string=)
         ttl)))
  #-sbcl (declare (ignore host ttl))
  #-sbcl (%unsupported "dns-resolve"))

(defun dns-reverse-resolve (ip &key (ttl *dns-default-ttl*))
  "Resolve IP to a cached PTR hostname."
  #+sbcl
  (let ((key (list :reverse (string ip))))
    (or (%dns-cache-lookup key)
        (%dns-cache-store
         key
         (sb-bsd-sockets:host-ent-name
          (sb-bsd-sockets:get-host-by-address (%parse-ipv4 ip)))
         ttl)))
  #-sbcl (declare (ignore ip ttl))
  #-sbcl (%unsupported "dns-reverse-resolve"))

(defun getaddrinfo (host &key service (family :ipv4))
  "Small POSIX-like getaddrinfo wrapper returning plist entries."
  (mapcar (lambda (ip) (list :address ip :service service :family family))
          (dns-resolve host)))

(defun dns-resolve-async (host &key (ttl *dns-default-ttl*))
  "Resolve HOST on a background thread. Poll with DNS-ASYNC-RESULT-DONE-P."
  #+sbcl
  (let ((result (make-dns-async-result)))
    (setf (dns-async-result-thread result)
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (setf (dns-async-result-result result) (dns-resolve host :ttl ttl))
               (error (e) (setf (dns-async-result-error result) e)))
             (setf (dns-async-result-done-p result) t))
           :name "cl-cc dns-resolve-async"))
    result)
  #-sbcl (declare (ignore host ttl))
  #-sbcl (%unsupported "dns-resolve-async"))

(defun make-tls-context (&key verify-peer ca-bundle)
  "Create a client TLS context descriptor."
  (%make-tls-context :verify-peer verify-peer :ca-bundle ca-bundle :server-p nil))

(defun tls-server-context (cert-file key-file &key ca-bundle verify-peer)
  "Create a server TLS context descriptor."
  (%make-tls-context :verify-peer verify-peer :ca-bundle ca-bundle
                     :cert-file cert-file :key-file key-file :server-p t))

(defun %cl+ssl-package ()
  (or (find-package :cl+ssl)
      (ignore-errors (require :cl+ssl) (find-package :cl+ssl))))

(defun tls-wrap-socket (socket context &key hostname sni)
  "Wrap SOCKET with CL+SSL when available, otherwise signal TLS-UNSUPPORTED."
  (let ((pkg (%cl+ssl-package)))
    (unless pkg
      (error 'tls-unsupported :reason "CL+SSL is not loadable"))
    (let* ((stream (make-socket-stream socket :element-type '(unsigned-byte 8)))
           (client-fn (find-symbol "MAKE-SSL-CLIENT-STREAM" pkg))
           (server-fn (find-symbol "MAKE-SSL-SERVER-STREAM" pkg))
           (wrapper (if (tls-context-server-p context) server-fn client-fn)))
      (unless (and wrapper (fboundp wrapper))
        (error 'tls-unsupported :reason "CL+SSL stream wrapper is unavailable"))
      (setf (vm-socket-stream socket)
            (if (tls-context-server-p context)
                (funcall wrapper stream
                         :certificate (tls-context-cert-file context)
                         :key (tls-context-key-file context))
                (funcall wrapper stream
                         :hostname (or sni hostname)
                         :verify (tls-context-verify-peer context))))
      socket)))

(defun tls-socket-stream (socket)
  "Return SOCKET's TLS stream; signal when the socket has not been wrapped."
  (or (vm-socket-stream socket)
      (error "Socket has no TLS stream")))

(defun %mmap-protection-writable-p (protection)
  (member protection '(:read-write :write) :test #'eq))

(defun mmap-file (path &key (protection :read) (flags :private) length)
  "Map PATH into a byte array descriptor. Portable backend mirrors file bytes."
  (let* ((truename (namestring (pathname path)))
         (file-length (with-open-file (in truename :direction :input
                                               :element-type '(unsigned-byte 8)
                                               :if-does-not-exist (if (%mmap-protection-writable-p protection)
                                                                       :create
                                                                       :error))
                        (file-length in)))
         (size (or length file-length))
         (buffer (make-array size :element-type '(unsigned-byte 8) :initial-element 0)))
    (when (plusp file-length)
      (with-open-file (in truename :direction :input :element-type '(unsigned-byte 8))
        (read-sequence buffer in :end (min size file-length))))
    (make-vm-mmap-region
     :path truename
     :protection protection
     :flags flags
     :length size
     :buffer buffer
     :array (make-array size :element-type '(unsigned-byte 8) :displaced-to buffer))))

(defun mmap-array (region)
  "Return REGION's displaced byte array for direct access."
  (when (vm-mmap-region-closed-p region)
    (error "mmap region is closed"))
  (vm-mmap-region-array region))

(defun mmap-sync (region &key start end)
  "Flush REGION bytes back to its file when it is shared and writable."
  (when (and (eq (vm-mmap-region-flags region) :shared)
             (%mmap-protection-writable-p (vm-mmap-region-protection region)))
    (with-open-file (out (vm-mmap-region-path region)
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :overwrite
                         :if-does-not-exist :create)
      (write-sequence (vm-mmap-region-buffer region) out
                      :start (or start 0)
                      :end (or end (vm-mmap-region-length region)))))
  (setf (vm-mmap-region-dirty-p region) nil)
  t)

(defun mmap-close (region)
  "Unmap REGION and flush shared writable data before marking it closed."
  (unless (vm-mmap-region-closed-p region)
    (mmap-sync region)
    (setf (vm-mmap-region-closed-p region) t))
  t)

(defmacro with-mmap ((var path &rest options) &body body)
  "Map PATH as VAR and close it after BODY."
  `(let ((,var (mmap-file ,path ,@options)))
     (unwind-protect (progn ,@body)
       (mmap-close ,var))))

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

(defun %vm-bridge-make-concatenated-stream (&rest streams)
  (apply #'make-concatenated-stream (mapcar #'%vm-bridge-stream-arg streams)))

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

(defun get-output-string-stream (stream)
  "Compatibility alias for GET-OUTPUT-STREAM-STRING."
  (get-output-stream-string (%vm-bridge-stream-arg stream)))

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
      (get-output-string-stream    . ,#'get-output-string-stream)
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
