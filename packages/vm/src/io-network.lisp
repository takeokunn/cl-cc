;;;; packages/vm/src/io-network.lisp — VM Network I/O: Sockets, DNS, TLS, mmap
;;;
;;; Contains:
;;;   - tls-unsupported condition
;;;   - vm-socket, dns-cache-entry, dns-async-result, tls-context, vm-mmap-region structs
;;;   - *dns-cache*, *dns-default-ttl* parameters
;;;   - %unsupported helper
;;;   - TCP/UDP socket creation and operations
;;;   - DNS resolution (sync and async)
;;;   - TLS context/wrapping helpers
;;;   - Memory-mapped file operations
;;;
;;; Load order: after io.lisp (core stream bridge, encoding helpers, vm-open).

(in-package :cl-cc/vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-bsd-sockets))
  (ignore-errors (require :sb-posix)))

;;; ─── Data types and variables for networking/mmap ────────────────────────────

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

;;; ─── Socket protocol helpers ─────────────────────────────────────────────────

(defun %socket-class (family)
  (ecase family
    ((:ipv4 :inet 4) 'sb-bsd-sockets:inet-socket)
    ((:ipv6 :inet6 6) 'sb-bsd-sockets:inet6-socket)))

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
               (sb-bsd-sockets:host-ent-address entry))))))))

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
  (declare (ignore dual-stack))
  (let ((socket (make-instance (%socket-class family)
                               :type :stream :protocol :tcp)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (when tcp-nodelay
      (ignore-errors (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)))
    (when keepalive
      (ignore-errors (setf (sb-bsd-sockets:sockopt-keep-alive socket) t)))
    (make-vm-socket :backend socket :family family :type :tcp)))

(defun make-udp-socket (&key (family :ipv4) reuse-address dual-stack)
  "Create a UDP socket object backed by SBCL sb-bsd-sockets."
  (declare (ignore dual-stack))
  (let ((socket (make-instance (%socket-class family)
                               :type :datagram :protocol :udp)))
    (when reuse-address
      (setf (sb-bsd-sockets:sockopt-reuse-address socket) t))
    (make-vm-socket :backend socket :family family :type :udp)))

(defun socket-bind (socket host port)
  "Bind SOCKET to HOST and PORT. Use port 0 for an ephemeral localhost port."
  (sb-bsd-sockets:socket-bind (vm-socket-backend socket)
                              (%host-address host (vm-socket-family socket))
                              port)
  socket)

(defun socket-listen (socket &optional (backlog 5))
  "Mark SOCKET as a TCP listener."
  (sb-bsd-sockets:socket-listen (vm-socket-backend socket) backlog)
  socket)

(defun socket-connect (socket host port)
  "Connect SOCKET to HOST and PORT."
  (sb-bsd-sockets:socket-connect (vm-socket-backend socket)
                                 (%host-address host (vm-socket-family socket))
                                 port)
  socket)

(defun socket-accept (socket)
  "Accept one TCP connection from listener SOCKET and return a socket object."
  (make-vm-socket :backend (sb-bsd-sockets:socket-accept (vm-socket-backend socket))
                  :family (vm-socket-family socket)
                  :type :tcp))

(defun make-socket-stream (socket &key (element-type '(unsigned-byte 8))
                                    (input t) (output t) buffering)
  "Create a bidirectional Gray/binary stream from SOCKET."
  (or (vm-socket-stream socket)
      (setf (vm-socket-stream socket)
            (sb-bsd-sockets:socket-make-stream
             (vm-socket-backend socket)
             :element-type element-type
             :input input :output output
             :buffering (or buffering :full)))))

(defun socket-send (socket data &key (start 0) end host port)
  "Send octets or a string over SOCKET. UDP may pass HOST and PORT."
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
        (sb-bsd-sockets:socket-send (vm-socket-backend socket) payload (length payload)))))

(defun socket-receive (socket &key (size 4096) stringp)
  "Receive up to SIZE bytes from SOCKET. Return data, count, peer address, and peer port."
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (multiple-value-bind (data count address port)
        (sb-bsd-sockets:socket-receive (vm-socket-backend socket) buffer size)
      (let* ((payload (subseq data 0 count))
             (peer-address (if (consp address) (first address) address))
             (peer-port (if (consp address) (second address) port)))
        (values (if stringp (map 'string #'code-char payload) payload)
                count
                (and peer-address (%ip-string peer-address))
                peer-port)))))

(defun socket-close (socket)
  "Close SOCKET and any stream created for it."
  (when (vm-socket-stream socket)
    (ignore-errors (close (vm-socket-stream socket))))
  (ignore-errors (sb-bsd-sockets:socket-close (vm-socket-backend socket)))
  t)

(defmacro with-socket ((var socket-form) &body body)
  "Bind VAR to SOCKET-FORM and always close it after BODY."
  `(let ((,var ,socket-form))
     (unwind-protect (progn ,@body)
       (when ,var (socket-close ,var)))))

(defun socket-local-address (socket)
  "Return SOCKET's local address and port as two values."
  (multiple-value-bind (address port)
      (sb-bsd-sockets:socket-name (vm-socket-backend socket))
    (values (%ip-string address) port)))

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
  (let ((key (list :resolve (string host))))
    (or (%dns-cache-lookup key)
        (%dns-cache-store
         key
         (delete-duplicates
          (mapcar #'%ip-string
                  (sb-bsd-sockets:host-ent-addresses
                   (sb-bsd-sockets:get-host-by-name (string host))))
          :test #'string=)
         ttl))))

(defun dns-reverse-resolve (ip &key (ttl *dns-default-ttl*))
  "Resolve IP to a cached PTR hostname."
  (let ((key (list :reverse (string ip))))
    (or (%dns-cache-lookup key)
        (%dns-cache-store
         key
         (sb-bsd-sockets:host-ent-name
          (sb-bsd-sockets:get-host-by-address (%parse-ipv4 ip)))
         ttl))))

(defun getaddrinfo (host &key service (family :ipv4))
  "Small POSIX-like getaddrinfo wrapper returning plist entries."
  (mapcar (lambda (ip) (list :address ip :service service :family family))
          (dns-resolve host)))

(defun dns-resolve-async (host &key (ttl *dns-default-ttl*))
  "Resolve HOST on a background thread. Poll with DNS-ASYNC-RESULT-DONE-P."
  (let ((result (make-dns-async-result)))
    (setf (dns-async-result-thread result)
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (setf (dns-async-result-result result) (dns-resolve host :ttl ttl))
               (error (e) (setf (dns-async-result-error result) e)))
             (setf (dns-async-result-done-p result) t))
           :name "cl-cc dns-resolve-async"))
    result))

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
