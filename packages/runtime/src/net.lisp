;;;; Socket/Network Primitives (FR-574)
(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (ignore-errors (require :sb-bsd-sockets)))

(defconstant +rt-af-inet+ 2)
(defconstant +rt-af-inet6+ 10)
(defconstant +rt-sock-stream+ 1)
(defconstant +rt-sock-dgram+ 2)
(defconstant +rt-socket-readable+ 1)
(defconstant +rt-socket-writable+ 2)
(defconstant +rt-socket-error+ 4)

(defstruct rt-socket-address
  (host "127.0.0.1" :type string)
  (port 0 :type integer)
  (family +rt-af-inet+ :type integer))

(defstruct rt-socket-entry
  (fd -1 :type integer)
  (type +rt-sock-stream+ :type integer)
  (protocol :tcp)
  (local-address nil)
  (remote-address nil)
  (state :created)
  (backlog 0 :type integer)
  (nonblocking-p nil)
  (rx-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (peer nil)
  (backend nil))

(defvar *rt-socket-registry* (make-hash-table))
(defvar *rt-listener-registry* (make-hash-table :test #'equal))
(defvar *rt-next-socket-fd* 100)

(defun rt-resolve-address (host port &key (family +rt-af-inet+))
  (make-rt-socket-address :host (string host) :port port :family family))

(defun %rt-addr-key (addr)
  (list (rt-socket-address-host addr) (rt-socket-address-port addr) (rt-socket-address-family addr)))

(defun %rt-socket-entry (fd)
  (or (gethash fd *rt-socket-registry*)
      (error "Bad runtime socket fd: ~a" fd)))

(defun rt-socket (type &key (protocol (if (= type +rt-sock-dgram+) :udp :tcp)) (family +rt-af-inet+))
  (declare (ignore family))
  (let* ((fd (prog1 *rt-next-socket-fd* (incf *rt-next-socket-fd*)))
         (entry (make-rt-socket-entry :fd fd :type type :protocol protocol)))
    (setf (gethash fd *rt-socket-registry*) entry)
    fd))

(defun rt-bind (fd addr)
  (let ((entry (%rt-socket-entry fd)))
    (setf (rt-socket-entry-local-address entry) addr
          (rt-socket-entry-state entry) :bound)
    t))

(defun rt-listen (fd &optional (backlog 5))
  (let ((entry (%rt-socket-entry fd)))
    (unless (rt-socket-entry-local-address entry)
      (error "Socket ~a must be bound before listen" fd))
    (setf (rt-socket-entry-backlog entry) backlog
          (rt-socket-entry-state entry) :listening
          (gethash (%rt-addr-key (rt-socket-entry-local-address entry)) *rt-listener-registry*) fd)
    t))

(defun rt-connect (fd addr)
  (let* ((entry (%rt-socket-entry fd))
         (listener-fd (gethash (%rt-addr-key addr) *rt-listener-registry*)))
    (setf (rt-socket-entry-remote-address entry) addr
          (rt-socket-entry-state entry) :connected)
    (when listener-fd
      (let ((listener (%rt-socket-entry listener-fd)))
        (vector-push-extend fd (rt-socket-entry-rx-buffer listener))))
    t))

(defun rt-accept (fd)
  (let ((entry (%rt-socket-entry fd)))
    (unless (eq (rt-socket-entry-state entry) :listening)
      (error "Socket ~a is not listening" fd))
    (let ((queue (rt-socket-entry-rx-buffer entry)))
      (if (plusp (fill-pointer queue))
          (let* ((client-fd (aref queue 0))
                 (accepted (rt-socket (rt-socket-entry-type entry)
                                      :protocol (rt-socket-entry-protocol entry))))
            (replace queue queue :start1 0 :start2 1)
            (decf (fill-pointer queue))
            (setf (rt-socket-entry-state (%rt-socket-entry accepted)) :connected
                  (rt-socket-entry-local-address (%rt-socket-entry accepted))
                  (rt-socket-entry-local-address entry)
                  (rt-socket-entry-peer (%rt-socket-entry accepted)) client-fd
                  (rt-socket-entry-peer (%rt-socket-entry client-fd)) accepted)
            accepted)
          nil))))

(defun rt-set-nonblocking (fd &optional (enabled t))
  (setf (rt-socket-entry-nonblocking-p (%rt-socket-entry fd)) enabled)
  t)

(defun rt-close-socket (fd)
  (let ((entry (gethash fd *rt-socket-registry*)))
    (when (and entry (rt-socket-entry-local-address entry))
      (remhash (%rt-addr-key (rt-socket-entry-local-address entry)) *rt-listener-registry*))
    (remhash fd *rt-socket-registry*)
    t))

(defun %rt-copy-to-rx-buffer (entry buf start end)
  (loop for i from start below end
        do (vector-push-extend (etypecase buf
                                 (string (char-code (char buf i)))
                                 (vector (aref buf i)))
                               (rt-socket-entry-rx-buffer entry)))
  (- end start))

(defun rt-socket-send (fd buf &key (start 0) end)
  (let* ((entry (%rt-socket-entry fd))
         (peer (and (rt-socket-entry-peer entry) (%rt-socket-entry (rt-socket-entry-peer entry))))
         (limit (or end (length buf))))
    (if peer
        (%rt-copy-to-rx-buffer peer buf start limit)
        0)))

(defun rt-socket-recv (fd buf &key (start 0) end)
  (let* ((entry (%rt-socket-entry fd))
         (rx (rt-socket-entry-rx-buffer entry))
         (limit (or end (length buf)))
         (count (min (- limit start) (fill-pointer rx))))
    (loop for i below count
          do (setf (elt buf (+ start i))
                   (etypecase buf
                     (string (code-char (aref rx i)))
                     (vector (aref rx i)))))
    (when (plusp count)
      (replace rx rx :start1 0 :start2 count)
      (decf (fill-pointer rx) count))
    count))

(defun rt-select (read-fds write-fds error-fds &optional timeout)
  (when timeout (sleep (min timeout 0.001)))
  (values (remove-if-not (lambda (fd) (plusp (fill-pointer (rt-socket-entry-rx-buffer (%rt-socket-entry fd))))) read-fds)
          (remove-if-not (lambda (fd) (gethash fd *rt-socket-registry*)) write-fds)
          (remove-if (lambda (fd) (gethash fd *rt-socket-registry*)) error-fds)))

(defun rt-epoll-create () (list :epoll (make-hash-table)))
(defun rt-epoll-ctl (epoll op fd events)
  (setf (gethash fd (second epoll)) (list op events))
  t)
(defun rt-epoll-wait (epoll &key timeout)
  (declare (ignore timeout))
  (loop for fd being the hash-keys of (second epoll) collect fd))

(defun rt-net-init ()
  (clrhash *rt-socket-registry*)
  (clrhash *rt-listener-registry*)
  (setf *rt-next-socket-fd* 100)
  t)
