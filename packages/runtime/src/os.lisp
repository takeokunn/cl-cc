;;;; OS Abstraction Layer (FR-570, FR-571, FR-573)
(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require :sb-posix))

(defconstant +rt-o-rdonly+ 0)
(defconstant +rt-o-wronly+ 1)
(defconstant +rt-o-rdwr+ 2)
(defconstant +rt-o-creat+ #o100)
(defconstant +rt-o-trunc+ #o1000)
(defconstant +rt-o-append+ #o2000)
(defconstant +rt-default-file-mode+ #o644)

(defstruct rt-file-handle
  "Host-backed runtime file descriptor wrapper."
  (stream nil)
  (path nil)
  (mode :input)
  (fd nil))

(defstruct rt-process-status
  (pid -1 :type integer)
  (status 0 :type integer)
  (exited-p nil)
  (signal nil))

(defvar *rt-saved-argv* nil)
(defvar *rt-max-call-stack-depth* 10000)
(defvar *rt-stack-depth* 0)
(defvar *rt-instruction-limit* nil)
(defvar *rt-instruction-count* 0)
(defvar *rt-open-files* (make-hash-table :test #'eq))
(defvar *rt-platform*
  #+darwin :darwin
  #+linux :linux
  #-(or darwin linux) :unknown)

(defun rt-platform () *rt-platform*)
(defun rt-platform-darwin-p () (eq *rt-platform* :darwin))
(defun rt-platform-linux-p () (eq *rt-platform* :linux))

(defun %rt-direction-for-mode (mode)
  (cond ((member mode '(:input :read +rt-o-rdonly+ 0) :test #'equal) :input)
        ((member mode '(:output :write +rt-o-wronly+ 1) :test #'equal) :output)
        ((member mode '(:io :read-write +rt-o-rdwr+ 2) :test #'equal) :io)
        ((and (integerp mode) (not (zerop (logand mode +rt-o-rdwr+)))) :io)
        ((and (integerp mode) (not (zerop (logand mode +rt-o-wronly+)))) :output)
        (t :input)))

(defun %rt-open-options (mode if-exists if-does-not-exist)
  (let ((direction (%rt-direction-for-mode mode)))
    (values direction
            (or if-exists
                (cond ((and (integerp mode) (not (zerop (logand mode +rt-o-append+)))) :append)
                      ((and (integerp mode) (not (zerop (logand mode +rt-o-trunc+)))) :supersede)
                      ((eq direction :output) :supersede)
                      (t nil)))
            (or if-does-not-exist
                (if (and (integerp mode) (not (zerop (logand mode +rt-o-creat+))))
                    :create
                    (if (eq direction :input) :error :create))))))

(defun rt-open (path mode &key if-exists if-does-not-exist (element-type 'character))
  "Open PATH and return an RT-FILE-HANDLE. MODE accepts keywords or POSIX-like flags."
  (multiple-value-bind (direction exists missing)
      (%rt-open-options mode if-exists if-does-not-exist)
    (let* ((stream (open path :direction direction
                             :if-exists exists
                             :if-does-not-exist missing
                             :element-type element-type))
           (handle (make-rt-file-handle :stream stream :path path :mode direction
                                        :fd #+sbcl (ignore-errors (sb-sys:fd-stream-fd stream))
                                            #-sbcl nil)))
      (setf (gethash handle *rt-open-files*) t)
      handle)))

(defun %rt-stream (handle)
  (etypecase handle
    (rt-file-handle (rt-file-handle-stream handle))
    (stream handle)))

(defun rt-read (handle buffer &key (start 0) end)
  "Read into BUFFER from HANDLE. Return byte/character count, 0 on EOF."
  (let* ((stream (%rt-stream handle))
         (limit (or end (length buffer))))
    (or (read-sequence buffer stream :start start :end limit) 0)))

(defun rt-write (handle buffer &key (start 0) end)
  "Write BUFFER to HANDLE and return the number of elements written."
  (let* ((stream (%rt-stream handle))
         (limit (or end (length buffer))))
    (write-sequence buffer stream :start start :end limit)
    (finish-output stream)
    (- limit start)))

(defun rt-close (handle)
  (let ((stream (%rt-stream handle)))
    (when (typep handle 'rt-file-handle) (remhash handle *rt-open-files*))
    (close stream)
    t))

(defun rt-getenv (name) #+sbcl (sb-ext:posix-getenv name) #-sbcl (declare (ignore name)) #-sbcl nil)
(defun %rt-sb-posix-call (name &rest args)
  #+sbcl
  (let ((symbol (find-symbol (string name) :sb-posix)))
    (when (and symbol (fboundp symbol))
      (apply (symbol-function symbol) args)))
  #-sbcl (declare (ignore name args))
  #-sbcl nil)

(defun rt-setenv (name value &key overwrite)
  (%rt-sb-posix-call :setenv name value (if overwrite 1 0)))

(defun rt-unsetenv (name)
  (%rt-sb-posix-call :unsetenv name))
(defun rt-argv () (or *rt-saved-argv* #+sbcl sb-ext:*posix-argv* #-sbcl (list "cl-cc")))
(defun rt-exit (&optional (code 0)) #+sbcl (sb-ext:exit :code code) #-sbcl (cl:quit code))

(defun rt-getcwd () (namestring (truename ".")))
(defun rt-chdir (path)
  #+sbcl (%rt-sb-posix-call :chdir (namestring path))
  (setf *default-pathname-defaults* (truename path))
  (namestring *default-pathname-defaults*))

(defun rt-fork ()
  "Fork the current process on POSIX hosts. Returns child pid, 0 in child, or NIL if unsupported."
  #+sbcl (%rt-sb-posix-call :fork)
  #-sbcl nil)

(defun rt-exec (path argv &key env)
  "Replace current process image with PATH. Stubbed through SB-POSIX on SBCL."
  (declare (ignore env))
  #+sbcl (%rt-sb-posix-call :execv path (coerce argv 'vector))
  #-sbcl (declare (ignore path argv))
  #-sbcl nil)

(defun rt-waitpid (pid &key nohang)
  #+sbcl
  (let ((flags (if nohang (or (ignore-errors (symbol-value (find-symbol "WNOHANG" :sb-posix))) 0) 0)))
    (multiple-value-bind (wpid status)
        (%rt-sb-posix-call :waitpid pid flags)
      (make-rt-process-status :pid (or wpid -1) :status (or status 0)
                              :exited-p (and wpid (not (zerop wpid))))))
  #-sbcl (declare (ignore pid nohang))
  #-sbcl (make-rt-process-status))

(defun rt-gettime (&optional (clock :monotonic))
  (ecase clock
    (:monotonic (/ (get-internal-real-time) (float internal-time-units-per-second)))
    (:realtime (get-universal-time))))

(defun rt-sleep (seconds)
  (check-type seconds (real 0 *))
  (sleep seconds)
  t)

(defun rt-gettime-monotonic () (rt-gettime :monotonic))
(defun rt-gettime-real () (rt-gettime :realtime))
(defun rt-probe-file (p) (probe-file p))
(defun rt-delete-file (p) (delete-file p) t)
(defun rt-rename-file (old new) (rename-file old new))
(defun rt-file-write-date (p) (file-write-date p))
(defun rt-ensure-directories-exist (p &key verbose) (ensure-directories-exist p :verbose verbose))
(defun rt-directory (p &key) (directory p))

(defun rt-os-init (&key argv)
  (setf *rt-saved-argv* argv)
  (clrhash *rt-open-files*)
  t)

(defun rt-bootstrap-standalone (&key argv entry)
  "Bootstrap hook used by standalone binary stubs before entering compiled code."
  (rt-os-init :argv argv)
  (when entry (funcall entry))
  t)
