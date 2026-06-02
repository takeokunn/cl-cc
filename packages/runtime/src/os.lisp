;;;; OS Abstraction Layer (FR-570, FR-571, FR-573)
(in-package :cl-cc/runtime)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

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

(defstruct rt-process
  "Runtime process object used by RT-RUN-PROGRAM.

PID is the operating-system process id when available.  STATUS tracks the
runtime lifecycle (:RUNNING, :EXITED, :SIGNALED, or :UNKNOWN).  EXIT-CODE is
filled by RT-PROCESS-WAIT or by an eager wait requested from RT-RUN-PROGRAM.
INPUT, OUTPUT, and ERROR are host streams corresponding to the child stdin,
stdout, and stderr pipes when :PIPE was requested.  HOST-PROCESS stores the
implementation/UIOP process handle used for portable process management."
  (pid nil)
  (status :running)
  (exit-code nil)
  (input nil)
  (output nil)
  (error nil)
  (host-process nil))

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
                                        :fd (ignore-errors (sb-sys:fd-stream-fd stream)))))
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

(defun rt-getenv (name) (sb-ext:posix-getenv name))
(defun %rt-sb-posix-call (name &rest args)
  (let ((symbol (find-symbol (string name) :sb-posix)))
    (when (and symbol (fboundp symbol))
      (apply (symbol-function symbol) args))))

(defun rt-setenv (name value &key overwrite)
  (%rt-sb-posix-call :setenv name value (if overwrite 1 0)))

(defun rt-unsetenv (name)
  (%rt-sb-posix-call :unsetenv name))
(defun rt-argv () (or *rt-saved-argv* sb-ext:*posix-argv*))
(defun rt-exit (&optional (code 0)) (sb-ext:exit :code code))

(defun rt-getcwd () (namestring (truename ".")))
(defun rt-chdir (path)
  (%rt-sb-posix-call :chdir (namestring path))
  (setf *default-pathname-defaults* (truename path))
  (namestring *default-pathname-defaults*))

(defun rt-fork ()
  "Fork the current process on POSIX hosts. Returns child pid, 0 in child, or NIL if unsupported."
  (%rt-sb-posix-call :fork))

(defun rt-exec (path argv &key env)
  "Replace current process image with PATH. Stubbed through SB-POSIX on SBCL."
  (declare (ignore env))
  (%rt-sb-posix-call :execv path (coerce argv 'vector)))

(defun rt-waitpid (pid &key nohang)
  (let ((flags (if nohang (or (ignore-errors (symbol-value (find-symbol "WNOHANG" :sb-posix))) 0) 0)))
    (multiple-value-bind (wpid status)
        (%rt-sb-posix-call :waitpid pid flags)
      (make-rt-process-status :pid (or wpid -1) :status (or status 0)
                              :exited-p (and wpid (not (zerop wpid)))))))

(defun %rt-uiop-process-stream-option (role option)
  "Translate runtime process stream OPTION for ROLE into a UIOP launch option."
  (declare (ignore role))
  (etypecase option
    (null nil)
    (stream option)
    (symbol
     (ecase option
       (:pipe :stream)
       (:null nil)
       (:inherit :interactive)))))

(defun %rt-process-info-accessor (name process-info)
  "Call UIOP process-info accessor NAME when it is available."
  (let ((symbol (find-symbol (string name) :uiop)))
    (when (and symbol (fboundp symbol))
      (funcall (symbol-function symbol) process-info))))

(defun %rt-process-info-pid (process-info)
  (or (%rt-process-info-accessor :process-info-pid process-info)
      (ignore-errors (sb-ext:process-pid process-info))))

(defun %rt-process-info-exit-code (process-info)
  (or (%rt-process-info-accessor :process-info-exit-code process-info)
      (ignore-errors (sb-ext:process-exit-code process-info))))

(defun %rt-process-info-stream (process-info accessor fallback-slot)
  (or (%rt-process-info-accessor accessor process-info)
      (ignore-errors (slot-value process-info fallback-slot))))

(defun %rt-command-vector (command args)
  (cons command (mapcar #'princ-to-string args)))

(defun rt-run-program (command args &key (input :inherit) (output :pipe) (error :inherit) (wait nil))
  "Run COMMAND with ARGS and return an RT-PROCESS object.

INPUT, OUTPUT, and ERROR accept :PIPE, :NULL, :INHERIT, or an existing stream.
When WAIT is true, wait for process termination before returning.  The
implementation delegates to UIOP:LAUNCH-PROGRAM, which uses fork/exec with pipe
redirection on POSIX hosts and the platform process API elsewhere."
  (check-type command string)
  (let* ((process-info
           (uiop:launch-program (%rt-command-vector command args)
                                :input (%rt-uiop-process-stream-option :input input)
                                :output (%rt-uiop-process-stream-option :output output)
                                :error-output (%rt-uiop-process-stream-option :error error)
                                :ignore-error-status t))
         (process (make-rt-process
                   :pid (%rt-process-info-pid process-info)
                   :host-process process-info
                   :input (%rt-process-info-stream process-info :process-info-input 'input)
                   :output (%rt-process-info-stream process-info :process-info-output 'output)
                   :error (%rt-process-info-stream process-info :process-info-error-output 'error-output))))
    (when wait
      (rt-process-wait process))
    process))

(defun rt-process-wait (process &optional check-for-stopped)
  "Wait for PROCESS and return its exit code."
  (declare (ignore check-for-stopped))
  (check-type process rt-process)
  (let* ((info (rt-process-host-process process))
         (code (or (ignore-errors (uiop:wait-process info :ignore-error-status t))
                   (%rt-process-info-exit-code info))))
    (setf (rt-process-exit-code process) code
          (rt-process-status process) :exited)
    code))

(defun rt-process-kill (process signal)
  "Send SIGNAL to PROCESS. Returns true when a signal was sent or requested."
  (check-type process rt-process)
  (check-type signal integer)
  (let ((pid (rt-process-pid process)))
    (cond
      (pid
       (%rt-sb-posix-call :kill pid signal)
       t)
      (t
       (let ((info (rt-process-host-process process)))
         (when (fboundp 'uiop:terminate-process)
           (uiop:terminate-process info)
           t))))))

(defun rt-process-alive-p (process)
  "Return true while PROCESS appears to still be running."
  (check-type process rt-process)
  (let ((info (rt-process-host-process process)))
    (cond
      ((eq (rt-process-status process) :exited) nil)
      ((fboundp 'uiop:process-alive-p)
       (uiop:process-alive-p info))
      (t
       (let ((pid (rt-process-pid process)))
         (and pid
              (let ((status (rt-waitpid pid :nohang t)))
                (not (rt-process-status-exited-p status)))))))))

(defun rt-shell (command)
  "Run COMMAND through the user's shell and return stdout as a string."
  (check-type command string)
  (uiop:run-program command :output :string :error-output :interactive :ignore-error-status t))

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
