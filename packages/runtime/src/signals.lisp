;;;; OS Signal Handling (FR-348, FR-572)
(in-package :cl-cc/runtime)

(defconstant +rt-sigint+ 2)
(defconstant +rt-sighup+ 1)
(defconstant +rt-sigfpe+ 8)
(defconstant +rt-sigsegv+ 11)
(defconstant +rt-sigpipe+ 13)
(defconstant +rt-sigterm+ 15)
(defconstant +rt-sigprof+ 27)
(defconstant +rt-sigusr1+ 10)
(defconstant +rt-sigusr2+ 12)
(defconstant +rt-sigwinch+ 28)

(define-condition rt-os-signal-condition (condition)
  ((signal :initarg :signal :reader rt-os-signal)
   (name :initarg :name :reader rt-os-signal-name))
  (:report (lambda (c s)
             (format s "Runtime OS signal ~a (~d)"
                     (rt-os-signal-name c) (rt-os-signal c)))))

(define-condition rt-segmentation-violation (storage-condition rt-os-signal-condition) ())
(define-condition rt-floating-point-exception (arithmetic-error rt-os-signal-condition) ())
(define-condition rt-interrupt (simple-condition rt-os-signal-condition) ())
(define-condition keyboard-interrupt (rt-interrupt) ())

(defstruct rt-signal-handler
  (signal 0 :type integer)
  (function nil :type (or null function))
  (one-shot-p nil)
  (installed-at 0))

(defvar *rt-signal-handlers* (make-hash-table))
(defvar *rt-sigaction-installed* (make-hash-table))
(defvar *pending-signals* nil
  "Signals received asynchronously and deferred until a runtime safepoint.")
(defvar *shutdown-hooks* nil
  "Functions called by the default SIGTERM handler for graceful shutdown.")
(defvar *rt-signal-mask* nil
  "Portable mirror of the blocked signal set for implementations without sigprocmask.")
(defvar *rt-signal-names*
  `((,+rt-sighup+ . :sighup) (,+rt-sigint+ . :sigint) (,+rt-sigsegv+ . :sigsegv)
    (,+rt-sigfpe+ . :sigfpe) (,+rt-sigpipe+ . :sigpipe)
    (,+rt-sigterm+ . :sigterm) (,+rt-sigprof+ . :sigprof)
    (,+rt-sigusr1+ . :sigusr1) (,+rt-sigusr2+ . :sigusr2)
    (,+rt-sigwinch+ . :sigwinch)))

(defun rt-signal-name (sig)
  (or (cdr (assoc sig *rt-signal-names*)) :unknown))

(defun rt-signal-condition-class (sig)
  (case sig
    (#.+rt-sigint+ 'rt-interrupt)
    (#.+rt-sigsegv+ 'rt-segmentation-violation)
    (#.+rt-sigfpe+ 'rt-floating-point-exception)
    (t 'rt-os-signal-condition)))

(defun rt-signal-to-condition (sig)
  "Map OS signal number SIG to a Common Lisp condition instance."
  (make-condition (rt-signal-condition-class sig)
                  :signal sig
                  :name (rt-signal-name sig)))

(defun rt-remove-signal-handler (handler)
  (let ((sig (rt-signal-handler-signal handler)))
    (setf (gethash sig *rt-signal-handlers*)
          (remove handler (gethash sig *rt-signal-handlers* nil)))
    t))

(defun %rt-record-pending-signal (sig)
  "Record SIG for safepoint-time dispatch. Intended for async host handlers."
  (push sig *pending-signals*)
  sig)

(defun %rt-run-shutdown-hooks (sig)
  (dolist (hook (copy-list *shutdown-hooks*))
    (funcall hook sig))
  t)

(defun rt-install-signal-handler (sig fn &key one-shot)
  "Install FN as the runtime handler for OS signal number SIG.

On SBCL this installs a real host signal interrupt handler with
SB-SYS:ENABLE-INTERRUPT and invokes FN with SIG when the signal is delivered."
  (check-type sig integer)
  (check-type fn function)
  (let (handler)
    (setf handler
          (make-rt-signal-handler :signal sig
                                  :function fn
                                  :one-shot-p one-shot
                                  :installed-at (get-internal-real-time)))
    (sb-sys:enable-interrupt
     sig
     (lambda (&rest interrupt-args)
       (declare (ignore interrupt-args))
       (when one-shot
         (sb-sys:enable-interrupt sig :default)
         (rt-remove-signal-handler handler))
       (funcall fn sig)))
    (setf (gethash sig *rt-sigaction-installed*) :sbcl-installed)
    (push handler (gethash sig *rt-signal-handlers* nil))
    handler))

(defun rt-set-signal-handler (signal function)
  "Install FUNCTION as SIGNAL's handler through the native host signal API.

FUNCTION may be :DEFAULT, :IGNORE, or a function.  User functions are invoked at
safepoints rather than directly inside the async OS signal context."
  (check-type signal integer)
  (cond
    ((eq function :default)
     (sb-sys:enable-interrupt signal :default)
     (remhash signal *rt-signal-handlers*)
     (setf (gethash signal *rt-sigaction-installed*) :default)
     :default)
    ((eq function :ignore)
     (sb-sys:enable-interrupt signal :ignore)
     (remhash signal *rt-signal-handlers*)
     (setf (gethash signal *rt-sigaction-installed*) :ignore)
     :ignore)
    (t
     (check-type function function)
     (setf (gethash signal *rt-signal-handlers*)
           (list (make-rt-signal-handler :signal signal
                                         :function function
                                         :installed-at (get-internal-real-time))))
     (sb-sys:enable-interrupt signal
                              (lambda (&rest args)
                                (declare (ignore args))
                                (%rt-record-pending-signal signal)))
     (setf (gethash signal *rt-sigaction-installed*) :native-deferred)
     function)))

(defun rt-get-signal-handler (signal)
  "Return SIGNAL's runtime handler function, :DEFAULT, or :IGNORE."
  (check-type signal integer)
  (let ((installed (gethash signal *rt-sigaction-installed*)))
    (cond
      ((member installed '(:default :ignore)) installed)
      ((first (gethash signal *rt-signal-handlers*))
       (rt-signal-handler-function (first (gethash signal *rt-signal-handlers*))))
      (t :default))))

(defun rt-signal-mask (&rest signals)
  "Block SIGNALS and return the old portable mask.

On SBCL/POSIX this mirrors the desired mask for runtime bookkeeping; direct
sigprocmask structures are deliberately avoided to keep the API portable across
Common Lisp implementations."
  (let ((old (copy-list *rt-signal-mask*)))
    (dolist (sig signals)
      (pushnew sig *rt-signal-mask*))
    old))

(defun rt-unblock-signal (signal)
  "Remove SIGNAL from the runtime signal mask mirror."
  (setf *rt-signal-mask* (remove signal *rt-signal-mask*))
  signal)

(defmacro rt-with-signal-handler ((sig fn) &body body)
  "Temporarily install FN as SIG's handler while evaluating BODY."
  (let ((old (gensym "OLD-HANDLER")))
    `(let ((,old (rt-get-signal-handler ,sig)))
       (unwind-protect
            (progn
              (rt-set-signal-handler ,sig ,fn)
              ,@body)
         (rt-set-signal-handler ,sig ,old)))))

(defun rt-dispatch-os-signal (sig)
  "Dispatch SIG through registered handlers, then signal mapped CL condition."
  (let ((handlers (copy-list (gethash sig *rt-signal-handlers*))))
    (dolist (handler handlers)
      (when (rt-signal-handler-function handler)
        (funcall (rt-signal-handler-function handler) sig))
      (when (rt-signal-handler-one-shot-p handler)
        (rt-remove-signal-handler handler)))
    (signal (rt-signal-to-condition sig))))

(defun rt-process-pending-signals ()
  "Dispatch signals deferred from async handlers. Called by GC safepoints."
  (let ((signals (nreverse (shiftf *pending-signals* nil))))
    (dolist (sig signals)
      (unless (member sig *rt-signal-mask*)
        (rt-dispatch-os-signal sig)))
    signals))

(defun rt-clear-signal-handlers (&optional sig)
  (if sig
      (remhash sig *rt-signal-handlers*)
      (clrhash *rt-signal-handlers*))
  t)

(defun rt-install-default-signal-handlers ()
  (rt-set-signal-handler +rt-sigint+
                         (lambda (sig)
                           (error (make-condition 'keyboard-interrupt
                                                  :signal sig
                                                  :name (rt-signal-name sig)))))
  (rt-set-signal-handler +rt-sigterm+
                         (lambda (sig)
                           (%rt-run-shutdown-hooks sig)))
  (rt-install-signal-handler +rt-sigsegv+
                             (lambda (sig)
                               (error (rt-signal-to-condition sig))))
  (rt-install-signal-handler +rt-sigfpe+
                             (lambda (sig)
                               (error (rt-signal-to-condition sig))))
  t)

(defun rt-signals-init ()
  (clrhash *rt-signal-handlers*)
  (clrhash *rt-sigaction-installed*)
  (rt-install-default-signal-handlers)
  t)
