;;;; OS Signal Handling (FR-348, FR-572)
(in-package :cl-cc/runtime)

(defconstant +rt-sigint+ 2)
(defconstant +rt-sigfpe+ 8)
(defconstant +rt-sigsegv+ 11)
(defconstant +rt-sigpipe+ 13)
(defconstant +rt-sigterm+ 15)
(defconstant +rt-sigprof+ 27)
(defconstant +rt-sigusr1+ 10)
(defconstant +rt-sigusr2+ 12)

(define-condition rt-os-signal-condition (condition)
  ((signal :initarg :signal :reader rt-os-signal)
   (name :initarg :name :reader rt-os-signal-name))
  (:report (lambda (c s)
             (format s "Runtime OS signal ~a (~d)"
                     (rt-os-signal-name c) (rt-os-signal c)))))

(define-condition rt-segmentation-violation (storage-condition rt-os-signal-condition) ())
(define-condition rt-floating-point-exception (arithmetic-error rt-os-signal-condition) ())
(define-condition rt-interrupt (simple-condition rt-os-signal-condition) ())

(defstruct rt-signal-handler
  (signal 0 :type integer)
  (function nil :type (or null function))
  (one-shot-p nil)
  (installed-at 0))

(defvar *rt-signal-handlers* (make-hash-table))
(defvar *rt-sigaction-installed* (make-hash-table))
(defvar *rt-signal-names*
  `((,+rt-sigint+ . :sigint) (,+rt-sigsegv+ . :sigsegv)
    (,+rt-sigfpe+ . :sigfpe) (,+rt-sigpipe+ . :sigpipe)
    (,+rt-sigterm+ . :sigterm) (,+rt-sigprof+ . :sigprof)
    (,+rt-sigusr1+ . :sigusr1) (,+rt-sigusr2+ . :sigusr2)))

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

(defun rt-install-sigaction-stub (sig)
  "Record sigaction installation. SBCL/native backend plugs in the real trampoline later."
  #+sbcl (setf (gethash sig *rt-sigaction-installed*) :sbcl-stub)
  #-sbcl (setf (gethash sig *rt-sigaction-installed*) :portable-stub)
  t)

(defun rt-install-signal-handler (sig fn &key one-shot)
  (check-type sig integer)
  (check-type fn function)
  (rt-install-sigaction-stub sig)
  (let ((handler (make-rt-signal-handler :signal sig
                                         :function fn
                                         :one-shot-p one-shot
                                         :installed-at (get-internal-real-time))))
    (push handler (gethash sig *rt-signal-handlers* nil))
    handler))

(defun rt-remove-signal-handler (handler)
  (let ((sig (rt-signal-handler-signal handler)))
    (setf (gethash sig *rt-signal-handlers*)
          (remove handler (gethash sig *rt-signal-handlers* nil)))
    t))

(defun rt-dispatch-os-signal (sig)
  "Dispatch SIG through registered handlers, then signal mapped CL condition."
  (let ((handlers (copy-list (gethash sig *rt-signal-handlers*))))
    (dolist (handler handlers)
      (when (rt-signal-handler-function handler)
        (funcall (rt-signal-handler-function handler) sig))
      (when (rt-signal-handler-one-shot-p handler)
        (rt-remove-signal-handler handler)))
    (signal (rt-signal-to-condition sig))))

(defun rt-clear-signal-handlers (&optional sig)
  (if sig
      (remhash sig *rt-signal-handlers*)
      (clrhash *rt-signal-handlers*))
  t)

(defun rt-install-default-signal-handlers ()
  (rt-install-signal-handler +rt-sigint+
                             (lambda (sig)
                               (declare (ignore sig))
                               (break "Runtime SIGINT")))
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
