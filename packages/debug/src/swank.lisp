(in-package :cl-cc/debug)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets)
  (require :sb-posix))

(defstruct (swank-server
            (:constructor %make-swank-server (&key host port socket thread running-p)))
  "Minimal Swank-like server handle for FR-687."
  host
  port
  socket
  thread
  running-p)

(defun %read-all-forms (string)
  "Read all forms from STRING."
  (with-input-from-string (stream string)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun interactive-eval (string &key (package *package*))
  "Evaluate STRING in PACKAGE and return a plist describing all values.

This is a deliberately small Swank skeleton operation, not a full Swank RPC
implementation.  The evaluation uses the host CL reader/evaluator so debugger
clients can smoke-test protocol round trips before CL-CC-specific evaluation is
plugged in."
  (let ((*package* (or (and (packagep package) package)
                       (and (or (stringp package) (symbolp package))
                            (find-package package))
                       *package*)))
    (handler-case
        (let ((values nil))
          (dolist (form (%read-all-forms string))
            (setf values (multiple-value-list (eval form))))
          (list :ok t
                :values values
                :printed-values (mapcar #'%safe-princ-to-string values)))
      (condition (condition)
        (list :ok nil :condition condition :message (%safe-princ-to-string condition))))))

(defun compile-string (string &key (package *package*) name)
  "Compile STRING as a zero-argument thunk and return a result plist."
  (let ((*package* (or (and (packagep package) package)
                       (and (or (stringp package) (symbolp package))
                            (find-package package))
                       *package*))
        (warnings nil))
    (handler-bind ((warning (lambda (warning) (push warning warnings))))
      (handler-case
          (let* ((forms (%read-all-forms string))
                 (lambda-form `(lambda () ,@forms)))
            (multiple-value-bind (function warnings-p failure-p)
                (compile name lambda-form)
              (list :ok (not failure-p)
                    :function function
                    :warnings-p warnings-p
                    :failure-p failure-p
                    :warnings (nreverse warnings))))
        (condition (condition)
          (list :ok nil :condition condition :message (%safe-princ-to-string condition)))))))

(defun completions (prefix &key (package *package*) external-only)
  "Return symbol-name completions for PREFIX in PACKAGE."
  (let* ((package (or (and (packagep package) package)
                      (and (or (stringp package) (symbolp package))
                           (find-package package))
                      *package*))
         (prefix (string-upcase prefix))
         (matches nil))
    (flet ((maybe-add (symbol)
             (let ((name (symbol-name symbol)))
               (when (and (<= (length prefix) (length name))
                          (string= prefix name :end2 (length prefix)))
                 (pushnew name matches :test #'string=)))))
      (if external-only
          (do-external-symbols (symbol package) (maybe-add symbol))
          (do-symbols (symbol package) (maybe-add symbol))))
    (sort matches #'string<)))

(defun operator-arglist (operator &key (package *package*))
  "Return a best-effort lambda list for OPERATOR."
  (let* ((symbol (etypecase operator
                   (symbol operator)
                   (string (or (find-symbol (string-upcase operator)
                                             (or (and (packagep package) package)
                                                 (and (or (stringp package) (symbolp package))
                                                      (find-package package))
                                                 *package*))
                               (intern (string-upcase operator) package)))))
         (callable (or (macro-function symbol)
                       (and (fboundp symbol) (symbol-function symbol)))))
    (when callable
      (multiple-value-bind (lambda-expression closure-p name)
          (function-lambda-expression callable)
        (declare (ignore closure-p name))
        (when (and (consp lambda-expression)
                   (eq (first lambda-expression) 'lambda))
          (second lambda-expression))))))

(defun %swank-dispatch (request)
  "Dispatch one skeleton Swank request form."
  (destructuring-bind (operation &rest args) request
    (case operation
      ((:interactive-eval interactive-eval) (apply #'interactive-eval args))
      ((:compile-string compile-string) (apply #'compile-string args))
      ((:completions completions) (apply #'completions args))
      ((:operator-arglist operator-arglist) (apply #'operator-arglist args))
      (otherwise (list :ok nil :message (format nil "Unknown Swank operation: ~S" operation))))))

#+sbcl
(defun %accept-one-swank-client (socket)
  "Serve one tiny line-oriented Swank skeleton client."
  (multiple-value-bind (client _peer)
      (sb-bsd-sockets:socket-accept socket)
    (declare (ignore _peer))
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream
                        client :input t :output t :element-type 'character
                        :buffering :line :external-format :utf-8)))
           (loop for line = (read-line stream nil nil)
                 while line
                 do (let ((response (handler-case
                                        (%swank-dispatch (read-from-string line))
                                      (condition (condition)
                                        (list :ok nil :message (%safe-princ-to-string condition))))))
                      (write response :stream stream :escape t)
                      (terpri stream)
                      (finish-output stream))))
      (ignore-errors (sb-bsd-sockets:socket-close client)))))

#+sbcl
(defun %swank-accept-loop (server)
  "Accept clients while SERVER is marked running."
  (loop while (swank-server-running-p server)
        do (handler-case
               (%accept-one-swank-client (swank-server-socket server))
             (condition (condition)
               (unless (swank-server-running-p server)
                 (return))
               (format *error-output* "~&Swank skeleton client error: ~A~%" condition)))))

(defun create-server (&key (host "127.0.0.1") (port 4005) background)
  "Create a minimal Swank skeleton socket server.

When BACKGROUND is true on SBCL, an accept loop is started in a background
thread.  The wire format is intentionally tiny: one printed request form per
line, returning one printed plist per line."
  #+sbcl
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (address (sb-bsd-sockets:make-inet-address host)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket address port)
    (sb-bsd-sockets:socket-listen socket 5)
    (let ((server (%make-swank-server :host host :port port :socket socket :running-p t)))
      (when background
        (setf (swank-server-thread server)
              (sb-thread:make-thread (lambda () (%swank-accept-loop server))
                                     :name "cl-cc swank skeleton")))
      server))
  #-sbcl
  (declare (ignore host port background))
  #-sbcl
  (error "CREATE-SERVER currently requires SBCL socket support."))

(defun stop-server (server)
  "Stop SERVER and close its listening socket."
  (setf (swank-server-running-p server) nil)
  #+sbcl
  (when (swank-server-socket server)
    (ignore-errors (sb-bsd-sockets:socket-close (swank-server-socket server))))
  server)
