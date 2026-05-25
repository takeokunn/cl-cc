(in-package :cl-cc/vm)

;;; FR-1085 — VM debugger surface: trace/step/break and inspection helpers.

(defparameter *trace-output* *standard-output*
  "Stream used by VM trace output.")

(defvar *vm-traced-functions* (make-hash-table :test #'equal)
  "Function names currently traced by the VM debugger.")

(defvar *vm-trace-depth* 0
  "Dynamic nesting depth for trace indentation.")

(defvar *vm-step-mode* nil
  "When true, VM stepping prompts before executing forms/instructions that opt in.")

(defvar *dribble-stream* nil
  "Current dribble transcript stream, or NIL.")

(defun %trace-key (function-name)
  (etypecase function-name
    (symbol function-name)
    (string (string-upcase function-name))))

(defun vm-traced-p (function-name)
  (gethash (%trace-key function-name) *vm-traced-functions*))

(defun vm-trace (&rest function-names)
  "Enable VM tracing for FUNCTION-NAMES and return the traced set."
  (dolist (name function-names)
    (setf (gethash (%trace-key name) *vm-traced-functions*) t))
  (loop for name being the hash-keys of *vm-traced-functions* collect name))

(defmacro trace (&rest function-names)
  "Enable tracing for FUNCTION-NAMES without evaluating the names."
  `(vm-trace ,@(mapcar (lambda (name) `',name) function-names)))

(defun vm-untrace (&rest function-names)
  "Disable VM tracing.  With no arguments, clear all traces."
  (if function-names
      (dolist (name function-names)
        (remhash (%trace-key name) *vm-traced-functions*))
      (clrhash *vm-traced-functions*))
  (loop for name being the hash-keys of *vm-traced-functions* collect name))

(defmacro untrace (&rest function-names)
  "Disable tracing.  With no names, clear all traces."
  `(vm-untrace ,@(mapcar (lambda (name) `',name) function-names)))

(defun vm-format-trace-call (function-name args)
  (format *trace-output* "~&~V@T~D: (~A~{ ~S~})~%"
          (* 2 *vm-trace-depth*) *vm-trace-depth* function-name args))

(defun vm-format-trace-return (function-name values)
  (format *trace-output* "~&~V@T~D: ~A returned~{ ~S~}~%"
          (* 2 *vm-trace-depth*) *vm-trace-depth* function-name values))

(defmacro vm-with-trace ((function-name args-form) &body body)
  "Execute BODY with trace logging when FUNCTION-NAME is traced."
  (let ((name (gensym "TRACE-NAME"))
        (args (gensym "TRACE-ARGS"))
        (values (gensym "TRACE-VALUES")))
    `(let ((,name ,function-name)
           (,args ,args-form))
       (if (vm-traced-p ,name)
           (progn
             (vm-format-trace-call ,name ,args)
             (let ((*vm-trace-depth* (1+ *vm-trace-depth*)))
               (let ((,values (multiple-value-list (progn ,@body))))
                 (vm-format-trace-return ,name ,values)
                 (values-list ,values))))
           (progn ,@body)))))

(defun vm-step (form &optional env)
  "Evaluate FORM with VM step mode enabled."
  (declare (ignore env))
  (let ((*vm-step-mode* t))
    (eval form)))

(defmacro step (form)
  "Evaluate FORM with VM step mode enabled."
  `(let ((*vm-step-mode* t)) ,form))

(defun vm-step-prompt (&optional (thing nil thing-p))
  "Prompt the user when *VM-STEP-MODE* is active."
  (when *vm-step-mode*
    (format *query-io* "~&Step~@[ ~S~]. Press Return to continue, q to quit: "
            (and thing-p thing))
    (finish-output *query-io*)
    (let ((line (read-line *query-io* nil "")))
      (when (member line '("q" "Q" ":q") :test #'string=)
        (error "Step aborted"))))
  (values))

(defun vm-invoke-debugger (condition &optional vm-state labels)
  "Invoke the debugger for CONDITION, honoring *DEBUGGER-HOOK* and restarts."
  (when vm-state
    (ignore-errors (vm-print-backtrace vm-state :labels labels)))
  (if *debugger-hook*
      (funcall *debugger-hook* condition nil)
      (let ((restart (vm-show-restart-menu condition)))
        (if restart
            (vm-invoke-restart-interactively restart)
            (cl:invoke-debugger condition)))))

(defun invoke-debugger (condition)
  "Enter the VM debugger for CONDITION."
  (vm-invoke-debugger condition))

(defun vm-break (&optional (format-control "Break") &rest args)
  "Enter the VM debugger with a simple break condition."
  (vm-invoke-debugger
   (make-vm-simple-error nil format-control args)))

(defun break (&optional (format-control "Break") &rest args)
  "Enter the VM debugger."
  (apply #'vm-break format-control args))

(defun vm-apropos-list (string &optional package)
  (if package (apropos-list string package) (apropos-list string)))

(defun vm-apropos (string &optional package)
  (if package (apropos string package) (apropos string)))

(defun vm-describe (object &optional (stream *standard-output*))
  "Describe OBJECT to STREAM and return no values."
  (format stream "~&~S is a ~S~%" object (type-of object))
  (when (symbolp object)
    (format stream "  package: ~A~%" (symbol-package object))
    (format stream "  boundp: ~A~%" (boundp object))
    (format stream "  fboundp: ~A~%" (fboundp object)))
  (values))

(defun describe (object &optional (stream *standard-output*))
  "Describe OBJECT to STREAM."
  (vm-describe object stream))

(defun vm-inspect (object)
  "Minimal interactive inspector for OBJECT."
  (vm-describe object *query-io*)
  object)

(defun inspect (object)
  "Inspect OBJECT using the VM inspector."
  (vm-inspect object))

(defun vm-room (&optional detail)
  "Print VM/host memory usage summary."
  (room detail))

(defun vm-ed (&optional x)
  "Editor invocation stub."
  (format *standard-output* "~&ED is not integrated in this VM build~@[ for ~S~].~%" x)
  nil)

(defun ed (&optional x)
  "Editor invocation stub."
  (vm-ed x))

(defun vm-dribble (&optional pathname)
  "Start or stop a simple session log."
  (cond (pathname
         (when *dribble-stream* (close *dribble-stream*))
         (setf *dribble-stream* (open pathname :direction :output :if-exists :supersede :if-does-not-exist :create))
         pathname)
        (*dribble-stream*
         (close *dribble-stream*)
         (setf *dribble-stream* nil)
         nil)
        (t nil)))

(defun dribble (&optional pathname)
  "Start or stop a VM dribble transcript."
  (vm-dribble pathname))

(eval-when (:load-toplevel :execute)
  (vm-register-host-bridge 'trace #'vm-trace)
  (vm-register-host-bridge 'untrace #'vm-untrace)
  (vm-register-host-bridge 'break #'vm-break)
  (vm-register-host-bridge 'apropos #'vm-apropos)
  (vm-register-host-bridge 'apropos-list #'vm-apropos-list)
  (vm-register-host-bridge 'describe #'vm-describe)
  (vm-register-host-bridge 'inspect #'vm-inspect)
  (vm-register-host-bridge 'room #'vm-room)
  (vm-register-host-bridge 'ed #'vm-ed)
  (vm-register-host-bridge 'dribble #'vm-dribble))
