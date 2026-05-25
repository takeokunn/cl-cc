;;;; dap-server.lisp -- Debug Adapter Protocol stdio server (FR-761).

(cl:in-package :cl-cc/tools/dap)

(defvar *dap-enabled* nil
  "When non-NIL, the DAP server accepts Debug Adapter Protocol requests.")

(defstruct (dap-server (:constructor make-dap-server (&key)))
  (seq 1 :type integer)
  (breakpoints (make-hash-table :test #'equal) :type hash-table)
  (variables (make-hash-table :test #'eql) :type hash-table)
  (next-variable-reference 2 :type integer)
  (vm-state nil)
  (running-p nil))

(defun read-jsonrpc-message (&optional (stream *standard-input*))
  (cl-cc/tools/lsp:read-jsonrpc-message stream))

(defun write-jsonrpc-message (stream payload)
  (cl-cc/tools/lsp:write-jsonrpc-message stream payload))

(defun %assoc= (key alist)
  (cdr (assoc key alist :test #'string=)))

(defun %dap-response (server request &key (success :true) body message)
  (let ((seq (prog1 (dap-server-seq server) (incf (dap-server-seq server)))))
    `(("seq" . ,seq)
      ("type" . "response")
      ("request_seq" . ,(or (%assoc= "seq" request) 0))
      ("success" . ,success)
      ("command" . ,(or (%assoc= "command" request) ""))
      ,@(when message `(("message" . ,message)))
      ("body" . ,body))))

(defun %source-path (arguments)
  (%assoc= "path" (%assoc= "source" arguments)))

(defun %set-breakpoints (server request)
  (let* ((arguments (%assoc= "arguments" request))
         (path (or (%source-path arguments) "<unknown>"))
         (requested (or (%assoc= "breakpoints" arguments) nil))
         (breakpoints (loop for bp in requested
                            for id from 1
                            for line = (or (%assoc= "line" bp) 1)
                            collect `(("id" . ,id)
                                      ("verified" . :true)
                                      ("line" . ,line)
                                      ("source" . (("path" . ,path)))))))
    (setf (gethash path (dap-server-breakpoints server)) breakpoints)
    (%dap-response server request :body `(("breakpoints" . ,breakpoints)))))

(defun %stack-frames (server)
  (let ((state (dap-server-vm-state server)))
    (cond
      ((and state (fboundp 'cl-cc/vm:vm-call-stack))
       (let ((frames nil))
         (loop for frame in (ignore-errors (cl-cc/vm:vm-call-stack state))
               for id from 1 do
                 (push `(("id" . ,id)
                         ("name" . ,(format nil "frame-~D" id))
                         ("line" . 1)
                         ("column" . 1)
                         ("source" . (("path" . "<vm>"))))
                       frames))
         (or (nreverse frames)
             '(( ("id" . 1) ("name" . "<toplevel>") ("line" . 1) ("column" . 1)
                 ("source" . (("path" . "<repl>"))))))))
      (t '(( ("id" . 1) ("name" . "<toplevel>") ("line" . 1) ("column" . 1)
             ("source" . (("path" . "<repl>")))))))))

(defun %default-variables (server)
  (let ((vars '(( ("name" . "result") ("value" . "<unavailable>") ("type" . "object") ("variablesReference" . 0)))))
    (setf (gethash 1 (dap-server-variables server)) vars)
    vars))

(defun %variables-from-vm-state (server)
  (or (gethash 1 (dap-server-variables server))
      (let ((state (dap-server-vm-state server))
            (vars nil))
        (when state
          (let ((globals (ignore-errors (cl-cc/vm:vm-global-vars state))))
            (when globals
              (maphash (lambda (name value)
                         (push `(("name" . ,(princ-to-string name))
                                 ("value" . ,(prin1-to-string value))
                                 ("type" . ,(string-downcase (symbol-name (type-of value))))
                                 ("variablesReference" . 0))
                               vars))
                       globals))))
        (setf (gethash 1 (dap-server-variables server)) (or (nreverse vars) (%default-variables server))))))

(defun dap-handle-request (server request)
  "Handle one DAP request and return a protocol response."
  (let ((command (%assoc= "command" request)))
    (cond
      ((string= command "initialize")
       (%dap-response server request
                      :body '(("supportsConfigurationDoneRequest" . :true)
                              ("supportsEvaluateForHovers" . :true)
                              ("supportsSetVariable" . :false))))
      ((string= command "launch")
       (%dap-response server request :body '()))
      ((string= command "setBreakpoints")
       (%set-breakpoints server request))
      ((string= command "configurationDone")
       (%dap-response server request :body '()))
      ((string= command "threads")
       (%dap-response server request :body '(("threads" . ((("id" . 1) ("name" . "main")))))))
      ((string= command "stackTrace")
       (let ((frames (%stack-frames server)))
         (%dap-response server request :body `(("stackFrames" . ,frames)
                                               ("totalFrames" . ,(length frames))))))
      ((string= command "scopes")
       (%dap-response server request
                      :body '(("scopes" . ((("name" . "Locals")
                                             ("variablesReference" . 1)
                                             ("expensive" . :false)))))))
      ((string= command "variables")
       (let* ((arguments (%assoc= "arguments" request))
              (ref (or (%assoc= "variablesReference" arguments) 1)))
         (%dap-response server request
                        :body `(("variables" . ,(or (gethash ref (dap-server-variables server))
                                                     (%variables-from-vm-state server)))))))
      ((string= command "continue")
       (%dap-response server request :body '(("allThreadsContinued" . :true))))
      ((member command '("next" "stepIn") :test #'string=)
       (%dap-response server request :success :false :message "stepping is not supported by the current VM adapter" :body '()))
      ((string= command "evaluate")
       (let* ((arguments (%assoc= "arguments" request))
              (expr (or (%assoc= "expression" arguments) "")))
         (%dap-response server request
                        :body `(("result" . ,(handler-case (prin1-to-string (cl-cc:run-string-repl expr))
                                              (error (e) (format nil "Error: ~A" e))))
                                ("variablesReference" . 0)))))
      ((string= command "disconnect")
       (setf (dap-server-running-p server) nil)
       (%dap-response server request :body '()))
      (t (%dap-response server request :success :false :message "command not supported" :body '())))))

(defun dap-start (&key (port 0) (input *standard-input*) (output *standard-output*))
  "Start the DAP server. PORT=0 selects stdio mode."
  (declare (ignore port))
  (let ((server (make-dap-server :running-p t)))
    (loop while (dap-server-running-p server) do
      (let* ((request (read-jsonrpc-message input))
             (response (dap-handle-request server request)))
        (when response (write-jsonrpc-message output response))))
    server))

(defun dap-stop ()
  "Stop the DAP server (stdio mode exits when it receives disconnect)."
  (setf *dap-enabled* nil)
  t)
