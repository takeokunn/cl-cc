(in-package :cl-cc/test)

(defsuite dap-server-suite
  :description "DAP tools protocol tests"
  :parent cl-cc-unit-suite)

(in-suite dap-server-suite)

(defun dap-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(deftest dap-server-system-loads
  "The DAP source is loadable through :cl-cc-tools."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-tools nil))
  (assert-true (fboundp 'cl-cc/tools/dap:make-dap-server))
  (assert-true (fboundp 'cl-cc/tools/dap:read-jsonrpc-message)))

(deftest dap-jsonrpc-framing-round-trips
  "FR-797: DAP uses the same Content-Length protocol framing."
  :timeout 5
  (let* ((payload '(("seq" . 1) ("type" . "request") ("command" . "initialize")
                    ("arguments" . (("adapterID" . "cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/dap:write-jsonrpc-message out payload))))
    (let ((decoded (cl-cc/tools/dap:read-jsonrpc-message (make-string-input-stream wire))))
      (assert-equal "request" (dap-test-get decoded "type"))
      (assert-equal "initialize" (dap-test-get decoded "command")))))

(deftest dap-initialize-and-breakpoints
  "FR-797: initialize and setBreakpoints return DAP 1.51-style response bodies."
  :timeout 5
  (let* ((server (cl-cc/tools/dap:make-dap-server))
         (source '(("path" . "/tmp/a.lisp")))
         (breakpoints '((("line" . 10)) (("line" . 12))))
         (init (cl-cc/tools/dap:dap-handle-request
                 server '(("seq" . 1) ("type" . "request") ("command" . "initialize"))))
         (bp (cl-cc/tools/dap:dap-handle-request
              server `(("seq" . 2) ("type" . "request") ("command" . "setBreakpoints")
                       ("arguments" . (("source" . ,source)
                                      ("breakpoints" . ,breakpoints)))))))
    (assert-true (dap-test-get init "success"))
    (assert-true (dap-test-get (dap-test-get init "body") "supportsConfigurationDoneRequest"))
    (assert-equal 2 (length (dap-test-get (dap-test-get bp "body") "breakpoints")))
    (assert-true (every (lambda (breakpoint) (dap-test-get breakpoint "verified"))
                        (dap-test-get (dap-test-get bp "body") "breakpoints")))))

(deftest dap-stack-scopes-variables-and-step-fallbacks
  "FR-797: execution inspection works and unsupported step controls are explicit failures."
  :timeout 5
  (let ((server (cl-cc/tools/dap:make-dap-server)))
    (cl-cc/tools/dap:dap-handle-request
     server '(("seq" . 1) ("type" . "request") ("command" . "setBreakpoints")
              ("arguments" . (("source" . (("path" . "/tmp/a.lisp")))
                             ("breakpoints" . ((("line" . 7))))))))
    (let* ((stack (cl-cc/tools/dap:dap-handle-request
                   server '(("seq" . 2) ("type" . "request") ("command" . "stackTrace")
                            ("arguments" . (("threadId" . 1))))))
           (scopes (cl-cc/tools/dap:dap-handle-request
                    server '(("seq" . 3) ("type" . "request") ("command" . "scopes")
                             ("arguments" . (("frameId" . 1))))))
           (variables (cl-cc/tools/dap:dap-handle-request
                       server '(("seq" . 4) ("type" . "request") ("command" . "variables")
                                ("arguments" . (("variablesReference" . 1))))))
           (next (cl-cc/tools/dap:dap-handle-request
                  server '(("seq" . 5) ("type" . "request") ("command" . "next")))))
      (assert-= 1 (dap-test-get (dap-test-get stack "body") "totalFrames"))
      (assert-true (dap-test-get (dap-test-get scopes "body") "scopes"))
      (assert-true (dap-test-get (dap-test-get variables "body") "variables"))
      (assert-equal :false (dap-test-get next "success"))
      (assert-true (search "not supported" (dap-test-get next "message") :test #'char=)))))

(deftest dap-threads-evaluate-disconnect-and-sequencing
  "FR-761/FR-797: core DAP request sequencing covers threads, evaluate, and disconnect behavior."
  :timeout 5
  (let* ((server (cl-cc/tools/dap:make-dap-server :running-p t))
         (threads (cl-cc/tools/dap:dap-handle-request
                   server '( ("seq" . 10) ("type" . "request") ("command" . "threads"))))
         (eval-response (cl-cc/tools/dap:dap-handle-request
                         server '( ("seq" . 11) ("type" . "request") ("command" . "evaluate")
                                  ("arguments" . (("expression" . "(+ 20 22)"))))))
         (disconnect (cl-cc/tools/dap:dap-handle-request
                      server '( ("seq" . 12) ("type" . "request") ("command" . "disconnect"))))
         (thread-list (dap-test-get (dap-test-get threads "body") "threads")))
    (assert-equal 1 (dap-test-get threads "seq"))
    (assert-equal 10 (dap-test-get threads "request_seq"))
    (assert-equal 2 (dap-test-get eval-response "seq"))
    (assert-equal 3 (dap-test-get disconnect "seq"))
    (assert-equal "main" (dap-test-get (first thread-list) "name"))
    (assert-output-contains (dap-test-get (dap-test-get eval-response "body") "result") "42")
    (assert-false (cl-cc/tools/dap:dap-server-running-p server))))
