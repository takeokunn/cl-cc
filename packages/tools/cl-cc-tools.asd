;;;; cl-cc-tools.asd -- ASDF system for editor/debugger protocol tools.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :cl-cc/tools
    (:use :cl))
  (defpackage :cl-cc/tools/lsp
    (:use :cl)
    (:export #:make-lsp-server
             #:read-jsonrpc-message
             #:write-jsonrpc-message
             #:lsp-handle-request
             #:lsp-open-document
             #:lsp-publish-diagnostics
             #:lsp-start
             #:lsp-stop))
  (defpackage :cl-cc/tools/dap
    (:use :cl)
    (:export #:make-dap-server
             #:read-jsonrpc-message
             #:write-jsonrpc-message
             #:dap-handle-request
             #:dap-start
             #:dap-stop)))

(asdf:defsystem :cl-cc-tools
  :description "CL-CC protocol tools (LSP and DAP)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :pathname "src"
  :serial t
  :components
  ((:file "lsp-server")
   (:file "dap-server")))

(asdf:defsystem :cl-cc-tools/tests
  :description "Tests for CL-CC protocol tools"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-tools :cl-cc-testing-framework)
  :pathname "tests"
  :serial t
  :components
  ((:file "lsp-server-tests")
   (:file "dap-server-tests"))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :cl-cc/test '#:run-tests)))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system :cl-cc-tools))))
  (asdf:load-system :cl-cc-tools/tests)
  (asdf:perform op (asdf:find-system :cl-cc-tools/tests)))
