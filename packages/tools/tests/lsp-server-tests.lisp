(in-package :cl-cc/test)

(defsuite lsp-server-suite
  :description "LSP tools protocol tests"
  :parent cl-cc-unit-suite)

(in-suite lsp-server-suite)

(defun lsp-test-get (alist key)
  (cdr (assoc key alist :test #'string=)))

(deftest lsp-server-system-loads
  "The LSP system is loadable through :cl-cc-tools."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-tools nil))
  (assert-true (fboundp 'cl-cc/tools/lsp:make-lsp-server))
  (assert-true (fboundp 'cl-cc/tools/lsp:read-jsonrpc-message)))

(deftest lsp-jsonrpc-framing-round-trips
  "FR-796: Content-Length framing and JSON parsing work without network I/O."
  :timeout 5
  (let* ((payload '(("jsonrpc" . "2.0") ("id" . 7) ("method" . "initialize")
                    ("params" . (("rootUri" . "file:///tmp/cl-cc")))))
         (wire (with-output-to-string (out)
                 (cl-cc/tools/lsp:write-jsonrpc-message out payload))))
    (assert-true (search "Content-Length:" wire :test #'char=))
    (let ((decoded (cl-cc/tools/lsp:read-jsonrpc-message (make-string-input-stream wire))))
      (assert-equal "initialize" (lsp-test-get decoded "method"))
      (assert-equal 7 (lsp-test-get decoded "id")))))

(deftest lsp-initialize-advertises-capabilities
  "FR-796: initialize returns LSP capabilities."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (response (cl-cc/tools/lsp:lsp-handle-request
                    server '(("jsonrpc" . "2.0") ("id" . 1) ("method" . "initialize")))))
    (assert-equal "2.0" (lsp-test-get response "jsonrpc"))
    (assert-true (lsp-test-get (lsp-test-get response "result") "capabilities"))))

(deftest lsp-hover-completion-definition-and-symbols
  "FR-796: hover, completion, definition, and workspace/symbol are protocol-correct."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///sample.lisp")
         (text "(defun add-one (x)\n  (+ x 1))\n\n(add-one 41)"))
    (cl-cc/tools/lsp:lsp-open-document server uri text)
    (let* ((position '(("line" . 3) ("character" . 3)))
           (doc `(("uri" . ,uri)))
           (definition (cl-cc/tools/lsp:lsp-handle-request
                        server `(("id" . 2) ("method" . "textDocument/definition")
                                 ("params" . (("textDocument" . ,doc) ("position" . ,position))))))
           (hover (cl-cc/tools/lsp:lsp-handle-request
                   server `(("id" . 3) ("method" . "textDocument/hover")
                            ("params" . (("textDocument" . ,doc) ("position" . ,position))))))
           (completion (cl-cc/tools/lsp:lsp-handle-request
                        server `(("id" . 4) ("method" . "textDocument/completion")
                                 ("params" . (("textDocument" . ,doc)
                                              ("position" . (("line" . 0) ("character" . 4))))))))
           (symbols (cl-cc/tools/lsp:lsp-handle-request
                     server '(("id" . 5) ("method" . "workspace/symbol")
                              ("params" . (("query" . "add")))))))
      (assert-equal 0 (lsp-test-get (lsp-test-get (lsp-test-get (lsp-test-get definition "result") "range") "start") "line"))
      (assert-true (search "add-one" (lsp-test-get (lsp-test-get (lsp-test-get hover "result") "contents") "value") :test #'char=))
      (assert-true (find "defun" (lsp-test-get (lsp-test-get completion "result") "items")
                         :key (lambda (item) (lsp-test-get item "label")) :test #'string=))
      (assert-true (find "add-one" (lsp-test-get symbols "result")
                         :key (lambda (item) (lsp-test-get item "name")) :test #'string=)))))

(deftest lsp-publishes-parenthesis-diagnostics
  "FR-796: diagnostics are emitted as textDocument/publishDiagnostics notifications."
  :timeout 5
  (let* ((server (cl-cc/tools/lsp:make-lsp-server))
         (uri "file:///broken.lisp"))
    (cl-cc/tools/lsp:lsp-open-document server uri "(defun broken (x) (+ x 1)")
    (let* ((notification (cl-cc/tools/lsp:lsp-publish-diagnostics server uri))
           (params (lsp-test-get notification "params")))
      (assert-equal "textDocument/publishDiagnostics" (lsp-test-get notification "method"))
      (assert-true (lsp-test-get params "diagnostics")))))
