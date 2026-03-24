;; Load and run tests
(load "../cl-cc.asd")
(asdf:load-system :cl-cc/test)
(uiop:symbol-call :cl-cc/test (quote run-tests))
(sb-ext:quit)
