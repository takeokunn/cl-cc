;;;; cl-cc-docgen.asd --- Deterministic Markdown API documentation generator

(asdf:defsystem :cl-cc-docgen
  :description "CL-CC Markdown API documentation generator"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "docgen")))

(asdf:defsystem :cl-cc-docgen/test
  :description "Tests for the CL-CC Markdown API documentation generator"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-docgen)
  :pathname "tests"
  :serial t
  :components
  ((:file "docgen-tests"))
  :perform (asdf:test-op (op system)
             (declare (ignore op system))
             (uiop:symbol-call :cl-cc/docgen-test '#:run-docgen-tests)))
