;;;; cl-cc-debug.asd -- Swank/debugging skeletons for CL-CC.

(asdf:defsystem :cl-cc-debug
  :description "Debugging support: minimal Swank protocol, object inspector, step debugger"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-vm)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "inspector")
   (:file "swank")))

(asdf:defsystem :cl-cc-debug/tests
  :description "Tests for the CL-CC debugging subsystem"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-debug :cl-cc-testing-framework)
  :pathname "tests"
  :serial t
  :components
  ((:file "debug-tests"))
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :cl-cc/test '#:run-tests)))

(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system :cl-cc-debug))))
  (asdf:load-system :cl-cc-debug/tests)
  (asdf:perform op (asdf:find-system :cl-cc-debug/tests)))
