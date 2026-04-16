;;;; cl-cc-test-framework.asd — standalone ASDF system for the CL-CC test framework
;;;;
;;;; Extracted from :cl-cc/test so that per-package test systems can depend on
;;;; the framework independently without pulling in the full test suite.

(asdf:defsystem :cl-cc-test-framework
  :description "CL-CC Test Framework — TAP output, assertion macros, test runner"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :pathname "tests/framework"
  :serial t
  :components
  ((:file "package")
   (:file "framework")
   (:file "framework-advanced")
   (:file "framework-compiler")
   (:file "framework-meta")
   (:file "framework-fuzz")
   (:file "framework-runner")))
