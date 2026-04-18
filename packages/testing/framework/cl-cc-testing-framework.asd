;;;; cl-cc-testing-framework.asd
;;;; Extracted from tests/framework/ as part of the packages/ reorganization.
;;;; System name :cl-cc/tests-framework matches existing references
;;;; (e.g. packages/engine/vm/cl-cc-vm.asd test-op :depends-on).

(asdf:defsystem :cl-cc/tests-framework
  :description "CL-CC testing framework — deftest, deftest-each, assert-*, fuzz, runner"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "persistent")
   (:file "framework")
   (:file "framework-fixtures")
   (:file "framework-assertions")
   (:file "framework-discovery")
   (:file "framework-advanced")
   (:file "framework-compiler")
   (:file "framework-meta")
   (:file "framework-fuzz")
   (:file "framework-runner")))
