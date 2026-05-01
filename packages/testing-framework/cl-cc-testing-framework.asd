;;;; cl-cc-testing-framework.asd
;;;; Extracted from tests/framework/ as part of the packages/ reorganization.
;;;; Canonical ASDF system for the CL-CC testing framework.

(asdf:defsystem :cl-cc-testing-framework
  :description "CL-CC testing framework — deftest, deftest-each, assert-*, fuzz, runner"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "package-imports-compile-parse")
   (:file "package-imports-core")
   (:file "package-imports-backend")
   (:file "package-exports")
   (:file "persistent")
   (:file "persistent-api")
   (:file "framework-conditions-state")
   (:file "framework-timeouts")
   (:file "framework-definitions")
   (:file "framework")
   (:file "framework-fixtures")
    (:file "framework-assertions")
   (:file "framework-discovery")
   (:file "framework-advanced")
   (:file "framework-compiler-run-string")
   (:file "framework-compiler")
   (:file "framework-pbt")
   (:file "framework-tap")
   (:file "framework-meta")
   (:file "framework-mutation")
   (:file "framework-fuzz")
   (:file "framework-parallel")
   (:file "framework-parallel-runner")
   (:file "framework-runner")))
