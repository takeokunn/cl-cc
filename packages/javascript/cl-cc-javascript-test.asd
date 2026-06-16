;;;; cl-cc-javascript-test.asd — test suite for the JavaScript frontend

(asdf:defsystem :cl-cc-javascript-test
  :description "Tests for the CL-CC JavaScript frontend"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc-testing-framework :cl-cc-javascript)
  :pathname "tests"
  :serial t
  :components
  ((:file "js-lexer-tests")
   (:file "js-parser-decl-tests")
   (:file "js-parser-stmt-tests")
   (:file "js-e2e-core-tests")
   (:file "js-e2e-ast-tests")
   (:file "js-e2e-advanced-tests")
   (:file "js-e2e-modern-tests")
   (:file "js-runtime-core-tests")
   (:file "js-runtime-array-tests")
   (:file "js-runtime-string-number-tests")
   (:file "js-runtime-collections-tests")
   (:file "js-runtime-resolver-tests")
   (:file "js-runtime-date-json-tests")
   (:file "js-runtime-object-ops-tests")
   (:file "js-runtime-symbol-tests")
   (:file "js-runtime-typed-array-methods-tests")
   (:file "js-runtime-misc-tests")))
