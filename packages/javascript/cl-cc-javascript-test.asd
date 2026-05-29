;;;; cl-cc-javascript-test.asd — test suite for the JavaScript frontend

(asdf:defsystem :cl-cc-javascript-test
  :description "Tests for the CL-CC JavaScript frontend"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-javascript :fiveam)
  :pathname "tests"
  :serial t
  :components
  ((:file "js-lexer-tests")
   (:file "js-parser-tests")
   (:file "js-e2e-tests")))
