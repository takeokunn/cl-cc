;;;; cl-cc-vm-tests.asd — standalone test system for :cl-cc-vm
;;;;
;;;; Run with: (asdf:test-system :cl-cc-vm)

(asdf:defsystem :cl-cc-vm-tests
  :description "Tests for the CL-CC VM subsystem"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-test-framework)
  :pathname "packages/engine/vm/tests"
  :serial t
  :components
  ((:file "vm-instructions-tests")
   (:file "vm-execute-tests")
   (:file "vm-transcendental-tests")
   (:file "vm-numeric-tests")
   (:file "vm-extensions-tests")
   (:file "vm-bitwise-tests")
   (:file "vm-clos-tests")
   (:file "vm-clos-execute-tests")
   (:file "vm-run-tests")
   (:file "vm-dispatch-tests")
   (:file "vm-dispatch-gf-tests")
   (:file "vm-dispatch-gf-multi-tests")
   (:file "vm-bridge-tests")
   (:file "vm-opcodes-tests")
   (:file "vm-opcodes-defs-tests")
   (:file "vm-tests")
   (:file "package-tests")
   (:file "primitives-tests")
   (:file "conditions-tests")
   (:file "io-tests")
   (:file "format-tests")
   (:file "list-tests")
   (:file "list-coerce-tests")
   (:file "array-tests")
   (:file "strings-tests")
   (:file "symbols-tests")
   (:file "hash-tests")))
