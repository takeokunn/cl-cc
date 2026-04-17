;;;; cl-cc-vm.asd -- independent ASDF system for the VM subsystem
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration.
;;;; All VM source files now live in :cl-cc/vm.
;;;; This system is independently loadable without :cl-cc.

(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system :cl-cc-bootstrap nil)
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (asdf:load-asd (merge-pathnames "../../foundation/bootstrap/cl-cc-bootstrap.asd" here)))))

(asdf:defsystem :cl-cc-vm
  :description "VM instruction set, executor, I/O, CLOS, conditions, collections"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap)
  :pathname "src"
  :serial t
  :components
  ((:file "facade-package")
   (:file "package")
   (:file "vm")
   (:file "vm-state-init")
   (:file "vm-bridge")
   (:file "vm-instructions")
   (:file "vm-dispatch")
   (:file "vm-dispatch-gf")
   (:file "vm-dispatch-gf-multi")
   (:file "vm-execute")
   (:file "vm-execute-mv")
   (:file "vm-clos")
   (:file "vm-clos-execute")
   (:file "vm-run")
   (:file "vm-opcodes")
   (:file "vm-opcodes-defs")
   (:file "vm-opcodes-run")
   (:file "primitives")
   (:file "primitives-typep")
   (:file "vm-bitwise")
   (:file "vm-transcendental")
   (:file "vm-numeric")
   (:file "vm-numeric-ext")
   (:file "vm-extensions")
   (:file "io")
   (:file "io-instructions")
   (:file "io-execute")
   (:file "io-runners")
   (:file "format")
   (:file "conditions")
   (:file "conditions-instructions")
   (:file "list-coerce")
   (:file "list")
   (:file "list-execute")
   (:file "array")
   (:file "array-bits")
   (:file "strings")
   (:file "symbols")
   (:file "hash")))

;;;; ---------------------------------------------------------------------
;;;; Tests — run with (asdf:test-system :cl-cc-vm) or :cl-cc-vm/tests.
(asdf:defsystem :cl-cc-vm/tests
  :description "Tests for the CL-CC VM subsystem"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc/tests-framework)
  :pathname "tests"
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
   (:file "hash-tests"))
  ;; NOTE: run-tests invokes the canonical cl-cc-suite (unit + integration + e2e),
  ;; not a VM-subset. There is intentionally one test entry point; suite filtering
  ;; is done via keyword args to run-tests, not via per-package ASDF systems.
  :perform (asdf:test-op (o c)
             (declare (ignore o c))
             (uiop:symbol-call :cl-cc/test '#:run-tests)))

;; Wire asdf:test-system :cl-cc-vm to dispatch to the /tests subsystem.
(defmethod asdf:perform ((op asdf:test-op) (c (eql (asdf:find-system :cl-cc-vm))))
  (asdf:load-system :cl-cc-vm/tests)
  (asdf:perform op (asdf:find-system :cl-cc-vm/tests)))
