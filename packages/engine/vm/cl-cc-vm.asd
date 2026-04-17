;;;; cl-cc-vm.asd -- independent ASDF system for the VM subsystem
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration.
;;;; All VM source files now live in :cl-cc/vm.
;;;; This system is independently loadable without :cl-cc.

(eval-when (:load-toplevel :execute)
  (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                             :name nil :type nil)))
    (asdf:load-asd (merge-pathnames "../../foundation/bootstrap/cl-cc-bootstrap.asd" here))))

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
