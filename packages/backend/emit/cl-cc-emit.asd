(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system :cl-cc-vm nil)
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (asdf:load-asd (merge-pathnames "../../engine/vm/cl-cc-vm.asd" here))
      (asdf:load-asd (merge-pathnames "../../foundation/mir/cl-cc-mir.asd" here))
      (asdf:load-asd (merge-pathnames "../../engine/optimize/cl-cc-optimize.asd" here)))))

(asdf:defsystem :cl-cc-emit
  :description "Emit backend subsystem: calling conventions, regalloc, codegen"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-mir :cl-cc-optimize)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "calling-convention")
   (:file "regalloc")
   (:file "regalloc-defs-uses")
   (:file "regalloc-allocate")
   (:file "x86-64")
   (:file "x86-64-encoding")
   (:file "x86-64-encoding-instrs")
   (:file "x86-64-sequences")
   (:file "x86-64-regs")
   (:file "x86-64-emit-ops")
   (:file "x86-64-emit-ops-bits")
   (:file "x86-64-emit-ops-logical")
   (:file "x86-64-codegen")
   (:file "x86-64-codegen-dispatch")
   (:file "aarch64")
   (:file "aarch64-codegen")
   (:file "aarch64-codegen-labels")
   (:file "aarch64-emitters")
   (:file "aarch64-program")
   (:file "wasm-types")
   (:file "wasm-ir")
   (:file "wasm-extract")
   (:file "wasm-trampoline")
   (:file "wasm-trampoline-emit")
   (:file "wasm-trampoline-build")
   (:file "wasm")
   (:file "wasm-emit")))
