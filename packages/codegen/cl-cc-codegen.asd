(asdf:defsystem :cl-cc-codegen
  :description "Per-target code generation: x86-64, AArch64, WASM backends"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-vm :cl-cc-mir :cl-cc-target
               :cl-cc-optimize :cl-cc-regalloc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ;; x86-64 backend
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
   ;; AArch64 backend
   (:file "aarch64")
   (:file "aarch64-codegen")
   (:file "aarch64-codegen-labels")
   (:file "aarch64-emitters")
   (:file "aarch64-program")
   ;; WASM backend
   (:file "wasm-types")
   (:file "wasm-ir")
   (:file "wasm-extract")
   (:file "wasm-trampoline")
   (:file "wasm-trampoline-tables")
   (:file "wasm-trampoline-emit")
   (:file "wasm-trampoline-build")
   (:file "wasm")
   (:file "wasm-emit")))
