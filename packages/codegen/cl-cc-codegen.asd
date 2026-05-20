(asdf:defsystem :cl-cc-codegen
  :description "Per-target code generation: x86-64, AArch64, WASM backends"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-vm :cl-cc-mir :cl-cc-target
               :cl-cc-binary
               :cl-cc-optimize :cl-cc-regalloc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "calling-convention")
   ;; x86-64 backend
   (:file "x86-64")
   (:file "x86-64-encoding")
   (:file "x86-64-encoding-instrs")
    (:file "x86-64-sequences")
    (:file "x86-64-regs")
    (:file "post-ra-scheduler")
    (:file "x86-64-emit-ops")
   (:file "x86-64-emit-ops-bits")
   (:file "x86-64-emit-ops-logical")
      (:file "x86-64-codegen-helpers")
      (:file "x86-64-codegen-data")
       (:file "x86-64-codegen-core")
      (:file "x86-64-codegen-emitters")
     (:file "x86-64-codegen-dispatch")
     (:file "x86-64-peephole")
     ;; isel rules
     (:file "isel/isel-core")
     (:file "isel/x86-64-rules")
     (:file "isel/aarch64-rules")
    ;; AArch64 backend
   (:file "aarch64")
   (:file "aarch64-codegen")
    (:file "aarch64-codegen-labels")
    (:file "aarch64-emitters")
    (:file "aarch64-program")
    ;; RISC-V backend
    (:file "riscv64-codegen")
    ;; WASM backend
   (:file "wasm-types")
   (:file "wasm-ir")
   (:file "wasm-extract")
   (:file "wasm-trampoline")
   (:file "wasm-trampoline-tables")
   (:file "wasm-trampoline-emit")
   (:file "wasm-trampoline-build")
     (:file "wasm")
     (:file "wasm-emit-data")
    (:file "wasm-emit-sections")
    (:file "wasm-emit-instrs")))
