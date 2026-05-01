;;;; cl-cc-bytecode.asd — independent ASDF system for bytecode ISA v2
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/bytecode package — 32-bit instruction encoding + disassembly.
;;;; Leaf system: no dependencies on other cl-cc systems.

(asdf:defsystem :cl-cc-bytecode
  :description "cl-cc bytecode ISA v2 — 32-bit instruction encoding, builder, decoder"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "encode")
   (:file "encode-ops")
   (:file "encode-ops-objects")
   (:file "decode")
   (:file "decode-disasm")))
