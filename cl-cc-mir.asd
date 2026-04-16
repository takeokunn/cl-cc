;;;; cl-cc-mir.asd — independent ASDF system for Machine IR + target descriptors
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/mir package
;;;; (mir-value, mir-const, mir-inst, mir-block, mir-function, mir-module,
;;;; SSA variable tracking, RPO, dominators, printer, target-desc, and
;;;; predefined x86-64/aarch64/riscv64/wasm32 descriptors). Depends on
;;;; nothing — pure :cl leaf system, mirroring cl-cc-ir.

(asdf:defsystem :cl-cc-mir
  :description "Machine IR: SSA CFG, dominators, phi nodes, target descriptors"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src/mir"
  :serial t
  :components
  ((:file "package")
   (:file "mir")
   (:file "mir-builder")
   (:file "mir-analysis")
   (:file "target")))
