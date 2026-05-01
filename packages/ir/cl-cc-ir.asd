;;;; cl-cc-ir.asd — independent ASDF system for the compile-level SSA IR
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/ir package
;;;; (ir-value, ir-inst, ir-block, ir-function, ir-module, CFG edges,
;;;; RPO, dominators, SSA variable tracking, printer). Depends on
;;;; nothing — pure :cl leaf system, mirroring cl-cc-binary / cl-cc-bytecode.

(asdf:defsystem :cl-cc-ir
  :description "Compile-level SSA IR: basic blocks, CFG, dominators, phi nodes, printer"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "types")
   (:file "block")
   (:file "ssa")
   (:file "printer")))
