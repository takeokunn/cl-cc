;;;; cl-cc-emit.asd — independent ASDF system for the emit backend subsystem
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/emit package
;;;; (calling conventions, register allocation, x86-64/AArch64/WASM codegen).
;;;;
;;;; The package facade is loaded as a dependency of :cl-cc; the actual
;;;; emit source files remain in the umbrella :cl-cc system's component
;;;; tree because they reference VM instruction types defined there.
;;;; After the umbrella's defpackage sets up (use-package :cl-cc :cl-cc/emit),
;;;; the emit source files can access all VM symbols unqualified.

(asdf:defsystem :cl-cc-emit
  :description "Emit backend subsystem: calling conventions, regalloc, codegen"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src/emit/"
  :serial t
  :components
  ((:file "package")))
