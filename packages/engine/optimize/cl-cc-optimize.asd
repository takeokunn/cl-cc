;;;; cl-cc-optimize.asd — independent ASDF system for the optimizer subsystem
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/optimize package
;;;; (CFG, SSA, E-graph, optimizer passes, pipeline).
;;;;
;;;; The package facade is loaded as a dependency of :cl-cc; the actual
;;;; optimizer source files remain in the umbrella :cl-cc system's component
;;;; tree because they reference VM instruction types defined there.
;;;; After the umbrella's defpackage sets up (use-package :cl-cc :cl-cc/optimize),
;;;; the optimize source files can access all VM symbols unqualified.

(asdf:defsystem :cl-cc-optimize
  :description "Optimizer subsystem: CFG, SSA, E-graph, peephole, pipeline"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
