;;;; cl-cc-compile.asd --- independent ASDF system for the compilation pipeline
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/compile package
;;;; (CPS transformation, codegen, builtin registry, pipeline entry points,
;;;; REPL state, our-eval/our-load).
;;;;
;;;; The package facade is loaded as a dependency of :cl-cc; the actual
;;;; compile source files remain in the umbrella :cl-cc system's component
;;;; tree because they reference VM instruction types defined there.
;;;; After the umbrella's defpackage sets up (use-package :cl-cc :cl-cc/compile),
;;;; the compile source files can access all VM symbols unqualified.

(asdf:defsystem :cl-cc-compile
  :description "Compilation pipeline: CPS, codegen, pipeline, REPL, our-eval"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
