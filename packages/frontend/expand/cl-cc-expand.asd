;;;; cl-cc-expand.asd --- independent ASDF system for the macro expansion subsystem
;;;;
;;;; Phase 2 leaf extraction. Files live in the :cl-cc/expand package
;;;; (macro environment, defmacro machinery, macroexpansion, lambda-list
;;;; parsing/destructuring, LOOP/DO/CASE/TYPECASE control-flow macros).
;;;;
;;;; The package facade is loaded as a dependency of :cl-cc; the actual
;;;; expand source files remain in the umbrella :cl-cc system's component
;;;; tree because they reference VM instruction types defined there.
;;;; After the umbrella's defpackage sets up (use-package :cl-cc :cl-cc/expand),
;;;; the expand source files can access all VM symbols unqualified.

(asdf:defsystem :cl-cc-expand
  :description "Macro expansion subsystem: macro-env, defmacro, macroexpand, lambda-list, LOOP"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
