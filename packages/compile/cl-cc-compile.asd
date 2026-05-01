;;;; cl-cc-compile.asd --- independent ASDF system for the compilation engine
;;;;
;;;; Phase 3d extraction. Compile source files (context, closure, CPS,
;;;; codegen, builtin registry) are now part of this system.
;;;; Pipeline files (stdlib-source, pipeline, pipeline-repl etc.) remain
;;;; in cl-cc.asd because they use (in-package :cl-cc) which requires
;;;; the umbrella package to exist first.
;;;;
;;;; Files use (in-package :cl-cc/compile) and access VM/expand/ast symbols
;;;; via the :use list below.
;;;;
;;;; Dependency order: bootstrap → ast → prolog → parse → type → optimize
;;;;                   → vm → expand → compile

(asdf:defsystem :cl-cc-compile
  :description "Compilation engine: CPS, codegen, builtin registry"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-type
               :cl-cc-optimize :cl-cc-vm :cl-cc-emit :cl-cc-expand :cl-cc-cps
               :cl-cc-codegen)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
