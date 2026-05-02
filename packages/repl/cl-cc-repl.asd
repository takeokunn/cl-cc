;;;; cl-cc-repl.asd — feature package for the REPL state machine.
;;;;
;;;; Phase 4 strict-packaging: pipeline-repl-*.lisp files moved here from
;;;; packages/pipeline/src/. Defines run-string-repl, run-form-repl,
;;;; our-load, and the persistent REPL state.

(asdf:defsystem :cl-cc-repl
  :description "REPL state machine"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-pipeline :cl-cc-selfhost :cl-cc-expand
               :cl-cc-vm :cl-cc-parse :cl-cc-compile :cl-cc-runtime
               :cl-cc-ast :cl-cc-optimize :cl-cc-emit :cl-cc-stdlib)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "pipeline-repl-state")
   (:file "pipeline-repl-data")
   (:file "pipeline-repl-load")
   (:file "pipeline-repl-ourload")))
