;;;; cl-cc-pipeline.asd — feature package for the public compilation API.
;;;;
;;;; Phase 4 strict-packaging: pipeline files moved here from
;;;; packages/pipeline/src/. Defines compile-expression, compile-string,
;;;; run-string, and the stdlib cache infrastructure.
;;;;
;;;; Note: pipeline-selfhost.lisp (warm-stdlib-cache, our-eval) lives in
;;;; cl-cc-selfhost; pipeline-repl-*.lisp lives in cl-cc-repl.

(asdf:defsystem :cl-cc-pipeline
  :description "Public compiler API: compile-string, run-string, our-eval"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-php :cl-cc-type
               :cl-cc-optimize :cl-cc-vm :cl-cc-expand :cl-cc-emit
               :cl-cc-stdlib :cl-cc-binary :cl-cc-compile)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "pipeline-stdlib")
   (:file "pipeline-data")
   (:file "pipeline-cps")
   (:file "pipeline")
   (:file "pipeline-runtime")
   (:file "pipeline-native")
   (:file "pipeline-native-typeclass")))
