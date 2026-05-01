;;;; cl-cc-selfhost.asd — feature package for self-hosting orchestration.
;;;;
;;;; Phase 4 strict-packaging: pipeline-selfhost.lisp moved here from
;;;; packages/pipeline/src/. Defines warm-stdlib-cache, our-eval,
;;;; and the host-bridge entry registration.

(asdf:defsystem :cl-cc-selfhost
  :description "Self-hosting orchestration"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-pipeline :cl-cc-expand :cl-cc-vm
               :cl-cc-runtime :cl-cc-compile)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "pipeline-selfhost")))
