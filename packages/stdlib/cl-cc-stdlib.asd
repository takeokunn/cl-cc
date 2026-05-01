;;;; cl-cc-stdlib.asd — feature package for the standard library source data.
;;;;
;;;; Phase 4 strict-packaging: stdlib-source*.lisp moved here from
;;;; packages/pipeline/src/. These are pure data files defining
;;;; *standard-library-source* and *standard-library-source-core*.

(asdf:defsystem :cl-cc-stdlib
  :description "Standard library source data (string constants)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "stdlib-source-core")
   (:file "stdlib-source")
   (:file "stdlib-source-ext")
   (:file "stdlib-source-clos")))
