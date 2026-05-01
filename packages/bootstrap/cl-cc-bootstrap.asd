;;;; cl-cc-bootstrap.asd — Phase 2: bootstrap symbols subsystem
;;;;
;;;; Placed in packages/bootstrap/ (co-located with its source) per the
;;;; package-by-feature monorepo design.  cl-cc.asd loads this via
;;;; (load (merge-pathnames "packages/bootstrap/cl-cc-bootstrap.asd" here))
;;;; before any other subsystem so the 12 exported symbols exist in
;;;; :cl-cc/bootstrap before cl-cc/prolog or cl-cc/compile are defined.

(asdf:defsystem :cl-cc-bootstrap
  :description "cl-cc bootstrap: pre-interned symbols shared by cl-cc/prolog and cl-cc/compile"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")))
