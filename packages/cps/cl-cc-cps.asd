;;;; cl-cc-cps.asd — skeleton for the cps feature package
;;;;
;;;; Phase 1 of the ideal package-by-feature redesign (post 2026-05-01 plan).
;;;; This system is intentionally empty — files will migrate here in
;;;; subsequent phases. Currently NOT wired into root cl-cc.asd, so loading
;;;; this system as part of `:cl-cc` is a no-op.

(asdf:defsystem :cl-cc-cps
  :description "CPS form definitions and AST->CPS transformation"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-parse)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cps")
   (:file "cps-ast")
   (:file "cps-ast-control")
   (:file "cps-ast-extended")
   (:file "cps-ast-functional")))
