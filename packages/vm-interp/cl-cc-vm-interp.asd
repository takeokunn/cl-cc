;;;; cl-cc-vm-interp.asd — skeleton for the vm-interp feature package
;;;;
;;;; Phase 1 of the ideal package-by-feature redesign (post 2026-05-01 plan).
;;;; This system is intentionally empty — files will migrate here in
;;;; subsequent phases. Currently NOT wired into root cl-cc.asd, so loading
;;;; this system as part of `:cl-cc` is a no-op.

(asdf:defsystem :cl-cc-vm-interp
  :description "VM interpreter — executes vm-isa instructions"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
