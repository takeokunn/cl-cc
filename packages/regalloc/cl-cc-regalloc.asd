;;;; cl-cc-regalloc.asd — skeleton for the regalloc feature package
;;;;
;;;; Phase 1 of the ideal package-by-feature redesign (post 2026-05-01 plan).
;;;; This system is intentionally empty — files will migrate here in
;;;; subsequent phases. Currently NOT wired into root cl-cc.asd, so loading
;;;; this system as part of `:cl-cc` is a no-op.

(asdf:defsystem :cl-cc-regalloc
  :description "Register allocation passes (linear scan, spilling, live-range)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-mir :cl-cc-target :cl-cc-optimize)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "regalloc")
   (:file "regalloc-defs-uses")
   (:file "regalloc-allocate")))
