;;;; cl-cc-cps.asd — skeleton for the cps feature package
;;;;
;;;; Feature-owned CPS source system.

(asdf:defsystem :cl-cc-cps
  :description "CPS form definitions and AST->CPS transformation"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cps")
   (:file "cps-ast")
   (:file "cps-ast-control")
   (:file "cps-ast-extended")
   (:file "cps-ast-functional")))
