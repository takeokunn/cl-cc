;;;; cl-cc-sb-mop.asd --- SB-MOP compatibility package scaffold

(asdf:defsystem :cl-cc-sb-mop
  :description "SB-MOP compatibility facade over CL-CC VM MOP helpers"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
