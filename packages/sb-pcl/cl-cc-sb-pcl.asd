;;;; cl-cc-sb-pcl.asd --- SB-PCL compatibility package scaffold

(asdf:defsystem :cl-cc-sb-pcl
  :description "Minimal SB-PCL compatibility facade over CL-CC VM CLOS helpers"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
