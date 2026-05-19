;;;; cl-cc-closer-mop.asd --- Closer-MOP compatibility package scaffold

(asdf:defsystem :cl-cc-closer-mop
  :description "Closer-MOP compatibility facade re-exporting CL-CC SB-MOP symbols"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-sb-mop)
  :pathname "src"
  :serial t
  :components
  ((:file "package")))
