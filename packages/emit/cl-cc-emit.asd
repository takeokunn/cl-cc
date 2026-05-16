(asdf:defsystem :cl-cc-emit
  :description "Emit backend subsystem: regalloc, codegen"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-mir :cl-cc-optimize :cl-cc-codegen)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "gpu")
   (:file "apple-ane")
   (:file "ebpf")
   (:file "fpga")))
