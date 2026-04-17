(eval-when (:load-toplevel :execute)
  (unless (asdf:find-system :cl-cc-vm nil)
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (asdf:load-asd (merge-pathnames "../../engine/vm/cl-cc-vm.asd" here))
      (asdf:load-asd (merge-pathnames "../../foundation/prolog/cl-cc-prolog.asd" here))
      (asdf:load-asd (merge-pathnames "../../foundation/type/cl-cc-type.asd" here)))))

(asdf:defsystem :cl-cc-optimize
  :description "Optimizer subsystem: CFG, SSA, E-graph, peephole, pipeline"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-vm :cl-cc-prolog :cl-cc-type)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "effects")
   (:file "cfg")
   (:file "cfg-analysis")
   (:file "cfg-layout")
   (:file "ssa")
   (:file "ssa-construction")
   (:file "egraph")
   (:file "egraph-saturation")
   (:file "egraph-rules")
   (:file "egraph-rules-advanced")
   (:file "optimizer-tables")
   (:file "optimizer-algebraic")
   (:file "optimizer-inline")
   (:file "optimizer-inline-pass")
   (:file "optimizer-dataflow")
   (:file "optimizer-copyprop")
   (:file "optimizer-memory")
   (:file "optimizer-memory-passes")
   (:file "optimizer-flow")
   (:file "optimizer-flow-passes")
   (:file "optimizer-strength")
   (:file "optimizer-strength-ext")
   (:file "optimizer-cse-gvn")
   (:file "optimizer-licm")
   (:file "optimizer")
   (:file "optimizer-pipeline")))
