;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection - ASDF System Definition

(asdf:defsystem :cl-cc
  :description "CL-CC: Common Lisp Compiler Collection"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :components
  ((:module "src"
    :serial t
    :components
      ((:file "package")
        (:file "frontend")
        (:file "macro")
        (:file "reader")
        (:file "cps")
        (:file "prolog")
        (:file "vm")
       (:file "vm-primitives")
       (:file "vm-io")
       (:file "vm-conditions")
       (:file "vm-list")
       (:file "vm-strings")
       (:file "vm-hash")
          ;; (:file "reader-printer")  ; TODO: rewrite needed - structural issues
        (:module "type"
         :serial t
         :components
         ((:file "package")
          (:file "representation")
          (:file "unification")
          (:file "parser")
          (:file "inference")))
 (:module "backend"
         :serial t
         :components
         ((:file "calling-convention")
          (:file "regalloc")
          (:file "x86-64")
          (:file "x86-64-codegen")
          (:file "aarch64")))
       (:module "binary"
        :serial t
        :components
        ((:file "package")
         (:file "macho")))
       (:file "compiler")))))

(asdf:defsystem :cl-cc/test
  :description "CL-CC tests"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :serial t
  :components
  ((:module "tests"
     :serial t
     :components
     ((:file "package")
      (:file "framework")
      (:file "framework-advanced")
      (:file "framework-compiler")
      (:file "framework-meta")
      (:file "framework-fuzz")
      (:file "compiler-tests")
      (:file "cps-tests")
      (:file "prolog-tests")
      (:file "ast-roundtrip-tests")
      (:file "macro-tests")
      (:file "closure-tests")
      (:file "call-conv-tests")
      (:file "control-flow-tests")
      (:file "vm-heap-tests")
      (:file "clos-tests")
      (:file "type-tests")
      (:file "regalloc-tests")))
   (:module "pbt"
     :serial t
     :pathname "tests/pbt"
     :components
     ((:file "package")
      (:file "framework")
      (:file "generators")
      (:file "vm-pbt-tests")
      (:file "cps-pbt-tests")
      (:file "ast-pbt-tests")
      (:file "macro-pbt-tests")
      (:file "prolog-pbt-tests")
      (:file "vm-heap-pbt-tests"))))
  :perform (asdf:test-op (op c)
              (declare (ignore op c))
              (uiop:symbol-call :cl-cc/test 'run-tests)))
