;;;; test-x86-64-codegen.lisp - Simple test for x86-64 code generation

(require :asdf)
(load "cl-cc.asd")
(asdf:load-system :cl-cc)

(use-package :cl-cc)

(format t "~%Testing x86-64 machine code generation...~%")

;; Create a simple VM program
(let* ((inst1 (make-instance 'vm-const :dst 0 :value 42))
       (inst2 (make-instance 'vm-const :dst 1 :value 10))
       (inst3 (make-instance 'vm-add :dst 0 :lhs 0 :rhs 1))
       (program (make-instance 'vm-program
                              :instructions (list inst1 inst2 inst3)
                              :result-register 0))
       (bytes (compile-to-x86-64-bytes program)))
  (format t "Generated ~D bytes~%" (length bytes))
  (format t "First 20 bytes: ~%")
  (loop for i from 0 below (min 20 (length bytes))
        do (format t "  ~2X: ~2X~%" i (aref bytes i))))

(format t "~%Test completed successfully!~%")
