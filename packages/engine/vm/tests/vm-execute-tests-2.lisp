;;;; tests/unit/vm/vm-execute-tests-2.lisp — Core VM Execute-Instruction Tests (Part 2)
;;;;
;;;; Continuation of vm-execute-tests.lisp:
;;;; multiple-values ops, vm-apply, values-buffer management,
;;;; side-effect instructions, and make-closure/closure-ref-idx.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest vm-execute-vm-values-stores-all
  "vm-values stores first value in dst and all values in values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 10)
    (cl-cc:vm-reg-set s :R2 20)
    (cl-cc:vm-reg-set s :R3 30)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-values :dst :R0 :src-regs (list :R1 :R2 :R3)) s 0 (make-hash-table :test #'equal))
    (assert-= 10 (cl-cc:vm-reg-get s :R0))
    (assert-equal '(10 20 30) (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-mv-bind-distributes
  "vm-mv-bind distributes values-list into individual destination registers."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-mv-bind :dst-regs (list :R0 :R1 :R2)) s 0 (make-hash-table :test #'equal))
    (assert-= 1 (cl-cc:vm-reg-get s :R0))
    (assert-= 2 (cl-cc:vm-reg-get s :R1))
    (assert-= 3 (cl-cc:vm-reg-get s :R2))))

(deftest vm-execute-values-to-list-copies
  "vm-values-to-list copies the current values-list into the destination register."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(7 8 9))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-values-to-list :dst :R0) s 0 (make-hash-table :test #'equal))
    (assert-equal '(7 8 9) (cl-cc:vm-reg-get s :R0))))

(deftest vm-execute-spread-values-roundtrip
  "vm-spread-values loads first element into dst and full list into values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 '(100 200 300))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-spread-values :dst :R0 :src :R1) s 0 (make-hash-table :test #'equal))
    (assert-= 100 (cl-cc:vm-reg-get s :R0))
    (assert-equal '(100 200 300) (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-vm-apply-spreads-final-list-on-host-function
  "vm-apply splices the final list argument and applies host functions directly."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 #'+)
    (cl-cc:vm-reg-set s :R2 10)
    (cl-cc:vm-reg-set s :R3 20)
    (cl-cc:vm-reg-set s :R4 '(30 40))
    (multiple-value-bind (next-pc halted result)
        (cl-cc/vm::execute-instruction
         (cl-cc::make-vm-apply :dst :R0 :func :R1 :args '(:R2 :R3 :R4))
         s 5 (make-hash-table :test #'equal))
      (declare (ignore result))
      (assert-= 6 next-pc)
      (assert-false halted)
      (assert-= 100 (cl-cc:vm-reg-get s :R0)))))

(deftest vm-execute-vm-values-buffer-management
  "vm-clear-values resets values-list; vm-ensure-values initialises it from src when nil."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-clear-values) s 0 (make-hash-table :test #'equal))
    (assert-null (cl-cc/vm::vm-values-list s)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 55)
    (setf (cl-cc/vm::vm-values-list s) nil)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-ensure-values :src :R0) s 0 (make-hash-table :test #'equal))
    (assert-equal '(55) (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-set-global-stores-value
  "vm-set-global writes the source register value into the global vars table."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-set-global :name 'myvar :src :R0) s 0 (make-hash-table :test #'equal))
    (assert-= 42 (gethash 'myvar (cl-cc/vm::vm-global-vars s)))))

(deftest vm-execute-get-global-loads-value
  "vm-get-global reads from the global vars table into the destination register."
  (let ((s (make-test-vm)))
    (setf (gethash 'myvar2 (cl-cc/vm::vm-global-vars s)) 99)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-get-global :dst :R0 :name 'myvar2) s 0 (make-hash-table :test #'equal))
    (assert-= 99 (cl-cc:vm-reg-get s :R0))))

(deftest vm-execute-print-writes-to-stream
  "vm-print writes the register value followed by a newline to the output stream."
  (let* ((str (make-string-output-stream))
         (s   (make-instance 'cl-cc/vm::vm-io-state :output-stream str)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-print :reg :R0) s 0 (make-hash-table :test #'equal))
    (assert-string= (format nil "42~%") (get-output-stream-string str))))

(deftest vm-execute-register-function-stores-in-registry
  "vm-register-function stores a vm-closure-object in the function registry."
  (let ((s (make-test-vm)))
    (let ((closure (make-instance 'cl-cc/vm::vm-closure-object
                                  :entry-label "myfn"
                                  :params nil
                                  :captured-values nil)))
      (cl-cc:vm-reg-set s :R0 closure)
      (cl-cc/vm::execute-instruction
       (cl-cc::make-vm-register-function :name 'myfn :src :R0) s 0 (make-hash-table :test #'equal))
      (assert-true (not (null (gethash 'myfn (cl-cc/vm::vm-function-registry s))))))))

(deftest vm-execute-make-closure-stores-vector-captures
  "vm-make-closure stores captured values in a vector and vm-closure-ref-idx reads by index."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 10)
    (cl-cc:vm-reg-set s :R2 20)
    (cl-cc:execute-instruction
     (cl-cc::make-vm-make-closure :dst :R0 :label "L" :params nil :env-regs '(:R1 :R2))
     s 0 (%labels))
    (let* ((addr (cl-cc:vm-reg-get s :R0))
           (closure (cl-cc:vm-heap-get s addr)))
      (assert-true (vectorp (cl-cc/vm::vm-closure-captured-values closure)))
      (assert-= 2 (length (cl-cc/vm::vm-closure-captured-values closure)))
      (cl-cc:vm-reg-set s :R3 addr)
      (cl-cc:execute-instruction
       (cl-cc::make-vm-closure-ref-idx :dst :R4 :closure :R3 :index 1)
       s 0 (%labels))
      (assert-= 20 (cl-cc:vm-reg-get s :R4)))))
