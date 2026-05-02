;;;; tests/unit/vm/vm-run-fusion-tests.lisp — VM2 superinstruction fusion and extended run-vm tests

(in-package :cl-cc/test)

(in-suite vm-run-suite)

(deftest run-vm-chains-arithmetic-correctly
  "run-vm executes a multi-instruction arithmetic sequence and halts with the correct result."
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 1 3   nil
                               cl-cc:+op2-const+ 2 4   nil
                               cl-cc:+op2-add2+  3 1   2
                               cl-cc:+op2-const+ 4 5   nil
                               cl-cc:+op2-mul2+  0 3   4
                               cl-cc:+op2-halt2+ 0 nil nil)))
      (assert-= 35 (cl-cc/vm::run-vm code s)))))

(deftest run-vm-loads-any-cl-object
  "run-vm can load and return any CL object as an immediate constant."
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 0 :hello nil
                               cl-cc:+op2-halt2+ 0 nil    nil)))
      (assert-equal :hello (cl-cc/vm::run-vm code s)))))

(deftest run-vm-preserves-non-result-registers
  "run-vm leaves non-halted registers intact after execution."
  (let ((s (cl-cc:make-vm2-state)))
    (let ((code (make-bytecode cl-cc:+op2-const+ 1 10  nil
                               cl-cc:+op2-const+ 2 20  nil
                               cl-cc:+op2-const+ 0 99  nil
                               cl-cc:+op2-halt2+ 0 nil nil)))
      (cl-cc/vm::run-vm code s)
      (assert-= 10 (cl-cc/vm::vm2-reg-get s 1))
      (assert-= 20 (cl-cc/vm::vm2-reg-get s 2)))))

(deftest vm2-bigram-collection-counts-pairs
  "vm2-collect-opcode-bigrams counts each consecutive opcode pair in the bytecode."
  (let* ((code (make-bytecode cl-cc:+op2-const+ 1 3 nil
                              cl-cc:+op2-add-imm2+ 0 1 4
                              cl-cc:+op2-halt2+ 0 nil nil
                              cl-cc:+op2-const+ 1 5 nil
                              cl-cc:+op2-add-imm2+ 0 1 6
                              cl-cc:+op2-halt2+ 0 nil nil))
         (counts (cl-cc/vm::vm2-collect-opcode-bigrams code)))
    (assert-= 2 (gethash '(cl-cc:const cl-cc:add-imm2) counts))
    (assert-= 2 (gethash '(cl-cc:add-imm2 cl-cc:halt2) counts))))

(deftest vm2-top-candidates-sorts-by-frequency
  "vm2-top-superoperator-candidates returns candidates sorted by bigram frequency."
  (let* ((code (make-bytecode cl-cc:+op2-const+ 1 3 nil
                              cl-cc:+op2-add-imm2+ 0 1 4
                              cl-cc:+op2-halt2+ 0 nil nil
                              cl-cc:+op2-const+ 1 5 nil
                              cl-cc:+op2-add-imm2+ 0 1 6
                              cl-cc:+op2-halt2+ 0 nil nil
                              cl-cc:+op2-move+ 2 1 nil
                              cl-cc:+op2-halt2+ 2 nil nil))
         (top (cl-cc/vm::vm2-top-superoperator-candidates code :limit 2))
         (add-imm2-name (aref cl-cc/vm::*opcode-name-table* cl-cc:+op2-add-imm2+))
         (halt2-name    (aref cl-cc/vm::*opcode-name-table* cl-cc:+op2-halt2+)))
    (assert-= 2 (second (first top)))
    (assert-true (member (list add-imm2-name halt2-name) (mapcar #'first top) :test #'equal))))

(deftest-each vm2-fuse-immediate-superinstructions-arith
  "const+arith2 pairs fuse to their immediate variants."
  :cases (("add" cl-cc:+op2-const+ 2 4 nil  cl-cc:+op2-add2+ 0 1 2  cl-cc:+op2-add-imm2+ 4)
          ("sub" cl-cc:+op2-const+ 2 5 nil  cl-cc:+op2-sub2+ 0 1 2  cl-cc:+op2-sub-imm2+ 5)
          ("mul" cl-cc:+op2-const+ 2 6 nil  cl-cc:+op2-mul2+ 0 2 1  cl-cc:+op2-mul-imm2+ 6))
  (c0 r0 v0 x0  c1 d1 s1 s2  expected-fused-op expected-imm)
  (let* ((code  (make-bytecode c0 r0 v0 x0  c1 d1 s1 s2  cl-cc:+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= expected-fused-op (aref fused 0))
    (assert-= expected-imm      (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-const-halt
  "const followed by halt on the same register fuses to const-halt2."
  (let* ((code (make-bytecode cl-cc:+op2-const+ 0 42 nil
                              cl-cc:+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc:+op2-const-halt2+ (aref fused 0))
    (assert-= 42 (aref fused 1))
    (assert-= 4 (length fused))))

(deftest vm2-fuse-immediate-superinstructions-compare
  "const+generic compare pairs fuse to their immediate compare variants."
  (let* ((eq-code (make-bytecode cl-cc:+op2-const+ 2 6 nil
                                 cl-cc:+op2-num-eq2+ 0 1 2
                                 cl-cc:+op2-halt2+ 0 nil nil))
         (lt-code (make-bytecode cl-cc:+op2-const+ 2 6 nil
                                 cl-cc:+op2-num-lt2+ 0 1 2
                                 cl-cc:+op2-halt2+ 0 nil nil))
         (gt-code (make-bytecode cl-cc:+op2-const+ 2 6 nil
                                 cl-cc:+op2-num-gt2+ 0 1 2
                                 cl-cc:+op2-halt2+ 0 nil nil))
         (le-code (make-bytecode cl-cc:+op2-const+ 2 6 nil
                                 cl-cc:+op2-num-le2+ 0 1 2
                                 cl-cc:+op2-halt2+ 0 nil nil))
         (ge-code (make-bytecode cl-cc:+op2-const+ 2 6 nil
                                 cl-cc:+op2-num-ge2+ 0 1 2
                                 cl-cc:+op2-halt2+ 0 nil nil)))
    (assert-= cl-cc:+op2-num-eq-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions eq-code) 0))
    (assert-= cl-cc:+op2-num-lt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions lt-code) 0))
    (assert-= cl-cc:+op2-num-gt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions gt-code) 0))
    (assert-= cl-cc:+op2-num-le-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions le-code) 0))
    (assert-= cl-cc:+op2-num-ge-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions ge-code) 0))))

(deftest run-vm-fusion-const-add-produces-fused-bytecode
  "vm2-fuse-immediate-superinstructions fuses const+add to an add-imm superinstruction."
  (let* ((code (make-bytecode cl-cc:+op2-const+ 2 4 nil
                              cl-cc:+op2-add2+  0 1 2
                              cl-cc:+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code))
         (s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 1 3)
    (assert-= 7 (cl-cc/vm::run-vm code s))
    (assert-= 8 (length fused))))

(deftest run-vm-fusion-const-halt-computes-result
  "run-vm executes const+halt and returns the constant value."
  (assert-= 42 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 0 42 nil
                                                cl-cc:+op2-halt2+ 0 nil nil)
                                 (cl-cc:make-vm2-state))))

(deftest run-vm-fusion-specialized-opcodes-execute-correctly
  "add-imm2 and num-gt-imm2 specialized opcodes produce correct results."
  (let ((s (cl-cc:make-vm2-state)))
    (assert-= 7 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 1 3 nil
                                             cl-cc:+op2-add-imm2+ 0 1 4
                                             cl-cc:+op2-halt2+ 0 nil nil)
                               s))
    (assert-= 1 (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-const+ 1 7 nil
                                             cl-cc:+op2-num-gt-imm2+ 0 1 5
                                             cl-cc:+op2-halt2+ 0 nil nil)
                               s))))

(deftest run-vm-with-opcode-bigrams-counts-executed-pairs
  "run-vm-with-opcode-bigrams returns executed opcode bigram counts."
  (let ((s (cl-cc:make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams
         (make-bytecode cl-cc:+op2-const+ 1 3 nil
                        cl-cc:+op2-add-imm2+ 0 1 4
                        cl-cc:+op2-halt2+ 0 nil nil)
         s)
      (assert-= 7 result)
      (assert-= 1 (gethash '(cl-cc:const cl-cc:add-imm2) counts))
      (assert-= 1 (gethash '(cl-cc:add-imm2 cl-cc:halt2) counts)))))

(deftest vm2-state-init-pre-populates-globals
  "make-vm2-state pre-populates *active-restarts* and *standard-output* in global-vars."
  (let ((s (cl-cc:make-vm2-state)))
    (multiple-value-bind (val found-p)
        (gethash 'cl-cc:*active-restarts* (cl-cc:vm2-state-global-vars s))
      (assert-true found-p)
      (assert-null val))
    (assert-true (not (null (gethash '*standard-output* (cl-cc:vm2-state-global-vars s)))))))

(deftest vm2-state-init-accepts-custom-output-stream
  "make-vm2-state :output-stream stores the supplied stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc:make-vm2-state :output-stream str)))
    (assert-eq str (cl-cc:vm2-state-output-stream s))))

(deftest vm2-state-init-vm-global-vars-contains-features
  "vm-global-vars on a vm2-state returns a hash table containing *features*."
  (let ((s (cl-cc:make-vm2-state)))
    (assert-true (hash-table-p (cl-cc/vm::vm-global-vars s)))
    (assert-true (not (null (gethash '*features* (cl-cc/vm::vm-global-vars s)))))))

(defclass vm2-test-instance ()
  ((x :initarg :x :reader vm2-test-instance-x)))

(deftest run-vm-make-instance-opcode
  "run-vm make-instance uses R0..R(n-1) as alternating initargs/value pairs."
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 0 :x)
    (cl-cc/vm::vm2-reg-set s 1 42)
    (let ((result (cl-cc/vm::run-vm (make-bytecode cl-cc:+op2-make-instance+ 2 'vm2-test-instance 2
                                                cl-cc:+op2-halt2+ 2 nil nil)
                                 s)))
      (assert-true (typep result 'vm2-test-instance))
      (assert-= 42 (vm2-test-instance-x result)))))

(deftest-each run-vm-edge-cases
  "run-vm handles edge-case operand values correctly."
  :cases (("sub-negative"
           (make-bytecode cl-cc:+op2-const+ 1 3   nil
                          cl-cc:+op2-const+ 2 10  nil
                          cl-cc:+op2-sub2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil)
           -7)
          ("mul-by-zero"
           (make-bytecode cl-cc:+op2-const+ 1 12  nil
                          cl-cc:+op2-const+ 2 0   nil
                          cl-cc:+op2-mul2+  0 1   2
                          cl-cc:+op2-halt2+ 0 nil nil)
           0)
          ("large-immediate"
           (make-bytecode cl-cc:+op2-const+ 0 1000000 nil
                          cl-cc:+op2-halt2+ 0 nil     nil)
           1000000)
          ("nil-immediate"
           (make-bytecode cl-cc:+op2-const+ 0 nil nil
                          cl-cc:+op2-halt2+ 0 nil nil)
           nil)
          ("move-chain"
           (make-bytecode cl-cc:+op2-const+ 1 55  nil
                          cl-cc:+op2-move+  2 1   nil
                          cl-cc:+op2-move+  0 2   nil
                          cl-cc:+op2-halt2+ 0 nil nil)
           55))
  (code expected)
  (let ((s (cl-cc:make-vm2-state)))
    (assert-equal expected (cl-cc/vm::run-vm code s))))

