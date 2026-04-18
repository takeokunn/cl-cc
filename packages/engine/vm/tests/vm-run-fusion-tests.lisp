;;;; tests/unit/vm/vm-run-fusion-tests.lisp — VM2 superinstruction fusion and extended run-vm tests

(in-package :cl-cc/test)

(in-suite vm-run-suite)

(deftest run-vm-chained-arithmetic
  "run-vm correctly chains multiple arithmetic operations."
  (let ((s (cl-cc::make-vm2-state)))
    ;; (3 + 4) * 5 = 35: r1=3, r2=4, r3=r1+r2=7, r4=5, r0=r3*r4=35
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 3   nil
                               cl-cc::+op2-const+ 2 4   nil
                               cl-cc::+op2-add2+  3 1   2
                               cl-cc::+op2-const+ 4 5   nil
                               cl-cc::+op2-mul2+  0 3   4
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 35 (cl-cc/vm::run-vm code s)))))

(deftest run-vm-const-any-cl-object
  "OP2-CONST can load any CL object as an immediate value."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 :hello nil
                               cl-cc::+op2-halt2+ 0 nil    nil)))
      (assert-equal :hello (cl-cc/vm::run-vm code s)))))

(deftest run-vm-register-independence
  "run-vm leaves non-result registers with their last computed values."
  (let ((s (cl-cc::make-vm2-state)))
    ;; r1=10, r2=20; halt on r0 (which we set to 0 via sub2 of same reg)
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 10  nil
                               cl-cc::+op2-const+ 2 20  nil
                               cl-cc::+op2-const+ 0 99  nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (cl-cc/vm::run-vm code s)
      (assert-= 10 (cl-cc/vm::vm2-reg-get s 1))
      (assert-= 20 (cl-cc/vm::vm2-reg-get s 2)))))

(deftest vm2-collect-opcode-bigrams-counts-adjacent-opcodes
  "vm2-collect-opcode-bigrams counts opcode pairs across 4-word instructions."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 1 3 nil
                              cl-cc::+op2-add-imm2+ 0 1 4
                              cl-cc::+op2-halt2+ 0 nil nil
                              cl-cc::+op2-const+ 1 5 nil
                              cl-cc::+op2-add-imm2+ 0 1 6
                              cl-cc::+op2-halt2+ 0 nil nil))
         (counts (cl-cc/vm::vm2-collect-opcode-bigrams code)))
    (assert-= 2 (gethash '(cl-cc::const cl-cc::add-imm2) counts))
    (assert-= 2 (gethash '(cl-cc::add-imm2 cl-cc::halt2) counts))))

(deftest vm2-top-superoperator-candidates-sorts-by-frequency
  "vm2-top-superoperator-candidates returns descending bigram counts."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 1 3 nil
                              cl-cc::+op2-add-imm2+ 0 1 4
                              cl-cc::+op2-halt2+ 0 nil nil
                              cl-cc::+op2-const+ 1 5 nil
                              cl-cc::+op2-add-imm2+ 0 1 6
                              cl-cc::+op2-halt2+ 0 nil nil
                              cl-cc::+op2-move+ 2 1 nil
                              cl-cc::+op2-halt2+ 2 nil nil))
         (top (cl-cc/vm::vm2-top-superoperator-candidates code :limit 2))
         ;; Lookup names from the opcode table (package-independent)
         (add-imm2-name (aref cl-cc/vm::*opcode-name-table* cl-cc::+op2-add-imm2+))
         (halt2-name    (aref cl-cc/vm::*opcode-name-table* cl-cc::+op2-halt2+)))
    ;; Both top bigrams have count 2
    (assert-= 2 (second (first top)))
    ;; The (add-imm2 halt2) bigram appears in the top-2 results
    (assert-true (member (list add-imm2-name halt2-name) (mapcar #'first top) :test #'equal))))

(deftest vm2-fuse-immediate-superinstructions-add
  "const+add2 using a temporary immediate register fuses to add-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 4 nil
                              cl-cc::+op2-add2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-add-imm2+ (aref fused 0))
    (assert-= 0 (aref fused 1))
    (assert-= 1 (aref fused 2))
    (assert-= 4 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-sub
  "const+sub2 with an immediate RHS fuses to sub-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 5 nil
                              cl-cc::+op2-sub2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-sub-imm2+ (aref fused 0))
    (assert-= 5 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-mul
  "const+mul2 using a temporary immediate register fuses to mul-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                              cl-cc::+op2-mul2+  0 2 1
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-mul-imm2+ (aref fused 0))
    (assert-= 0 (aref fused 1))
    (assert-= 1 (aref fused 2))
    (assert-= 6 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-const-halt
  "const followed by halt on the same register fuses to const-halt2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 0 42 nil
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-const-halt2+ (aref fused 0))
    (assert-= 42 (aref fused 1))
    (assert-= 4 (length fused))))

(deftest vm2-fuse-immediate-superinstructions-compare
  "const+generic compare pairs fuse to their immediate compare variants."
  (let* ((eq-code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                                 cl-cc::+op2-num-eq2+ 0 1 2
                                 cl-cc::+op2-halt2+ 0 nil nil))
         (lt-code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                                 cl-cc::+op2-num-lt2+ 0 1 2
                                 cl-cc::+op2-halt2+ 0 nil nil))
         (gt-code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                                 cl-cc::+op2-num-gt2+ 0 1 2
                                 cl-cc::+op2-halt2+ 0 nil nil))
         (le-code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                                 cl-cc::+op2-num-le2+ 0 1 2
                                 cl-cc::+op2-halt2+ 0 nil nil))
         (ge-code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                                 cl-cc::+op2-num-ge2+ 0 1 2
                                 cl-cc::+op2-halt2+ 0 nil nil)))
    (assert-= cl-cc::+op2-num-eq-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions eq-code) 0))
    (assert-= cl-cc::+op2-num-lt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions lt-code) 0))
    (assert-= cl-cc::+op2-num-gt-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions gt-code) 0))
    (assert-= cl-cc::+op2-num-le-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions le-code) 0))
    (assert-= cl-cc::+op2-num-ge-imm2+ (aref (cl-cc/vm::vm2-fuse-immediate-superinstructions ge-code) 0))))

(deftest run-vm-uses-immediate-fusion-helper
  "run-vm accepts unfused const+arith bytecode and still executes via fused code."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 4 nil
                              cl-cc::+op2-add2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc/vm::vm2-fuse-immediate-superinstructions code))
         (s (cl-cc::make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 1 3)
    (assert-= 7 (cl-cc/vm::run-vm code s))
    (assert-= 8 (length fused))))

(deftest run-vm-uses-const-halt-fusion-helper
  "run-vm accepts const+halt and returns via fused const-halt2."
  (assert-= 42 (cl-cc/vm::run-vm (make-bytecode cl-cc::+op2-const+ 0 42 nil
                                            cl-cc::+op2-halt2+ 0 nil nil)
                              (cl-cc::make-vm2-state))))

(deftest run-vm-specializes-hot-opcodes-with-fallback
  "run-vm keeps the hot specialized opcodes fast while preserving generic fallback."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= 7 (cl-cc/vm::run-vm (make-bytecode cl-cc::+op2-const+ 1 3 nil
                                             cl-cc::+op2-add-imm2+ 0 1 4
                                             cl-cc::+op2-halt2+ 0 nil nil)
                               s))
    (assert-= 1 (cl-cc/vm::run-vm (make-bytecode cl-cc::+op2-const+ 1 7 nil
                                             cl-cc::+op2-num-gt-imm2+ 0 1 5
                                             cl-cc::+op2-halt2+ 0 nil nil)
                               s))))

(deftest run-vm-with-opcode-bigrams-counts-executed-pairs
  "run-vm-with-opcode-bigrams returns executed opcode bigram counts."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams
         (make-bytecode cl-cc::+op2-const+ 1 3 nil
                        cl-cc::+op2-add-imm2+ 0 1 4
                        cl-cc::+op2-halt2+ 0 nil nil)
         s)
      (assert-= 7 result)
      (assert-= 1 (gethash '(cl-cc::const cl-cc::add-imm2) counts))
      (assert-= 1 (gethash '(cl-cc::add-imm2 cl-cc::halt2) counts)))))

(deftest vm2-state-globals-init
  "vm2-state pre-populates *active-restarts* (nil) and *standard-output* (non-nil)."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (val found-p)
        (gethash 'cl-cc::*active-restarts* (cl-cc::vm2-state-global-vars s))
      (assert-true found-p)
      (assert-null val))
    (assert-true (not (null (gethash '*standard-output* (cl-cc::vm2-state-global-vars s)))))))

(deftest vm2-state-output-stream-writable
  "vm2-state :output-stream slot can be set to a custom stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

(defclass vm2-test-instance ()
  ((x :initarg :x :reader vm2-test-instance-x)))

(deftest run-vm-make-instance-opcode
  "run-vm make-instance uses R0..R(n-1) as alternating initargs/value pairs."
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 0 :x)
    (cl-cc/vm::vm2-reg-set s 1 42)
    (let ((result (cl-cc/vm::run-vm (make-bytecode cl-cc::+op2-make-instance+ 2 'vm2-test-instance 2
                                                cl-cc::+op2-halt2+ 2 nil nil)
                                 s)))
      (assert-true (typep result 'vm2-test-instance))
      (assert-= 42 (vm2-test-instance-x result)))))

(deftest-each run-vm-edge-cases
  "run-vm handles edge-case operand values correctly."
  :cases (("sub-negative"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-const+ 2 10  nil
                          cl-cc::+op2-sub2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           -7)
          ("mul-by-zero"
           (make-bytecode cl-cc::+op2-const+ 1 12  nil
                          cl-cc::+op2-const+ 2 0   nil
                          cl-cc::+op2-mul2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("large-immediate"
           (make-bytecode cl-cc::+op2-const+ 0 1000000 nil
                          cl-cc::+op2-halt2+ 0 nil     nil)
           1000000)
          ("nil-immediate"
           (make-bytecode cl-cc::+op2-const+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           nil)
          ("move-chain"
           (make-bytecode cl-cc::+op2-const+ 1 55  nil
                          cl-cc::+op2-move+  2 1   nil
                          cl-cc::+op2-move+  0 2   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           55))
  (code expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-equal expected (cl-cc/vm::run-vm code s))))

(deftest vm2-global-vars-shim
  "vm-global-vars shim returns a hash table; vm2-state pre-populates *features*."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (hash-table-p (cl-cc/vm::vm-global-vars s)))
    (assert-true (not (null (gethash '*features* (cl-cc/vm::vm-global-vars s)))))))
