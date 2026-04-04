;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package :cl-cc/test)

(defsuite vm-run-suite
  :description "Unit tests for vm-run.lisp error matching"
  :parent cl-cc-suite)

(in-suite vm-run-suite)

(deftest-each vm-error-type-matches
  "vm-error-type-matches-p dispatch table"
  :cases
  (("string-matches-error"
    "boom"  'error   t)
   ("string-matches-condition"
    "boom"  'condition t)
   ("string-matches-t"
    "boom"  't       t)
   ("string-no-match-specific-subtype"
    "boom"  'type-error nil)
   ("condition-object-matches-t"
    (make-condition 'simple-error :format-control "x") 't t))
  (error-val handler-type expected-result)
  (let ((actual (cl-cc::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (assert-true actual)
        (assert-false actual))))

(deftest build-label-table-uses-integer-keyed-buckets
  "build-label-table keeps string labels working while using integer outer keys."
  (let* ((instructions (list (cl-cc::make-vm-label :name "entry")
                             (cl-cc::make-vm-const :dst :r0 :value 1)
                             (cl-cc::make-vm-label :name :done)
                             (cl-cc::make-vm-halt :reg :r0)))
         (labels (cl-cc::build-label-table instructions)))
    (assert-eq #'eql (hash-table-test labels))
    (assert-= 0 (cl-cc::vm-label-table-lookup labels "entry"))
    (assert-= 2 (cl-cc::vm-label-table-lookup labels :done))))

(deftest vm-jump-uses-label-table-lookup
  "vm-jump resolves labels through the integer-keyed table produced by build-label-table."
  (let* ((instructions (list (cl-cc::make-vm-jump :label "target")
                             (cl-cc::make-vm-label :name "target")
                             (cl-cc::make-vm-halt :reg :r0)))
         (labels (cl-cc::build-label-table instructions))
         (state (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc::execute-instruction (first instructions) state 0 labels)
      (declare (ignore result))
      (assert-= 1 next-pc)
      (assert-false halt-p))))

;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

(deftest vm2-opcode-registration
  "defopcode: each opcode has a numeric id, dispatch handler, name entry, and encoder mapping."
  (assert-true (numberp cl-cc::+op2-const+))
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-const+))))
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-add2+))))
  (assert-equal 'cl-cc::add2 (aref cl-cc::*opcode-name-table* cl-cc::+op2-add2+))
  (assert-= cl-cc::+op2-const+ (gethash 'cl-cc::const cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-move+  (gethash 'cl-cc::move  cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-add2+  (gethash 'cl-cc::add2  cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-add-imm2+ (gethash 'cl-cc::add-imm2 cl-cc::*opcode-encoder-table*)))

(deftest vm2-opcode-distinct-values
  "Each defopcode gets a unique opcode number."
  (let ((ops (list cl-cc::+op2-const+
                   cl-cc::+op2-move+
                   cl-cc::+op2-add2+
                   cl-cc::+op2-add-imm2+
                   cl-cc::+op2-sub2+
                   cl-cc::+op2-sub-imm2+
                   cl-cc::+op2-mul2+
                   cl-cc::+op2-mul-imm2+
                   cl-cc::+op2-num-eq-imm2+
                   cl-cc::+op2-num-lt-imm2+
                   cl-cc::+op2-num-gt-imm2+
                   cl-cc::+op2-num-le-imm2+
                   cl-cc::+op2-num-ge-imm2+
                   cl-cc::+op2-halt2+)))
    (assert-= (length ops) (length (remove-duplicates ops)))))

(deftest vm2-state-structure
  "make-vm2-state: struct predicate, 256-register vector (all nil), :output-stream kwarg, *features* populated."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (cl-cc::vm2-state-p s))
    (assert-true (simple-vector-p (cl-cc::vm2-state-registers s)))
    (assert-= 256 (length (cl-cc::vm2-state-registers s)))
    (dotimes (i 256)
      (assert-true (null (cl-cc::vm2-reg-get s i))))
    (assert-null (cl-cc::vm2-state-values-buffer s))
    (assert-true (not (null (gethash '*features* (cl-cc::vm2-state-global-vars s))))))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-equal str (cl-cc::vm2-state-output-stream s))))

(deftest vm2-reg-operations
  "vm2-reg-set/get: stores, retrieves, returns value, overwrites; all 256 slots independent."
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc::vm2-reg-set s 0 42)
    (assert-= 42 (cl-cc::vm2-reg-get s 0))
    ;; set returns the written value
    (assert-= 99 (cl-cc::vm2-reg-set s 5 99))
    ;; overwrite
    (cl-cc::vm2-reg-set s 3 100)
    (cl-cc::vm2-reg-set s 3 200)
    (assert-= 200 (cl-cc::vm2-reg-get s 3))
    ;; all 256 slots are independent
    (dotimes (i 256)
      (cl-cc::vm2-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc::vm2-reg-get s i)))))

(deftest-each run-vm-basic-ops
  "run-vm executes a basic VM2 program and returns the correct halted value."
  :cases (("nop"
           (make-bytecode cl-cc::+op2-nop+ 0 nil nil
                          cl-cc::+op2-const+ 0 42 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("load-const"
           (make-bytecode cl-cc::+op2-load-const+ 0 '(1 2 3) nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           '(1 2 3))
          ("const-load"
           (make-bytecode cl-cc::+op2-const+ 0 42 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("load-nil"
           (make-bytecode cl-cc::+op2-load-nil+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           nil)
          ("load-true"
           (make-bytecode cl-cc::+op2-load-true+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           t)
          ("load-fixnum"
           (make-bytecode cl-cc::+op2-load-fixnum+ 0 123 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           123)
          ("move"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-move+  0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("neg"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-neg+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           -7)
          ("inc"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-inc+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           8)
          ("dec"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-dec+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           6)
          ("add"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-const+ 2 4   nil
                          cl-cc::+op2-add2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("sub"
           (make-bytecode cl-cc::+op2-const+ 1 10  nil
                          cl-cc::+op2-const+ 2 3   nil
                          cl-cc::+op2-sub2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("mul"
           (make-bytecode cl-cc::+op2-const+ 1 6   nil
                          cl-cc::+op2-const+ 2 7   nil
                          cl-cc::+op2-mul2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("div"
           (make-bytecode cl-cc::+op2-const+ 1 21  nil
                          cl-cc::+op2-const+ 2 3   nil
                          cl-cc::+op2-div+   0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("mod"
           (make-bytecode cl-cc::+op2-const+ 1 22  nil
                          cl-cc::+op2-const+ 2 5   nil
                          cl-cc::+op2-mod+   0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump"
           (make-bytecode cl-cc::+op2-jump+ 8 nil nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump-if-nil"
           (make-bytecode cl-cc::+op2-load-nil+ 1 nil nil
                          cl-cc::+op2-jump-if-nil+ 1 8 nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump-if-true"
           (make-bytecode cl-cc::+op2-load-true+ 1 nil nil
                          cl-cc::+op2-jump-if-true+ 1 8 nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("values/recv-values"
           (make-bytecode cl-cc::+op2-load-fixnum+ 0 10 nil
                          cl-cc::+op2-load-fixnum+ 1 20 nil
                          cl-cc::+op2-values+ 2 nil nil
                          cl-cc::+op2-load-nil+ 0 nil nil
                          cl-cc::+op2-load-nil+ 1 nil nil
                          cl-cc::+op2-recv-values+ 2 nil nil
                          cl-cc::+op2-halt2+ 1 nil nil)
           20)
          ("return"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 33 nil
                          cl-cc::+op2-return+ 1 nil nil)
           33)
          ("return-nil"
           (make-bytecode cl-cc::+op2-return-nil+ 0 nil nil)
           nil)
          ("fixnump"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 123 nil
                          cl-cc::+op2-fixnump+    0 1   nil
                          cl-cc::+op2-halt2+      0 nil nil)
           1)
          ("consp"
           (make-bytecode cl-cc::+op2-const+ 1 '(a . b) nil
                          cl-cc::+op2-consp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("symbolp"
           (make-bytecode cl-cc::+op2-const+ 1 foo nil
                          cl-cc::+op2-symbolp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("functionp"
           (make-bytecode cl-cc::+op2-const+ 1 #'car nil
                          cl-cc::+op2-functionp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("stringp"
           (make-bytecode cl-cc::+op2-const+ 1 "hi" nil
                          cl-cc::+op2-stringp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("cons/car"
           (make-bytecode cl-cc::+op2-const+ 1 'a nil
                          cl-cc::+op2-const+ 2 'b nil
                          cl-cc::+op2-cons+  3 1 2
                          cl-cc::+op2-car+   0 3 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           'a)
          ("cdr"
           (make-bytecode cl-cc::+op2-const+ 1 'a nil
                          cl-cc::+op2-const+ 2 'b nil
                          cl-cc::+op2-cons+  3 1 2
                          cl-cc::+op2-cdr+   0 3 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           'b)
          ("vector-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 3 nil
                          cl-cc::+op2-load-fixnum+ 2 0 nil
                          cl-cc::+op2-load-fixnum+ 3 7 nil
                          cl-cc::+op2-make-vector+ 4 1 2
                          cl-cc::+op2-vector-set+ 4 2 3
                          cl-cc::+op2-vector-ref+ 0 4 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("hash-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 8 nil
                          cl-cc::+op2-make-hash+  4 1 nil
                          cl-cc::+op2-const+      2 'foo nil
                          cl-cc::+op2-load-fixnum+ 3 77 nil
                          cl-cc::+op2-hash-set+   4 2 3
                          cl-cc::+op2-hash-ref+   0 4 2
                          cl-cc::+op2-halt2+      0 nil nil)
           77)
          ("global-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 55 nil
                          cl-cc::+op2-set-global+ 'foo 1 nil
                          cl-cc::+op2-get-global+ 0 'foo nil
                          cl-cc::+op2-halt2+      0 nil nil)
           55))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc::run-vm bytecode s))))

(deftest-each run-vm-immediate-ops
  "run-vm executes immediate arithmetic/comparison ops."
  :cases (("add-imm"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-add-imm2+ 0 1   4
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("cmp-imm"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-num-gt-imm2+ 0 1   5
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("cmp-imm-false"
           (make-bytecode cl-cc::+op2-const+ 1 2   nil
                          cl-cc::+op2-num-eq-imm2+ 0 1   5
                          cl-cc::+op2-halt2+ 0 nil nil)
           0))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc::run-vm bytecode s))))

(deftest-each run-vm-generic-compare-ops
  "run-vm executes generic compare opcodes over register operands."
  :cases (("num-eq-true"
           (make-bytecode cl-cc::+op2-const+ 1 5 nil
                          cl-cc::+op2-const+ 2 5 nil
                          cl-cc::+op2-num-eq2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-lt-true"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-lt2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-gt-false"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-gt2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("num-le-true"
           (make-bytecode cl-cc::+op2-const+ 1 9 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-le2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-ge-false"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-ge2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("eq-true"
           (make-bytecode cl-cc::+op2-const+ 1 7 nil
                          cl-cc::+op2-move+  2 1 nil
                          cl-cc::+op2-eq+    0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("eql-true"
           (make-bytecode cl-cc::+op2-const+ 1 7 nil
                          cl-cc::+op2-const+ 2 7 nil
                          cl-cc::+op2-eql+   0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("equal-true"
           (make-bytecode cl-cc::+op2-const+ 1 '(1 2) nil
                          cl-cc::+op2-const+ 2 '(1 2) nil
                          cl-cc::+op2-equal+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc::run-vm bytecode s))))

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
      (assert-= 35 (cl-cc::run-vm code s)))))

(deftest run-vm-const-any-cl-object
  "OP2-CONST can load any CL object as an immediate value."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 :hello nil
                               cl-cc::+op2-halt2+ 0 nil    nil)))
      (assert-equal :hello (cl-cc::run-vm code s)))))

(deftest run-vm-register-independence
  "run-vm leaves non-result registers with their last computed values."
  (let ((s (cl-cc::make-vm2-state)))
    ;; r1=10, r2=20; halt on r0 (which we set to 0 via sub2 of same reg)
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 10  nil
                               cl-cc::+op2-const+ 2 20  nil
                               cl-cc::+op2-const+ 0 99  nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (cl-cc::run-vm code s)
      (assert-= 10 (cl-cc::vm2-reg-get s 1))
      (assert-= 20 (cl-cc::vm2-reg-get s 2)))))

(deftest vm2-collect-opcode-bigrams-counts-adjacent-opcodes
  "vm2-collect-opcode-bigrams counts opcode pairs across 4-word instructions."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 1 3 nil
                              cl-cc::+op2-add-imm2+ 0 1 4
                              cl-cc::+op2-halt2+ 0 nil nil
                              cl-cc::+op2-const+ 1 5 nil
                              cl-cc::+op2-add-imm2+ 0 1 6
                              cl-cc::+op2-halt2+ 0 nil nil))
         (counts (cl-cc::vm2-collect-opcode-bigrams code)))
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
         (top (cl-cc::vm2-top-superoperator-candidates code :limit 2)))
    (assert-equal '(cl-cc::add-imm2 cl-cc::halt2) (first (first top)))
    (assert-= 2 (second (first top)))))

(deftest vm2-fuse-immediate-superinstructions-add
  "const+add2 using a temporary immediate register fuses to add-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 4 nil
                              cl-cc::+op2-add2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-add-imm2+ (aref fused 0))
    (assert-= 0 (aref fused 1))
    (assert-= 1 (aref fused 2))
    (assert-= 4 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-sub
  "const+sub2 with an immediate RHS fuses to sub-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 5 nil
                              cl-cc::+op2-sub2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-sub-imm2+ (aref fused 0))
    (assert-= 5 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-mul
  "const+mul2 using a temporary immediate register fuses to mul-imm2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 6 nil
                              cl-cc::+op2-mul2+  0 2 1
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc::vm2-fuse-immediate-superinstructions code)))
    (assert-= cl-cc::+op2-mul-imm2+ (aref fused 0))
    (assert-= 0 (aref fused 1))
    (assert-= 1 (aref fused 2))
    (assert-= 6 (aref fused 3))))

(deftest vm2-fuse-immediate-superinstructions-const-halt
  "const followed by halt on the same register fuses to const-halt2."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 0 42 nil
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc::vm2-fuse-immediate-superinstructions code)))
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
    (assert-= cl-cc::+op2-num-eq-imm2+ (aref (cl-cc::vm2-fuse-immediate-superinstructions eq-code) 0))
    (assert-= cl-cc::+op2-num-lt-imm2+ (aref (cl-cc::vm2-fuse-immediate-superinstructions lt-code) 0))
    (assert-= cl-cc::+op2-num-gt-imm2+ (aref (cl-cc::vm2-fuse-immediate-superinstructions gt-code) 0))
    (assert-= cl-cc::+op2-num-le-imm2+ (aref (cl-cc::vm2-fuse-immediate-superinstructions le-code) 0))
    (assert-= cl-cc::+op2-num-ge-imm2+ (aref (cl-cc::vm2-fuse-immediate-superinstructions ge-code) 0))))

(deftest run-vm-uses-immediate-fusion-helper
  "run-vm accepts unfused const+arith bytecode and still executes via fused code."
  (let* ((code (make-bytecode cl-cc::+op2-const+ 2 4 nil
                              cl-cc::+op2-add2+  0 1 2
                              cl-cc::+op2-halt2+ 0 nil nil))
         (fused (cl-cc::vm2-fuse-immediate-superinstructions code))
         (s (cl-cc::make-vm2-state)))
    (cl-cc::vm2-reg-set s 1 3)
    (assert-= 7 (cl-cc::run-vm code s))
    (assert-= 8 (length fused))))

(deftest run-vm-uses-const-halt-fusion-helper
  "run-vm accepts const+halt and returns via fused const-halt2." 
  (assert-= 42 (cl-cc::run-vm (make-bytecode cl-cc::+op2-const+ 0 42 nil
                                            cl-cc::+op2-halt2+ 0 nil nil)
                              (cl-cc::make-vm2-state))))

(deftest run-vm-specializes-hot-opcodes-with-fallback
  "run-vm keeps the hot specialized opcodes fast while preserving generic fallback."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= 7 (cl-cc::run-vm (make-bytecode cl-cc::+op2-const+ 1 3 nil
                                             cl-cc::+op2-add-imm2+ 0 1 4
                                             cl-cc::+op2-halt2+ 0 nil nil)
                               s))
    (assert-= 1 (cl-cc::run-vm (make-bytecode cl-cc::+op2-const+ 1 7 nil
                                             cl-cc::+op2-num-gt-imm2+ 0 1 5
                                             cl-cc::+op2-halt2+ 0 nil nil)
                               s))))

(deftest run-vm-with-opcode-bigrams-counts-executed-pairs
  "run-vm-with-opcode-bigrams returns executed opcode bigram counts."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc::run-vm-with-opcode-bigrams
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
    (cl-cc::vm2-reg-set s 0 :x)
    (cl-cc::vm2-reg-set s 1 42)
    (let ((result (cl-cc::run-vm (make-bytecode cl-cc::+op2-make-instance+ 2 'vm2-test-instance 2
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
    (assert-equal expected (cl-cc::run-vm code s))))

(deftest vm2-global-vars-shim
  "vm-global-vars shim returns a hash table; vm2-state pre-populates *features*."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (hash-table-p (cl-cc::vm-global-vars s)))
    (assert-true (not (null (gethash '*features* (cl-cc::vm-global-vars s)))))))
