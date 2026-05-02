;;;; tests/unit/vm/vm-run-tests.lisp — VM error dispatch tests

(in-package :cl-cc/test)

(defsuite vm-run-suite
  :description "Unit tests for vm-run.lisp error matching"
  :parent cl-cc-unit-suite)

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
  (let ((actual (cl-cc/vm::vm-error-type-matches-p error-val handler-type)))
    (if expected-result
        (assert-true actual)
        (assert-false actual))))

(deftest build-label-table-uses-integer-keyed-buckets
  "build-label-table keeps string labels working while using integer outer keys."
  (let* ((instructions (list (cl-cc:make-vm-label :name "entry")
                             (cl-cc:make-vm-const :dst :r0 :value 1)
                             (cl-cc:make-vm-label :name :done)
                             (cl-cc:make-vm-halt :reg :r0)))
         (labels (cl-cc/vm::build-label-table instructions)))
    ;; SBCL's HASH-TABLE-TEST returns the symbol 'EQL, not the function object.
    (assert-eq 'eql (hash-table-test labels))
    (assert-= 0 (cl-cc/vm::vm-label-table-lookup labels "entry"))
    (assert-= 2 (cl-cc/vm::vm-label-table-lookup labels :done))))

(deftest vm-jump-uses-label-table-lookup
  "vm-jump resolves labels through the integer-keyed table produced by build-label-table."
  (let* ((instructions (list (cl-cc:make-vm-jump :label "target")
                             (cl-cc:make-vm-label :name "target")
                             (cl-cc:make-vm-halt :reg :r0)))
         (labels (cl-cc/vm::build-label-table instructions))
         (state (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction (first instructions) state 0 labels)
      (declare (ignore result))
      (assert-= 1 next-pc)
      (assert-false halt-p))))

;;; ─── VM2 defopcode / run-vm tests ───────────────────────────────────────────

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

(deftest vm2-opcode-registration
  "defopcode: each opcode has a numeric id, dispatch handler, name entry, and encoder mapping."
  (assert-true (numberp cl-cc:+op2-const+))
  (assert-true (not (null (aref cl-cc/vm::*opcode-dispatch-table* cl-cc:+op2-const+))))
  (assert-true (not (null (aref cl-cc/vm::*opcode-dispatch-table* cl-cc:+op2-add2+))))
  (assert-equal 'cl-cc:add2 (aref cl-cc/vm::*opcode-name-table* cl-cc:+op2-add2+))
  (assert-= cl-cc:+op2-const+ (gethash 'cl-cc:const cl-cc/vm::*opcode-encoder-table*))
  (assert-= cl-cc:+op2-move+  (gethash 'cl-cc:move  cl-cc/vm::*opcode-encoder-table*))
  (assert-= cl-cc:+op2-add2+  (gethash 'cl-cc:add2  cl-cc/vm::*opcode-encoder-table*))
  (assert-= cl-cc:+op2-add-imm2+ (gethash 'cl-cc:add-imm2 cl-cc/vm::*opcode-encoder-table*)))

(deftest vm2-opcode-distinct-values
  "Each defopcode gets a unique opcode number."
  (let ((ops (list cl-cc:+op2-const+
                   cl-cc:+op2-move+
                   cl-cc:+op2-add2+
                   cl-cc:+op2-add-imm2+
                   cl-cc:+op2-sub2+
                   cl-cc:+op2-sub-imm2+
                   cl-cc:+op2-mul2+
                   cl-cc:+op2-mul-imm2+
                   cl-cc:+op2-num-eq-imm2+
                   cl-cc:+op2-num-lt-imm2+
                   cl-cc:+op2-num-gt-imm2+
                   cl-cc:+op2-num-le-imm2+
                   cl-cc:+op2-num-ge-imm2+
                   cl-cc:+op2-halt2+)))
    (assert-= (length ops) (length (remove-duplicates ops)))))

(deftest vm2-state-structure
  "make-vm2-state: struct predicate, 256-register vector (all nil), :output-stream kwarg, *features* populated."
  (let ((s (cl-cc:make-vm2-state)))
    (assert-true (cl-cc:vm2-state-p s))
    (assert-true (simple-vector-p (cl-cc:vm2-state-registers s)))
    (assert-= 256 (length (cl-cc:vm2-state-registers s)))
    (dotimes (i 256)
      (assert-true (null (cl-cc/vm::vm2-reg-get s i))))
    (assert-null (cl-cc:vm2-state-values-buffer s))
    (assert-true (not (null (gethash '*features* (cl-cc:vm2-state-global-vars s))))))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc:make-vm2-state :output-stream str)))
    (assert-equal str (cl-cc:vm2-state-output-stream s))))

(deftest vm2-reg-operations
  "vm2-reg-set/get: stores, retrieves, returns value, overwrites; all 256 slots independent."
  (let ((s (cl-cc:make-vm2-state)))
    (cl-cc/vm::vm2-reg-set s 0 42)
    (assert-= 42 (cl-cc/vm::vm2-reg-get s 0))
    ;; set returns the written value
    (assert-= 99 (cl-cc/vm::vm2-reg-set s 5 99))
    ;; overwrite
    (cl-cc/vm::vm2-reg-set s 3 100)
    (cl-cc/vm::vm2-reg-set s 3 200)
    (assert-= 200 (cl-cc/vm::vm2-reg-get s 3))
    ;; all 256 slots are independent
    (dotimes (i 256)
      (cl-cc/vm::vm2-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc/vm::vm2-reg-get s i)))))
