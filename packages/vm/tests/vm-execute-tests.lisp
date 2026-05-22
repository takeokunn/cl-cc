;;;; tests/unit/vm/vm-execute-tests.lisp — Core VM Execute-Instruction Tests
;;;;
;;;; Tests for src/vm/vm-execute.lisp:
;;;; vm-falsep, vm-classify-arg, vm-save/restore-registers,
;;;; vm-list-to-lisp-list, and execute-instruction for const/move/halt/jump-zero.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest vm-execute-safety-level-bounds-checking
  "Safety level 1 signals TYPE-ERROR for invalid string/array indexes; safety 0 skips explicit checks."
  (let ((cl-cc/vm::*safety-level* 1))
    (assert-signals type-error (cl-cc/vm::vm-check-index "abc" 3 'char)))
  (let ((cl-cc/vm::*safety-level* 0))
    (assert-equal 99 (cl-cc/vm::vm-check-index "abc" 99 'char))))

(deftest vm-execute-stack-canary-verification
  "Stack canary mismatch signals MEMORY-FAULT when canaries are enabled."
  (let ((cl-cc/vm::*security-canaries* t)
        (snapshot (make-hash-table :test #'eq)))
    (setf (gethash :__stack-canary__ snapshot) 1
          (gethash :__stack-canary-check__ snapshot) 2)
    (assert-signals cl-cc/vm::memory-fault
      (cl-cc/vm::vm-verify-stack-canary snapshot))))

(deftest vm-execute-cow-array-make-and-adjust
  "make-array supports copy-on-write wrappers and adjust-array resolves them."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 3)
    (cl-cc:vm-reg-set s :R2 9)
    (exec1 (cl-cc:make-vm-make-array :dst :R0 :size-reg :R1 :initial-element :R2
                                      :adjustable t :copy-on-write t) s)
    (assert-true (cl-cc/vm::vm-cow-vector-p (cl-cc:vm-reg-get s :R0)))
    (cl-cc:vm-reg-set s :R3 5)
    (exec1 (cl-cc:make-vm-adjust-array :dst :R4 :arr :R0 :dims :R3 :initial-element nil) s)
    (assert-equal 5 (array-total-size (cl-cc:vm-reg-get s :R4)))))

;;; ─── vm-falsep ──────────────────────────────────────────────────────────────

(deftest vm-execute-vm-falsep-semantics
  "vm-falsep returns T for nil and numeric zero, and NIL for other truthy values."
  (assert-true (cl-cc/vm::vm-falsep nil))
  (assert-true (cl-cc/vm::vm-falsep 0))
  (dolist (val '(1 -1 "hello" foo t (1 2 3)))
    (assert-false (cl-cc/vm::vm-falsep val))))

;;; ─── vm-classify-arg ────────────────────────────────────────────────────────

(deftest-each vm-execute-vm-classify-arg
  "vm-classify-arg identifies CL primitive types; returns T for unknown/composite values."
  :cases (("integer"    42              'integer)
          ("string"     "hello"         'string)
          ("symbol"     'foo            'symbol)
          ("list"       '(a b c)        't)
          ("float"      3.14            't)
          ("hash-table" (make-hash-table) 't))
  (arg expected)
  (let ((s (make-test-vm)))
    (assert-eq expected (cl-cc/vm::vm-classify-arg arg s))))

;;; ─── vm-save-registers / vm-restore-registers ───────────────────────────────

(deftest vm-execute-register-save-restore
  "save-registers snapshots (immutable copy); restore-registers restores; subset variants scope correctly."
  (let ((s (make-test-vm)))
    ;; Full save: snapshot is immutable
    (cl-cc:vm-reg-set s :R0 99)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 200)
      (assert-= 99 (gethash :R0 snap)))
    ;; Full restore: overwrites
    (cl-cc:vm-reg-set s :R0 42)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 999)
      (cl-cc/vm::vm-restore-registers s snap)
      (assert-= 42 (cl-cc:vm-reg-get s :R0)))
    ;; Subset save: only saves :R1
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc:vm-reg-set s :R1 99)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (assert-false (gethash :R0 snap))
      (assert-= 99 (gethash :R1 snap)))
    ;; Subset restore: only restores :R1, leaves :R0 as-is
    (cl-cc:vm-reg-set s :R0 1)
    (cl-cc:vm-reg-set s :R1 2)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (cl-cc:vm-reg-set s :R0 10)
      (cl-cc:vm-reg-set s :R1 20)
      (cl-cc/vm::vm-restore-registers-subset s snap)
      (assert-= 10 (cl-cc:vm-reg-get s :R0))
      (assert-= 2  (cl-cc:vm-reg-get s :R1)))))

;;; ─── vm-list-to-lisp-list ───────────────────────────────────────────────────

(deftest-each vm-execute-vm-list-to-lisp-list
  "vm-list-to-lisp-list converts VM list values to proper Lisp lists."
  :cases (("nil"        nil       nil)
          ("proper-list" '(1 2 3) '(1 2 3))
          ("atom-wraps" 42        '(42))
          ("string-wraps" "hi"   '("hi")))
  (input expected)
  (let ((s (make-test-vm)))
    (assert-equal expected (cl-cc/vm::vm-list-to-lisp-list s input))))

;;; ─── execute-instruction: vm-const ──────────────────────────────────────────

(deftest-each vm-execute-vm-const-loads-values
  "vm-const loads the immediate value into the destination register."
  :cases (("integer" :R0 42  42)
          ("nil"     :R1 nil nil)
          ("string"  :R2 "x" "x")
          ("neg"     :R3 -7  -7))
  (dst val expected)
  (let ((s (make-test-vm)))
    (exec1 (cl-cc:make-vm-const :dst dst :value val) s)
    (assert-equal expected (cl-cc:vm-reg-get s dst))))

;;; ─── execute-instruction: vm-halt ───────────────────────────────────────────

(deftest vm-execute-pc-advancing
  "vm-const and vm-move advance pc by 1 and load/copy values correctly."
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p)
        (exec1 (cl-cc:make-vm-const :dst :R0 :value 0) s)
      (assert-= 1 next-pc)
      (assert-false halt-p))
    (cl-cc:vm-reg-set s :R1 'hello)
    (exec1 (cl-cc:make-vm-move :dst :R0 :src :R1) s)
    (assert-eq 'hello (cl-cc:vm-reg-get s :R0))))

(deftest vm-execute-halt-signal
  "vm-halt returns nil pc, halt-p=t, and the halting register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 'done)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc:make-vm-halt :reg :R0) s)
      (assert-null next-pc)
      (assert-true halt-p)
      (assert-eq 'done result))))

;;; ─── execute-instruction: vm-jump-zero ──────────────────────────────────────

(deftest-each vm-execute-vm-jump-zero-branching
  "vm-jump-zero jumps when register is falsy (nil or 0); falls through when truthy."
  :cases (("taken-false"    nil 0  "target" 99 99)
          ("not-taken-true" 1   5  "target" 99 6)
          ("taken-zero"     0   10 "loop"   3  3))
  (reg-val current-pc label label-pc expected-pc)
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'eql)))
    (cl-cc/vm::vm-label-table-store lbls label label-pc)
    (cl-cc:vm-reg-set s :R0 reg-val)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-jump-zero :reg :R0 :label label) s current-pc lbls)
      (declare (ignore halt-p))
      (assert-= expected-pc next-pc))))

(deftest vm-execute-vm-label-advances-pc
  "vm-label is a no-op instruction that just increments pc."
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-label :name "entry") s 4 (make-hash-table :test #'equal))
      (declare (ignore result))
      (assert-= 5 next-pc)
      (assert-false halt-p))))

(deftest vm-execute-vm-jump-taken
  "vm-jump transfers control to the target label.
Label tables are integer-keyed with collision buckets — use the store helper
rather than raw gethash to produce a lookupable entry."
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'equal)))
    (cl-cc/vm::vm-label-table-store lbls "entry" 11)
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-jump :label "entry") s 2 lbls)
      (declare (ignore result))
      (assert-= 11 next-pc)
      (assert-false halt-p))))

(deftest vm-execute-vm-func-ref-resolves-cl-host-function
  "vm-func-ref resolves host-bridge function designators from the CL package.

This guards the runtime path used by forms like #'1+ and #'length inside stdlib
expansions, which previously fell back to bogus local closures."
  (let ((s (make-test-vm))
        (labels (make-hash-table :test #'equal)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc:make-vm-func-ref :dst :R0 :label "1+") s 7 labels)
      (declare (ignore result))
      (assert-= 8 next-pc)
      (assert-false halt-p))
    (let ((fn (cl-cc:vm-reg-get s :R0)))
      (assert-true (functionp fn))
      (assert-= 42 (funcall fn 41)))))

(deftest vm-execute-call/cc-restores-copied-stack
  "FR-800: call/cc captures a reusable heap copy of the VM stack and resumes at the saved PC."
  (let* ((program (cl-cc:make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-closure :dst :FN :label "fn" :params '(:K)
                                               :optional-params nil :rest-param nil
                                               :key-params nil :rest-stack-alloc-p nil
                                               :inline-policy nil :dispatch-tag nil :captured nil)
                         (cl-cc:make-vm-call/cc :dst :R0 :func :FN)
                         (cl-cc:make-vm-halt :reg :R0)
                         (cl-cc:make-vm-label :name "fn")
                         (cl-cc:make-vm-const :dst :V :value 42)
                         (cl-cc:make-vm-call :dst :IGNORED :func :K :args '(:V))
                         (cl-cc:make-vm-const :dst :BAD :value 9)
                         (cl-cc:make-vm-ret :reg :BAD))
                   :result-register :R0)))
    (assert-= 42 (cl-cc:run-compiled program))))

(deftest vm-execute-abort-to-prompt-jumps-to-delimiter
  "FR-914: abort-to-prompt unwinds to the named prompt and returns the supplied value."
  (let* ((program (cl-cc:make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-closure :dst :FN :label "fn" :params nil
                                               :optional-params nil :rest-param nil
                                               :key-params nil :rest-stack-alloc-p nil
                                               :inline-policy nil :dispatch-tag nil :captured nil)
                         (cl-cc:make-vm-const :dst :PROMPT :value 'p)
                         (cl-cc:make-vm-call-with-prompt :dst :R0 :func :FN :prompt :PROMPT)
                         (cl-cc:make-vm-halt :reg :R0)
                         (cl-cc:make-vm-label :name "fn")
                         (cl-cc:make-vm-const :dst :P2 :value 'p)
                         (cl-cc:make-vm-const :dst :V :value 77)
                         (cl-cc:make-vm-abort-to-prompt :prompt :P2 :value :V)
                         (cl-cc:make-vm-const :dst :BAD :value 9)
                         (cl-cc:make-vm-ret :reg :BAD))
                   :result-register :R0)))
    (assert-= 77 (cl-cc:run-compiled program))))

(deftest vm-execute-mv-buffer-frame-protocol
  "vm-values populates the fixed MV buffer; vm-nth-value and mv-bind read it in O(1)."
  (let ((s (make-test-vm))
        (labels nil))
    (cl-cc:vm-reg-set s :A 10)
    (cl-cc:vm-reg-set s :B 20)
    (cl-cc:vm-reg-set s :C 30)
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values :dst :R :src-regs '(:A :B :C)) s 0 labels)
    (assert-= 3 (cl-cc/vm::vm-mv-count s))
    (assert-= 20 (aref (cl-cc/vm::vm-mv-buffer s) 1))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-nth-value :dst :N :index 2) s 1 labels)
    (assert-= 30 (cl-cc:vm-reg-get s :N))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-mv-bind :dst-regs '(:X :Y :Z :MISSING)) s 2 labels)
    (assert-= 10 (cl-cc:vm-reg-get s :X))
    (assert-= 20 (cl-cc:vm-reg-get s :Y))
    (assert-= 30 (cl-cc:vm-reg-get s :Z))
    (assert-eq nil (cl-cc:vm-reg-get s :MISSING))))

(deftest vm-execute-empty-values-buffer
  "(values) is represented as mv-count=0 with mv-buffer[0]=NIL."
  (let ((s (make-test-vm)))
    (cl-cc/vm::execute-instruction
     (cl-cc:make-vm-values :dst :R :src-regs nil) s 0 nil)
    (assert-= 0 (cl-cc/vm::vm-mv-count s))
    (assert-eq nil (aref (cl-cc/vm::vm-mv-buffer s) 0))
    (assert-eq nil (cl-cc:vm-reg-get s :R))))
