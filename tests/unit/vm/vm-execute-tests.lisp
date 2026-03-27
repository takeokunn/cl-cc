;;;; tests/unit/vm/vm-execute-tests.lisp — Core VM Execute-Instruction Tests
;;;;
;;;; Tests for src/vm/vm-execute.lisp:
;;;; vm-falsep, vm-classify-arg, vm-save/restore-registers,
;;;; vm-list-to-lisp-list, and execute-instruction for const/move/halt/jump-zero.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── vm-falsep ──────────────────────────────────────────────────────────────

(deftest-each vm-execute-vm-falsep-falsy-values
  "vm-falsep returns T for nil, 0, and false."
  :cases (("nil"   nil)
          ("zero"  0)
          ("false" nil))
  (val)
  (assert-true (cl-cc::vm-falsep val)))

(deftest-each vm-execute-vm-falsep-truthy-values
  "vm-falsep returns NIL for non-zero integers, strings, symbols, and lists."
  :cases (("positive-int"  1)
          ("negative-int"  -1)
          ("string"        "hello")
          ("symbol"        'foo)
          ("t"             t)
          ("list"          '(1 2 3)))
  (val)
  (assert-false (cl-cc::vm-falsep val)))

;;; ─── vm-classify-arg ────────────────────────────────────────────────────────

(deftest-each vm-execute-vm-classify-arg-primitives
  "vm-classify-arg identifies primitive CL types for single dispatch."
  :cases (("integer" 42      'integer)
          ("string"  "hello" 'string)
          ("symbol"  'foo    'symbol))
  (arg expected)
  (let ((s (make-test-vm)))
    (assert-eq expected (cl-cc::vm-classify-arg arg s))))

(deftest vm-execute-vm-classify-arg-unknown
  "vm-classify-arg returns T for lists and other unrecognised values."
  (let ((s (make-test-vm)))
    (assert-eq t (cl-cc::vm-classify-arg '(a b c) s))
    (assert-eq t (cl-cc::vm-classify-arg 3.14 s))))

(deftest vm-execute-vm-classify-arg-plain-hash-table
  "vm-classify-arg returns T for a plain hash-table with no :__class__ key."
  (let ((s (make-test-vm))
        (ht (make-hash-table)))
    (assert-eq t (cl-cc::vm-classify-arg ht s))))

;;; ─── vm-save-registers / vm-restore-registers ───────────────────────────────

(deftest vm-execute-save-registers-snapshot
  "vm-save-registers returns a copy — later mutations don't affect the snapshot."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 99)
    (let ((snap (cl-cc::vm-save-registers s)))
      ;; Mutate state after saving
      (cl-cc:vm-reg-set s :R0 200)
      ;; Snapshot still has the old value
      (assert-= 99 (gethash :R0 snap)))))

(deftest vm-execute-restore-registers-overwrites
  "vm-restore-registers replaces the register file with the saved snapshot."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (let ((snap (cl-cc::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 999)
      (cl-cc::vm-restore-registers s snap)
      (assert-= 42 (cl-cc:vm-reg-get s :R0)))))

;;; ─── vm-list-to-lisp-list ───────────────────────────────────────────────────

(deftest-each vm-execute-vm-list-to-lisp-list
  "vm-list-to-lisp-list converts VM list values to proper Lisp lists."
  :cases (("nil"        nil       nil)
          ("proper-list" '(1 2 3) '(1 2 3))
          ("atom-wraps" 42        '(42))
          ("string-wraps" "hi"   '("hi")))
  (input expected)
  (let ((s (make-test-vm)))
    (assert-equal expected (cl-cc::vm-list-to-lisp-list s input))))

;;; ─── execute-instruction: vm-const ──────────────────────────────────────────

(deftest-each vm-execute-vm-const-loads-values
  "vm-const loads the immediate value into the destination register."
  :cases (("integer" :R0 42  42)
          ("nil"     :R1 nil nil)
          ("string"  :R2 "x" "x")
          ("neg"     :R3 -7  -7))
  (dst val expected)
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-const :dst dst :value val) s)
    (assert-equal expected (cl-cc:vm-reg-get s dst))))

(deftest vm-execute-vm-const-advances-pc
  "vm-const returns next-pc = 1 (pc+1), not halted."
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc::make-vm-const :dst :R0 :value 0) s)
      (declare (ignore result))
      (assert-= 1 next-pc)
      (assert-false halt-p))))

;;; ─── execute-instruction: vm-move ───────────────────────────────────────────

(deftest vm-execute-vm-move-copies-register
  "vm-move copies src register value into dst register."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 'hello)
    (exec1 (cl-cc::make-vm-move :dst :R0 :src :R1) s)
    (assert-eq 'hello (cl-cc:vm-reg-get s :R0))))

;;; ─── execute-instruction: vm-halt ───────────────────────────────────────────

(deftest vm-execute-vm-halt-signals-halt
  "vm-halt returns (values nil t result-value)."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 'done)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc::make-vm-halt :reg :R0) s)
      (assert-null next-pc)
      (assert-true halt-p)
      (assert-eq 'done result))))

;;; ─── execute-instruction: vm-jump-zero ──────────────────────────────────────

(deftest vm-execute-vm-jump-zero-taken-when-false
  "vm-jump-zero jumps when register holds nil (falsy)."
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'equal)))
    (setf (gethash "target" lbls) 99)
    (cl-cc:vm-reg-set s :R0 nil)
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc:execute-instruction
         (cl-cc::make-vm-jump-zero :reg :R0 :label "target") s 0 lbls)
      (declare (ignore result))
      (assert-= 99 next-pc)
      (assert-false halt-p))))

(deftest vm-execute-vm-jump-zero-not-taken-when-true
  "vm-jump-zero falls through when register holds a truthy value."
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'equal)))
    (setf (gethash "target" lbls) 99)
    (cl-cc:vm-reg-set s :R0 1)
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc:execute-instruction
         (cl-cc::make-vm-jump-zero :reg :R0 :label "target") s 5 lbls)
      (declare (ignore result))
      (assert-= 6 next-pc)
      (assert-false halt-p))))

(deftest vm-execute-vm-jump-zero-taken-when-zero
  "vm-jump-zero jumps when register holds 0 (falsy integer)."
  (let ((s (make-test-vm))
        (lbls (make-hash-table :test #'equal)))
    (setf (gethash "loop" lbls) 3)
    (cl-cc:vm-reg-set s :R1 0)
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc:execute-instruction
         (cl-cc::make-vm-jump-zero :reg :R1 :label "loop") s 10 lbls)
      (declare (ignore halt-p result))
      (assert-= 3 next-pc))))
