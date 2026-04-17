;;;; tests/unit/vm/vm-execute-tests.lisp — Core VM Execute-Instruction Tests
;;;;
;;;; Tests for src/vm/vm-execute.lisp:
;;;; vm-falsep, vm-classify-arg, vm-save/restore-registers,
;;;; vm-list-to-lisp-list, and execute-instruction for const/move/halt/jump-zero.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest vm-execute-save-registers-snapshot
  "vm-save-registers returns a copy — later mutations don't affect the snapshot."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 99)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      ;; Mutate state after saving
      (cl-cc:vm-reg-set s :R0 200)
      ;; Snapshot still has the old value
      (assert-= 99 (gethash :R0 snap)))))

(deftest vm-execute-restore-registers-overwrites
  "vm-restore-registers replaces the register file with the saved snapshot."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (let ((snap (cl-cc/vm::vm-save-registers s)))
      (cl-cc:vm-reg-set s :R0 999)
      (cl-cc/vm::vm-restore-registers s snap)
      (assert-= 42 (cl-cc:vm-reg-get s :R0)))))

(deftest vm-execute-save-registers-subset
  "vm-save-registers-subset captures only the requested registers."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc:vm-reg-set s :R1 99)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (assert-false (gethash :R0 snap))
      (assert-= 99 (gethash :R1 snap)))))

(deftest vm-execute-restore-registers-subset
  "vm-restore-registers-subset updates only the saved bindings."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 1)
    (cl-cc:vm-reg-set s :R1 2)
    (let ((snap (cl-cc/vm::vm-save-registers-subset s '(:R1))))
      (cl-cc:vm-reg-set s :R0 10)
      (cl-cc:vm-reg-set s :R1 20)
      (cl-cc/vm::vm-restore-registers-subset s snap)
      (assert-= 10 (cl-cc:vm-reg-get s :R0))
      (assert-= 2 (cl-cc:vm-reg-get s :R1)))))

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
         (cl-cc::make-vm-jump-zero :reg :R0 :label label) s current-pc lbls)
      (declare (ignore halt-p))
      (assert-= expected-pc next-pc))))

(deftest vm-execute-vm-label-advances-pc
  "vm-label is a no-op instruction that just increments pc."
  (let ((s (make-test-vm)))
    (multiple-value-bind (next-pc halt-p result)
        (cl-cc/vm::execute-instruction
         (cl-cc::make-vm-label :name "entry") s 4 (make-hash-table :test #'equal))
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
         (cl-cc::make-vm-jump :label "entry") s 2 lbls)
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
         (cl-cc::make-vm-func-ref :dst :R0 :label "1+") s 7 labels)
      (declare (ignore result))
      (assert-= 8 next-pc)
      (assert-false halt-p))
    (let ((fn (cl-cc:vm-reg-get s :R0)))
      (assert-true (functionp fn))
      (assert-= 42 (funcall fn 41)))))

(deftest vm-execute-vm-values-stores-all-values
  "vm-values stores the primary value in dst and all values in values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 10)
    (cl-cc:vm-reg-set s :R2 20)
    (cl-cc:vm-reg-set s :R3 30)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-values :dst :R0 :src-regs (list :R1 :R2 :R3)) s 0 (make-hash-table :test #'equal))
    (assert-= 10 (cl-cc:vm-reg-get s :R0))
    (assert-equal '(10 20 30) (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-vm-mv-bind-distributes-values
  "vm-mv-bind distributes values-list to destination registers."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-mv-bind :dst-regs (list :R0 :R1 :R2)) s 0 (make-hash-table :test #'equal))
    (assert-= 1 (cl-cc:vm-reg-get s :R0))
    (assert-= 2 (cl-cc:vm-reg-get s :R1))
    (assert-= 3 (cl-cc:vm-reg-get s :R2))))

(deftest vm-execute-vm-values-to-list-copies-list
  "vm-values-to-list copies values-list into a register."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(7 8 9))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-values-to-list :dst :R0) s 0 (make-hash-table :test #'equal))
    (assert-equal '(7 8 9) (cl-cc:vm-reg-get s :R0))))

(deftest vm-execute-vm-spread-values-roundtrips
  "vm-spread-values loads values-list from a register holding a list."
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

(deftest vm-execute-vm-clear-values-resets-buffer
  "vm-clear-values resets values-list to nil."
  (let ((s (make-test-vm)))
    (setf (cl-cc/vm::vm-values-list s) '(1 2 3))
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-clear-values) s 0 (make-hash-table :test #'equal))
    (assert-null (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-vm-ensure-values-initialises-buffer
  "vm-ensure-values initialises values-list from src when nil."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 55)
    (setf (cl-cc/vm::vm-values-list s) nil)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-ensure-values :src :R0) s 0 (make-hash-table :test #'equal))
    (assert-equal '(55) (cl-cc/vm::vm-values-list s))))

(deftest vm-execute-global-operations
  "vm-set-global/vm-get-global round-trip global variables."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-set-global :name 'myvar :src :R0) s 0 (make-hash-table :test #'equal))
    (assert-= 42 (gethash 'myvar (cl-cc/vm::vm-global-vars s))))
  (let ((s (make-test-vm)))
    (setf (gethash 'myvar2 (cl-cc/vm::vm-global-vars s)) 99)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-get-global :dst :R0 :name 'myvar2) s 0 (make-hash-table :test #'equal))
    (assert-= 99 (cl-cc:vm-reg-get s :R0))))

(deftest vm-execute-print-writes-stream
  "vm-print writes the register value followed by newline to output-stream."
  (let* ((str (make-string-output-stream))
         (s   (make-instance 'cl-cc/vm::vm-state :output-stream str)))
    (cl-cc:vm-reg-set s :R0 42)
    (cl-cc/vm::execute-instruction
     (cl-cc::make-vm-print :reg :R0) s 0 (make-hash-table :test #'equal))
    (assert-string= (format nil "42~%") (get-output-stream-string str))))

(deftest vm-execute-register-function-stores-closure
  "vm-register-function stores a closure in the function registry."
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

;;; ─── vm-call / vm-tail-call / vm-ret ───────────────────────────────────────

(defun %make-test-closure (entry-label params)
  "Build a bare vm-closure-object with no captures or optional params."
  (make-instance 'cl-cc:vm-closure-object
                 :entry-label entry-label
                 :params params))

(defun %labels (&rest kv-pairs)
  "Build a labels hash table from alternating label/pc pairs.
Uses vm-label-table-store so the internal sxhash-bucketed layout matches
what vm-label-table-lookup expects — raw (setf gethash) produces entries
that lookup can't find."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label pc) on kv-pairs by #'cddr
          do (cl-cc/vm::vm-label-table-store ht label pc))
    ht))

(deftest vm-call-host-fn-result-written-to-dst
  "vm-call with a host CL function stores the result in :dst."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (x) (* x 2)))
    (cl-cc:vm-reg-set s :R2 21)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 0 (%labels))
    (assert-= 42 (cl-cc:vm-reg-get s :R0))))

(deftest vm-call-host-fn-advances-pc-and-no-frame
  "vm-call with a host function returns pc+1 (no jump) and does not push a call frame."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda () 99))
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s 7 (%labels))
      (assert-= 8 new-pc)
      (assert-null sig)
      (assert-null ret)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (x y) (+ x y)))
    (cl-cc:vm-reg-set s :R2 10)
    (cl-cc:vm-reg-set s :R3 32)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2 :R3)) s 0 (%labels))
    (assert-null (cl-cc:vm-call-stack s))))

(deftest vm-call-closure-behavior
  "vm-call with a closure: jumps to entry label, pushes one frame with return-pc=caller+1, and binds args to params."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R2)))
         (lbl (%labels "fn_entry" 42)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 0)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
      (assert-= 42 new-pc)
      (assert-null sig)
      (assert-null ret)))
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R2)))
         (lbl (%labels "fn_entry" 10)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 0)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    ;; Frame is (return-pc dst-reg old-closure-env saved-regs)
    (assert-= 1 (length (cl-cc:vm-call-stack s)))
    (assert-= 6 (first (first (cl-cc:vm-call-stack s)))))
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R3)))
         (lbl (%labels "fn_entry" 10)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 77)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    (assert-= 77 (cl-cc:vm-reg-get s :R3))))

(deftest vm-tail-call-closure-tco-behavior
  "vm-tail-call with a closure: does NOT push a call frame (TCO) and jumps to entry label."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_tail" '(:R2)))
         (lbl (%labels "fn_tail" 20)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 55)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    (assert-null (cl-cc:vm-call-stack s)))
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_tail" '(:R2)))
         (lbl (%labels "fn_tail" 99)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 1)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
      (assert-= 99 new-pc)
      (assert-null sig)
      (assert-null ret))))

(deftest vm-tail-call-host-fn-same-as-vm-call
  "vm-tail-call with a host function applies it directly, same as vm-call."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (n) (+ n 1)))
    (cl-cc:vm-reg-set s :R2 41)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args '(:R2)) s 0 (%labels))
    (assert-= 42 (cl-cc:vm-reg-get s :R0))))

(deftest vm-ret-behavior
  "vm-ret: empty stack signals halt; non-empty stack returns to saved pc, writes result, pops frame."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-ret :reg :R0) s 0 (%labels))
      (assert-null new-pc)
      (assert-true sig)
      (assert-= 42 ret)))
  (let ((s (make-test-vm)))
    ;; Push a frame: return to pc 8, result into :R4
    (cl-cc/vm::vm-push-call-frame s 8 :R4)
    (push nil (cl-cc:vm-method-call-stack s))
    (cl-cc:vm-reg-set s :R5 777)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-ret :reg :R5) s 5 (%labels))
      (assert-= 8 new-pc)
      (assert-null sig)
      (assert-null ret)
      (assert-= 777 (cl-cc:vm-reg-get s :R4))
      (assert-null (cl-cc:vm-call-stack s)))))
