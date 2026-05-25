;;;; tests/unit/vm/vm-call-tests.lisp — vm-call / vm-tail-call / vm-ret Tests
;;;;
;;;; Tests for call/tail-call/ret instruction behavior in src/vm/vm-execute.lisp.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest vm-call-host-fn-behavior
  "vm-call with host CL function: stores result in dst; advances pc+1; does not push call frame."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (x) (* x 2)))
    (cl-cc:vm-reg-set s :R2 21)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 0 (%labels))
    (assert-= 42 (cl-cc:vm-reg-get s :R0)))
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

(deftest vm-tail-call-behavior
  "vm-tail-call with closure: no frame pushed (TCO), jumps to label; with host fn: applies directly."
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
      (assert-null ret)))
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (n) (+ n 1)))
    (cl-cc:vm-reg-set s :R2 41)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args '(:R2)) s 0 (%labels))
    (assert-= 42 (cl-cc:vm-reg-get s :R0))))

(deftest vm-call/cc-basic-escape
  "FR-800/801: vm-call/cc captures the VM continuation and resumes with the invoked value."
  (let* ((receiver (%make-test-closure "receive-k" '(:k)))
         (program (cl-cc/vm::make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-const :dst :fn :value receiver)
                         (cl-cc:make-vm-call/cc :dst :result :func :fn)
                         (cl-cc:make-vm-halt :reg :result)
                         (cl-cc:make-vm-label :name "receive-k")
                         (cl-cc:make-vm-const :dst :value :value 42)
                         (cl-cc:make-vm-call :dst :ignored :func :k :args '(:value))
                         (cl-cc:make-vm-const :dst :never :value :unreachable)
                         (cl-cc:make-vm-ret :reg :never))
                   :result-register :result)))
    (assert-= 42 (cl-cc:run-compiled program :state (make-test-vm)))))

(deftest vm-call/cc-non-local-return
  "FR-800/801: invoking a captured continuation unwinds nested VM calls."
  (let* ((receiver (%make-test-closure "receive-k-non-local" '(:k)))
         (helper (%make-test-closure "invoke-k-from-helper" '(:k2)))
         (program (cl-cc/vm::make-vm-program
                   :instructions
                   (list (cl-cc:make-vm-const :dst :fn :value receiver)
                         (cl-cc:make-vm-call/cc :dst :result :func :fn)
                         (cl-cc:make-vm-halt :reg :result)
                         (cl-cc:make-vm-label :name "receive-k-non-local")
                         (cl-cc:make-vm-const :dst :helper :value helper)
                         (cl-cc:make-vm-call :dst :tmp :func :helper :args '(:k))
                         (cl-cc:make-vm-const :dst :normal-return :value 99)
                         (cl-cc:make-vm-ret :reg :normal-return)
                         (cl-cc:make-vm-label :name "invoke-k-from-helper")
                         (cl-cc:make-vm-const :dst :escape-value :value 77)
                         (cl-cc:make-vm-call :dst :ignored :func :k2 :args '(:escape-value))
                         (cl-cc:make-vm-const :dst :never :value -1)
                         (cl-cc:make-vm-ret :reg :never))
                   :result-register :result)))
    (assert-= 77 (cl-cc:run-compiled program :state (make-test-vm)))))

(deftest vm-continuation-multi-shot-invocation
  "FR-800: full continuations are multi-shot and restore fresh control-state copies."
  (let* ((state (make-test-vm))
         (labels (%labels "resume" 12)))
    (cl-cc:vm-reg-set state :saved :original)
    (cl-cc/vm::vm-push-call-frame state 9 :return)
    (let ((continuation (cl-cc:vm-capture-continuation state 12 :result :labels labels)))
      (cl-cc:vm-reg-set state :saved :mutated)
      (setf (cl-cc:vm-call-stack state) (list :different-stack))
      (assert-= 12 (cl-cc:vm-invoke-continuation state continuation 10))
      (assert-= 10 (cl-cc:vm-reg-get state :result))
      (assert-eq :original (cl-cc:vm-reg-get state :saved))
      (assert-= 1 (length (cl-cc:vm-call-stack state)))
      (cl-cc:vm-reg-set state :saved :mutated-again)
      (setf (cl-cc:vm-call-stack state) nil)
      (assert-= 12 (cl-cc:vm-invoke-continuation state continuation 20))
      (assert-= 20 (cl-cc:vm-reg-get state :result))
      (assert-eq :original (cl-cc:vm-reg-get state :saved))
      (assert-= 1 (length (cl-cc:vm-call-stack state))))))

(deftest vm-continuation-restores-dynamic-extent-stacks
  "FR-800: continuation invocation restores handler/method/prompt dynamic-extent stacks."
  (let ((state (make-test-vm)))
    (setf (cl-cc:vm-handler-stack state) (list (list :handler :result 'error nil (cl-cc/vm::vm-save-registers state)))
          (cl-cc:vm-method-call-stack state) (list :method-frame)
          (cl-cc/vm::vm-continuation-prompts state) (list (list :name :prompt :pc 5 :dst-reg :result)))
    (let ((continuation (cl-cc:vm-capture-continuation state 8 :result)))
      (setf (cl-cc:vm-handler-stack state) nil
            (cl-cc:vm-method-call-stack state) nil
            (cl-cc/vm::vm-continuation-prompts state) nil)
      (assert-= 8 (cl-cc:vm-invoke-continuation state continuation :done))
      (assert-equal :done (cl-cc:vm-reg-get state :result))
      (assert-= 1 (length (cl-cc:vm-handler-stack state)))
      (assert-equal '(:method-frame) (cl-cc:vm-method-call-stack state))
      (assert-equal :prompt (getf (first (cl-cc/vm::vm-continuation-prompts state)) :name)))))

(deftest vm-apply-tail-p-closure-skips-frame-push
  "vm-apply with tail-p on a closure spreads args and reuses the current frame."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_apply_tail" '(:R2 :R3)))
         (lbl (%labels "fn_apply_tail" 33)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 10)
    (cl-cc:vm-reg-set s :R4 '(20))
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-apply :dst :R0 :func :R1 :args '(:R2 :R4) :tail-p t)
         s 5 lbl)
      (assert-= 33 new-pc)
      (assert-null sig)
      (assert-null ret))
    (assert-null (cl-cc:vm-call-stack s))
    (assert-= 10 (cl-cc:vm-reg-get s :R2))
    (assert-= 20 (cl-cc:vm-reg-get s :R3))))

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
