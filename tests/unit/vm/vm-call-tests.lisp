;;;; tests/unit/vm/vm-call-tests.lisp — vm-call / vm-tail-call / vm-ret tests
;;;
;;; Tests for execute-instruction on vm-call, vm-tail-call, vm-ret,
;;; verifying host-function dispatch, closure frame management, TCO,
;;; and return-value restoration.
;;;
;;; Relies on make-test-vm / exec1 from list-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helper ────────────────────────────────────────────────────────────────

(defun %make-test-closure (entry-label params)
  "Build a bare vm-closure-object with no captures or optional params."
  (make-instance 'cl-cc:vm-closure-object
                 :entry-label entry-label
                 :params params))

(defun %labels (&rest kv-pairs)
  "Build a string-keyed labels hash table from alternating label/pc pairs."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label pc) on kv-pairs by #'cddr
          do (setf (gethash label ht) pc))
    ht))

;;; ─── Section 1: vm-call with host (CL lambda) ─────────────────────────────

(deftest vm-call-host-fn-result-written-to-dst
  "vm-call with a host CL function stores the result in :dst."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (x) (* x 2)))
    (cl-cc:vm-reg-set s :R2 21)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 0 (%labels))
    (assert-= 42 (cl-cc:vm-reg-get s :R0))))

(deftest vm-call-host-fn-advances-pc
  "vm-call with a host function returns pc+1 (no jump)."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda () 99))
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-call :dst :R0 :func :R1 :args nil) s 7 (%labels))
      (assert-= 8 new-pc)
      (assert-null sig)
      (assert-null ret))))

(deftest vm-call-host-fn-no-frame-pushed
  "vm-call with a host function does not push a call frame."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R1 (lambda (x y) (+ x y)))
    (cl-cc:vm-reg-set s :R2 10)
    (cl-cc:vm-reg-set s :R3 32)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2 :R3)) s 0 (%labels))
    (assert-null (cl-cc:vm-call-stack s))))

;;; ─── Section 2: vm-call with vm-closure-object ────────────────────────────

(deftest vm-call-closure-jumps-to-entry-label
  "vm-call with a closure returns the pc mapped to the closure's entry label."
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
      (assert-null ret))))

(deftest vm-call-closure-pushes-one-frame
  "vm-call with a closure pushes exactly one entry onto vm-call-stack."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R2)))
         (lbl (%labels "fn_entry" 10)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 0)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    (assert-= 1 (length (cl-cc:vm-call-stack s)))))

(deftest vm-call-closure-frame-return-pc-is-pc-plus-one
  "The frame pushed by vm-call records return-pc as caller pc+1."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R2)))
         (lbl (%labels "fn_entry" 10)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 0)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    ;; Frame is (return-pc dst-reg old-closure-env saved-regs)
    (assert-= 6 (first (first (cl-cc:vm-call-stack s))))))

(deftest vm-call-closure-binds-args-to-params
  "vm-call copies argument register values into the closure's parameter registers."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_entry" '(:R3)))
         (lbl (%labels "fn_entry" 10)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 77)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    (assert-= 77 (cl-cc:vm-reg-get s :R3))))

;;; ─── Section 3: vm-tail-call — TCO ───────────────────────────────────────

(deftest vm-tail-call-does-not-push-frame
  "vm-tail-call with a closure does NOT push a call frame (TCO)."
  (let* ((s (make-test-vm))
         (cl (%make-test-closure "fn_tail" '(:R2)))
         (lbl (%labels "fn_tail" 20)))
    (cl-cc:vm-reg-set s :R1 cl)
    (cl-cc:vm-reg-set s :R2 55)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args '(:R2)) s 5 lbl)
    (assert-null (cl-cc:vm-call-stack s))))

(deftest vm-tail-call-jumps-to-entry-label
  "vm-tail-call returns the entry-label pc just like vm-call."
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

;;; ─── Section 4: vm-ret ────────────────────────────────────────────────────

(deftest vm-ret-empty-stack-signals-halt
  "vm-ret with an empty call stack signals halt: (values nil t result)."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s :R0 42)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-ret :reg :R0) s 0 (%labels))
      (assert-null new-pc)
      (assert-true sig)
      (assert-= 42 ret))))

(deftest vm-ret-non-empty-stack-returns-to-saved-pc
  "vm-ret with a frame on the stack returns to the saved return-pc."
  (let ((s (make-test-vm)))
    ;; Push a frame: return to pc 8, result into :R4
    (cl-cc::vm-push-call-frame s 8 :R4)
    (push nil (cl-cc:vm-method-call-stack s))
    (cl-cc:vm-reg-set s :R5 777)
    (multiple-value-bind (new-pc sig ret)
        (cl-cc:execute-instruction
         (cl-cc:make-vm-ret :reg :R5) s 5 (%labels))
      (assert-= 8 new-pc)
      (assert-null sig)
      (assert-null ret))))

(deftest vm-ret-writes-result-to-dst-reg
  "vm-ret pops the frame and writes the return value into the saved dst register."
  (let ((s (make-test-vm)))
    (cl-cc::vm-push-call-frame s 8 :R4)
    (push nil (cl-cc:vm-method-call-stack s))
    (cl-cc:vm-reg-set s :R5 999)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-ret :reg :R5) s 5 (%labels))
    (assert-= 999 (cl-cc:vm-reg-get s :R4))))

(deftest vm-ret-pops-frame-from-stack
  "vm-ret removes the top frame from vm-call-stack."
  (let ((s (make-test-vm)))
    (cl-cc::vm-push-call-frame s 8 :R4)
    (push nil (cl-cc:vm-method-call-stack s))
    (cl-cc:vm-reg-set s :R0 0)
    (cl-cc:execute-instruction
     (cl-cc:make-vm-ret :reg :R0) s 5 (%labels))
    (assert-null (cl-cc:vm-call-stack s))))
