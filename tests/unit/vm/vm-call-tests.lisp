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

;;; ─── Section 2: vm-call with vm-closure-object ────────────────────────────

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

;;; ─── Section 3: vm-tail-call — TCO ───────────────────────────────────────

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

;;; ─── Section 4: vm-ret ────────────────────────────────────────────────────

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
    (cl-cc::vm-push-call-frame s 8 :R4)
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
