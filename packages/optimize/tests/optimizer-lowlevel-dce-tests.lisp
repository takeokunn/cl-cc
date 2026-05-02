;;;; tests/integration/optimizer-lowlevel-dce-tests.lisp — DCE and Jump Threading Tests
;;;;
;;;; Continuation of optimizer-lowlevel-tests.lisp.
;;;; Tests for opt-pass-dce (dead code elimination) and opt-pass-jump (jump threading).

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

;;; ── opt-pass-dce: Dead Code Elimination ──────────────────────────────────

(deftest-each dce-pass-cases
  "opt-pass-dce: removes dead consts/moves, keeps used consts and impure calls."
  :cases (("removes-unused-const"
           (lambda ()
             (let* ((i1  (make-vm-const :dst :r0 :value 42))
                    (i2  (make-vm-const :dst :r1 :value 7))
                    (ret (make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) (list i1) (list i2 ret)))))
          ("keeps-const-read-by-add"
           (lambda ()
             (let* ((i1  (make-vm-const :dst :r0 :value 5))
                    (i2  (make-vm-add  :dst :r1 :lhs :r0 :rhs :r0))
                    (ret (make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) nil (list i1 i2)))))
          ("removes-unused-move"
           (lambda ()
             (let* ((c   (make-vm-const :dst :r0 :value 1))
                    (m   (make-vm-move  :dst :r2 :src :r0))
                    (ret (make-vm-ret :reg :r0)))
               (list (list c m ret) (list m) (list c)))))
          ("preserves-impure-call"
           (lambda ()
             (let* ((fn  (make-vm-const :dst :r0 :value 'f))
                    (c   (make-vm-call  :dst :r1 :func :r0 :args nil))
                    (ret (make-vm-ret :reg :r0)))
               (list (list fn c ret) nil (list c))))))
  (make-case)
  (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-dce instrs)))
      (dolist (inst absent)  (assert-false (member inst out)))
      (dolist (inst present) (assert-true  (member inst out))))))

;;; ── opt-pass-jump: Jump Threading ────────────────────────────────────────

(deftest jump-threading-cases
  "opt-pass-jump removes falls-through jump; threads jump-to-jump chain to final target."
  (let* ((j   (make-vm-jump  :label "end"))
         (lbl (make-vm-label :name "end"))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list j lbl ret))))
    (assert-false (member j out))
    (assert-true  (member lbl out))
    (assert-true  (member ret out)))
  (let* ((j1   (make-vm-jump  :label "mid"))
         (lbl1 (make-vm-label :name "mid"))
         (j2   (make-vm-jump  :label "end"))
         (lbl2 (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-jump (list j1 lbl1 j2 lbl2 ret))))
    (let ((j1-out (find-if #'cl-cc:vm-jump-p out)))
      (when j1-out
        (assert-equal (cl-cc/vm::vm-label-name j1-out) "end")))))

(deftest jump-threading-follows-long-acyclic-chain
  "opt-pass-jump fully threads long jump chains that exceed the old recursion cap."
  (let ((instructions nil))
    (loop for i from 0 below 25 do
      (push (make-vm-jump :label (format nil "L~D" (1+ i))) instructions)
      (push (make-vm-label :name (format nil "L~D" (1+ i))) instructions))
    (push (make-vm-ret :reg :r0) instructions)
    (let* ((out (cl-cc/optimize::opt-pass-jump (nreverse instructions)))
           (j0  (find-if #'cl-cc:vm-jump-p out)))
      (assert-true j0)
      (assert-equal (cl-cc/vm::vm-label-name j0) "L25"))))

(deftest jump-zero-threading-updates-label
  "opt-pass-jump threads vm-jump-zero target through a chain."
  (let* ((c   (make-vm-const     :dst :r0 :value 0))
         (jz  (make-vm-jump-zero :reg :r0 :label "mid"))
         (lbl (make-vm-label     :name "mid"))
         (j2  (make-vm-jump      :label "end"))
         (end (make-vm-label     :name "end"))
         (ret (make-vm-ret       :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list c jz lbl j2 end ret))))
    ;; The jump-zero should now target "end" directly
    (let ((jz-out (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out)))
      (when jz-out
        (assert-equal (cl-cc/vm::vm-label-name jz-out) "end")))))
