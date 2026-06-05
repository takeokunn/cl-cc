;;;; packages/optimize/tests/optimizer-scheduler-tests.lisp
;;;; Unit tests for packages/optimize/src/optimizer-scheduler.lisp
;;;;
;;;; Covers:
;;;;   %opt-scheduler-barrier-p  — returns T for each barrier type, NIL for pure
;;;;   opt-pass-schedule-local   — reorders instructions in a RAW-dependency chain,
;;;;                               preserves barriers in original relative position
;;;;   schedule-pre-ra           — pressure-aware scheduling produces valid orderings

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Test helpers ─────────────────────────────────────────────────────────

(defun %sched-index (instructions instruction)
  "Return the 0-based position of INSTRUCTION in INSTRUCTIONS by EQ."
  (position instruction instructions :test #'eq))

;;; ─── %opt-scheduler-barrier-p ─────────────────────────────────────────────

(deftest-each scheduler-barrier-p-returns-true-for-barrier-types
  "%opt-scheduler-barrier-p returns T for instructions that must not move."
  :cases
  (("vm-call"
    (make-vm-call :dst :r0 :func :f :args nil))
   ("vm-apply"
    (cl-cc:make-vm-apply :dst :r0 :func :f :args '(:r1)))
   ("vm-set-global"
    (make-vm-set-global :src :r0 :name 'x))
   ("vm-signal-error"
    (cl-cc:make-vm-signal-error :error-reg :r0))
   ("vm-slot-write"
    (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r0)))
  (inst)
  (assert-true (cl-cc/optimize::%opt-scheduler-barrier-p inst)))

(deftest-each scheduler-barrier-p-returns-false-for-pure-instructions
  "%opt-scheduler-barrier-p returns NIL for pure/read-only instructions."
  :cases
  (("vm-const"
    (make-vm-const :dst :r0 :value 42))
   ("vm-move"
    (make-vm-move :dst :r1 :src :r0))
   ("vm-add"
    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
   ("vm-mul"
    (make-vm-mul :dst :r3 :lhs :r0 :rhs :r1))
   ("vm-sub"
    (make-vm-sub :dst :r4 :lhs :r2 :rhs :r1))
   ("vm-get-global"
    (make-vm-get-global :dst :r5 :name 'x)))
  (inst)
  (assert-false (cl-cc/optimize::%opt-scheduler-barrier-p inst)))

;;; ─── opt-pass-schedule-local: basic reordering ────────────────────────────

(deftest schedule-local-raw-chain-produces-valid-topological-order
  "opt-pass-schedule-local preserves RAW order: CONST must precede ADD."
  ;; CONST :r0 ← 1   (writes :r0)
  ;; CONST :r1 ← 2   (writes :r1, independent of :r0)
  ;; ADD   :r2 ← :r0 + :r1  (RAW on :r0 and :r1)
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (c2  (make-vm-const :dst :r1 :value 2))
         (add (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c1 c2 add))))
    ;; ADD must follow both CONSTs
    (let ((idx-c1  (%sched-index result c1))
          (idx-c2  (%sched-index result c2))
          (idx-add (%sched-index result add)))
      (assert-true (and idx-c1 idx-c2 idx-add))
      (assert-true (< idx-c1 idx-add))
      (assert-true (< idx-c2 idx-add)))))

(deftest schedule-local-independent-instructions-all-preserved
  "opt-pass-schedule-local emits all instructions even when they can be reordered."
  ;; Three fully independent CONSTs — any order is valid.
  (let* ((c0 (make-vm-const :dst :r0 :value 10))
         (c1 (make-vm-const :dst :r1 :value 20))
         (c2 (make-vm-const :dst :r2 :value 30))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 c1 c2))))
    (assert-= 3 (length result))
    (assert-true (member c0 result :test #'eq))
    (assert-true (member c1 result :test #'eq))
    (assert-true (member c2 result :test #'eq))))

(deftest schedule-local-deep-raw-chain-preserves-order
  "opt-pass-schedule-local keeps a deep RAW chain in-order."
  ;; c0 → add1 (RAW :r0) → add2 (RAW :r1) — must stay in original order.
  (let* ((c0   (make-vm-const :dst :r0 :value 1))
         (add1 (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0))
         (add2 (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 add1 add2))))
    (assert-= 3 (length result))
    (let ((idx-c0   (%sched-index result c0))
          (idx-add1 (%sched-index result add1))
          (idx-add2 (%sched-index result add2)))
      (assert-true (< idx-c0 idx-add1))
      (assert-true (< idx-add1 idx-add2)))))

;;; ─── opt-pass-schedule-local: barriers stop scheduling ────────────────────

(deftest schedule-local-call-barrier-splits-run
  "opt-pass-schedule-local never moves instructions across a vm-call barrier."
  ;; [CONST r0] CALL r1←(f) [ADD r2←r1+r0]
  ;; The ADD reads r1 (written by CALL) and r0 (pre-call), so it cannot cross
  ;; the call; the CONST must remain before the call.
  (let* ((c0   (make-vm-const :dst :r0 :value 1))
         (call (make-vm-call  :dst :r1 :func :f :args nil))
         (add  (make-vm-add   :dst :r2 :lhs :r1 :rhs :r0))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 call add))))
    (assert-= 3 (length result))
    ;; call must appear between c0 and add in the output
    (let ((idx-c0   (%sched-index result c0))
          (idx-call (%sched-index result call))
          (idx-add  (%sched-index result add)))
      (assert-true (and idx-c0 idx-call idx-add))
      (assert-true (< idx-c0 idx-call))
      (assert-true (< idx-call idx-add)))))

(deftest schedule-local-slot-write-barrier-preserved
  "opt-pass-schedule-local does not move any instruction past a vm-slot-write."
  (let* ((c0    (make-vm-const :dst :r0 :value 99))
         (write (cl-cc:make-vm-slot-write :obj-reg :obj :slot-name 'x :value-reg :r0))
         (c1    (make-vm-const :dst :r1 :value 1))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 write c1))))
    (assert-= 3 (length result))
    (let ((idx-write (%sched-index result write)))
      (assert-true idx-write)
      ;; c0 must stay before the write
      (assert-true (< (%sched-index result c0) idx-write))
      ;; c1 must stay after the write
      (assert-true (< idx-write (%sched-index result c1))))))

(deftest schedule-local-multiple-barriers-each-preserved
  "opt-pass-schedule-local keeps multiple barriers in their original relative order."
  (let* ((call1 (make-vm-call :dst :r0 :func :f1 :args nil))
         (call2 (make-vm-call :dst :r1 :func :f2 :args nil))
         (call3 (make-vm-call :dst :r2 :func :f3 :args nil))
         (result (cl-cc/optimize::opt-pass-schedule-local (list call1 call2 call3))))
    (assert-= 3 (length result))
    (assert-true (< (%sched-index result call1) (%sched-index result call2)))
    (assert-true (< (%sched-index result call2) (%sched-index result call3)))))

(deftest schedule-local-set-global-barrier-preserved
  "opt-pass-schedule-local treats vm-set-global as a barrier."
  (let* ((c0    (make-vm-const :dst :r0 :value 5))
         (store (make-vm-set-global :src :r0 :name 'g))
         (c1    (make-vm-const :dst :r1 :value 6))
         (result (cl-cc/optimize::opt-pass-schedule-local (list c0 store c1))))
    (assert-= 3 (length result))
    (let ((idx-store (%sched-index result store)))
      (assert-true (< (%sched-index result c0) idx-store))
      (assert-true (< idx-store (%sched-index result c1))))))

;;; ─── schedule-pre-ra: pressure-aware ordering ─────────────────────────────

(deftest schedule-pre-ra-raw-chain-preserves-topological-order
  "schedule-pre-ra preserves RAW topological order."
  (let* ((c0  (make-vm-const :dst :r0 :value 1))
         (c1  (make-vm-const :dst :r1 :value 2))
         (add (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1))
         (result (cl-cc/optimize::schedule-pre-ra (list c0 c1 add))))
    (assert-= 3 (length result))
    (let ((idx-c0  (%sched-index result c0))
          (idx-c1  (%sched-index result c1))
          (idx-add (%sched-index result add)))
      (assert-true (< idx-c0 idx-add))
      (assert-true (< idx-c1 idx-add)))))

(deftest schedule-pre-ra-all-instructions-emitted
  "schedule-pre-ra emits all instructions with no duplicates or omissions."
  (let* ((c0  (make-vm-const :dst :r0 :value 10))
         (c1  (make-vm-const :dst :r1 :value 20))
         (mul (make-vm-mul   :dst :r2 :lhs :r0 :rhs :r1))
         (add (make-vm-add   :dst :r3 :lhs :r2 :rhs :r0))
         (all (list c0 c1 mul add))
         (result (cl-cc/optimize::schedule-pre-ra all)))
    (assert-= 4 (length result))
    (dolist (inst all)
      (assert-true (member inst result :test #'eq)))))

(deftest schedule-pre-ra-call-barrier-not-moved
  "schedule-pre-ra never moves any instruction across a vm-call."
  (let* ((c0   (make-vm-const :dst :r0 :value 7))
         (call (make-vm-call  :dst :r1 :func :g :args nil))
         (add  (make-vm-add   :dst :r2 :lhs :r1 :rhs :r0))
         (result (cl-cc/optimize::schedule-pre-ra (list c0 call add))))
    (assert-= 3 (length result))
    (let ((idx-call (%sched-index result call)))
      (assert-true (< (%sched-index result c0) idx-call))
      (assert-true (< idx-call (%sched-index result add))))))

(deftest schedule-pre-ra-single-instruction-unchanged
  "schedule-pre-ra with a single instruction returns that instruction unchanged."
  (let* ((c0     (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::schedule-pre-ra (list c0))))
    (assert-= 1 (length result))
    (assert-eq c0 (first result))))

(deftest schedule-pre-ra-empty-input-returns-empty
  "schedule-pre-ra with empty input returns an empty list."
  (let ((result (cl-cc/optimize::schedule-pre-ra nil)))
    (assert-= 0 (length result))))
