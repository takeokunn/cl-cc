;;;; tests/unit/optimize/optimizer-inline-pass-ext-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp — inlining policy and full passes.
;;;;
;;;; Covers:
;;;;   Inlining policy — opt-inline-inst-cost, opt-inline-body-cost,
;;;;                     opt-adaptive-inline-threshold, opt-inline-eligible-p
;;;;   Full passes     — opt-pass-global-dce, opt-pass-inline
;;;;
;;;; Depends on helpers defined in optimizer-inline-pass-tests.lisp
;;;; (loaded before this file via :serial t ASDF).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-inline-inst-cost ───────────────────────────────────────────────────

(deftest opt-inline-inst-cost-returns-number-ext
  "opt-inline-inst-cost returns a non-negative number for common instructions."
  (dolist (inst (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r1 :src :r0)
                      (make-vm-ret   :reg :r0)))
    (assert-true (>= (cl-cc/optimize::opt-inline-inst-cost inst) 0))))

;;; ─── opt-inline-body-cost ───────────────────────────────────────────────────

(deftest-each opt-inline-body-cost-cases
  "opt-inline-body-cost sums non-ret costs: [const, ret]→cost(const); [ret]→0."
  :cases (("excludes-ret"
           (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))
           (cl-cc/optimize::opt-inline-inst-cost (make-vm-const :dst :r0 :value 1)))
          ("only-ret"
           (list (make-vm-ret :reg :r0))
           0))
  (body expected-cost)
  (assert-= expected-cost (cl-cc/optimize::opt-inline-body-cost body)))

;;; ─── opt-adaptive-inline-threshold ──────────────────────────────────────────

(deftest opt-adaptive-threshold-cheap-body-ext
  "opt-adaptive-inline-threshold returns higher threshold for all-cheap instructions."
  (let* ((ci   (make-vm-closure :dst :r9 :label "cheap" :params '(:r0) :captured nil))
         ;; vm-move has cost ≤ 1 — saturate cheap-ratio to 1.0
         (body (loop repeat 5
                     collect (make-vm-move :dst :r1 :src :r0)))
         (def  (list :closure ci :params '(:r0) :body (append body (list (make-vm-ret :reg :r1)))))
         (threshold (cl-cc/optimize::opt-adaptive-inline-threshold def)))
    ;; Should be > base (15) because cheap-ratio >= 0.75
    (assert-true (>= threshold 15))))

(deftest opt-adaptive-threshold-has-floor-ext
  "opt-adaptive-inline-threshold returns at least 8 even for empty bodies."
  (let* ((ci  (make-vm-closure :dst :r9 :label "empty" :params '(:r0) :captured nil))
         (def (list :closure ci :params '(:r0) :body (list (make-vm-ret :reg :r0))))
         (threshold (cl-cc/optimize::opt-adaptive-inline-threshold def)))
    (assert-true (>= threshold 8))))

;;; ─── opt-inline-eligible-p ──────────────────────────────────────────────────

(defun %make-eligible-def (label params body-insts)
  "Build a plist-style func def suitable for opt-inline-eligible-p."
  (let ((ci (make-vm-closure :dst :r9 :label label :params params :captured nil)))
    (list :closure ci
          :params params
          :body (append body-insts (list (make-vm-ret :reg (first params)))))))

(deftest opt-inline-eligible-simple-function-ext
  "opt-inline-eligible-p returns T for a simple captured-var-free function."
  (let* ((def (%make-eligible-def "add1" '(:r0)
                 (list (make-vm-const :dst :r1 :value 1)))))
    (assert-true (cl-cc/optimize::opt-inline-eligible-p def 50))))

(deftest opt-inline-eligible-rejects-captured-vars-ext
  "opt-inline-eligible-p returns NIL when the closure has captured variables."
  (let* ((ci  (make-vm-closure :dst :r9 :label "cap"
                                :params '(:r0)
                                :captured '((:r5 . 42))))
         (def (list :closure ci :params '(:r0)
                    :body (list (make-vm-ret :reg :r0)))))
    (assert-null (cl-cc/optimize::opt-inline-eligible-p def 50))))

(deftest opt-inline-eligible-rejects-over-threshold-ext
  "opt-inline-eligible-p returns NIL when body cost exceeds THRESHOLD."
  (let* ((big-body (loop repeat 20
                         collect (make-vm-move :dst :r1 :src :r0)))
         (def (%make-eligible-def "big" '(:r0) big-body)))
    ;; threshold=0 → any non-empty body exceeds it
    (assert-null (cl-cc/optimize::opt-inline-eligible-p def 0))))

;;; ─── opt-pass-global-dce ────────────────────────────────────────────────────

(deftest opt-pass-global-dce-empty-program-ext
  "opt-pass-global-dce on an empty instruction list returns nil."
  (assert-null (cl-cc/optimize::opt-pass-global-dce nil)))

(deftest opt-pass-global-dce-preserves-reachable-function-ext
  "opt-pass-global-dce keeps a function that is called from top-level code."
  (let* (;; Function "fn" takes :r1 as param (non-nil so it gets collected)
         (cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         ;; Top-level: call the closure held in :r0
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-global-dce insts)))
    ;; All instructions should be preserved since "fn" is reachable
    (assert-= (length insts) (length result))))

(deftest opt-pass-global-dce-removes-dead-function-ext
  "opt-pass-global-dce removes a function that is never called."
  (let* (;; Dead function: must have non-nil params so opt-collect-function-defs
         ;; picks it up. Without params, it's invisible to the DCE analysis.
         (dead-cl  (make-vm-closure :dst :r9 :label "dead"
                                    :params '(:r0) :captured nil))
         (dead-lbl (make-vm-label :name "dead"))
         (dead-body (make-vm-const :dst :r1 :value 99))
         (dead-ret  (make-vm-ret   :reg :r0))
         ;; Top-level: just a constant, never loads or calls "dead"
         (top-const (make-vm-const :dst :r0 :value 0))
         (insts (list dead-cl dead-lbl dead-body dead-ret top-const))
         (result (cl-cc/optimize::opt-pass-global-dce insts)))
    ;; Only top-const should survive — 4 dead-function instructions removed
    (assert-= 1 (length result))
    (assert-true (cl-cc:vm-const-p (first result)))))

;;; ─── opt-pass-inline ────────────────────────────────────────────────────────

(deftest opt-pass-inline-empty-program-ext
  "opt-pass-inline on nil returns nil."
  (assert-null (cl-cc/optimize::opt-pass-inline nil)))

(deftest opt-pass-inline-inlines-eligible-call-ext
  "opt-pass-inline replaces a vm-call of a small function with inlined moves."
  (let* (;; Define a trivial function: (lambda (:r1) (const :r2 42) (ret :r1))
         (cl   (make-vm-closure :dst :r0 :label "const42" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "const42"))
         (body (make-vm-const :dst :r2 :value 42))
         (ret  (make-vm-ret   :reg :r2))
         ;; Call it: (const42 :r5) → :r6
         (call (make-vm-call :dst :r6 :func :r0 :args (list :r5)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-inline insts :threshold 50)))
    ;; vm-call should be gone; result should contain vm-move for arg passing
    (assert-null (find-if #'cl-cc:vm-call-p result))
    ;; A vm-move or vm-const should appear for argument/return
    (assert-true (some (lambda (i) (or (cl-cc:vm-move-p i) (cl-cc:vm-const-p i))) result))))

(deftest opt-pass-inline-preserves-non-eligible-call-ext
  "opt-pass-inline keeps vm-call for a function with captured variables (never eligible)."
  ;; A closure with captured vars fails opt-inline-eligible-p regardless of threshold
  (let* ((cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1)
                                 :captured '((:r5 . 42))))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc/optimize::opt-pass-inline insts :threshold 50)))
    ;; Captured vars → not eligible → vm-call must remain in output
    (assert-true (find-if #'cl-cc:vm-call-p result))))
