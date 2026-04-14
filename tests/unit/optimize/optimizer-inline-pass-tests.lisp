;;;; tests/unit/optimize/optimizer-inline-pass-tests.lisp
;;;; Unit tests for src/optimize/optimizer-inline-pass.lisp
;;;;
;;;; Covers:
;;;;   Memo utilities  — opt-make-pure-function-memo-table, opt-pure-function-memo-key,
;;;;                     opt-pure-function-memo-get, opt-pure-function-memo-put
;;;;   Body analysis   — opt-function-body-instruction-set, opt-function-body-instruction-labels
;;;;   Reachability    — opt-reachable-function-labels
;;;;   Inlining policy — opt-inline-inst-cost, opt-inline-body-cost,
;;;;                     opt-adaptive-inline-threshold, opt-inline-eligible-p
;;;;   Full passes     — opt-pass-global-dce, opt-pass-inline

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

;;; Note: opt-collect-function-defs requires (vm-closure-params inst) to be
;;; non-nil — functions with zero parameters are excluded from collection.
;;; Always give test closures at least one param so they are collected.

(defun make-inline-pass-func-insts (label params body-insts)
  "Return instruction list: vm-closure + vm-label + BODY-INSTS + vm-ret."
  (list* (make-vm-closure :dst :r9 :label label :params params :captured nil)
         (make-vm-label  :name label)
         (append body-insts
                 (list (make-vm-ret :reg (first params))))))

(defun collect-inline-pass-func-defs (label params body-insts)
  "Build func-defs table directly for functions with the given structure."
  (cl-cc::opt-collect-function-defs
   (make-inline-pass-func-insts label params body-insts)))

;;; ─── opt-make-pure-function-memo-table ──────────────────────────────────────

(deftest opt-memo-table-is-hash-table
  "opt-make-pure-function-memo-table returns an empty equal hash-table."
  (let ((ht (cl-cc::opt-make-pure-function-memo-table)))
    (assert-true (hash-table-p ht))
    (assert-= 0 (hash-table-count ht))))

;;; ─── opt-pure-function-memo-key ─────────────────────────────────────────────

(deftest opt-memo-key-structure
  "opt-pure-function-memo-key returns a list of label and args."
  (let ((key (cl-cc::opt-pure-function-memo-key "fn" '(1 2))))
    (assert-equal '("fn" (1 2)) key)))

(deftest opt-memo-key-empty-args
  "opt-pure-function-memo-key works with empty arg list."
  (let ((key (cl-cc::opt-pure-function-memo-key "f" nil)))
    (assert-equal '("f" nil) key)))

;;; ─── opt-pure-function-memo-get / opt-pure-function-memo-put ────────────────

(deftest opt-memo-get-miss-for-impure-label
  "opt-pure-function-memo-get returns nil/nil for a label not in pure-labels."
  (let ((memo   (cl-cc::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (multiple-value-bind (val found-p)
        (cl-cc::opt-pure-function-memo-get memo pures "fn" '(1))
      (assert-null val)
      (assert-null found-p))))

(deftest opt-memo-get-miss-before-put
  "opt-pure-function-memo-get returns nil/nil for a pure label before any put."
  (let ((memo   (cl-cc::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (multiple-value-bind (val found-p)
        (cl-cc::opt-pure-function-memo-get memo pures "fn" '(1))
      (assert-null val)
      (assert-null found-p))))

(deftest opt-memo-roundtrip
  "opt-pure-function-memo-put then get returns the stored result."
  (let ((memo   (cl-cc::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (setf (gethash "fn" pures) t)
    (cl-cc::opt-pure-function-memo-put memo pures "fn" '(2) :answer)
    (multiple-value-bind (val found-p)
        (cl-cc::opt-pure-function-memo-get memo pures "fn" '(2))
      (assert-eq :answer val)
      (assert-true found-p))))

(deftest opt-memo-put-ignores-impure-label
  "opt-pure-function-memo-put does not store for labels absent from pure-labels."
  (let ((memo   (cl-cc::opt-make-pure-function-memo-table))
        (pures  (make-hash-table :test #'equal)))
    (cl-cc::opt-pure-function-memo-put memo pures "impure" '() :result)
    (assert-= 0 (hash-table-count memo))))

;;; ─── opt-function-body-instruction-set ──────────────────────────────────────

(deftest opt-body-instruction-set-empty
  "opt-function-body-instruction-set on empty func-defs returns empty table."
  (let ((ht (cl-cc::opt-function-body-instruction-set (make-hash-table :test #'equal))))
    (assert-= 0 (hash-table-count ht))))

(deftest opt-body-instruction-set-contains-body-insts
  "opt-function-body-instruction-set includes every instruction in every body."
  (let* ((c1    (make-vm-const :dst :r1 :value 42))
         (ret   (make-vm-ret   :reg :r1))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "f" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (let ((s (cl-cc::opt-function-body-instruction-set fdefs)))
      (assert-true (gethash c1  s))
      (assert-true (gethash ret s)))))

;;; ─── opt-function-body-instruction-labels ───────────────────────────────────

(deftest opt-body-instruction-labels-maps-to-label
  "opt-function-body-instruction-labels maps each body inst back to its label."
  (let* ((c1    (make-vm-const :dst :r0 :value 1))
         (ret   (make-vm-ret   :reg :r0))
         (fdefs (make-hash-table :test #'equal)))
    (setf (gethash "myfn" fdefs)
          (list :closure nil :params nil :body (list c1 ret)))
    (let ((inst->label (cl-cc::opt-function-body-instruction-labels fdefs)))
      (assert-equal "myfn" (gethash c1  inst->label))
      (assert-equal "myfn" (gethash ret inst->label)))))

;;; ─── opt-reachable-function-labels ──────────────────────────────────────────

(deftest opt-reachable-empty-roots
  "opt-reachable-function-labels returns empty set when roots is empty."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (let ((r (cl-cc::opt-reachable-function-labels graph roots)))
      (assert-= 0 (hash-table-count r)))))

(deftest opt-reachable-direct-root
  "opt-reachable-function-labels includes the root label itself."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "fn" graph) nil)
    (setf (gethash "fn" roots) t)
    (let ((r (cl-cc::opt-reachable-function-labels graph roots)))
      (assert-true (gethash "fn" r)))))

(deftest opt-reachable-transitive
  "opt-reachable-function-labels follows call edges transitively."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "a" graph) '("b"))
    (setf (gethash "b" graph) '("c"))
    (setf (gethash "c" graph) nil)
    (setf (gethash "a" roots) t)
    (let ((r (cl-cc::opt-reachable-function-labels graph roots)))
      (assert-true (gethash "a" r))
      (assert-true (gethash "b" r))
      (assert-true (gethash "c" r)))))

(deftest opt-reachable-cycle-terminates
  "opt-reachable-function-labels terminates on cyclic call graphs."
  (let ((graph (make-hash-table :test #'equal))
        (roots (make-hash-table :test #'equal)))
    (setf (gethash "x" graph) '("y"))
    (setf (gethash "y" graph) '("x"))
    (setf (gethash "x" roots) t)
    (let ((r (cl-cc::opt-reachable-function-labels graph roots)))
      (assert-true  (gethash "x" r))
      (assert-true  (gethash "y" r)))))

;;; ─── opt-inline-inst-cost ───────────────────────────────────────────────────

(deftest opt-inline-inst-cost-returns-number
  "opt-inline-inst-cost returns a non-negative number for common instructions."
  (dolist (inst (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r1 :src :r0)
                      (make-vm-ret   :reg :r0)))
    (assert-true (>= (cl-cc::opt-inline-inst-cost inst) 0))))

;;; ─── opt-inline-body-cost ───────────────────────────────────────────────────

(deftest opt-inline-body-cost-excludes-ret
  "opt-inline-body-cost excludes the final vm-ret from the cost sum."
  (let* ((c1   (make-vm-const :dst :r0 :value 1))
         (ret  (make-vm-ret   :reg :r0))
         (cost (cl-cc::opt-inline-body-cost (list c1 ret))))
    ;; cost of [c1] only, not ret
    (assert-= cost (cl-cc::opt-inline-inst-cost c1))))

(deftest opt-inline-body-cost-empty-except-ret
  "opt-inline-body-cost is 0 for a body with only a vm-ret."
  (let ((body (list (make-vm-ret :reg :r0))))
    (assert-= 0 (cl-cc::opt-inline-body-cost body))))

;;; ─── opt-adaptive-inline-threshold ──────────────────────────────────────────

(deftest opt-adaptive-threshold-cheap-body
  "opt-adaptive-inline-threshold returns higher threshold for all-cheap instructions."
  (let* ((ci   (make-vm-closure :dst :r9 :label "cheap" :params '(:r0) :captured nil))
         ;; vm-move has cost ≤ 1 — saturate cheap-ratio to 1.0
         (body (loop repeat 5
                     collect (make-vm-move :dst :r1 :src :r0)))
         (def  (list :closure ci :params '(:r0) :body (append body (list (make-vm-ret :reg :r1)))))
         (threshold (cl-cc::opt-adaptive-inline-threshold def)))
    ;; Should be > base (15) because cheap-ratio >= 0.75
    (assert-true (>= threshold 15))))

(deftest opt-adaptive-threshold-has-floor
  "opt-adaptive-inline-threshold returns at least 8 even for empty bodies."
  (let* ((ci  (make-vm-closure :dst :r9 :label "empty" :params '(:r0) :captured nil))
         (def (list :closure ci :params '(:r0) :body (list (make-vm-ret :reg :r0))))
         (threshold (cl-cc::opt-adaptive-inline-threshold def)))
    (assert-true (>= threshold 8))))

;;; ─── opt-inline-eligible-p ──────────────────────────────────────────────────

(defun %make-eligible-def (label params body-insts)
  "Build a plist-style func def suitable for opt-inline-eligible-p."
  (let ((ci (make-vm-closure :dst :r9 :label label :params params :captured nil)))
    (list :closure ci
          :params params
          :body (append body-insts (list (make-vm-ret :reg (first params)))))))

(deftest opt-inline-eligible-simple-function
  "opt-inline-eligible-p returns T for a simple captured-var-free function."
  (let* ((def (%make-eligible-def "add1" '(:r0)
                 (list (make-vm-const :dst :r1 :value 1)))))
    (assert-true (cl-cc::opt-inline-eligible-p def 50))))

(deftest opt-inline-eligible-rejects-captured-vars
  "opt-inline-eligible-p returns NIL when the closure has captured variables."
  (let* ((ci  (make-vm-closure :dst :r9 :label "cap"
                                :params '(:r0)
                                :captured '((:r5 . 42))))
         (def (list :closure ci :params '(:r0)
                    :body (list (make-vm-ret :reg :r0)))))
    (assert-null (cl-cc::opt-inline-eligible-p def 50))))

(deftest opt-inline-eligible-rejects-over-threshold
  "opt-inline-eligible-p returns NIL when body cost exceeds THRESHOLD."
  (let* ((big-body (loop repeat 20
                         collect (make-vm-move :dst :r1 :src :r0)))
         (def (%make-eligible-def "big" '(:r0) big-body)))
    ;; threshold=0 → any non-empty body exceeds it
    (assert-null (cl-cc::opt-inline-eligible-p def 0))))

;;; ─── opt-pass-global-dce ────────────────────────────────────────────────────

(deftest opt-pass-global-dce-empty-program
  "opt-pass-global-dce on an empty instruction list returns nil."
  (assert-null (cl-cc::opt-pass-global-dce nil)))

(deftest opt-pass-global-dce-preserves-reachable-function
  "opt-pass-global-dce keeps a function that is called from top-level code."
  (let* (;; Function "fn" takes :r1 as param (non-nil so it gets collected)
         (cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         ;; Top-level: call the closure held in :r0
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc::opt-pass-global-dce insts)))
    ;; All instructions should be preserved since "fn" is reachable
    (assert-= (length insts) (length result))))

(deftest opt-pass-global-dce-removes-dead-function
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
         (result (cl-cc::opt-pass-global-dce insts)))
    ;; Only top-const should survive — 4 dead-function instructions removed
    (assert-= 1 (length result))
    (assert-true (cl-cc::vm-const-p (first result)))))

;;; ─── opt-pass-inline ────────────────────────────────────────────────────────

(deftest opt-pass-inline-empty-program
  "opt-pass-inline on nil returns nil."
  (assert-null (cl-cc::opt-pass-inline nil)))

(deftest opt-pass-inline-inlines-eligible-call
  "opt-pass-inline replaces a vm-call of a small function with inlined moves."
  (let* (;; Define a trivial function: (lambda (:r1) (const :r2 42) (ret :r1))
         (cl   (make-vm-closure :dst :r0 :label "const42" :params '(:r1) :captured nil))
         (lbl  (make-vm-label :name "const42"))
         (body (make-vm-const :dst :r2 :value 42))
         (ret  (make-vm-ret   :reg :r2))
         ;; Call it: (const42 :r5) → :r6
         (call (make-vm-call :dst :r6 :func :r0 :args (list :r5)))
         (insts (list cl lbl body ret call))
         (result (cl-cc::opt-pass-inline insts :threshold 50)))
    ;; vm-call should be gone; result should contain vm-move for arg passing
    (assert-null (find-if #'cl-cc::vm-call-p result))
    ;; A vm-move or vm-const should appear for argument/return
    (assert-true (some (lambda (i) (or (cl-cc::vm-move-p i) (cl-cc::vm-const-p i))) result))))

(deftest opt-pass-inline-preserves-non-eligible-call
  "opt-pass-inline keeps vm-call for a function with captured variables (never eligible)."
  ;; A closure with captured vars fails opt-inline-eligible-p regardless of threshold
  (let* ((cl   (make-vm-closure :dst :r0 :label "fn" :params '(:r1)
                                 :captured '((:r5 . 42))))
         (lbl  (make-vm-label :name "fn"))
         (body (make-vm-const :dst :r2 :value 1))
         (ret  (make-vm-ret   :reg :r2))
         (call (make-vm-call :dst :r3 :func :r0 :args (list :r1)))
         (insts (list cl lbl body ret call))
         (result (cl-cc::opt-pass-inline insts :threshold 50)))
    ;; Captured vars → not eligible → vm-call must remain in output
    (assert-true (find-if #'cl-cc::vm-call-p result))))
