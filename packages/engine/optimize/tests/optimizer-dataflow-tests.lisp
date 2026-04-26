;;;; tests/unit/optimize/optimizer-dataflow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-dataflow.lisp
;;;;
;;;; Covers: %sccp-env-copy, %sccp-env-equal-p, %sccp-env-merge,
;;;;   %sccp-fold-inst (const, move, jump-zero, binary, unary),
;;;;   %sccp-redirect-successors, %sccp-process-block,
;;;;   opt-pass-sccp (empty, straight-line const fold).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %sccp-env-copy ───────────────────────────────────────────────────────

(deftest sccp-env-copy-behavior
  "%sccp-env-copy: distinct table, copies all bindings, mutations don't affect original."
  (let ((empty (make-hash-table :test #'eq)))
    (assert-false (eq empty (cl-cc/optimize::%sccp-env-copy empty))))
  (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 42
          (gethash :r1 env) 99)
    (let ((copy (cl-cc/optimize::%sccp-env-copy env)))
      (assert-= 42 (gethash :r0 copy))
      (assert-= 99 (gethash :r1 copy))
      (setf (gethash :r0 copy) 999)
      (assert-= 42 (gethash :r0 env)))))

;;; ─── %sccp-env-equal-p ───────────────────────────────────────────────────

(deftest sccp-env-equal-p-cases
  "%sccp-env-equal-p: T for empty/same-bindings; NIL for different values or different keys."
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (assert-true (cl-cc/optimize::%sccp-env-equal-p a b))  ; both empty
    (setf (gethash :r0 a) 5  (gethash :r0 b) 5)
    (assert-true (cl-cc/optimize::%sccp-env-equal-p a b))) ; same bindings
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 6)
    (assert-false (cl-cc/optimize::%sccp-env-equal-p a b))) ; different values
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 1)
    (setf (gethash :r1 b) 1)
    (assert-false (cl-cc/optimize::%sccp-env-equal-p a b)))) ; different keys

;;; ─── %sccp-env-merge ─────────────────────────────────────────────────────

(deftest sccp-env-merge-trivial-cases
  "%sccp-env-merge: empty→empty table; singleton→copy of that env."
  (assert-= 0 (hash-table-count (cl-cc/optimize::%sccp-env-merge nil)))
  (let* ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 7)
    (let ((merged (cl-cc/optimize::%sccp-env-merge (list env))))
      (assert-= 7 (gethash :r0 merged))
      (assert-false (eq env merged)))))

(deftest sccp-env-merge-intersects-common-keys
  "%sccp-env-merge keeps only bindings that agree across all envs."
  (let* ((a (make-hash-table :test #'eq))
         (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 5)   ; agree
    (setf (gethash :r1 a) 3  (gethash :r1 b) 9)   ; disagree
    (setf (gethash :r2 a) 1)                        ; only in a
    (let ((merged (cl-cc/optimize::%sccp-env-merge (list a b))))
      (assert-= 5  (gethash :r0 merged))
      (assert-false (nth-value 1 (gethash :r1 merged)))
      (assert-false (nth-value 1 (gethash :r2 merged))))))

;;; ─── %sccp-fold-inst ─────────────────────────────────────────────────────

(deftest-each sccp-fold-inst-passthrough-cases
  "%sccp-fold-inst returns vm-const and vm-label unchanged."
  :cases (("const" (make-vm-const :dst :r0 :value 42))
          ("label" (make-vm-label :name "x")))
  (inst)
  (assert-eq inst (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq))))

(deftest sccp-fold-inst-move-with-known-src-folds-to-const
  "%sccp-fold-inst replaces vm-move with vm-const when src is known."
  (let* ((inst (make-vm-move :dst :r1 :src :r0))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 77)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (assert-true (typep result 'cl-cc/vm::vm-const))
      (assert-= 77 (cl-cc/vm::vm-value result)))))

(deftest-each sccp-fold-inst-unknown-passthrough-cases
  "%sccp-fold-inst returns vm-move and vm-add unchanged when operands are unknown."
  :cases (("move"   (make-vm-move :dst :r1 :src :r0))
          ("binary" (make-vm-add  :dst :r2 :lhs :r0 :rhs :r1)))
  (inst)
  (assert-eq inst (cl-cc/optimize::%sccp-fold-inst inst (make-hash-table :test #'eq))))

(deftest sccp-fold-inst-binary-folds-when-both-known
  "%sccp-fold-inst folds vm-add to vm-const when both operands are known constants."
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 3
          (gethash :r1 env) 4)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (assert-true (typep result 'cl-cc/vm::vm-const))
      (assert-= 7 (cl-cc/vm::vm-value result)))))

;;; ─── %sccp-redirect-successors ──────────────────────────────────────────

(deftest sccp-redirect-successors-rewires-edges
  "%sccp-redirect-successors removes old successor/predecessor links and adds new ones."
  (let* ((cfg    (cl-cc/optimize::cfg-build
                  (list (make-vm-const :dst :r0 :value 0)
                        (make-vm-jump  :label "lbl")
                        (make-vm-label :name "lbl")
                        (make-vm-ret   :reg :r0))))
         (entry  (cl-cc/optimize::cfg-entry cfg))
         (succ   (first (cl-cc::bb-successors entry)))
         (extra  (cl-cc/optimize::cfg-new-block cfg)))
    (cl-cc/optimize::%sccp-redirect-successors entry (list extra))
    (assert-true  (member extra (cl-cc::bb-successors entry) :test #'eq))
    (assert-false (member succ  (cl-cc::bb-successors entry) :test #'eq))
    (assert-true  (member entry (cl-cc::bb-predecessors extra) :test #'eq))
    (assert-false (member entry (cl-cc::bb-predecessors succ)  :test #'eq))))

;;; ─── %sccp-process-block ─────────────────────────────────────────────────

(deftest sccp-process-block-folds-const-in-block
  "%sccp-process-block folds vm-add to vm-const when env has both operands."
  (let* ((env   (make-hash-table :test #'eq))
         (cfg   (cl-cc/optimize::cfg-build
                 (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                       (make-vm-ret :reg :r2))))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (setf (gethash :r0 env) 3 (gethash :r1 env) 4)
    (let ((out-env (cl-cc/optimize::%sccp-process-block entry env)))
      (let ((folded (first (cl-cc::bb-instructions entry))))
        (assert-true (typep folded 'cl-cc/vm::vm-const))
        (assert-= 7 (cl-cc/vm::vm-value folded)))
      (assert-= 7 (gethash :r2 out-env)))))

;;; ─── opt-pass-sccp ───────────────────────────────────────────────────────

(deftest sccp-pass-empty-returns-nil-or-list
  "opt-pass-sccp on empty input returns nil or empty list."
  (let ((result (cl-cc/optimize::opt-pass-sccp nil)))
    (assert-true (listp result))))

(deftest sccp-pass-fold-and-preserve
  "opt-pass-sccp folds known-constant binops; preserves add with unknown operands."
  (let* ((insts (list (make-vm-const :dst :r0 :value 10)
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const)) result)))
      (assert-true (some (lambda (c) (= 30 (cl-cc/vm::vm-value c))) consts))))
  (let* ((insts (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret  :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))))

;;; ─── %sccp-update-env-for-inst ───────────────────────────────────────────

(deftest-each sccp-update-env-for-inst-cases
  "%sccp-update-env-for-inst binds env for vm-const, kills env entry for other instructions."
  :cases (("const-binds"  (make-vm-const :dst :r0 :value 42)  42)
          ("non-const-kills" (make-vm-add :dst :r0 :lhs :r1 :rhs :r2) nil))
  (inst expected-val)
  (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 99)
    (cl-cc/optimize::%sccp-update-env-for-inst inst env)
    (if expected-val
        (assert-= expected-val (gethash :r0 env))
        (assert-false (nth-value 1 (gethash :r0 env))))))

(deftest sccp-update-env-for-inst-no-dst-noop
  "%sccp-update-env-for-inst leaves env unchanged when instruction has no destination."
  (let ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 7)
    (cl-cc/optimize::%sccp-update-env-for-inst (make-vm-ret :reg :r0) env)
    (assert-= 7 (gethash :r0 env))))

;;; ─── %sccp-fold-inst jump-zero cases ─────────────────────────────────────

(deftest sccp-fold-inst-jump-zero-false-becomes-jump
  "%sccp-fold-inst folds vm-jump-zero to vm-jump when condition register is known falsy (0)."
  (let* ((inst (make-vm-jump-zero :reg :r0 :label "done"))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 0)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (assert-true (typep result 'cl-cc/vm::vm-jump))
      (assert-equal "done" (cl-cc/vm::vm-label-name result)))))

(deftest sccp-fold-inst-jump-zero-true-eliminates-branch
  "%sccp-fold-inst returns NIL when condition register is known truthy (non-zero, non-nil)."
  (let* ((inst (make-vm-jump-zero :reg :r0 :label "done"))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 1)
    (assert-null (cl-cc/optimize::%sccp-fold-inst inst env))))
