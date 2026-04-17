;;;; tests/unit/optimize/optimizer-dataflow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-dataflow.lisp
;;;;
;;;; Covers: %sccp-env-copy, %sccp-env-equal-p, %sccp-env-merge,
;;;;   %sccp-fold-inst (const, move, jump-zero, binary, unary),
;;;;   opt-pass-sccp (empty, straight-line const fold).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %sccp-env-copy ───────────────────────────────────────────────────────

(deftest sccp-env-copy-produces-independent-table
  "%sccp-env-copy returns a distinct but equal hash table."
  (let* ((env (make-hash-table :test #'eq))
         (copy (cl-cc/optimize::%sccp-env-copy env)))
    (assert-false (eq env copy))
    (assert-= 0 (hash-table-count copy))))

(deftest sccp-env-copy-contains-same-bindings
  "%sccp-env-copy copies all key/value pairs."
  (let* ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 42
          (gethash :r1 env) 99)
    (let ((copy (cl-cc/optimize::%sccp-env-copy env)))
      (assert-= 42 (gethash :r0 copy))
      (assert-= 99 (gethash :r1 copy)))))

(deftest sccp-env-copy-is-not-shared
  "Mutations to the copy do not affect the original."
  (let* ((env (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 1)
    (let ((copy (cl-cc/optimize::%sccp-env-copy env)))
      (setf (gethash :r0 copy) 999)
      (assert-= 1 (gethash :r0 env)))))

;;; ─── %sccp-env-equal-p ───────────────────────────────────────────────────

(deftest sccp-env-equal-empty-envs
  "%sccp-env-equal-p returns T for two empty tables."
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (assert-true (cl-cc/optimize::%sccp-env-equal-p a b))))

(deftest sccp-env-equal-same-bindings
  "%sccp-env-equal-p returns T when both tables have the same key/value pairs."
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 5)
    (assert-true (cl-cc/optimize::%sccp-env-equal-p a b))))

(deftest sccp-env-equal-different-values
  "%sccp-env-equal-p returns NIL when values differ."
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 5  (gethash :r0 b) 6)
    (assert-false (cl-cc/optimize::%sccp-env-equal-p a b))))

(deftest sccp-env-equal-different-keys
  "%sccp-env-equal-p returns NIL when key sets differ."
  (let ((a (make-hash-table :test #'eq))
        (b (make-hash-table :test #'eq)))
    (setf (gethash :r0 a) 1)
    (setf (gethash :r1 b) 1)
    (assert-false (cl-cc/optimize::%sccp-env-equal-p a b))))

;;; ─── %sccp-env-merge ─────────────────────────────────────────────────────

(deftest sccp-env-merge-empty-list-returns-empty
  "%sccp-env-merge on nil returns an empty hash table."
  (let ((merged (cl-cc/optimize::%sccp-env-merge nil)))
    (assert-= 0 (hash-table-count merged))))

(deftest sccp-env-merge-singleton-returns-copy
  "%sccp-env-merge on a one-element list returns a copy of that env."
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

(deftest sccp-fold-inst-const-unchanged
  "%sccp-fold-inst returns vm-const unchanged."
  (let* ((inst (make-vm-const :dst :r0 :value 42))
         (env  (make-hash-table :test #'eq))
         (result (cl-cc/optimize::%sccp-fold-inst inst env)))
    (assert-eq inst result)))

(deftest sccp-fold-inst-label-unchanged
  "%sccp-fold-inst returns vm-label unchanged."
  (let* ((inst (make-vm-label :name "x"))
         (env  (make-hash-table :test #'eq))
         (result (cl-cc/optimize::%sccp-fold-inst inst env)))
    (assert-eq inst result)))

(deftest sccp-fold-inst-move-with-known-src-folds-to-const
  "%sccp-fold-inst replaces vm-move with vm-const when src is known."
  (let* ((inst (make-vm-move :dst :r1 :src :r0))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 77)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (assert-true (typep result 'cl-cc/vm::vm-const))
      (assert-= 77 (cl-cc/vm::vm-value result)))))

(deftest sccp-fold-inst-move-with-unknown-src-unchanged
  "%sccp-fold-inst returns vm-move unchanged when src is unknown."
  (let* ((inst (make-vm-move :dst :r1 :src :r0))
         (env  (make-hash-table :test #'eq))
         (result (cl-cc/optimize::%sccp-fold-inst inst env)))
    (assert-eq inst result)))

(deftest sccp-fold-inst-binary-folds-when-both-known
  "%sccp-fold-inst folds vm-add to vm-const when both operands are known constants."
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (env  (make-hash-table :test #'eq)))
    (setf (gethash :r0 env) 3
          (gethash :r1 env) 4)
    (let ((result (cl-cc/optimize::%sccp-fold-inst inst env)))
      (assert-true (typep result 'cl-cc/vm::vm-const))
      (assert-= 7 (cl-cc/vm::vm-value result)))))

(deftest sccp-fold-inst-binary-unchanged-when-unknown
  "%sccp-fold-inst returns vm-add unchanged when operands are unknown."
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (env  (make-hash-table :test #'eq))
         (result (cl-cc/optimize::%sccp-fold-inst inst env)))
    (assert-eq inst result)))

;;; ─── opt-pass-sccp ───────────────────────────────────────────────────────

(deftest sccp-pass-empty-returns-nil-or-list
  "opt-pass-sccp on empty input returns nil or empty list."
  (let ((result (cl-cc/optimize::opt-pass-sccp nil)))
    (assert-true (listp result))))

(deftest sccp-pass-folds-constant-arithmetic
  "opt-pass-sccp folds vm-add of two known consts into a single vm-const."
  (let* ((insts (list (make-vm-const :dst :r0 :value 10)
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    ;; After SCCP, the vm-add should be folded into vm-const 30
    (let ((consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const)) result)))
      ;; There should be a const with value 30
      (assert-true (some (lambda (c) (= 30 (cl-cc/vm::vm-value c))) consts)))))

(deftest sccp-pass-preserves-unknown-values
  "opt-pass-sccp leaves instructions alone when operands are not constant."
  (let* ((insts (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret  :reg :r2)))
         (result (cl-cc/optimize::opt-pass-sccp insts)))
    ;; The vm-add should remain since r0/r1 are unknown
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))))
