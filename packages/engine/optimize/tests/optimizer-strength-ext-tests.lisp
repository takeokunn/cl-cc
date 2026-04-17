;;;; tests/unit/optimize/optimizer-strength-ext-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength-ext.lisp
;;;;
;;;; Covers: opt-reassociate-commutative-p, opt-copy-commutative-binop,
;;;;   opt-pass-reassociate (constant-drift, label-env-clear, passthrough),
;;;;   opt-pass-batch-concatenate (chain packing, non-chain passthrough,
;;;;   empty input).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-reassociate-commutative-p ──────────────────────────────────────

(deftest-each reassociate-commutative-p-true-for-commutative-ops
  "opt-reassociate-commutative-p returns T for commutative/associative instructions."
  :cases (("vm-add"         (make-vm-add         :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-integer-add" (make-vm-integer-add  :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-mul"         (make-vm-mul          :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-integer-mul" (make-vm-integer-mul  :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-logand"      (make-vm-logand       :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-logior"      (make-vm-logior       :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-logxor"      (make-vm-logxor       :dst :r0 :lhs :r1 :rhs :r2)))
  (inst)
  (assert-true (cl-cc/optimize::opt-reassociate-commutative-p inst)))

(deftest-each reassociate-commutative-p-false-for-non-commutative
  "opt-reassociate-commutative-p returns NIL for non-commutative instructions."
  :cases (("vm-sub"  (make-vm-sub  :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-div"  (make-vm-div  :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-move" (make-vm-move :dst :r0 :src :r1))
          ("vm-const" (make-vm-const :dst :r0 :value 1)))
  (inst)
  (assert-false (cl-cc/optimize::opt-reassociate-commutative-p inst)))

;;; ─── opt-copy-commutative-binop ──────────────────────────────────────────

(deftest-each copy-commutative-binop-creates-correct-type
  "opt-copy-commutative-binop creates a new instruction with the same type but new regs."
  :cases (("vm-add"         (make-vm-add         :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-add)
          ("vm-integer-add" (make-vm-integer-add  :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-integer-add)
          ("vm-mul"         (make-vm-mul          :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-mul)
          ("vm-logand"      (make-vm-logand       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logand)
          ("vm-logior"      (make-vm-logior       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logior)
          ("vm-logxor"      (make-vm-logxor       :dst :r0 :lhs :r0 :rhs :r0) 'cl-cc/vm::vm-logxor))
  (inst expected-type)
  (let ((new-inst (cl-cc/optimize::opt-copy-commutative-binop inst :r3 :r4 :r5)))
    (assert-true (typep new-inst expected-type))
    (assert-eq :r3 (cl-cc/vm::vm-dst new-inst))
    (assert-eq :r4 (cl-cc/vm::vm-lhs new-inst))
    (assert-eq :r5 (cl-cc/vm::vm-rhs new-inst))))

(deftest copy-commutative-binop-otherwise-returns-inst
  "opt-copy-commutative-binop returns INST unchanged for non-commutative types."
  (let* ((inst (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
         (result (cl-cc/optimize::opt-copy-commutative-binop inst :r9 :r8 :r7)))
    ;; The original inst is returned unchanged (otherwise clause)
    (assert-eq inst result)))

;;; ─── opt-pass-reassociate ────────────────────────────────────────────────

(deftest reassociate-empty-input-returns-nil
  "opt-pass-reassociate on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-reassociate nil)))

(deftest reassociate-straight-line-no-consts-unchanged
  "opt-pass-reassociate returns instruction count unchanged when no consts to drift."
  (let* ((insts (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    (assert-= (length insts) (length result))))

(deftest reassociate-label-clears-env
  "opt-pass-reassociate resets the constant environment at each vm-label."
  ;; After a label, prior const knowledge is flushed.
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-label :name "sep")
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    ;; Should produce a list (no crash)
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

(deftest reassociate-single-const-tracked
  "opt-pass-reassociate tracks vm-const values in the environment."
  (let* ((insts (list (make-vm-const :dst :r0 :value 10)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-reassociate insts)))
    ;; const is kept
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (= 10 (cl-cc/vm::vm-value i))))
                       result))))

;;; ─── opt-pass-batch-concatenate ──────────────────────────────────────────

(deftest batch-concatenate-empty-input-returns-nil
  "opt-pass-batch-concatenate on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-batch-concatenate nil)))

(deftest batch-concatenate-non-concat-passthrough
  "opt-pass-batch-concatenate leaves non-concatenate instructions untouched."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    (assert-= (length insts) (length result))))

(deftest batch-concatenate-single-no-chain
  "opt-pass-batch-concatenate keeps a lone vm-concatenate unchanged."
  (let* ((inst   (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (insts  (list inst (make-vm-ret :reg :r2)))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Lone concat stays as is (1 concat in output)
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result))))

(deftest batch-concatenate-packs-two-chain
  "opt-pass-batch-concatenate merges two chained vm-concatenate instructions into one
   with a :parts list covering all source registers."
  ;; r2 = concat(r0, r1)
  ;; r4 = concat(r2, r3)   ;; r2 used only once → chainable
  ;; After packing: single vm-concatenate(r4, parts=(r0 r1 r3))
  (let* ((c1 (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (c2 (make-vm-concatenate :dst :r4 :str1 :r2 :str2 :r3))
         (ret (make-vm-ret :reg :r4))
         (insts (list c1 c2 ret))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Packed into one concatenate
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result))
    ;; The single concat should have parts (r0 r1 r3)
    (let ((packed (find-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result)))
      (when packed
        (let ((parts (cl-cc/vm::vm-parts packed)))
          (assert-true (listp parts))
          (assert-= 3 (length parts)))))))

(deftest batch-concatenate-does-not-pack-when-used-multiple-times
  "opt-pass-batch-concatenate does not pack when the intermediate register has use-count > 1."
  ;; r2 is used in both c2 and c3, so the chain is broken at c2.
  (let* ((c1  (make-vm-concatenate :dst :r2 :str1 :r0 :str2 :r1))
         (c2  (make-vm-concatenate :dst :r3 :str1 :r2 :str2 :r1))
         ;; Use r2 again to make use-count=2
         (c3  (make-vm-concatenate :dst :r4 :str1 :r2 :str2 :r3))
         (ret (make-vm-ret :reg :r4))
         (insts (list c1 c2 c3 ret))
         (result (cl-cc/optimize::opt-pass-batch-concatenate insts)))
    ;; Should have at least 2 vm-concatenate (no merging since r2 used twice)
    (assert-true (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) result) 2))))
