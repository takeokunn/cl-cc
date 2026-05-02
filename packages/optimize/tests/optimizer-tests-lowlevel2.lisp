(in-package :cl-cc/test)

;;; ── CSE (Common Subexpression Elimination) Unit Tests ──────────────────────

(deftest-each cse-dedup
  "Duplicate vm-add — same or commuted operands — CSE's second to vm-move."
  :cases (("same-order"    (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          ("swapped-order" (cl-cc:make-vm-add :dst :R3 :lhs :R1 :rhs :R0)))
  (i4)
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc:vm-add-p (third out)))
    (assert-true (cl-cc:vm-move-p (fourth out)))
    (assert-equal :R2 (cl-cc/vm::vm-src (fourth out)))))

(deftest cse-label-flushes
  "A branch-target vm-label between two identical vm-add prevents CSE."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
          (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
          (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
          (j  (cl-cc:make-vm-jump :label "L1"))
          (lbl (cl-cc:make-vm-label :name "L1"))
          (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 j lbl i4))))
    (assert-true (cl-cc:vm-add-p (third out)))
    (assert-true (cl-cc:vm-jump-p (fourth out)))
    (assert-true (cl-cc:vm-label-p (fifth out)))
    (assert-true (cl-cc:vm-add-p (sixth out)))))

(deftest cse-fallthrough-label-preserves-state
  "A non-target fallthrough label does not flush CSE state."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (lbl (cl-cc:make-vm-label :name "L1"))
         (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 lbl i4))))
    (assert-true (cl-cc:vm-label-p (fourth out)))
    (assert-true (cl-cc:vm-move-p (fifth out)))))

(deftest cse-unary-dedup
  "Two identical vm-neg: second becomes vm-move."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc:make-vm-neg :dst :R1 :src :R0))
         (i3 (cl-cc:make-vm-neg :dst :R2 :src :R0))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3))))
    (assert-true (cl-cc:vm-neg-p (second out)))
    (assert-true (cl-cc:vm-move-p (third out)))
    (assert-equal :R1 (cl-cc/vm::vm-src (third out)))))

(deftest cse-const-no-move
  "Two identical vm-const: second remains vm-const, not vm-move."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 42))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2))))
    (assert-true (cl-cc:vm-const-p (first out)))
    (assert-true (cl-cc:vm-const-p (second out)))))

(deftest cse-different-ops-no-dedup
  "vm-add and vm-sub with same operands are not CSE'd."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc:make-vm-sub :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc:vm-add-p (third out)))
    (assert-true (cl-cc:vm-sub-p (fourth out)))))

(deftest-each optimizer-value-ordering-structural
  "Structural optimizer ordering stays deterministic without format-based comparison."
  :cases (("pair-ascending"    t   '(:r0 . 1) '(:r0 . 2))
          ("keyword-ascending" t   :r0        :r1)
          ("integer-ascending" t   1          2)
          ("string-descending" nil "b"        "a"))
  (expected a b)
  (assert-equal expected (not (null (cl-cc/optimize::%opt-value< a b)))))

(deftest optimizer-gvn-dominates-branch
  "GVN reuses a dominator-block value inside a dominated block."
  (let* ((i1 (cl-cc:make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc:make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc:make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (j  (cl-cc:make-vm-jump :label "L1"))
         (l  (cl-cc:make-vm-label :name "L1"))
         (i4 (cl-cc:make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (r  (cl-cc:make-vm-ret :reg :R3))
         (out (cl-cc/optimize::opt-pass-gvn (list i1 i2 i3 j l i4 r))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out))
     (assert-true (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-move)
                               (eq :R3 (cl-cc/vm::vm-dst i))))
                        out))))
