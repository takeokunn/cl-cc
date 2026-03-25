;;;; tests/unit/type/solver-tests.lisp — Constraint Solver Tests
;;;;
;;;; Tests for src/type/solver.lisp:
;;;; solve-constraints, collect-constraints, make-constraint (compat alias).

(in-package :cl-cc/test)

(defsuite solver-suite :description "OutsideIn(X) constraint solver tests")

;;; ─── make-constraint backward compat ───────────────────────────────────────

(deftest solver-make-constraint-compat
  "make-constraint builds an equality constraint (backward compat)."
  (let ((c (make-constraint type-int type-string)))
    (assert-true (constraint-p c))
    (assert-eq :equal (constraint-kind c))))

;;; ─── solve-constraints: equality ───────────────────────────────────────────

(deftest solver-equality-trivial
  "Solving (v ~ int) extends subst with v→int."
  (let* ((v (fresh-type-var "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (assert-null residual)
      (let ((resolved (zonk v new-subst)))
        (assert-true (type-equal-p type-int resolved))))))

(deftest solver-equality-chain
  "Solving (v1 ~ v2) then (v2 ~ int) resolves both."
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (c1 (make-equal-constraint v1 v2))
         (c2 (make-equal-constraint v2 type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type::solve-constraints (list c1 c2) s)
      (assert-null residual)
      (assert-true (type-equal-p type-int (zonk v1 new-subst)))
      (assert-true (type-equal-p type-int (zonk v2 new-subst))))))

(deftest solver-equality-conflicting
  "Conflicting equalities (v ~ int, v ~ string) produce residual."
  (let* ((v (fresh-type-var "a"))
         (c1 (make-equal-constraint v type-int))
         (c2 (make-equal-constraint v type-string))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c1 c2) s)
      (declare (ignore _subst))
      (assert-true (> (length residual) 0)))))

(deftest solver-equality-same-type
  "Solving (int ~ int) succeeds with no residual."
  (let* ((c (make-equal-constraint type-int type-int))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

;;; ─── solve-constraints: subtyping ──────────────────────────────────────────

(deftest solver-subtype-satisfied
  "(:subtype int t) is satisfied — no residual."
  (let* ((c (make-subtype-constraint type-int type-any))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-subtype-violated
  "(:subtype string int) is not satisfied — becomes residual."
  (let* ((c (make-subtype-constraint type-string type-int))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

;;; ─── solve-constraints: typeclass ──────────────────────────────────────────

(deftest solver-typeclass-free-var-deferred
  "(:typeclass C v) with unresolved v becomes residual."
  (let* ((v (fresh-type-var "a"))
         (c (make-typeclass-constraint 'eq v))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

(deftest solver-typeclass-unknown-accepted
  "(:typeclass C unknown) is accepted (gradual typing)."
  (let* ((c (make-typeclass-constraint 'eq +type-unknown+))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

;;; ─── solve-constraints: effect-subset ──────────────────────────────────────

(deftest solver-effect-subset-satisfied
  "Pure ⊆ IO is satisfied."
  (let* ((c (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-effect-subset-violated
  "IO ⊆ Pure is not satisfied."
  (let* ((c (make-effect-subset-constraint +io-effect-row+ +pure-effect-row+))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

;;; ─── solve-constraints: mult-leq ──────────────────────────────────────────

(deftest solver-mult-leq-satisfied
  "(:mult-leq :zero :omega) is satisfied."
  (let* ((c (make-mult-leq-constraint :zero :omega))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-mult-leq-violated
  "(:mult-leq :omega :zero) is not satisfied."
  (let* ((c (make-mult-leq-constraint :omega :zero))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

;;; ─── solve-constraints: kind-equal ─────────────────────────────────────────

(deftest solver-kind-equal-satisfied
  "(:kind-equal * *) is satisfied."
  (let* ((c (make-kind-equal-constraint +kind-type+ +kind-type+))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-kind-equal-violated
  "(:kind-equal * Effect) is not satisfied."
  (let* ((c (make-kind-equal-constraint +kind-type+ +kind-effect+))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

;;; ─── solve-constraints: row-lacks ──────────────────────────────────────────

(deftest solver-row-lacks-open-var
  "(:row-lacks v x) with type-var v — no violation (not a record)."
  (let* ((v (fresh-type-var "rho"))
         (c (make-row-lacks-constraint v 'x))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-row-lacks-record-absent
  "Row-lacks satisfied when label is absent from record."
  (let* ((rec (make-type-record :fields (list (cons 'y type-int)) :row-var nil))
         (c (make-row-lacks-constraint rec 'x))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-row-lacks-record-present
  "Row-lacks violated when label IS present in record."
  (let* ((rec (make-type-record :fields (list (cons 'x type-int)) :row-var nil))
         (c (make-row-lacks-constraint rec 'x))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))

;;; ─── solve-constraints: implication ────────────────────────────────────────

(deftest solver-implication-solvable
  "Solvable implication: ∀a. (a ~ int) ⊃ (a ~ int) — no residual."
  (let* ((v (fresh-type-var "a"))
         (given (list (make-equal-constraint v type-int)))
         (wanted (list (make-equal-constraint v type-int)))
         (c (make-implication-constraint (list v) given wanted))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

;;; ─── solve-constraints: empty input ────────────────────────────────────────

(deftest solver-empty-constraints
  "Solving empty constraint list returns empty residual."
  (multiple-value-bind (subst residual)
      (cl-cc/type::solve-constraints nil nil)
    (assert-true (substitution-p subst))
    (assert-null residual)))

;;; ─── solve-constraints: nil subst ──────────────────────────────────────────

(deftest solver-nil-subst-creates-fresh
  "Passing nil subst creates a fresh substitution."
  (let* ((c (make-equal-constraint type-int type-int)))
    (multiple-value-bind (subst residual)
        (cl-cc/type::solve-constraints (list c) nil)
      (assert-true (substitution-p subst))
      (assert-null residual))))

;;; ─── solve-constraints: mixed ──────────────────────────────────────────────

(deftest solver-mixed-constraints
  "Mixed constraint list: some solved, some residual."
  (let* ((v (fresh-type-var "a"))
         (c1 (make-equal-constraint v type-int))            ; solvable
         (c2 (make-subtype-constraint type-string type-int)) ; violated → residual
         (c3 (make-mult-leq-constraint :zero :omega))       ; satisfied
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type::solve-constraints (list c1 c2 c3) s)
      (assert-true (type-equal-p type-int (zonk v new-subst)))
      (assert-equal 1 (length residual))
      (assert-eq :subtype (constraint-kind (first residual))))))
