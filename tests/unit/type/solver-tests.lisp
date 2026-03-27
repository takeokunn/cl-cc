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

(deftest solver-equality-edge-cases
  "Conflicting equalities (v~int, v~string) produce residual; trivial (int~int) has none."
  (let* ((v  (fresh-type-var "a"))
         (c1 (make-equal-constraint v type-int))
         (c2 (make-equal-constraint v type-string))
         (s  (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c1 c2) s)
      (declare (ignore _subst))
      (assert-true (> (length residual) 0))))
  (let* ((c (make-equal-constraint type-int type-int))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type::solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

;;; ─── solve-constraints: subtyping ──────────────────────────────────────────

(deftest-each solver-binary-constraint-kinds
  "Each constraint kind: satisfying case produces no residual; violating case produces 1."
  :cases (("subtype"       (make-subtype-constraint type-int type-any)
                           (make-subtype-constraint type-string type-int))
          ("typeclass"     (make-typeclass-constraint 'eq +type-unknown+)
                           (make-typeclass-constraint 'eq (fresh-type-var "a")))
          ("effect-subset" (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+)
                           (make-effect-subset-constraint +io-effect-row+ +pure-effect-row+))
          ("mult-leq"      (make-mult-leq-constraint :zero :omega)
                           (make-mult-leq-constraint :omega :zero))
          ("kind-equal"    (make-kind-equal-constraint +kind-type+ +kind-type+)
                           (make-kind-equal-constraint +kind-type+ +kind-effect+)))
  (sat-c viol-c)
  (flet ((residuals (c)
           (nth-value 1 (cl-cc/type::solve-constraints (list c) (make-substitution)))))
    (assert-null   (residuals sat-c))
    (assert-equal 1 (length (residuals viol-c)))))

;;; ─── solve-constraints: row-lacks ──────────────────────────────────────────

(deftest solver-row-lacks-constraints
  "Row-lacks: open-var no residual; label absent no residual; label present → 1 residual."
  (flet ((residuals (c)
           (nth-value 1 (cl-cc/type::solve-constraints (list c) (make-substitution)))))
    (assert-null (residuals (make-row-lacks-constraint (fresh-type-var "rho") 'x)))
    (assert-null (residuals (make-row-lacks-constraint
                              (make-type-record :fields (list (cons 'y type-int)) :row-var nil) 'x)))
    (assert-equal 1 (length (residuals (make-row-lacks-constraint
                                         (make-type-record :fields (list (cons 'x type-int)) :row-var nil) 'x))))))

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

(deftest solver-empty-and-nil-subst
  "Empty constraint list and nil subst both return a valid substitution with no residual."
  (multiple-value-bind (subst residual)
      (cl-cc/type::solve-constraints nil nil)
    (assert-true (substitution-p subst))
    (assert-null residual))
  (multiple-value-bind (subst residual)
      (cl-cc/type::solve-constraints (list (make-equal-constraint type-int type-int)) nil)
    (assert-true (substitution-p subst))
    (assert-null residual)))

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
