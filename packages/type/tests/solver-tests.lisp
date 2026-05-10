;;;; tests/unit/type/solver-tests.lisp — Constraint Solver Tests
;;;;
;;;; Tests for src/type/solver.lisp:
;;;; solve-constraints and collect-constraints.

(in-package :cl-cc/test)

(defsuite solver-suite
  :description "OutsideIn(X) constraint solver tests"
  :parent cl-cc-unit-suite)

(in-suite solver-suite)

;;; ─── solve-constraints: equality ───────────────────────────────────────────

(deftest solver-equality-trivial
  "Solving (v ~ int) extends subst with v→int."
  (let* ((v (fresh-type-var :name "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (assert-null residual)
      (let ((resolved (zonk v new-subst)))
        (assert-true (type-equal-p type-int resolved))))))

(deftest solver-equality-chain
  "Solving (v1 ~ v2) then (v2 ~ int) resolves both."
  (let* ((v1 (fresh-type-var :name "a"))
         (v2 (fresh-type-var :name "b"))
         (c1 (make-equal-constraint v1 v2))
         (c2 (make-equal-constraint v2 type-int))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c1 c2) s)
      (assert-null residual)
      (assert-true (type-equal-p type-int (zonk v1 new-subst)))
      (assert-true (type-equal-p type-int (zonk v2 new-subst))))))

(deftest solver-bounded-type-var-equality
  "Bounded type variables accept bindings inside bounds and reject violations."
  (let* ((number-type (make-type-primitive :name 'number))
         (upper-ok (fresh-type-var :name "a" :upper-bound number-type))
         (upper-bad (fresh-type-var :name "b" :upper-bound number-type))
         (lower-ok (fresh-type-var :name "c" :lower-bound type-int))
         (lower-bad (fresh-type-var :name "d" :lower-bound type-int)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint upper-ok type-int))
         (make-substitution))
      (declare (ignore _subst))
      (assert-null residual))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint upper-bad type-string))
         (make-substitution))
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint lower-ok type-any))
         (make-substitution))
      (declare (ignore _subst))
      (assert-null residual))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints
         (list (make-equal-constraint lower-bad type-string))
         (make-substitution))
      (declare (ignore _subst))
      (assert-equal 1 (length residual)))))


(deftest solver-bounded-type-var-propagates-through-var
  "Variable-to-variable unification preserves bounds on the surviving variable."
  (let* ((number-type (make-type-primitive :name 'number))
         (bounded (fresh-type-var :name "a" :upper-bound number-type))
         (survivor (fresh-type-var :name "b")))
    (multiple-value-bind (subst ok)
        (cl-cc/type:type-unify bounded survivor (make-substitution))
      (assert-true ok)
      (assert-true (type-equal-p number-type (cl-cc/type:type-var-upper-bound survivor)))
      (multiple-value-bind (_bad bad-ok)
          (cl-cc/type:type-unify survivor type-string subst)
        (declare (ignore _bad))
        (assert-false bad-ok))
      (multiple-value-bind (_good good-ok)
          (cl-cc/type:type-unify survivor type-int subst)
        (declare (ignore _good))
        (assert-true good-ok)))))

(deftest solver-instantiate-preserves-bounded-quantifier
  "Instantiating a type scheme copies upper/lower bounds onto fresh variables."
  (let* ((number-type (make-type-primitive :name 'number))
         (qvar (fresh-type-var :name "a"
                               :upper-bound number-type
                               :lower-bound type-int))
         (scheme (make-type-scheme (list qvar) qvar))
         (instantiated (instantiate scheme)))
    (assert-true (type-var-p instantiated))
    (assert-false (type-var-equal-p qvar instantiated))
    (assert-true (type-equal-p number-type
                               (cl-cc/type:type-var-upper-bound instantiated)))
    (assert-true (type-equal-p type-int
                               (cl-cc/type:type-var-lower-bound instantiated)))))

(deftest solver-conflicting-equalities-produce-residual
  "Conflicting equalities (v~int and v~string) leave a non-empty residual."
  (let* ((v  (fresh-type-var :name "a"))
         (c1 (make-equal-constraint v type-int))
         (c2 (make-equal-constraint v type-string))
         (s  (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c1 c2) s)
      (declare (ignore _subst))
      (assert-true (> (length residual) 0)))))

(deftest solver-trivial-equality-has-no-residual
  "Trivial equality (int~int) produces no residual."
  (let* ((c (make-equal-constraint type-int type-int))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

;;; ─── solve-constraints: subtyping ──────────────────────────────────────────

(deftest-each solver-binary-constraint-kinds
  "Each constraint kind: satisfying case produces no residual; violating case produces 1."
  :cases (("subtype"       (make-subtype-constraint type-int type-any)
                           (make-subtype-constraint type-string type-int))
          ("typeclass"     (make-typeclass-constraint 'eq cl-cc/type:+type-unknown+)
                           (make-typeclass-constraint 'eq (fresh-type-var :name "a")))
          ("effect-subset" (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+)
                           (make-effect-subset-constraint +io-effect-row+ +pure-effect-row+))
          ("mult-leq"      (make-mult-leq-constraint :zero :omega)
                           (make-mult-leq-constraint :omega :zero))
          ("kind-equal"    (make-kind-equal-constraint +kind-type+ +kind-type+)
                           (make-kind-equal-constraint +kind-type+ +kind-effect+)))
  (sat-c viol-c)
  (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (assert-null   (residuals sat-c))
    (assert-equal 1 (length (residuals viol-c)))))

;;; ─── solve-constraints: row-lacks ──────────────────────────────────────────

(deftest solver-row-lacks-constraints
  "Row-lacks: open-var no residual; label absent no residual; label present → 1 residual."
  (flet ((residuals (c)
           (nth-value 1 (cl-cc/type:solve-constraints (list c) (make-substitution)))))
    (assert-null (residuals (make-row-lacks-constraint (fresh-type-var :name "rho") 'x)))
    (assert-null (residuals (make-row-lacks-constraint
                              (make-type-record :fields (list (cons 'y type-int)) :row-var nil) 'x)))
    (assert-equal 1 (length (residuals (make-row-lacks-constraint
                                         (make-type-record :fields (list (cons 'x type-int)) :row-var nil) 'x))))))

;;; ─── solve-constraints: implication ────────────────────────────────────────

(deftest solver-implication-solvable
  "Solvable implication: ∀a. (a ~ int) ⊃ (a ~ int) — no residual."
  (let* ((v (fresh-type-var :name "a"))
         (given (list (make-equal-constraint v type-int)))
         (wanted (list (make-equal-constraint v type-int)))
         (c (make-implication-constraint (list v) given wanted))
         (s (make-substitution)))
    (multiple-value-bind (_subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (declare (ignore _subst))
      (assert-null residual))))

(deftest solver-defaults-numeric-typeclass-vars
  "Unresolved numeric typeclass variables default to the configured numeric type."
  (let* ((v (fresh-type-var :name "a"))
         (c (make-typeclass-constraint 'num v))
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c) s)
      (assert-null residual)
      (assert-true (type-equal-p type-int (zonk v new-subst))))))

;;; ─── solve-constraints: empty input ────────────────────────────────────────

(deftest solver-empty-and-nil-subst
  "Empty constraint list and nil subst both return a valid substitution with no residual."
  (multiple-value-bind (subst residual)
      (cl-cc/type:solve-constraints nil nil)
    (assert-true (substitution-p subst))
    (assert-null residual))
  (multiple-value-bind (subst residual)
      (cl-cc/type:solve-constraints (list (make-equal-constraint type-int type-int)) nil)
    (assert-true (substitution-p subst))
    (assert-null residual)))

;;; ─── solve-constraints: mixed ──────────────────────────────────────────────

(deftest solver-mixed-constraints
  "Mixed constraint list: some solved, some residual."
  (let* ((v (fresh-type-var :name "a"))
         (c1 (make-equal-constraint v type-int))            ; solvable
         (c2 (make-subtype-constraint type-string type-int)) ; violated → residual
         (c3 (make-mult-leq-constraint :zero :omega))       ; satisfied
         (s (make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints (list c1 c2 c3) s)
      (assert-true (type-equal-p type-int (zonk v new-subst)))
      (assert-equal 1 (length residual))
      (assert-eq :subtype (constraint-kind (first residual))))))

(deftest solve-constraints-trivial-success
  "solve-constraints returns a valid substitution for empty list and identical-primitive equality."
  (let* ((subst  (cl-cc/type:make-substitution))
         (result (cl-cc/type:solve-constraints nil subst)))
    (assert-true (cl-cc/type:substitution-p result)))
  (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints
       (list (cl-cc/type:make-equal-constraint type-int type-int))
       (cl-cc/type:make-substitution))
    (assert-true  (cl-cc/type:substitution-p new-subst))
    (assert-null  residual)))

(deftest solve-constraints-equal-binds-var
  "An :equal constraint on (?a ~ int) binds ?a to int."
  (let* ((tvar  (cl-cc/type:fresh-type-var "a"))
         (subst (cl-cc/type:make-substitution)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint tvar type-int))
         subst)
      (assert-null residual)
      (let ((bound (cl-cc/type:zonk tvar new-subst)))
        (assert-true (type-equal-p type-int bound))))))

(deftest-each solve-constraints-produces-residual
  "Unsolvable constraints (type mismatch and subtype violation) each produce exactly one residual."
  :cases (("equal-mismatch"    (cl-cc/type:make-equal-constraint type-int type-string))
          ("subtype-violation" (cl-cc/type:make-subtype-constraint type-string type-int)))
  (c)
  (multiple-value-bind (new-subst residual)
      (cl-cc/type:solve-constraints (list c) (cl-cc/type:make-substitution))
    (declare (ignore new-subst))
    (assert-= 1 (length residual))))

(deftest-each solve-constraints-subtype-ok
  "Subtype constraints for valid relationships produce no residual."
  :cases (("fixnum<integer" 'fixnum 'integer)
          ("integer<number" 'integer 'number)
          ("float<real"     'float   'real))
  (sub-name super-name)
  (let ((t1 (make-type-primitive :name sub-name))
        (t2 (make-type-primitive :name super-name)))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-subtype-constraint t1 t2))
         (cl-cc/type:make-substitution))
      (declare (ignore new-subst))
      (assert-null residual))))

(deftest solve-constraints-multiple-sequential
  "Multiple equality constraints are solved in sequence."
  (let* ((ta    (cl-cc/type:fresh-type-var "a"))
         (tb    (cl-cc/type:fresh-type-var "b")))
    (multiple-value-bind (new-subst residual)
        (cl-cc/type:solve-constraints
         (list (cl-cc/type:make-equal-constraint ta type-int)
               (cl-cc/type:make-equal-constraint tb ta))
         (cl-cc/type:make-substitution))
      (assert-null residual)
      (assert-true (type-equal-p type-int (cl-cc/type:zonk ta new-subst)))
      (assert-true (type-equal-p type-int (cl-cc/type:zonk tb new-subst))))))
