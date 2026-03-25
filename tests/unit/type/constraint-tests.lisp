;;;; tests/unit/type/constraint-tests.lisp — Constraint Language Tests
;;;;
;;;; Tests for src/type/constraint.lisp:
;;;; Smart constructors, constraint-free-vars, constraint-substitute.

(in-package :cl-cc/test)

(defsuite constraint-suite :description "OutsideIn(X) constraint language tests")

;;; ─── Smart constructors ────────────────────────────────────────────────────

(deftest constraint-equal-creation
  "make-equal-constraint produces (:equal t1 t2)."
  (let ((c (make-equal-constraint type-int type-string)))
    (assert-true (constraint-p c))
    (assert-eq :equal (constraint-kind c))
    (assert-equal 2 (length (constraint-args c)))
    (assert-true (type-equal-p type-int (first (constraint-args c))))
    (assert-true (type-equal-p type-string (second (constraint-args c))))))

(deftest constraint-subtype-creation
  "make-subtype-constraint produces (:subtype t1 t2)."
  (let ((c (make-subtype-constraint type-int type-any)))
    (assert-eq :subtype (constraint-kind c))
    (assert-true (type-equal-p type-int (first (constraint-args c))))))

(deftest constraint-typeclass-creation
  "make-typeclass-constraint produces (:typeclass class-name type)."
  (let ((c (make-typeclass-constraint 'num type-int)))
    (assert-eq :typeclass (constraint-kind c))
    (assert-eq 'num (first (constraint-args c)))
    (assert-true (type-equal-p type-int (second (constraint-args c))))))

(deftest constraint-implication-creation
  "make-implication-constraint stores quantified vars, given, and wanted."
  (let* ((v (fresh-type-var "a"))
         (given (list (make-equal-constraint v type-int)))
         (wanted (list (make-subtype-constraint v type-any)))
         (c (make-implication-constraint (list v) given wanted)))
    (assert-eq :implication (constraint-kind c))
    (assert-equal 3 (length (constraint-args c)))
    (assert-equal 1 (length (first (constraint-args c))))
    (assert-equal 1 (length (second (constraint-args c))))
    (assert-equal 1 (length (third (constraint-args c))))))

(deftest constraint-effect-subset-creation
  "make-effect-subset-constraint produces (:effect-subset e1 e2)."
  (let ((c (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+)))
    (assert-eq :effect-subset (constraint-kind c))))

(deftest constraint-mult-leq-creation
  "make-mult-leq-constraint produces (:mult-leq q1 q2)."
  (let ((c (make-mult-leq-constraint :zero :omega)))
    (assert-eq :mult-leq (constraint-kind c))
    (assert-eq :zero (first (constraint-args c)))
    (assert-eq :omega (second (constraint-args c)))))

(deftest constraint-row-lacks-creation
  "make-row-lacks-constraint produces (:row-lacks rho label)."
  (let* ((rv (fresh-type-var "rho"))
         (c (make-row-lacks-constraint rv 'x)))
    (assert-eq :row-lacks (constraint-kind c))
    (assert-eq 'x (second (constraint-args c)))))

(deftest constraint-kind-equal-creation
  "make-kind-equal-constraint produces (:kind-equal k1 k2)."
  (let ((c (make-kind-equal-constraint +kind-type+ +kind-type+)))
    (assert-eq :kind-equal (constraint-kind c))))

;;; ─── constraint-free-vars ──────────────────────────────────────────────────

(deftest constraint-free-vars-equal
  "Free vars of (:equal v1 v2) are {v1, v2}."
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (c (make-equal-constraint v1 v2))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 2 (length fvs))))

(deftest constraint-free-vars-equal-dedup
  "Free vars of (:equal v v) are {v} (deduplicated)."
  (let* ((v (fresh-type-var "a"))
         (c (make-equal-constraint v v))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 1 (length fvs))))

(deftest constraint-free-vars-subtype
  "Free vars of (:subtype v1 v2) are {v1, v2}."
  (let* ((v1 (fresh-type-var "x"))
         (v2 (fresh-type-var "y"))
         (c (make-subtype-constraint v1 v2))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 2 (length fvs))))

(deftest constraint-free-vars-typeclass
  "Free vars of (:typeclass C v) are just {v}."
  (let* ((v (fresh-type-var "a"))
         (c (make-typeclass-constraint 'eq v))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 1 (length fvs))))

(deftest constraint-free-vars-implication-binds
  "Implication quantified vars are removed from free vars."
  (let* ((v (fresh-type-var "a"))
         (c (make-implication-constraint
             (list v)
             (list (make-equal-constraint v type-int))
             (list (make-subtype-constraint v type-any))))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 0 (length fvs))))

(deftest constraint-free-vars-mult-leq-empty
  "(:mult-leq) has no type-vars."
  (let* ((c (make-mult-leq-constraint :one :omega))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-null fvs)))

(deftest constraint-free-vars-kind-equal-empty
  "(:kind-equal) has no type-vars."
  (let* ((c (make-kind-equal-constraint +kind-type+ +kind-effect+))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-null fvs)))

(deftest constraint-free-vars-row-lacks-var
  "(:row-lacks v label) has v as free."
  (let* ((v (fresh-type-var "rho"))
         (c (make-row-lacks-constraint v 'x))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal 1 (length fvs))))

(deftest constraint-free-vars-row-lacks-no-var
  "(:row-lacks non-type-node label) has no free vars."
  (let* ((c (make-row-lacks-constraint 'not-a-type 'x))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-null fvs)))

;;; ─── constraint-substitute ─────────────────────────────────────────────────

(deftest constraint-substitute-equal
  "Substituting in (:equal v int) with v→string yields (:equal string int)."
  (let* ((v (fresh-type-var "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-eq :equal (constraint-kind c2))
      (assert-true (type-equal-p type-string (first (constraint-args c2))))
      (assert-true (type-equal-p type-int (second (constraint-args c2)))))))

(deftest constraint-substitute-subtype
  "Substitution applies to both sides of (:subtype)."
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (c (make-subtype-constraint v1 v2))
         (s (make-substitution)))
    (subst-extend! v1 type-int s)
    (subst-extend! v2 type-any s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-true (type-equal-p type-int (first (constraint-args c2))))
      (assert-true (type-equal-p type-any (second (constraint-args c2)))))))

(deftest constraint-substitute-typeclass
  "Substitution applies only to type arg, not class name."
  (let* ((v (fresh-type-var "a"))
         (c (make-typeclass-constraint 'show v))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-eq 'show (first (constraint-args c2)))
      (assert-true (type-equal-p type-string (second (constraint-args c2)))))))

(deftest constraint-substitute-mult-leq-identity
  "(:mult-leq) substitution is identity."
  (let* ((c (make-mult-leq-constraint :one :omega))
         (s (make-substitution))
         (c2 (cl-cc/type::constraint-substitute c s)))
    (assert-eq c c2)))

(deftest constraint-substitute-kind-equal-identity
  "(:kind-equal) substitution is identity."
  (let* ((c (make-kind-equal-constraint +kind-type+ +kind-type+))
         (s (make-substitution))
         (c2 (cl-cc/type::constraint-substitute c s)))
    (assert-eq c c2)))

(deftest constraint-substitute-effect-subset
  "Substitution applies to both effect rows."
  (let* ((v (fresh-type-var "ε"))
         (c (make-effect-subset-constraint v +pure-effect-row+))
         (s (make-substitution)))
    (subst-extend! v +io-effect-row+ s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-eq :effect-subset (constraint-kind c2)))))
