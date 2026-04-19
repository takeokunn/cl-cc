;;;; tests/unit/type/constraint-tests.lisp — Constraint Language Tests
;;;;
;;;; Tests for src/type/constraint.lisp:
;;;; Smart constructors, constraint-free-vars, constraint-substitute.

(in-package :cl-cc/test)

(defsuite constraint-suite :description "OutsideIn(X
  :parent cl-cc-unit-suite) 
(in-suite constraint-suite)
constraint language tests")

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

(deftest constraint-ground-kinds-creation
  "effect-subset, kind-equal, mult-leq, and row-lacks have the correct :kind and :args."
  (assert-eq :effect-subset (constraint-kind
                              (make-effect-subset-constraint +pure-effect-row+ +io-effect-row+)))
  (assert-eq :kind-equal    (constraint-kind
                              (make-kind-equal-constraint +kind-type+ +kind-type+)))
  (let ((c (make-mult-leq-constraint :zero :omega)))
    (assert-eq :mult-leq (constraint-kind c))
    (assert-eq :zero  (first  (constraint-args c)))
    (assert-eq :omega (second (constraint-args c))))
  (let* ((rv (fresh-type-var "rho"))
         (c  (make-row-lacks-constraint rv 'x)))
    (assert-eq :row-lacks (constraint-kind c))
    (assert-eq 'x (second (constraint-args c)))))

;;; ─── constraint-free-vars ──────────────────────────────────────────────────

(deftest-each constraint-free-vars-binary-constraints
  "Binary constraints (:equal and :subtype) each have 2 free vars when both args are distinct vars."
  :cases (("equal"   (let* ((v1 (fresh-type-var "a")) (v2 (fresh-type-var "b"))) (make-equal-constraint v1 v2)))
          ("subtype" (let* ((v1 (fresh-type-var "x")) (v2 (fresh-type-var "y"))) (make-subtype-constraint v1 v2))))
  (c)
  (assert-equal 2 (length (cl-cc/type::constraint-free-vars c))))

(deftest constraint-free-vars-dedup-and-binding
  "(:equal v v) deduplicates to 1 var; (:typeclass C v) yields 1; implication quantifies away its vars."
  (let* ((v  (fresh-type-var "a"))
         (c1 (make-equal-constraint v v)))
    (assert-equal 1 (length (cl-cc/type::constraint-free-vars c1))))
  (let* ((v  (fresh-type-var "a"))
         (c2 (make-typeclass-constraint 'eq v)))
    (assert-equal 1 (length (cl-cc/type::constraint-free-vars c2))))
  (let* ((v  (fresh-type-var "a"))
         (c3 (make-implication-constraint
              (list v)
              (list (make-equal-constraint v type-int))
              (list (make-subtype-constraint v type-any)))))
    (assert-equal 0 (length (cl-cc/type::constraint-free-vars c3)))))

(deftest constraint-free-vars-ground-types-empty
  "Ground-type-only constraints (:mult-leq, :kind-equal) have no type-vars."
  (assert-null (cl-cc/type::constraint-free-vars (make-mult-leq-constraint :one :omega)))
  (assert-null (cl-cc/type::constraint-free-vars (make-kind-equal-constraint +kind-type+ +kind-effect+))))

(deftest-each constraint-free-vars-row-lacks
  "(:row-lacks rho x) has rho free; non-type-node has no free vars."
  :cases (("with-var"    (fresh-type-var "rho") 1)
          ("without-var" 'not-a-type             0))
  (rho-val expected-count)
  (let* ((c (make-row-lacks-constraint rho-val 'x))
         (fvs (cl-cc/type::constraint-free-vars c)))
    (assert-equal expected-count (length fvs))))

;;; ─── constraint-substitute ─────────────────────────────────────────────────

(deftest constraint-substitute-typed-cases
  "constraint-substitute applies to :equal, :subtype, and :typeclass (not class name)."
  (let* ((v (fresh-type-var "a"))
         (c (make-equal-constraint v type-int))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-eq :equal (constraint-kind c2))
      (assert-true (type-equal-p type-string (first (constraint-args c2))))
      (assert-true (type-equal-p type-int (second (constraint-args c2))))))
  (let* ((v1 (fresh-type-var "a"))
         (v2 (fresh-type-var "b"))
         (c  (make-subtype-constraint v1 v2))
         (s  (make-substitution)))
    (subst-extend! v1 type-int s)
    (subst-extend! v2 type-any s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-true (type-equal-p type-int (first (constraint-args c2))))
      (assert-true (type-equal-p type-any (second (constraint-args c2))))))
  (let* ((v  (fresh-type-var "a"))
         (c  (make-typeclass-constraint 'show v))
         (s  (make-substitution)))
    (subst-extend! v type-string s)
    (let ((c2 (cl-cc/type::constraint-substitute c s)))
      (assert-eq 'show (first (constraint-args c2)))
      (assert-true (type-equal-p type-string (second (constraint-args c2)))))))

(deftest constraint-substitute-ground-and-effect
  "Ground constraints (:mult-leq, :kind-equal) are identity; :effect-subset applies to rows."
  (let ((s (make-substitution)))
    (let ((c (make-mult-leq-constraint :one :omega)))
      (assert-eq c (cl-cc/type::constraint-substitute c s)))
    (let ((c (make-kind-equal-constraint +kind-type+ +kind-type+)))
      (assert-eq c (cl-cc/type::constraint-substitute c s))))
  (let* ((v  (fresh-type-var "ε"))
         (c  (make-effect-subset-constraint v +pure-effect-row+))
         (s  (make-substitution)))
    (subst-extend! v +io-effect-row+ s)
    (assert-eq :effect-subset (constraint-kind (cl-cc/type::constraint-substitute c s)))))

(deftest-each constraint-kind-check
  "Each constraint constructor produces the expected :kind keyword."
  :cases (("subtype"       :subtype
           (cl-cc/type:make-subtype-constraint type-int type-any))
          ("typeclass"     :typeclass
           (cl-cc/type:make-typeclass-constraint 'num (cl-cc/type:fresh-type-var "a")))
          ("implication"   :implication
           (let* ((tv (cl-cc/type:fresh-type-var "a"))
                  (eq-c (cl-cc/type:make-equal-constraint tv type-int))
                  (tc-c (cl-cc/type:make-typeclass-constraint 'num tv)))
             (cl-cc/type:make-implication-constraint (list tv) (list eq-c) (list tc-c))))
          ("effect-subset" :effect-subset
           (cl-cc/type:make-effect-subset-constraint
            cl-cc/type:+pure-effect-row+ cl-cc/type:+io-effect-row+))
          ("kind-equal"    :kind-equal
           (cl-cc/type:make-kind-equal-constraint
            cl-cc/type:+kind-type+ cl-cc/type:+kind-type+))
          ("mult-leq"      :mult-leq
           (cl-cc/type:make-mult-leq-constraint :one :omega))
          ("row-lacks"     :row-lacks
           (cl-cc/type:make-row-lacks-constraint (cl-cc/type:fresh-type-var "r") 'x)))
  (expected-kind c)
  (assert-eq expected-kind (cl-cc/type:constraint-kind c)))

(deftest constraint-free-vars-count
  "constraint-free-vars finds the correct number of vars in equality and typeclass constraints."
  (let* ((tv1   (cl-cc/type:fresh-type-var "a"))
         (tv2   (cl-cc/type:fresh-type-var "b"))
         (ceq   (cl-cc/type:make-equal-constraint tv1 tv2))
         (ctc   (cl-cc/type:make-typeclass-constraint 'num tv1)))
    (assert-equal 2 (length (cl-cc/type:constraint-free-vars ceq)))
    (assert-equal 1 (length (cl-cc/type:constraint-free-vars ctc)))))

(deftest-each constraint-free-vars-zero-vars
  "constraint-free-vars returns nil for ground constraints and fully-quantified implications."
  :cases (("mult-leq"       (cl-cc/type:make-mult-leq-constraint :one :omega))
          ("kind-equal"     (cl-cc/type:make-kind-equal-constraint
                             cl-cc/type:+kind-type+ cl-cc/type:+kind-effect+))
          ("implication-quantified"
           (let* ((tv    (cl-cc/type:fresh-type-var "a"))
                  (inner (cl-cc/type:make-equal-constraint tv type-int)))
             (cl-cc/type:make-implication-constraint (list tv) (list inner) (list inner)))))
  (c)
  (assert-null (cl-cc/type:constraint-free-vars c)))

(deftest constraint-substitute
  "constraint-substitute applies substitution to typed args; is identity for ground constraints."
  (let* ((tv    (cl-cc/type:fresh-type-var "a"))
         (c     (cl-cc/type:make-equal-constraint tv type-string))
         (subst (cl-cc/type:subst-extend tv type-int (cl-cc/type:make-substitution)))
         (c2    (cl-cc/type:constraint-substitute c subst)))
    (assert-eq :equal (cl-cc/type:constraint-kind c2))
    (assert-true (type-equal-p type-int (first (cl-cc/type:constraint-args c2)))))
  (let* ((c     (cl-cc/type:make-mult-leq-constraint :one :omega))
         (c2    (cl-cc/type:constraint-substitute c (cl-cc/type:make-substitution))))
    (assert-eq c c2)))
