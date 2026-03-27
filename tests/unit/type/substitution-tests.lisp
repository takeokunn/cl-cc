;;;; tests/unit/type/substitution-tests.lisp — Substitution & Zonking Tests
;;;
;;; Tests for substitution data structure, zonk on various type constructors,
;;; composition, occurs check, generalize/instantiate, and normalization.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Substitution Structure ─────────────────────────────────────────────

(deftest subst-make-empty
  "make-substitution creates a fresh substitution with generation 0."
  (let ((s (make-substitution)))
    (assert-= 0 (substitution-generation s))))

(deftest subst-lookup-empty-cases
  "Looking up in empty substitution or nil substitution both return nil/not-found."
  (let ((v (cl-cc/type:make-type-variable 'a)))
    (multiple-value-bind (v1 f1) (subst-lookup v (make-substitution))
      (assert-null v1)
      (assert-false f1))
    (multiple-value-bind (v2 f2) (subst-lookup v nil)
      (assert-null v2)
      (assert-false f2))))

(deftest subst-extend-functional-creates-new
  "subst-extend returns a new substitution, leaving original unchanged."
  (let* ((v (cl-cc/type:make-type-variable 'a))
         (s1 (make-substitution))
         (s2 (subst-extend v cl-cc/type:type-int s1)))
    (multiple-value-bind (val found) (subst-lookup v s1)
      (declare (ignore val))
      (assert-false found))
    (multiple-value-bind (val found) (subst-lookup v s2)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))))

(deftest subst-extend-from-nil
  "subst-extend with nil subst creates a fresh substitution."
  (let* ((v (cl-cc/type:make-type-variable 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (multiple-value-bind (val found) (subst-lookup v s)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))
    (assert-= 1 (substitution-generation s))))

(deftest subst-extend-bang-behavior
  "subst-extend! mutates in place and increments generation on each call."
  (let* ((v1 (cl-cc/type:make-type-variable 'a))
         (v2 (cl-cc/type:make-type-variable 'b))
         (s  (make-substitution)))
    (subst-extend! v1 cl-cc/type:type-int s)
    (multiple-value-bind (val found) (subst-lookup v1 s)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))
    (assert-= 1 (substitution-generation s))
    (subst-extend! v2 cl-cc/type:type-string s)
    (assert-= 2 (substitution-generation s))))

;;; ─── Composition ────────────────────────────────────────────────────────

(deftest subst-compose-nil-cases
  "Boundary cases when composing nil substitutions."
  (let ((s (subst-compose nil nil)))
    (assert-true (cl-cc/type:substitution-p s)))
  (let* ((v (cl-cc/type:make-type-variable 'a))
         (s2 (subst-extend v cl-cc/type:type-int nil))
         (result (subst-compose nil s2)))
    (assert-eq s2 result))
  (let* ((v (cl-cc/type:make-type-variable 'a))
         (s1 (subst-extend v cl-cc/type:type-int nil))
         (result (subst-compose s1 nil)))
    (multiple-value-bind (val found) (subst-lookup v result)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))))

(deftest subst-compose-applies-s1-to-s2-range
  "Composition applies s1 to the range of s2."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         ;; s2: a -> b
         (s2 (subst-extend a b nil))
         ;; s1: b -> int
         (s1 (subst-extend b cl-cc/type:type-int nil))
         ;; compose: a -> zonk(b, s1) = int
         (result (subst-compose s1 s2)))
    (multiple-value-bind (val found) (subst-lookup a result)
      (assert-true found)
      (assert-true (cl-cc/type:type-primitive-p val))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name val)))))

;;; ─── Zonk: Various Type Constructors ────────────────────────────────────

(deftest zonk-unchanged-types
  "Zonking nil, a primitive, or an unbound variable leaves them unchanged."
  (let ((s (make-substitution)))
    (assert-null (zonk nil s))
    (assert-eq cl-cc/type:type-int (zonk cl-cc/type:type-int s))
    (let ((v (cl-cc/type:make-type-variable 'a)))
      (assert-eq v (zonk v s)))))

(deftest zonk-bound-var-resolves
  "Zonking a bound variable resolves to its binding."
  (let* ((v (cl-cc/type:make-type-variable 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (let ((result (zonk v s)))
      (assert-eq cl-cc/type:type-int result))))

(deftest zonk-chain-resolves
  "Zonking follows variable chains: a->b->int."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         (s (make-substitution)))
    (subst-extend! a b s)
    (subst-extend! b cl-cc/type:type-int s)
    (let ((result (zonk a s)))
      (assert-true (cl-cc/type:type-primitive-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name result)))))

(deftest zonk-type-constructors
  "Zonking substitutes variables in all type constructor positions."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return b))
         (s (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk fn-ty s)))
      (assert-true (cl-cc/type:type-arrow-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name (car (cl-cc/type:type-arrow-params result))))
      (assert-eq 'string (cl-cc/type:type-primitive-name (cl-cc/type:type-arrow-return result)))))
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (prod (cl-cc/type:make-type-product :elems (list a cl-cc/type:type-string)))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (let ((result (zonk prod s)))
      (assert-true (cl-cc/type:type-product-p result))
      (assert-= 2 (length (cl-cc/type:type-product-elems result)))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                           (first (cl-cc/type:type-product-elems result))))))
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         (forall-ty (cl-cc/type:make-type-forall :var a :body b))
         (s (subst-extend b cl-cc/type:type-int nil)))
    (let ((result (zonk forall-ty s)))
      (assert-true (cl-cc/type:type-forall-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                           (cl-cc/type:type-forall-body result)))))
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         (app-ty (cl-cc/type:make-type-app :fun a :arg b))
         (s (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk app-ty s)))
      (assert-true (cl-cc/type:type-app-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name (cl-cc/type:type-app-fun result)))
      (assert-eq 'string (cl-cc/type:type-primitive-name (cl-cc/type:type-app-arg result)))))
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (union-ty (cl-cc/type:make-type-union-raw :types (list a cl-cc/type:type-string)))
         (s (subst-extend a cl-cc/type:type-int nil)))
    (let ((result (zonk union-ty s)))
      (assert-true (cl-cc/type:type-union-p result))
      (assert-= 2 (length (cl-cc/type:type-union-types result))))))

(deftest zonk-effect-row-cases
  "Zonking effect rows: substitutes row-var and merges resolved effect rows."
  (let* ((rv (cl-cc/type:make-type-variable 'r))
         (eff (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list eff) :row-var rv))
         (s (subst-extend rv (cl-cc/type:make-type-effect-row :effects nil :row-var nil) nil)))
    (let ((result (zonk row s)))
      (assert-true (cl-cc/type:type-effect-row-p result))))
  (let* ((rv (cl-cc/type:make-type-variable 'r))
         (eff1 (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (eff2 (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list eff1) :row-var rv))
         (row2 (cl-cc/type:make-type-effect-row :effects (list eff2) :row-var nil))
         (s (subst-extend rv row2 nil)))
    (let ((result (zonk row1 s)))
      (assert-true (cl-cc/type:type-effect-row-p result))
      (assert-= 2 (length (cl-cc/type:type-effect-row-effects result))))))

;;; ─── Occurs Check ───────────────────────────────────────────────────────

(deftest type-occurs-check
  "Occurs check: var in itself, in arrow, not in unrelated type, follows subst chains."
  (let ((s (make-substitution)))
    (let ((v (cl-cc/type:make-type-variable 'a)))
      (assert-true  (type-occurs-p v v s))
      (assert-true  (type-occurs-p v (cl-cc/type:make-type-arrow-raw :params (list v) :return cl-cc/type:type-int) s))
      (assert-false (type-occurs-p v cl-cc/type:type-int s))))
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (b (cl-cc/type:make-type-variable 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int))
         (s (subst-extend b fn-ty nil)))
    (assert-true (type-occurs-p a b s))))

;;; ─── Generalize / Instantiate ───────────────────────────────────────────

(deftest generalize-free-var-quantified
  "Free vars in type but not in env are quantified."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return a))
         (scheme (generalize nil fn-ty)))
    (assert-= 1 (length (cl-cc/type:type-scheme-quantified-vars scheme)))))

(deftest generalize-env-var-not-quantified
  "Vars free in env are not quantified."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (env (list (cons 'x a)))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int))
         (scheme (generalize env fn-ty)))
    (assert-= 0 (length (cl-cc/type:type-scheme-quantified-vars scheme)))))

(deftest instantiate-produces-fresh
  "Instantiate replaces quantified vars with fresh ones."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return a))
         (scheme (generalize nil fn-ty))
         (inst1 (instantiate scheme)))
    ;; Instantiation produces an arrow with fresh vars
    (assert-true (cl-cc/type:type-arrow-p inst1))
    (let ((p1 (car (cl-cc/type:type-arrow-params inst1)))
          (r1 (cl-cc/type:type-arrow-return inst1)))
      ;; Param and return should be the same fresh var (since both were 'a')
      (assert-true (cl-cc/type:type-var-p p1))
      (assert-true (cl-cc/type:type-var-equal-p p1 r1))
      ;; Fresh var should be different from the original quantified var
      (assert-false (cl-cc/type:type-var-equal-p p1 a)))))

;;; ─── Normalize ──────────────────────────────────────────────────────────

(deftest normalize-type-variables-behavior
  "normalize-type-variables: renames vars to canonical a,b,c,...; same var in two positions gets same canonical."
  (let* ((fn (cl-cc/type:make-type-arrow-raw
              :params (list (cl-cc/type:make-type-variable 'xyz))
              :return (cl-cc/type:make-type-variable 'qqq)))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (let ((p (car (cl-cc/type:type-arrow-params normed)))
          (r (cl-cc/type:type-arrow-return normed)))
      (assert-true  (cl-cc/type:type-var-p p))
      (assert-true  (cl-cc/type:type-var-p r))
      (assert-false (cl-cc/type:type-var-equal-p p r))))
  (let* ((v (cl-cc/type:make-type-variable 'x))
         (fn (cl-cc/type:make-type-arrow-raw :params (list v) :return v))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (assert-true (cl-cc/type:type-var-equal-p
                  (car (cl-cc/type:type-arrow-params normed))
                  (cl-cc/type:type-arrow-return normed)))))

;;; ─── apply-subst (env) ─────────────────────────────────────────────────

(deftest apply-subst-env-substitutes-bindings
  "apply-subst substitutes in environment bindings."
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (env (list (cons 'x a) (cons 'y cl-cc/type:type-string)))
         (s (subst-extend a cl-cc/type:type-int nil))
         (result (cl-cc/type:apply-subst env s)))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name (cdr (first result))))
    (assert-eq 'string (cl-cc/type:type-primitive-name (cdr (second result))))))
