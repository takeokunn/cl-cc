;;;; tests/unit/type/substitution-tests.lisp — Substitution & Zonking Tests
;;;
;;; Tests for substitution data structure, zonk on various type constructors,
;;; composition, occurs check, generalize/instantiate, and normalization.

(in-package :cl-cc/test)
(in-suite cl-cc-type-serial-suite)

;;; ─── Substitution Structure ─────────────────────────────────────────────

(deftest-each subst-lookup-empty-cases
  "Looking up in empty substitution or nil substitution both return nil/not-found."
  :cases (("empty-subst" (make-substitution))
          ("nil-subst"   nil))
  (subst)
  (let ((v (cl-cc/type:fresh-type-var 'a)))
    (multiple-value-bind (val found-p) (subst-lookup v subst)
      (assert-null val)
      (assert-false found-p))))

(deftest subst-make-substitution-starts-at-generation-0
  "make-substitution creates a fresh substitution at generation 0."
  (let ((s (make-substitution)))
    (assert-= 0 (substitution-generation s))))

(deftest subst-extend-leaves-original-unchanged
  "subst-extend returns a new substitution; the original is not modified."
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s1 (make-substitution))
         (s2 (subst-extend v cl-cc/type:type-int s1)))
    (multiple-value-bind (val found) (subst-lookup v s1)
      (declare (ignore val))
      (assert-false found))
    (multiple-value-bind (val found) (subst-lookup v s2)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))))

(deftest subst-extend-to-nil-creates-generation-1
  "subst-extend onto nil creates a substitution at generation 1 with the binding."
  (let* ((v (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (multiple-value-bind (val found) (subst-lookup v s)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))
    (assert-= 1 (substitution-generation s))))

(deftest subst-extend!-mutates-and-increments-generation
  "subst-extend! mutates the substitution in place; generation increments on each call."
  (let* ((v1 (cl-cc/type:fresh-type-var 'a))
         (v2 (cl-cc/type:fresh-type-var 'b))
         (s  (make-substitution)))
    (subst-extend! v1 cl-cc/type:type-int s)
    (multiple-value-bind (val found) (subst-lookup v1 s)
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))
    (assert-= 1 (substitution-generation s))
    (subst-extend! v2 cl-cc/type:type-string s)
    (assert-= 2 (substitution-generation s))))

;;; ─── Composition ────────────────────────────────────────────────────────

(deftest subst-compose-nil-nil-returns-valid-substitution
  "subst-compose of nil and nil returns a valid (empty) substitution."
  (let ((s (subst-compose nil nil)))
    (assert-true (cl-cc/type:substitution-p s))))

(deftest subst-compose-nil-left-returns-s2
  "subst-compose nil+s2 is identity on s2 (returns s2 directly)."
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s2 (subst-extend v cl-cc/type:type-int nil)))
    (assert-eq s2 (subst-compose nil s2))))

(deftest subst-compose-nil-right-preserves-bindings
  "subst-compose s1+nil preserves all bindings from s1."
  (let* ((v  (cl-cc/type:fresh-type-var 'a))
         (s1 (subst-extend v cl-cc/type:type-int nil)))
    (multiple-value-bind (val found) (subst-lookup v (subst-compose s1 nil))
      (assert-true found)
      (assert-eq cl-cc/type:type-int val))))

(deftest subst-compose-resolves-chains-through-s2-range
  "subst-compose: s1 is applied to s2's range, resolving variable chains."
  (let* ((a      (cl-cc/type:fresh-type-var 'a))
         (b      (cl-cc/type:fresh-type-var 'b))
         (s2     (subst-extend a b nil))
         (s1     (subst-extend b cl-cc/type:type-int nil))
         (result (subst-compose s1 s2)))
    (multiple-value-bind (val found) (subst-lookup a result)
      (assert-true found)
      (assert-true (cl-cc/type:type-primitive-p val))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name val)))))

;;; ─── Zonk: Various Type Constructors ────────────────────────────────────

(deftest zonk-nil-primitive-and-unbound-are-unchanged
  "zonk: nil, a primitive type, and an unbound type variable all pass through unchanged."
  (let ((s (make-substitution)))
    (assert-null (zonk nil s))
    (assert-eq cl-cc/type:type-int (zonk cl-cc/type:type-int s))
    (let ((v (cl-cc/type:fresh-type-var 'a)))
      (assert-eq v (zonk v s)))))

(deftest zonk-bound-var-resolves-to-bound-type
  "zonk: a type variable bound in the substitution resolves to the bound type."
  (let* ((v (cl-cc/type:fresh-type-var 'a))
         (s (subst-extend v cl-cc/type:type-int nil)))
    (assert-eq cl-cc/type:type-int (zonk v s))))

(deftest zonk-chain-resolves-to-terminal
  "zonk: a variable chain a→b→int fully resolves to the terminal primitive type."
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (s (make-substitution)))
    (subst-extend! a b s)
    (subst-extend! b cl-cc/type:type-int s)
    (let ((result (zonk a s)))
      (assert-true (cl-cc/type:type-primitive-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name result)))))

(deftest zonk-arrow-type-substitutes-params-and-return
  "zonk: type-arrow variables in params and return are substituted correctly."
  (let* ((a     (cl-cc/type:fresh-type-var 'a))
         (b     (cl-cc/type:fresh-type-var 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return b))
         (s     (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk fn-ty s)))
      (assert-true (cl-cc/type:type-arrow-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name (car (cl-cc/type:type-arrow-params result))))
      (assert-eq 'string (cl-cc/type:type-primitive-name (cl-cc/type:type-arrow-return result))))))

(deftest zonk-product-type-substitutes-elements
  "zonk: type-product variables in elem positions are substituted."
  (let* ((a    (cl-cc/type:fresh-type-var 'a))
         (prod (cl-cc/type:make-type-product :elems (list a cl-cc/type:type-string)))
         (s    (subst-extend a cl-cc/type:type-int nil))
         (result (zonk prod s)))
    (assert-true (cl-cc/type:type-product-p result))
    (assert-= 2 (length (cl-cc/type:type-product-elems result)))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                         (first (cl-cc/type:type-product-elems result))))))

(deftest zonk-forall-type-substitutes-body
  "zonk: type-forall body variable is substituted, preserving the forall wrapper."
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (b        (cl-cc/type:fresh-type-var 'b))
         (forall-ty (cl-cc/type:make-type-forall :var a :body b))
         (s        (subst-extend b cl-cc/type:type-int nil))
         (result   (zonk forall-ty s)))
    (assert-true (cl-cc/type:type-forall-p result))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                         (cl-cc/type:type-forall-body result)))))

(deftest zonk-type-app-substitutes-fun-and-arg
  "zonk: type-app variables in fun and arg positions are both substituted."
  (let* ((a      (cl-cc/type:fresh-type-var 'a))
         (b      (cl-cc/type:fresh-type-var 'b))
         (app-ty (cl-cc/type:make-type-app :fun a :arg b))
         (s      (make-substitution)))
    (subst-extend! a cl-cc/type:type-int s)
    (subst-extend! b cl-cc/type:type-string s)
    (let ((result (zonk app-ty s)))
      (assert-true (cl-cc/type:type-app-p result))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name (cl-cc/type:type-app-fun result)))
      (assert-eq 'string (cl-cc/type:type-primitive-name (cl-cc/type:type-app-arg result))))))

(deftest zonk-union-type-substitutes-members
  "zonk: type-union member variables are substituted, preserving member count."
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (union-ty (cl-cc/type:make-type-union-raw :types (list a cl-cc/type:type-string)))
         (s        (subst-extend a cl-cc/type:type-int nil))
         (result   (zonk union-ty s)))
    (assert-true (cl-cc/type:type-union-p result))
    (assert-= 2 (length (cl-cc/type:type-union-types result)))))

(deftest zonk-effect-row-var-is-resolved
  "zonk: effect row variable is resolved when bound in the substitution."
  (let* ((rv  (cl-cc/type:fresh-type-var 'r))
         (eff (cl-cc/type:make-type-effect-op :name 'io :args nil))
         (row (cl-cc/type:make-type-effect-row :effects (list eff) :row-var rv))
         (s   (subst-extend rv (cl-cc/type:make-type-effect-row :effects nil :row-var nil) nil)))
    (assert-true (cl-cc/type:type-effect-row-p (zonk row s)))))

(deftest zonk-effect-rows-are-merged-when-var-resolves-to-row
  "zonk: when a row variable resolves to another effect row, effects are merged."
  (let* ((rv   (cl-cc/type:fresh-type-var 'r))
         (eff1 (cl-cc/type:make-type-effect-op :name 'io  :args nil))
         (eff2 (cl-cc/type:make-type-effect-op :name 'exn :args nil))
         (row1 (cl-cc/type:make-type-effect-row :effects (list eff1) :row-var rv))
         (row2 (cl-cc/type:make-type-effect-row :effects (list eff2) :row-var nil))
         (s    (subst-extend rv row2 nil))
         (result (zonk row1 s)))
    (assert-true (cl-cc/type:type-effect-row-p result))
    (assert-= 2 (length (cl-cc/type:type-effect-row-effects result)))))

;;; ─── Occurs Check ───────────────────────────────────────────────────────

(deftest type-occurs-check
  "Occurs check: var in itself, in arrow, not in unrelated type, follows subst chains."
  (let ((s (make-substitution)))
    (let ((v (cl-cc/type:fresh-type-var 'a)))
      (assert-true  (type-occurs-p v v s))
      (assert-true  (type-occurs-p v (cl-cc/type:make-type-arrow-raw :params (list v) :return cl-cc/type:type-int) s))
      (assert-false (type-occurs-p v cl-cc/type:type-int s))))
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (b (cl-cc/type:fresh-type-var 'b))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return cl-cc/type:type-int))
         (s (subst-extend b fn-ty nil)))
    (assert-true (type-occurs-p a b s))))

;;; ─── Generalize / Instantiate ───────────────────────────────────────────

(deftest-each generalize-quantification-cases
  "Free vars outside env are quantified (count=1); vars in env are not (count=0)."
  :cases (("outside-env" 1 nil)
          ("in-env"      0 t))
  (expected-count var-in-env-p)
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (env (if var-in-env-p
                  (cl-cc/type:type-env-extend 'x (cl-cc/type:type-to-scheme a) (cl-cc/type:type-env-empty))
                  nil))
         (ret (if var-in-env-p cl-cc/type:type-int a))
         (fn-ty (cl-cc/type:make-type-arrow-raw :params (list a) :return ret))
         (scheme (generalize env fn-ty)))
    (assert-= expected-count (length (cl-cc/type:type-scheme-quantified-vars scheme)))))

(deftest instantiate-produces-fresh
  "Instantiate replaces quantified vars with fresh ones."
  (let* ((a (cl-cc/type:fresh-type-var 'a))
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

(deftest normalize-type-variables-renames-distinct-vars
  "normalize-type-variables: two distinct type vars get distinct canonical names."
  (let* ((fn    (cl-cc/type:make-type-arrow-raw
                 :params (list (cl-cc/type:fresh-type-var 'xyz))
                 :return (cl-cc/type:fresh-type-var 'qqq)))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (let ((p (car (cl-cc/type:type-arrow-params normed)))
          (r (cl-cc/type:type-arrow-return normed)))
      (assert-true  (cl-cc/type:type-var-p p))
      (assert-true  (cl-cc/type:type-var-p r))
      (assert-false (cl-cc/type:type-var-equal-p p r)))))

(deftest normalize-type-variables-preserves-shared-variable
  "normalize-type-variables: a variable shared across positions normalizes to the same canonical var."
  (let* ((v     (cl-cc/type:fresh-type-var 'x))
         (fn    (cl-cc/type:make-type-arrow-raw :params (list v) :return v))
         (normed (cl-cc/type:normalize-type-variables fn)))
    (assert-true (cl-cc/type:type-var-equal-p
                  (car (cl-cc/type:type-arrow-params normed))
                  (cl-cc/type:type-arrow-return normed)))))

(deftest zonk-env-substitutes-all-bindings
  "zonk-env applies the substitution to all bindings in the type environment."
  (let* ((a        (cl-cc/type:fresh-type-var 'a))
         (env      (cl-cc/type:make-type-env
                    :bindings (list (cons 'x a)
                                    (cons 'y cl-cc/type:type-string))))
         (s        (subst-extend a cl-cc/type:type-int nil))
         (result   (cl-cc/type:zonk-env env s))
         (bindings (cl-cc/type:type-env-bindings result)))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name (cdr (first bindings))))
    (assert-eq 'string (cl-cc/type:type-primitive-name (cdr (second bindings))))))

;;; ─── %nv-canonical / %nv-norm (extracted helpers) ────────────────────────────

(deftest nv-canonical-creates-fresh-var
  "%nv-canonical: first call creates a fresh var, second call returns same var."
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0))
        (v            (cl-cc/type:fresh-type-var 'x)))
    (let ((nv1 (cl-cc/type::%nv-canonical v mapping counter-cell)))
      (assert-true (cl-cc/type:type-var-p nv1))
      (assert-= 1 (car counter-cell))
      (let ((nv2 (cl-cc/type::%nv-canonical v mapping counter-cell)))
        (assert-eq nv1 nv2)
        (assert-= 1 (car counter-cell))))))

(deftest nv-norm-passes-through-non-var
  "%nv-norm on a non-type-var primitive type returns it unchanged."
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0)))
    (assert-eq cl-cc/type:type-int
               (cl-cc/type::%nv-norm cl-cc/type:type-int mapping counter-cell))
    (assert-= 0 (car counter-cell))))

(deftest nv-norm-renames-type-var
  "%nv-norm replaces a type-var with its canonical rename."
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0))
        (v            (cl-cc/type:fresh-type-var 'z)))
    (let ((result (cl-cc/type::%nv-norm v mapping counter-cell)))
      (assert-true (cl-cc/type:type-var-p result))
      (assert-false (eq result v)))))

;;; ─── apply-unification ───────────────────────────────────────────────────────

(deftest apply-unification-nil-subst-returns-nil
  "apply-unification returns NIL when SUBST is NIL (nothing to apply)."
  (let ((ty (cl-cc/type:parse-type-specifier 'fixnum)))
    (assert-false (cl-cc/type:apply-unification ty nil))))

(deftest apply-unification-empty-subst-returns-ty
  "apply-unification with an empty (but non-nil) substitution returns TY unchanged."
  (let ((ty    (cl-cc/type:parse-type-specifier 'fixnum))
        (subst (make-hash-table :test #'eql)))
    (assert-eq ty (cl-cc/type:apply-unification ty subst))))

(deftest apply-unification-resolves-mapped-var
  "apply-unification substitutes a mapped type variable with its concrete type."
  (let* ((v     (cl-cc/type:fresh-type-var 'x))
          (target (cl-cc/type:parse-type-specifier 'fixnum))
         (subst  (cl-cc/type:subst-extend v target nil)))
    (let ((result (cl-cc/type:apply-unification v subst)))
      (assert-true result)
      (assert-false (cl-cc/type:type-var-p result)))))
