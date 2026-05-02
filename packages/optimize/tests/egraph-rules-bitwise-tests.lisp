;;;; tests/unit/optimize/egraph-rules-bitwise-tests.lisp — E-Graph Bitwise & Advanced Rule Tests
;;;
;;; Continuation of egraph-rules-tests.lisp.  Covers bitwise rules, strength
;;; reduction, type predicate rules, advanced/composed rules, and the full
;;; rule registry check.

(in-package :cl-cc/test)
(in-suite cl-cc-coverage-unstable-unit-suite)

;;; ─── Bitwise: self-identity (logand-self, logior-self) ──────────────────

(deftest-each egraph-bitwise-self-identity-fires
  "Bitwise self-identity: (op ?x ?x) merges with ?x for logand and logior."
  :cases (("logand-self" 'cl-cc/optimize::logand)
          ("logior-self" 'cl-cc/optimize::logior))
  (op)
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg op x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-zero / logxor-zero (bidirectional zero-identity) ───

(deftest-each egraph-bitwise-zero-identity-fires
  "Bidirectional zero-identity: both (op ?x (const 0)) and (op (const 0) ?x) merge with ?x."
  :cases (("logior-zero" 'cl-cc/optimize::logior)
          ("logxor-zero" 'cl-cc/optimize::logxor))
  (op)
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x)))
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg op c0 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))


;;; ─── Bitwise: logxor-self / ash-zero-base ───────────────────────────────

(deftest egraph-rule-const-producing-rules-fire
  "logxor-self and ash-zero-base each produce a const-class result."
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logxor x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const)))
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::ash c0 x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const))))

;;; ─── Strength Reduction Rules — Registration Tests ───────────────────────
;;; mul-pow2/mul-pow2-l/div-pow2 use (const ?n) guards that cannot bind ?n
;;; via the current pattern-matcher.  Test registration only.

(deftest-each egraph-strength-reduction-rules-registered
  "Strength-reduction rules (mul-pow2, mul-pow2-l, div-pow2) are registered."
  :cases (("mul-pow2"   'cl-cc/optimize::mul-pow2)
          ("mul-pow2-l" 'cl-cc/optimize::mul-pow2-l)
          ("div-pow2"   'cl-cc/optimize::div-pow2))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

(deftest egraph-rule-mul-pow2-has-when-guard
  "mul-pow2 rule has a :when guard clause."
  (let ((rule (find 'cl-cc/optimize::mul-pow2
                    (cl-cc/optimize:egraph-builtin-rules)
                    :key (lambda (r) (getf r :name)))))
    (assert-true (not (null (getf rule :when))))))

(deftest egraph-rule-mul-pow2-non-power-of-2-does-not-introduce-ash
  "mul-pow2: multiplying by a non-power-of-2 constant does not introduce an ash node."
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c7 (make-eg-const eg 7))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x c7)))
    (declare (ignore id))
    (eg-saturate eg)
    (assert-false (eg-any-class-has-op-p eg 'cl-cc/optimize::ash))))

;;; ─── Type Predicate Rules — Registration Tests ───────────────────────────

(deftest-each egraph-type-predicate-rules-registered
  "Type-predicate constant rules are registered."
  :cases (("null-p-const"    'cl-cc/optimize::null-p-const)
          ("cons-p-const"    'cl-cc/optimize::cons-p-const)
          ("number-p-const"  'cl-cc/optimize::number-p-const)
          ("integer-p-const" 'cl-cc/optimize::integer-p-const))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

;;; ─── Advanced: mul-neg-neg ───────────────────────────────────────────────
;;; mul-neg-neg: (mul (neg ?x) (neg ?y)) → (mul ?x ?y).
;;; Use var for x and a const for y so they're in different classes.

(deftest egraph-rule-mul-neg-neg-distinct-vars-merges-with-mul
  "mul-neg-neg: (mul (neg ?x) (neg ?y)) merges with (mul ?x ?y) for distinct x and y."
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 3))
         (nx   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (ny   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg y))
         (mul1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx ny))
         (mul2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

(deftest egraph-rule-mul-neg-neg-same-var-also-fires
  "mul-neg-neg: (mul (neg ?x) (neg ?x)) merges with (mul ?x ?x)."
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (nx   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (mul1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx nx))
         (mul2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

;;; ─── Advanced: neg-sub ───────────────────────────────────────────────────
;;; neg-sub: (neg (sub ?x ?y)) → (sub ?y ?x).

(deftest egraph-rule-neg-sub-distinct-vars-merges-with-reversed-sub
  "neg-sub: (neg (sub ?x ?y)) merges with (sub ?y ?x) for distinct x and y."
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 11))
         (sub1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x y))
         (neg  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg sub1))
         (sub2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub y x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg neg sub2))))

(deftest egraph-rule-neg-sub-same-var-reduces-to-const-via-sub-self
  "neg-sub with same variable: (neg (sub ?x ?x)) reduces to a const class via sub-self."
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (sub1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x x))
         (neg  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg sub1)))
    (declare (ignore neg))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg sub1 'cl-cc/optimize::const))))

;;; ─── Rule Registry: All 51 Rules Present ────────────────────────────────

(deftest egraph-rule-registry-complete
  "Rule registry: >=51 rules, all have :lhs/:rhs, and all expected names are present."
  (let* ((rules (cl-cc/optimize:egraph-builtin-rules))
         (names (mapcar (lambda (r) (getf r :name)) rules)))
    (assert-true (>= (length rules) 51))
    (dolist (rule rules)
      (assert-true (getf rule :lhs))
      (assert-true (not (eq (getf rule :rhs :missing) :missing))))
    (dolist (n '(cl-cc/optimize::fold-add cl-cc/optimize::fold-sub cl-cc/optimize::fold-mul
                 cl-cc/optimize::fold-neg cl-cc/optimize::fold-not
                 cl-cc/optimize::fold-lt cl-cc/optimize::fold-gt cl-cc/optimize::fold-le cl-cc/optimize::fold-ge
                 cl-cc/optimize::add-zero-r cl-cc/optimize::add-zero-l cl-cc/optimize::sub-zero
                 cl-cc/optimize::mul-one-r cl-cc/optimize::mul-one-l
                 cl-cc/optimize::mul-zero-r cl-cc/optimize::mul-zero-l cl-cc/optimize::div-one
                 cl-cc/optimize::sub-self cl-cc/optimize::eq-self
                 cl-cc/optimize::lt-self cl-cc/optimize::gt-self cl-cc/optimize::le-self cl-cc/optimize::ge-self
                  cl-cc/optimize::mul-neg1-r cl-cc/optimize::mul-neg1-l
                  cl-cc/optimize::double-neg cl-cc/optimize::not-not
                  cl-cc/optimize::not-lt cl-cc/optimize::not-gt cl-cc/optimize::not-le cl-cc/optimize::not-ge
                  cl-cc/optimize::add-neg cl-cc/optimize::sub-neg
                  cl-cc/optimize::logand-zero cl-cc/optimize::logand-zero-l
                  cl-cc/optimize::logand-neg1 cl-cc/optimize::logand-neg1-l cl-cc/optimize::logand-self
                 cl-cc/optimize::logior-zero cl-cc/optimize::logior-zero-l cl-cc/optimize::logior-self
                 cl-cc/optimize::logxor-zero cl-cc/optimize::logxor-zero-l cl-cc/optimize::logxor-self
                 cl-cc/optimize::ash-zero cl-cc/optimize::ash-zero-base
                 cl-cc/optimize::mul-pow2 cl-cc/optimize::mul-pow2-l cl-cc/optimize::div-pow2
                 cl-cc/optimize::null-p-const cl-cc/optimize::cons-p-const
                 cl-cc/optimize::number-p-const cl-cc/optimize::integer-p-const
                 cl-cc/optimize::mul-neg-neg cl-cc/optimize::neg-sub))
      (assert-true (member n names)))))

;;; ─── Idempotency ─────────────────────────────────────────────────────────

(deftest egraph-saturation-idempotent
  "Running saturation twice on a saturated graph adds no new e-class entries."
  (let* ((eg (cl-cc/optimize:make-e-graph))
         (x  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add x c0)))
    (declare (ignore id))
    (eg-saturate eg)
    (let ((n1 (hash-table-count (cl-cc:eg-classes eg))))
      (eg-saturate eg)
      (assert-true (<= (hash-table-count (cl-cc:eg-classes eg)) n1)))))

;;; ─── Composition ─────────────────────────────────────────────────────────

(deftest egraph-rule-double-neg-then-identity
  "Composition: (add (neg (neg ?x)) (const 0)) — double-neg + add-zero-r."
  (let* ((eg   (cl-cc/optimize:make-e-graph))
         (x    (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (neg1 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (neg2 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg neg1))
         (c0   (make-eg-const eg 0))
         (add  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add neg2 c0)))
    (eg-saturate eg)
    ;; double-neg: neg2 merges with x.
    ;; add-zero-r: add(x, 0) merges with x.
    ;; So add should merge with x.
    (assert-true (eg-merged-p eg add x))))

(deftest egraph-rule-mul-neg1-then-double-neg
  "Composition: (mul (neg ?x) (const -1)) — mul-neg1-r fires; class contains both mul and neg."
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (nx  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg x))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::mul nx cn1)))
    (eg-saturate eg)
    ;; mul-neg1-r: mul(neg(x), -1) merges with a neg class.
    ;; The merged class contains both mul and neg nodes.
    (assert-true (eg-class-contains-op-p eg mul 'cl-cc/optimize::neg))))

(deftest egraph-rule-sub-neg-then-add-zero
  "Composition: (sub ?x (neg (const 0))) — sub-neg fires; class contains both sub and add."
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (c0  (make-eg-const eg 0))
         (nc0 (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::neg c0))
         (sub (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::sub x nc0))
         (add (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::add x c0)))
    (eg-saturate eg)
    ;; sub-neg: sub(x, neg(c0)) merges with add(x, c0).
    ;; The sub and add classes should be merged.
    (assert-true (eg-merged-p eg sub add))))

(deftest egraph-rule-logand-self-then-logior-zero
  "Composition: (logior (logand ?x ?x) (const 0)) — logand-self + logior-zero."
  (let* ((eg  (cl-cc/optimize:make-e-graph))
         (x   (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::var))
         (and (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logand x x))
         (c0  (make-eg-const eg 0))
         (or  (cl-cc/optimize:egraph-add eg 'cl-cc/optimize::logior and c0)))
    (eg-saturate eg)
    ;; logand-self: logand(x,x) merges with x.
    ;; logior-zero: logior(x, 0) merges with x.
    (assert-true (eg-merged-p eg or x))))
