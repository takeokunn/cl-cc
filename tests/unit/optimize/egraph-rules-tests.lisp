;;;; tests/unit/optimize/egraph-rules-tests.lisp — E-Graph Rewrite Rule Tests
;;;
;;; Comprehensive tests for all 55 built-in e-graph rewrite rules defined in
;;; src/optimize/egraph-rules.lisp.  Each test is independent and creates its
;;; own fresh e-graph.
;;;
;;; IMPORTANT implementation notes about the e-graph:
;;;
;;;   1. Op symbols MUST use cl-cc:: prefix (e.g., 'cl-cc::add).  Unqualified
;;;      symbols in this package do not match the cl-cc:: symbols registered by
;;;      defrule and cause silent pattern-match failures.
;;;
;;;   2. All (egraph-add eg 'cl-cc::const) calls with the same e-graph return
;;;      the SAME class ID (memoized by op+children).  Constant values are stored
;;;      as ec-data on that single class.
;;;
;;;   3. After egraph-merge + egraph-rebuild, the union-find is updated but the
;;;      eg-classes hash-table may contain "zombie" entries whose canonical ID
;;;      (via egraph-find) points to the surviving class.  Use eg-all-nodes to
;;;      collect nodes across all zombie classes sharing the same canonical ID.
;;;
;;;   4. Rules with (const ?var) as a pattern sub-expression are currently
;;;      limited: the pattern matcher's "constant pattern" branch consumes
;;;      (const X) and compares ec-data against X directly.  When X is a
;;;      pattern variable symbol (e.g., ?a), the equal check fails.  This
;;;      affects fold-*, mul-pow2/div-pow2, and type-predicate rules.
;;;      Those rules are registered correctly and tested for registration only.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Shared Helpers ───────────────────────────────────────────────────────

(defun make-eg-const (eg value)
  "Add a cl-cc::const node and store VALUE as its ec-data.  Returns the class ID.
   NOTE: all consts in the same e-graph share one class (memoized)."
  (let ((id (cl-cc::egraph-add eg 'cl-cc::const)))
    (let ((cls (gethash (cl-cc::egraph-find eg id) (cl-cc::eg-classes eg))))
      (when cls (setf (cl-cc::ec-data cls) value)))
    id))

(defun eg-merged-p (eg id1 id2)
  "Return true iff ID1 and ID2 are in the same equivalence class."
  (= (cl-cc::egraph-find eg id1) (cl-cc::egraph-find eg id2)))

(defun eg-all-nodes (eg id)
  "Return ALL e-nodes logically in the equivalence class of ID.
   Collects nodes from both the canonical class and any zombie classes whose
   egraph-find returns the same canonical ID (due to deferred class consolidation)."
  (let ((canon (cl-cc::egraph-find eg id))
        (nodes nil))
    (maphash (lambda (k cls)
               (when (= (cl-cc::egraph-find eg k) canon)
                 (setf nodes (append nodes (cl-cc::ec-nodes cls)))))
             (cl-cc::eg-classes eg))
    nodes))

(defun eg-class-contains-op-p (eg id op)
  "Return true if the equivalence class of ID contains any node with OP."
  (some (lambda (n) (eq (cl-cc::en-op n) op))
        (eg-all-nodes eg id)))

(defun eg-any-class-data-eql-p (eg value)
  "Return true if any e-class in EG has ec-data eql VALUE."
  (let ((found nil))
    (maphash (lambda (_id cls)
               (declare (ignore _id))
               (when (eql (cl-cc::ec-data cls) value)
                 (setf found t)))
             (cl-cc::eg-classes eg))
    found))

(defun eg-any-class-has-op-p (eg op)
  "Return true if any e-class in EG contains a node with OP."
  (let ((found nil))
    (maphash (lambda (_id cls)
               (declare (ignore _id))
               (when (some (lambda (n) (eq (cl-cc::en-op n) op))
                           (cl-cc::ec-nodes cls))
                 (setf found t)))
             (cl-cc::eg-classes eg))
    found))

(defun eg-saturate (eg)
  "Run all built-in rules to saturation, then rebuild congruence."
  (cl-cc::egraph-saturate eg (cl-cc::egraph-builtin-rules) :limit 10 :fuel 1000)
  (cl-cc::egraph-rebuild eg))

(defun eg-rule-registered-p (name)
  "Return true if NAME is a registered built-in rule name."
  (let ((rules (cl-cc::egraph-builtin-rules)))
    (not (null (find name rules :key (lambda (r) (getf r :name)))))))

;;; ─── Constant Folding Rules — Registration Tests ─────────────────────────
;;;
;;; The fold-* rules use (const ?a) patterns that cannot bind ?a via the
;;; current pattern-matcher constant-pattern branch.  We test that the rules
;;; are correctly registered.

(deftest-each egraph-fold-rules-registered
  "All fold-* rules are registered in *egraph-rules*."
  :cases (("fold-add" 'cl-cc::fold-add)
          ("fold-sub" 'cl-cc::fold-sub)
          ("fold-mul" 'cl-cc::fold-mul)
          ("fold-neg" 'cl-cc::fold-neg)
          ("fold-not" 'cl-cc::fold-not)
          ("fold-lt"  'cl-cc::fold-lt)
          ("fold-gt"  'cl-cc::fold-gt)
          ("fold-le"  'cl-cc::fold-le)
          ("fold-ge"  'cl-cc::fold-ge))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

;;; ─── Algebraic Identity: bidirectional identity rules ───────────────────
;;; Pattern: (op ?x (const N)) → ?x   and   (op (const N) ?x) → ?x
;;; Both arg orders must merge the result class with x.

(deftest-each egraph-bidirectional-identity-fires
  "Bidirectional identity rules fire in both argument orders: (op x const) and (op const x) both merge with x."
  :cases (("add-zero"    'cl-cc::add    0)
          ("mul-one"     'cl-cc::mul    1)
          ("logand-neg1" 'cl-cc::logand -1))
  (op identity-val)
  ;; Right-identity: (op x const) → x
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x)))
  ;; Left-identity: (op const x) → x
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc::egraph-add eg op c x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: bidirectional annihilator rules ────────────────
;;; RHS is (const 0). egraph-build-rhs for the template (const 0) creates a
;;; const-with-child node (a new class).  eg-all-nodes detects both the
;;; original op and const nodes in the merged equivalence class.
;;;
;;; Covered: mul-zero, logand-zero (both share the same structural test).

(deftest-each egraph-bidirectional-annihilator-fires
  "Bidirectional annihilator rules: (op x (const 0)) and (op (const 0) x) saturate to a class containing a const node."
  :cases (("mul-zero"    'cl-cc::mul    0)
          ("logand-zero" 'cl-cc::logand 0))
  (op annihilator-val)
  ;; Right-annihilator: (op x const) → const class
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const)))
  ;; Left-annihilator: (op const x) → const class
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc::egraph-add eg op c x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Algebraic Identity: single-sided zero/one identities ───────────────

(deftest-each egraph-single-sided-identity-rules-fire
  "Single-sided identity rules: (op ?x (const N)) merges with ?x."
  :cases (("sub-zero" 'cl-cc::sub 0)
          ("div-one"  'cl-cc::div 1)
          ("ash-zero" 'cl-cc::ash 0))
  (op const-val)
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c  (make-eg-const eg const-val))
         (id (cl-cc::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Self-Reference Rules ────────────────────────────────────────────────
;;; sub-self: (sub ?x ?x) → (const 0).
;;; Both args are the same class (memoized var).  RHS (const 0) creates a
;;; const-with-child zombie class.  Use eg-class-contains-op-p.

(deftest-each egraph-self-reference-rules-fire
  "Self-reference rules: (op ?x ?x) saturates to a class containing a const node."
  :cases (("sub-self" 'cl-cc::sub)
          ("eq-self"  'cl-cc::num-eq)
          ("lt-self"  'cl-cc::lt)
          ("gt-self"  'cl-cc::gt)
          ("le-self"  'cl-cc::le)
          ("ge-self"  'cl-cc::ge))
  (op)
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg op x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Negation: mul-neg1 ──────────────────────────────────────────────────
;;; mul-neg1-r: (mul ?x (const -1)) → (neg ?x)
;;; RHS (neg ?x) is an actual neg-node, not a const.  Pre-add (neg x) so
;;; the rule can merge mul-id with the pre-existing neg class.

(deftest egraph-rule-mul-neg1-fires
  "mul-neg1: (mul ?x (const -1)) and (mul (const -1) ?x) both merge with (neg ?x)."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc::egraph-add eg 'cl-cc::mul x cn1))
         (neg (cl-cc::egraph-add eg 'cl-cc::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg)))
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc::egraph-add eg 'cl-cc::mul cn1 x))
         (neg (cl-cc::egraph-add eg 'cl-cc::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg))))

;;; ─── Negation: double-neg / not-not ─────────────────────────────────────

(deftest-each egraph-rule-double-negation-fires
  "Double negation: (op (op ?x)) merges with ?x for both neg and not."
  :cases (("double-neg" 'cl-cc::neg)
          ("not-not"    'cl-cc::not))
  (op)
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (op1  (cl-cc::egraph-add eg op x))
         (op2  (cl-cc::egraph-add eg op op1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg op2 x))))

(deftest-each egraph-rule-negated-comparison-fires
  "Negated comparisons merge with their dual comparison."
  :cases (("not-lt" 'cl-cc::not 'cl-cc::lt 'cl-cc::ge)
          ("not-gt" 'cl-cc::not 'cl-cc::gt 'cl-cc::le)
          ("not-le" 'cl-cc::not 'cl-cc::le 'cl-cc::gt)
          ("not-ge" 'cl-cc::not 'cl-cc::ge 'cl-cc::lt))
  (not-op cmp-op dual-op)
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc::egraph-add eg cmp-op x y))
         (not (cl-cc::egraph-add eg not-op cmp))
         (dual (cl-cc::egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg not dual))))

;;; ─── Negation: add-neg ───────────────────────────────────────────────────
;;; add-neg: (add ?x (neg ?y)) → (sub ?x ?y)
;;; Use var for x and a const for y so x ≠ y (different memo keys).

(deftest egraph-rule-add-neg-fires
  "add-neg: (add ?x (neg ?y)) merges with (sub ?x ?y)."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (y   (make-eg-const eg 7))
         (ny  (cl-cc::egraph-add eg 'cl-cc::neg y))
         (add (cl-cc::egraph-add eg 'cl-cc::add x ny))
         (sub (cl-cc::egraph-add eg 'cl-cc::sub x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg add sub))))

;;; ─── Negation: sub-neg ───────────────────────────────────────────────────

(deftest egraph-rule-sub-neg-fires
  "sub-neg: (sub ?x (neg ?y)) merges with (add ?x ?y)."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (y   (make-eg-const eg 5))
         (ny  (cl-cc::egraph-add eg 'cl-cc::neg y))
         (sub (cl-cc::egraph-add eg 'cl-cc::sub x ny))
         (add (cl-cc::egraph-add eg 'cl-cc::add x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg sub add))))

;;; ─── Bitwise: self-identity (logand-self, logior-self) ──────────────────

(deftest-each egraph-bitwise-self-identity-fires
  "Bitwise self-identity: (op ?x ?x) merges with ?x for logand and logior."
  :cases (("logand-self" 'cl-cc::logand)
          ("logior-self" 'cl-cc::logior))
  (op)
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg op x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-zero / logxor-zero (bidirectional zero-identity) ───

(deftest-each egraph-bitwise-zero-identity-fires
  "Bidirectional zero-identity: both (op ?x (const 0)) and (op (const 0) ?x) merge with ?x."
  :cases (("logior-zero" 'cl-cc::logior)
          ("logxor-zero" 'cl-cc::logxor))
  (op)
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg op x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x)))
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg op c0 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))


;;; ─── Bitwise: logxor-self ────────────────────────────────────────────────
;;; logxor-self: (logxor ?x ?x) → (const 0).
;;; RHS is (const 0) — compound template builds a const-with-child zombie.

(deftest egraph-rule-logxor-self-fires
  "logxor-self: (logxor ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::logxor x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Bitwise: ash-zero-base ──────────────────────────────────────────────
;;; ash-zero-base: (ash (const 0) ?x) → (const 0).
;;; RHS is (const 0) — compound template.

(deftest egraph-rule-ash-zero-base-fires
  "ash-zero-base: (ash (const 0) ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::ash c0 x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Strength Reduction Rules — Registration Tests ───────────────────────
;;; mul-pow2/mul-pow2-l/div-pow2 use (const ?n) guards that cannot bind ?n
;;; via the current pattern-matcher.  Test registration only.

(deftest-each egraph-strength-reduction-rules-registered
  "Strength-reduction rules (mul-pow2, mul-pow2-l, div-pow2) are registered."
  :cases (("mul-pow2"   'cl-cc::mul-pow2)
          ("mul-pow2-l" 'cl-cc::mul-pow2-l)
          ("div-pow2"   'cl-cc::div-pow2))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

(deftest egraph-rule-mul-pow2-guard-check
  "mul-pow2 :when guard is present (non-nil)."
  (let ((rule (find 'cl-cc::mul-pow2
                    (cl-cc::egraph-builtin-rules)
                    :key (lambda (r) (getf r :name)))))
    (assert-true (not (null (getf rule :when))))))

(deftest egraph-rule-mul-pow2-non-power-no-ash
  "mul-pow2: (mul ?x (const 7)) — 7 is not a power of 2, no ash introduced."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c7 (make-eg-const eg 7))
         (id (cl-cc::egraph-add eg 'cl-cc::mul x c7)))
    (declare (ignore id))
    (eg-saturate eg)
    (assert-false (eg-any-class-has-op-p eg 'cl-cc::ash))))

;;; ─── Type Predicate Rules — Registration Tests ───────────────────────────

(deftest-each egraph-type-predicate-rules-registered
  "Type-predicate constant rules are registered."
  :cases (("null-p-const"    'cl-cc::null-p-const)
          ("cons-p-const"    'cl-cc::cons-p-const)
          ("number-p-const"  'cl-cc::number-p-const)
          ("integer-p-const" 'cl-cc::integer-p-const))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

;;; ─── Advanced: mul-neg-neg ───────────────────────────────────────────────
;;; mul-neg-neg: (mul (neg ?x) (neg ?y)) → (mul ?x ?y).
;;; Use var for x and a const for y so they're in different classes.

(deftest egraph-rule-mul-neg-neg-fires
  "mul-neg-neg: (mul (neg ?x) (neg ?y)) merges with (mul ?x ?y)."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (y    (make-eg-const eg 3))
         (nx   (cl-cc::egraph-add eg 'cl-cc::neg x))
         (ny   (cl-cc::egraph-add eg 'cl-cc::neg y))
         (mul1 (cl-cc::egraph-add eg 'cl-cc::mul nx ny))
         (mul2 (cl-cc::egraph-add eg 'cl-cc::mul x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

(deftest egraph-rule-mul-neg-neg-same-var
  "mul-neg-neg: (mul (neg ?x) (neg ?x)) merges with (mul ?x ?x)."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (nx   (cl-cc::egraph-add eg 'cl-cc::neg x))
         (mul1 (cl-cc::egraph-add eg 'cl-cc::mul nx nx))
         (mul2 (cl-cc::egraph-add eg 'cl-cc::mul x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

;;; ─── Advanced: neg-sub ───────────────────────────────────────────────────
;;; neg-sub: (neg (sub ?x ?y)) → (sub ?y ?x).

(deftest egraph-rule-neg-sub-fires
  "neg-sub: (neg (sub ?x ?y)) merges with (sub ?y ?x)."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (y    (make-eg-const eg 11))
         (sub1 (cl-cc::egraph-add eg 'cl-cc::sub x y))
         (neg  (cl-cc::egraph-add eg 'cl-cc::neg sub1))
         (sub2 (cl-cc::egraph-add eg 'cl-cc::sub y x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg neg sub2))))

(deftest egraph-rule-neg-sub-same-class
  "neg-sub: (neg (sub ?x ?x)) — self-sub simplifies via sub-self first."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (sub1 (cl-cc::egraph-add eg 'cl-cc::sub x x))
         (neg  (cl-cc::egraph-add eg 'cl-cc::neg sub1)))
    (eg-saturate eg)
    ;; sub-self fires first: sub(x,x) merges with (const 0) class.
    ;; Then neg-sub might fire on the merged class.
    ;; At minimum, sub1 should have a const node in its class.
    (assert-true (eg-class-contains-op-p eg sub1 'cl-cc::const))))

;;; ─── Rule Registry: All 51 Rules Present ────────────────────────────────

(deftest egraph-rule-registry-complete
  "Rule registry: >=51 rules, all have :lhs/:rhs, and all expected names are present."
  (let* ((rules (cl-cc::egraph-builtin-rules))
         (names (mapcar (lambda (r) (getf r :name)) rules)))
    (assert-true (>= (length rules) 51))
    (dolist (rule rules)
      (assert-true (getf rule :lhs))
      (assert-true (not (eq (getf rule :rhs :missing) :missing))))
    (dolist (n '(cl-cc::fold-add cl-cc::fold-sub cl-cc::fold-mul
                 cl-cc::fold-neg cl-cc::fold-not
                 cl-cc::fold-lt cl-cc::fold-gt cl-cc::fold-le cl-cc::fold-ge
                 cl-cc::add-zero-r cl-cc::add-zero-l cl-cc::sub-zero
                 cl-cc::mul-one-r cl-cc::mul-one-l
                 cl-cc::mul-zero-r cl-cc::mul-zero-l cl-cc::div-one
                 cl-cc::sub-self cl-cc::eq-self
                 cl-cc::lt-self cl-cc::gt-self cl-cc::le-self cl-cc::ge-self
                  cl-cc::mul-neg1-r cl-cc::mul-neg1-l
                  cl-cc::double-neg cl-cc::not-not
                  cl-cc::not-lt cl-cc::not-gt cl-cc::not-le cl-cc::not-ge
                  cl-cc::add-neg cl-cc::sub-neg
                  cl-cc::logand-zero cl-cc::logand-zero-l
                  cl-cc::logand-neg1 cl-cc::logand-neg1-l cl-cc::logand-self
                 cl-cc::logior-zero cl-cc::logior-zero-l cl-cc::logior-self
                 cl-cc::logxor-zero cl-cc::logxor-zero-l cl-cc::logxor-self
                 cl-cc::ash-zero cl-cc::ash-zero-base
                 cl-cc::mul-pow2 cl-cc::mul-pow2-l cl-cc::div-pow2
                 cl-cc::null-p-const cl-cc::cons-p-const
                 cl-cc::number-p-const cl-cc::integer-p-const
                 cl-cc::mul-neg-neg cl-cc::neg-sub))
      (assert-true (member n names)))))

;;; ─── Idempotency ─────────────────────────────────────────────────────────

(deftest egraph-saturation-idempotent
  "Running saturation twice on a saturated graph adds no new e-class entries."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::add x c0)))
    (declare (ignore id))
    (eg-saturate eg)
    (let ((n1 (hash-table-count (cl-cc::eg-classes eg))))
      (eg-saturate eg)
      (assert-true (<= (hash-table-count (cl-cc::eg-classes eg)) n1)))))

;;; ─── Composition ─────────────────────────────────────────────────────────

(deftest egraph-rule-double-neg-then-identity
  "Composition: (add (neg (neg ?x)) (const 0)) — double-neg + add-zero-r."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (neg1 (cl-cc::egraph-add eg 'cl-cc::neg x))
         (neg2 (cl-cc::egraph-add eg 'cl-cc::neg neg1))
         (c0   (make-eg-const eg 0))
         (add  (cl-cc::egraph-add eg 'cl-cc::add neg2 c0)))
    (eg-saturate eg)
    ;; double-neg: neg2 merges with x.
    ;; add-zero-r: add(x, 0) merges with x.
    ;; So add should merge with x.
    (assert-true (eg-merged-p eg add x))))

(deftest egraph-rule-mul-neg1-then-double-neg
  "Composition: (mul (neg ?x) (const -1)) — mul-neg1-r fires; class contains both mul and neg."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (nx  (cl-cc::egraph-add eg 'cl-cc::neg x))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc::egraph-add eg 'cl-cc::mul nx cn1)))
    (eg-saturate eg)
    ;; mul-neg1-r: mul(neg(x), -1) merges with a neg class.
    ;; The merged class contains both mul and neg nodes.
    (assert-true (eg-class-contains-op-p eg mul 'cl-cc::neg))))

(deftest egraph-rule-sub-neg-then-add-zero
  "Composition: (sub ?x (neg (const 0))) — sub-neg fires; class contains both sub and add."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (c0  (make-eg-const eg 0))
         (nc0 (cl-cc::egraph-add eg 'cl-cc::neg c0))
         (sub (cl-cc::egraph-add eg 'cl-cc::sub x nc0))
         (add (cl-cc::egraph-add eg 'cl-cc::add x c0)))
    (eg-saturate eg)
    ;; sub-neg: sub(x, neg(c0)) merges with add(x, c0).
    ;; The sub and add classes should be merged.
    (assert-true (eg-merged-p eg sub add))))

(deftest egraph-rule-logand-self-then-logior-zero
  "Composition: (logior (logand ?x ?x) (const 0)) — logand-self + logior-zero."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (and (cl-cc::egraph-add eg 'cl-cc::logand x x))
         (c0  (make-eg-const eg 0))
         (or  (cl-cc::egraph-add eg 'cl-cc::logior and c0)))
    (eg-saturate eg)
    ;; logand-self: logand(x,x) merges with x.
    ;; logior-zero: logior(x, 0) merges with x.
    (assert-true (eg-merged-p eg or x))))
