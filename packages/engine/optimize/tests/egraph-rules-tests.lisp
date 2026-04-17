;;;; tests/unit/optimize/egraph-rules-tests.lisp — E-Graph Rewrite Rule Tests
;;;
;;; Comprehensive tests for all 55 built-in e-graph rewrite rules defined in
;;; src/optimize/egraph-rules.lisp.  Each test is independent and creates its
;;; own fresh e-graph.
;;;
;;; IMPORTANT implementation notes about the e-graph:
;;;
;;;   1. Op symbols MUST use cl-cc:: prefix (e.g., 'cl-cc/optimize::add).  Unqualified
;;;      symbols in this package do not match the cl-cc:: symbols registered by
;;;      defrule and cause silent pattern-match failures.
;;;
;;;   2. All (egraph-add eg 'cl-cc/optimize::const) calls with the same e-graph return
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
(in-suite cl-cc-unit-suite)

;;; ─── Shared Helpers ───────────────────────────────────────────────────────

(defun make-eg-const (eg value)
  "Add a cl-cc::const node and store VALUE as its ec-data.  Returns the class ID.
   NOTE: all consts in the same e-graph share one class (memoized)."
  (let ((id (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::const)))
    (let ((cls (gethash (cl-cc/optimize::egraph-find eg id) (cl-cc::eg-classes eg))))
      (when cls (setf (cl-cc::ec-data cls) value)))
    id))

(defun eg-merged-p (eg id1 id2)
  "Return true iff ID1 and ID2 are in the same equivalence class."
  (= (cl-cc/optimize::egraph-find eg id1) (cl-cc/optimize::egraph-find eg id2)))

(defun eg-all-nodes (eg id)
  "Return ALL e-nodes logically in the equivalence class of ID.
   Collects nodes from both the canonical class and any zombie classes whose
   egraph-find returns the same canonical ID (due to deferred class consolidation)."
  (let ((canon (cl-cc/optimize::egraph-find eg id))
        (nodes nil))
    (maphash (lambda (k cls)
               (when (= (cl-cc/optimize::egraph-find eg k) canon)
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
  (cl-cc/optimize::egraph-saturate eg (cl-cc/optimize::egraph-builtin-rules) :limit 10 :fuel 1000)
  (cl-cc/optimize::egraph-rebuild eg))

(defun eg-rule-registered-p (name)
  "Return true if NAME is a registered built-in rule name."
  (let ((rules (cl-cc/optimize::egraph-builtin-rules)))
    (not (null (find name rules :key (lambda (r) (getf r :name)))))))

;;; ─── Constant Folding Rules — Registration Tests ─────────────────────────
;;;
;;; The fold-* rules use (const ?a) patterns that cannot bind ?a via the
;;; current pattern-matcher constant-pattern branch.  We test that the rules
;;; are correctly registered.

(deftest-each egraph-fold-rules-registered
  "All fold-* rules are registered in *egraph-rules*."
  :cases (("fold-add" 'cl-cc/optimize::fold-add)
          ("fold-sub" 'cl-cc/optimize::fold-sub)
          ("fold-mul" 'cl-cc/optimize::fold-mul)
          ("fold-neg" 'cl-cc/optimize::fold-neg)
          ("fold-not" 'cl-cc/optimize::fold-not)
          ("fold-lt"  'cl-cc/optimize::fold-lt)
          ("fold-gt"  'cl-cc/optimize::fold-gt)
          ("fold-le"  'cl-cc/optimize::fold-le)
          ("fold-ge"  'cl-cc/optimize::fold-ge))
  (rule-name)
  (assert-true (eg-rule-registered-p rule-name)))

;;; ─── Algebraic Identity: bidirectional identity rules ───────────────────
;;; Pattern: (op ?x (const N)) → ?x   and   (op (const N) ?x) → ?x
;;; Both arg orders must merge the result class with x.

(deftest-each egraph-bidirectional-identity-fires
  "Bidirectional identity rules fire in both argument orders: (op x const) and (op const x) both merge with x."
  :cases (("add-zero"    'cl-cc/optimize::add    0)
          ("mul-one"     'cl-cc/optimize::mul    1)
          ("logand-neg1" 'cl-cc/optimize::logand -1))
  (op identity-val)
  ;; Right-identity: (op x const) → x
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x)))
  ;; Left-identity: (op const x) → x
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg identity-val))
         (id (cl-cc/optimize::egraph-add eg op c x)))
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
  :cases (("mul-zero"    'cl-cc/optimize::mul    0)
          ("logand-zero" 'cl-cc/optimize::logand 0))
  (op annihilator-val)
  ;; Right-annihilator: (op x const) → const class
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const)))
  ;; Left-annihilator: (op const x) → const class
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg annihilator-val))
         (id (cl-cc/optimize::egraph-add eg op c x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const))))

;;; ─── Algebraic Identity: single-sided zero/one identities ───────────────

(deftest-each egraph-single-sided-identity-rules-fire
  "Single-sided identity rules: (op ?x (const N)) merges with ?x."
  :cases (("sub-zero" 'cl-cc/optimize::sub 0)
          ("div-one"  'cl-cc/optimize::div 1)
          ("ash-zero" 'cl-cc/optimize::ash 0))
  (op const-val)
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c  (make-eg-const eg const-val))
         (id (cl-cc/optimize::egraph-add eg op x c)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Self-Reference Rules ────────────────────────────────────────────────
;;; sub-self: (sub ?x ?x) → (const 0).
;;; Both args are the same class (memoized var).  RHS (const 0) creates a
;;; const-with-child zombie class.  Use eg-class-contains-op-p.

(deftest-each egraph-self-reference-rules-fire
  "Self-reference rules: (op ?x ?x) saturates to a class containing a const node."
  :cases (("sub-self" 'cl-cc/optimize::sub)
          ("eq-self"  'cl-cc/optimize::num-eq)
          ("lt-self"  'cl-cc/optimize::lt)
          ("gt-self"  'cl-cc/optimize::gt)
          ("le-self"  'cl-cc/optimize::le)
          ("ge-self"  'cl-cc/optimize::ge))
  (op)
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize::egraph-add eg op x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const))))

;;; ─── Negation: mul-neg1 ──────────────────────────────────────────────────
;;; mul-neg1-r: (mul ?x (const -1)) → (neg ?x)
;;; RHS (neg ?x) is an actual neg-node, not a const.  Pre-add (neg x) so
;;; the rule can merge mul-id with the pre-existing neg class.

(deftest egraph-rule-mul-neg1-fires
  "mul-neg1: (mul ?x (const -1)) and (mul (const -1) ?x) both merge with (neg ?x)."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul x cn1))
         (neg (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg)))
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul cn1 x))
         (neg (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg))))

;;; ─── Negation: double-neg / not-not ─────────────────────────────────────

(deftest-each egraph-rule-double-negation-fires
  "Double negation: (op (op ?x)) merges with ?x for both neg and not."
  :cases (("double-neg" 'cl-cc/optimize::neg)
          ("not-not"    'cl-cc/optimize::not))
  (op)
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (op1  (cl-cc/optimize::egraph-add eg op x))
         (op2  (cl-cc/optimize::egraph-add eg op op1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg op2 x))))

(deftest-each egraph-rule-negated-comparison-fires
  "Negated comparisons merge with their dual comparison."
  :cases (("not-lt" 'cl-cc/optimize::not 'cl-cc/optimize::lt 'cl-cc/optimize::ge)
          ("not-gt" 'cl-cc/optimize::not 'cl-cc/optimize::gt 'cl-cc/optimize::le)
          ("not-le" 'cl-cc/optimize::not 'cl-cc/optimize::le 'cl-cc/optimize::gt)
          ("not-ge" 'cl-cc/optimize::not 'cl-cc/optimize::ge 'cl-cc/optimize::lt))
  (not-op cmp-op dual-op)
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (cmp (cl-cc/optimize::egraph-add eg cmp-op x y))
         (not (cl-cc/optimize::egraph-add eg not-op cmp))
         (dual (cl-cc/optimize::egraph-add eg dual-op x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg not dual))))

;;; ─── Negation: add-neg ───────────────────────────────────────────────────
;;; add-neg: (add ?x (neg ?y)) → (sub ?x ?y)
;;; Use var for x and a const for y so x ≠ y (different memo keys).

(deftest egraph-rule-add-neg-fires
  "add-neg: (add ?x (neg ?y)) merges with (sub ?x ?y)."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 7))
         (ny  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg y))
         (add (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::add x ny))
         (sub (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg add sub))))

;;; ─── Negation: sub-neg ───────────────────────────────────────────────────

(deftest egraph-rule-sub-neg-fires
  "sub-neg: (sub ?x (neg ?y)) merges with (add ?x ?y)."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y   (make-eg-const eg 5))
         (ny  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg y))
         (sub (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub x ny))
         (add (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::add x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg sub add))))

;;; ─── Bitwise: self-identity (logand-self, logior-self) ──────────────────

(deftest-each egraph-bitwise-self-identity-fires
  "Bitwise self-identity: (op ?x ?x) merges with ?x for logand and logior."
  :cases (("logand-self" 'cl-cc/optimize::logand)
          ("logior-self" 'cl-cc/optimize::logior))
  (op)
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize::egraph-add eg op x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-zero / logxor-zero (bidirectional zero-identity) ───

(deftest-each egraph-bitwise-zero-identity-fires
  "Bidirectional zero-identity: both (op ?x (const 0)) and (op (const 0) ?x) merge with ?x."
  :cases (("logior-zero" 'cl-cc/optimize::logior)
          ("logxor-zero" 'cl-cc/optimize::logxor))
  (op)
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize::egraph-add eg op x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x)))
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize::egraph-add eg op c0 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))


;;; ─── Bitwise: logxor-self ────────────────────────────────────────────────
;;; logxor-self: (logxor ?x ?x) → (const 0).
;;; RHS is (const 0) — compound template builds a const-with-child zombie.

(deftest egraph-rule-logxor-self-fires
  "logxor-self: (logxor ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (id (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::logxor x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc/optimize::const))))

;;; ─── Bitwise: ash-zero-base ──────────────────────────────────────────────
;;; ash-zero-base: (ash (const 0) ?x) → (const 0).
;;; RHS is (const 0) — compound template.

(deftest egraph-rule-ash-zero-base-fires
  "ash-zero-base: (ash (const 0) ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::ash c0 x)))
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

(deftest egraph-rule-mul-pow2-guard-check
  "mul-pow2 :when guard is present (non-nil)."
  (let ((rule (find 'cl-cc/optimize::mul-pow2
                    (cl-cc/optimize::egraph-builtin-rules)
                    :key (lambda (r) (getf r :name)))))
    (assert-true (not (null (getf rule :when))))))

(deftest egraph-rule-mul-pow2-non-power-no-ash
  "mul-pow2: (mul ?x (const 7)) — 7 is not a power of 2, no ash introduced."
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c7 (make-eg-const eg 7))
         (id (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul x c7)))
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

(deftest egraph-rule-mul-neg-neg-fires
  "mul-neg-neg: (mul (neg ?x) (neg ?y)) merges with (mul ?x ?y)."
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 3))
         (nx   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x))
         (ny   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg y))
         (mul1 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul nx ny))
         (mul2 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul x y)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

(deftest egraph-rule-mul-neg-neg-same-var
  "mul-neg-neg: (mul (neg ?x) (neg ?x)) merges with (mul ?x ?x)."
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (nx   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x))
         (mul1 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul nx nx))
         (mul2 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul1 mul2))))

;;; ─── Advanced: neg-sub ───────────────────────────────────────────────────
;;; neg-sub: (neg (sub ?x ?y)) → (sub ?y ?x).

(deftest egraph-rule-neg-sub-fires
  "neg-sub: (neg (sub ?x ?y)) merges with (sub ?y ?x)."
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (y    (make-eg-const eg 11))
         (sub1 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub x y))
         (neg  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg sub1))
         (sub2 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub y x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg neg sub2))))

(deftest egraph-rule-neg-sub-same-class
  "neg-sub: (neg (sub ?x ?x)) — self-sub simplifies via sub-self first."
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (sub1 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub x x))
         (neg  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg sub1)))
    (eg-saturate eg)
    ;; sub-self fires first: sub(x,x) merges with (const 0) class.
    ;; Then neg-sub might fire on the merged class.
    ;; At minimum, sub1 should have a const node in its class.
    (assert-true (eg-class-contains-op-p eg sub1 'cl-cc/optimize::const))))

;;; ─── Rule Registry: All 51 Rules Present ────────────────────────────────

(deftest egraph-rule-registry-complete
  "Rule registry: >=51 rules, all have :lhs/:rhs, and all expected names are present."
  (let* ((rules (cl-cc/optimize::egraph-builtin-rules))
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
  (let* ((eg (cl-cc/optimize::make-e-graph))
         (x  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::add x c0)))
    (declare (ignore id))
    (eg-saturate eg)
    (let ((n1 (hash-table-count (cl-cc::eg-classes eg))))
      (eg-saturate eg)
      (assert-true (<= (hash-table-count (cl-cc::eg-classes eg)) n1)))))

;;; ─── Composition ─────────────────────────────────────────────────────────

(deftest egraph-rule-double-neg-then-identity
  "Composition: (add (neg (neg ?x)) (const 0)) — double-neg + add-zero-r."
  (let* ((eg   (cl-cc/optimize::make-e-graph))
         (x    (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (neg1 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x))
         (neg2 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg neg1))
         (c0   (make-eg-const eg 0))
         (add  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::add neg2 c0)))
    (eg-saturate eg)
    ;; double-neg: neg2 merges with x.
    ;; add-zero-r: add(x, 0) merges with x.
    ;; So add should merge with x.
    (assert-true (eg-merged-p eg add x))))

(deftest egraph-rule-mul-neg1-then-double-neg
  "Composition: (mul (neg ?x) (const -1)) — mul-neg1-r fires; class contains both mul and neg."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (nx  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg x))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::mul nx cn1)))
    (eg-saturate eg)
    ;; mul-neg1-r: mul(neg(x), -1) merges with a neg class.
    ;; The merged class contains both mul and neg nodes.
    (assert-true (eg-class-contains-op-p eg mul 'cl-cc/optimize::neg))))

(deftest egraph-rule-sub-neg-then-add-zero
  "Composition: (sub ?x (neg (const 0))) — sub-neg fires; class contains both sub and add."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (c0  (make-eg-const eg 0))
         (nc0 (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::neg c0))
         (sub (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::sub x nc0))
         (add (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::add x c0)))
    (eg-saturate eg)
    ;; sub-neg: sub(x, neg(c0)) merges with add(x, c0).
    ;; The sub and add classes should be merged.
    (assert-true (eg-merged-p eg sub add))))

(deftest egraph-rule-logand-self-then-logior-zero
  "Composition: (logior (logand ?x ?x) (const 0)) — logand-self + logior-zero."
  (let* ((eg  (cl-cc/optimize::make-e-graph))
         (x   (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::var))
         (and (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::logand x x))
         (c0  (make-eg-const eg 0))
         (or  (cl-cc/optimize::egraph-add eg 'cl-cc/optimize::logior and c0)))
    (eg-saturate eg)
    ;; logand-self: logand(x,x) merges with x.
    ;; logior-zero: logior(x, 0) merges with x.
    (assert-true (eg-merged-p eg or x))))
