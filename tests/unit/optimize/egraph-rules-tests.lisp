;;;; tests/unit/optimize/egraph-rules-tests.lisp — E-Graph Rewrite Rule Tests
;;;
;;; Comprehensive tests for all 51 built-in e-graph rewrite rules defined in
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

(deftest egraph-rule-fold-add-registered
  "fold-add rule is registered in *egraph-rules*."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-add)))

(deftest egraph-rule-fold-sub-registered
  "fold-sub rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-sub)))

(deftest egraph-rule-fold-mul-registered
  "fold-mul rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-mul)))

(deftest egraph-rule-fold-neg-registered
  "fold-neg rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-neg)))

(deftest egraph-rule-fold-not-registered
  "fold-not rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-not)))

(deftest egraph-rule-fold-lt-registered
  "fold-lt rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-lt)))

(deftest egraph-rule-fold-gt-registered
  "fold-gt rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-gt)))

(deftest egraph-rule-fold-le-registered
  "fold-le rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-le)))

(deftest egraph-rule-fold-ge-registered
  "fold-ge rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::fold-ge)))

;;; ─── Algebraic Identity: add-zero-r ─────────────────────────────────────
;;; Pattern: (add ?x (const 0)) → ?x
;;; The (const 0) uses a LITERAL match: ec-data == 0 via #'equal.

(deftest egraph-rule-add-zero-r-fires
  "add-zero-r: (add ?x (const 0)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::add x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: add-zero-l ─────────────────────────────────────

(deftest egraph-rule-add-zero-l-fires
  "add-zero-l: (add (const 0) ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::add c0 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: sub-zero ────────────────────────────────────────

(deftest egraph-rule-sub-zero-fires
  "sub-zero: (sub ?x (const 0)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::sub x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: mul-one-r ──────────────────────────────────────

(deftest egraph-rule-mul-one-r-fires
  "mul-one-r: (mul ?x (const 1)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c1 (make-eg-const eg 1))
         (id (cl-cc::egraph-add eg 'cl-cc::mul x c1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: mul-one-l ──────────────────────────────────────

(deftest egraph-rule-mul-one-l-fires
  "mul-one-l: (mul (const 1) ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c1 (make-eg-const eg 1))
         (id (cl-cc::egraph-add eg 'cl-cc::mul c1 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Algebraic Identity: mul-zero-r ─────────────────────────────────────
;;; RHS is (const 0). egraph-build-rhs for the template (const 0) creates a
;;; const-with-child node (a new class).  eg-all-nodes detects both the mul
;;; and const nodes in the merged equivalence class.

(deftest egraph-rule-mul-zero-r-fires
  "mul-zero-r: (mul ?x (const 0)) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::mul x c0)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Algebraic Identity: mul-zero-l ─────────────────────────────────────

(deftest egraph-rule-mul-zero-l-fires
  "mul-zero-l: (mul (const 0) ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::mul c0 x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Algebraic Identity: div-one ─────────────────────────────────────────

(deftest egraph-rule-div-one-fires
  "div-one: (div ?x (const 1)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c1 (make-eg-const eg 1))
         (id (cl-cc::egraph-add eg 'cl-cc::div x c1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Self-Reference: sub-self ────────────────────────────────────────────
;;; sub-self: (sub ?x ?x) → (const 0).
;;; Both args are the same class (memoized var).  RHS (const 0) creates a
;;; const-with-child zombie class.  Use eg-class-contains-op-p.

(deftest egraph-rule-sub-self-fires
  "sub-self: (sub ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::sub x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Self-Reference: eq-self ─────────────────────────────────────────────

(deftest egraph-rule-eq-self-fires
  "eq-self: (num-eq ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::num-eq x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Self-Reference: lt-self ─────────────────────────────────────────────

(deftest egraph-rule-lt-self-fires
  "lt-self: (lt ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::lt x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Self-Reference: gt-self ─────────────────────────────────────────────

(deftest egraph-rule-gt-self-fires
  "gt-self: (gt ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::gt x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Self-Reference: le-self ─────────────────────────────────────────────

(deftest egraph-rule-le-self-fires
  "le-self: (le ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::le x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Self-Reference: ge-self ─────────────────────────────────────────────

(deftest egraph-rule-ge-self-fires
  "ge-self: (ge ?x ?x) merges with a class containing a const node."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::ge x x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Negation: mul-neg1-r ────────────────────────────────────────────────
;;; mul-neg1-r: (mul ?x (const -1)) → (neg ?x)
;;; RHS (neg ?x) is an actual neg-node, not a const.  Pre-add (neg x) so
;;; the rule can merge mul-id with the pre-existing neg class.

(deftest egraph-rule-mul-neg1-r-fires
  "mul-neg1-r: (mul ?x (const -1)) merges with (neg ?x)."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc::egraph-add eg 'cl-cc::mul x cn1))
         (neg (cl-cc::egraph-add eg 'cl-cc::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg))))

;;; ─── Negation: mul-neg1-l ────────────────────────────────────────────────

(deftest egraph-rule-mul-neg1-l-fires
  "mul-neg1-l: (mul (const -1) ?x) merges with (neg ?x)."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (mul (cl-cc::egraph-add eg 'cl-cc::mul cn1 x))
         (neg (cl-cc::egraph-add eg 'cl-cc::neg x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg mul neg))))

;;; ─── Negation: double-neg ────────────────────────────────────────────────

(deftest egraph-rule-double-neg-fires
  "double-neg: (neg (neg ?x)) merges with ?x."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (neg1 (cl-cc::egraph-add eg 'cl-cc::neg x))
         (neg2 (cl-cc::egraph-add eg 'cl-cc::neg neg1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg neg2 x))))

;;; ─── Negation: not-not ───────────────────────────────────────────────────

(deftest egraph-rule-not-not-fires
  "not-not: (not (not ?x)) merges with ?x."
  (let* ((eg   (cl-cc::make-e-graph))
         (x    (cl-cc::egraph-add eg 'cl-cc::var))
         (not1 (cl-cc::egraph-add eg 'cl-cc::not x))
         (not2 (cl-cc::egraph-add eg 'cl-cc::not not1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg not2 x))))

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

;;; ─── Bitwise: logand-zero ────────────────────────────────────────────────

(deftest egraph-rule-logand-zero-fires
  "logand-zero: (logand ?x (const 0)) merges with a class containing a const."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logand x c0)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Bitwise: logand-zero-l ──────────────────────────────────────────────

(deftest egraph-rule-logand-zero-l-fires
  "logand-zero-l: (logand (const 0) ?x) merges with a class containing a const."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logand c0 x)))
    (eg-saturate eg)
    (assert-true (eg-class-contains-op-p eg id 'cl-cc::const))))

;;; ─── Bitwise: logand-neg1 ────────────────────────────────────────────────

(deftest egraph-rule-logand-neg1-fires
  "logand-neg1: (logand ?x (const -1)) merges with ?x."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (id  (cl-cc::egraph-add eg 'cl-cc::logand x cn1)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logand-neg1-l ──────────────────────────────────────────────

(deftest egraph-rule-logand-neg1-l-fires
  "logand-neg1-l: (logand (const -1) ?x) merges with ?x."
  (let* ((eg  (cl-cc::make-e-graph))
         (x   (cl-cc::egraph-add eg 'cl-cc::var))
         (cn1 (make-eg-const eg -1))
         (id  (cl-cc::egraph-add eg 'cl-cc::logand cn1 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logand-self ────────────────────────────────────────────────

(deftest egraph-rule-logand-self-fires
  "logand-self: (logand ?x ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::logand x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-zero ────────────────────────────────────────────────

(deftest egraph-rule-logior-zero-fires
  "logior-zero: (logior ?x (const 0)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logior x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-zero-l ──────────────────────────────────────────────

(deftest egraph-rule-logior-zero-l-fires
  "logior-zero-l: (logior (const 0) ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logior c0 x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logior-self ────────────────────────────────────────────────

(deftest egraph-rule-logior-self-fires
  "logior-self: (logior ?x ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (id (cl-cc::egraph-add eg 'cl-cc::logior x x)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logxor-zero ────────────────────────────────────────────────

(deftest egraph-rule-logxor-zero-fires
  "logxor-zero: (logxor ?x (const 0)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logxor x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

;;; ─── Bitwise: logxor-zero-l ──────────────────────────────────────────────

(deftest egraph-rule-logxor-zero-l-fires
  "logxor-zero-l: (logxor (const 0) ?x) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::logxor c0 x)))
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

;;; ─── Bitwise: ash-zero ───────────────────────────────────────────────────

(deftest egraph-rule-ash-zero-fires
  "ash-zero: (ash ?x (const 0)) merges with ?x."
  (let* ((eg (cl-cc::make-e-graph))
         (x  (cl-cc::egraph-add eg 'cl-cc::var))
         (c0 (make-eg-const eg 0))
         (id (cl-cc::egraph-add eg 'cl-cc::ash x c0)))
    (eg-saturate eg)
    (assert-true (eg-merged-p eg id x))))

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

(deftest egraph-rule-mul-pow2-registered
  "mul-pow2 rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::mul-pow2)))

(deftest egraph-rule-mul-pow2-l-registered
  "mul-pow2-l rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::mul-pow2-l)))

(deftest egraph-rule-div-pow2-registered
  "div-pow2 rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::div-pow2)))

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

(deftest egraph-rule-null-p-const-registered
  "null-p-const rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::null-p-const)))

(deftest egraph-rule-cons-p-const-registered
  "cons-p-const rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::cons-p-const)))

(deftest egraph-rule-number-p-const-registered
  "number-p-const rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::number-p-const)))

(deftest egraph-rule-integer-p-const-registered
  "integer-p-const rule is registered."
  (assert-true (eg-rule-registered-p 'cl-cc::integer-p-const)))

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

(deftest egraph-all-51-rules-registered
  "All 51 built-in rule names are present in *egraph-rules*."
  (let* ((rules (cl-cc::egraph-builtin-rules))
         (names (mapcar (lambda (r) (getf r :name)) rules)))
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

(deftest egraph-rules-count-at-least-51
  "At least 51 rules are registered."
  (assert-true (>= (length (cl-cc::egraph-builtin-rules)) 51)))

(deftest egraph-rules-all-have-lhs-rhs
  "Every registered rule has non-null :lhs and a :rhs key."
  (dolist (rule (cl-cc::egraph-builtin-rules))
    (assert-true (getf rule :lhs))
    (assert-true (not (eq (getf rule :rhs :missing) :missing)))))

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
