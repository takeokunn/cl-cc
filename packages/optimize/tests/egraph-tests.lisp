;;;; tests/unit/optimize/egraph-tests.lisp — E-Graph Equality Saturation Tests
;;;
;;; Tests for Phase 2: e-graph data structures, union-find, saturation,
;;; extraction, and rewrite rules.

(in-package :cl-cc/test)

(defsuite cl-cc-coverage-unstable-unit-suite
  :description "Unit tests that are known to be unstable under source instrumentation coverage runs"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-coverage-unstable-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun make-test-egraph ()
  "Create a fresh empty e-graph."
  (cl-cc/optimize::make-e-graph))

(defun egraph-find-dst-inst (instructions dst)
  "Return the first instruction in INSTRUCTIONS whose :dst register equals DST."
  (find dst instructions :key #'cl-cc/vm::vm-dst))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

(deftest egraph-find-self
  "A newly added e-class is its own canonical representative."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize::egraph-add eg 'const)))
    (assert-= id (cl-cc/optimize::egraph-find eg id))))

(deftest-each egraph-add-id-identity
  "egraph-add: same node → same class ID (memo); different nodes → distinct IDs."
  :cases (("deduplicates" 'const 'const t)
          ("different"    'const 'var   nil))
  (op1 op2 expect-equal)
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize::egraph-add eg op1))
         (id1 (cl-cc/optimize::egraph-add eg op2)))
    (if expect-equal
        (assert-= id0 id1)
        (assert-true (/= id0 id1)))))

;;; ─── Merge ───────────────────────────────────────────────────────────────

(deftest egraph-merge-same-class
  "Merging a class with itself is a no-op."
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize::egraph-add eg 'const)))
    (cl-cc/optimize::egraph-merge eg id id)
    (assert-= id (cl-cc/optimize::egraph-find eg id))))

(deftest egraph-merge-two-classes
  "After merging two classes, egraph-find returns the same canonical ID for both."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize::egraph-add eg 'const))
         (id1 (cl-cc/optimize::egraph-add eg 'var)))
    (cl-cc/optimize::egraph-merge eg id0 id1)
    (cl-cc/optimize::egraph-rebuild eg)
    (assert-= (cl-cc/optimize::egraph-find eg id0) (cl-cc/optimize::egraph-find eg id1))))

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

(deftest-each egraph-stats-class-count
  "egraph-stats :classes is 0 for an empty graph; 1 after adding one node."
  :cases (("empty"    nil    0)
          ("one-node" 'const 1))
  (node-op expected-classes)
  (let ((eg (make-test-egraph)))
    (when node-op (cl-cc/optimize::egraph-add eg node-op))
    (assert-= expected-classes (getf (cl-cc/optimize::egraph-stats eg) :classes))))

;;; ─── Pattern Matching ────────────────────────────────────────────────────

(deftest-each egraph-pattern-var-p
  "egraph-pattern-var-p recognizes ?-prefixed symbols and rejects non-variables."
  :cases (("var-x"     t   '?x)
          ("var-foo"   t   '?foo)
          ("no-prefix" nil 'x)
          ("number"    nil 42))
  (expected sym)
  (if expected
      (assert-true  (cl-cc/optimize::egraph-pattern-var-p sym))
      (assert-false (cl-cc/optimize::egraph-pattern-var-p sym))))

(deftest egraph-match-pattern-variable
  "A pattern variable matches any e-class."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize::egraph-add eg 'const))
         (matches (cl-cc/optimize::egraph-match-pattern eg '?x id)))
    (assert-true (= 1 (length matches)))
    (assert-= id (cdr (assoc '?x (car matches))))))

(deftest egraph-match-pattern-consistent-binding
  "Same pattern variable must bind to same e-class."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize::egraph-add eg 'const))
         (id1 (cl-cc/optimize::egraph-add eg 'var))
         ;; Bind ?x to id0, then try to match ?x against id1 (inconsistent)
         (bindings (list (cons '?x id0)))
         (matches (cl-cc/optimize::egraph-match-pattern eg '?x id1 bindings)))
    ;; Should fail — ?x is already bound to id0, not id1
    (assert-null matches)))

;;; ─── Rule Application ────────────────────────────────────────────────────

(deftest-each egraph-rule-registered
  "Each builtin rewrite rule name is present in egraph-builtin-rules."
  :cases (("add-zero-r"  'cl-cc/optimize::add-zero-r)
          ("fold-add"    'cl-cc/optimize::fold-add)
          ("mul-pow2"    'cl-cc/optimize::mul-pow2))
  (rule-name)
  (let ((rules (cl-cc/optimize::egraph-builtin-rules)))
    (assert-true (find rule-name rules
                        :key (lambda (r) (getf r :name))))))

(deftest egraph-builtin-rules-consults-prolog-facts
  "egraph-builtin-rules consults the Prolog egraph-rule facts when they are available." 
  (let ((called nil))
    (with-replaced-function (cl-cc/prolog:query-all
                             (lambda (goal)
                               (setf called goal)
                               '((egraph-rule fold-add (add (const ?a) (const ?b)) (const)))))
      (let ((rules (cl-cc/optimize::egraph-builtin-rules)))
        (assert-true called)
        (let ((rule (find 'cl-cc/optimize::fold-add rules
                          :key (lambda (r) (getf r :name)))))
          (assert-true rule)
          (assert-equal '(add (const ?a) (const ?b)) (getf rule :lhs))
          (assert-equal '(const) (getf rule :rhs)))))))
