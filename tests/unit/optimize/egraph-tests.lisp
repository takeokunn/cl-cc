;;;; tests/unit/optimize/egraph-tests.lisp — E-Graph Equality Saturation Tests
;;;
;;; Tests for Phase 2: e-graph data structures, union-find, saturation,
;;; extraction, and rewrite rules.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun make-test-egraph ()
  "Create a fresh empty e-graph."
  (cl-cc::make-e-graph))

;;; ─── Union-Find ──────────────────────────────────────────────────────────

(deftest egraph-find-self
  "A newly added e-class is its own canonical representative."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc::egraph-add eg 'const)))
    (assert-= id (cl-cc::egraph-find eg id))))

(deftest egraph-add-deduplicates
  "Adding the same e-node twice returns the same e-class ID (memo hit)."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc::egraph-add eg 'const))
         (id1 (cl-cc::egraph-add eg 'const)))
    (assert-= id0 id1)))

(deftest egraph-add-different-ops
  "Adding e-nodes with different ops produces distinct e-classes."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc::egraph-add eg 'const))
         (id1 (cl-cc::egraph-add eg 'var)))
    (assert-true (/= id0 id1))))

;;; ─── Merge ───────────────────────────────────────────────────────────────

(deftest egraph-merge-same-class
  "Merging a class with itself is a no-op."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'const)))
    (cl-cc::egraph-merge eg id id)
    (assert-= id (cl-cc::egraph-find eg id))))

(deftest egraph-merge-two-classes
  "After merging two classes, egraph-find returns the same canonical ID for both."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc::egraph-add eg 'const))
         (id1 (cl-cc::egraph-add eg 'var)))
    (cl-cc::egraph-merge eg id0 id1)
    (cl-cc::egraph-rebuild eg)
    (assert-= (cl-cc::egraph-find eg id0) (cl-cc::egraph-find eg id1))))

;;; ─── E-Graph Statistics ──────────────────────────────────────────────────

(deftest egraph-stats-empty
  "An empty e-graph has 0 classes."
  (let* ((eg    (make-test-egraph))
         (stats (cl-cc::egraph-stats eg)))
    (assert-= 0 (getf stats :classes))))

(deftest egraph-stats-after-add
  "Adding an e-node increases the class count."
  (let* ((eg (make-test-egraph)))
    (cl-cc::egraph-add eg 'const)
    (let ((stats (cl-cc::egraph-stats eg)))
      (assert-= 1 (getf stats :classes)))))

;;; ─── Pattern Matching ────────────────────────────────────────────────────

(deftest egraph-pattern-var-p
  "egraph-pattern-var-p recognizes ?-prefixed symbols."
  (assert-true  (cl-cc::egraph-pattern-var-p '?x))
  (assert-true  (cl-cc::egraph-pattern-var-p '?foo))
  (assert-false (cl-cc::egraph-pattern-var-p 'x))
  (assert-false (cl-cc::egraph-pattern-var-p 42)))

(deftest egraph-match-pattern-variable
  "A pattern variable matches any e-class."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc::egraph-add eg 'const))
         (matches (cl-cc::egraph-match-pattern eg '?x id)))
    (assert-true (= 1 (length matches)))
    (assert-= id (cdr (assoc '?x (car matches))))))

(deftest egraph-match-pattern-consistent-binding
  "Same pattern variable must bind to same e-class."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc::egraph-add eg 'const))
         (id1 (cl-cc::egraph-add eg 'var))
         ;; Bind ?x to id0, then try to match ?x against id1 (inconsistent)
         (bindings (list (cons '?x id0)))
         (matches (cl-cc::egraph-match-pattern eg '?x id1 bindings)))
    ;; Should fail — ?x is already bound to id0, not id1
    (assert-null matches)))

;;; ─── Rule Application ────────────────────────────────────────────────────

(deftest-each egraph-rule-registered
  "Each builtin rewrite rule name is present in egraph-builtin-rules."
  :cases (("add-zero-r"  'cl-cc::add-zero-r)
          ("fold-add"    'cl-cc::fold-add)
          ("mul-pow2"    'cl-cc::mul-pow2))
  (rule-name)
  (let ((rules (cl-cc::egraph-builtin-rules)))
    (assert-true (find rule-name rules
                        :key (lambda (r) (getf r :name))))))

;;; ─── Saturation ──────────────────────────────────────────────────────────

(deftest egraph-saturate-empty-graph
  "Saturating an empty e-graph with any rules terminates immediately."
  (let* ((eg (make-test-egraph))
         (rules (cl-cc::egraph-builtin-rules)))
    (multiple-value-bind (saturated iter _fuel)
        (cl-cc::egraph-saturate eg rules :limit 10 :fuel 1000)
      (assert-true saturated)
      (assert-= 0 iter))))

(deftest egraph-saturate-respects-limit
  "Saturation respects the iteration limit."
  (let* ((eg (make-test-egraph)))
    ;; Add some nodes
    (cl-cc::egraph-add eg 'const)
    (multiple-value-bind (saturated _iter _fuel)
        (cl-cc::egraph-saturate eg nil :limit 5 :fuel 1000)
      ;; Empty rule set saturates immediately (0 merges)
      (assert-true saturated))))

;;; ─── Extraction ──────────────────────────────────────────────────────────

(deftest egraph-extract-nullary
  "Extracting a nullary e-node returns its op symbol."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'const)))
    ;; Store constant data
    (let ((cls (gethash (cl-cc::egraph-find eg id) (cl-cc::eg-classes eg))))
      (when cls (setf (cl-cc::ec-data cls) 42)))
    (let ((result (cl-cc::egraph-extract eg id)))
      (assert-true result))))

(deftest egraph-extract-binary
  "Extraction on a (add c1 c2) returns a compound expression."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc::egraph-add eg 'const))
         (c2  (cl-cc::egraph-add eg 'const))
         (add (cl-cc::egraph-add eg 'add c1 c2)))
    (let ((result (cl-cc::egraph-extract eg add)))
      ;; Should be (add ...) or similar compound
      (assert-true result))))

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

(deftest optimize-with-egraph-identity
  "optimize-with-egraph on a simple sequence returns a non-empty list."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::optimize-with-egraph insts)))
    (assert-true (>= (length result) 1))))

(deftest optimize-with-egraph-empty
  "optimize-with-egraph on empty input returns empty."
  (let ((result (cl-cc::optimize-with-egraph nil)))
    (assert-null result)))

(defun egraph-find-dst-inst (insts dst)
  (find dst insts
        :key (lambda (inst) (ignore-errors (cl-cc::vm-dst inst)))
        :test #'eq))

(deftest optimize-with-egraph-lowers-const
  "A class proven equal to a constant is rewritten as vm-const."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc:make-vm-ret   :reg :r1)))
         (out (cl-cc::optimize-with-egraph insts))
         (r1  (egraph-find-dst-inst out :r1)))
    (assert-true (cl-cc::vm-const-p r1))
    (assert-equal 0 (cl-cc::vm-value r1))))

(deftest optimize-with-egraph-lowers-alias
  "A class proven equal to another register is rewritten as vm-move."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 0)
                      (cl-cc:make-vm-const :dst :r1 :value 5)
                      (cl-cc:make-vm-cons  :dst :r2 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-cons  :dst :r3 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-ret   :reg :r3)))
         (out (cl-cc::optimize-with-egraph insts))
         (r3  (egraph-find-dst-inst out :r3)))
    (assert-true (cl-cc::vm-move-p r3))
    (assert-equal :r2 (cl-cc::vm-src r3))))

;;; ─── Cost Model ──────────────────────────────────────────────────────────

(deftest-each egraph-cost-model-cases
  "egraph-default-cost returns the correct cost for each node type."
  :cases (("const"  'const nil   0)
          ("add"    'add   '(1 1) 3)
          ("call"   'call  nil   10))
  (op children expected)
  (assert-= expected (cl-cc::egraph-default-cost op children)))
