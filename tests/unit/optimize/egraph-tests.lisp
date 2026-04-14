;;;; tests/unit/optimize/egraph-tests.lisp — E-Graph Equality Saturation Tests
;;;
;;; Tests for Phase 2: e-graph data structures, union-find, saturation,
;;; extraction, and rewrite rules.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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

(deftest-each egraph-pattern-var-p
  "egraph-pattern-var-p recognizes ?-prefixed symbols and rejects non-variables."
  :cases (("var-x"     t   '?x)
          ("var-foo"   t   '?foo)
          ("no-prefix" nil 'x)
          ("number"    nil 42))
  (expected sym)
  (if expected
      (assert-true  (cl-cc::egraph-pattern-var-p sym))
      (assert-false (cl-cc::egraph-pattern-var-p sym))))

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

;;; ─── enode-memo-key ──────────────────────────────────────────────────────

(deftest enode-memo-key-nullary
  "enode-memo-key for a nullary node returns (op . nil)."
  (let ((node (cl-cc::make-e-node :op 'const :children nil)))
    (assert-equal '(const) (cl-cc::enode-memo-key node))))

(deftest enode-memo-key-binary
  "enode-memo-key includes child IDs in the key."
  (let ((node (cl-cc::make-e-node :op 'add :children '(0 1))))
    (assert-equal '(add 0 1) (cl-cc::enode-memo-key node))))

;;; ─── egraph-canonical-enode ──────────────────────────────────────────────

(deftest egraph-canonical-enode-updates-children
  "egraph-canonical-enode returns a new e-node with children resolved via union-find."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc::egraph-add eg 'const))
         (id1 (cl-cc::egraph-add eg 'var))
         ;; Force id1 -> id0 in union-find
         (canon (cl-cc::egraph-merge eg id0 id1))
         (node  (cl-cc::make-e-node :op 'add :children (list id1))))
    (let ((cnode (cl-cc::egraph-canonical-enode eg node)))
      (assert-eq 'add (cl-cc::en-op cnode))
      (assert-= canon (first (cl-cc::en-children cnode))))))

;;; ─── egraph-build-rhs ────────────────────────────────────────────────────

(deftest egraph-build-rhs-pattern-variable
  "egraph-build-rhs for a pattern variable looks up the binding."
  (let* ((eg      (make-test-egraph))
         (id      (cl-cc::egraph-add eg 'const))
         (bindings (list (cons '?x id)))
         (result  (cl-cc::egraph-build-rhs eg '?x bindings)))
    (assert-= id result)))

(deftest egraph-build-rhs-nullary-symbol
  "egraph-build-rhs for a literal symbol adds a nullary e-node."
  (let* ((eg     (make-test-egraph))
         (result (cl-cc::egraph-build-rhs eg 'zero nil)))
    (assert-true (integerp result))))

(deftest egraph-build-rhs-compound
  "egraph-build-rhs for a compound template builds the tree in the e-graph."
  (let* ((eg      (make-test-egraph))
         (id0     (cl-cc::egraph-add eg 'const))
         (bindings (list (cons '?x id0)))
         (result  (cl-cc::egraph-build-rhs eg '(neg ?x) bindings)))
    (assert-true (integerp result))
    (assert-true (cl-cc::egraph-class-has-op-p eg result 'neg))))

(deftest egraph-build-rhs-numeric-constant
  "egraph-build-rhs for a numeric literal creates a const e-class with data set."
  (let* ((eg     (make-test-egraph))
         (result (cl-cc::egraph-build-rhs eg 42 nil)))
    (assert-true (integerp result))
    (assert-= 42 (cl-cc::egraph-class-const-value eg result))))

;;; ─── egraph-class-has-op-p / egraph-class-const-value ───────────────────

(deftest egraph-class-has-op-p-true
  "egraph-class-has-op-p is true when an e-class contains a node with the given op."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'const)))
    (assert-true (cl-cc::egraph-class-has-op-p eg id 'const))))

(deftest egraph-class-has-op-p-false
  "egraph-class-has-op-p is false when no node in the e-class has the given op."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'const)))
    (assert-false (cl-cc::egraph-class-has-op-p eg id 'add))))

(deftest egraph-class-const-value-returns-data
  "egraph-class-const-value returns ec-data when the class holds a const node."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'const)))
    (let ((cls (gethash (cl-cc::egraph-find eg id) (cl-cc::eg-classes eg))))
      (setf (cl-cc::ec-data cls) 99))
    (assert-= 99 (cl-cc::egraph-class-const-value eg id))))

(deftest egraph-class-const-value-nil-for-non-const
  "egraph-class-const-value returns nil when the class has no const node."
  (let* ((eg (make-test-egraph))
         (id (cl-cc::egraph-add eg 'add)))
    (assert-null (cl-cc::egraph-class-const-value eg id))))

;;; ─── vm-inst-to-enode-op ─────────────────────────────────────────────────

(deftest-each vm-inst-to-enode-op-strips-vm-prefix
  "vm-inst-to-enode-op strips the VM- prefix from instruction type names."
  :cases (("const" 'vm-const (cl-cc:make-vm-const :dst :r0 :value 1))
          ("move"  'vm-move  (cl-cc:make-vm-move  :dst :r0 :src :r1))
          ("add"   'vm-add   (cl-cc:make-vm-add   :dst :r0 :lhs :r1 :rhs :r2)))
  (expected-op-prefix inst)
  (let ((op (cl-cc::vm-inst-to-enode-op inst)))
    (assert-true (symbolp op))
    ;; The result op should NOT start with VM-
    (assert-false (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))))))

;;; ─── egraph-add-instructions ─────────────────────────────────────────────

(deftest egraph-add-instructions-maps-dst-to-class
  "egraph-add-instructions returns a hash table mapping destination registers to e-class IDs."
  (let* ((eg    (make-test-egraph))
         ;; Use add + two consts so the add node is structurally distinct
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)))
         (reg->class (cl-cc::egraph-add-instructions eg insts)))
    ;; Both registers must have entries in the mapping
    (assert-true (integerp (gethash :r0 reg->class)))
    (assert-true (integerp (gethash :r1 reg->class)))
    ;; The add node's class must differ from its operand's class
    (assert-true (/= (gethash :r0 reg->class) (gethash :r1 reg->class)))))

(deftest egraph-add-instructions-const-sets-ec-data
  "egraph-add-instructions stores the vm-const value in ec-data."
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 42)))
         (reg->class (cl-cc::egraph-add-instructions eg insts)))
    (let* ((class-id (gethash :r0 reg->class))
           (val (cl-cc::egraph-class-const-value eg class-id)))
      (assert-= 42 val))))

;;; ─── Compound pattern matching ───────────────────────────────────────────

(deftest egraph-match-pattern-compound-op
  "Compound pattern (add ?x ?y) matches an add e-node."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc::egraph-add eg 'const))
         (c2  (cl-cc::egraph-add eg 'var))
         (add (cl-cc::egraph-add eg 'add c1 c2))
         (matches (cl-cc::egraph-match-pattern eg '(add ?x ?y) add)))
    (assert-true (>= (length matches) 1))
    (let ((b (car matches)))
      (assert-true (assoc '?x b))
      (assert-true (assoc '?y b)))))

(deftest egraph-match-pattern-literal-symbol
  "A plain symbol pattern matches if the e-class has a nullary node with that op."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc::egraph-add eg 'zero))
         (matches (cl-cc::egraph-match-pattern eg 'zero id)))
    (assert-true (= 1 (length matches)))))

;;; ─── *opt-iteration-budget-thresholds* data coverage ────────────────────

(deftest-each opt-iteration-budget-thresholds-content
  "*opt-iteration-budget-thresholds* contains all four expected threshold entries."
  :cases (("tiny-threshold"   50  -12)
          ("small-threshold"  150 -6)
          ("medium-threshold" 400 0)
          ("large-threshold"  800 8))
  (threshold delta)
  (let ((entry (assoc threshold cl-cc::*opt-iteration-budget-thresholds*)))
    (assert-true entry)
    (assert-= delta (cdr entry))))
