;;;; tests/unit/optimize/egraph-extraction-tests.lisp — E-Graph Extraction/Saturation Tests
;;;
;;; Tests for saturation, extraction, cost model, enode internals,
;;; optimize-with-egraph, and the iteration-budget thresholds data table.
;;;
;;; Helper make-test-egraph and egraph-find-dst-inst are defined in
;;; egraph-tests.lisp (loaded first).

(in-package :cl-cc/test)
(in-suite cl-cc-coverage-unstable-unit-suite)

;;; ─── Saturation ──────────────────────────────────────────────────────────

(deftest egraph-saturate-empty-graph-terminates-at-iter-0
  "Saturating an empty e-graph with builtin rules terminates immediately at iteration 0."
  (let* ((eg (make-test-egraph))
         (rules (cl-cc/optimize::egraph-builtin-rules)))
    (multiple-value-bind (saturated iter _fuel)
        (cl-cc/optimize::egraph-saturate eg rules :limit 10 :fuel 1000)
      (assert-true saturated)
      (assert-= 0 iter))))

(deftest egraph-saturate-with-empty-rules-terminates
  "Saturating a non-empty e-graph with an empty rule set terminates as saturated."
  (let* ((eg (make-test-egraph)))
    (cl-cc/optimize::egraph-add eg 'const)
    (multiple-value-bind (saturated _iter _fuel)
        (cl-cc/optimize::egraph-saturate eg nil :limit 5 :fuel 1000)
      (assert-true saturated))))

;;; ─── Extraction ──────────────────────────────────────────────────────────

(deftest egraph-extract-nullary-node-returns-non-nil
  "Extracting from an e-class containing a nullary const node returns a non-nil result."
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize::egraph-add eg 'const)))
    (let ((cls (gethash (cl-cc/optimize::egraph-find eg id) (cl-cc::eg-classes eg))))
      (when cls (setf (cl-cc::ec-data cls) 42)))
    (assert-true (cl-cc/optimize::egraph-extract eg id))))

(deftest egraph-extract-binary-add-returns-compound
  "Extracting from a binary (add c1 c2) e-class returns a compound expression."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize::egraph-add eg 'const))
         (c2  (cl-cc/optimize::egraph-add eg 'const))
         (add (cl-cc/optimize::egraph-add eg 'add c1 c2)))
    (assert-true (cl-cc/optimize::egraph-extract eg add))))

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

(deftest optimize-with-egraph-simple-sequence-returns-non-empty
  "optimize-with-egraph on a simple const+ret sequence returns at least one instruction."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::optimize-with-egraph insts)))
    (assert-true (>= (length result) 1))))

(deftest optimize-with-egraph-empty-input-returns-nil
  "optimize-with-egraph on nil input returns nil."
  (assert-null (cl-cc/optimize::optimize-with-egraph nil)))

(deftest optimize-with-egraph-lowers-const
  "A class proven equal to a constant is rewritten as vm-const."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0)
                      (cl-cc:make-vm-ret   :reg :r1)))
         (out (cl-cc/optimize::optimize-with-egraph insts))
         (r1  (egraph-find-dst-inst out :r1)))
    (assert-true (cl-cc::vm-const-p r1))
    (assert-equal 0 (cl-cc/vm::vm-value r1))))

(deftest optimize-with-egraph-lowers-alias
  "A class proven equal to another register is rewritten as vm-move."
  (let* ((insts (list (cl-cc:make-vm-const :dst :r0 :value 0)
                      (cl-cc:make-vm-const :dst :r1 :value 5)
                      (cl-cc:make-vm-cons  :dst :r2 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-cons  :dst :r3 :car-src :r1 :cdr-src :r0)
                      (cl-cc:make-vm-ret   :reg :r3)))
         (out (cl-cc/optimize::optimize-with-egraph insts))
         (r3  (egraph-find-dst-inst out :r3)))
    (assert-true (cl-cc::vm-move-p r3))
    (assert-equal :r2 (cl-cc/vm::vm-src r3))))

;;; ─── Cost Model ──────────────────────────────────────────────────────────

(deftest-each egraph-cost-model-cases
  "egraph-default-cost returns the correct cost for each node type."
  :cases (("const"  'const nil   0)
          ("add"    'add   '(1 1) 3)
          ("call"   'call  nil   10))
  (op children expected)
  (assert-= expected (cl-cc/optimize::egraph-default-cost op children)))

;;; ─── enode-memo-key ──────────────────────────────────────────────────────

(deftest-each enode-memo-key
  "enode-memo-key returns a list of op followed by child class IDs."
  :cases (("nullary" 'const nil    '(const))
          ("binary"  'add   '(0 1) '(add 0 1)))
  (op children expected)
  (let ((node (cl-cc/optimize::make-e-node :op op :children children)))
    (assert-equal expected (cl-cc/optimize::enode-memo-key node))))

;;; ─── egraph-canonical-enode ──────────────────────────────────────────────

(deftest egraph-canonical-enode-updates-children
  "egraph-canonical-enode returns a new e-node with children resolved via union-find."
  (let* ((eg  (make-test-egraph))
         (id0 (cl-cc/optimize::egraph-add eg 'const))
         (id1 (cl-cc/optimize::egraph-add eg 'var))
         ;; Force id1 -> id0 in union-find
         (canon (cl-cc/optimize::egraph-merge eg id0 id1))
         (node  (cl-cc/optimize::make-e-node :op 'add :children (list id1))))
    (let ((cnode (cl-cc/optimize::egraph-canonical-enode eg node)))
      (assert-eq 'add (cl-cc::en-op cnode))
      (assert-= canon (first (cl-cc::en-children cnode))))))

;;; ─── egraph-build-rhs ────────────────────────────────────────────────────

(deftest egraph-build-rhs-pattern-var-looks-up-binding
  "egraph-build-rhs: a pattern variable (?x) returns the bound e-class ID."
  (let* ((eg      (make-test-egraph))
         (id      (cl-cc/optimize::egraph-add eg 'const))
         (bindings (list (cons '?x id))))
    (assert-= id (cl-cc/optimize::egraph-build-rhs eg '?x bindings))))

(deftest egraph-build-rhs-symbol-adds-nullary-node
  "egraph-build-rhs: a plain symbol adds a nullary e-node and returns an integer class ID."
  (let* ((eg (make-test-egraph)))
    (assert-true (integerp (cl-cc/optimize::egraph-build-rhs eg 'zero nil)))))

(deftest egraph-build-rhs-compound-builds-tree-with-op
  "egraph-build-rhs: a compound form (neg ?x) adds a neg node and returns its class."
  (let* ((eg      (make-test-egraph))
         (id0     (cl-cc/optimize::egraph-add eg 'const))
         (bindings (list (cons '?x id0)))
         (result  (cl-cc/optimize::egraph-build-rhs eg '(neg ?x) bindings)))
    (assert-true (integerp result))
    (assert-true (cl-cc/optimize::egraph-class-has-op-p eg result 'neg))))

(deftest egraph-build-rhs-numeric-sets-ec-data
  "egraph-build-rhs: a numeric literal creates a const e-class with that value as ec-data."
  (let* ((eg     (make-test-egraph))
         (result (cl-cc/optimize::egraph-build-rhs eg 42 nil)))
    (assert-true (integerp result))
    (assert-= 42 (cl-cc/optimize::egraph-class-const-value eg result))))

;;; ─── egraph-class-has-op-p / egraph-class-const-value ───────────────────

(deftest-each egraph-class-has-op-p
  "egraph-class-has-op-p is true only when the e-class holds a node with the queried op."
  :cases (("matching-op"  'const t)
          ("different-op" 'add   nil))
  (query-op expected)
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize::egraph-add eg 'const)))
    (if expected
        (assert-true  (cl-cc/optimize::egraph-class-has-op-p eg id query-op))
        (assert-false (cl-cc/optimize::egraph-class-has-op-p eg id query-op)))))

(deftest-each egraph-class-const-value-cases
  "egraph-class-const-value: returns ec-data for a const node; nil for a non-const node."
  :cases (("const-returns-data" 'const t   99)
          ("non-const-nil"      'add   nil nil))
  (op set-data-p expected)
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize::egraph-add eg op)))
    (when set-data-p
      (let ((cls (gethash (cl-cc/optimize::egraph-find eg id) (cl-cc::eg-classes eg))))
        (setf (cl-cc::ec-data cls) 99)))
    (if expected
        (assert-= expected (cl-cc/optimize::egraph-class-const-value eg id))
        (assert-null (cl-cc/optimize::egraph-class-const-value eg id)))))

;;; ─── vm-inst-to-enode-op ─────────────────────────────────────────────────

(deftest-each vm-inst-to-enode-op-strips-vm-prefix
  "vm-inst-to-enode-op strips the VM- prefix from instruction type names."
  :cases (("const" 'vm-const (cl-cc:make-vm-const :dst :r0 :value 1))
          ("move"  'vm-move  (cl-cc:make-vm-move  :dst :r0 :src :r1))
          ("add"   'vm-add   (cl-cc:make-vm-add   :dst :r0 :lhs :r1 :rhs :r2)))
  (expected-op-prefix inst)
  (let ((op (cl-cc/optimize::vm-inst-to-enode-op inst)))
    (assert-true (symbolp op))
    ;; The result op should NOT start with VM-
    (assert-false (string= "VM-" (subseq (symbol-name op) 0 (min 3 (length (symbol-name op))))))))

;;; ─── egraph-add-instructions ─────────────────────────────────────────────

(deftest egraph-add-instructions-maps-dst-registers-to-distinct-classes
  "egraph-add-instructions maps dst registers to integer class IDs; add dst differs from its operands."
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    (assert-true (integerp (gethash :r0 reg->class)))
    (assert-true (integerp (gethash :r1 reg->class)))
    (assert-true (/= (gethash :r0 reg->class) (gethash :r1 reg->class)))))

(deftest egraph-add-instructions-const-stores-ec-data
  "egraph-add-instructions stores the integer value as ec-data for vm-const instructions."
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 42)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    (assert-= 42 (cl-cc/optimize::egraph-class-const-value eg (gethash :r0 reg->class)))))

;;; ─── Compound pattern matching ───────────────────────────────────────────

(deftest egraph-match-pattern-compound-binds-pattern-vars
  "egraph-match-pattern on (add ?x ?y) returns matches with both ?x and ?y bound."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize::egraph-add eg 'const))
         (c2  (cl-cc/optimize::egraph-add eg 'var))
         (add (cl-cc/optimize::egraph-add eg 'add c1 c2))
         (matches (cl-cc/optimize::egraph-match-pattern eg '(add ?x ?y) add)))
    (assert-true (>= (length matches) 1))
    (let ((b (car matches)))
      (assert-true (assoc '?x b))
      (assert-true (assoc '?y b)))))

(deftest egraph-match-pattern-literal-symbol-matches-nullary-node
  "egraph-match-pattern on a literal symbol produces exactly one match for a nullary node."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize::egraph-add eg 'zero))
         (matches (cl-cc/optimize::egraph-match-pattern eg 'zero id)))
    (assert-= 1 (length matches))))

;;; ─── *opt-iteration-budget-thresholds* data coverage ────────────────────

(deftest-each opt-iteration-budget-thresholds-content
  "*opt-iteration-budget-thresholds* contains all four expected threshold entries."
  :cases (("tiny-threshold"   50  -12)
          ("small-threshold"  150 -6)
          ("medium-threshold" 400 0)
          ("large-threshold"  800 8))
  (threshold delta)
  (let ((entry (assoc threshold cl-cc/optimize::*opt-iteration-budget-thresholds*)))
    (assert-true entry)
    (assert-= delta (cdr entry))))

;;; ─── %egraph-rewrite-inst ────────────────────────────────────────────────

(deftest egraph-rewrite-inst-no-dst-returns-unchanged
  "%egraph-rewrite-inst: instruction with no dst (e.g. vm-ret) is returned unchanged."
  (let* ((eg         (make-test-egraph))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (ret        (make-vm-ret :reg :r0)))
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst ret eg reg-map class->regs)))
      (assert-eq ret result))))

(deftest egraph-rewrite-inst-dst-in-const-class-becomes-vm-const
  "%egraph-rewrite-inst rewrites to vm-const when the dst class is proven constant."
  (let* ((eg         (make-test-egraph))
         (class-id   (cl-cc/optimize::egraph-add eg 'const))
         (class-ht   (gethash (cl-cc/optimize::egraph-find eg class-id) (cl-cc::eg-classes eg)))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (move       (make-vm-move :dst :r1 :src :r0)))
    (setf (cl-cc::ec-data class-ht) 42)
    (setf (gethash :r1 reg-map) class-id)
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst move eg reg-map class->regs)))
      (assert-true (cl-cc::vm-const-p result))
      (assert-eq :r1 (cl-cc::vm-dst result))
      (assert-equal 42 (cl-cc/vm::vm-value result)))))

(deftest egraph-rewrite-inst-dst-with-alias-becomes-vm-move
  "%egraph-rewrite-inst rewrites to vm-move when dst has an alias in the same e-class."
  (let* ((eg         (make-test-egraph))
         (class-id   (cl-cc/optimize::egraph-add eg 'add
                       (cl-cc/optimize::egraph-add eg 'const)
                       (cl-cc/optimize::egraph-add eg 'const)))
         (reg-map    (make-hash-table :test #'eq))
         (class->regs (make-hash-table :test #'equal))
         (canon      (cl-cc/optimize::egraph-find eg class-id))
         (add-inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (setf (gethash :r2 reg-map) class-id)
    (setf (gethash canon class->regs) (list :r2 :r3))
    (let ((result (cl-cc/optimize::%egraph-rewrite-inst add-inst eg reg-map class->regs)))
      (assert-true (cl-cc::vm-move-p result))
      (assert-eq :r2 (cl-cc::vm-dst result))
      (assert-eq :r3 (cl-cc/vm::vm-src result)))))
