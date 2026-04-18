;;;; tests/unit/optimize/egraph-extraction-tests.lisp — E-Graph Extraction/Saturation Tests
;;;
;;; Tests for saturation, extraction, cost model, enode internals,
;;; optimize-with-egraph, and the iteration-budget thresholds data table.
;;;
;;; Helper make-test-egraph and egraph-find-dst-inst are defined in
;;; egraph-tests.lisp (loaded first).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Saturation ──────────────────────────────────────────────────────────

(deftest egraph-saturate-empty-graph
  "Saturating an empty e-graph with any rules terminates immediately."
  (let* ((eg (make-test-egraph))
         (rules (cl-cc/optimize::egraph-builtin-rules)))
    (multiple-value-bind (saturated iter _fuel)
        (cl-cc/optimize::egraph-saturate eg rules :limit 10 :fuel 1000)
      (assert-true saturated)
      (assert-= 0 iter))))

(deftest egraph-saturate-respects-limit
  "Saturation respects the iteration limit."
  (let* ((eg (make-test-egraph)))
    ;; Add some nodes
    (cl-cc/optimize::egraph-add eg 'const)
    (multiple-value-bind (saturated _iter _fuel)
        (cl-cc/optimize::egraph-saturate eg nil :limit 5 :fuel 1000)
      ;; Empty rule set saturates immediately (0 merges)
      (assert-true saturated))))

;;; ─── Extraction ──────────────────────────────────────────────────────────

(deftest egraph-extract-nullary
  "Extracting a nullary e-node returns its op symbol."
  (let* ((eg (make-test-egraph))
         (id (cl-cc/optimize::egraph-add eg 'const)))
    ;; Store constant data
    (let ((cls (gethash (cl-cc/optimize::egraph-find eg id) (cl-cc::eg-classes eg))))
      (when cls (setf (cl-cc::ec-data cls) 42)))
    (let ((result (cl-cc/optimize::egraph-extract eg id)))
      (assert-true result))))

(deftest egraph-extract-binary
  "Extraction on a (add c1 c2) returns a compound expression."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize::egraph-add eg 'const))
         (c2  (cl-cc/optimize::egraph-add eg 'const))
         (add (cl-cc/optimize::egraph-add eg 'add c1 c2)))
    (let ((result (cl-cc/optimize::egraph-extract eg add)))
      ;; Should be (add ...) or similar compound
      (assert-true result))))

;;; ─── Optimize-With-Egraph ────────────────────────────────────────────────

(deftest optimize-with-egraph-identity
  "optimize-with-egraph on a simple sequence returns a non-empty list."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::optimize-with-egraph insts)))
    (assert-true (>= (length result) 1))))

(deftest optimize-with-egraph-empty
  "optimize-with-egraph on empty input returns empty."
  (let ((result (cl-cc/optimize::optimize-with-egraph nil)))
    (assert-null result)))

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

(deftest egraph-build-rhs-pattern-variable
  "egraph-build-rhs for a pattern variable looks up the binding."
  (let* ((eg      (make-test-egraph))
         (id      (cl-cc/optimize::egraph-add eg 'const))
         (bindings (list (cons '?x id)))
         (result  (cl-cc/optimize::egraph-build-rhs eg '?x bindings)))
    (assert-= id result)))

(deftest egraph-build-rhs-nullary-symbol
  "egraph-build-rhs for a literal symbol adds a nullary e-node."
  (let* ((eg     (make-test-egraph))
         (result (cl-cc/optimize::egraph-build-rhs eg 'zero nil)))
    (assert-true (integerp result))))

(deftest egraph-build-rhs-compound
  "egraph-build-rhs for a compound template builds the tree in the e-graph."
  (let* ((eg      (make-test-egraph))
         (id0     (cl-cc/optimize::egraph-add eg 'const))
         (bindings (list (cons '?x id0)))
         (result  (cl-cc/optimize::egraph-build-rhs eg '(neg ?x) bindings)))
    (assert-true (integerp result))
    (assert-true (cl-cc/optimize::egraph-class-has-op-p eg result 'neg))))

(deftest egraph-build-rhs-numeric-constant
  "egraph-build-rhs for a numeric literal creates a const e-class with data set."
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

(deftest egraph-add-instructions-maps-dst-to-class
  "egraph-add-instructions returns a hash table mapping destination registers to e-class IDs."
  (let* ((eg    (make-test-egraph))
         ;; Use add + two consts so the add node is structurally distinct
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 7)
                      (cl-cc:make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    ;; Both registers must have entries in the mapping
    (assert-true (integerp (gethash :r0 reg->class)))
    (assert-true (integerp (gethash :r1 reg->class)))
    ;; The add node's class must differ from its operand's class
    (assert-true (/= (gethash :r0 reg->class) (gethash :r1 reg->class)))))

(deftest egraph-add-instructions-const-sets-ec-data
  "egraph-add-instructions stores the vm-const value in ec-data."
  (let* ((eg    (make-test-egraph))
         (insts (list (cl-cc:make-vm-const :dst :r0 :value 42)))
         (reg->class (cl-cc/optimize::egraph-add-instructions eg insts)))
    (let* ((class-id (gethash :r0 reg->class))
           (val (cl-cc/optimize::egraph-class-const-value eg class-id)))
      (assert-= 42 val))))

;;; ─── Compound pattern matching ───────────────────────────────────────────

(deftest egraph-match-pattern-compound-op
  "Compound pattern (add ?x ?y) matches an add e-node."
  (let* ((eg  (make-test-egraph))
         (c1  (cl-cc/optimize::egraph-add eg 'const))
         (c2  (cl-cc/optimize::egraph-add eg 'var))
         (add (cl-cc/optimize::egraph-add eg 'add c1 c2))
         (matches (cl-cc/optimize::egraph-match-pattern eg '(add ?x ?y) add)))
    (assert-true (>= (length matches) 1))
    (let ((b (car matches)))
      (assert-true (assoc '?x b))
      (assert-true (assoc '?y b)))))

(deftest egraph-match-pattern-literal-symbol
  "A plain symbol pattern matches if the e-class has a nullary node with that op."
  (let* ((eg  (make-test-egraph))
         (id  (cl-cc/optimize::egraph-add eg 'zero))
         (matches (cl-cc/optimize::egraph-match-pattern eg 'zero id)))
    (assert-true (= 1 (length matches)))))

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
