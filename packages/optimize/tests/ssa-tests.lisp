;;;; tests/unit/optimize/ssa-tests.lisp — SSA Construction/Destruction Tests
;;;
;;; Tests for Phase 1: ssa-construct, ssa-round-trip, ssa-place-phis,
;;; and SSA destruction (phi elimination → parallel copies).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun count-type (insts type-sym)
  "Count instructions of TYPE-SYM in INSTS."
  (count-if (lambda (i) (typep i (find-symbol (symbol-name type-sym) :cl-cc)))
            insts))

(defun make-ssa-test-block (id)
  (cl-cc/optimize::make-basic-block :id id))

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(deftest-each ssa-versioned-reg-format
  "ssa-versioned-reg produces correctly formatted register keywords."
  :cases (("version-3" :r5 3 "R5.3")
          ("version-0" :r0 0 "R0.0"))
  (reg ver expected)
  (assert-equal expected (symbol-name (cl-cc/optimize::ssa-versioned-reg reg ver))))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(deftest ssa-rename-state-push-pop
  "ssr-push-new-version and ssr-pop-version maintain a stack correctly."
  (let ((s (cl-cc/optimize::make-ssa-rename-state)))
    (let ((v0 (cl-cc/optimize::ssr-push-new-version s :r0)))
      (assert-eq v0 (cl-cc/optimize::ssr-current-version s :r0))
      (let ((v1 (cl-cc/optimize::ssr-push-new-version s :r0)))
        (assert-eq v1 (cl-cc/optimize::ssr-current-version s :r0))
        (cl-cc/optimize::ssr-pop-version s :r0)
        (assert-eq v0 (cl-cc/optimize::ssr-current-version s :r0))))))

(deftest ssa-rename-state-unversioned
  "Unknown register returns itself before any version is assigned."
  (let ((s (cl-cc/optimize::make-ssa-rename-state)))
    (assert-eq :r99 (cl-cc/optimize::ssr-current-version s :r99))))

;;; ─── SSA Round-Trip ──────────────────────────────────────────────────────

(deftest-each ssa-round-trip-cases
  "ssa-round-trip handles various instruction sequences correctly."
  :cases (("linear-sequence"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-const :dst :r1 :value 2)
                 (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                 (make-vm-ret   :reg :r2))
           (lambda (result) (assert-true (>= (length result) 1))))
          ("preserves-types"
           (list (make-vm-const :dst :r0 :value 42)
                 (make-vm-ret   :reg :r0))
           (lambda (result)
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ret)) result))))
          ("empty"
           nil
           (lambda (result) (assert-null result)))
          ("with-label"
           (list (make-vm-const    :dst :r0 :value 1)
                 (make-vm-jump-zero :reg :r0 :label "L1")
                 (make-vm-const    :dst :r0 :value 2)
                 (make-vm-label    :name "L1")
                 (make-vm-ret      :reg :r0))
           (lambda (result)
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result)))))
  (insts assert-fn)
  (let ((result (cl-cc/optimize::ssa-round-trip insts)))
    (funcall assert-fn result)))

;;; ─── Phi Placement ───────────────────────────────────────────────────────

(deftest ssa-phi-placement-no-phis-linear
  "A linear sequence (no branches) needs no phi-nodes."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize::ssa-construct insts)
      (declare (ignore cfg _renamed))
      ;; No phi-nodes needed for linear code
      (let ((total-phis 0))
        (maphash (lambda (_b phis) (incf total-phis (length phis))) phi-map)
        (assert-= 0 total-phis)))))

(deftest ssa-phi-placement-merge-point
  "A join point (two predecessors) may receive phi-nodes."
  (let* ((insts (list (make-vm-const    :dst :r0 :value 0)
                      (make-vm-jump-zero :reg :r0 :label "merge")
                      (make-vm-const    :dst :r1 :value 42)
                      (make-vm-label    :name "merge")
                      (make-vm-ret      :reg :r1))))
    ;; Just verify construction succeeds without error
    (multiple-value-bind (cfg _phi _renamed)
        (cl-cc/optimize::ssa-construct insts)
      (declare (ignore _phi _renamed))
      (assert-true (cl-cc/optimize::cfg-entry cfg)))))

(deftest ssa-trivial-phi-elimination-rewrites-uses
  "Trivial phi elimination removes dead phis and rewrites downstream uses."
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi    (cl-cc/optimize::make-ssa-phi
                  :dst (cl-cc/optimize::ssa-versioned-reg :r1 0)
                  :args (list (cons pred-a :r2)
                              (cons pred-b :r2))
                  :reg :r1))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst    (make-vm-add :dst :r3 :lhs (cl-cc:phi-dst phi) :rhs :r4)))
    (setf (gethash join phi-map) (list phi)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize::ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore new-phi-map))
      (let ((rewritten (first (gethash join new-renamed-map))))
        (assert-eq :r2 (cl-cc/vm::vm-lhs rewritten))
        (assert-eq :r4 (cl-cc/vm::vm-rhs rewritten))))))

;;; ─── Parallel Copy Sequentialization ─────────────────────────────────────

(deftest ssa-seq-copies-behavior
  "ssa-sequentialize-copies: empty→nil; simple→N vm-move; swap→≥2 vm-move."
  ;; empty parallel copies
  (assert-null (cl-cc/optimize::ssa-sequentialize-copies nil))
  ;; simple non-conflicting: 2 copies → 2 moves
  (let* ((result (cl-cc/optimize::ssa-sequentialize-copies '((:r0 . :r1) (:r2 . :r3)))))
    (assert-= 2 (length result))
    (assert-true (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result)))
  ;; swap: needs ≥2 moves, all vm-move
  (let* ((result (cl-cc/optimize::ssa-sequentialize-copies '((:r0 . :r1) (:r1 . :r0)))))
    (assert-true (>= (length result) 2))
    (assert-true (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))))

;;; ─── %ssa-resolve-reg ────────────────────────────────────────────────────

(deftest ssa-resolve-reg-follows-chain
  "%ssa-resolve-reg follows the replacement chain to its terminal value."
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r1
          (gethash :r1 r) :r2)
    (assert-eq :r2 (cl-cc/optimize::%ssa-resolve-reg :r0 r))))

(deftest ssa-resolve-reg-identity-when-not-mapped
  "%ssa-resolve-reg returns the register itself when it has no replacement."
  (let ((r (make-hash-table :test #'eq)))
    (assert-eq :r5 (cl-cc/optimize::%ssa-resolve-reg :r5 r))))

;;; ─── %ssa-rewrite-tree ───────────────────────────────────────────────────

(deftest ssa-rewrite-tree-replaces-symbol
  "%ssa-rewrite-tree replaces a mapped symbol with its resolved replacement."
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r9)
    (assert-eq :r9 (cl-cc/optimize::%ssa-rewrite-tree :r0 r))))

(deftest ssa-rewrite-tree-passthrough-unmapped
  "%ssa-rewrite-tree passes through atoms that have no mapping."
  (let ((r (make-hash-table :test #'eq)))
    (assert-eq :r3  (cl-cc/optimize::%ssa-rewrite-tree :r3 r))
    (assert-= 42    (cl-cc/optimize::%ssa-rewrite-tree 42  r))
    (assert-eq 'foo (cl-cc/optimize::%ssa-rewrite-tree 'foo r))))

(deftest ssa-rewrite-tree-walks-cons
  "%ssa-rewrite-tree recursively rewrites both car and cdr of a cons."
  (let ((r (make-hash-table :test #'eq)))
    (setf (gethash :r0 r) :r1)
    (assert-equal '(:r1 :r2) (cl-cc/optimize::%ssa-rewrite-tree '(:r0 :r2) r))))

;;; ─── ssa-rewrite-dst ─────────────────────────────────────────────────────

(deftest ssa-rewrite-dst-changes-destination
  "ssa-rewrite-dst returns an instruction with the new destination register."
  (let* ((inst   (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::ssa-rewrite-dst inst :r0 :r9)))
    (assert-true (typep result 'cl-cc/vm::vm-const))
    (assert-eq :r9 (cl-cc/vm::vm-dst result))
    (assert-= 42 (cl-cc/vm::vm-value result))))

(deftest ssa-rewrite-dst-noop-when-no-match
  "ssa-rewrite-dst returns INST unchanged when old-dst does not appear in the sexp."
  (let* ((inst (make-vm-const :dst :r1 :value 7)))
    (assert-eq inst (cl-cc/optimize::ssa-rewrite-dst inst :r99 :r0))))

;;; ─── %ssa-collect-uses ───────────────────────────────────────────────────

(deftest ssa-collect-uses-records-instruction-reads
  "%ssa-collect-uses collects registers read by instructions in renamed-map."
  (let* ((phi-map    (make-hash-table :test #'eq))
         (renamed    (make-hash-table :test #'eq))
         (blk        (make-ssa-test-block 1))
         (inst       (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (setf (gethash blk renamed) (list inst))
    (let ((uses (cl-cc/optimize::%ssa-collect-uses phi-map renamed)))
      (assert-true (gethash :r0 uses))
      (assert-true (gethash :r1 uses))
      (assert-false (gethash :r2 uses)))))
