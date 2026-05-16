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
  (cl-cc/optimize:make-basic-block :id id))

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(deftest-each ssa-versioned-reg-format
  "ssa-versioned-reg produces correctly formatted register keywords."
  :cases (("version-3" :r5 3 "R5.3")
          ("version-0" :r0 0 "R0.0"))
  (reg ver expected)
  (assert-equal expected (symbol-name (cl-cc/optimize:ssa-versioned-reg reg ver))))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(deftest ssa-rename-state-push-pop
  "ssr-push-new-version and ssr-pop-version maintain a stack correctly."
  (let ((s (cl-cc/optimize:make-ssa-rename-state)))
    (let ((v0 (cl-cc/optimize:ssr-push-new-version s :r0)))
      (assert-eq v0 (cl-cc/optimize:ssr-current-version s :r0))
      (let ((v1 (cl-cc/optimize:ssr-push-new-version s :r0)))
        (assert-eq v1 (cl-cc/optimize:ssr-current-version s :r0))
        (cl-cc/optimize:ssr-pop-version s :r0)
        (assert-eq v0 (cl-cc/optimize:ssr-current-version s :r0))))))

(deftest ssa-rename-state-unversioned
  "Unknown register returns itself before any version is assigned."
  (let ((s (cl-cc/optimize:make-ssa-rename-state)))
    (assert-eq :r99 (cl-cc/optimize:ssr-current-version s :r99))))

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
  (let ((result (cl-cc/optimize:ssa-round-trip insts)))
    (funcall assert-fn result)))

;;; ─── Phi Placement ───────────────────────────────────────────────────────

(deftest ssa-phi-placement-no-phis-linear
  "A linear sequence (no branches) needs no phi-nodes."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
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
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore _phi _renamed))
      (assert-true (cl-cc/optimize:cfg-entry cfg)))))

(deftest ssa-phi-placement-prunes-never-read-reg
  "ssa-place-phis skips phi insertion for registers that are never read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-label :name "join")
                      ;; join never reads :r1
                      (make-vm-ret :reg :r0))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((total-phis 0)
            (has-r1-phi nil))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (incf total-phis (length phis))
                   (when (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1)) phis)
                     (setf has-r1-phi t)))
                 phi-map)
        (assert-false has-r1-phi)
        (assert-= 0 total-phis)))))

(deftest ssa-phi-placement-prunes-local-only-reads
  "ssa-place-phis skips phis when a register is only read inside defining blocks."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-add :dst :r3 :lhs :r1 :rhs :r0)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r0))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((has-r1-phi nil))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (when (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1)) phis)
                     (setf has-r1-phi t)))
                 phi-map)
        (assert-false has-r1-phi)))))

(deftest ssa-phi-placement-keeps-live-in-join-phi
  "ssa-place-phis keeps a join phi when a multi-defined register is read at the join."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "then")
                      (make-vm-const :dst :r1 :value 10)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "then")
                      (make-vm-const :dst :r1 :value 20)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-ret :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc/optimize:ssa-construct insts)
      (declare (ignore cfg _renamed))
      (let ((join (cl-cc/optimize:cfg-get-block-by-label cfg "join")))
        (assert-true
         (find-if (lambda (p) (eq (cl-cc:phi-reg p) :r1))
                  (gethash join phi-map)))))))

(deftest ssa-lcssa-inserts-exit-phi-for-loop-defined-value
  "ssa-place-lcssa-phis adds an exit phi when a loop-defined reg is read outside." 
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-const :dst :lim :value 2)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :i)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (phi-map nil)
         (exit-block nil)
         (has-i-phi nil))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-dominance-frontiers cfg)
    (setf phi-map (cl-cc/optimize:ssa-place-phis cfg))
    (setf phi-map (cl-cc/optimize::ssa-place-lcssa-phis cfg phi-map))
    (setf exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "Lexit"))
    (setf has-i-phi
          (find-if (lambda (p) (eq (cl-cc:phi-reg p) :i))
                   (gethash exit-block phi-map)))
    (assert-true has-i-phi)))

(deftest ssa-trivial-phi-elimination-rewrites-uses
  "Trivial phi elimination removes dead phis and rewrites downstream uses."
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi    (cl-cc/optimize:make-ssa-phi
                  :dst (cl-cc/optimize:ssa-versioned-reg :r1 0)
                  :args (list (cons pred-a :r2)
                              (cons pred-b :r2))
                  :reg :r1))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst    (make-vm-add :dst :r3 :lhs (cl-cc:phi-dst phi) :rhs :r4)))
    (setf (gethash join phi-map) (list phi)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore new-phi-map))
      (let ((rewritten (first (gethash join new-renamed-map))))
        (assert-eq :r2 (cl-cc/vm::vm-lhs rewritten))
        (assert-eq :r4 (cl-cc/vm::vm-rhs rewritten))))))

(deftest ssa-trivial-phi-elimination-shortcuts-phi-of-phi-chain
  "Phi elimination shortcuts A=phi(B,...) through a predecessor-local B phi."
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                  :dst :b.0
                  :args (list (cons pred-a :b-from-1)
                              (cons pred-b :b-from-2))
                  :reg :b))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                  :dst :a.0
                  :args (list (cons pred-b :b.0)
                              (cons pred-a :c.0))
                  :reg :a))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash pred-b phi-map) (list phi-b)
          (gethash join phi-map) (list phi-a)
          (gethash join renamed) (list (make-vm-ret :reg :a.0)))
    (multiple-value-bind (new-phi-map _new-renamed)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (declare (ignore _new-renamed))
      (let* ((new-phi-a (find-if (lambda (p) (eq (cl-cc:phi-dst p) :a.0))
                                 (gethash join new-phi-map)))
             (shortcut (assoc pred-b (cl-cc:phi-args new-phi-a) :test #'eq)))
        (assert-true new-phi-a)
        (assert-eq :b-from-2 (cdr shortcut))))))

(deftest ssa-trivial-phi-elimination-runs-all-passes-together
  "Trivial phi elimination combines shortcutting, same-arg collapse, and dead removal."
  (let* ((pred-a (make-ssa-test-block 1))
         (pred-b (make-ssa-test-block 2))
         (join   (make-ssa-test-block 3))
         (phi-b  (cl-cc/optimize:make-ssa-phi
                  :dst :b.0
                  :args (list (cons pred-a :x.0)
                              (cons pred-b :x.0))
                  :reg :b))
         (phi-a  (cl-cc/optimize:make-ssa-phi
                  :dst :a.0
                  :args (list (cons pred-b :b.0)
                              (cons pred-a :x.0))
                  :reg :a))
         (phi-same (cl-cc/optimize:make-ssa-phi
                    :dst :same.0
                    :args (list (cons pred-a :v.0)
                                (cons pred-b :v.0))
                    :reg :same))
         (phi-dead (cl-cc/optimize:make-ssa-phi
                    :dst :dead.0
                    :args (list (cons pred-a :d1.0)
                                (cons pred-b :d2.0))
                    :reg :dead))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r9 :lhs :a.0 :rhs :same.0)))
    (setf (gethash pred-b phi-map) (list phi-b)
          (gethash join phi-map) (list phi-a phi-same phi-dead)
          (gethash join renamed) (list inst))
    (multiple-value-bind (new-phi-map new-renamed-map)
        (cl-cc/optimize:ssa-eliminate-trivial-phis phi-map renamed)
      (let ((total-phis 0)
            (rewritten (first (gethash join new-renamed-map))))
        (maphash (lambda (_b phis)
                   (declare (ignore _b))
                   (incf total-phis (length phis)))
                 new-phi-map)
        (assert-= 0 total-phis)
        (assert-eq :x.0 (cl-cc/vm::vm-lhs rewritten))
        (assert-eq :v.0 (cl-cc/vm::vm-rhs rewritten))))))

;;; ─── Parallel Copy Sequentialization ─────────────────────────────────────

(deftest ssa-seq-copies-behavior
  "ssa-sequentialize-copies: empty→nil; simple→N vm-move; swap→≥2 vm-move."
  ;; empty parallel copies
  (assert-null (cl-cc/optimize:ssa-sequentialize-copies nil))
  ;; simple non-conflicting: 2 copies → 2 moves
  (let* ((result (cl-cc/optimize:ssa-sequentialize-copies '((:r0 . :r1) (:r2 . :r3)))))
    (assert-= 2 (length result))
    (assert-true (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result)))
  ;; swap: needs ≥2 moves, all vm-move
  (let* ((result (cl-cc/optimize:ssa-sequentialize-copies '((:r0 . :r1) (:r1 . :r0)))))
    (assert-true (>= (length result) 2))
    (assert-true (every (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))))

(deftest ssa-destroy-places-phi-copies-before-terminator
  "ssa-destroy emits predecessor phi copies before the branch terminator."
  (let* ((insts (list (make-vm-label :name "pred")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r2)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (pred (cl-cc/optimize:cfg-get-block-by-label cfg "pred"))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash join phi-map)
          (list (cl-cc/optimize:make-ssa-phi
                 :dst :r2
                 :args (list (cons pred :r1))
                 :reg :r2))
          (gethash pred renamed) (list (make-vm-jump :label "join"))
          (gethash join renamed) (list (make-vm-ret :reg :r2)))
    (let* ((out (cl-cc/optimize:ssa-destroy cfg phi-map renamed))
           (move-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-move)) out))
           (jump-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out)))
      (assert-true move-pos)
      (assert-true jump-pos)
      (assert-true (< move-pos jump-pos)))))

(deftest ssa-destroy-keeps-conditional-edge-phi-copy-on-target-edge
  "ssa-destroy keeps target-edge phi copies out of the conditional fallthrough path."
  (let* ((insts (list (make-vm-label :name "pred")
                      (make-vm-jump-zero :reg :cond :label "join")
                      (make-vm-label :name "fallthrough")
                      (make-vm-ret :reg :rf)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r2)))
         (cfg (cl-cc/optimize:cfg-build insts))
         (pred (cl-cc/optimize:cfg-get-block-by-label cfg "pred"))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (phi-map (make-hash-table :test #'eq))
         (renamed (make-hash-table :test #'eq)))
    (setf (gethash join phi-map)
          (list (cl-cc/optimize:make-ssa-phi
                 :dst :r2
                 :args (list (cons pred :r1))
                 :reg :r2))
          (gethash pred renamed) (list (make-vm-jump-zero :reg :cond :label "join"))
          (gethash join renamed) (list (make-vm-ret :reg :r2)))
    (let* ((out (cl-cc/optimize:ssa-destroy cfg phi-map renamed))
           (jump-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
           (jump-inst (nth jump-pos out))
           (pad-name (cl-cc/vm::vm-label-name jump-inst))
           (pad-pos (position-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-label)
                                        (string= pad-name (cl-cc/vm::vm-name i))))
                                 out))
           (move-pos (position-if (lambda (i) (typep i 'cl-cc/vm::vm-move)) out)))
      (assert-true jump-pos)
      (assert-false (string= "join" pad-name))
      (assert-true pad-pos)
      (assert-true move-pos)
      (assert-true (< jump-pos move-pos))
      (assert-true (< pad-pos move-pos))
      (assert-eq :r2 (cl-cc/vm::vm-dst (nth (1+ pad-pos) out)))
      (assert-eq :r1 (cl-cc/vm::vm-src (nth (1+ pad-pos) out))))))

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
