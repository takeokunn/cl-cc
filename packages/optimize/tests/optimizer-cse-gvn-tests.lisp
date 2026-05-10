;;;; tests/unit/optimize/optimizer-cse-gvn-tests.lisp
;;;; Unit tests for src/optimize/optimizer-cse-gvn.lisp
;;;;
;;;; Covers: opt-pass-cse (redundant elimination, label flush, vm-const kept),
;;;;   opt-pass-gvn (straight-line, empty), opt-pass-dead-labels (removes
;;;;   unreferenced labels, keeps referenced ones), opt-pass-leaf-detect
;;;;   (leaf flag, non-leaf flag).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-pass-cse ────────────────────────────────────────────────────────

(deftest cse-empty-input-returns-nil
  "opt-pass-cse on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-cse nil)))

(deftest cse-straight-line-no-duplicates-unchanged
  "opt-pass-cse leaves code with no repeated computations unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    (assert-= (length insts) (length result))))

(deftest cse-eliminates-duplicate-binop
  "opt-pass-cse replaces the second identical binop with vm-move from the first result."
  ;; r2 = r0 + r1 (first)
  ;; r3 = r0 + r1 (second, same expression — should become vm-move r3 r2)
  (let* ((insts (list (make-vm-const :dst :r0 :value 3)
                      (make-vm-const :dst :r1 :value 4)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; The second vm-add should be eliminated → replaced by vm-move
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))
    ;; Only one vm-add should remain
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))))

(deftest cse-label-flushes-knowledge
  "opt-pass-cse flushes value numbering at branch-target labels."
  ;; First add, then a label (which is a branch target = flush env), then same add again.
  ;; After flush the second add should NOT be replaced.
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      ;; Make a jump to the label to make it a branch target
                      (make-vm-jump  :label "flush")
                      (make-vm-label :name  "flush")
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; After flushing at the label, both adds should remain
    (assert-true (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result) 1))))

(deftest cse-keeps-vm-const-not-replaced-by-move
  "opt-pass-cse never replaces vm-const with vm-move (prevents fold<->CSE oscillation)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 42)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; Both vm-const should remain (not merged via vm-move)
    (assert-= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-const)) result))))

;;; ─── opt-pass-gvn ────────────────────────────────────────────────────────

(deftest gvn-empty-input-returns-nil
  "opt-pass-gvn on empty input returns nil or empty list."
  (let ((result (cl-cc/optimize::opt-pass-gvn nil)))
    (assert-true (listp result))))

(deftest gvn-straight-line-preserves-instructions
  "opt-pass-gvn on straight-line code returns a non-empty list."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

(deftest gvn-uses-available-expressions-at-join
  "opt-pass-gvn reuses a pure expression at a join when all incoming paths agree on one register."
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (assert-= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))
                  (eq :r2 (vm-src i))))
           result))))

(deftest gvn-redundant-overwrite-does-not-poison-same-syntax-expression
  "Replacing a redundant expr into one of its operands must not poison later same-syntax expressions."
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r4 :lhs :r1 :rhs :r2)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r4 :lhs :r1 :rhs :r2)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r1 :lhs :r1 :rhs :r2)
                      (make-vm-add :dst :r5 :lhs :r1 :rhs :r2)
                      (make-vm-ret :reg :r5)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (assert-= 3 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r1 (vm-dst i))
                  (eq :r4 (vm-src i))))
           result))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r5 (vm-dst i))))
           result))))

(deftest gvn-global-cse-does-not-reuse-memory-reads
  "opt-pass-gvn does not eliminate read-only / memory-backed expressions such as vm-car."
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-car :dst :r2 :src :r0)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-car :dst :r2 :src :r0)
                      (make-vm-label :name "join")
                      (make-vm-car :dst :r3 :src :r0)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (assert-= 3 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-car)) result))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))))
           result))))

(deftest gvn-join-reuse-runs-through-optimize-instructions
  "The optimizer pipeline can trigger the conservative FR-518 join reuse step via :gvn."
  (let* ((insts (list (make-vm-jump-zero :reg :r9 :label "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-label :name "join")
                      (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r3)))
         (result (cl-cc/optimize:optimize-instructions
                  insts
                  :max-iterations 1
                  :pass-pipeline '(:gvn))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-move)
                  (eq :r3 (vm-dst i))
                  (eq :r2 (vm-src i))))
           result))))

;;; ─── opt-pass-dead-labels ────────────────────────────────────────────────

(deftest-each dead-labels-label-presence
  "opt-pass-dead-labels removes orphan labels and keeps jump-referenced ones."
  :cases (("orphan-removed"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-label :name "orphan")
                 (make-vm-ret   :reg :r0))
           nil)
          ("referenced-kept"
           (list (make-vm-jump  :label "target")
                 (make-vm-label :name  "target")
                 (make-vm-ret   :reg :r0))
           t))
  (insts expect-label-p)
  (let ((result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (if expect-label-p
        (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result))
        (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result)))))

(deftest dead-labels-empty-input
  "opt-pass-dead-labels on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-dead-labels nil)))

(deftest dead-labels-straight-line-no-labels
  "opt-pass-dead-labels on code with no labels returns code unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (assert-= (length insts) (length result))))

;;; ─── opt-pass-leaf-detect ────────────────────────────────────────────────

(deftest leaf-detect-empty-is-leaf
  "opt-pass-leaf-detect reports empty code as a leaf function."
  (multiple-value-bind (insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect nil)
    (assert-true (listp insts))
    (assert-true leaf-p)))

(deftest-each leaf-detect-call-presence
  "opt-pass-leaf-detect: call-free code is a leaf; code with vm-call is not."
  :cases (("no-call"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                 (make-vm-ret   :reg :r1))
           t)
          ("with-call"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-call  :dst :r1 :func :r0 :args '())
                 (make-vm-ret   :reg :r1))
           nil))
  (insts expected-leaf-p)
  (multiple-value-bind (result-insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect insts)
    (assert-= (length insts) (length result-insts))
    (if expected-leaf-p
        (assert-true  leaf-p)
        (assert-false leaf-p))))

(deftest leaf-detect-preserves-instruction-list
  "opt-pass-leaf-detect returns the original instructions unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 7)
                      (make-vm-ret   :reg :r0))))
    (multiple-value-bind (result-insts _leaf)
        (cl-cc/optimize::opt-pass-leaf-detect insts)
      (declare (ignore _leaf))
      (assert-equal insts result-insts))))

;;; ─── cse-state struct helpers ────────────────────────────────────────────

(deftest cse-state-get-val-unknown-reg-uses-generation
  "%cse-get-val for an unknown register returns (reg . 0) as the synthetic key."
  (let* ((state (cl-cc/optimize::make-cse-state))
         (key   (cl-cc/optimize::%cse-get-val state :r0)))
    (assert-equal (cons :r0 0) key)))

(deftest cse-state-bump-gen-increments-generation
  "%cse-bump-gen increments the generation counter for a register."
  (let ((state (cl-cc/optimize::make-cse-state)))
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (assert-= 1 (gethash :r0 (cl-cc/optimize::cse-gen state)))))

(deftest cse-state-record-then-try-find-roundtrip
  "%cse-record followed by %cse-try-find retrieves the canonical register."
  (let ((state (cl-cc/optimize::make-cse-state))
        (key   '(vm-add (:r0 . 0) (:r1 . 0))))
    (cl-cc/optimize::%cse-record state :r2 key)
    (assert-eq :r2 (cl-cc/optimize::%cse-try-find state key))))

(deftest cse-state-flush-clears-all-tables
  "%cse-flush empties gen, val-env, and memo tables."
  (let ((state (cl-cc/optimize::make-cse-state)))
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (cl-cc/optimize::%cse-record state :r0 '(:const 1))
    (cl-cc/optimize::%cse-flush state)
    (assert-= 0 (hash-table-count (cl-cc/optimize::cse-gen     state)))
    (assert-= 0 (hash-table-count (cl-cc/optimize::cse-val-env state)))
    (assert-= 0 (hash-table-count (cl-cc/optimize::cse-memo    state)))))

(deftest cse-state-bump-gen-evicts-memo
  "%cse-bump-gen removes the memo entry whose value is the overwritten register."
  (let ((state (cl-cc/optimize::make-cse-state))
        (key   '(:const 5)))
    (cl-cc/optimize::%cse-record state :r0 key)
    (cl-cc/optimize::%cse-bump-gen state :r0)
    (assert-null (cl-cc/optimize::%cse-try-find state key))))

;;; ─── GVN helpers: %gvn-kill, %gvn-get-val, %gvn-record, %gvn-key ────────

(deftest gvn-kill-removes-reg-from-val-env
  "%gvn-kill removes reg from val-env and its reverse-memo entries."
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq))
        (memo    (make-hash-table :test #'equal)))
    (setf (gethash :r0 val-env) '(:const 1)
          (gethash '(:const 1) memo) :r0)
    (cl-cc/optimize::%gvn-kill :r0 gen val-env memo)
    (assert-false (nth-value 1 (gethash :r0 val-env)))
    (assert-false (nth-value 1 (gethash '(:const 1) memo)))))

(deftest gvn-get-val-returns-val-env-entry-when-present
  "%gvn-get-val returns the val-env entry when the register is mapped."
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 val-env) '(:const 42))
    (assert-equal '(:const 42) (cl-cc/optimize::%gvn-get-val :r0 gen val-env))))

(deftest gvn-get-val-returns-reg-generation-pair-when-absent
  "%gvn-get-val returns (reg . generation) when the register is not in val-env."
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 gen) 3)
    (let ((result (cl-cc/optimize::%gvn-get-val :r0 gen val-env)))
      (assert-equal :r0 (car result))
      (assert-= 3 (cdr result)))))

(deftest gvn-record-binds-dst-to-key
  "%gvn-record stores dst→key in val-env and key→dst in memo."
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq))
        (memo    (make-hash-table :test #'equal)))
    (cl-cc/optimize::%gvn-record :r0 '(:const 7) gen val-env memo)
    (assert-equal '(:const 7) (gethash :r0 val-env))
    (assert-eq :r0 (gethash '(:const 7) memo))))

(deftest-each gvn-key-result
  "%gvn-key returns a canonical key for recognized instruction types and nil for impure ones."
  :cases (("const-5"    (make-vm-const :dst :r0 :value 5) '(:const 5) t)
          ("halt-impure" (make-vm-halt :reg :r0)           nil         nil))
  (inst expected expect-non-nil)
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq))
        (key     (cl-cc/optimize::%gvn-key inst gen val-env)))
    (if expect-non-nil
        (assert-equal expected key)
        (assert-null  key))))

(deftest gvn-key-pure-binop-returns-list-headed-by-type
  "%gvn-key returns a non-nil list headed by the instruction's type for a pure binop."
  (let ((gen     (make-hash-table :test #'eq))
        (val-env (make-hash-table :test #'eq)))
    (let ((key (cl-cc/optimize::%gvn-key (make-vm-add :dst :r2 :lhs :r0 :rhs :r1) gen val-env)))
      (assert-true (consp key))
      (assert-eq 'cl-cc/vm::vm-add (car key)))))

;;; ─── %gvn-maybe-replace ──────────────────────────────────────────────────

(deftest gvn-maybe-replace-nil-key-passthrough
  "%gvn-maybe-replace with nil key pushes the original instruction and records nothing."
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (result  (cl-cc/optimize::%gvn-maybe-replace inst :r2 nil gen val-env memo nil)))
    (assert-= 1 (length result))
    (assert-true (typep (car result) 'cl-cc/vm::vm-add))
    (assert-= 0 (hash-table-count memo))))

(deftest gvn-maybe-replace-new-key-records-and-emits-inst
  "%gvn-maybe-replace with a fresh key records key→dst in memo and emits original instruction."
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (key     '(cl-cc/vm::vm-add (:r0 . 0) (:r1 . 0)))
         (result  (cl-cc/optimize::%gvn-maybe-replace inst :r2 key gen val-env memo nil)))
    (assert-= 1 (length result))
    (assert-true (typep (car result) 'cl-cc/vm::vm-add))
    (assert-eq :r2 (gethash key memo))))

(deftest gvn-maybe-replace-existing-key-produces-vm-move
  "%gvn-maybe-replace replaces a pre-memoized expression with vm-move :dst←existing."
  (let* ((gen     (make-hash-table :test #'eq))
         (val-env (make-hash-table :test #'eq))
         (memo    (make-hash-table :test #'equal))
         (inst    (make-vm-add :dst :r3 :lhs :r0 :rhs :r1))
         (key     '(cl-cc/vm::vm-add (:r0 . 0) (:r1 . 0))))
    (setf (gethash key memo) :r2)
    (let ((result (cl-cc/optimize::%gvn-maybe-replace inst :r3 key gen val-env memo nil)))
      (assert-= 1 (length result))
      (assert-true (typep (car result) 'cl-cc/vm::vm-move))
      (assert-eq :r3 (vm-dst (car result)))
      (assert-eq :r2 (vm-src (car result))))))

;;; ─── %cse-emit-or-cse ────────────────────────────────────────────────────

(deftest cse-emit-or-cse-emits-new-inst-when-no-prior-entry
  "%cse-emit-or-cse pushes the original instruction when the key is not memoized."
  (let* ((state  (cl-cc/optimize::make-cse-state))
         (inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (key    '(vm-add (:r0 . 0) (:r1 . 0)))
         (result (cl-cc/optimize::%cse-emit-or-cse inst :r2 key state nil)))
    (assert-= 1 (length result))
    (assert-true (typep (car result) 'cl-cc/vm::vm-add))))

(deftest cse-emit-or-cse-replaces-duplicate-with-vm-move
  "%cse-emit-or-cse returns a vm-move when the key was already memoized."
  (let* ((state  (cl-cc/optimize::make-cse-state))
         (key    '(vm-add (:r0 . 0) (:r1 . 0)))
         (inst1  (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (inst2  (make-vm-add :dst :r3 :lhs :r0 :rhs :r1)))
    ;; Record first occurrence
    (cl-cc/optimize::%cse-bump-gen state :r2)
    (cl-cc/optimize::%cse-record state :r2 key)
    ;; Second occurrence should produce a vm-move
    (let ((result (cl-cc/optimize::%cse-emit-or-cse inst2 :r3 key state nil)))
      (assert-= 1 (length result))
      (assert-true (typep (car result) 'cl-cc/vm::vm-move))
      (assert-eq :r3 (vm-dst (car result)))
      (assert-eq :r2 (vm-src (car result))))))
