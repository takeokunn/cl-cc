;;;; tests/unit/optimize/optimizer-licm-tests.lisp
;;;; Unit tests for src/optimize/optimizer-licm.lisp
;;;;
;;;; Covers: opt-inst-loop-invariant-p, %opt-pre-expression-key,
;;;;   %opt-pre-splice-before-terminator, opt-pass-licm (trivial paths),
;;;;   optimize-with-egraph.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-inst-loop-invariant-p ───────────────────────────────────────────

(deftest-each licm-invariant-p-cases
  "opt-inst-loop-invariant-p: pure const is invariant; loop-defined read reg makes it variant; impure instruction is never invariant."
  :cases (("pure-const"   (make-vm-const :dst :r0 :value 42)         nil   t)
          ("dst-in-loop"  (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0) :r0   nil)
          ("impure"       (make-vm-halt  :reg :r0)                    nil   nil))
  (inst def-reg expected)
  (let ((loop-def-regs (make-hash-table :test #'eq))
        (loop-members  (make-hash-table :test #'eq))
        (def-sites     (make-hash-table :test #'eq)))
    (when def-reg
      (setf (gethash def-reg loop-def-regs) t))
    (if expected
        (assert-true  (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites))
        (assert-false (cl-cc/optimize::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites)))))

;;; ─── %opt-pre-expression-key ─────────────────────────────────────────────

(deftest-each pre-expression-key-cases
  "%opt-pre-expression-key: (:const v) for vm-const; typed key for binary; nil for impure."
  :cases (("const"   (make-vm-const :dst :r0 :value 7) :const   '(:const 7))
          ("binary"  (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1) :binary nil)
          ("impure"  (make-vm-halt  :reg :r0)                    :null   nil))
  (inst shape expected)
  (let ((key (cl-cc/optimize::%opt-pre-expression-key inst)))
    (ecase shape
      (:const  (assert-equal expected key))
      (:binary (assert-true (consp key)) (assert-eq 'cl-cc/vm::vm-add (car key)))
      (:null   (assert-null key)))))

(deftest pre-expression-key-commutative
  "%opt-pre-expression-key produces the same key regardless of lhs/rhs order (vm-add is commutative)."
  (let ((key-ab (cl-cc/optimize::%opt-pre-expression-key (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
        (key-ba (cl-cc/optimize::%opt-pre-expression-key (make-vm-add :dst :r2 :lhs :r1 :rhs :r0))))
    (assert-equal key-ab key-ba)))

;;; ─── %opt-pre-splice-before-terminator ───────────────────────────────────

(deftest-each pre-splice-inserts-before-terminator
  "%opt-pre-splice-before-terminator places an extra instruction just before vm-jump and vm-ret terminators."
  :cases (("before-jump"  (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "end"))  'cl-cc/vm::vm-jump)
          ("before-ret"   (list (make-vm-move  :dst :r0 :src :r1) (make-vm-ret  :reg  :r0))     'cl-cc/vm::vm-ret))
  (insts term-type)
  (let* ((extra  (make-vm-const :dst :r9 :value 0))
         (result (cl-cc/optimize::%opt-pre-splice-before-terminator insts (list extra))))
    (assert-= 3 (length result))
    (assert-true (typep (second result) 'cl-cc/vm::vm-const))
    (assert-true (typep (third  result) term-type))))

(deftest pre-splice-appends-when-no-terminator
  "%opt-pre-splice-before-terminator appends at end when no terminator is present."
  (let* ((const (make-vm-const :dst :r0 :value 5))
         (extra (make-vm-const :dst :r1 :value 6))
         (result (cl-cc/optimize::%opt-pre-splice-before-terminator
                  (list const)
                  (list extra))))
    (assert-= 2 (length result))
    (assert-true (typep (second result) 'cl-cc/vm::vm-const))))

;;; ─── %opt-pre-env-evict-dst ──────────────────────────────────────────────

(deftest-each licm-pre-env-evict-dst-cases
  "%opt-pre-env-evict-dst removes all entries whose value equals DST."
  :cases (("evicts-matching"  :r0 '((:a . :r0) (:b . :r1)) '((:b . :r1)))
          ("noop-no-match"    :r9 '((:a . :r0) (:b . :r1)) '((:a . :r0) (:b . :r1)))
          ("evicts-all"       :r0 '((:a . :r0) (:b . :r0)) nil))
  (dst initial-alist expected-alist)
  (let ((env (make-hash-table :test #'equal)))
    (dolist (pair initial-alist)
      (setf (gethash (car pair) env) (cdr pair)))
    (cl-cc/optimize::%opt-pre-env-evict-dst env dst)
    (let ((result (loop for k being the hash-keys of env using (hash-value v) collect (cons k v))))
      (assert-= (length expected-alist) (length result))
      (dolist (pair expected-alist)
        (assert-equal (cdr pair) (gethash (car pair) env))))))

;;; ─── opt-pass-licm (trivial paths) ───────────────────────────────────────

(deftest licm-pass-returns-nil-for-empty-input
  "opt-pass-licm returns nil immediately for an empty instruction list."
  (assert-null (cl-cc/optimize::opt-pass-licm nil)))

(deftest licm-pass-returns-straight-line-code-unchanged
  "opt-pass-licm returns code of the same length when no loops are detected."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-licm insts)))
    (assert-= (length insts) (length result))))

;;; ─── %opt-pre-reconstruct-inst ──────────────────────────────────────────

(deftest pre-reconstruct-inst-round-trips-vm-const
  "%opt-pre-reconstruct-inst round-trips a vm-const, preserving its value."
  (let* ((inst   (make-vm-const :dst :r0 :value 42))
         (result (cl-cc/optimize::%opt-pre-reconstruct-inst inst)))
    (assert-true (typep result 'cl-cc/vm::vm-const))
    (assert-equal 42 (cl-cc/vm::vm-value result))))

(deftest pre-reconstruct-inst-passthrough-for-unrecognized
  "%opt-pre-reconstruct-inst returns the original instruction when it cannot round-trip it."
  (let* ((inst   (make-vm-halt :reg :r0))
         (result (cl-cc/optimize::%opt-pre-reconstruct-inst inst)))
    (assert-true (typep result 'cl-cc/vm::vm-halt))))

;;; ─── %opt-pre-available-in-any-p ────────────────────────────────────────

(defun %pre-envs-with-key (key in-env1-p in-env2-p)
  "Build two predecessor (label . env) pairs; seed KEY in each env according to the boolean flags."
  (let ((e1 (make-hash-table :test #'equal))
        (e2 (make-hash-table :test #'equal)))
    (when in-env1-p (setf (gethash key e1) :r0))
    (when in-env2-p (setf (gethash key e2) :r0))
    (list (cons :p1 e1) (cons :p2 e2))))

(deftest-each pre-available-in-any-p-cases
  "%opt-pre-available-in-any-p: T when key is in any predecessor env; NIL when absent from all."
  :cases (("in-first"   t   nil  t)
          ("in-second"  nil t    t)
          ("in-neither" nil nil  nil))
  (in1 in2 expected)
  (let ((preds (%pre-envs-with-key '(:const 1) in1 in2)))
    (if expected
        (assert-true  (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds))
        (assert-false (cl-cc/optimize::%opt-pre-available-in-any-p '(:const 1) preds)))))

;;; ─── %licm-collect-def-sites ────────────────────────────────────────────

(deftest licm-collect-def-sites-maps-defined-registers
  "%licm-collect-def-sites maps each defined register to the block(s) that define it."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc/optimize:cfg-build insts))
         (sites (cl-cc/optimize::%licm-collect-def-sites cfg)))
    (assert-true (gethash :r0 sites))
    (assert-true (gethash :r1 sites))
    (assert-null (gethash :r9 sites))))

(deftest licm-collect-def-sites-empty-cfg-returns-empty-table
  "%licm-collect-def-sites returns an empty hash table for an empty CFG."
  (let* ((cfg   (cl-cc/optimize:cfg-build nil))
         (sites (cl-cc/optimize::%licm-collect-def-sites cfg)))
    (assert-= 0 (hash-table-count sites))))

;;; ─── %licm-loop-def-regs ────────────────────────────────────────────────

(deftest licm-loop-def-regs-collects-from-members
  "%licm-loop-def-regs returns all registers defined in member blocks."
  (let* ((b (make-instance 'cl-cc/optimize:basic-block))
         (members (make-hash-table :test #'eq)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-const :dst :r5 :value 99)
                (make-vm-add   :dst :r6 :lhs :r5 :rhs :r5)))
    (setf (gethash b members) t)
    (let ((regs (cl-cc/optimize::%licm-loop-def-regs members)))
      (assert-true (gethash :r5 regs))
      (assert-true (gethash :r6 regs))
      (assert-null (gethash :r0 regs)))))

;;; ─── %licm-find-loop-headers (smoke test via opt-pass-licm) ────────────

(deftest licm-find-loop-headers-detects-self-loop
  "%licm-find-loop-headers identifies the header of a simple counted loop."
  (let* ((start (make-vm-label :name "start"))
         (seed  (make-vm-const :dst :r0 :value 0))
         (jmp1  (make-vm-jump  :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (hoist (make-vm-const :dst :r1 :value 1))
         (jmp2  (make-vm-jump-zero :reg :r0 :label "exit"))
         (body  (make-vm-label :name "body"))
         (back  (make-vm-jump  :label "loop"))
         (exit  (make-vm-label :name "exit"))
         (ret   (make-vm-ret   :reg :r1))
         (insts (list start seed jmp1 loop hoist jmp2 body back exit ret))
         (cfg   (cl-cc/optimize:cfg-build insts)))
    (cl-cc/optimize:cfg-compute-dominators cfg)
    (cl-cc/optimize:cfg-compute-loop-depths cfg)
    (multiple-value-bind (headers _)
        (cl-cc/optimize::%licm-find-loop-headers cfg)
      (declare (ignore _))
      (assert-true (consp headers)))))

;;; ─── %opt-rewrite-block-terminator (shared helper via licm) ─────────────

(deftest-each opt-rewrite-block-terminator-cases
  "%opt-rewrite-block-terminator rewrites matching jump label; leaves non-matching unchanged."
  :cases (("jump-match"      (make-vm-jump      :label "old") "old" "new" 'cl-cc/vm::vm-jump)
          ("jump-zero-match" (make-vm-jump-zero :reg :r0 :label "old") "old" "new" 'cl-cc/vm::vm-jump-zero)
          ("no-match"        (make-vm-jump      :label "other") "old" "new" nil))
  (term-inst old new expected-type)
  (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b) (list term-inst))
    (cl-cc/optimize::%opt-rewrite-block-terminator b old new)
    (let ((result (car (cl-cc/optimize:bb-instructions b))))
      (if expected-type
          (progn
          (assert-true (typep result expected-type))
          (assert-equal new (cl-cc/vm::vm-label-name result)))
          (assert-false (equal new (cl-cc/vm::vm-label-name result)))))))

;;; ─── %licm-redirect-successor ────────────────────────────────────────────

(deftest licm-redirect-successor-updates-edges
  "%licm-redirect-successor swaps old→new in block's successors and updates predecessors."
  (let ((block (make-instance 'cl-cc/optimize:basic-block))
        (old   (make-instance 'cl-cc/optimize:basic-block))
        (new   (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors   block) (list old)
          (cl-cc/optimize:bb-predecessors old)   (list block)
          (cl-cc/optimize:bb-predecessors new)   nil)
    (cl-cc/optimize::%licm-redirect-successor block old new)
    (assert-true  (member new (cl-cc/optimize:bb-successors   block) :test #'eq))
    (assert-false (member old (cl-cc/optimize:bb-successors   block) :test #'eq))
    (assert-false (member block (cl-cc/optimize:bb-predecessors old)  :test #'eq))
    (assert-true  (member block (cl-cc/optimize:bb-predecessors new)  :test #'eq))))

;;; ─── %licm-collect-members ──────────────────────────────────────────────

(deftest licm-collect-members-returns-loop-blocks
  "%licm-collect-members returns a hash-table containing all blocks in the natural loop."
  (let* ((header (make-instance 'cl-cc/optimize:basic-block))
         (tail   (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-predecessors header) (list tail)
          (cl-cc/optimize:bb-successors   header) (list tail)
          (cl-cc/optimize:bb-predecessors tail)   (list header)
          (cl-cc/optimize:bb-successors   tail)   (list header))
    (let ((members (cl-cc/optimize::%licm-collect-members header (list tail))))
      (assert-true  (hash-table-p members))
      (assert-true  (gethash header members))
      (assert-true  (gethash tail   members)))))

;;; ─── %licm-collect-invariants ────────────────────────────────────────────

(deftest licm-collect-invariants-finds-pure-const
  "%licm-collect-invariants returns pure instructions not reading loop-defined registers."
  (let* ((b        (make-instance 'cl-cc/optimize:basic-block))
         (members  (make-hash-table :test #'eq))
         (def-sites (make-hash-table :test #'eq))
         (c42      (make-vm-const :dst :r0 :value 42)))
    (setf (cl-cc/optimize:bb-instructions b) (list c42))
    (setf (gethash b members) t)
    (setf (gethash :r0 def-sites) (list b))
    (let ((invs (cl-cc/optimize::%licm-collect-invariants members def-sites)))
      (assert-true (member c42 invs :test #'eq)))))

;;; ─── %opt-pre-block-out-env ──────────────────────────────────────────────

(deftest pre-block-out-env-maps-key-to-defining-register
  "%opt-pre-block-out-env maps the expression key (:const 42) to its destination register :r0."
  (let* ((b   (make-instance 'cl-cc/optimize:basic-block))
         (c42 (make-vm-const :dst :r0 :value 42)))
    (setf (cl-cc/optimize:bb-instructions b) (list c42))
    (let ((env (cl-cc/optimize::%opt-pre-block-out-env b)))
      (assert-true  (hash-table-p env))
      (assert-equal :r0 (gethash '(:const 42) env)))))

(deftest pre-block-out-env-removes-stale-entries-on-overwrite
  "%opt-pre-block-out-env evicts the first definition when :r0 is redefined; only (:const 2) survives."
  (let* ((b  (make-instance 'cl-cc/optimize:basic-block))
         (c1 (make-vm-const :dst :r0 :value 1))
         (c2 (make-vm-const :dst :r0 :value 2)))
    (setf (cl-cc/optimize:bb-instructions b) (list c1 c2))
    (let ((env (cl-cc/optimize::%opt-pre-block-out-env b)))
      (assert-null  (gethash '(:const 1) env))
      (assert-equal :r0 (gethash '(:const 2) env)))))

;;; ─── %opt-pre-join-elim ──────────────────────────────────────────────────

(deftest pre-join-elim-no-change-on-straight-line
  "%opt-pre-join-elim returns NIL (no changes) for straight-line code without join points."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (cfg (cl-cc/optimize:cfg-build insts)))
    (assert-false (cl-cc/optimize::%opt-pre-join-elim cfg))))

;;; ─── %opt-pre-emit-compensating ────────────────────────────────────────

(defun %make-pre-compensate-state (&optional (prior-src nil))
  "Build (pair inst pred-inserts) for %opt-pre-emit-compensating tests.
   When PRIOR-SRC is non-nil, pre-seed key :k with that register in the env."
  (let* ((pred  (make-instance 'cl-cc/optimize:basic-block))
         (env   (make-hash-table :test #'eq))
         (table (make-hash-table :test #'eq))
         (inst  (make-vm-const :dst :r0 :value 7))
         (pair  (cons pred env)))
    (when prior-src (setf (gethash :k env) prior-src))
    (values pair inst table pred)))

(deftest pre-emit-compensating-fresh-key
  "%opt-pre-emit-compensating emits a copy of INST when key has no prior src."
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (assert-= 1 (length (gethash pred table)))
    (assert-eq :r0 (gethash :k (cdr pair)))))

(deftest pre-emit-compensating-different-src
  "%opt-pre-emit-compensating emits a vm-move from prior-src to dst when key has a different src."
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state :r1)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (let ((emitted (first (gethash pred table))))
      (assert-true (cl-cc/vm::vm-move-p emitted))
      (assert-eq :r0 (cl-cc/vm::vm-move-dst emitted))
      (assert-eq :r1 (cl-cc/vm::vm-move-src emitted)))))

(deftest pre-emit-compensating-same-src-noop
  "%opt-pre-emit-compensating does NOT insert when src already equals dst."
  (multiple-value-bind (pair inst table pred) (%make-pre-compensate-state :r0)
    (cl-cc/optimize::%opt-pre-emit-compensating pair :k :r0 inst table)
    (assert-null (gethash pred table))))

;;; ─── opt-pass-pre ────────────────────────────────────────────────────────

(deftest pre-pass-returns-instruction-list
  "opt-pass-pre returns an instruction list for straight-line code (no loops, no PRE opportunities)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-pre insts)))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

;;; ─── optimize-with-egraph ────────────────────────────────────────────────

(deftest egraph-pass-returns-list-for-empty-input
  "optimize-with-egraph on empty instruction list returns a list (possibly empty)."
  (let ((result (cl-cc/optimize:optimize-with-egraph nil)))
    (assert-true (listp result))))
