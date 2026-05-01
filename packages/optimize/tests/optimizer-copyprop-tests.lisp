;;;; tests/unit/optimize/optimizer-copyprop-tests.lisp — Copy Propagation Tests
;;;;
;;;; Tests for src/optimize/optimizer-copyprop.lisp:
;;;;   opt-map-tree, %opt-copy-prop-env-copy/equal-p/canonical/merge,
;;;;   %opt-copy-prop-add/kill, %opt-value<, copyprop-pass-state helpers,
;;;;   opt-pass-copy-prop.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ── Helpers ─────────────────────────────────────────────────────────────────

(defun %make-copy-env (&rest pairs)
  "Build a copy-propagation environment hash-table from PAIRS (k v k v ...)."
  (let ((env (make-hash-table :test #'eq)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k env) v))
    env))

(defun %copy-env-get (env key)
  "Return the value for KEY in ENV, or NIL if absent."
  (gethash key env))

;;; ── opt-map-tree ────────────────────────────────────────────────────────────

(deftest-each copyprop-map-tree-basic
  "opt-map-tree applies fn to every leaf, preserving the cons structure."
  :cases (("atom"       2         1         (lambda (x) (if (numberp x) (* x 2) x)))
          ("nil-leaf"   nil       nil       #'identity)
          ("sym-leaf"   :r0       :r0       #'identity)
          ("double-sym" :r0       :r0       (lambda (x) (if (eq x :r1) :r0 x))))
  (expected tree fn)
  (assert-equal expected (cl-cc/optimize::opt-map-tree fn tree)))

(deftest-each copyprop-map-tree-structured
  "opt-map-tree doubles number leaves in cons pairs, proper lists, and improper lists."
  :cases (("pair"     '(1 . 2)   '(2 . 4))
          ("proper"   '(1 2 3)   '(2 4 6))
          ("improper" '(1 2 . 3) '(2 4 . 6)))
  (tree expected)
  (assert-equal expected (cl-cc/optimize::opt-map-tree (lambda (x) (if (numberp x) (* x 2) x)) tree)))

(deftest copyprop-map-tree-rewrite-register
  "opt-map-tree rewrites :r1 to :r0 everywhere inside a nested sexp."
  (let* ((sexp   '(:add :r2 :r1 :r1))
         (result (cl-cc/optimize::opt-map-tree (lambda (x) (if (eq x :r1) :r0 x)) sexp)))
    (assert-equal '(:add :r2 :r0 :r0) result)))

;;; ── %opt-copy-prop-env-copy / %opt-copy-prop-env-equal-p ───────────────────

(deftest copyprop-env-copy-is-independent
  "env-copy produces an equal copy; mutating the copy does not affect the original."
  (let* ((env  (%make-copy-env :r1 :r0 :r2 :r0))
         (copy (cl-cc/optimize::%opt-copy-prop-env-copy env)))
    (assert-true  (cl-cc/optimize::%opt-copy-prop-env-equal-p env copy))
    (remhash :r1 copy)
    (assert-false (cl-cc/optimize::%opt-copy-prop-env-equal-p env copy))))

(deftest copyprop-env-copy-empty-yields-empty
  "Copying an empty environment produces an empty environment."
  (let* ((env  (%make-copy-env))
         (copy (cl-cc/optimize::%opt-copy-prop-env-copy env)))
    (assert-equal 0 (hash-table-count copy))))

(deftest-each copyprop-env-equal-p
  "env-equal-p returns T iff both maps have identical bindings."
  :cases (("both-empty"    t   '()                    '())
          ("one-binding"   t   '(:r1 :r0)             '(:r1 :r0))
          ("two-bindings"  t   '(:r1 :r0 :r2 :r0)     '(:r2 :r0 :r1 :r0))
          ("diff-value"    nil '(:r1 :r0)             '(:r1 :r2))
          ("size-mismatch" nil '(:r1 :r0 :r2 :r0)     '(:r1 :r0))
          ("extra-key"     nil '(:r1 :r0)             '(:r1 :r0 :r2 :r1)))
  (expected pairs-a pairs-b)
  (let ((a (apply #'%make-copy-env pairs-a))
        (b (apply #'%make-copy-env pairs-b)))
    (assert-equal expected (cl-cc/optimize::%opt-copy-prop-env-equal-p a b))))

;;; ── %opt-copy-prop-canonical ────────────────────────────────────────────────

(deftest-each copyprop-canonical
  "canonical chases the copy chain to its ultimate source."
  :cases (("absent"       :r0  :r0  '())
          ("self-absent"  :r1  :r1  '())
          ("direct"       :r0  :r1  '(:r1 :r0))
          ("two-hop"      :r0  :r2  '(:r2 :r1 :r1 :r0))
          ("three-hop"    :r0  :r3  '(:r3 :r2 :r2 :r1 :r1 :r0)))
  (expected reg pairs)
  (let ((env (apply #'%make-copy-env pairs)))
    (assert-eq expected (cl-cc/optimize::%opt-copy-prop-canonical reg env))))

(deftest-each copyprop-canonical-termination-cases
  "Canonical terminates on self-loops and mutual copy cycles."
  :cases (("self-loop" '(:r0 :r0)        (lambda (r) (eq r :r0)))
          ("cycle"     '(:r0 :r1 :r1 :r0) (lambda (r) (or (eq r :r0) (eq r :r1)))))
  (pairs pred)
  (let* ((env    (apply #'%make-copy-env pairs))
         (result (cl-cc/optimize::%opt-copy-prop-canonical :r0 env)))
    (assert-true (funcall pred result))))

;;; ── %opt-copy-prop-add / %opt-copy-prop-kill ────────────────────────────────

(deftest copyprop-add-registers-copy
  "add records dst → src in the copies table."
  (let* ((copies  (make-hash-table :test #'eq))
         (reverse (make-hash-table :test #'eq)))
    (cl-cc/optimize::%opt-copy-prop-add :r1 :r0 copies reverse)
    (assert-eq :r0 (gethash :r1 copies))
    (assert-true (member :r1 (gethash :r0 reverse)))))

(deftest-each copyprop-kill-cases
  "kill source removes all dependents; kill destination removes only that fact."
  :cases (("kill-source" :r0 '(:r1 :r2) nil)
          ("kill-dst"    :r1 '(:r1)     :r0))
  (kill-reg removed-regs surviving-r2-val)
  (let* ((copies  (%make-copy-env :r1 :r0 :r2 :r0))
         (reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies)))
    (cl-cc/optimize::%opt-copy-prop-kill kill-reg copies reverse)
    (dolist (r removed-regs)
      (assert-false (gethash r copies)))
    (when surviving-r2-val
      (assert-eq surviving-r2-val (gethash :r2 copies)))))

;;; ── %opt-copy-prop-merge ────────────────────────────────────────────────────

(deftest-each copyprop-merge-empty-result-cases
  "Merging an empty list or two disjoint environments both yield an empty environment."
  :cases (("empty-input" nil)
          ("disjoint"    (list (%make-copy-env :r1 :r0) (%make-copy-env :r3 :r2))))
  (envs)
  (assert-equal 0 (hash-table-count (cl-cc/optimize::%opt-copy-prop-merge envs))))

(deftest copyprop-merge-single
  "Merging a single-element list copies that environment."
  (let* ((env    (%make-copy-env :r1 :r0 :r2 :r0))
         (result (cl-cc/optimize::%opt-copy-prop-merge (list env))))
    (assert-true (cl-cc/optimize::%opt-copy-prop-env-equal-p env result))))

(deftest-each copyprop-merge-disagreement-cases
  "Merge drops bindings where environments disagree; 2-way and 3-way."
  :cases (("two-way"   (list (%make-copy-env :r1 :r0 :r2 :r0)
                             (%make-copy-env :r1 :r0 :r2 :r3)))
          ("three-way" (list (%make-copy-env :r1 :r0 :r2 :r0)
                             (%make-copy-env :r1 :r0 :r2 :r3)
                             (%make-copy-env :r1 :r0))))
  (envs)
  (let ((result (cl-cc/optimize::%opt-copy-prop-merge envs)))
    (assert-eq    :r0   (gethash :r1 result))
    (assert-false (gethash :r2 result))))

;;; ── %opt-value-rank / *opt-type-rank-table* ─────────────────────────────────

(deftest-each copyprop-value-rank-types
  "%opt-value-rank assigns the correct rank from *opt-type-rank-table* for each type."
  :cases (("null"      0 nil)
          ("number"    1 42)
          ("character" 2 #\a)
          ("string"    3 "hello")
          ("symbol"    4 'foo)
          ("cons"      5 '(a b))
          ("vector"    6 #(1 2 3))
          ("unknown"   7 #'identity))
  (expected value)
  (assert-= expected (cl-cc/optimize::%opt-value-rank value)))

;;; ── %opt-value< ─────────────────────────────────────────────────────────────

(deftest-each copyprop-value<-cross-type
  "%opt-value< orders nil < number < character < string < symbol."
  :cases (("nil<num"    t   nil  1)
          ("num<char"   t   1    #\a)
          ("char<str"   t   #\a  "z")
          ("str<sym"    t   "z"  :a)
          ("irrefl-nil" nil nil  nil)
          ("irrefl-num" nil 5    5))
  (expected a b)
  (assert-equal expected (cl-cc/optimize::%opt-value< a b)))

(deftest-each copyprop-value<-same-type
  "%opt-value< within a type follows natural ordering.
%opt-value< returns a truthy value for less-than — for strings this may be
the mismatch index from cl:string< (an integer ≥ 0), not necessarily T.
Use `not null` semantics to match both numeric booleans and string indices."
  :cases (("num-less"    t   3    5)
          ("num-greater" nil 5    3)
          ("str-less"    t   "ab" "b")
          ("str-equal"   nil "ab" "ab")
          ("char-less"   t   #\a  #\z))
  (expected a b)
  (assert-equal expected (not (null (cl-cc/optimize::%opt-value< a b)))))

;;; ── opt-pass-copy-prop integration ──────────────────────────────────────────

(defun %copyprop-find (instrs type-name)
  "Return the first instruction of TYPE-NAME in INSTRS, or NIL."
  (let ((type (find-symbol (symbol-name type-name) :cl-cc)))
    (find-if (lambda (i) (typep i type)) instrs)))

(deftest copyprop-pass-empty-input
  "opt-pass-copy-prop on an empty list returns nil."
  (assert-equal nil (cl-cc/optimize::opt-pass-copy-prop nil)))

(deftest copyprop-pass-no-moves-unchanged
  "Instructions with no vm-move keep their original register operands."
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-const :dst :r1 :value 2)
                       (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (assert-true add)
    (assert-eq :r0 (vm-lhs add))
    (assert-eq :r1 (vm-rhs add))))

(deftest copyprop-pass-basic-rewrite
  "A vm-move r1←r0 causes subsequent uses of :r1 to be rewritten to :r0."
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 42)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (assert-true add)
    (assert-eq :r0 (vm-lhs add))
    (assert-eq :r0 (vm-rhs add))))

(deftest copyprop-pass-chain-rewrite
  "Chained moves r0→r1→r2 cause uses of :r2 to be rewritten all the way back to :r0."
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 7)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-move  :dst :r2 :src :r1)
                       (make-vm-add   :dst :r3 :lhs :r2 :rhs :r0)
                       (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (assert-true add)
    (assert-eq :r0 (vm-lhs add))))

(deftest copyprop-pass-kill-stops-propagation
  "Overwriting a copy register kills the copy fact; subsequent uses of that register are not rewritten."
  (let* ((instrs (list (make-vm-label :name "entry")
                       (make-vm-const :dst :r0 :value 1)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-const :dst :r1 :value 99)
                       (make-vm-add   :dst :r2 :lhs :r1 :rhs :r1)
                       (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add    (%copyprop-find result 'vm-add)))
    (assert-true add)
    (assert-eq :r1 (vm-lhs add))
    (assert-eq :r1 (vm-rhs add))))

(deftest copyprop-pass-preserves-labels
  "opt-pass-copy-prop preserves all vm-label instructions in the output."
  (let* ((instrs (list (make-vm-label :name "start")
                       (make-vm-const :dst :r0 :value 0)
                       (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-copy-prop instrs))
         (labels (loop for i in result
                       when (typep i 'cl-cc/vm::vm-label)
                       collect (cl-cc::vm-lbl-name i))))
    (assert-true (member "start" labels :test #'equal))))

;;; ── copyprop-pass-state helpers ─────────────────────────────────────────────

(deftest copyprop-enqueue-is-idempotent
  "%copyprop-enqueue adds a block at most once; re-enqueueing the same block is a no-op."
  (let ((state (cl-cc/optimize::make-copyprop-pass-state))
        (blk   (cl-cc/optimize::cfg-new-block (cl-cc/optimize::make-cfg))))
    (cl-cc/optimize::%copyprop-enqueue blk state)
    (assert-= 1 (length (cl-cc/optimize::cpps-worklist state)))
    (cl-cc/optimize::%copyprop-enqueue blk state)
    (assert-= 1 (length (cl-cc/optimize::cpps-worklist state)))))

(deftest copyprop-enqueue-two-distinct-blocks
  "Enqueueing two distinct blocks results in a worklist of length 2; re-enqueueing one is still idempotent."
  (let ((state (cl-cc/optimize::make-copyprop-pass-state))
        (b1    (cl-cc/optimize::cfg-new-block (cl-cc/optimize::make-cfg)))
        (b2    (cl-cc/optimize::cfg-new-block (cl-cc/optimize::make-cfg))))
    (cl-cc/optimize::%copyprop-enqueue b1 state)
    (cl-cc/optimize::%copyprop-enqueue b2 state)
    (assert-= 2 (length (cl-cc/optimize::cpps-worklist state)))
    (cl-cc/optimize::%copyprop-enqueue b1 state)
    (assert-= 2 (length (cl-cc/optimize::cpps-worklist state)))))

(deftest copyprop-process-block-propagates-copy-to-successor
  "%copyprop-process-block enqueues successors when out-env changes."
  (let* ((cfg   (cl-cc/optimize::cfg-build
                 (list (make-vm-const :dst :r0 :value 1)
                       (make-vm-move  :dst :r1 :src :r0)
                       (make-vm-jump  :label "end")
                       (make-vm-label :name "end")
                       (make-vm-ret   :reg :r1))))
         (entry (cl-cc/optimize::cfg-entry cfg))
         (state (cl-cc/optimize::make-copyprop-pass-state)))
    (cl-cc/optimize::%copyprop-process-block entry state)
    (let ((out (gethash entry (cl-cc/optimize::cpps-out-envs state))))
      (assert-true (hash-table-p out)))))

;;; ── %opt-copy-prop-build-reverse ─────────────────────────────────────────

(deftest copyprop-build-reverse-builds-reverse-map
  "%opt-copy-prop-build-reverse builds a src→list-of-dsts reverse index."
  (let ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r1 copies) :r0
          (gethash :r2 copies) :r0)
    (let ((rev (cl-cc/optimize::%opt-copy-prop-build-reverse copies)))
      (assert-true (member :r1 (gethash :r0 rev) :test #'eq))
      (assert-true (member :r2 (gethash :r0 rev) :test #'eq)))))

(deftest copyprop-build-reverse-empty-input
  "%opt-copy-prop-build-reverse on an empty copies table yields an empty reverse table."
  (let ((rev (cl-cc/optimize::%opt-copy-prop-build-reverse
              (make-hash-table :test #'eq))))
    (assert-= 0 (hash-table-count rev))))

;;; ── %opt-copy-prop-transfer-block ────────────────────────────────────────

(deftest copyprop-transfer-block-records-move
  "%opt-copy-prop-transfer-block records a vm-move as a copy fact in the out-env."
  (let* ((blk    (make-instance 'cl-cc/optimize::basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (cl-cc/optimize::bb-instructions blk)
          (list (make-vm-move :dst :r1 :src :r0)))
    (let ((out (cl-cc/optimize::%opt-copy-prop-transfer-block blk in-env)))
      (assert-eq :r0 (gethash :r1 out)))))

(deftest copyprop-transfer-block-kills-overwritten
  "%opt-copy-prop-transfer-block kills the copy fact when the destination register is overwritten."
  (let* ((blk    (make-instance 'cl-cc/optimize::basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (gethash :r1 in-env) :r0)
    (setf (cl-cc/optimize::bb-instructions blk)
          (list (make-vm-const :dst :r1 :value 99)))
    (let ((out (cl-cc/optimize::%opt-copy-prop-transfer-block blk in-env)))
      (assert-null (gethash :r1 out)))))

;;; ── %opt-copy-prop-rewrite-inst ─────────────────────────────────────────

(deftest copyprop-rewrite-inst-substitutes-copies
  "%opt-copy-prop-rewrite-inst rewrites operand registers to their canonical copies."
  (let ((copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r5)
    (let* ((inst   (make-vm-add :dst :r2 :lhs :r0 :rhs :r0))
           (result (cl-cc/optimize::%opt-copy-prop-rewrite-inst inst copies)))
      (assert-eq :r5 (cl-cc/vm::vm-lhs result))
      (assert-eq :r5 (cl-cc/vm::vm-rhs result)))))

(deftest copyprop-rewrite-inst-identity-when-no-copy
  "%opt-copy-prop-rewrite-inst returns the same instruction object when no copy facts apply."
  (let ((copies (make-hash-table :test #'eq))
        (inst   (make-vm-const :dst :r0 :value 1)))
    (assert-eq inst (cl-cc/optimize::%opt-copy-prop-rewrite-inst inst copies))))

;;; ── %opt-copy-prop-rewrite-block ────────────────────────────────────────

(deftest copy-prop-rewrite-block-rewrites-instructions
  "%opt-copy-prop-rewrite-block rewrites instructions using in-env copies."
  (let* ((blk    (make-instance 'cl-cc/optimize::basic-block))
         (in-env (make-hash-table :test #'eq)))
    (setf (gethash :r0 in-env) :r5)
    (setf (cl-cc/optimize::bb-instructions blk)
          (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r0)))
    (let ((result (cl-cc/optimize::%opt-copy-prop-rewrite-block blk in-env)))
      (assert-= 1 (length result))
      (assert-eq :r5 (cl-cc/vm::vm-lhs (first result))))))
