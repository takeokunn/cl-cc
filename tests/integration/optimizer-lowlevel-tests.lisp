(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── Direct opt-pass-fold Tests ─────────────────────────────────────────

(deftest fold-label-handling-cases
  "Branch-target labels flush constant env; fallthrough labels preserve constant knowledge."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-jump :label "join")
                       (cl-cc::make-vm-label :name "join")
                        (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs)))
    (assert-true (find-if (lambda (i) (typep i 'cl-cc/vm::vm-inc)) out)))
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal 43 (cl-cc/vm::vm-value r1-const))))

(deftest-each fold-type-pred
  "Type predicate and vm-not instructions fold at compile time against a known constant."
  :cases (("number-p" 42  (cl-cc::make-vm-number-p :dst :R1 :src :R0) 1)
          ("symbol-p" 42  (cl-cc::make-vm-symbol-p :dst :R1 :src :R0) 0)
          ("not-nil"  nil (cl-cc::make-vm-not       :dst :R1 :src :R0) t)
          ("not-zero" 0   (cl-cc::make-vm-not       :dst :R1 :src :R0) nil))
  (const-val pred-inst expected)
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value const-val) pred-inst))
         (out (cl-cc/optimize::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc/vm::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal expected (cl-cc/vm::vm-value r1-const))))

(deftest-each fold-branch-known
  "vm-jump-zero with a known constant is eliminated or replaced by an unconditional jump."
  :cases (("known-true-no-branch"
           1
           (lambda (out)
             (assert-false (find-if #'cl-cc::vm-jump-p out))))
          ("known-false-jump"
           nil
           (lambda (out)
             (assert-true (find-if #'cl-cc::vm-jump-p out)))))
  (const-val verify)
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value const-val)
                       (cl-cc::make-vm-jump-zero :reg :R0 :label "target")))
          (out (cl-cc/optimize::opt-pass-fold instrs)))
    (funcall verify out)
    (assert-false (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))))

;;; ── Direct opt-pass-copy-prop Tests ────────────────────────────────────

(deftest-each copy-prop-operand-resolution
  "opt-pass-copy-prop resolves add operands through copy chains, fallthrough labels, and kills."
  :cases (("chain-follows-two-moves"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-move :dst :R2 :src :R1)
                 (cl-cc::make-vm-add :dst :R3 :lhs :R2 :rhs :R2))
           :R0)
          ("fallthrough-label-preserves-copies"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-label :name "join")
                 (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1))
           :R0)
          ("overwrite-kills-alias"
           (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                 (cl-cc::make-vm-const :dst :R0 :value 99)
                 (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1))
           :R1))
  (instrs expected-lhs)
  (let* ((out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-true add-inst)
    (assert-equal expected-lhs (cl-cc/vm::vm-lhs add-inst))))

(deftest copy-prop-kill-and-elim-cases
  "Reverse-map kill invalidates direct aliases; self-move after resolution is eliminated."
  (let* ((copies (make-hash-table :test #'eq))
         (reverse nil))
    (setf (gethash :R1 copies) :R0)
    (setf (gethash :R2 copies) :R0)
    (setf (gethash :R3 copies) :R2)
    (setf reverse (cl-cc/optimize::%opt-copy-prop-build-reverse copies))
    (cl-cc/optimize::%opt-copy-prop-kill :R0 copies reverse)
    (assert-false (gethash :R1 copies))
    (assert-false (gethash :R2 copies))
    (assert-true (eql :R2 (gethash :R3 copies))))
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-move :dst :R0 :src :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move)) out)))
    (assert-equal 1 (length moves))))

(deftest copy-prop-join-point
  "Copies that agree on every predecessor survive a CFG join and rewrite uses."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R9 :value nil)
                       (cl-cc::make-vm-jump-zero :reg :R9 :label "else")
                       (cl-cc::make-vm-label :name "then")
                       (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-jump :label "join")
                       (cl-cc::make-vm-label :name "else")
                       (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc/optimize::opt-pass-copy-prop instrs)))
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R0 (cl-cc/vm::vm-lhs add-inst))
      (assert-equal :R0 (cl-cc/vm::vm-rhs add-inst)))))

(deftest heap-alias-cases
  "Must-alias propagates through move; distinct allocs not may-alias; unknown root conservatively may-alias."
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize::opt-compute-heap-aliases (list alloc copy))))
    (assert-true  (cl-cc/optimize::opt-must-alias-p :r0 :r3 roots))
    (assert-false (cl-cc/optimize::opt-must-alias-p :r0 :r9 roots)))
  (let* ((alloc-a (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-b (make-vm-make-array :dst :r4 :size-reg :r5))
         (roots   (cl-cc/optimize::opt-compute-heap-aliases (list alloc-a alloc-b))))
    (assert-false (cl-cc/optimize::opt-may-alias-p :r0 :r4 roots))
    (assert-true  (cl-cc/optimize::opt-may-alias-p :r0 :r9 roots))))

(deftest points-to-helper-tracks-moves-and-kills
  "Flow-sensitive points-to propagates through vm-move and is killed by overwrite."
  (let* ((alloc (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (copy  (make-vm-move :dst :r3 :src :r0))
         (kill  (make-vm-const :dst :r3 :value 9))
         (pt1   (cl-cc/optimize::opt-compute-points-to (list alloc copy)))
         (pt2   (cl-cc/optimize::opt-compute-points-to (list alloc copy kill))))
    (assert-eq :r0 (cl-cc/optimize::opt-points-to-root :r0 pt1))
    (assert-eq :r0 (cl-cc/optimize::opt-points-to-root :r3 pt1))
    (assert-false (cl-cc/optimize::opt-points-to-root :r3 pt2))))

(deftest heap-kind-helper-distinguishes-object-classes
  "TBAA helper can prove non-aliasing across different fresh heap object kinds."
  (let* ((alloc-cons  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (alloc-array (make-vm-make-array :dst :r4 :size-reg :r5))
         (points-to   (cl-cc/optimize::opt-compute-points-to (list alloc-cons alloc-array)))
         (heap-kinds  (cl-cc/optimize::opt-compute-heap-kinds (list alloc-cons alloc-array))))
    (assert-eq :cons (gethash :r0 heap-kinds))
    (assert-eq :array (gethash :r4 heap-kinds))
    (assert-false (cl-cc/optimize::opt-may-alias-by-type-p :r0 :r4 points-to heap-kinds))
    (assert-true  (cl-cc/optimize::opt-may-alias-by-type-p :r0 :r9 points-to heap-kinds))))

(deftest constant-interval-helper-propagates-basic-arithmetic
  "Constant interval propagation handles add/sub/mul in straight-line code."
  (let* ((c1 (make-vm-const :dst :r0 :value 3))
         (c2 (make-vm-const :dst :r1 :value 5))
         (a  (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (s  (make-vm-sub :dst :r3 :lhs :r2 :rhs :r0))
         (m  (make-vm-mul :dst :r4 :lhs :r3 :rhs :r1))
         (intervals (cl-cc/optimize::opt-compute-constant-intervals (list c1 c2 a s m))))
    (assert-equal '(8 . 8) (gethash :r2 intervals))
    (assert-equal '(5 . 5) (gethash :r3 intervals))
    (assert-equal '(25 . 25) (gethash :r4 intervals))))

;;; ─── opt-inst-read-regs ──────────────────────────────────────────────────────

(deftest-each opt-inst-read-regs-cases
  "opt-inst-read-regs returns the correct source register list for each instruction type."
  :cases (("const"      (make-vm-const      :dst :r0 :value 42)             '())
          ("func-ref"   (make-vm-func-ref   :dst :r0 :label "fn")           '())
          ("get-global" (make-vm-get-global :dst :r0 :name 'x)              '())
          ("move"       (make-vm-move       :dst :r0 :src :r1)              '(:r1))
          ("neg"        (make-vm-neg        :dst :r0 :src :r1)              '(:r1))
          ("null-p"     (make-vm-null-p     :dst :r0 :src :r1)              '(:r1))
          ("ret"        (make-vm-ret        :reg :r0)                        '(:r0))
          ("set-global" (make-vm-set-global :src :r0 :name 'x)              '(:r0))
          ("add"        (make-vm-add        :dst :r0 :lhs :r1 :rhs :r2)    '(:r1 :r2))
          ("lt"         (make-vm-lt         :dst :r0 :lhs :r1 :rhs :r2)    '(:r1 :r2))
          ("call"       (make-vm-call       :dst :r0 :func :r1 :args '(:r2 :r3)) '(:r1 :r2 :r3)))
  (inst expected-members)
  (let ((regs (cl-cc/optimize::opt-inst-read-regs inst)))
    (assert-equal (length expected-members) (length regs))
    (dolist (r expected-members)
      (assert-true (member r regs)))))

;;; ── opt-pass-dce: Dead Code Elimination ──────────────────────────────────

(deftest-each dce-pass-cases
  "opt-pass-dce: removes dead consts/moves, keeps used consts and impure calls."
  :cases (("removes-unused-const"
           (lambda ()
             (let* ((i1  (make-vm-const :dst :r0 :value 42))
                    (i2  (make-vm-const :dst :r1 :value 7))
                    (ret (make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) (list i1) (list i2 ret)))))
          ("keeps-const-read-by-add"
           (lambda ()
             (let* ((i1  (make-vm-const :dst :r0 :value 5))
                    (i2  (make-vm-add  :dst :r1 :lhs :r0 :rhs :r0))
                    (ret (make-vm-ret :reg :r1)))
               (list (list i1 i2 ret) nil (list i1 i2)))))
          ("removes-unused-move"
           (lambda ()
             (let* ((c   (make-vm-const :dst :r0 :value 1))
                    (m   (make-vm-move  :dst :r2 :src :r0))
                    (ret (make-vm-ret :reg :r0)))
               (list (list c m ret) (list m) (list c)))))
          ("preserves-impure-call"
           (lambda ()
             (let* ((fn  (make-vm-const :dst :r0 :value 'f))
                    (c   (make-vm-call  :dst :r1 :func :r0 :args nil))
                    (ret (make-vm-ret :reg :r0)))
               (list (list fn c ret) nil (list c))))))
  (make-case)
  (destructuring-bind (instrs absent present) (funcall make-case)
    (let ((out (cl-cc/optimize::opt-pass-dce instrs)))
      (dolist (inst absent)  (assert-false (member inst out)))
      (dolist (inst present) (assert-true  (member inst out))))))

;;; ── opt-pass-jump: Jump Threading ────────────────────────────────────────

(deftest jump-threading-cases
  "opt-pass-jump removes falls-through jump; threads jump-to-jump chain to final target."
  (let* ((j   (make-vm-jump  :label "end"))
         (lbl (make-vm-label :name "end"))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list j lbl ret))))
    (assert-false (member j out))
    (assert-true  (member lbl out))
    (assert-true  (member ret out)))
  (let* ((j1   (make-vm-jump  :label "mid"))
         (lbl1 (make-vm-label :name "mid"))
         (j2   (make-vm-jump  :label "end"))
         (lbl2 (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-jump (list j1 lbl1 j2 lbl2 ret))))
    (let ((j1-out (find-if #'cl-cc::vm-jump-p out)))
      (when j1-out
        (assert-equal (cl-cc/vm::vm-label-name j1-out) "end")))))

(deftest jump-threading-follows-long-acyclic-chain
  "opt-pass-jump fully threads long jump chains that exceed the old recursion cap."
  (let ((instructions nil))
    (loop for i from 0 below 25 do
      (push (make-vm-jump :label (format nil "L~D" (1+ i))) instructions)
      (push (make-vm-label :name (format nil "L~D" (1+ i))) instructions))
    (push (make-vm-ret :reg :r0) instructions)
    (let* ((out (cl-cc/optimize::opt-pass-jump (nreverse instructions)))
           (j0  (find-if #'cl-cc::vm-jump-p out)))
      (assert-true j0)
      (assert-equal (cl-cc/vm::vm-label-name j0) "L25"))))

(deftest jump-zero-threading-updates-label
  "opt-pass-jump threads vm-jump-zero target through a chain."
  (let* ((c   (make-vm-const     :dst :r0 :value 0))
         (jz  (make-vm-jump-zero :reg :r0 :label "mid"))
         (lbl (make-vm-label     :name "mid"))
         (j2  (make-vm-jump      :label "end"))
         (end (make-vm-label     :name "end"))
         (ret (make-vm-ret       :reg :r0))
         (out (cl-cc/optimize::opt-pass-jump (list c jz lbl j2 end ret))))
    ;; The jump-zero should now target "end" directly
    (let ((jz-out (find-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out)))
      (when jz-out
        (assert-equal (cl-cc/vm::vm-label-name jz-out) "end")))))

;;; CFG reachability / label cleanup tests moved to optimizer-tests.lisp.
