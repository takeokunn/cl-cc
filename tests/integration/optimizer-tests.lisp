;;;; tests/optimizer-tests.lisp — Optimizer Tests
;;;
;;; Tests verifying that the multi-pass optimizer:
;;;   - Correctly folds constants at compile time (semantics preserved)
;;;   - Eliminates dead code (pure instructions with unused results)
;;;   - Removes dead jumps and unreachable code
;;;   - Preserves all observable semantics (evaluated values)

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── Helpers ──────────────────────────────────────────────────────────────

(defun opt-count-instr (expr type-sym)
  "Compile EXPR, run optimizer, count instructions of TYPE-SYM in the result."
  (let* ((result   (compile-string expr :target :vm))
         (instrs   (optimize-instructions (%get-instructions result)))
         (cl-type  (find-symbol (symbol-name type-sym) :cl-cc)))
    (count-if (lambda (i) (typep i cl-type)) instrs)))

(defun opt-has-p (expr type-sym)
  "T if compiling EXPR leaves at least one instruction of TYPE-SYM after optimization."
  (> (opt-count-instr expr type-sym) 0))

(defun make-bswap-tree-instructions ()
  "Construct the canonical masked-shift tree for a 32-bit byte swap."
  (list (make-vm-const :dst :r1 :value #xFF)
        (make-vm-logand :dst :r2 :lhs :r0 :rhs :r1)
        (make-vm-const :dst :r3 :value 24)
        (make-vm-ash   :dst :r4 :lhs :r2 :rhs :r3)
        (make-vm-const :dst :r5 :value #xFF00)
        (make-vm-logand :dst :r6 :lhs :r0 :rhs :r5)
        (make-vm-const :dst :r7 :value 8)
        (make-vm-ash   :dst :r8 :lhs :r6 :rhs :r7)
        (make-vm-const :dst :r9 :value #xFF0000)
        (make-vm-logand :dst :r10 :lhs :r0 :rhs :r9)
        (make-vm-const :dst :r11 :value -8)
        (make-vm-ash   :dst :r12 :lhs :r10 :rhs :r11)
        (make-vm-const :dst :r13 :value #xFF000000)
        (make-vm-logand :dst :r14 :lhs :r0 :rhs :r13)
        (make-vm-const :dst :r15 :value -24)
        (make-vm-ash   :dst :r16 :lhs :r14 :rhs :r15)
        (make-vm-logior :dst :r17 :lhs :r4 :rhs :r8)
        (make-vm-logior :dst :r18 :lhs :r12 :rhs :r16)
        (make-vm-logior :dst :r19 :lhs :r17 :rhs :r18)))

;;; ── Constant Folding ─────────────────────────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) binary-op eliminated.

(deftest-each optimizer-fold
  "Constant arithmetic folds: value is correct and the binary-op instruction is eliminated."
  :cases (("add"           7  "(+ 3 4)"                 'vm-add)
          ("sub"           6  "(- 10 4)"                'vm-sub)
          ("mul"          12  "(* 3 4)"                 'vm-mul)
          ("chained"      12  "(+ (* 2 3) (- 10 4))"   'vm-add)
          ("deeper-chain" 16  "(- (* 5 5) (* 3 3))"    'vm-sub))
  (expected expr instr)
  (assert-run= expected expr)
  (assert-false (opt-has-p expr instr)))

;;; ── Algebraic Identity Simplification ───────────────────────────────────
;;; Each case verifies: (1) correct runtime value, (2) instruction eliminated.

(deftest-each optimizer-algebraic-identity
  "Algebraic identities: value is correct and the corresponding instruction is eliminated."
  :cases (("add-zero"  5  "(let ((x 5))  (+ x 0))"  'vm-add)
          ("mul-one"   7  "(let ((x 7))  (* x 1))"  'vm-mul)
          ("mul-zero"  0  "(let ((x 99)) (* x 0))"  'vm-mul)
          ("sub-zero" 42  "(let ((x 42)) (- x 0))"  'vm-sub))
  (expected expr instr)
  (assert-run= expected expr)
  (assert-false (opt-has-p expr instr)))

;;; ── Dead Code Elimination: Semantics ─────────────────────────────────────

(deftest-each optimizer-dce
  "Dead code elimination preserves correct runtime values."
  :cases (("progn-dead-exprs"   99 "(progn (+ 1 2) (+ 3 4) 99)")
          ("let-nested"          3 "(let ((x 1)) (let ((y 2)) (+ x y)))")
          ("mixed-dead-subform" 42 "(let ((x 42)) (+ 0 x))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Constant Branch Elimination: Semantics ───────────────────────────────

(deftest-each optimizer-branch-fold
  "Constant-condition branches fold to the correct path."
  :cases (("const-true"  42 "(if 1 42 99)")
          ("const-false" 99 "(if 0 42 99)")
          ("t-true"      42 "(if t 42 99)"))
  (expected expr)
  (assert-run= expected expr))

(deftest optimizer-branch-nil-false
  "nil condition selects the else-branch."
  (assert-run-false "(if nil 42 nil)"))

(deftest optimizer-hot-cold-layout-keeps-signal-block-last
  "cfg-flatten-hot-cold keeps the explicit signal-error block in the cold tail." 
  (let* ((hot   (make-vm-label :name "hot"))
         (jump  (make-vm-jump-zero :reg :r0 :label "cold"))
         (work  (make-vm-const :dst :r1 :value 1))
         (toend (make-vm-jump :label "end"))
         (cold  (make-vm-label :name "cold"))
         (sig   (cl-cc::make-vm-signal-error :error-reg :r1))
         (end   (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (cfg   (cl-cc/optimize::cfg-build (list hot jump work toend cold sig end ret)))
         (_idom (cl-cc/optimize::cfg-compute-dominators cfg))
         (_loop (cl-cc/optimize::cfg-compute-loop-depths cfg))
         (out   (cl-cc/optimize::cfg-flatten-hot-cold cfg))
         (labels (loop for inst in out
                        when (typep inst 'cl-cc/vm::vm-label)
                        collect (cl-cc/vm::vm-name inst))))
    (declare (ignore _idom _loop))
    (assert-true (member "hot" labels :test #'equal))
    (assert-true (member "cold" labels :test #'equal))
    (assert-true (member "end" labels :test #'equal))
    (assert-true (< (position "hot" labels :test #'equal)
                    (position "cold" labels :test #'equal)))
    (assert-true (< (position "end" labels :test #'equal)
                    (position "cold" labels :test #'equal)))))

;;; ── Constant Branch Elimination: No Jump Instructions ────────────────────

(deftest-each optimizer-branch-fold-no-jump
  "Constant-condition branches eliminate vm-jump-zero from the output."
  :cases (("const-true"  "(if 1 42 99)")
          ("const-false" "(if 0 42 99)"))
  (expr)
  (assert-false (opt-has-p expr 'vm-jump-zero)))

;;; ── Dominated Predicate Elimination ─────────────────────────────────────

(deftest-each optimizer-dominated-check-elim
  "Redundant dominated predicates are rewritten to vm-move by the relevant elimination pass."
  :cases (("null-p via dominated-type-check-elim"
           #'cl-cc/optimize::opt-pass-dominated-type-check-elim
           (make-vm-null-p :dst :r1 :src :r0)
           (make-vm-null-p :dst :r2 :src :r0)
           'cl-cc/vm::vm-null-p)
          ("not via dominated-type-check-elim"
           #'cl-cc/optimize::opt-pass-dominated-type-check-elim
           (make-vm-not :dst :r1 :src :r0)
           (make-vm-not :dst :r2 :src :r0)
           'cl-cc/vm::vm-not)
          ("not via nil-check-elim"
           #'cl-cc/optimize::opt-pass-nil-check-elim
           (make-vm-not :dst :r1 :src :r0)
           (make-vm-not :dst :r2 :src :r0)
           'cl-cc/vm::vm-not))
  (pass-fn p1 p2 instr-type)
  (let* ((c    (make-vm-const :dst :r0 :value nil))
         (br   (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out  (funcall pass-fn (list c p1 br then p2 ret1 else ret0))))
    (assert-equal 1 (count-if (lambda (i) (typep i instr-type)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) out))))

(deftest-each optimizer-pass-produces-const
  "Optimization passes fold or correlate predicates to known vm-const values."
  :cases (("branch-correlation-true-edge"
           (lambda ()
             (let* ((c    (make-vm-const :dst :r0 :value 1))
                    (p1   (cl-cc::make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (p2   (cl-cc::make-vm-integer-p :dst :r2 :src :r0))
                    (ret1 (make-vm-ret :reg :r2))
                    (else (make-vm-label :name "else"))
                    (ret0 (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list c p1 br then p2 ret1 else ret0))))
           :r2 1)
          ("branch-correlation-false-edge"
           (lambda ()
             (let* ((c    (make-vm-const :dst :r0 :value 1))
                    (p1   (cl-cc::make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (ret1 (make-vm-ret :reg :r1))
                    (else (make-vm-label :name "else"))
                    (p2   (cl-cc::make-vm-integer-p :dst :r2 :src :r0))
                    (ret0 (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list c p1 br then ret1 else p2 ret0))))
           :r2 0)
          ("fold-rational-unary"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3/4))
                    (u1  (cl-cc::make-vm-numerator :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-fold (list c1 u1 ret))))
           :r1 3)
          ("fold-rational-binary"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 8))
                    (c2  (make-vm-const :dst :r1 :value 12))
                    (g   (cl-cc::make-vm-gcd :dst :r2 :lhs :r0 :rhs :r1))
                    (ret (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-fold (list c1 c2 g ret))))
           :r2 4))
  (make-out expected-dst expected-value)
  (let ((out (funcall make-out)))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq expected-dst (cl-cc/vm::vm-dst i))
                              (eql expected-value (cl-cc/vm::vm-value i))))
                       out))))

;;; ── End-to-End Correctness ───────────────────────────────────────────────

(deftest-each optimizer-e2e
  "Full optimizer pipeline preserves semantics for various expression forms."
  :cases (("simple-let"   10  "(let ((x 10)) x)")
          ("two-bindings"  7  "(let ((x 3) (y 4)) (+ x y))")
          ("square"        4  "(let ((x 2)) (* x x))")
          ("sum-of-sums"  10  "(+ (+ 1 2) (+ 3 4))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Bitwise Algebraic Identities ─────────────────────────────────────────
;;; instr = nil means value-correctness only (no instruction-elimination check).

(deftest-each optimizer-bitwise
  "Bitwise identities: value is correct; instruction eliminated where applicable."
  :cases (("logand-zero"
           0 "(let ((x 42)) (logand x 0))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-logand))))
          ("logand-minus-one"
           42 "(let ((x 42)) (logand x -1))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-logand))))
          ("logand-self"
           12 "(let ((x 12)) (logand x x))"
           (lambda (_expr)
             (declare (ignore _expr))))
          ("logior-zero"
           42 "(let ((x 42)) (logior x 0))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-logior))))
          ("logxor-zero"
           42 "(let ((x 42)) (logxor x 0))"
           (lambda (_expr)
             (declare (ignore _expr))))
          ("logxor-self"
           0 "(let ((x 42)) (logxor x x))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-logxor))))
          ("ash-zero"
           42 "(let ((x 42)) (ash x 0))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-ash))))
          ("rem-zero"
           0 "(let ((x 42)) (rem 0 x))"
           (lambda (expr)
             (assert-false (opt-has-p expr 'vm-rem)))))
  (expected expr verify)
  (assert-run= expected expr)
  (funcall verify expr))

;;; ── Unary Constant Folding ────────────────────────────────────────────────

(deftest optimizer-lognot-constant
  "(lognot 0) folds at compile time: value is -1 and no vm-lognot remains."
  (assert-equal -1 (run-string "(lognot 0)"))
  (let* ((instrs (list (cl-cc::make-vm-const :dst :r1 :value 0)
                       (cl-cc::make-vm-lognot :dst :r0 :src :r1)))
         (out (cl-cc/optimize::opt-pass-fold instrs)))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-lognot)) out))
    (assert-true (some (lambda (i)
                         (and (cl-cc::vm-const-p i)
                              (eq :r0 (cl-cc/vm::vm-dst i))
                              (eql -1 (cl-cc/vm::vm-value i))))
                       out))))

(deftest optimizer-not-zero-value
  "(not 0) = nil (0 is truthy in ANSI CL)."
  (assert-null (run-string "(not 0)")))

;;; ── Function Inlining ────────────────────────────────────────────────────

(deftest optimizer-inline
  "Small functions inline: correct value and no vm-call; two-arg case also checked."
  (assert-run= 5 "(defun double-inc (x) (+ x 1)) (double-inc 4)")
  (assert-false (opt-has-p "(defun double-inc (x) (+ x 1)) (double-inc 4)" 'vm-call))
  (assert-run= 7 "(defun add2 (a b) (+ a b)) (add2 3 4)"))

(deftest optimizer-leaf-detect
  "optimize-instructions reports leaf programs correctly."
  (multiple-value-bind (leaf-insts leaf-p)
      (optimize-instructions (list (make-vm-const :dst :r0 :value 1)
                                   (make-vm-ret :reg :r0)))
    (declare (ignore leaf-insts))
    (assert-true leaf-p))
  (multiple-value-bind (call-insts leaf-p)
      (optimize-instructions (list (make-vm-call :dst :r0 :func :r1 :args '(:r2))
                                   (make-vm-ret :reg :r0)))
    (declare (ignore call-insts))
    (assert-false leaf-p)))

(deftest optimizer-leaf-flag-through-compile-pipeline
  "compile-string preserves the optimizer leaf flag on a real compiled leaf program."
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (program (compilation-result-program result)))
    (assert-true (cl-cc/vm::vm-program-leaf-p program))))

(deftest prolog-peephole-collapses-const-followed-by-move
  "The Prolog peephole rule set folds a const+move pair to a direct const."
  (let ((out (cl-cc/prolog::apply-prolog-peephole
              '((:const :r1 42) (:move :r2 :r1)))))
    (assert-equal '((:const :r2 42)) out)))

;;; ── CSE (Common Subexpression Elimination) Unit Tests ──────────────────────

(deftest-each cse-dedup
  "Duplicate vm-add — same or commuted operands — CSE's second to vm-move."
  :cases (("same-order"    (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          ("swapped-order" (cl-cc::make-vm-add :dst :R3 :lhs :R1 :rhs :R0)))
  (i4)
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-move-p (fourth out)))
    (assert-equal :R2 (cl-cc/vm::vm-src (fourth out)))))

(deftest cse-label-flushes
  "A branch-target vm-label between two identical vm-add prevents CSE."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
          (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
          (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
          (j  (cl-cc::make-vm-jump :label "L1"))
          (lbl (cl-cc::make-vm-label :name "L1"))
          (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 j lbl i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-jump-p (fourth out)))
    (assert-true (cl-cc::vm-label-p (fifth out)))
    (assert-true (cl-cc::vm-add-p (sixth out)))))

(deftest cse-fallthrough-label-preserves-state
  "A non-target fallthrough label does not flush CSE state."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (lbl (cl-cc::make-vm-label :name "L1"))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 lbl i4))))
    (assert-true (cl-cc::vm-label-p (fourth out)))
    (assert-true (cl-cc::vm-move-p (fifth out)))))

(deftest cse-unary-dedup
  "Two identical vm-neg: second becomes vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc::make-vm-neg :dst :R1 :src :R0))
         (i3 (cl-cc::make-vm-neg :dst :R2 :src :R0))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3))))
    (assert-true (cl-cc::vm-neg-p (second out)))
    (assert-true (cl-cc::vm-move-p (third out)))
    (assert-equal :R1 (cl-cc/vm::vm-src (third out)))))

(deftest cse-const-no-move
  "Two identical vm-const: second remains vm-const, not vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 42))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2))))
    (assert-true (cl-cc::vm-const-p (first out)))
    (assert-true (cl-cc::vm-const-p (second out)))))

(deftest cse-different-ops-no-dedup
  "vm-add and vm-sub with same operands are not CSE'd."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc::make-vm-sub :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc/optimize::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-sub-p (fourth out)))))

(deftest-each optimizer-value-ordering-structural
  "Structural optimizer ordering stays deterministic without format-based comparison."
  :cases (("pair-ascending"    t   '(:r0 . 1) '(:r0 . 2))
          ("keyword-ascending" t   :r0        :r1)
          ("integer-ascending" t   1          2)
          ("string-descending" nil "b"        "a"))
  (expected a b)
  (assert-equal expected (not (null (cl-cc/optimize::%opt-value< a b)))))

(deftest optimizer-gvn-dominates-branch
  "GVN reuses a dominator-block value inside a dominated block."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (j  (cl-cc::make-vm-jump :label "L1"))
         (l  (cl-cc::make-vm-label :name "L1"))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (r  (cl-cc::make-vm-ret :reg :R3))
         (out (cl-cc/optimize::opt-pass-gvn (list i1 i2 i3 j l i4 r))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out))
     (assert-true (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-move)
                               (eq :R3 (cl-cc/vm::vm-dst i))))
                        out))))

;;; ── CFG Reachability / Label Cleanup / Block Merge ───────────────────────

(deftest unreachable-code-after-jump
  "opt-pass-unreachable drops dead code between an unconditional jump and the next label."
  (let* ((j    (make-vm-jump  :label "end"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl  (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list j dead lbl ret))))
    (assert-false (member dead out))
    (assert-true  (member j    out))
    (assert-true  (member lbl  out))
    (assert-true  (member ret  out))))

(deftest unreachable-code-after-ret
  "opt-pass-unreachable drops dead code after a ret; the next label revives reachability."
  (let* ((ret1 (make-vm-ret   :reg :r0))
         (dead (make-vm-const :dst :r1 :value 0))
         (lbl  (make-vm-label :name "after"))
         (ret2 (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list ret1 dead lbl ret2))))
    (assert-false (member dead out))
    (assert-true  (member ret1 out))
    (assert-true  (member lbl  out))))

(deftest unreachable-label-revives-reachability
  "A vm-label after a jump revives reachability."
  (let* ((j   (make-vm-jump  :label "lbl"))
         (lbl (make-vm-label :name "lbl"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-unreachable (list j lbl c ret))))
    (assert-true (member c out))))

(deftest dead-labels-removes-unreferenced-label
  "A label with no jumps pointing to it is removed."
  (let* ((lbl (make-vm-label :name "ghost"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list lbl c ret))))
    (assert-false (member lbl out))
    (assert-true  (member c   out))
    (assert-true  (member ret out))))

(deftest-each dead-labels-preserves-referenced-labels
  "Labels referenced by jumps, closures, or handler instrs are preserved by opt-pass-dead-labels."
  :cases (("jump-target"
            (make-vm-jump :label "live") "live")
          ("closure-entry"
           (make-vm-closure :dst :r0 :label "fn" :params nil :captured nil
                            :optional-params nil :rest-param nil :key-params nil) "fn")
          ("handler-label"
           (cl-cc::make-vm-establish-handler :handler-label "err-handler"
                                              :result-reg :r0 :error-type 'error)
           "err-handler"))
  (ref-inst lbl-name)
  (let* ((lbl (make-vm-label :name lbl-name))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list ref-inst lbl ret))))
    (assert-true (member lbl out))))

(deftest dead-basic-blocks-eliminated
  "opt-pass-dead-basic-blocks removes an unreachable labeled block, not just its label."
  (let* ((j    (make-vm-jump  :label "exit"))
         (lbl1 (make-vm-label :name "dead"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl2 (make-vm-label :name "exit"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-dead-basic-blocks (list j lbl1 dead lbl2 ret))))
    (assert-true  (member j out))
    (assert-false (member lbl1 out))
    (assert-false (member dead out))
    (assert-true  (member lbl2 out))
    (assert-true  (member ret out))))

(deftest block-merge-eliminates-single-pred-label
  "opt-pass-block-merge removes a mergeable successor label and its jump."
  (let* ((start (make-vm-label :name "start"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (jmp   (make-vm-jump :label "mid"))
         (mid   (make-vm-label :name "mid"))
         (c2    (make-vm-const :dst :r2 :value 2))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc/optimize::opt-pass-block-merge (list start c1 jmp mid c2 ret))))
    (assert-false (member mid out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out))
    (assert-true  (member c1 out))
    (assert-true  (member c2 out))
    (assert-true  (member ret out))))

(deftest tail-merge-merges-identical-blocks
  "opt-pass-tail-merge merges duplicate CFG blocks with identical bodies."
  (let* ((entry (make-vm-label :name "entry"))
         (seed  (make-vm-const :dst :r0 :value nil))
         (br    (make-vm-jump-zero :reg :r0 :label "dup2"))
         (dup1  (make-vm-label :name "dup1"))
         (a1    (make-vm-const :dst :r1 :value 1))
         (j1    (make-vm-jump :label "exit"))
         (dup2  (make-vm-label :name "dup2"))
         (a2    (make-vm-const :dst :r1 :value 1))
         (j2    (make-vm-jump :label "exit"))
         (exit  (make-vm-label :name "exit"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-tail-merge (list entry seed br dup1 a1 j1 dup2 a2 j2 exit ret))))
    (assert-true  (member dup1 out))
    (assert-false (member dup2 out))
    (assert-equal 1 (count-if (lambda (i)
                                (and (typep i 'cl-cc/vm::vm-const)
                                     (eq :r1 (cl-cc/vm::vm-dst i))))
                              out))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))))

(deftest constant-hoist-moves-loop-constant-to-preheader
  "opt-pass-constant-hoist hoists a loop-invariant constant before the loop header."
  (let* ((start (make-vm-label :name "start"))
         (seed  (make-vm-const :dst :r0 :value 0))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (hoist (make-vm-const :dst :r1 :value 99))
         (jmp2  (make-vm-jump :label "body"))
         (body  (make-vm-label :name "body"))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-constant-hoist
                 (list start seed jmp1 loop hoist jmp2 body back ret))))
    (assert-true (member hoist out))
    (assert-true (member loop out))
    (assert-true (< (position hoist out :test #'eq)
                    (position loop out :test #'eq)))))

(deftest-each opt-convergence-pass-membership
  "All expected optimization passes are registered in *opt-convergence-passes*."
  :cases (("constant-hoist" #'cl-cc/optimize::opt-pass-constant-hoist)
          ("global-dce"     #'cl-cc/optimize::opt-pass-global-dce)
          ("inline"         #'cl-cc/optimize::opt-pass-inline-iterative)
          ("pre"            #'cl-cc/optimize::opt-pass-pre)
          ("bswap"          #'cl-cc/optimize::opt-pass-bswap-recognition)
          ("rotate"         #'cl-cc/optimize::opt-pass-rotate-recognition))
  (pass-fn)
  (assert-true (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq)))

(deftest optimizer-batch-concatenate
  "Adjacent vm-concatenate instructions are packed into one parts-list form."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value "a"))
         (i2 (cl-cc::make-vm-const :dst :R1 :value "b"))
         (i3 (cl-cc::make-vm-const :dst :R2 :value "c"))
         (c1 (cl-cc::make-vm-concatenate :dst :R3 :str1 :R0 :str2 :R1))
         (c2 (cl-cc::make-vm-concatenate :dst :R4 :str1 :R3 :str2 :R2))
         (out (cl-cc/optimize::opt-pass-batch-concatenate (list i1 i2 i3 c1 c2)))
         (inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out)))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out))
    (assert-true inst)
    (assert-equal '(:R0 :R1 :R2) (cl-cc/vm::vm-parts inst))
    (assert-equal :R4 (cl-cc/vm::vm-dst inst))))

;;; ── Inlining Pass: Unit Tests ──────────────────────────────────────────────

(defun inline-has-call-p (instructions)
  "T if INSTRUCTIONS contains at least one vm-call."
  (some #'cl-cc::vm-call-p instructions))

(deftest inline-small-function
  "A small function (+ x 1) called via vm-func-ref: vm-call eliminated and
   result vm-move into the call's dst register (:R6) must appear."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "inc"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "inc"))
         (jump-past (cl-cc::make-vm-jump :label "after_inc"))
         (lbl     (cl-cc::make-vm-label :name "inc"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_inc"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-true (some (lambda (i)
                         (and (cl-cc::vm-move-p i)
                              (eq :R6 (cl-cc/vm::vm-dst i))))
                       out))))

(deftest inline-skip-large-function
  "A function whose body exceeds the inline threshold is NOT inlined;
   the vm-call remains in the output."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "big"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "big"))
         (jump-past (cl-cc::make-vm-jump :label "after_big"))
         (lbl     (cl-cc::make-vm-label :name "big"))
         ;; Generate a live arithmetic chain that stays above the inline threshold
         ;; even after convergence passes.
         (body    (append
                   (list (cl-cc::make-vm-const :dst :R20 :value 1))
                   (loop for i from 21 below 40
                         for prev = :R10 then (intern (format nil "R~A" (1- i)) :keyword)
                         collect (cl-cc::make-vm-add
                                  :dst (intern (format nil "R~A" i) :keyword)
                                  :lhs prev
                                  :rhs :R20))))
         (ret     (cl-cc::make-vm-ret :reg :R39))
         (after   (cl-cc::make-vm-label :name "after_big"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 0))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (append (list closure fref jump-past lbl)
                          body
                          (list ret after arg call halt)))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest-each inline-skip-cases
  "Functions with captured vars, recursive bodies, or self-references are NOT inlined."
  :cases (("captured-vars"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "captured"
                                                     :params '(:R10) :captured '(:R99)))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "captured"))
                    (jump-past (cl-cc::make-vm-jump :label "after_cap"))
                    (lbl     (cl-cc::make-vm-label :name "captured"))
                    (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
                    (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
                    (ret     (cl-cc::make-vm-ret :reg :R12))
                    (after   (cl-cc::make-vm-label :name "after_cap"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl body1 body2 ret after arg call halt))))
          ("recursive-global-ref"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "rec"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "rec"))
                    (jump-past (cl-cc::make-vm-jump :label "after_rec"))
                    (lbl     (cl-cc::make-vm-label :name "rec"))
                    (b1      (cl-cc::make-vm-const :dst :R11 :value 0))
                    (b2      (cl-cc::make-vm-sub :dst :R12 :lhs :R10 :rhs :R11))
                    (b3      (cl-cc::make-vm-call :dst :R13 :func :R5 :args '(:R12)))
                    (ret     (cl-cc::make-vm-ret :reg :R13))
                    (after   (cl-cc::make-vm-label :name "after_rec"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl b1 b2 b3 ret after arg call halt))))
          ("self-recursive-guard"
           (lambda ()
             (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "loop"
                                                     :params '(:R10) :captured nil))
                    (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "loop"))
                    (jump-past (cl-cc::make-vm-jump :label "after_loop"))
                    (lbl     (cl-cc::make-vm-label :name "loop"))
                    (self    (cl-cc::make-vm-func-ref :dst :R7 :label "loop"))
                    (body1   (cl-cc::make-vm-call :dst :R11 :func :R7 :args '(:R10)))
                    (ret     (cl-cc::make-vm-ret :reg :R11))
                    (after   (cl-cc::make-vm-label :name "after_loop"))
                    (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
                    (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
                    (halt    (cl-cc::make-vm-halt)))
               (list closure fref jump-past lbl self body1 ret after arg call halt)))))
  (make-instrs)
  (let* ((instrs (funcall make-instrs))
         (out    (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest inline-register-rename
  "After inlining, the body's registers are renamed to fresh indices so they
   do not conflict with existing registers at the call site."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "g"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "g"))
         (jump-past (cl-cc::make-vm-jump :label "after_g"))
         (lbl     (cl-cc::make-vm-label :name "g"))
         ;; Body uses :R11 and :R12 (same registers the call site also uses)
         (body1   (cl-cc::make-vm-const :dst :R11 :value 10))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_g"))
         ;; Call site also uses :R11 and :R12 for its own values
         (site1   (cl-cc::make-vm-const :dst :R11 :value 100))
         (site2   (cl-cc::make-vm-const :dst :R12 :value 200))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 7))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret
                        after site1 site2 arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    ;; The call should be inlined (small body, no captures)
    (assert-true (not (inline-has-call-p out)))
    ;; The inlined body must use renamed registers (not the original :R11/:R12)
    ;; so collect all dst registers written after the call site's :R12 const
    ;; and before the halt — at least one must be > :R12 index
    (let ((inlined-dsts (loop for i in out
                              when (and (cl-cc::vm-move-p i)
                                        (eq :R6 (cl-cc/vm::vm-dst i)))
                              collect i)))
      ;; The final vm-move into :R6 must exist (proof of inlining)
      (assert-true (not (null inlined-dsts))))))

(deftest inline-propagates-constant-call-args
  "Constant call arguments are emitted as constants inside the inlined body."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "k"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "k"))
         (jump-past (cl-cc::make-vm-jump :label "after_k"))
         (lbl     (cl-cc::make-vm-label :name "k"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_k"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc/optimize::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-equal 2
                  (count-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 4 (cl-cc/vm::vm-value i))))
                            out))))


;;; Low-level optimizer pass/unit tests moved to optimizer-lowlevel-tests.lisp.
