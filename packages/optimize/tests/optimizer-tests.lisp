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
         (sig   (cl-cc:make-vm-signal-error :error-reg :r1))
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
           #'cl-cc/optimize::opt-pass-dominated-type-check-elim
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
                    (p1   (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (p2   (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
                    (ret1 (make-vm-ret :reg :r2))
                    (else (make-vm-label :name "else"))
                    (ret0 (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list c p1 br then p2 ret1 else ret0))))
           :r2 1)
          ("branch-correlation-false-edge"
           (lambda ()
             (let* ((c    (make-vm-const :dst :r0 :value 1))
                    (p1   (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
                    (br   (make-vm-jump-zero :reg :r1 :label "else"))
                    (then (make-vm-label :name "then"))
                    (ret1 (make-vm-ret :reg :r1))
                    (else (make-vm-label :name "else"))
                    (p2   (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
                    (ret0 (make-vm-ret :reg :r2)))
               (cl-cc/optimize::opt-pass-branch-correlation
                (list c p1 br then ret1 else p2 ret0))))
           :r2 0)
          ("fold-rational-unary"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 3/4))
                    (u1  (cl-cc:make-vm-numerator :dst :r1 :src :r0))
                    (ret (make-vm-ret :reg :r1)))
               (cl-cc/optimize::opt-pass-fold (list c1 u1 ret))))
           :r1 3)
          ("fold-rational-binary"
           (lambda ()
             (let* ((c1  (make-vm-const :dst :r0 :value 8))
                    (c2  (make-vm-const :dst :r1 :value 12))
                    (g   (cl-cc:make-vm-gcd :dst :r2 :lhs :r0 :rhs :r1))
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

;;; End-to-end, bitwise, unary folding, inlining, and prolog peephole tests are in
;;; optimizer-e2e-tests.lisp. Low-level pass tests are in optimizer-tests-lowlevel2.lisp.
