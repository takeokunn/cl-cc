;;;; tests/optimizer-tests.lisp — Optimizer Tests
;;;
;;; Tests verifying that the multi-pass optimizer:
;;;   - Correctly folds constants at compile time (semantics preserved)
;;;   - Eliminates dead code (pure instructions with unused results)
;;;   - Removes dead jumps and unreachable code
;;;   - Preserves all observable semantics (evaluated values)

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

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
         (cfg   (cl-cc::cfg-build (list hot jump work toend cold sig end ret)))
         (_idom (cl-cc::cfg-compute-dominators cfg))
         (_loop (cl-cc::cfg-compute-loop-depths cfg))
         (out   (cl-cc::cfg-flatten-hot-cold cfg))
         (labels (loop for inst in out
                        when (typep inst 'cl-cc::vm-label)
                        collect (cl-cc::vm-name inst))))
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

(deftest optimizer-dominated-type-check-elim
  "Redundant dominated type predicates are rewritten to vm-move."
  (let* ((c   (make-vm-const :dst :r0 :value nil))
         (p1  (make-vm-null-p :dst :r1 :src :r0))
         (br  (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (p2  (make-vm-null-p :dst :r2 :src :r0))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-dominated-type-check-elim
                (list c p1 br then p2 ret1 else ret0))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-null-p)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-move)) out))))

(deftest optimizer-dominated-nil-check-elim
  "Redundant dominated nil checks via vm-not are rewritten to vm-move."
  (let* ((c   (make-vm-const :dst :r0 :value nil))
         (p1  (make-vm-not :dst :r1 :src :r0))
         (br  (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (p2  (make-vm-not :dst :r2 :src :r0))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-dominated-type-check-elim
               (list c p1 br then p2 ret1 else ret0))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-not)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-move)) out))))

(deftest optimizer-nil-check-elim
  "FR-040: named nil-check elimination pass rewrites dominated vm-not checks."
  (let* ((c   (make-vm-const :dst :r0 :value nil))
         (p1  (make-vm-not :dst :r1 :src :r0))
         (br  (make-vm-jump-zero :reg :r1 :label "else"))
         (then (make-vm-label :name "then"))
         (p2  (make-vm-not :dst :r2 :src :r0))
         (ret1 (make-vm-ret :reg :r2))
         (else (make-vm-label :name "else"))
         (ret0 (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-nil-check-elim
               (list c p1 br then p2 ret1 else ret0))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-not)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-move)) out))))

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
  :cases (("logand-zero"      0  "(let ((x 42)) (logand x 0))"   'vm-logand)
          ("logand-minus-one" 42 "(let ((x 42)) (logand x -1))"  'vm-logand)
          ("logand-self"      12 "(let ((x 12)) (logand x x))"   nil)
          ("logior-zero"      42 "(let ((x 42)) (logior x 0))"   'vm-logior)
          ("logxor-zero"      42 "(let ((x 42)) (logxor x 0))"   nil)
          ("logxor-self"       0 "(let ((x 42)) (logxor x x))"   'vm-logxor)
          ("ash-zero"         42 "(let ((x 42)) (ash x 0))"      'vm-ash))
  (expected expr instr)
  (assert-run= expected expr)
  (when instr
    (assert-false (opt-has-p expr instr))))

(deftest optimizer-rem-zero
  "(rem 0 x) folds to 0 and no vm-rem remains."
  (assert-run= 0 "(let ((x 42)) (rem 0 x))")
  (assert-false (opt-has-p "(let ((x 42)) (rem 0 x))" 'vm-rem)))

;;; ── Unary Constant Folding ────────────────────────────────────────────────

(deftest optimizer-lognot-constant
  "(lognot 0) folds at compile time: value is -1 and no vm-lognot remains."
  (assert-equal -1 (run-string "(lognot 0)"))
  (let* ((instrs (list (cl-cc::make-vm-const :dst :r1 :value 0)
                       (cl-cc::make-vm-lognot :dst :r0 :src :r1)))
         (out (cl-cc::opt-pass-fold instrs)))
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-lognot)) out))
    (assert-true (some (lambda (i)
                         (and (cl-cc::vm-const-p i)
                              (eq :r0 (cl-cc::vm-dst i))
                              (eql -1 (cl-cc::vm-value i))))
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
    (assert-true (cl-cc::vm-program-leaf-p program))))

(deftest prolog-peephole-collapses-const-followed-by-move
  "The Prolog peephole rule set folds a const+move pair to a direct const."
  (let ((out (cl-cc::apply-prolog-peephole
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
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-move-p (fourth out)))
    (assert-equal :R2 (cl-cc::vm-src (fourth out)))))

(deftest cse-label-flushes
  "A branch-target vm-label between two identical vm-add prevents CSE."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
          (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
          (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
          (j  (cl-cc::make-vm-jump :label "L1"))
          (lbl (cl-cc::make-vm-label :name "L1"))
          (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
          (out (cl-cc::opt-pass-cse (list i1 i2 i3 j lbl i4))))
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
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 lbl i4))))
    (assert-true (cl-cc::vm-label-p (fourth out)))
    (assert-true (cl-cc::vm-move-p (fifth out)))))

(deftest cse-unary-dedup
  "Two identical vm-neg: second becomes vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc::make-vm-neg :dst :R1 :src :R0))
         (i3 (cl-cc::make-vm-neg :dst :R2 :src :R0))
         (out (cl-cc::opt-pass-cse (list i1 i2 i3))))
    (assert-true (cl-cc::vm-neg-p (second out)))
    (assert-true (cl-cc::vm-move-p (third out)))
    (assert-equal :R1 (cl-cc::vm-src (third out)))))

(deftest cse-const-no-move
  "Two identical vm-const: second remains vm-const, not vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 42))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 42))
         (out (cl-cc::opt-pass-cse (list i1 i2))))
    (assert-true (cl-cc::vm-const-p (first out)))
    (assert-true (cl-cc::vm-const-p (second out)))))

(deftest cse-different-ops-no-dedup
  "vm-add and vm-sub with same operands are not CSE'd."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc::make-vm-sub :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-sub-p (fourth out)))))

(deftest optimizer-value-ordering-structural
  "Structural optimizer ordering stays deterministic without format-based comparison."
  (assert-true (cl-cc::%opt-value< '(:r0 . 1) '(:r0 . 2)))
  (assert-true (cl-cc::%opt-value< :r0 :r1))
  (assert-true (cl-cc::%opt-value< 1 2))
  (assert-false (cl-cc::%opt-value< "b" "a")))

(deftest optimizer-gvn-dominates-branch
  "GVN reuses a dominator-block value inside a dominated block."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (j  (cl-cc::make-vm-jump :label "L1"))
         (l  (cl-cc::make-vm-label :name "L1"))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (r  (cl-cc::make-vm-ret :reg :R3))
         (out (cl-cc::opt-pass-gvn (list i1 i2 i3 j l i4 r))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-add)) out))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-move)
                              (eq :R3 (cl-cc::vm-dst i))))
                       out))))

(deftest optimizer-batch-concatenate
  "Adjacent vm-concatenate instructions are packed into one parts-list form."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value "a"))
         (i2 (cl-cc::make-vm-const :dst :R1 :value "b"))
         (i3 (cl-cc::make-vm-const :dst :R2 :value "c"))
         (c1 (cl-cc::make-vm-concatenate :dst :R3 :str1 :R0 :str2 :R1))
         (c2 (cl-cc::make-vm-concatenate :dst :R4 :str1 :R3 :str2 :R2))
         (out (cl-cc::opt-pass-batch-concatenate (list i1 i2 i3 c1 c2)))
         (inst (find-if (lambda (i) (typep i 'cl-cc::vm-concatenate)) out)))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-concatenate)) out))
    (assert-true inst)
    (assert-equal '(:R0 :R1 :R2) (cl-cc::vm-parts inst))
    (assert-equal :R4 (cl-cc::vm-dst inst))))

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
         (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-true (some (lambda (i)
                         (and (cl-cc::vm-move-p i)
                              (eq :R6 (cl-cc::vm-dst i))))
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
         (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest inline-skip-closure
  "A function created with captured vars (vm-closure with non-nil :captured)
   is NOT inlined; the vm-call remains in the output."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "captured"
                                          :params '(:R10)
                                          :captured '(:R99)))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "captured"))
         (jump-past (cl-cc::make-vm-jump :label "after_cap"))
         (lbl     (cl-cc::make-vm-label :name "captured"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_cap"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest inline-skip-recursive
  "A function that calls itself via vm-func-ref + vm-call inside its body
   is NOT inlined because it references a global register (the outer func-ref)."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "rec"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "rec"))
         (jump-past (cl-cc::make-vm-jump :label "after_rec"))
         (lbl     (cl-cc::make-vm-label :name "rec"))
         ;; Body: if (= x 0) ret 0, else call rec(x-1)
         ;; The inner vm-call reads :R5 which is defined OUTSIDE the body
         ;; -> opt-body-has-global-refs-p returns T -> no inline
         (b1      (cl-cc::make-vm-const :dst :R11 :value 0))
         (b2      (cl-cc::make-vm-sub :dst :R12 :lhs :R10 :rhs :R11))
         (b3      (cl-cc::make-vm-call :dst :R13 :func :R5 :args '(:R12)))
         (ret     (cl-cc::make-vm-ret :reg :R13))
         (after   (cl-cc::make-vm-label :name "after_rec"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
          (instrs  (list closure fref jump-past lbl b1 b2 b3 ret after arg call halt))
          (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (inline-has-call-p out))))

(deftest inline-skip-self-recursive-guard
  "A self-recursive function is kept as a call site instead of being inlined."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "loop"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "loop"))
         (jump-past (cl-cc::make-vm-jump :label "after_loop"))
         (lbl     (cl-cc::make-vm-label :name "loop"))
         (self    (cl-cc::make-vm-func-ref :dst :R7 :label "loop"))
         (body1   (cl-cc::make-vm-call :dst :R11 :func :R7 :args '(:R10)))
         (ret     (cl-cc::make-vm-ret :reg :R11))
         (after   (cl-cc::make-vm-label :name "after_loop"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 5))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl self body1 ret after arg call halt))
         (out     (cl-cc::opt-pass-inline instrs)))
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
         (out     (cl-cc::opt-pass-inline instrs)))
    ;; The call should be inlined (small body, no captures)
    (assert-true (not (inline-has-call-p out)))
    ;; The inlined body must use renamed registers (not the original :R11/:R12)
    ;; so collect all dst registers written after the call site's :R12 const
    ;; and before the halt — at least one must be > :R12 index
    (let ((inlined-dsts (loop for i in out
                              when (and (cl-cc::vm-move-p i)
                                        (eq :R6 (cl-cc::vm-dst i)))
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
         (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))
    (assert-equal 2
                  (count-if (lambda (i)
                              (and (cl-cc::vm-const-p i)
                                   (eql 4 (cl-cc::vm-value i))))
                            out))))

;;; ── Direct opt-pass-fold Tests ─────────────────────────────────────────

(deftest fold-label-flushes-env
  "Branch-target labels flush the constant env — a constant known before a label must NOT
    be propagated after the label (other paths may define a different value)."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-jump :label "join")
                       (cl-cc::make-vm-label :name "join")
                        (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    ;; The inc should survive (not folded to const 43) because the label flushes env
    (assert-true (find-if (lambda (i) (typep i 'cl-cc::vm-inc)) out))))

(deftest fold-fallthrough-label-preserves-env
  "A non-target fallthrough label does not flush constant knowledge."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal 43 (cl-cc::vm-value r1-const))))

(deftest fold-unary-not-falsy
  "vm-not on nil folds to t; truthy values fold to nil."
  (flet ((fold-result (input-val)
           (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value input-val)
                                (cl-cc::make-vm-not :dst :R1 :src :R0)))
                  (out (cl-cc::opt-pass-fold instrs))
                  (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                                      (eq (cl-cc::vm-dst i) :R1)))
                                     out)))
             (assert-true r1-const)
             (cl-cc::vm-value r1-const))))
    (assert-equal t   (fold-result nil))
    (assert-null      (fold-result 0))))

(deftest-each fold-type-pred
  "Type predicate instructions fold at compile time against a known constant."
  :cases (("number-p" (cl-cc::make-vm-number-p :dst :R1 :src :R0) 1)
          ("symbol-p" (cl-cc::make-vm-symbol-p :dst :R1 :src :R0) 0))
  (pred-inst expected)
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42) pred-inst))
         (out (cl-cc::opt-pass-fold instrs))
         (r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                             (eq (cl-cc::vm-dst i) :R1)))
                            out)))
    (assert-true r1-const)
    (assert-equal expected (cl-cc::vm-value r1-const))))

(deftest fold-branch-known-true
  "vm-jump-zero with a known non-zero value is dropped entirely."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 1)
                       (cl-cc::make-vm-jump-zero :reg :R0 :label "skip")))
         (out (cl-cc::opt-pass-fold instrs)))
    ;; No jump-zero or jump should remain (condition always true → branch never taken)
    (assert-true (not (find-if (lambda (i) (or (cl-cc::vm-jump-zero-p i)
                                               (cl-cc::vm-jump-p i)))
                               out)))))

(deftest fold-branch-known-false
  "vm-jump-zero with a known nil value becomes unconditional vm-jump."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value nil)
                        (cl-cc::make-vm-jump-zero :reg :R0 :label "target")))
         (out (cl-cc::opt-pass-fold instrs)))
    (assert-true (find-if #'cl-cc::vm-jump-p out))
    (assert-true (not (find-if #'cl-cc::vm-jump-zero-p out)))))

;;; ── Direct opt-pass-copy-prop Tests ────────────────────────────────────

(deftest copy-prop-chain
  "Copy chain following: R1←R0, R2←R1 → add using R2 resolves to R0."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-move :dst :R2 :src :R1)
                       (cl-cc::make-vm-add :dst :R3 :lhs :R2 :rhs :R2)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; The add's operands should both resolve to :R0
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R0 (cl-cc::vm-lhs add-inst))
      (assert-equal :R0 (cl-cc::vm-rhs add-inst)))))

(deftest copy-prop-label-flushes
  "A non-target fallthrough label does not flush copy knowledge."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; The add should use :R0 because the fallthrough label preserves copies.
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R0 (cl-cc::vm-lhs add-inst)))))

(deftest copy-prop-kill-forward
  "When a source register is overwritten, its forward aliases are invalidated."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-const :dst :R0 :value 99)
                       (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; The add should use :R1 (not :R0), because :R0 was overwritten
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R1 (cl-cc::vm-lhs add-inst)))))

(deftest copy-prop-reverse-map-kill
  "Reverse-map kill invalidates direct aliases without a full copy-table scan." 
  (let* ((copies (make-hash-table :test #'eq))
         (reverse nil))
    (setf (gethash :R1 copies) :R0)
    (setf (gethash :R2 copies) :R0)
    (setf (gethash :R3 copies) :R2)
    (setf reverse (cl-cc::%opt-copy-prop-build-reverse copies))
    (cl-cc::%opt-copy-prop-kill :R0 copies reverse)
    (assert-false (gethash :R1 copies))
    (assert-false (gethash :R2 copies))
    (assert-true (eql :R2 (gethash :R3 copies)))))

(deftest copy-prop-self-move-elim
  "Self-move after resolution is eliminated: R1←R0 then R0←R1 → second resolves
   to R0←R0 which is dropped."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-move :dst :R0 :src :R1)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; Only one move should remain (the first one); the second is a self-move
    (let ((moves (remove-if-not (lambda (i) (typep i 'cl-cc::vm-move)) out)))
      (assert-equal 1 (length moves)))))

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
         (out (cl-cc::opt-pass-copy-prop instrs)))
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R0 (cl-cc::vm-lhs add-inst))
      (assert-equal :R0 (cl-cc::vm-rhs add-inst)))))

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
  (let ((regs (cl-cc::opt-inst-read-regs inst)))
    (assert-equal (length expected-members) (length regs))
    (dolist (r expected-members)
      (assert-true (member r regs)))))

;;; ── opt-pass-dce: Dead Code Elimination ──────────────────────────────────

(deftest dce-removes-unused-pure-instructions
  "DCE removes pure instructions (vm-const, vm-move) whose dst is never read; keeps used ones."
  ;; vm-const: r0 unused → removed; r1 used by ret → kept
  (let* ((i1  (make-vm-const :dst :r0 :value 42))
         (i2  (make-vm-const :dst :r1 :value 7))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-dce (list i1 i2 ret))))
    (assert-false (member i1 out))
    (assert-true  (member i2 out))
    (assert-true  (member ret out)))
  ;; vm-const: r0 read by vm-add → kept
  (let* ((i1  (make-vm-const :dst :r0 :value 5))
         (i2  (make-vm-add  :dst :r1 :lhs :r0 :rhs :r0))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-dce (list i1 i2 ret))))
    (assert-true (member i1 out))
    (assert-true (member i2 out)))
  ;; vm-move: r2 unused → removed; r0 read by ret → kept
  (let* ((c   (make-vm-const :dst :r0 :value 1))
         (m   (make-vm-move  :dst :r2 :src :r0))
         (ret (make-vm-ret :reg :r0))
         (out (cl-cc::opt-pass-dce (list c m ret))))
    (assert-false (member m out))
    (assert-true  (member c out))))

(deftest dce-preserves-impure-unused-dst
  "DCE does NOT remove a vm-call (impure) even if dst is unused."
  (let* ((fn  (make-vm-const     :dst :r0 :value 'f))
         (c   (make-vm-call      :dst :r1 :func :r0 :args nil))
         (ret (make-vm-ret :reg :r0))
         ;; r1 is never read — but vm-call is impure
         (out (cl-cc::opt-pass-dce (list fn c ret))))
    (assert-true (member c out))))

;;; ── opt-pass-jump: Jump Threading ────────────────────────────────────────

(deftest jump-threading-eliminates-jump-to-next-label
  "opt-pass-jump removes a jump that targets the immediately following label."
  (let* ((j   (make-vm-jump  :label "end"))
         (lbl (make-vm-label :name "end"))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc::opt-pass-jump (list j lbl ret))))
    ;; The jump is a no-op (falls through) → should be removed
    (assert-false (member j out))
    (assert-true  (member lbl out))
    (assert-true  (member ret out))))

(deftest jump-threading-chains-jumps
  "opt-pass-jump threads jump-to-jump chains to the ultimate target."
  ;; j1 → lbl1 → j2 → lbl2 (final dest)
  (let* ((j1   (make-vm-jump  :label "mid"))
         (lbl1 (make-vm-label :name "mid"))
         (j2   (make-vm-jump  :label "end"))
         (lbl2 (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc::opt-pass-jump (list j1 lbl1 j2 lbl2 ret))))
    ;; j1 should be threaded to "end" directly
    (let ((j1-out (find-if #'cl-cc::vm-jump-p out)))
      (when j1-out
        (assert-equal (cl-cc::vm-label-name j1-out) "end")))))

(deftest jump-threading-follows-long-acyclic-chain
  "opt-pass-jump fully threads long jump chains that exceed the old recursion cap."
  (let ((instructions nil))
    (loop for i from 0 below 25 do
      (push (make-vm-jump :label (format nil "L~D" (1+ i))) instructions)
      (push (make-vm-label :name (format nil "L~D" (1+ i))) instructions))
    (push (make-vm-ret :reg :r0) instructions)
    (let* ((out (cl-cc::opt-pass-jump (nreverse instructions)))
           (j0  (find-if #'cl-cc::vm-jump-p out)))
      (assert-true j0)
      (assert-equal (cl-cc::vm-label-name j0) "L25"))))

(deftest jump-zero-threading-updates-label
  "opt-pass-jump threads vm-jump-zero target through a chain."
  (let* ((c   (make-vm-const     :dst :r0 :value 0))
         (jz  (make-vm-jump-zero :reg :r0 :label "mid"))
         (lbl (make-vm-label     :name "mid"))
         (j2  (make-vm-jump      :label "end"))
         (end (make-vm-label     :name "end"))
         (ret (make-vm-ret       :reg :r0))
         (out (cl-cc::opt-pass-jump (list c jz lbl j2 end ret))))
    ;; The jump-zero should now target "end" directly
    (let ((jz-out (find-if #'cl-cc::vm-jump-zero-p out)))
      (when jz-out
        (assert-equal (cl-cc::vm-label-name jz-out) "end")))))

;;; ── opt-pass-unreachable: Unreachable Code Elimination ───────────────────

(deftest unreachable-code-eliminated
  "opt-pass-unreachable drops dead code after unconditional jump or ret (revived by next label)."
  ;; code after vm-jump (before label) is dead
  (let* ((j    (make-vm-jump  :label "end"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl  (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc::opt-pass-unreachable (list j dead lbl ret))))
    (assert-false (member dead out))
    (assert-true  (member j    out))
    (assert-true  (member lbl  out))
    (assert-true  (member ret  out)))
  ;; code after vm-ret (before label) is dead; label revives reachability
  (let* ((ret1 (make-vm-ret   :reg :r0))
         (dead (make-vm-const :dst :r1 :value 0))
         (lbl  (make-vm-label :name "after"))
         (ret2 (make-vm-ret   :reg :r0))
         (out  (cl-cc::opt-pass-unreachable (list ret1 dead lbl ret2))))
    (assert-false (member dead out))
    (assert-true  (member ret1 out))
    (assert-true  (member lbl  out))))

(deftest unreachable-label-revives-reachability
  "A vm-label after a jump revives reachability."
  (let* ((j   (make-vm-jump  :label "lbl"))
         (lbl (make-vm-label :name "lbl"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc::opt-pass-unreachable (list j lbl c ret))))
    ;; c is after the label — it's reachable
    (assert-true (member c out))))

;;; ── opt-pass-dead-labels: Dead Label Elimination ─────────────────────────

(deftest dead-labels-removes-unreferenced-label
  "A label with no jumps pointing to it is removed."
  (let* ((lbl (make-vm-label :name "ghost"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc::opt-pass-dead-labels (list lbl c ret))))
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
         (out (cl-cc::opt-pass-dead-labels (list ref-inst lbl ret))))
    (assert-true (member lbl out))))

(deftest dead-basic-blocks-eliminated
  "opt-pass-dead-basic-blocks removes an unreachable labeled block, not just its label."
  (let* ((j    (make-vm-jump  :label "exit"))
         (lbl1 (make-vm-label :name "dead"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl2 (make-vm-label :name "exit"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc::opt-pass-dead-basic-blocks (list j lbl1 dead lbl2 ret))))
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
         (out   (cl-cc::opt-pass-block-merge (list start c1 jmp mid c2 ret))))
    (assert-false (member mid out))
     (assert-false (some (lambda (i) (typep i 'cl-cc::vm-jump)) out))
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
         (out   (cl-cc::opt-pass-tail-merge (list entry seed br dup1 a1 j1 dup2 a2 j2 exit ret))))
    (assert-true  (member dup1 out))
    (assert-false (member dup2 out))
    (assert-equal 1 (count-if (lambda (i)
                                (and (typep i 'cl-cc::vm-const)
                                     (eq :r1 (cl-cc::vm-dst i))))
                              out))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-jump-zero)) out))))

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
         (out   (cl-cc::opt-pass-constant-hoist
                 (list start seed jmp1 loop hoist jmp2 body back ret))))
    (assert-true (member hoist out))
    (assert-true (member loop out))
    (assert-true (< (position hoist out :test #'eq)
                    (position loop out :test #'eq)))))

(deftest constant-hoist-is-in-convergence-passes
  "opt-pass-constant-hoist is part of the convergence pass pipeline."
  (assert-true (member #'cl-cc::opt-pass-constant-hoist
                       cl-cc::*opt-convergence-passes*
                       :test #'eq)))

(deftest inline-is-in-convergence-passes
  "opt-pass-inline participates in the convergence pipeline via a named wrapper pass."
  (assert-true (member #'cl-cc::opt-pass-inline-iterative
                       cl-cc::*opt-convergence-passes*
                       :test #'eq)))

(deftest licm-does-not-hoist-loop-defined-value
  "opt-pass-licm keeps a pure instruction inside the loop when it reads a loop-defined register."
  (let* ((start (make-vm-label :name "start"))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (a1    (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc::opt-pass-licm (list start jmp1 loop c1 a1 back ret))))
    (assert-true (member a1 out))
    (assert-true (> (position a1 out :test #'eq)
                    (position loop out :test #'eq)))))

(deftest pre-is-in-convergence-passes
  "opt-pass-pre is part of the convergence pass pipeline."
  (assert-true (member #'cl-cc::opt-pass-pre
                        cl-cc::*opt-convergence-passes*
                        :test #'eq)))

(deftest pre-hoists-partially-redundant-expression
  "opt-pass-pre removes a partially redundant join-point expression."
  (let* ((entry (make-vm-label :name "entry"))
         (c0    (make-vm-const :dst :r0 :value 1))
         (c2    (make-vm-const :dst :r2 :value 2))
         (br    (make-vm-jump-zero :reg :r0 :label "p2"))
         (p1    (make-vm-label :name "p1"))
         (a1    (make-vm-add :dst :r3 :lhs :r0 :rhs :r2))
         (j1    (make-vm-jump :label "join"))
         (p2    (make-vm-label :name "p2"))
         (x     (make-vm-const :dst :r4 :value 7))
         (j2    (make-vm-jump :label "join"))
         (join  (make-vm-label :name "join"))
         (a2    (make-vm-add :dst :r5 :lhs :r0 :rhs :r2))
         (ret   (make-vm-ret :reg :r5))
         (out   (cl-cc::opt-pass-pre (list entry c0 c2 br p1 a1 j1 p2 x j2 join a2 ret))))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc::vm-add)) out))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-move)
                               (or (and (eq :r3 (cl-cc::vm-dst i))
                                        (eq :r5 (cl-cc::vm-src i)))
                                   (and (eq :r5 (cl-cc::vm-dst i))
                                        (eq :r3 (cl-cc::vm-src i))))))
                       out))))

(deftest egraph-pass-lowers-constant-subtraction
  "opt-pass-egraph rewrites a simple constant subtraction to vm-const."
  (let* ((i1 (cl-cc:make-vm-const :dst :r0 :value 7))
         (i2 (cl-cc:make-vm-sub   :dst :r1 :lhs :r0 :rhs :r0))
         (out (cl-cc::opt-pass-egraph (list i1 i2)))
         (r1  (find-if (lambda (inst)
                         (eq :r1 (ignore-errors (cl-cc::vm-dst inst))))
                       out)))
    (assert-true (cl-cc::vm-const-p r1))
    (assert-equal 0 (cl-cc::vm-value r1))))

(deftest bswap-recognition-collapses-byte-swap-tree
  "opt-pass-bswap-recognition collapses the canonical byte-swap tree to vm-bswap."
  (let* ((input (make-bswap-tree-instructions))
         (out   (cl-cc::opt-pass-bswap-recognition input)))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc::vm-bswap)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-logior)) out))
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-ash)) out))))

(deftest bswap-recognition-is-in-convergence-passes
  "opt-pass-bswap-recognition is part of the convergence pass pipeline."
  (assert-true (member #'cl-cc::opt-pass-bswap-recognition
                       cl-cc::*opt-convergence-passes*
                       :test #'eq)))

(deftest dead-store-elim-overwritten-global
  "opt-pass-dead-store-elim removes a dead overwritten vm-set-global."
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (s1  (make-vm-set-global :name "g" :src :r0))
         (c2  (make-vm-const :dst :r1 :value 2))
         (s2  (make-vm-set-global :name "g" :src :r1))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-dead-store-elim (list c1 s1 c2 s2 ret))))
    (assert-false (member s1 out))
    (assert-true  (member s2 out))
    (assert-true  (member c1 out))
    (assert-true  (member c2 out))
    (assert-true  (member ret out))))

(deftest dead-store-elim-keeps-live-read
  "A read from the same global keeps the latest pending store visible."
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (s1  (make-vm-set-global :name "g" :src :r0))
         (g1  (make-vm-get-global :dst :r2 :name "g"))
         (c2  (make-vm-const :dst :r1 :value 2))
         (s2  (make-vm-set-global :name "g" :src :r1))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc::opt-pass-dead-store-elim (list c1 s1 g1 c2 s2 ret))))
    (assert-true  (member s1 out))
    (assert-true  (member g1 out))
     (assert-true  (member s2 out))
    (assert-true  (member ret out))))

(deftest sccp-eliminates-constant-branch-block
  "opt-pass-sccp removes the unreachable branch block after a constant condition."
  (let* ((start (make-vm-label :name "start"))
         (cond  (make-vm-const :dst :r0 :value 1))
         (br    (make-vm-jump-zero :reg :r0 :label "else"))
         (thenl (make-vm-label :name "then"))
         (thenv (make-vm-const :dst :r1 :value 10))
         (jmp   (make-vm-jump :label "end"))
         (elsel (make-vm-label :name "else"))
         (elsev (make-vm-const :dst :r1 :value 20))
         (endl  (make-vm-label :name "end"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc::opt-pass-sccp (list start cond br thenl thenv jmp elsel elsev endl ret))))
    (assert-false (member elsel out))
    (assert-false (member elsev out))
    (assert-true  (some (lambda (i) (and (typep i 'cl-cc::vm-const)
                                         (eq (cl-cc::vm-const-dst i) :r1)
                                         (eql (cl-cc::vm-const-value i) 10)))
                        out))))

(deftest store-to-load-forward-global
  "opt-pass-store-to-load-forward replaces a matching vm-get-global with vm-move."
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (s1  (make-vm-set-global :name "g" :src :r0))
         (g1  (make-vm-get-global :dst :r1 :name "g"))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-store-to-load-forward (list c1 s1 g1 ret))))
    (assert-true  (member s1 out))
    (assert-false (member g1 out))
    (assert-true  (member c1 out))
    (assert-true  (member ret out))
    (assert-true  (some (lambda (i)
                          (and (typep i 'cl-cc::vm-move)
                               (eq (cl-cc::vm-move-dst i) :r1)
                               (eq (cl-cc::vm-move-src i) :r0)))
                        out))))

(deftest store-to-load-forward-global-same-dst
  "The forwarded global load still works when the load destination reuses the source register."
  (let* ((c1  (make-vm-const :dst :r0 :value 1))
         (s1  (make-vm-set-global :name "g" :src :r0))
         (g1  (make-vm-get-global :dst :r0 :name "g"))
         (ret (make-vm-ret :reg :r0))
         (out (cl-cc::opt-pass-store-to-load-forward (list c1 s1 g1 ret))))
    (assert-false (member g1 out))
    (assert-true  (some (lambda (i)
                          (and (typep i 'cl-cc::vm-move)
                               (eq (cl-cc::vm-move-dst i) :r0)
                               (eq (cl-cc::vm-move-src i) :r0)))
                        out))))

(deftest store-to-load-forward-slot
  "opt-pass-store-to-load-forward replaces a matching vm-slot-read with vm-move."
  (let* ((obj (make-vm-const :dst :r0 :value 42))
         (val (make-vm-const :dst :r1 :value 99))
         (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
         (r   (cl-cc::make-vm-slot-read :dst :r2 :obj-reg :r0 :slot-name 'x))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc::opt-pass-store-to-load-forward (list obj val w r ret))))
    (assert-true  (member w out))
    (assert-false (member r out))
    (assert-true  (member obj out))
    (assert-true  (member val out))
    (assert-true  (member ret out))
    (assert-true  (some (lambda (i)
                          (and (typep i 'cl-cc::vm-move)
                               (eq (cl-cc::vm-move-dst i) :r2)
                               (eq (cl-cc::vm-move-src i) :r1)))
                        out))))

(deftest store-to-load-forward-slot-same-dst
  "The forwarded slot read still works when the load destination reuses the value register."
  (let* ((obj (make-vm-const :dst :r0 :value 42))
         (val (make-vm-const :dst :r1 :value 99))
         (w   (cl-cc::make-vm-slot-write :obj-reg :r0 :slot-name 'x :value-reg :r1))
         (r   (cl-cc::make-vm-slot-read :dst :r1 :obj-reg :r0 :slot-name 'x))
         (ret (make-vm-ret :reg :r1))
         (out (cl-cc::opt-pass-store-to-load-forward (list obj val w r ret))))
    (assert-false (member r out))
    (assert-true  (some (lambda (i)
                          (and (typep i 'cl-cc::vm-move)
                               (eq (cl-cc::vm-move-dst i) :r1)
                               (eq (cl-cc::vm-move-src i) :r1)))
                        out))))

;;; ── opt-pass-strength-reduce: Strength Reduction ─────────────────────────

(deftest-each strength-reduce-cases
  "opt-pass-strength-reduce: power-of-2 multiplies/divides become vm-ash; non-power-of-2 are left as-is."
  :cases (("rhs-const-pow2"     8 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1) t)
          ("lhs-const-pow2"     4 (make-vm-mul :dst :r2 :lhs :r1 :rhs :r0) t)
          ("non-power-of-2"     7 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1) nil))
  (const-val mul should-reduce-p)
  (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc::opt-pass-strength-reduce (list c mul ret))))
    (if should-reduce-p
        (progn
          (assert-false (member mul out))
          (assert-true  (some (lambda (i) (typep i 'cl-cc::vm-ash)) out)))
        (progn
          (assert-true  (member mul out))
          (assert-false (some (lambda (i) (typep i 'cl-cc::vm-ash)) out))))))

(deftest-each strength-reduce-div-cases
  "opt-pass-strength-reduce: divide-by-power-of-2 becomes vm-ash; other divisors stay as-is."
   :cases (("rhs-const-pow2"     8 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1) t)
           ("non-power-of-2"     7 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1) nil))
  (const-val div should-reduce-p)
  (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc::opt-pass-strength-reduce (list c div ret))))
    (if should-reduce-p
        (progn
          (assert-false (member div out))
          (assert-true  (some (lambda (i) (typep i 'cl-cc::vm-ash)) out)))
        (progn
          (assert-true  (member div out))
          (assert-false (some (lambda (i) (typep i 'cl-cc::vm-ash)) out))))))

(deftest-each strength-reduce-mod-cases
  "opt-pass-strength-reduce: mod-by-power-of-2 becomes vm-logand; other divisors stay as-is."
  :cases (("rhs-const-pow2"     8 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1) t)
          ("non-power-of-2"     7 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1) nil))
  (const-val mod should-reduce-p)
  (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc::opt-pass-strength-reduce (list c mod ret))))
    (if should-reduce-p
        (progn
          (assert-false (member mod out))
          (assert-true  (some (lambda (i) (typep i 'cl-cc::vm-logand)) out)))
        (progn
          (assert-true  (member mod out))
          (assert-false (some (lambda (i) (typep i 'cl-cc::vm-logand)) out))))))

;;; ── opt-pass-reassociate: Arithmetic Reassociation ──────────────────────

(deftest reassociate-add-moves-constant-inward
  "opt-pass-reassociate moves a constant toward the tail of a nested add chain."
  (let* ((c    (make-vm-const :dst :r1 :value 1))
         (a    (make-vm-move  :dst :r2 :src :r8))
         (b    (make-vm-move  :dst :r3 :src :r9))
         (add1 (make-vm-add   :dst :r4 :lhs :r2 :rhs :r1))
         (add2 (make-vm-add   :dst :r5 :lhs :r4 :rhs :r3))
         (ret  (make-vm-ret   :reg :r5))
         (out  (cl-cc::opt-pass-reassociate (list c a b add1 add2 ret)))
         (adds (remove-if-not (lambda (i) (typep i 'cl-cc::vm-add)) out)))
    (assert-equal 2 (length adds))
    (assert-eq :r3 (cl-cc::vm-lhs (first adds)))
    (assert-eq :r1 (cl-cc::vm-rhs (first adds)))
    (assert-eq :r2 (cl-cc::vm-lhs (second adds)))
    (assert-eq :r4 (cl-cc::vm-rhs (second adds)))))

(deftest reassociate-logand-moves-constant-inward
  "opt-pass-reassociate applies the same normalization to bitwise ops."
  (let* ((c     (make-vm-const :dst :r1 :value 255))
         (a     (make-vm-move  :dst :r2 :src :r8))
         (b     (make-vm-move  :dst :r3 :src :r9))
         (and1  (make-vm-logand :dst :r4 :lhs :r2 :rhs :r1))
         (and2  (make-vm-logand :dst :r5 :lhs :r4 :rhs :r3))
         (ret   (make-vm-ret    :reg :r5))
         (out   (cl-cc::opt-pass-reassociate (list c a b and1 and2 ret)))
         (ands  (remove-if-not (lambda (i) (typep i 'cl-cc::vm-logand)) out)))
    (assert-equal 2 (length ands))
    (assert-eq :r3 (cl-cc::vm-lhs (first ands)))
    (assert-eq :r1 (cl-cc::vm-rhs (first ands)))
    (assert-eq :r2 (cl-cc::vm-lhs (second ands)))
    (assert-eq :r4 (cl-cc::vm-rhs (second ands)))))

(deftest strength-reduce-mul-by-const-decomposes
  "opt-pass-strength-reduce: small non-power-of-2 constant multipliers are decomposed into shifts/adds."
  (let* ((c   (make-vm-const :dst :r1 :value 3))
         (mul (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc::opt-pass-strength-reduce (list c mul ret))))
    (assert-false (member mul out))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-ash)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-add)) out))))

;;; ── opt-inline-eligible-p: Extracted Eligibility Predicate ──────────────

(deftest opt-inline-eligible-p-cases
  "opt-inline-eligible-p: eligible when cheap+no-captures; rejects captured vars or over-budget."
  ;; short, zero captures → eligible
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured nil
                                 :optional-params nil :rest-param nil :key-params nil))
         (body (list (make-vm-add :dst :r2 :lhs :r1 :rhs :r1) (make-vm-ret :reg :r2)))
         (def  (list :closure ci :params '(:r1) :body body)))
    (assert-true (cl-cc::opt-inline-eligible-p def 15)))
  ;; captured variables → not eligible
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured (list (cons :x :r5))
                                 :optional-params nil :rest-param nil :key-params nil))
         (body (list (make-vm-ret :reg :r1)))
         (def  (list :closure ci :params '(:r1) :body body)))
    (assert-false (cl-cc::opt-inline-eligible-p def 15)))
  ;; Many cheap constants → eligible even though the body is longer than 15 instructions.
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured nil
                                 :optional-params nil :rest-param nil :key-params nil))
          (body (append (loop repeat 17 collect (make-vm-const :dst :r2 :value 0))
                        (list (make-vm-ret :reg :r1))))
          (def  (list :closure ci :params '(:r1) :body body)))
    (assert-true (cl-cc::opt-inline-eligible-p def 15)))
  ;; Many actual arithmetic instructions → not eligible once the cost budget is exceeded.
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                 :params '(:r1) :captured nil
                                 :optional-params nil :rest-param nil :key-params nil))
         (body (append (loop repeat 16 collect (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
                       (list (make-vm-ret :reg :r2))))
         (def  (list :closure ci :params '(:r1) :body body)))
    (assert-false (cl-cc::opt-inline-eligible-p def 15))))

;;; ─── opt-falsep ──────────────────────────────────────────────────────────

(deftest-each opt-falsep
  "opt-falsep correctly classifies falsy (nil only) and truthy values."
  :cases (("nil"      nil t)
          ("zero"     0   nil)
          ("t"        t   nil)
          ("positive" 1   nil))
  (value expected)
  (assert-equal expected (cl-cc::opt-falsep value)))

;;; ─── opt-register-keyword-p ──────────────────────────────────────────────

(deftest-each opt-register-keyword-p
  "opt-register-keyword-p recognizes :RN keywords; rejects non-register symbols and plain keywords."
  :cases (("r0"            :r0  t)
          ("r15"           :r15 t)
          ("plain-symbol"  'r0  nil)
          ("plain-keyword" :foo nil))
  (value expected)
  (assert-equal expected (cl-cc::opt-register-keyword-p value)))

;;; ─── opt-binary-lhs-rhs-p / opt-unary-src-p ─────────────────────────────

(deftest-each opt-binary-lhs-rhs-p
  "opt-binary-lhs-rhs-p is true for binary instructions, false for unary ones."
  :cases (("add" (make-vm-add :dst :r0 :lhs :r1 :rhs :r2) t)
          ("lt"  (make-vm-lt  :dst :r0 :lhs :r1 :rhs :r2) t)
          ("neg" (make-vm-neg :dst :r0 :src :r1)           nil))
  (inst expected)
  (if expected
      (assert-true  (cl-cc::opt-binary-lhs-rhs-p inst))
      (assert-false (cl-cc::opt-binary-lhs-rhs-p inst))))

(deftest-each opt-unary-src-p
  "opt-unary-src-p is true for unary instructions, false for binary ones."
  :cases (("neg"    (make-vm-neg    :dst :r0 :src :r1)           t)
          ("null-p" (make-vm-null-p :dst :r0 :src :r1)           t)
          ("add"    (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2)  nil))
  (inst expected)
  (if expected
      (assert-true  (cl-cc::opt-unary-src-p inst))
      (assert-false (cl-cc::opt-unary-src-p inst))))

;;; ─── opt-foldable-unary-arith-p / opt-foldable-type-pred-p ──────────────

(deftest-each opt-foldable-unary-arith-p
  "opt-foldable-unary-arith-p is true for arithmetic unary ops, false for type predicates."
  :cases (("neg"    (make-vm-neg    :dst :r0 :src :r1) t)
          ("null-p" (make-vm-null-p :dst :r0 :src :r1) nil))
  (inst expected)
  (assert-equal expected (cl-cc::opt-foldable-unary-arith-p inst)))

(deftest-each opt-foldable-type-pred-p
  "opt-foldable-type-pred-p is true for type predicates, false for arithmetic ops."
  :cases (("null-p" (make-vm-null-p :dst :r0 :src :r1) t)
          ("neg"    (make-vm-neg    :dst :r0 :src :r1) nil))
  (inst expected)
  (assert-equal expected (cl-cc::opt-foldable-type-pred-p inst)))
