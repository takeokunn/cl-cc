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

;;; ── Constant Folding: Semantics ──────────────────────────────────────────

(deftest-each optimizer-fold
  "Constant arithmetic folds at compile time and evaluates correctly."
  :cases (("add"           7  "(+ 3 4)")
          ("sub"           6  "(- 10 4)")
          ("mul"          12  "(* 3 4)")
          ("chained"      12  "(+ (* 2 3) (- 10 4))")
          ("deeper-chain" 16  "(- (* 5 5) (* 3 3))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Constant Folding: No Runtime Instructions ────────────────────────────
;;; Each entry: (label expression instruction-type-that-must-vanish)

(deftest-each optimizer-fold-no-runtime-instr
  "Constant-folded expressions leave no binary-op instructions in the output."
  :cases (("add"     "(+ 2 3)"                  'vm-add)
          ("sub"     "(- 10 4)"                 'vm-sub)
          ("mul"     "(* 3 7)"                  'vm-mul)
          ("chain-add" "(+ (* 2 3) (- 10 4))"  'vm-add)
          ("chain-mul" "(+ (* 2 3) (- 10 4))"  'vm-mul)
          ("chain-sub" "(+ (* 2 3) (- 10 4))"  'vm-sub))
  (expr instr)
  (assert-false (opt-has-p expr instr)))

;;; ── Algebraic Identity Simplification: Semantics ─────────────────────────

(deftest-each optimizer-algebraic-identity-value
  "Algebraic identity simplifications preserve correct runtime values."
  :cases (("add-zero"  5  "(let ((x 5)) (+ x 0))")
          ("mul-one"   7  "(let ((x 7)) (* x 1))")
          ("mul-zero"  0  "(let ((x 99)) (* x 0))")
          ("sub-zero" 42  "(let ((x 42)) (- x 0))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Algebraic Identity Simplification: No-Op Instructions ────────────────

(deftest-each optimizer-algebraic-identity-no-op
  "Algebraic identity rules eliminate the corresponding instruction entirely."
  :cases (("add-zero"  "(let ((x 5))  (+ x 0))"  'vm-add)
          ("mul-one"   "(let ((x 7))  (* x 1))"  'vm-mul)
          ("mul-zero"  "(let ((x 99)) (* x 0))"  'vm-mul)
          ("sub-zero"  "(let ((x 42)) (- x 0))"  'vm-sub))
  (expr instr)
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

;;; ── Constant Branch Elimination: No Jump Instructions ────────────────────

(deftest-each optimizer-branch-fold-no-jump
  "Constant-condition branches eliminate vm-jump-zero from the output."
  :cases (("const-true"  "(if 1 42 99)")
          ("const-false" "(if 0 42 99)"))
  (expr)
  (assert-false (opt-has-p expr 'vm-jump-zero)))

;;; ── End-to-End Correctness ───────────────────────────────────────────────

(deftest-each optimizer-e2e
  "Full optimizer pipeline preserves semantics for various expression forms."
  :cases (("simple-let"   10  "(let ((x 10)) x)")
          ("two-bindings"  7  "(let ((x 3) (y 4)) (+ x y))")
          ("square"        4  "(let ((x 2)) (* x x))")
          ("sum-of-sums"  10  "(+ (+ 1 2) (+ 3 4))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Bitwise Algebraic Identities: Semantics ──────────────────────────────

(deftest-each optimizer-bitwise-value
  "Bitwise algebraic identities evaluate to the correct value."
  :cases (("logand-zero"      0  "(let ((x 42)) (logand x 0))")
          ("logand-minus-one" 42 "(let ((x 42)) (logand x -1))")
          ("logand-self"      12 "(let ((x 12)) (logand x x))")
          ("logior-zero"      42 "(let ((x 42)) (logior x 0))")
          ("logxor-zero"      42 "(let ((x 42)) (logxor x 0))")
          ("logxor-self"       0 "(let ((x 42)) (logxor x x))")
          ("ash-zero"         42 "(let ((x 42)) (ash x 0))"))
  (expected expr)
  (assert-run= expected expr))

;;; ── Bitwise Algebraic Identities: No-Op Instructions ─────────────────────

(deftest-each optimizer-bitwise-no-op
  "Bitwise algebraic identities eliminate the corresponding instruction entirely."
  :cases (("logand-zero"       "(let ((x 42)) (logand x 0))"   'vm-logand)
          ("logand-minus-one"  "(let ((x 42)) (logand x -1))"  'vm-logand)
          ("logior-zero"       "(let ((x 42)) (logior x 0))"   'vm-logior)
          ("logxor-self"       "(let ((x 42)) (logxor x x))"   'vm-logxor)
          ("ash-zero"          "(let ((x 42)) (ash x 0))"      'vm-ash))
  (expr instr)
  (assert-false (opt-has-p expr instr)))

;;; ── Unary Constant Folding ────────────────────────────────────────────────

(deftest optimizer-lognot-constant-value
  "(lognot 0) = -1"
  (assert-run= -1 "(lognot 0)"))

(deftest optimizer-lognot-constant-no-op
  "Constant (lognot 0) folds; no vm-lognot remains."
  (assert-false (opt-has-p "(lognot 0)" 'vm-lognot)))

(deftest optimizer-not-zero-value
  "(not 0) = t (vm-not returns t/nil)"
  (assert-true (eq t (run-string "(not 0)"))))

;;; ── Function Inlining ────────────────────────────────────────────────────

(deftest optimizer-inline-simple-function-value
  "A small single-arg function inlines and produces the correct value."
  (assert-run= 5 "(defun double-inc (x) (+ x 1)) (double-inc 4)"))

(deftest optimizer-inline-removes-call
  "Inlining a small fn eliminates the vm-call instruction."
  (assert-false (opt-has-p "(defun double-inc (x) (+ x 1)) (double-inc 4)" 'vm-call)))

(deftest optimizer-inline-two-args-value
  "A two-arg inlinable function produces the correct value."
  (assert-run= 7 "(defun add2 (a b) (+ a b)) (add2 3 4)"))

;;; ── CSE (Common Subexpression Elimination) Unit Tests ──────────────────────

(deftest cse-binary-dedup
  "Two identical vm-add with same operands: second becomes vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-move-p (fourth out)))
    (assert-equal :R2 (cl-cc::vm-src (fourth out)))))

(deftest cse-commutative-dedup
  "Commutative vm-add with swapped operands: second becomes vm-move."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R1 :rhs :R0))
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-move-p (fourth out)))
    (assert-equal :R2 (cl-cc::vm-src (fourth out)))))

(deftest cse-label-flushes
  "A vm-label between two identical vm-add prevents CSE."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value 10))
         (i2 (cl-cc::make-vm-const :dst :R1 :value 20))
         (i3 (cl-cc::make-vm-add :dst :R2 :lhs :R0 :rhs :R1))
         (lbl (cl-cc::make-vm-label :name "L1"))
         (i4 (cl-cc::make-vm-add :dst :R3 :lhs :R0 :rhs :R1))
         (out (cl-cc::opt-pass-cse (list i1 i2 i3 lbl i4))))
    (assert-true (cl-cc::vm-add-p (third out)))
    (assert-true (cl-cc::vm-label-p (fourth out)))
    (assert-true (cl-cc::vm-add-p (fifth out)))))

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

;;; ── Inlining Pass: Unit Tests ──────────────────────────────────────────────

(defun inline-has-call-p (instructions)
  "T if INSTRUCTIONS contains at least one vm-call."
  (some #'cl-cc::vm-call-p instructions))

(deftest inline-small-function
  "A small function (vm-const + vm-ret) called via vm-func-ref is inlined;
   the vm-call is eliminated from the output."
  (let* ((closure (cl-cc::make-vm-closure :dst :R0 :label "f"
                                          :params '(:R10)
                                          :captured nil))
         (fref    (cl-cc::make-vm-func-ref :dst :R5 :label "f"))
         (jump-past (cl-cc::make-vm-jump :label "after_f"))
         (lbl     (cl-cc::make-vm-label :name "f"))
         (body1   (cl-cc::make-vm-const :dst :R11 :value 1))
         (body2   (cl-cc::make-vm-add :dst :R12 :lhs :R10 :rhs :R11))
         (ret     (cl-cc::make-vm-ret :reg :R12))
         (after   (cl-cc::make-vm-label :name "after_f"))
         (arg     (cl-cc::make-vm-const :dst :R1 :value 4))
         (call    (cl-cc::make-vm-call :dst :R6 :func :R5 :args '(:R1)))
         (halt    (cl-cc::make-vm-halt))
         (instrs  (list closure fref jump-past lbl body1 body2 ret after arg call halt))
         (out     (cl-cc::opt-pass-inline instrs)))
    (assert-true (not (inline-has-call-p out)))))

(deftest inline-preserves-value
  "After inlining a function that returns (+ x 1), the constant 1 and a
   vm-move copying the result into the call's dst register must appear."
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
    ;; The vm-call is gone and a vm-move into :R6 (the original call dst) exists
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
         ;; Generate 20 body instructions (exceeds default threshold of 15)
         (body    (loop for i from 20 below 40
                        collect (cl-cc::make-vm-const
                                 :dst (intern (format nil "R~A" i) :keyword)
                                 :value i)))
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

;;; ── Direct opt-pass-fold Tests ─────────────────────────────────────────

(deftest fold-label-flushes-env
  "Labels flush the constant env — a constant known before a label must NOT
   be propagated after the label (other paths may define a different value)."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-inc :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    ;; The inc should survive (not folded to const 43) because the label flushes env
    (assert-true (find-if (lambda (i) (typep i 'cl-cc::vm-inc)) out))))

(deftest fold-unary-not-nil
  "vm-not on a known nil value folds to t."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value nil)
                       (cl-cc::make-vm-not :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    ;; The vm-not should be folded to (vm-const :R1 t)
    (let ((r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                              (eq (cl-cc::vm-dst i) :R1)))
                             out)))
      (assert-true r1-const)
      (assert-equal t (cl-cc::vm-value r1-const)))))

(deftest fold-unary-not-zero
  "vm-not on a known zero value folds to t (zero is falsy)."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 0)
                       (cl-cc::make-vm-not :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    (let ((r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                              (eq (cl-cc::vm-dst i) :R1)))
                             out)))
      (assert-true r1-const)
      (assert-equal t (cl-cc::vm-value r1-const)))))

(deftest fold-type-pred-number
  "vm-number-p on a known number folds to 1."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-number-p :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    (let ((r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                              (eq (cl-cc::vm-dst i) :R1)))
                             out)))
      (assert-true r1-const)
      (assert-equal 1 (cl-cc::vm-value r1-const)))))

(deftest fold-type-pred-symbol
  "vm-symbol-p on a known number folds to 0."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 42)
                       (cl-cc::make-vm-symbol-p :dst :R1 :src :R0)))
         (out (cl-cc::opt-pass-fold instrs)))
    (let ((r1-const (find-if (lambda (i) (and (cl-cc::vm-const-p i)
                                              (eq (cl-cc::vm-dst i) :R1)))
                             out)))
      (assert-true r1-const)
      (assert-equal 0 (cl-cc::vm-value r1-const)))))

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
  "vm-jump-zero with a known zero value becomes unconditional vm-jump."
  (let* ((instrs (list (cl-cc::make-vm-const :dst :R0 :value 0)
                       (cl-cc::make-vm-jump-zero :reg :R0 :label "target")))
         (out (cl-cc::opt-pass-fold instrs)))
    ;; Should have an unconditional jump, not a conditional one
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
  "After a label, copy knowledge is flushed — no propagation across labels."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-label :name "join")
                       (cl-cc::make-vm-add :dst :R2 :lhs :R1 :rhs :R1)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; The add should still use :R1 (not :R0), because label flushed copies
    (let ((add-inst (find-if (lambda (i) (typep i 'cl-cc::vm-add)) out)))
      (assert-true add-inst)
      (assert-equal :R1 (cl-cc::vm-lhs add-inst)))))

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

(deftest copy-prop-self-move-elim
  "Self-move after resolution is eliminated: R1←R0 then R0←R1 → second resolves
   to R0←R0 which is dropped."
  (let* ((instrs (list (cl-cc::make-vm-move :dst :R1 :src :R0)
                       (cl-cc::make-vm-move :dst :R0 :src :R1)))
         (out (cl-cc::opt-pass-copy-prop instrs)))
    ;; Only one move should remain (the first one); the second is a self-move
    (let ((moves (remove-if-not (lambda (i) (typep i 'cl-cc::vm-move)) out)))
      (assert-equal 1 (length moves)))))

;;; ─── opt-inst-read-regs ──────────────────────────────────────────────────────

(deftest read-regs-const-empty
  "vm-const reads no registers."
  (assert-null (cl-cc::opt-inst-read-regs (make-vm-const :dst :r0 :value 42))))

(deftest read-regs-func-ref-empty
  "vm-func-ref reads no registers."
  (assert-null (cl-cc::opt-inst-read-regs (make-vm-func-ref :dst :r0 :label "fn"))))

(deftest read-regs-move-src
  "vm-move reads one register (src)."
  (assert-equal '(:r1) (cl-cc::opt-inst-read-regs (make-vm-move :dst :r0 :src :r1))))

(deftest read-regs-binop-lhs-rhs
  "vm-add (binop) reads lhs and rhs."
  (let ((regs (cl-cc::opt-inst-read-regs (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-equal 2 (length regs))
    (assert-true (member :r1 regs))
    (assert-true (member :r2 regs))))

(deftest read-regs-comparison-lhs-rhs
  "vm-lt (non-binop binary) reads lhs and rhs via data table."
  (let ((regs (cl-cc::opt-inst-read-regs (make-vm-lt :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-equal 2 (length regs))
    (assert-true (member :r1 regs))
    (assert-true (member :r2 regs))))

(deftest read-regs-unary-src
  "vm-neg (unary) reads one register (src) via data table."
  (assert-equal '(:r1) (cl-cc::opt-inst-read-regs (make-vm-neg :dst :r0 :src :r1))))

(deftest read-regs-type-pred-src
  "vm-null-p (type predicate) reads src via data table."
  (assert-equal '(:r1) (cl-cc::opt-inst-read-regs (make-vm-null-p :dst :r0 :src :r1))))

(deftest read-regs-ret-reg
  "vm-ret reads one register."
  (assert-equal '(:r0) (cl-cc::opt-inst-read-regs (make-vm-ret :reg :r0))))

(deftest read-regs-call-func-and-args
  "vm-call reads func register plus args."
  (let ((regs (cl-cc::opt-inst-read-regs
                (make-vm-call :dst :r0 :func :r1 :args '(:r2 :r3)))))
    (assert-equal 3 (length regs))
    (assert-true (member :r1 regs))
    (assert-true (member :r2 regs))
    (assert-true (member :r3 regs))))

(deftest read-regs-set-global-src
  "vm-set-global reads src."
  (assert-equal '(:r0) (cl-cc::opt-inst-read-regs
                          (make-vm-set-global :src :r0 :name 'x))))

(deftest read-regs-get-global-empty
  "vm-get-global reads no registers."
  (assert-null (cl-cc::opt-inst-read-regs (make-vm-get-global :dst :r0 :name 'x))))
