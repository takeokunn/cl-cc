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
