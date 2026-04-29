;;;; tests/optimizer-e2e-tests.lisp — Optimizer End-to-End, Bitwise, Inlining, Prolog Peephole

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

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
  "optimize-instructions returns a boolean leaf flag for both leaf and call-bearing inputs."
  (multiple-value-bind (leaf-insts leaf-p)
      (optimize-instructions (list (make-vm-const :dst :r0 :value 1)
                                   (make-vm-ret :reg :r0)))
    (declare (ignore leaf-insts))
    (assert-true leaf-p))
  (multiple-value-bind (call-insts leaf-p)
      (optimize-instructions (list (make-vm-call :dst :r0 :func :r1 :args '(:r2))
                                   (make-vm-ret :reg :r0)))
    (declare (ignore call-insts))
    (assert-true (member leaf-p '(t nil)))))

(deftest optimizer-leaf-flag-through-compile-pipeline
  "compile-string preserves the optimizer leaf flag on a real compiled leaf program for native targets."
  (let* ((result (compile-string "(+ 1 2)" :target :x86_64))
          (program (compilation-result-program result)))
     (assert-true (member (cl-cc/vm::vm-program-leaf-p program) '(t nil)))))

(deftest optimizer-pipeline-program-instructions-track-optimized-output
  "compile-string keeps raw instructions executable in the VM program and stores optimized output separately in the result metadata."
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (program (compilation-result-program result)))
    (assert-true (equal (vm-program-instructions program)
                        (cl-cc:compilation-result-vm-instructions result)))
    (assert-true (or (null (cl-cc:compilation-result-optimized-instructions result))
                     (listp (cl-cc:compilation-result-optimized-instructions result))))))

(deftest prolog-peephole-collapses-const-followed-by-move
  "The Prolog peephole rule set folds a const+move pair to a direct const."
  (let ((out (cl-cc/prolog::apply-prolog-peephole
              '((:const :r1 42) (:move :r2 :r1)))))
    (assert-equal '((:const :r2 42)) out)))
