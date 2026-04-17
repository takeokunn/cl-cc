;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-pipeline.lisp
;;;;
;;;; Covers: opt-parse-pass-pipeline-string (tokenisation, whitespace, empty),
;;;;   opt-converged-p (eq-identity, structural-equal-not-enough, length-change),
;;;;   opt-adaptive-max-iterations (boundary regions, default clamping),
;;;;   opt-verify-instructions (success, duplicate-label, unknown-target).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

(deftest parse-pass-pipeline-string-single-pass
  "opt-parse-pass-pipeline-string parses a single pass name to a keyword."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp")))
    (assert-= 1 (length result))
    (assert-eq :SCCP (first result))))

(deftest parse-pass-pipeline-string-multiple-passes
  "opt-parse-pass-pipeline-string splits comma-separated pass names."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp,cse,dce")))
    (assert-= 3 (length result))
    (assert-eq :SCCP (first  result))
    (assert-eq :CSE  (second result))
    (assert-eq :DCE  (third  result))))

(deftest parse-pass-pipeline-string-trims-whitespace
  "opt-parse-pass-pipeline-string strips leading/trailing whitespace from each name."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string " sccp , cse ")))
    (assert-= 2 (length result))
    (assert-eq :SCCP (first result))
    (assert-eq :CSE  (second result))))

(deftest parse-pass-pipeline-string-empty-string-returns-nil
  "opt-parse-pass-pipeline-string on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-parse-pass-pipeline-string "")))

(deftest parse-pass-pipeline-string-upcases-names
  "opt-parse-pass-pipeline-string produces upper-case keywords."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp")))
    (assert-eq :SCCP (first result))))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(deftest opt-converged-p-empty-lists
  "opt-converged-p returns T for two empty lists."
  (assert-true (cl-cc/optimize::opt-converged-p nil nil)))

(deftest opt-converged-p-same-objects
  "opt-converged-p returns T when both lists contain the same instruction objects."
  (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2)))
    (assert-true (cl-cc/optimize::opt-converged-p prog prog))))

(deftest opt-converged-p-different-objects-same-value
  "opt-converged-p returns NIL for structurally equal but distinct instruction objects."
  ;; eq is object identity, not structural equality
  (let* ((a (make-vm-const :dst :r0 :value 1))
         (b (make-vm-const :dst :r0 :value 1)))  ; distinct but equal
    (assert-false (cl-cc/optimize::opt-converged-p (list a) (list b)))))

(deftest opt-converged-p-different-lengths
  "opt-converged-p returns NIL when the instruction lists have different lengths."
  (let* ((i (make-vm-const :dst :r0 :value 1)))
    (assert-false (cl-cc/optimize::opt-converged-p (list i) (list i i)))))

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

(deftest-each adaptive-max-iterations-regions
  "opt-adaptive-max-iterations returns the correct budget for each size region."
  :cases (("tiny"   20  8)   ; < 50 insts: base-20 + (-12) = 8 (clamped to min 6)
          ("small"  100 14)  ; 50-149: base-20 + (-6) = 14
          ("medium" 200 20)  ; 150-399: base-20 + 0 = 20
          ("large"  500 28)  ; 400-799: base-20 + 8 = 28
          ("huge"   1000 35)) ; >= 800: base-20 + 15 = 35
  (n-insts expected)
  (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= expected (cl-cc/optimize::opt-adaptive-max-iterations insts))))

(deftest adaptive-max-iterations-respects-max-cap
  "opt-adaptive-max-iterations never exceeds :max-iterations.
For n=2000 insts the natural computed value is base(20) + 15 = 35; pass a
max-iterations of 30 to actually exercise the cap clamping (35 → 30)."
  (let ((insts (make-list 2000 :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= 30 (cl-cc/optimize::opt-adaptive-max-iterations insts :max-iterations 30))))

(deftest adaptive-max-iterations-respects-min-floor
  "opt-adaptive-max-iterations never returns less than :min-iterations."
  (let ((insts nil))  ; empty → smallest budget
    (assert-true (>= (cl-cc/optimize::opt-adaptive-max-iterations insts :min-iterations 6) 6))))

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

(deftest verify-instructions-returns-t-for-valid-code
  "opt-verify-instructions returns T for a simple valid instruction sequence."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0))))
    (assert-true (cl-cc/optimize::opt-verify-instructions insts))))

(deftest verify-instructions-with-jump-to-known-label
  "opt-verify-instructions passes when a jump targets a known label."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump  :label "target")
                      (make-vm-label :name "target")
                      (make-vm-ret   :reg :r0))))
    (assert-true (cl-cc/optimize::opt-verify-instructions insts))))

(deftest verify-instructions-signals-on-duplicate-label
  "opt-verify-instructions signals an error when a label name is duplicated."
  ;; Pass 1 (label collection) catches duplicates before use-before-def checks.
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-label :name "dup")
                      (make-vm-label :name "dup")  ; duplicate!
                      (make-vm-ret   :reg :r0))))
    (assert-signals error
      (cl-cc/optimize::opt-verify-instructions insts))))

(deftest verify-instructions-signals-on-unknown-jump-target
  "opt-verify-instructions signals an error when a jump has no matching label."
  (let* ((insts (list (make-vm-jump :label "ghost")
                      (make-vm-ret  :reg :r0))))
    (assert-signals error
      (cl-cc/optimize::opt-verify-instructions insts))))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(deftest resolve-pass-pipeline-nil-returns-convergence-passes
  "opt-resolve-pass-pipeline with nil returns *opt-convergence-passes*."
  (assert-eq cl-cc/optimize::*opt-convergence-passes*
             (cl-cc/optimize::opt-resolve-pass-pipeline nil)))

(deftest resolve-pass-pipeline-function-list-is-identity
  "opt-resolve-pass-pipeline with a list of functions returns it unchanged."
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (assert-eq fn (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)))))

(deftest resolve-pass-pipeline-keyword-list-resolves-from-registry
  "opt-resolve-pass-pipeline resolves keyword names via *opt-pass-registry*."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline (list :fold :dce))))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-string-parses-and-resolves
  "opt-resolve-pass-pipeline accepts a comma-separated string of pass names."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline "fold,dce")))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-unknown-keyword-signals-error
  "opt-resolve-pass-pipeline signals an error for an unrecognized keyword."
  (assert-signals error
    (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(deftest opt-convergence-passes-is-non-empty-function-list
  "*opt-convergence-passes* is a non-empty list of functions."
  (assert-true (listp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (> (length cl-cc/optimize::*opt-convergence-passes*) 10))
  (assert-true (every #'functionp cl-cc/optimize::*opt-convergence-passes*)))

(deftest opt-pass-registry-contains-fold-dce-cse
  "*opt-pass-registry* has entries for the core passes :fold, :dce, :cse."
  (assert-true (gethash :fold cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :dce  cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cse  cl-cc/optimize::*opt-pass-registry*)))
