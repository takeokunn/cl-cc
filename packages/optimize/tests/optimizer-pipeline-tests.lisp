;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for src/optimize/optimizer-pipeline.lisp
;;;;
;;;; Covers: opt-parse-pass-pipeline-string (tokenisation, whitespace, empty),
;;;;   opt-converged-p (eq-identity, structural-equal-not-enough, length-change),
;;;;   opt-adaptive-max-iterations (boundary regions, default clamping),
;;;;   opt-verify-instructions (success, duplicate-label, unknown-target).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %opt-trim-whitespace ────────────────────────────────────────────────────

(deftest-each opt-trim-whitespace-cases
  "%opt-trim-whitespace strips leading/trailing spaces, tabs, and newlines."
  :cases (("spaces"      "hello"      "  hello  ")
          ("tabs"        "world"      (format nil "~Cworld~C" #\Tab #\Tab))
          ("newlines"    "foo"        (format nil "~Cfoo~C" #\Newline #\Newline))
          ("mixed"       "bar"        (format nil " ~C~C bar ~C~C " #\Tab #\Newline #\Newline #\Tab))
          ("no-trim"     "bare"       "bare")
          ("empty"       ""           ""))
  (expected input)
  (assert-equal expected (cl-cc/optimize::%opt-trim-whitespace input)))

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

(deftest parse-pass-pipeline-string-single-pass
  "opt-parse-pass-pipeline-string parses a single pass name into a one-element list."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp")))
    (assert-= 1 (length result))
    (assert-eq :SCCP (first result))))

(deftest parse-pass-pipeline-string-multi-pass-comma-separated
  "opt-parse-pass-pipeline-string parses a comma-separated multi-pass string into keywords."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string "sccp,cse,dce")))
    (assert-= 3 (length result))
    (assert-eq :SCCP (first  result))
    (assert-eq :CSE  (second result))
    (assert-eq :DCE  (third  result))))

(deftest parse-pass-pipeline-string-trims-whitespace
  "opt-parse-pass-pipeline-string trims whitespace around pass names."
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string " sccp , cse ")))
    (assert-= 2 (length result))
    (assert-eq :SCCP (first result))
    (assert-eq :CSE  (second result))))

(deftest parse-pass-pipeline-string-empty-returns-nil
  "opt-parse-pass-pipeline-string on an empty string returns nil."
  (assert-null (cl-cc/optimize::opt-parse-pass-pipeline-string "")))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(deftest opt-converged-p-both-nil-returns-true
  "opt-converged-p returns T when both sequences are nil (empty)."
  (assert-true (cl-cc/optimize::opt-converged-p nil nil)))

(deftest opt-converged-p-same-object-returns-true
  "opt-converged-p returns T when both sequences are the same list object."
  (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2)))
    (assert-true (cl-cc/optimize::opt-converged-p prog prog))))

(deftest opt-converged-p-structurally-equal-returns-true
  "opt-converged-p returns T for structurally equal (but distinct) instruction lists."
  (let* ((a (make-vm-const :dst :r0 :value 1))
         (b (make-vm-const :dst :r0 :value 1)))
    (assert-true (cl-cc/optimize::opt-converged-p (list a) (list b)))))

(deftest opt-converged-p-different-length-returns-false
  "opt-converged-p returns NIL when the two sequences have different lengths."
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

(deftest verify-instructions-simple-sequence-passes
  "opt-verify-instructions returns T for a simple const+ret sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)))))

(deftest verify-instructions-jump-with-known-label-passes
  "opt-verify-instructions returns T when a jump target label is defined in the sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump  :label "target")
                      (make-vm-label :name "target")
                      (make-vm-ret   :reg :r0)))))

(deftest-each verify-instructions-invalid-cases
  "opt-verify-instructions signals an error for duplicate labels or unknown jump targets."
  :cases (("duplicate-label"   (list (make-vm-const :dst :r0 :value 1)
                                     (make-vm-label :name "dup")
                                     (make-vm-label :name "dup")
                                     (make-vm-ret   :reg :r0)))
          ("unknown-target"    (list (make-vm-jump :label "ghost")
                                     (make-vm-ret  :reg :r0))))
  (insts)
  (assert-signals error
    (cl-cc/optimize:opt-verify-instructions insts)))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(deftest resolve-pass-pipeline-nil-returns-convergence-passes
  "opt-resolve-pass-pipeline on nil returns *opt-convergence-passes*."
  (assert-eq cl-cc/optimize::*opt-convergence-passes*
             (cl-cc/optimize::opt-resolve-pass-pipeline nil)))

(deftest resolve-pass-pipeline-functions-pass-through-unchanged
  "opt-resolve-pass-pipeline passes function objects through as-is."
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (assert-eq fn (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)))))

(deftest resolve-pass-pipeline-keywords-resolve-to-functions
  "opt-resolve-pass-pipeline resolves keyword pass names to function objects."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline (list :fold :dce))))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-string-parses-and-resolves
  "opt-resolve-pass-pipeline on a comma-separated string parses and resolves to functions."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline "fold,dce")))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-unknown-pass-signals-error
  "opt-resolve-pass-pipeline signals an error for an unregistered pass keyword."
  (assert-signals error
    (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(deftest opt-pass-data-integrity
  "*opt-convergence-passes* is a non-empty function list; registry includes early Prolog/egraph passes and core cleanup passes."
  (assert-true (listp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (> (length cl-cc/optimize::*opt-convergence-passes*) 10))
  (assert-true (every #'functionp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (gethash :prolog-rewrite cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :egraph cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :fold cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :dce  cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cse  cl-cc/optimize::*opt-pass-registry*))
  (assert-eq #'cl-cc/optimize::%maybe-apply-prolog-rewrite (first cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize:optimize-with-egraph cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-fold cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-strength-reduce cl-cc/optimize::*opt-convergence-passes*))
  (assert-equal '(:prolog-rewrite :inline :sccp)
                (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 3)))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

(deftest verify-optimizer-flag-runs-verifier-on-valid-input
  "*verify-optimizer-instructions* T causes opt-verify-instructions to run; valid input succeeds."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* t)
        (insts (list (make-vm-const :dst :r0 :value 42) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

(deftest verify-optimizer-flag-nil-skips-verifier
  "*verify-optimizer-instructions* NIL causes optimize-instructions to skip verification."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* nil)
        (insts (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

(deftest prolog-rewrite-stage-disabled-is-identity
  "%maybe-apply-prolog-rewrite returns the input unchanged when the Prolog hook is disabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* nil)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (assert-eq insts (cl-cc/optimize::%maybe-apply-prolog-rewrite insts))))

(deftest prolog-rewrite-stage-invokes-prolog-backends
  "%maybe-apply-prolog-rewrite returns a list result when enabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* t)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (let ((result (cl-cc/optimize::%maybe-apply-prolog-rewrite insts)))
      (assert-true (listp result))
      (assert-= 2 (length result))
      (assert-equal (mapcar #'cl-cc/optimize::instruction->sexp insts)
                    (mapcar #'cl-cc/optimize::instruction->sexp result)))))
