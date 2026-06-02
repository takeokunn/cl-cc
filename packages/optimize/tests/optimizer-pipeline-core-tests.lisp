;;;; tests/unit/optimize/optimizer-pipeline-core-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism (core)
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage,
;;;;   COW helpers, bump/slab allocators, inline cache layer.

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

(deftest-each parse-pass-pipeline-string-cases
  "opt-parse-pass-pipeline-string correctly tokenises pass name strings."
  :cases (("single-pass"              "sccp"         1 :SCCP nil  nil)
          ("multi-pass-comma-separated" "sccp,cse,dce" 3 :SCCP :CSE :DCE)
          ("trims-whitespace"         " sccp , cse " 2 :SCCP :CSE nil)
          ("empty-returns-nil"        ""             0 nil   nil  nil))
  (input expected-len first-kw second-kw third-kw)
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (assert-null result)
        (progn
          (assert-= expected-len (length result))
          (when first-kw  (assert-eq first-kw  (first  result)))
          (when second-kw (assert-eq second-kw (second result)))
          (when third-kw  (assert-eq third-kw  (third  result)))))))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(deftest-each opt-converged-p-cases
  "opt-converged-p correctly detects convergence across nil, same-object, structural-equal, and length-mismatch cases."
  :cases (("both-nil"           :nil-case   t)
          ("same-object"        :same-case  t)
          ("structurally-equal" :equal-case t)
          ("different-length"   :diff-case  nil))
  (scenario expected-result)
  (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (assert-true result)
          (assert-false result)))))

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

(deftest adaptive-loop-unroll-factor-reacts-to-hotness
  "opt-adaptive-loop-unroll-factor raises loop budgets for hot functions."
  (multiple-value-bind (cold-factor cold-trip)
      (cl-cc/optimize::opt-adaptive-loop-unroll-factor nil :call-count 0)
    (multiple-value-bind (hot-factor hot-trip)
        (cl-cc/optimize::opt-adaptive-loop-unroll-factor nil :call-count 100)
      (assert-true (> hot-factor cold-factor))
      (assert-true (> hot-trip cold-trip)))))

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

(deftest-each verify-instructions-valid-cases
  "opt-verify-instructions returns T for well-formed instruction sequences."
  :cases (("simple-sequence"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret   :reg :r0)))
          ("jump-with-known-label"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-jump  :label "target")
                 (make-vm-label :name "target")
                 (make-vm-ret   :reg :r0))))
  (insts)
  (assert-true (cl-cc/optimize:opt-verify-instructions insts)))

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

(deftest-each resolve-pass-pipeline-multi-pass-input-forms
  "opt-resolve-pass-pipeline accepts keyword lists and comma-separated strings, yielding function lists."
  :cases (("keywords" (list :fold :dce))
          ("string"   "fold,dce"))
  (input)
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline input)))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-unknown-pass-signals-error
  "opt-resolve-pass-pipeline signals an error for an unregistered pass keyword."
  (assert-signals error
    (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

;;; Three focused tests replace the former monolithic opt-pass-data-integrity:
;;;   1. structural list invariants
;;;   2. registry key presence (deftest-each — one case per pass)
;;;   3. pipeline ordering invariants (deftest-each — one case per constraint)

(deftest opt-convergence-passes-list-structure
  "*opt-convergence-passes* is a non-empty function list with Prolog rewriter first."
  (assert-true (listp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (> (length cl-cc/optimize::*opt-convergence-passes*) 10))
  (assert-true (every #'functionp cl-cc/optimize::*opt-convergence-passes*))
  (assert-eq #'cl-cc/optimize::%maybe-apply-prolog-rewrite
             (first cl-cc/optimize::*opt-convergence-passes*)))

(deftest-each opt-convergence-passes-raw-fns-absent
  "Raw optimizer pass functions must not appear directly in *opt-convergence-passes*."
  :cases (("optimize-with-egraph"     #'cl-cc/optimize:optimize-with-egraph)
          ("opt-pass-fold"            #'cl-cc/optimize::opt-pass-fold)
          ("opt-pass-strength-reduce" #'cl-cc/optimize::opt-pass-strength-reduce))
  (fn)
  (assert-false (member fn cl-cc/optimize::*opt-convergence-passes*)))

(deftest opt-convergence-keys-first-six-fixed
  "The first 6 keys of *opt-default-convergence-pass-keys* are fixed and ordered."
  (assert-equal '(:prolog-rewrite :call-site-splitting :devirtualize :if-conversion
                  :closure-capture-dedup :closure-thunk-sharing)
                (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 6)))

(deftest-each opt-convergence-keys-positional-slots
  "Keys 7–10 of *opt-default-convergence-pass-keys* occupy specific positions."
  :cases (("7th-inline"             7  :inline)
          ("8th-overflow-check"     8  :overflow-check-elim)
          ("9th-sccp"               9  :sccp)
          ("10th-cons-slot-forward" 10 :cons-slot-forward))
  (n expected)
  (assert-eq expected (nth (1- n) cl-cc/optimize::*opt-default-convergence-pass-keys*)))

(deftest-each opt-pass-registry-required-keys-present
  "Each core optimizer pass key is registered in *opt-pass-registry*."
  :cases (("prolog-rewrite"         :prolog-rewrite)
          ("egraph"                 :egraph)
          ("fold"                   :fold)
          ("cons-slot-forward"      :cons-slot-forward)
          ("pure-call-optimization" :pure-call-optimization)
          ("dce"                    :dce)
          ("cse"                    :cse)
          ("if-conversion"          :if-conversion)
          ("fma-recognition"        :fma-recognition))
  (key)
  (assert-true (gethash key cl-cc/optimize::*opt-pass-registry*)))

(deftest-each opt-pass-pipeline-ordering-invariants
  "Key passes appear in the correct relative order in *opt-default-convergence-pass-keys*."
  :cases (("devirt-before-if-conv"      :devirtualize           :if-conversion)
          ("if-conv-before-inline"      :if-conversion          :inline)
          ("copy-prop-before-pure"      :copy-prop              :pure-call-optimization)
          ("pure-before-gvn"           :pure-call-optimization :gvn)
          ("pure-before-cse"           :pure-call-optimization :cse)
          ("pure-before-dce"           :pure-call-optimization :dce)
          ("fma-before-schedule-local" :fma-recognition        :schedule-local))
  (earlier later)
  (let ((keys cl-cc/optimize::*opt-default-convergence-pass-keys*))
    (assert-true (< (position earlier keys) (position later keys)))))

;;; ─── FR-099 FMA recognition ───────────────────────────────────────────────

(defun %test-count-type (type insts)
  (count-if (lambda (inst) (typep inst type)) insts))

(deftest-each fr-099-fma-recognition-cases
  "FR-099: opt-pass-fma-recognition fuses/rejects float mul+add patterns as expected."
  :cases (("mul-plus-accumulator"
           (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2))
           1 0 0)
          ("commuted-add"
           (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r2 :rhs :r3))
           1 0 0)
          ("multiple-consumers-no-fuse"
           (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2)
                 (cl-cc/vm::make-vm-float-add :dst :r5 :lhs :r3 :rhs :r6))
           0 1 2)
          ("integer-arithmetic-no-fuse"
           (list (make-vm-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-add :dst :r4 :lhs :r3 :rhs :r2))
           0 nil nil)
          ("cross-block-boundary-no-fuse"
           (list (cl-cc/vm::make-vm-float-mul :dst :r3 :lhs :r0 :rhs :r1)
                 (make-vm-label :name "next")
                 (cl-cc/vm::make-vm-float-add :dst :r4 :lhs :r3 :rhs :r2))
           0 1 1))
  (insts expected-fma expected-float-mul-or-mul expected-float-add-or-add)
  (let ((out (cl-cc/optimize::opt-pass-fma-recognition insts)))
    (assert-= expected-fma (%test-count-type 'cl-cc/vm::vm-fma out))
    (when expected-float-mul-or-mul
      (let ((float-mul-count (%test-count-type 'cl-cc/vm::vm-float-mul out))
            (int-mul-count   (%test-count-type 'cl-cc/vm::vm-mul out)))
        (assert-= expected-float-mul-or-mul (+ float-mul-count int-mul-count))))
    (when expected-float-add-or-add
      (let ((float-add-count (%test-count-type 'cl-cc/vm::vm-float-add out))
            (int-add-count   (%test-count-type 'cl-cc/vm::vm-add out)))
        (assert-= expected-float-add-or-add (+ float-add-count int-add-count))))))

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

(defun %make-pure-square-insts ()
  "Build a small program where a closure squares its argument — used to test pure-call optimization."
  (let ((label "pure-square"))
    (list (make-vm-closure :dst :r9 :label label :params '(:r0) :captured nil)
          (make-vm-label   :name label)
          (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
          (make-vm-ret     :reg :r1)
          (make-vm-closure :dst :r2 :label label :params '(:r0) :captured nil)
          (make-vm-const   :dst :r0 :value 7)
          (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
          (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
          (make-vm-ret     :reg :r4))))

(deftest pure-call-policy-gate-disables-pass
  "*opt-enable-pure-call-optimization* NIL disables :pure-call-optimization pass execution."
  (let* ((insts (%make-pure-square-insts))
         (cl-cc/optimize::*opt-enable-pure-call-optimization* nil)
         (optimized (cl-cc/optimize:optimize-instructions
                     insts
                     :max-iterations 1
                     :pass-pipeline '(:pure-call-optimization))))
    (assert-= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) optimized))
    (assert-false
     (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-move)
                  (eq (cl-cc/vm::vm-dst inst) :r4)
                  (eq (cl-cc/vm::vm-src inst) :r3)))
           optimized))))

(deftest optimize-policy-config-speed-threshold
  "opt-configure-optimization-policy toggles pure-call gate at speed >= 3."
  (let ((cl-cc/optimize::*opt-enable-pure-call-optimization* t))
    (assert-false (cl-cc/optimize:opt-configure-optimization-policy :speed 2))
    (assert-true  (cl-cc/optimize:opt-configure-optimization-policy :speed 3))))

(deftest optimize-instructions-speed-keyword-controls-pure-call-pass
  "optimize-instructions :speed drives pure-call optimization gate behavior."
  (let* ((insts (%make-pure-square-insts))
         (slow (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 2 :pass-pipeline '(:pure-call-optimization)))
         (fast (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 3 :pass-pipeline '(:pure-call-optimization))))
    (assert-= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) slow))
    (assert-= 1 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) fast))))

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

;;; ─── Copy-on-Write helper layer (FR-253 partial) ───────────────────────────

(deftest optimize-cow-copy-is-constant-time-share
  "opt-cow-copy increments refcount without duplicating payload immediately."
  (let* ((cow (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy cow)))
    (assert-eq cow shared)
    (assert-= 2 (cl-cc/optimize:opt-cow-object-refcount cow))))

(deftest optimize-cow-write-detaches-when-shared
  "opt-cow-write detaches payload when refcount > 1 and preserves original payload."
  (let* ((original (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy original))
         (written (cl-cc/optimize:opt-cow-write
                   shared
                   (lambda (payload)
                     (setf (cdar payload) 99)))))
    (assert-false (eq written original))
    (assert-= 1 (cl-cc/optimize:opt-cow-object-refcount original))
    (assert-= 1 (cl-cc/optimize:opt-cow-object-refcount written))
    (assert-equal '((a . 1)) (cl-cc/optimize:opt-cow-object-payload original))
    (assert-equal '((a . 99)) (cl-cc/optimize:opt-cow-object-payload written))))

;;; ─── Region/bump allocation helpers (FR-254 partial) ───────────────────────

(deftest optimize-bump-region-mark-reset-restores-cursor
  "opt-bump-mark/opt-bump-reset restore cursor to saved point."
  (let ((region (cl-cc/optimize:make-opt-bump-region :cursor 0 :limit 64 :marks nil)))
    (assert-= 0 (cl-cc/optimize:opt-bump-allocate region 8))
    (assert-= 8 (cl-cc/optimize:opt-bump-mark region))
    (assert-= 8 (cl-cc/optimize:opt-bump-allocate region 4))
    (assert-= 12 (cl-cc/optimize:opt-bump-region-cursor region))
    (cl-cc/optimize:opt-bump-reset region)
    (assert-= 8 (cl-cc/optimize:opt-bump-region-cursor region))))

(deftest optimize-slab-pool-reuses-freed-object
  "opt-slab-pool recycles freed objects through freelist."
  (let* ((pool (cl-cc/optimize:make-opt-slab-pool :object-size 2 :free-list nil :next-id 0 :allocated-count 0))
         (obj1 (cl-cc/optimize:opt-slab-allocate pool)))
    (cl-cc/optimize:opt-slab-free pool obj1)
    (let ((obj2 (cl-cc/optimize:opt-slab-allocate pool)))
      (assert-equal obj1 obj2))))

;;; ─── Inline cache helper layer (FR-009 / FR-019 partial) ──────────────────

(deftest optimize-ic-resolve-target-prefers-site-local-entry
  "opt-ic-resolve-target returns site-local target when IC entry exists."
  (let ((site (cl-cc/optimize::make-opt-ic-site :state :monomorphic :entries nil)))
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-a)
      (assert-eq :target-a target)
      (assert-eq :site-local source))))

(deftest optimize-ic-resolve-target-uses-shared-megamorphic-cache
  "Megamorphic site misses consult shared megamorphic cache as fallback."
  (let* ((site (cl-cc/optimize::make-opt-ic-site
                :state :megamorphic :entries nil :max-polymorphic-entries 2))
         (cache (cl-cc/optimize::make-opt-megamorphic-cache :max-size 2)))
    (cl-cc/optimize::opt-mega-cache-put cache :shape-z :target-z)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-z cache)
      (assert-eq :target-z target)
      (assert-eq :megamorphic-shared source))))
