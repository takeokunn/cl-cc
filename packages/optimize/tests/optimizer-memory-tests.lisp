;;;; tests/unit/optimize/optimizer-memory-tests.lisp
;;;; Unit tests for optimizer-memory.lisp — heap alias and interval analysis
;;;;
;;;; Covers: opt-heap-root-inst-p, opt-heap-root-kind,
;;;;   opt-compute-heap-aliases, opt-compute-points-to,
;;;;   opt-interval-* arithmetic, opt-compute-constant-intervals,
;;;;   cfg-value-ranges, simple-induction-variable detection.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-heap-root-inst-p ───────────────────────────────────────────────

(deftest-each heap-root-inst-p-true-cases
  "opt-heap-root-inst-p returns T for heap-allocating instructions."
  :cases (("vm-cons"         (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2))
          ("vm-make-array"   (make-vm-make-array  :dst :r0 :size-reg :r1))
          ("vm-make-closure" (make-vm-make-closure :dst :r0 :label "f" :env-regs '(:r1))))
  (inst)
  (assert-true (cl-cc/optimize::opt-heap-root-inst-p inst)))

(deftest-each heap-root-inst-p-false-cases
  "opt-heap-root-inst-p returns NIL for non-allocating instructions."
  :cases (("vm-const" (make-vm-const :dst :r0 :value 1))
          ("vm-add"   (make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-move"  (make-vm-move  :dst :r0 :src :r1)))
  (inst)
  (assert-false (cl-cc/optimize::opt-heap-root-inst-p inst)))

;;; ─── opt-heap-root-kind ─────────────────────────────────────────────────

(deftest-each heap-root-kind-values
  "opt-heap-root-kind returns the expected symbolic heap kind."
  :cases (("cons"    (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2) :cons)
          ("array"   (make-vm-make-array  :dst :r0 :size-reg :r1)             :array)
          ("closure" (make-vm-make-closure :dst :r0 :label "f" :env-regs '(:r1)) :closure))
  (inst expected-kind)
  (assert-eq expected-kind (cl-cc/optimize::opt-heap-root-kind inst)))

;;; ─── %opt-build-root-map / opt-compute-heap-aliases ────────────────────

(deftest heap-alias-cons-sets-own-root
  "opt-compute-heap-aliases: a vm-cons instruction sets its dst register as its own canonical root."
  (let* ((inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list inst))))
    (assert-eq :r0 (gethash :r0 roots))))

(deftest heap-alias-move-propagates-root
  "opt-compute-heap-aliases: a vm-move propagates the cons root to the destination register."
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (move-inst (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list cons-inst move-inst))))
    (assert-eq :r0 (gethash :r3 roots))))

(deftest heap-alias-non-heap-write-kills-root
  "opt-compute-heap-aliases: overwriting a heap root with a non-heap instruction removes the root entry."
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (add-inst  (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
         (roots (cl-cc/optimize:opt-compute-heap-aliases (list cons-inst add-inst))))
    (assert-false (nth-value 1 (gethash :r0 roots)))))

(deftest heap-alias-unknown-register-returns-nil
  "An unregistered key in a fresh alias table returns nil."
  (let ((pt (make-hash-table :test #'eq)))
    (assert-null (gethash :r99 pt))))

(deftest heap-alias-known-register-returns-canonical-root
  "A manually set alias table entry returns the canonical root for a known register."
  (let ((pt (make-hash-table :test #'eq)))
    (setf (gethash :r0 pt) :r0)
    (assert-eq :r0 (gethash :r0 pt))))

(deftest points-to-tracks-fresh-root-and-move
  "opt-compute-points-to tracks fresh heap roots and vm-move aliases."
  (let* ((insts (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)
                      (make-vm-move :dst :r3 :src :r0)))
         (points-to (cl-cc/optimize:opt-compute-points-to insts)))
    (assert-eq :r0 (cl-cc/optimize:opt-points-to-root :r0 points-to))
    (assert-eq :r0 (cl-cc/optimize:opt-points-to-root :r3 points-to))))

(deftest points-to-root-reports-unknown-register
  "opt-points-to-root returns NIL/NIL for an unknown register."
  (let ((points-to (cl-cc/optimize:opt-compute-points-to nil)))
    (multiple-value-bind (root found-p)
        (cl-cc/optimize:opt-points-to-root :missing points-to)
      (assert-null root)
      (assert-false found-p))))

(deftest points-to-overwrite-kills-stale-root
  "opt-compute-points-to kills a stale points-to fact after a non-heap write."
  (let* ((insts (list (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)
                      (make-vm-add :dst :r0 :lhs :r1 :rhs :r2)))
         (points-to (cl-cc/optimize:opt-compute-points-to insts)))
    (assert-false (nth-value 1 (cl-cc/optimize:opt-points-to-root :r0 points-to)))))

;;; ─── opt-interval-* arithmetic ───────────────────────────────────────────

(deftest interval-make-and-read-lo-hi
  "opt-make-interval sets lo and hi accessible via opt-interval-lo and opt-interval-hi."
  (let ((iv (cl-cc/optimize::opt-make-interval 3 7)))
    (assert-= 3 (cl-cc/optimize::opt-interval-lo iv))
    (assert-= 7 (cl-cc/optimize::opt-interval-hi iv))))

(deftest-each interval-arithmetic-cases
  "Interval arithmetic produces expected [lo, hi] bounds for add, sub, and mul."
  :cases (("add"       #'cl-cc/optimize:opt-interval-add  1  2  3  4   4  6)
          ("sub"       #'cl-cc/optimize:opt-interval-sub  5  8  1  3   2  7)
          ("mul-pos"   #'cl-cc/optimize::opt-interval-mul  2  3  4  5   8 15)
          ("mul-mixed" #'cl-cc/optimize::opt-interval-mul -1  2  3  4  -4  8))
  (op a-lo a-hi b-lo b-hi expected-lo expected-hi)
  (let* ((a (cl-cc/optimize::opt-make-interval a-lo a-hi))
         (b (cl-cc/optimize::opt-make-interval b-lo b-hi))
         (r (funcall op a b)))
    (assert-= expected-lo (cl-cc/optimize::opt-interval-lo r))
    (assert-= expected-hi (cl-cc/optimize::opt-interval-hi r))))

;;; ─── opt-compute-constant-intervals ─────────────────────────────────────

(deftest constant-intervals-integer-const-yields-singleton
  "opt-compute-constant-intervals: an integer vm-const produces a [v,v] singleton interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (let ((iv (gethash :r0 ivals)))
      (assert-true iv)
      (assert-= 5 (cl-cc/optimize::opt-interval-lo iv))
      (assert-= 5 (cl-cc/optimize::opt-interval-hi iv)))))

(deftest constant-intervals-float-const-yields-no-entry
  "opt-compute-constant-intervals: a float vm-const does not produce an interval entry."
  (let* ((insts (list (make-vm-const :dst :r0 :value 3.14)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (assert-false (nth-value 1 (gethash :r0 ivals)))))

(deftest constant-intervals-add-two-known-yields-sum-interval
  "opt-compute-constant-intervals: adding two known constants yields their sum as an interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-const :dst :r1 :value 3)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (let ((iv (gethash :r2 ivals)))
      (assert-true iv)
      (assert-= 5 (cl-cc/optimize::opt-interval-lo iv)))))

(deftest value-ranges-propagate-move-and-arithmetic
  "opt-compute-value-ranges propagates intervals through moves and arithmetic."
  (let* ((insts (list (make-vm-const :dst :r0 :value 4)
                      (make-vm-move  :dst :r1 :src :r0)
                      (make-vm-const :dst :r2 :value 6)
                      (make-vm-add   :dst :r3 :lhs :r1 :rhs :r2)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :r3 ivals)))
    (assert-true iv)
    (assert-= 10 (cl-cc/optimize::opt-interval-lo iv))
    (assert-= 10 (cl-cc/optimize::opt-interval-hi iv))))

(deftest value-ranges-prove-array-index-in-bounds
  "opt-array-bounds-check-eliminable-p proves simple non-negative index ranges."
  (let* ((insts (list (make-vm-const :dst :idx :value 2)
                      (make-vm-const :dst :len :value 5)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts)))
    (assert-true (cl-cc/optimize::opt-array-bounds-check-eliminable-p :idx :len ivals))))

(deftest value-ranges-reject-out-of-bounds-index
  "opt-array-bounds-check-eliminable-p stays conservative for invalid index ranges."
  (let* ((insts (list (make-vm-const :dst :idx :value 5)
                      (make-vm-const :dst :len :value 5)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts)))
    (assert-false (cl-cc/optimize::opt-array-bounds-check-eliminable-p :idx :len ivals))))

(deftest cfg-value-ranges-join-unions-constant-predecessors
  "CFG range analysis unions predecessor intervals at joins conservatively."
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :cond :label "else")
                     (make-vm-const :dst :r0 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r0 :value 3)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r0))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result)))
         (iv (gethash :r0 join-in)))
    (assert-true iv)
    (assert-= 1 (cl-cc/optimize::opt-interval-lo iv))
    (assert-= 3 (cl-cc/optimize::opt-interval-hi iv))))

(deftest cfg-value-ranges-join-drops-missing-predecessor-fact
  "CFG range analysis drops a register that is not known on every predecessor."
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-jump-zero :reg :cond :label "else")
                     (make-vm-const :dst :r0 :value 1)
                     (make-vm-jump :label "join")
                     (make-vm-label :name "else")
                     (make-vm-const :dst :r1 :value 3)
                     (make-vm-label :name "join")
                     (make-vm-ret :reg :r1))))
         (join (cl-cc/optimize:cfg-get-block-by-label cfg "join"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (join-in (gethash join (cl-cc/optimize:opt-dataflow-result-in result))))
    (assert-false (nth-value 1 (gethash :r0 join-in)))))

(deftest cfg-value-ranges-loop-self-update-kills-unsafe-fact
  "Loop-carried self-updates kill their destination fact instead of diverging."
  (let* ((cfg (cl-cc/optimize:cfg-build
               (list (make-vm-const :dst :i :value 0)
                     (make-vm-label :name "loop")
                     (make-vm-const :dst :one :value 1)
                     (make-vm-add :dst :i :lhs :i :rhs :one)
                     (make-vm-jump-zero :reg :exit-flag :label "exit")
                     (make-vm-jump :label "loop")
                     (make-vm-label :name "exit")
                     (make-vm-ret :reg :i))))
         (loop-block (cl-cc/optimize:cfg-get-block-by-label cfg "loop"))
         (exit-block (cl-cc/optimize:cfg-get-block-by-label cfg "exit"))
         (result (cl-cc/optimize:opt-compute-cfg-value-ranges cfg))
         (loop-in (gethash loop-block (cl-cc/optimize:opt-dataflow-result-in result)))
         (loop-out (gethash loop-block (cl-cc/optimize:opt-dataflow-result-out result)))
         (exit-in (gethash exit-block (cl-cc/optimize:opt-dataflow-result-in result))))
    (assert-false (nth-value 1 (gethash :i loop-in)))
    (assert-false (nth-value 1 (gethash :i loop-out)))
    (assert-false (nth-value 1 (gethash :i exit-in)))))

(deftest value-ranges-convenience-wrapper-merges-branch-exit-ranges
  "opt-compute-value-ranges keeps the old API while using CFG analysis on branches."
  (let* ((insts (list (make-vm-jump-zero :reg :cond :label "else")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "join")
                      (make-vm-label :name "else")
                      (make-vm-const :dst :r0 :value 3)
                      (make-vm-label :name "join")
                      (make-vm-ret :reg :r0)))
         (ivals (cl-cc/optimize::opt-compute-value-ranges insts))
         (iv (gethash :r0 ivals)))
    (assert-true iv)
    (assert-= 1 (cl-cc/optimize::opt-interval-lo iv))
    (assert-= 3 (cl-cc/optimize::opt-interval-hi iv))))

(deftest simple-induction-detects-affine-update
  "opt-compute-simple-inductions records init and step for affine self updates."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-add   :dst :i :lhs :i :rhs :one)))
         (ivs (cl-cc/optimize::opt-compute-simple-inductions insts))
         (iv (gethash :i ivs)))
    (assert-true iv)
    (assert-= 0 (cl-cc/optimize::opt-iv-init iv))
    (assert-= 1 (cl-cc/optimize::opt-iv-step iv))))

(deftest simple-induction-kills-stale-fact-after-overwrite
  "opt-compute-simple-inductions removes an induction fact after a later write."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-add   :dst :i :lhs :i :rhs :one)
                      (make-vm-const :dst :i :value 42)))
         (ivs (cl-cc/optimize::opt-compute-simple-inductions insts)))
    (assert-false (nth-value 1 (gethash :i ivs)))))

(deftest induction-trip-count-cases
  "opt-induction-trip-count handles positive, inclusive, and negative steps."
  (assert-= 5 (cl-cc/optimize::opt-induction-trip-count 0 10 2))
  (assert-= 6 (cl-cc/optimize::opt-induction-trip-count 0 10 2 :inclusive-p t))
  (assert-= 4 (cl-cc/optimize::opt-induction-trip-count 10 0 -3)))

(deftest constant-intervals-unknown-operand-kills-dst
  "opt-compute-constant-intervals: an unknown operand in a binary op removes the dst interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r99)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (assert-false (nth-value 1 (gethash :r2 ivals)))))
