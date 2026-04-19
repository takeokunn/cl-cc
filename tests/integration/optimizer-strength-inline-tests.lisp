;;;; optimizer-strength-inline-tests.lisp
;;;
;;; Integration tests for strength-reduction, reassociation,
;;; mul-by-const decomposition, adaptive inlining, and optimizer
;;; helper predicates.
;;;
;;; Extracted from optimizer-lowlevel-tests.lisp (> 600 lines).

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── opt-pass-strength-reduce: Strength Reduction ─────────────────────────

(deftest-each strength-reduce-cases
  "opt-pass-strength-reduce: power-of-2 mul/div become vm-ash; mod-by-power-of-2 becomes vm-logand."
  :cases (("mul-rhs-pow2"
           8 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mul-lhs-pow2"
           4 (make-vm-mul :dst :r2 :lhs :r1 :rhs :r0)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mul-non-power-of-2"
           7 (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("div-rhs-pow2"
           8 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("div-non-power-of-2"
           7 (cl-cc::make-vm-div :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))))
          ("mod-pow2"
           8 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-false (member op out))
             (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out))))
          ("mod-non-power-of-2"
           7 (cl-cc::make-vm-mod :dst :r2 :lhs :r0 :rhs :r1)
           (lambda (out op)
             (assert-true (member op out))
             (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) out)))))
  (const-val op verify)
  (let* ((c   (make-vm-const :dst :r1 :value const-val))
         (ret (make-vm-ret   :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c op ret))))
    (funcall verify out op)))

;;; ── opt-pass-reassociate: Arithmetic Reassociation ──────────────────────

(deftest-each reassociate-moves-constant-inward
  "opt-pass-reassociate moves constants toward the tail of nested op chains (add and logand)."
  :cases (("add"    1   (make-vm-add    :dst :r4 :lhs :r2 :rhs :r1) (make-vm-add    :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-add)
          ("logand" 255 (make-vm-logand :dst :r4 :lhs :r2 :rhs :r1) (make-vm-logand :dst :r5 :lhs :r4 :rhs :r3) 'cl-cc/vm::vm-logand))
  (const-val op1 op2 op-type)
  (let* ((c    (make-vm-const :dst :r1 :value const-val))
         (a    (make-vm-move  :dst :r2 :src :r8))
         (b    (make-vm-move  :dst :r3 :src :r9))
         (ret  (make-vm-ret   :reg :r5))
         (out  (cl-cc/optimize::opt-pass-reassociate (list c a b op1 op2 ret)))
         (ops  (remove-if-not (lambda (i) (typep i op-type)) out)))
    (assert-equal 2 (length ops))
    (assert-eq :r3 (cl-cc/vm::vm-lhs (first ops)))
    (assert-eq :r1 (cl-cc/vm::vm-rhs (first ops)))
    (assert-eq :r2 (cl-cc/vm::vm-lhs (second ops)))
    (assert-eq :r4 (cl-cc/vm::vm-rhs (second ops)))))

(deftest strength-reduce-mul-by-const-decomposes
  "opt-pass-strength-reduce: small non-power-of-2 constant multipliers are decomposed into shifts/adds."
  (let* ((c   (make-vm-const :dst :r1 :value 3))
         (mul (make-vm-mul :dst :r2 :lhs :r0 :rhs :r1))
         (ret (make-vm-ret :reg :r2))
         (out (cl-cc/optimize::opt-pass-strength-reduce (list c mul ret))))
    (assert-false (member mul out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) out))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-add)) out))))

;;; ── %opt-mul-by-const-seq: Extracted Shift/Add Decomposition ────────────

(defun make-test-new-reg ()
  "Return a thunk that allocates fresh keyword registers :R1000, :R1001, ..."
  (let ((n 1000))
    (lambda () (prog1 (intern (format nil "R~A" n) :keyword) (incf n)))))

(deftest-each mul-by-const-seq-cases
  "%opt-mul-by-const-seq: correctly decomposes constant multipliers into shift/add sequences."
  :cases (("zero"     0  'cl-cc/vm::vm-const nil)
          ("one"      1  'cl-cc/vm::vm-move  nil)
          ("neg-one" -1  'cl-cc/vm::vm-move  'cl-cc/vm::vm-neg)
          ("two"      2  'cl-cc/vm::vm-ash   nil)
          ("three"    3  'cl-cc/vm::vm-add   nil)
          ("four"     4  'cl-cc/vm::vm-ash   nil)
          ("six"      6  'cl-cc/vm::vm-add   nil))
  (n expected-type-1 expected-type-2)
  (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 n (make-test-new-reg))))
    (assert-true (some (lambda (i) (typep i expected-type-1)) seq))
    (when expected-type-2
      (assert-true (some (lambda (i) (typep i expected-type-2)) seq)))))

(deftest mul-by-const-seq-correctness
  "%opt-mul-by-const-seq: emitted instructions have correct destination register."
  (let* ((seq (cl-cc/optimize::%opt-mul-by-const-seq :r0 :r1 5 (make-test-new-reg)))
         (add (find-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) seq)))
    (assert-true add)
    (assert-equal :r0 (cl-cc/vm::vm-dst add))))

;;; ── opt-inline-eligible-p: Extracted Eligibility Predicate ──────────────

(deftest-each opt-inline-eligible-p-cases
  "opt-inline-eligible-p: eligible when cheap+no-captures; rejects captured vars or over-budget."
  :cases (("short-no-captures"
           nil
           (list (make-vm-add :dst :r2 :lhs :r1 :rhs :r1) (make-vm-ret :reg :r2))
           t)
          ("captured-vars-ineligible"
           (list (cons :x :r5))
           (list (make-vm-ret :reg :r1))
           nil)
          ("cheap-consts-eligible-over-threshold"
           nil
           (append (loop repeat 17 collect (make-vm-const :dst :r2 :value 0))
                   (list (make-vm-ret :reg :r1)))
           t)
          ("arith-instrs-over-budget-ineligible"
           nil
           (append (loop repeat 16 collect (make-vm-add :dst :r2 :lhs :r1 :rhs :r1))
                   (list (make-vm-ret :reg :r2)))
           nil))
  (captured body expected)
  (let* ((ci  (make-vm-closure :dst :r0 :label "f"
                                :params '(:r1) :captured captured
                                :optional-params nil :rest-param nil :key-params nil))
         (def (list :closure ci :params '(:r1) :body body)))
    (assert-equal expected (not (null (cl-cc/optimize::opt-inline-eligible-p def 15))))))

(deftest opt-adaptive-inline-threshold-cases
  "Cheap bodies get a higher threshold; call-heavy bodies get a tighter one."
  (let* ((cheap-ci (make-vm-closure :dst :r0 :label "cheap"
                                    :params '(:r1) :captured nil
                                    :optional-params nil :rest-param nil :key-params nil))
         (cheap-body (append (loop repeat 20 collect (make-vm-const :dst :r2 :value 0))
                             (list (make-vm-ret :reg :r1))))
         (cheap-def (list :closure cheap-ci :params '(:r1) :body cheap-body))
         (call-ci (make-vm-closure :dst :r0 :label "callish"
                                   :params '(:r1) :captured nil
                                   :optional-params nil :rest-param nil :key-params nil))
         (call-body (list (make-vm-call :dst :r2 :func :r3 :args '(:r1))
                          (make-vm-ret :reg :r2)))
         (call-def (list :closure call-ci :params '(:r1) :body call-body)))
    (assert-true (> (cl-cc/optimize::opt-adaptive-inline-threshold cheap-def) 15))
    (assert-true (< (cl-cc/optimize::opt-adaptive-inline-threshold call-def) 15))))

(deftest opt-pass-inline-iterative-uses-adaptive-threshold
  "The iterative inline wrapper runs with adaptive thresholds without error."
  (let* ((ci   (make-vm-closure :dst :r0 :label "f"
                                :params '(:r1) :captured nil
                                :optional-params nil :rest-param nil :key-params nil))
         (fref (make-vm-func-ref :dst :r3 :label "f"))
         (body (append (loop repeat 18 collect (make-vm-const :dst :r2 :value 0))
                       (list (make-vm-ret :reg :r1))))
         (call (make-vm-call :dst :r4 :func :r3 :args '(:r5)))
         (ret  (make-vm-ret :reg :r4))
         (out  (cl-cc/optimize::opt-pass-inline-iterative
                (append (list ci) body (list fref call ret)))))
    (assert-true out)))

(deftest-each opt-adaptive-max-iterations-cases
  "Adaptive max-iterations shrinks for small programs and grows for larger ones."
  :cases (("small" 10  #'<  20)
          ("large" 900 #'>  20))
  (n-insts pred threshold)
  (let ((iters (cl-cc/optimize::opt-adaptive-max-iterations
                (loop repeat n-insts collect (make-vm-const :dst :r0 :value 0)))))
    (assert-true (funcall pred iters threshold))))

(deftest optimize-instructions-accepts-adaptive-max-iterations
  "optimize-instructions accepts :adaptive as the iteration budget selector."
  (let ((out (cl-cc/optimize::optimize-instructions
              (list (make-vm-const :dst :r0 :value 1)
                    (make-vm-ret :reg :r0))
              :max-iterations :adaptive
              :pass-pipeline '(:fold :dce))))
    (assert-true out)))

;;; ─── opt-falsep ──────────────────────────────────────────────────────────

(deftest-each opt-falsep
  "opt-falsep correctly classifies falsy (nil only) and truthy values."
  :cases (("nil"      nil t)
          ("zero"     0   t)
          ("t"        t   nil)
          ("positive" 1   nil))
  (value expected)
  (assert-equal expected (cl-cc/optimize::opt-falsep value)))

;;; ─── opt-register-keyword-p ──────────────────────────────────────────────

(deftest-each opt-register-keyword-p
  "opt-register-keyword-p recognizes :RN keywords; rejects non-register symbols and plain keywords."
  :cases (("r0"            :r0  t)
          ("r15"           :r15 t)
          ("plain-symbol"  'r0  nil)
          ("plain-keyword" :foo nil))
  (value expected)
  (assert-equal expected (cl-cc/optimize::opt-register-keyword-p value)))

;;; ─── opt-binary-lhs-rhs-p / opt-unary-src-p ─────────────────────────────

(deftest-each opt-instruction-shape-predicates
  "opt-binary-lhs-rhs-p and opt-unary-src-p classify instruction shapes correctly."
  :cases (("binary-add"    #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2) t)
          ("binary-lt"     #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-lt     :dst :r0 :lhs :r1 :rhs :r2) t)
          ("binary-neg"    #'cl-cc/optimize::opt-binary-lhs-rhs-p (make-vm-neg    :dst :r0 :src :r1)           nil)
          ("unary-neg"     #'cl-cc/optimize::opt-unary-src-p      (make-vm-neg    :dst :r0 :src :r1)           t)
          ("unary-null-p"  #'cl-cc/optimize::opt-unary-src-p      (make-vm-null-p :dst :r0 :src :r1)           t)
          ("unary-add"     #'cl-cc/optimize::opt-unary-src-p      (make-vm-add    :dst :r0 :lhs :r1 :rhs :r2)  nil))
  (pred-fn inst expected)
  (assert-equal expected (not (null (funcall pred-fn inst)))))

;;; ─── opt-foldable-unary-arith-p / opt-foldable-type-pred-p ──────────────

(deftest-each opt-foldable-predicates
  "opt-foldable-unary-arith-p and opt-foldable-type-pred-p classify fold eligibility."
  :cases (("arith-neg"    #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-neg    :dst :r0 :src :r1) t)
          ("arith-null-p" #'cl-cc/optimize::opt-foldable-unary-arith-p (make-vm-null-p :dst :r0 :src :r1) nil)
          ("pred-null-p"  #'cl-cc/optimize::opt-foldable-type-pred-p   (make-vm-null-p :dst :r0 :src :r1) t)
          ("pred-neg"     #'cl-cc/optimize::opt-foldable-type-pred-p   (make-vm-neg    :dst :r0 :src :r1) nil))
  (pred-fn inst expected)
  (assert-equal expected (funcall pred-fn inst)))
