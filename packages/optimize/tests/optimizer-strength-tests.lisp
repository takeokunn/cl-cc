;;;; tests/unit/optimize/optimizer-strength-tests.lisp
;;;; Unit tests for src/optimize/optimizer-strength.lisp
;;;;
;;;; Covers: opt-power-of-2-p, opt-pass-strength-reduce,
;;;;   opt-pass-bswap-recognition (pass-through), opt-pass-rotate-recognition.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-power-of-2-p ────────────────────────────────────────────────────

(deftest-each opt-power-of-2-p-true-for-powers-of-two
  "opt-power-of-2-p is true for 2, 4, 8, 16, 256."
  :cases (("two"         2)
          ("four"        4)
          ("eight"       8)
          ("sixteen"     16)
          ("two-fifty-six" 256))
  (n)
  (assert-true (cl-cc/optimize::opt-power-of-2-p n)))

(deftest-each opt-power-of-2-p-false-for-non-powers
  "opt-power-of-2-p is false for 0, 1, 3, 6, 7, and non-integers."
  :cases (("zero"        0)
          ("one"         1)
          ("three"       3)
          ("six"         6)
          ("seven"       7)
          ("negative"    -4)
          ("float"       4.0))
  (n)
  (assert-false (cl-cc/optimize::opt-power-of-2-p n)))

;;; ─── opt-pass-strength-reduce — multiply by power of 2 ──────────────────

(deftest strength-reduce-mul-by-power-of-2-emits-ash
  "opt-pass-strength-reduce replaces (* x 4) with (ash x 2) via vm-ash."
  ;; Input: CONST :R0 ← 4 ;  MUL :R1 ← :X * :R0
  (let* ((const-4 (make-vm-const :dst :r0 :value 4))
         (mul     (make-vm-mul   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-4 mul))))
    ;; The mul should become ash; check no vm-mul in output
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))
    (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))))

(deftest strength-reduce-mul-rhs-power-of-2-emits-ash
  "opt-pass-strength-reduce handles commutative case (power-of-2 on lhs)."
  ;; Input: CONST :R0 ← 8 ;  MUL :R1 ← :R0 * :X
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (mul     (make-vm-mul   :dst :r1 :lhs :r0 :rhs :x))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-8 mul))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))
    (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))))

(deftest strength-reduce-div-by-power-of-2-emits-ash
  "opt-pass-strength-reduce replaces (/ x 8) with (ash x -3) via vm-ash."
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (div     (make-vm-div   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-8 div))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result))
    (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))
    ;; The shift count must be negative (right shift)
    (let ((ash-inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result)))
      (when ash-inst
        (let ((shift-dst (cl-cc/vm::vm-rhs ash-inst)))
          (let ((const-inst (find-if (lambda (i)
                                       (and (typep i 'cl-cc/vm::vm-const)
                                            (eq (cl-cc/vm::vm-dst i) shift-dst)))
                                     result)))
            (when const-inst
              (assert-true (minusp (cl-cc/vm::vm-value const-inst))))))))))

(deftest strength-reduce-mod-by-power-of-2-emits-logand
  "opt-pass-strength-reduce replaces (mod x 8) with (logand x 7) via vm-logand."
  (let* ((const-8 (make-vm-const :dst :r0 :value 8))
         (mod     (make-vm-mod   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-8 mod))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-mod)) result))
    (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-logand)) result))
    ;; The mask constant should be 7 (= 8 - 1)
    (let ((mask-inst (find-if (lambda (i)
                                (and (typep i 'cl-cc/vm::vm-const)
                                     (= (cl-cc/vm::vm-value i) 7)))
                              result)))
      (assert-true mask-inst))))

(deftest strength-reduce-non-power-of-2-mul-passthrough
  "opt-pass-strength-reduce passes through (* x 7) unchanged (7 is not power-of-2 and has 3 set bits)."
  (let* ((const-7 (make-vm-const :dst :r0 :value 7))
         (mul     (make-vm-mul   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-7 mul))))
    ;; 7 has 3 bits set — logcount(7)=3 > 2, so no decomposition
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))))

(deftest strength-reduce-label-clears-constant-env
  "opt-pass-strength-reduce flushes its constant tracking at vm-label boundaries."
  ;; CONST R0←4 ; LABEL top ; MUL R1←X*R0 — after label, R0 is unknown, so no strength reduce
  (let* ((const-4 (make-vm-const :dst :r0 :value 4))
         (lbl     (make-vm-label :name "top"))
         (mul     (make-vm-mul   :dst :r1 :lhs :x :rhs :r0))
         (result  (cl-cc/optimize::opt-pass-strength-reduce (list const-4 lbl mul))))
    ;; After label, constant env is cleared so mul should pass through unchanged
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))))

;;; ─── opt-pass-bswap-recognition ──────────────────────────────────────────

(deftest bswap-recognition-passes-through-non-bswap
  "opt-pass-bswap-recognition passes through ordinary instructions unchanged."
  (let* ((const (make-vm-const :dst :r0 :value 42))
         (move  (make-vm-move  :dst :r1 :src :r0))
         (result (cl-cc/optimize::opt-pass-bswap-recognition (list const move))))
    (assert-= 2 (length result))
    (assert-true (typep (first result)  'cl-cc/vm::vm-const))
    (assert-true (typep (second result) 'cl-cc/vm::vm-move))))

;;; ─── opt-pass-rotate-recognition ─────────────────────────────────────────

(deftest rotate-recognition-passes-through-non-rotate
  "opt-pass-rotate-recognition passes through ordinary instructions unchanged."
  (let* ((const (make-vm-const :dst :r0 :value 10))
         (add   (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0))
         (result (cl-cc/optimize::opt-pass-rotate-recognition (list const add))))
    (assert-= 2 (length result))
    (assert-true (typep (first result)  'cl-cc/vm::vm-const))
    (assert-true (typep (second result) 'cl-cc/vm::vm-add))))

(deftest rotate-recognition-collapses-rotate-idiom
  "opt-pass-rotate-recognition collapses (ash x k) | (ash x (k-64)) to vm-rotate."
  ;; Build: CONST K0←8  ASH A0←X,K0  CONST K1←-56  ASH A1←X,K1  LOGIOR OUT←A0,A1
  (let* ((k0  (make-vm-const :dst :rc0 :value 8))
         (a0  (make-vm-ash   :dst :ra0 :lhs :x :rhs :rc0))
         (k1  (make-vm-const :dst :rc1 :value -56))
         (a1  (make-vm-ash   :dst :ra1 :lhs :x :rhs :rc1))
         (or0 (make-vm-logior :dst :rout :lhs :ra0 :rhs :ra1))
         (result (cl-cc/optimize::opt-pass-rotate-recognition (list k0 a0 k1 a1 or0))))
    ;; Should collapse to two instructions: a vm-const + vm-rotate
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-rotate)) result))
    ;; vm-logior should not appear in the output
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-logior)) result))))

;;; ─── opt-pass-fill-recognition ───────────────────────────────────────────

(defun make-fill-loop-instructions (&key extra-exit-jump)
  (append (when extra-exit-jump
            (list (make-vm-jump :label "Lexit")))
          (list (make-vm-array-length :dst :rlen :src :rvec)
                (make-vm-const :dst :ri :value 0)
                (make-vm-label :name "Lfill")
                (make-vm-lt :dst :rcond :lhs :ri :rhs :rlen)
                (make-vm-jump-zero :reg :rcond :label "Lexit")
                (make-vm-aset :array-reg :rvec :index-reg :ri :val-reg :rval)
                (make-vm-const :dst :rone :value 1)
                (make-vm-add :dst :rnext :lhs :ri :rhs :rone)
                (make-vm-move :dst :ri :src :rnext)
                (make-vm-jump :label "Lfill")
                (make-vm-label :name "Lexit")
                (make-vm-ret :reg :rvec))))

(defun fill-inst-p (inst)
  (eq (type-of inst) 'cl-cc/vm::vm-fill))

(deftest fill-recognition-collapses-canonical-fill-loop
  "opt-pass-fill-recognition replaces a private zero-based aset loop with vm-fill."
  (let ((result (cl-cc/optimize::opt-pass-fill-recognition (make-fill-loop-instructions))))
    (assert-= 1 (count-if #'fill-inst-p result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-move)
                              (eq (cl-cc/vm::vm-dst i) :ri)
                              (eq (cl-cc/vm::vm-src i) :rlen)))
                       result))))

(deftest fill-recognition-skips-externally-targeted-exit
  "opt-pass-fill-recognition leaves the loop unchanged when another jump targets the exit label."
  (let ((result (cl-cc/optimize::opt-pass-fill-recognition
                 (make-fill-loop-instructions :extra-exit-jump t))))
    (assert-false (some #'fill-inst-p result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-aset)) result))))
