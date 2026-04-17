;;;; tests/unit/optimize/optimizer-memory-tests.lisp
;;;; Unit tests for src/optimize/optimizer-memory.lisp
;;;;
;;;; Covers: opt-heap-root-inst-p, opt-heap-root-kind,
;;;;   %opt-build-root-map / opt-compute-heap-aliases / opt-compute-points-to,
;;;;   opt-points-to-root, opt-interval-* arithmetic,
;;;;   opt-compute-constant-intervals,
;;;;   opt-must-alias-p, opt-may-alias-p, opt-may-alias-by-type-p,
;;;;   opt-slot-alias-key, opt-rewrite-inst-regs,
;;;;   opt-pass-dead-store-elim, opt-pass-store-to-load-forward.

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

(deftest heap-root-kind-nil-for-non-heap
  "opt-heap-root-kind returns NIL for non-heap instructions."
  (assert-null (cl-cc/optimize::opt-heap-root-kind (make-vm-const :dst :r0 :value 1))))

;;; ─── %opt-build-root-map / opt-compute-heap-aliases ────────────────────

(deftest build-root-map-cons-sets-own-root
  "A vm-cons instruction maps its dst to itself as root."
  (let* ((inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (roots (cl-cc/optimize::opt-compute-heap-aliases (list inst))))
    (assert-eq :r0 (gethash :r0 roots))))

(deftest build-root-map-move-propagates-root
  "vm-move propagates the source's root to the destination."
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (move-inst (make-vm-move :dst :r3 :src :r0))
         (roots (cl-cc/optimize::opt-compute-heap-aliases (list cons-inst move-inst))))
    (assert-eq :r0 (gethash :r3 roots))))

(deftest build-root-map-non-heap-write-kills-root
  "A non-heap write to a register removes its root mapping."
  (let* ((cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (add-inst  (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
         (roots (cl-cc/optimize::opt-compute-heap-aliases (list cons-inst add-inst))))
    (assert-false (nth-value 1 (gethash :r0 roots)))))

;;; ─── opt-points-to-root ─────────────────────────────────────────────────

(deftest points-to-root-returns-nil-for-unknown
  "opt-points-to-root returns NIL for a register not in the points-to map."
  (let ((pt (make-hash-table :test #'eq)))
    (assert-null (cl-cc/optimize::opt-points-to-root :r99 pt))))

(deftest points-to-root-returns-canonical-root
  "opt-points-to-root returns the canonical root stored in the map."
  (let ((pt (make-hash-table :test #'eq)))
    (setf (gethash :r0 pt) :r0)
    (assert-eq :r0 (cl-cc/optimize::opt-points-to-root :r0 pt))))

;;; ─── opt-interval-* arithmetic ───────────────────────────────────────────

(deftest interval-make-lo-hi
  "opt-make-interval sets lo and hi correctly."
  (let ((iv (cl-cc/optimize::opt-make-interval 3 7)))
    (assert-= 3 (cl-cc/optimize::opt-interval-lo iv))
    (assert-= 7 (cl-cc/optimize::opt-interval-hi iv))))

(deftest interval-add-produces-correct-range
  "[1,2] + [3,4] = [4,6]."
  (let* ((a (cl-cc/optimize::opt-make-interval 1 2))
         (b (cl-cc/optimize::opt-make-interval 3 4))
         (r (cl-cc/optimize::opt-interval-add a b)))
    (assert-= 4 (cl-cc/optimize::opt-interval-lo r))
    (assert-= 6 (cl-cc/optimize::opt-interval-hi r))))

(deftest interval-sub-produces-correct-range
  "[5,8] - [1,3] = [2,7]."
  (let* ((a (cl-cc/optimize::opt-make-interval 5 8))
         (b (cl-cc/optimize::opt-make-interval 1 3))
         (r (cl-cc/optimize::opt-interval-sub a b)))
    (assert-= 2 (cl-cc/optimize::opt-interval-lo r))
    (assert-= 7 (cl-cc/optimize::opt-interval-hi r))))

(deftest interval-mul-positive-ranges
  "[2,3] * [4,5] = [8,15]."
  (let* ((a (cl-cc/optimize::opt-make-interval 2 3))
         (b (cl-cc/optimize::opt-make-interval 4 5))
         (r (cl-cc/optimize::opt-interval-mul a b)))
    (assert-= 8  (cl-cc/optimize::opt-interval-lo r))
    (assert-= 15 (cl-cc/optimize::opt-interval-hi r))))

(deftest interval-mul-mixed-signs
  "[-1,2] * [3,4] has min=-4, max=8 (conservative)."
  (let* ((a (cl-cc/optimize::opt-make-interval -1 2))
         (b (cl-cc/optimize::opt-make-interval  3 4))
         (r (cl-cc/optimize::opt-interval-mul a b)))
    (assert-= -4 (cl-cc/optimize::opt-interval-lo r))
    (assert-= 8  (cl-cc/optimize::opt-interval-hi r))))

;;; ─── opt-compute-constant-intervals ─────────────────────────────────────

(deftest constant-intervals-vm-const-integer
  "vm-const with integer value produces a singleton interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (let ((iv (gethash :r0 ivals)))
      (assert-true iv)
      (assert-= 5 (cl-cc/optimize::opt-interval-lo iv))
      (assert-= 5 (cl-cc/optimize::opt-interval-hi iv)))))

(deftest constant-intervals-vm-const-float-ignored
  "vm-const with float value is ignored (no interval entry)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 3.14)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (assert-false (nth-value 1 (gethash :r0 ivals)))))

(deftest constant-intervals-propagate-through-add
  "vm-add on two known intervals produces their sum interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-const :dst :r1 :value 3)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (let ((iv (gethash :r2 ivals)))
      (assert-true iv)
      (assert-= 5 (cl-cc/optimize::opt-interval-lo iv)))))

(deftest constant-intervals-unknown-kills-result
  "vm-add with an unknown operand removes the dst interval."
  (let* ((insts (list (make-vm-const :dst :r0 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r99)))
         (ivals (cl-cc/optimize::opt-compute-constant-intervals insts)))
    (assert-false (nth-value 1 (gethash :r2 ivals)))))

;;; ─── opt-must-alias-p / opt-may-alias-p ─────────────────────────────────

(deftest-each must-alias-cases
  "opt-must-alias-p: true iff both registers share the same canonical root."
  :cases (("same-root"    :r0 :r1  '(:r0 :root0 :r1 :root0) t)
          ("diff-roots"   :r0 :r1  '(:r0 :root0 :r1 :root1) nil)
          ("unknown-root" :r0 :r1  '(:r0 :root0)             nil))
  (reg-a reg-b entries expected)
  (let ((alias (make-hash-table :test #'eq)))
    (loop for (k v) on entries by #'cddr do (setf (gethash k alias) v))
    (if expected
        (assert-true  (cl-cc/optimize::opt-must-alias-p reg-a reg-b alias))
        (assert-false (cl-cc/optimize::opt-must-alias-p reg-a reg-b alias)))))

(deftest-each may-alias-cases
  "opt-may-alias-p: conservatively true when a register is unknown or shares root."
  :cases (("same-root"        :r0 :r1  '(:r0 :root0 :r1 :root0) t)
          ("unknown-register" :r0 :r99 '(:r0 :root0)             t)
          ("diff-known-roots" :r0 :r1  '(:r0 :root0 :r1 :root1)  nil))
  (reg-a reg-b entries expected)
  (let ((alias (make-hash-table :test #'eq)))
    (loop for (k v) on entries by #'cddr do (setf (gethash k alias) v))
    (if expected
        (assert-true  (cl-cc/optimize::opt-may-alias-p reg-a reg-b alias))
        (assert-false (cl-cc/optimize::opt-may-alias-p reg-a reg-b alias)))))

;;; ─── opt-slot-alias-key ─────────────────────────────────────────────────

(deftest slot-alias-key-uses-known-root
  "opt-slot-alias-key uses the canonical root when available."
  (let ((alias (make-hash-table :test #'eq)))
    (setf (gethash :r0 alias) :root0)
    (let ((key (cl-cc/optimize::opt-slot-alias-key :r0 'x alias)))
      (assert-equal '(:slot :root0 x) key))))

(deftest slot-alias-key-falls-back-to-reg
  "opt-slot-alias-key uses the register directly when no root is known."
  (let ((alias (make-hash-table :test #'eq)))
    (let ((key (cl-cc/optimize::opt-slot-alias-key :r5 'y alias)))
      (assert-equal '(:slot :r5 y) key))))

;;; ─── opt-rewrite-inst-regs ───────────────────────────────────────────────

(deftest rewrite-inst-regs-replaces-source-register
  "opt-rewrite-inst-regs substitutes copy-propagated source registers."
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r5)
    (let ((new-inst (cl-cc/optimize::opt-rewrite-inst-regs inst copies)))
      ;; dst should be unchanged, lhs should be :r5
      (assert-eq :r2 (cl-cc/vm::vm-dst new-inst))
      (assert-eq :r5 (cl-cc/vm::vm-lhs new-inst)))))

(deftest rewrite-inst-regs-preserves-dst
  "opt-rewrite-inst-regs never rewrites the destination register."
  (let* ((inst (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
         (copies (make-hash-table :test #'eq)))
    (setf (gethash :r0 copies) :r99)
    (let ((new-inst (cl-cc/optimize::opt-rewrite-inst-regs inst copies)))
      (assert-eq :r0 (cl-cc/vm::vm-dst new-inst)))))

(deftest rewrite-inst-regs-no-copies-unchanged
  "opt-rewrite-inst-regs with empty copies returns structurally equivalent instruction."
  (let* ((inst (make-vm-move :dst :r0 :src :r1))
         (copies (make-hash-table :test #'eq))
         (new-inst (cl-cc/optimize::opt-rewrite-inst-regs inst copies)))
    (assert-eq :r1 (cl-cc/vm::vm-src new-inst))))

;;; ─── opt-pass-dead-store-elim ────────────────────────────────────────────

(deftest dead-store-elim-drops-overwritten-global-store
  "opt-pass-dead-store-elim removes an earlier vm-set-global that is overwritten
   by a later store to the same global before any intervening read."
  (let* ((insts (list (make-vm-const      :dst :r0 :value 1)
                      (make-vm-set-global :src :r0 :name 'g)
                      (make-vm-const      :dst :r1 :value 2)
                      (make-vm-set-global :src :r1 :name 'g)
                      (make-vm-ret        :reg :r1)))
         (result (cl-cc/optimize::opt-pass-dead-store-elim insts)))
    ;; Only one vm-set-global should remain
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-set-global)) result))))

(deftest dead-store-elim-preserves-store-before-read
  "opt-pass-dead-store-elim keeps vm-set-global that is read before overwrite."
  (let* ((insts (list (make-vm-const      :dst :r0 :value 1)
                      (make-vm-set-global :src :r0 :name 'g)
                      (make-vm-get-global :dst :r1 :name 'g)
                      (make-vm-ret        :reg :r1)))
         (result (cl-cc/optimize::opt-pass-dead-store-elim insts)))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-set-global)) result))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

(deftest dead-store-elim-preserves-store-before-ret
  "opt-pass-dead-store-elim preserves stores that reach vm-ret (flushed by terminator)."
  (let* ((insts (list (make-vm-const      :dst :r0 :value 42)
                      (make-vm-set-global :src :r0 :name 'result)
                      (make-vm-ret        :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dead-store-elim insts)))
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-set-global)) result))))

;;; ─── opt-pass-store-to-load-forward ──────────────────────────────────────

(deftest store-to-load-forward-replaces-get-global-with-move
  "opt-pass-store-to-load-forward replaces vm-get-global with vm-move when
   the preceding vm-set-global for the same name is pending."
  (let* ((insts (list (make-vm-const      :dst :r0 :value 10)
                      (make-vm-set-global :src :r0 :name 'x)
                      (make-vm-get-global :dst :r1 :name 'x)
                      (make-vm-ret        :reg :r1)))
         (result (cl-cc/optimize::opt-pass-store-to-load-forward insts)))
    ;; The get-global should be replaced by a vm-move
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))

(deftest store-to-load-forward-preserves-when-no-pending-store
  "opt-pass-store-to-load-forward leaves vm-get-global alone when no prior store exists."
  (let* ((insts (list (make-vm-get-global :dst :r0 :name 'unknown)
                      (make-vm-ret        :reg :r0)))
         (result (cl-cc/optimize::opt-pass-store-to-load-forward insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-get-global)) result))))
