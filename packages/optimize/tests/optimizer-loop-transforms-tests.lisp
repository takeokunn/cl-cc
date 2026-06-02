;;;; tests/unit/optimize/optimizer-loop-transforms-tests.lisp
;;;; Unit tests for loop optimization passes: rotation, peeling, unrolling.
;;;;
;;;; Covers: opt-pass-loop-rotation, opt-pass-loop-peeling, opt-pass-loop-unrolling.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper: build instruction sequences ─────────────────────────────────

(defun %loop-transforms-make-while-loop-insts ()
  "Build a simple while-loop instruction sequence used by loop optimization tests.
Pattern: loop on :r0 while (integer-p :r0), adding :r2 each iteration."
  (list (make-vm-label :name "Lh")
        (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
        (make-vm-jump-zero :reg :r1 :label "Lexit")
        (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
        (make-vm-jump :label "Lh")
        (make-vm-label :name "Lexit")
        (make-vm-ret :reg :r0)))

(defun %loop-transforms-make-counted-loop-insts (&key (lim 3))
  "Build a counted-loop instruction sequence for loop unrolling tests.
Pattern: count :i from 0 to LIM by 1, accumulating in :sum."
  (list (make-vm-const :dst :i   :value 0)
        (make-vm-const :dst :lim :value lim)
        (make-vm-const :dst :one :value 1)
        (make-vm-label :name "Lh")
        (make-vm-lt :dst :c :lhs :i :rhs :lim)
        (make-vm-jump-zero :reg :c :label "Lexit")
        (make-vm-add :dst :sum :lhs :sum :rhs :i)
        (make-vm-add :dst :i   :lhs :i   :rhs :one)
        (make-vm-jump :label "Lh")
        (make-vm-label :name "Lexit")
        (make-vm-ret :reg :sum)))

;;; ─── Loop rotation ────────────────────────────────────────────────────────

(deftest loop-rotation-rotates-simple-while-shape
  "opt-pass-loop-rotation rewrites a simple while-shape into guard+body form."
  (let* ((insts (%loop-transforms-make-while-loop-insts))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts))
         (first-inst (first out))
         (jumps-to-lh (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "Lh")))
                                out))
         (guard-jumps (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump-zero)
                                       (equal (cl-cc/vm::vm-label-name i) "Lexit")))
                                out)))
    (assert-true (typep first-inst 'cl-cc/vm::vm-jump))
    (assert-= 0 jumps-to-lh)
    (assert-= 1 guard-jumps)))

(deftest-each loop-passes-noop-on-nonmatching-shape
  "Loop optimization passes leave non-matching control-flow shapes unchanged in length."
  :cases (("rotation" #'cl-cc/optimize::opt-pass-loop-rotation)
          ("peeling"  #'cl-cc/optimize::opt-pass-loop-peeling))
  (pass)
  (let* ((insts (list (make-vm-label :name "A")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "B")
                      (make-vm-label :name "B")
                      (make-vm-ret :reg :r0)))
         (out (funcall pass insts)))
    (assert-= (length insts) (length out))))

;;; ─── Loop peeling ─────────────────────────────────────────────────────────

(deftest loop-peeling-duplicates-first-iteration-for-simple-while
  "opt-pass-loop-peeling duplicates first iteration for simple while shape."
  (let* ((insts (%loop-transforms-make-while-loop-insts))
         (out (cl-cc/optimize::opt-pass-loop-peeling insts))
         (jz-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-= 2 jz-count)
    (assert-= 2 add-count)))

;;; ─── Loop unrolling ───────────────────────────────────────────────────────

(deftest loop-unrolling-fully-unrolls-small-counted-loop
  "opt-pass-loop-unrolling fully unrolls a tiny counted loop with known trip count."
  (let* ((insts (%loop-transforms-make-counted-loop-insts :lim 3))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (jump-to-lh (count-if (lambda (x)
                                (and (typep x 'cl-cc/vm::vm-jump)
                                     (equal (cl-cc/vm::vm-label-name x) "Lh")))
                              out))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (assert-= 0 jump-to-lh)
    (assert-= 0 lt-count)
    (assert-= 3 step-count)))

(deftest loop-unrolling-partially-unrolls-unknown-trip-with-remainder
  "opt-pass-loop-unrolling emits guarded partial copies plus the original loop for unknown trips."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (assert-= 3 lt-count)
    (assert-= 1 jump-to-lh)))

(deftest-each loop-unrolling-supports-additional-comparisons
  "opt-pass-loop-unrolling fully unrolls small counted loops using le/ge/eq predicates."
  :cases (("le" (make-vm-const :dst :i :value 0)
                 (make-vm-const :dst :lim :value 2)
                 (make-vm-const :dst :step :value 1)
                 (make-vm-le :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-le 3)
          ("ge" (make-vm-const :dst :i :value 3)
                 (make-vm-const :dst :lim :value 1)
                 (make-vm-const :dst :step :value -1)
                 (make-vm-ge :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-ge 3)
          ("eq" (make-vm-const :dst :i :value 4)
                 (make-vm-const :dst :lim :value 4)
                 (make-vm-const :dst :step :value 1)
                 (make-vm-eq :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-eq 1))
  (init-inst limit-inst step-inst cmp-inst cmp-type expected-steps)
  (let* ((insts (list init-inst
                      limit-inst
                      step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (assert-= 0 cmp-count)
    (assert-= expected-steps step-count)))

(deftest loop-unrolling-partial-keeps-remainder-loop
  "opt-pass-loop-unrolling partially unrolls larger loops and keeps a back-edge remainder loop."
  (let* ((insts (%loop-transforms-make-counted-loop-insts :lim 10))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (assert-= 3 lt-count)
    (assert-= 3 step-count)
    (assert-= 1 jump-to-lh)))

(deftest-each cfg-natural-loop-transforms-detected
  "CFG-based loop rotation and peeling detect single-latch natural loops."
  :cases (("rotation" #'cl-cc/optimize::opt-pass-loop-rotation 0 1)
          ("peeling"  #'cl-cc/optimize::opt-pass-loop-peeling  1 2))
  (pass expected-jumps-to-lh expected-adds)
  (let* ((insts (list (make-vm-const :dst :one :value 1)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :c :src :i)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :i)))
         (out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (assert-= expected-jumps-to-lh jumps-to-lh)
    (assert-= expected-adds add-count)))
