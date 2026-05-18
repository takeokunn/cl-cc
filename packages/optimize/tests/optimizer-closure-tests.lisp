;;;; tests/optimizer-closure-tests.lisp — Closure optimization pass tests
;;; FR-330 closure capture dedup and FR-079 closure thunk sharing.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ── Helpers ───────────────────────────────────────────────────────────────

(defun make-test-closure (dst label &rest captured-regs)
  "Create a simple vm-closure capturing CAPTURED-REGS."
  (cl-cc/vm::make-vm-closure
   :dst dst
   :label label
   :captured (mapcar (lambda (reg) (cons (gensym "V") reg)) captured-regs)))

;;; ── FR-330 closure-capture-dedup ──────────────────────────────────────────

(deftest closure-capture-dedup-shares-duplicate-environments
  "Two closures sharing entry-label + capture set → vm-move to first."
  (let* ((c1 (make-test-closure :r1 "L0" :r5))
         (c2 (make-test-closure :r2 "L0" :r5))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-true (typep (first out) 'cl-cc/vm::vm-make-closure))
    (assert-true (typep (second out) 'cl-cc/vm::vm-move))
    (assert-eq (cl-cc/vm::vm-src (second out)) :r1)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)))

(deftest closure-capture-dedup-preserves-non-shareable
  "Closures with different capture sets stay untouched."
  (let* ((c1 (make-test-closure :r1 "L0" :r5))
         (c2 (make-test-closure :r2 "L0" :r6))
         (out (cl-cc/optimize::opt-pass-closure-capture-dedup
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-true (typep (first out) 'cl-cc/vm::vm-make-closure))
    (assert-true (typep (second out) 'cl-cc/vm::vm-make-closure))))

;;; ── FR-079 closure-thunk-sharing ──────────────────────────────────────────

(deftest closure-thunk-sharing-deduplicates-safe-siblings
  "Safe sibling closures sharing capture set → vm-move replacement."
  (let* ((c1 (make-test-closure :r1 "L0" :r5))
         (c2 (make-test-closure :r2 "L0" :r5))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-true (typep (first out) 'cl-cc/vm::vm-closure))
    (assert-true (typep (second out) 'cl-cc/vm::vm-move))
    (assert-eq (cl-cc/vm::vm-src (second out)) :r1)
    (assert-eq (cl-cc/vm::vm-dst (second out)) :r2)))

(deftest closure-thunk-sharing-noops-on-register-overwrite
  "Intermediate register overwrite blocks thunk sharing."
  (let* ((c1 (make-test-closure :r1 "L0" :r5))
         (kill (cl-cc/vm::make-vm-const :dst :r1 :value 0))
         (c2 (make-test-closure :r2 "L0" :r5))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill c2))))
    (assert-eq (length out) 3)
    (assert-true (typep (first out) 'cl-cc/vm::vm-closure))
    (assert-true (typep (second out) 'cl-cc/vm::vm-const))
    (assert-true (typep (third out) 'cl-cc/vm::vm-closure))))

(deftest closure-thunk-sharing-preserves-different-capture
  "Closures with different capture sets → no sharing."
  (let* ((c1 (make-test-closure :r1 "L0" :r5))
         (c2 (make-test-closure :r2 "L0" :r6))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 c2))))
    (assert-eq (length out) 2)
    (assert-true (typep (first out) 'cl-cc/vm::vm-closure))
    (assert-true (typep (second out) 'cl-cc/vm::vm-closure))))

(deftest closure-thunk-sharing-noops-on-env-reg-write
  "Writing to a captured environment register blocks sharing."
  (let* ((c1 (make-test-closure :r1 "L0" :r5 :r7))
         (kill-env (cl-cc/vm::make-vm-const :dst :r5 :value 0))
         (c2 (make-test-closure :r2 "L0" :r5 :r7))
         (out (cl-cc/optimize::opt-pass-closure-thunk-sharing
               (list c1 kill-env c2))))
    (assert-eq (length out) 3)
    (assert-true (typep (third out) 'cl-cc/vm::vm-closure))))
