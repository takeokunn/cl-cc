;;;; packages/foundation/ir/tests/ir-ssa-dominator-tests.lisp — IR Dominator & Collect-Uses Tests
;;;
;;; Continuation of ir-ssa-advanced-tests.lisp.
;;; Covers: ir-dominators (deeper chains), ir-collect-uses (with operands),
;;; ir-verify-ssa (cross-block), and independent variable tracking.
;;;
;;; Note: test-use-inst struct/method is defined in ir-ssa-advanced-tests.lisp
;;; and available here via shared :cl-cc/test package.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── ir-dominators: deeper chains and loops ─────────────────────────────────

(deftest ir-dominators-deep-chain
  "In a 4-block linear chain A->B->C->D, idom(C)=B and idom(D)=C."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (cl-cc/ir:ir-add-edge c d)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom))
      (assert-eq c (gethash d idom)))))

(deftest ir-dominators-two-branches-then-merge
  "In A->{B,C}->D->E: idom(D)=A, idom(E)=D."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d))
         (e  (cl-cc/ir:ir-new-block fn :e)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge a c)
    (cl-cc/ir:ir-add-edge b d)
    (cl-cc/ir:ir-add-edge c d)
    (cl-cc/ir:ir-add-edge d e)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash d idom))
      (assert-eq d (gethash e idom)))))

(deftest ir-dominators-unreachable-absent
  "ir-dominators does not include unreachable blocks in the result table."
  (let* ((fn      (cl-cc/ir:ir-make-function 'test-fn))
         (entry   (cl-cc/ir:irf-entry fn))
         (reached (cl-cc/ir:ir-new-block fn :reached))
         (orphan  (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-add-edge entry reached)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (assert-true  (gethash entry   idom))
      (assert-true  (gethash reached idom))
      (assert-false (gethash orphan  idom)))))

;;; ─── ir-collect-uses with custom operands ────────────────────────────────────

(deftest ir-collect-uses-single-operand
  "ir-collect-uses records a single use of an ir-value."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         ;; i0 produces v0; i1 uses v0 and produces v1
         (i0    (cl-cc/ir:make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit entry i1)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      ;; v0 is used by i1
      (assert-true (member i1 (gethash v0 uses) :test #'eq))
      ;; v1 is not used by anyone
      (assert-null (gethash v1 uses)))))

(deftest ir-collect-uses-multiple-operands
  "ir-collect-uses records multiple uses of the same ir-value."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         (v2    (cl-cc/ir:ir-new-value fn))
         ;; i1 uses v0; i2 also uses v0
         (i1    (make-test-use-inst :result v1 :operands (list v0)))
         (i2    (make-test-use-inst :result v2 :operands (list v0))))
    (cl-cc/ir:ir-emit entry i1)
    (cl-cc/ir:ir-emit entry i2)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      (let ((users (gethash v0 uses)))
        (assert-= 2 (length users))
        (assert-true (member i1 users :test #'eq))
        (assert-true (member i2 users :test #'eq))))))

(deftest ir-collect-uses-across-blocks
  "ir-collect-uses traverses multiple blocks via RPO."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn))
         ;; define v0 in entry; use it in next
         (i0    (cl-cc/ir:make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit next  i1)
    (let ((uses (cl-cc/ir:ir-collect-uses fn)))
      (assert-true (member i1 (gethash v0 uses) :test #'eq)))))

;;; ─── ir-verify-ssa: cross-block checks ──────────────────────────────────────

(deftest ir-verify-ssa-cross-block-valid
  "ir-verify-ssa passes for distinct defs across blocks and for void instructions."
  ;; distinct values in separate blocks
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v0    (cl-cc/ir:ir-new-value fn))
         (v1    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v0))
    (cl-cc/ir:ir-emit next  (cl-cc/ir:make-ir-inst :result v1))
    (assert-true (cl-cc/ir:ir-verify-ssa fn)))
  ;; void instructions (nil result) define no SSA value — still valid
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (i0    (cl-cc/ir:make-ir-inst))
         (i1    (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit entry i0)
    (cl-cc/ir:ir-emit entry i1)
    (assert-true (cl-cc/ir:ir-verify-ssa fn))))

;;; ─── ir-write-var / ir-read-var: multiple independent variables ──────────────

(deftest ir-ssa-independent-vars
  "Different variables are tracked independently, both within and across blocks."
  ;; within same block: a, b, c remain distinct
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (va    (cl-cc/ir:ir-new-value fn))
         (vb    (cl-cc/ir:ir-new-value fn))
         (vc    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'a entry va)
    (cl-cc/ir:ir-write-var fn 'b entry vb)
    (cl-cc/ir:ir-write-var fn 'c entry vc)
    (assert-eq va (cl-cc/ir:ir-read-var fn 'a entry))
    (assert-eq vb (cl-cc/ir:ir-read-var fn 'b entry))
    (assert-eq vc (cl-cc/ir:ir-read-var fn 'c entry)))
  ;; across blocks: vars written in entry are both readable from successor
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (va    (cl-cc/ir:ir-new-value fn))
         (vb    (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn next)
    (cl-cc/ir:ir-write-var fn 'a entry va)
    (cl-cc/ir:ir-write-var fn 'b entry vb)
    (assert-eq va (cl-cc/ir:ir-read-var fn 'a next))
    (assert-eq vb (cl-cc/ir:ir-read-var fn 'b next))))
