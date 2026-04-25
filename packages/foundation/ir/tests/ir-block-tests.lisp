;;;; tests/unit/compile/ir/ir-block-tests.lisp — CFG Block Structure Tests
;;;
;;; Tests for ir-add-edge, ir-emit, ir-set-terminator, ir-rpo, ir-dominators.
;;; SSA construction/verification → ir-block-ssa-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-test-fn (&optional (name 'test-fn))
  "Create a fresh IR function with entry block."
  (cl-cc/ir:ir-make-function name))

(defun make-test-inst (fn &key result)
  "Create a simple IR instruction, optionally with a result value."
  (let ((inst (cl-cc/ir:make-ir-inst :result result)))
    inst))

;;; ─── ir-add-edge ────────────────────────────────────────────────────────────

(deftest ir-add-edge-behavior
  "ir-add-edge wires predecessor/successor links between blocks."
  (let* ((fn   (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next  (cl-cc/ir:ir-new-block fn :next)))
    ;; Before edge: no successors, no predecessors
    (assert-null (cl-cc/ir:irb-successors entry))
    (assert-null (cl-cc/ir:irb-predecessors next))
    (cl-cc/ir:ir-add-edge entry next)
    ;; After edge: entry has next as successor, next has entry as predecessor
    (assert-equal (list next)  (cl-cc/ir:irb-successors entry))
    (assert-equal (list entry) (cl-cc/ir:irb-predecessors next)))
  ;; Multiple edges from one block
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (b1    (cl-cc/ir:ir-new-block fn :b1))
         (b2    (cl-cc/ir:ir-new-block fn :b2)))
    (cl-cc/ir:ir-add-edge entry b1)
    (cl-cc/ir:ir-add-edge entry b2)
    (assert-= 2 (length (cl-cc/ir:irb-successors entry))))
  ;; Duplicate edge should not be added twice
  (let* ((fn    (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (b1    (cl-cc/ir:ir-new-block fn :b1)))
    (cl-cc/ir:ir-add-edge entry b1)
    (cl-cc/ir:ir-add-edge entry b1)
    (assert-= 1 (length (cl-cc/ir:irb-successors entry)))))

;;; ─── ir-emit / ir-set-terminator ────────────────────────────────────────────

(deftest ir-emit-behavior
  "ir-emit appends instructions to a block in order."
  (let* ((fn   (make-test-fn))
         (blk  (cl-cc/ir:irf-entry fn))
         (i1   (cl-cc/ir:make-ir-inst))
         (i2   (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit blk i1)
    (cl-cc/ir:ir-emit blk i2)
    (let ((body (cl-cc/ir:irb-insts blk)))
      (assert-= 2 (length body))
      (assert-eq i1 (first body))
      (assert-eq i2 (second body)))))

(deftest ir-set-terminator-stores
  "ir-set-terminator stores the given instruction as the block terminator."
  (let* ((fn   (make-test-fn))
         (blk  (cl-cc/ir:irf-entry fn))
         (term (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-set-terminator blk term)
    (assert-eq term (cl-cc/ir:irb-terminator blk))))

;;; ─── ir-rpo ─────────────────────────────────────────────────────────────────

(deftest-each ir-rpo-cases
  "ir-rpo returns blocks in reverse post-order for various graph shapes."
  :cases
  (("single-block"
    (lambda ()
      (let ((fn (make-test-fn)))
        (cl-cc/ir:ir-seal-block fn (cl-cc/ir:irf-entry fn))
        (values fn (list (cl-cc/ir:irf-entry fn)))))
    (lambda (fn expected-order)
      (assert-equal expected-order (cl-cc/ir:ir-rpo fn))))

   ("linear-chain"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (b1    (cl-cc/ir:ir-new-block fn :b1))
             (b2    (cl-cc/ir:ir-new-block fn :b2)))
        (cl-cc/ir:ir-add-edge entry b1)
        (cl-cc/ir:ir-add-edge b1 b2)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn b1)
        (cl-cc/ir:ir-seal-block fn b2)
        (values fn (list entry b1 b2))))
    (lambda (fn expected-order)
      (assert-equal expected-order (cl-cc/ir:ir-rpo fn))))

   ("diamond"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (left  (cl-cc/ir:ir-new-block fn :left))
             (right (cl-cc/ir:ir-new-block fn :right))
             (join  (cl-cc/ir:ir-new-block fn :join)))
        (cl-cc/ir:ir-add-edge entry left)
        (cl-cc/ir:ir-add-edge entry right)
        (cl-cc/ir:ir-add-edge left join)
        (cl-cc/ir:ir-add-edge right join)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn left)
        (cl-cc/ir:ir-seal-block fn right)
        (cl-cc/ir:ir-seal-block fn join)
        (values fn (list entry left right join))))
    (lambda (fn expected-order)
      (let ((rpo (cl-cc/ir:ir-rpo fn)))
        (assert-= 4 (length rpo))
        (assert-eq (first expected-order) (first rpo))
        (assert-eq (car (last expected-order)) (car (last rpo))))))

   ("back-edge-loop"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (body  (cl-cc/ir:ir-new-block fn :body))
             (exit  (cl-cc/ir:ir-new-block fn :exit)))
        (cl-cc/ir:ir-add-edge entry body)
        (cl-cc/ir:ir-add-edge body exit)
        (cl-cc/ir:ir-add-edge body body)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn body)
        (cl-cc/ir:ir-seal-block fn exit)
        (values fn nil)))
    (lambda (fn _)
      (let ((rpo (cl-cc/ir:ir-rpo fn)))
        (assert-= 3 (length rpo))
        (assert-true (member (cl-cc/ir:irf-entry fn) rpo))))))
  (setup verify)
  (multiple-value-bind (fn expected-order) (funcall setup)
    (funcall verify fn expected-order)))

;;; ─── ir-dominators ──────────────────────────────────────────────────────────

(deftest-each ir-dominators-cases
  "ir-dominators computes correct dominator sets."
  :cases
  (("entry-dominates-itself"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn)))
        (cl-cc/ir:ir-seal-block fn entry)
        (values fn entry (list (cons entry entry)))))
    (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom))))))

   ("linear-chain-dominators"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (b1    (cl-cc/ir:ir-new-block fn :b1))
             (b2    (cl-cc/ir:ir-new-block fn :b2)))
        (cl-cc/ir:ir-add-edge entry b1)
        (cl-cc/ir:ir-add-edge b1 b2)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn b1)
        (cl-cc/ir:ir-seal-block fn b2)
        (values fn entry (list (cons b1 entry) (cons b2 b1)))))
    (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom))))))

   ("diamond-join-dominated-by-entry"
    (lambda ()
      (let* ((fn    (make-test-fn))
             (entry (cl-cc/ir:irf-entry fn))
             (left  (cl-cc/ir:ir-new-block fn :left))
             (right (cl-cc/ir:ir-new-block fn :right))
             (join  (cl-cc/ir:ir-new-block fn :join)))
        (cl-cc/ir:ir-add-edge entry left)
        (cl-cc/ir:ir-add-edge entry right)
        (cl-cc/ir:ir-add-edge left join)
        (cl-cc/ir:ir-add-edge right join)
        (cl-cc/ir:ir-seal-block fn entry)
        (cl-cc/ir:ir-seal-block fn left)
        (cl-cc/ir:ir-seal-block fn right)
        (cl-cc/ir:ir-seal-block fn join)
        (values fn entry (list (cons left entry) (cons right entry) (cons join entry)))))
    (lambda (fn entry checks)
      (declare (ignore entry))
      (let ((idom (cl-cc/ir:ir-dominators fn)))
        (dolist (pair checks)
          (assert-eq (cdr pair) (gethash (car pair) idom)))))))
  (setup verify)
  (multiple-value-bind (fn entry checks) (funcall setup)
    (funcall verify fn entry checks)))
