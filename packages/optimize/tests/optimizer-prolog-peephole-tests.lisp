;;;; packages/optimize/tests/optimizer-prolog-peephole-tests.lisp
;;;;
;;;; Relocated from packages/prolog/tests/prolog-peephole-tests.lisp and
;;;; prolog-peephole-tests-internal.lisp as part of the migration off cl-cc's
;;;; own homegrown Prolog engine onto the external cl-prolog-kit library — this
;;;; is genuine cl-cc functionality (the peephole optimizer), unlike the rest
;;;; of packages/prolog/tests, which tested the now-removed engine's own
;;;; unification/DCG/builtin-dispatch machinery (now the external cl-prolog-kit
;;;; library's responsibility to test, not cl-cc's). The two TYPE-OF/3 tests
;;;; from the original file exercised packages/prolog's dead (no production
;;;; caller) declarative type-inference rules and were dropped, not ported.

(in-package :cl-cc/test)


(it-sequential "prolog-peephole-equality-cases const-move-fusion"
  (destructuring-bind (input expected) (list '((:const :r0 42) (:move :r1 :r0)) '((:const :r1 42)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-equality-cases passthrough"
  (destructuring-bind (input expected) (list '((:add :r2 :r0 :r1)) '((:add :r2 :r0 :r1)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-equality-cases single-instruction"
  (destructuring-bind (input expected) (list '((:const :r0 7)) '((:const :r0 7)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-equality-cases jump-chain-shortest"
  (destructuring-bind (input expected) (list '((:jump "L1") (:jump "L2")) '((:jump "L1")))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-self-move-removal"
  (let ((results (cl-cc/optimize:apply-prolog-peephole '((:move :r0 :r0) (:const :r1 1)))))
    (expect (member '(:move :r0 :r0) results :test #'equal) :to-be-falsy)))

(it-sequential "prolog-peephole-arithmetic-identities add-zero"
  (destructuring-bind (input expected) (list '((:add :r2 :r0 0) (:const :r3 1)) '((:move :r2 :r0) (:const :r3 1)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities sub-from-zero"
  (destructuring-bind (input expected) (list '((:sub :r4 0 :r1) (:const :r5 2)) '((:neg :r4 :r1) (:const :r5 2)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities sub-same"
  (destructuring-bind (input expected) (list '((:sub :r4 :r1 :r1) (:const :r5 2)) '((:const :r4 0) (:const :r5 2)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities mul-zero"
  (destructuring-bind (input expected) (list '((:mul :r6 :r1 0) (:const :r7 3)) '((:const :r6 0) (:const :r7 3)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities div-one"
  (destructuring-bind (input expected) (list '((:div :r8 :r2 1) (:const :r9 4)) '((:move :r8 :r2) (:const :r9 4)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities logand-all"
  (destructuring-bind (input expected) (list '((:logand :r10 :r3 -1) (:const :r11 5)) '((:move :r10 :r3) (:const :r11 5)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities logand-zero"
  (destructuring-bind (input expected) (list '((:logand :r10 :r3 0) (:const :r11 5)) '((:const :r10 0) (:const :r11 5)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities logior-all"
  (destructuring-bind (input expected) (list '((:logior :r10 :r3 -1) (:const :r11 5)) '((:const :r10 -1) (:const :r11 5)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities num-eq-same"
  (destructuring-bind (input expected) (list '((:num-eq :r12 :r4 :r4) (:const :r13 6)) '((:const :r12 1) (:const :r13 6)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-arithmetic-identities logxor-same"
  (destructuring-bind (input expected) (list '((:logxor :r14 :r5 :r5) (:const :r15 7)) '((:const :r14 0) (:const :r15 7)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-same-reg-identities eq"
  (destructuring-bind (input expected) (list '((:eq :r0 :r1 :r1) (:const :r2 1)) '((:const :r0 1) (:const :r2 1)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-same-reg-identities gt"
  (destructuring-bind (input expected) (list '((:gt :r3 :r4 :r4) (:const :r5 2)) '((:const :r3 0) (:const :r5 2)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-same-reg-identities le"
  (destructuring-bind (input expected) (list '((:le :r6 :r7 :r7) (:const :r8 3)) '((:const :r6 1) (:const :r8 3)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-same-reg-identities logand"
  (destructuring-bind (input expected) (list '((:logand :r9 :r10 :r10) (:const :r11 4)) '((:move :r9 :r10) (:const :r11 4)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-same-reg-identities logior"
  (destructuring-bind (input expected) (list '((:logior :r12 :r13 :r13) (:const :r14 5)) '((:move :r12 :r13) (:const :r14 5)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-negated-comparisons lt->ge"
  (destructuring-bind (input expected) (list '((:lt :r0 :r1 :r2) (:not :r3 :r0)) '((:ge :r3 :r1 :r2)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-negated-comparisons gt->le"
  (destructuring-bind (input expected) (list '((:gt :r4 :r5 :r6) (:not :r7 :r4)) '((:le :r7 :r5 :r6)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-negated-comparisons le->gt"
  (destructuring-bind (input expected) (list '((:le :r8 :r9 :r10) (:not :r11 :r8)) '((:gt :r11 :r9 :r10)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-negated-comparisons ge->lt"
  (destructuring-bind (input expected) (list '((:ge :r12 :r13 :r14) (:not :r15 :r12)) '((:lt :r15 :r13 :r14)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-empty-input"
  (expect (cl-cc/optimize:apply-prolog-peephole nil) :to-equal nil))

(it-sequential "prolog-peephole-multiple-pairs"
  (expect (cl-cc/optimize:apply-prolog-peephole
                 '((:const :r0 1) (:move :r1 :r0) (:add :r2 :r1 :r1))) :to-equal '((:const :r1 1) (:add :r2 :r1 :r1))))

(it-sequential "prolog-peephole-single-pair-reduction jump-to-label"
  (destructuring-bind (input expected-first) (list '((:jump "L0") (:label "L0")) '(:label "L0"))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal (list expected-first))))

(it-sequential "prolog-peephole-single-pair-reduction double-const"
  (destructuring-bind (input expected-first) (list '((:const :r0 1) (:const :r0 99)) '(:const :r0 99))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal (list expected-first))))

(it-sequential "prolog-peephole-terminal-sequence-rules jump-before-ret"
  (destructuring-bind (input expected) (list '((:jump "L1") (:ret  :r0)) '((:jump "L1")))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules jump-before-halt"
  (destructuring-bind (input expected) (list '((:jump "L1") (:halt :r0)) '((:jump "L1")))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules ret-before-jump"
  (destructuring-bind (input expected) (list '((:ret  :r0) (:jump "L1")) '((:ret  :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules halt-before-jump"
  (destructuring-bind (input expected) (list '((:halt :r0) (:jump "L1")) '((:halt :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules ret-before-ret"
  (destructuring-bind (input expected) (list '((:ret  :r0) (:ret  :r1)) '((:ret  :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules halt-before-halt"
  (destructuring-bind (input expected) (list '((:halt :r0) (:halt :r1)) '((:halt :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules ret-before-halt"
  (destructuring-bind (input expected) (list '((:ret  :r0) (:halt :r1)) '((:ret  :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-terminal-sequence-rules halt-before-ret"
  (destructuring-bind (input expected) (list '((:halt :r0) (:ret  :r1)) '((:halt :r0)))
    (expect (cl-cc/optimize:apply-prolog-peephole input) :to-equal expected)))

(it-sequential "prolog-peephole-move-chain-propagates-source"
  (expect (cl-cc/optimize:apply-prolog-peephole
                 '((:move :r1 :r0) (:move :r2 :r1))) :to-equal '((:move :r1 :r0) (:move :r2 :r0))))

(it-sequential "prolog-peephole-pair-preserved jump-label-mismatch"
  (destructuring-bind (insts) (list '((:jump "L1") (:label "L0")))
    (expect (cl-cc/optimize:apply-prolog-peephole insts) :to-equal insts)))

(it-sequential "prolog-peephole-pair-preserved const-different-regs"
  (destructuring-bind (insts) (list '((:const :r0 1) (:const :r1 2)))
    (expect (cl-cc/optimize:apply-prolog-peephole insts) :to-equal insts)))

(it-sequential "prolog-peephole-pair-preserved move-different-source"
  (destructuring-bind (insts) (list '((:move :r1 :r0) (:move :r3 :r2)))
    (expect (cl-cc/optimize:apply-prolog-peephole insts) :to-equal insts)))

;;; %peephole-walk unit tests (internal helper)

(it-sequential "peephole-walk-direct-cases nil"
  (destructuring-bind (input expected) (list nil nil)
    (expect (cl-cc/optimize::%peephole-walk input nil) :to-equal expected)))

(it-sequential "peephole-walk-direct-cases singleton"
  (destructuring-bind (input expected) (list '((:const :r0 1)) '((:const :r0 1)))
    (expect (cl-cc/optimize::%peephole-walk input nil) :to-equal expected)))

(it-sequential "peephole-walk-direct-cases no-match"
  (destructuring-bind (input expected) (list '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1)) '((:add :r1 :r0 :r0) (:sub :r2 :r1 :r1)))
    (expect (cl-cc/optimize::%peephole-walk input nil) :to-equal expected)))

(it-sequential "peephole-walk-matching-pair-fused"
  (expect (cl-cc/optimize::%peephole-walk
                 '((:const :r0 99) (:move :r1 :r0))
                 nil) :to-equal '((:const :r1 99))))

(it-sequential "peephole-walk-out-accumulator-is-prepended"
  (expect (cl-cc/optimize::%peephole-walk
                 '((:const :r0 1))
                 '((:add :r2 :r1 :r1))) :to-equal '((:add :r2 :r1 :r1) (:const :r0 1))))
