;;;; tests/unit/optimize/optimizer-flow-block-tests.lisp
;;;; Unit tests for optimizer-flow.lisp — block/CFG helpers and correlation passes
;;;;
;;;; Covers: %block-mergeable-successor-p, %block-strip-merge-jump,
;;;;   cfg-block-temperature, opt-pass-branch-correlation,
;;;;   opt-pass-tail-merge (tail-duplication).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Straight-line preservation / empty-input ───────────────────────────

(deftest-each opt-passes-preserve-straight-line
  "dead-basic-blocks/nil-check-elim/block-merge/tail-merge all preserve straight-line code."
  :cases (("dead-basic-blocks" #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("nil-check-elim"    #'cl-cc/optimize:opt-pass-dominated-type-check-elim)
          ("block-merge"       #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"        #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (assert-true (listp result))))

(deftest-each opt-pass-returns-list-for-empty-input
  "opt-pass-dead-basic-blocks, opt-pass-block-merge, and opt-pass-tail-merge all return a list for empty input."
  :cases (("dead-basic-blocks"  #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("block-merge"        #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"         #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (assert-true (listp (funcall pass-fn nil))))

;;; ─── %type-check-elim helpers ───────────────────────────────────────────────

(deftest-each type-check-elim-lookup-fact-cases
  "%type-check-elim-lookup-fact finds matching pred+src or returns nil."
  :cases (("match"     'p :r0 t)
          ("wrong-src" 'p :r9 nil)
          ("wrong-pred" 'q :r0 nil))
  (pred src should-match)
  (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (result (cl-cc/optimize::%type-check-elim-lookup-fact facts pred src)))
    (if should-match
        (assert-true result)
        (assert-null result))))

;;; ─── %block-mergeable-successor-p / %block-strip-merge-jump / cfg-block-temperature ──

(deftest block-mergeable-successor-single-predecessor
  "%block-mergeable-successor-p returns T when the block's sole successor has exactly one predecessor."
  (let* ((blk  (%make-test-basic-block))
         (succ (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk))
    (assert-true (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest block-mergeable-successor-multiple-predecessors
  "%block-mergeable-successor-p returns NIL when the successor has more than one predecessor."
  (let* ((blk   (%make-test-basic-block))
         (other (%make-test-basic-block))
         (succ  (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk other))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest block-mergeable-multiple-successors
  "%block-mergeable-successor-p returns NIL when the block itself has more than one successor."
  (let* ((blk   (%make-test-basic-block))
         (succ1 (%make-test-basic-block))
         (succ2 (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-successors blk) (list succ1 succ2))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest-each block-strip-merge-jump-cases
  "%block-strip-merge-jump: matching label removes trailing jump; non-matching leaves unchanged; nil returns nil."
  :cases (("matching"     (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "next"))  "next" 1)
          ("non-matching" (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "other")) "next" 2)
          ("empty"        nil                                                                      "next" 0))
  (insts target expected-len)
  (assert-= expected-len (length (cl-cc/optimize::%block-strip-merge-jump insts target))))

(deftest-each cfg-block-cold-p-signal-error-and-ordinary
  "%cfg-block-cold-p returns T for error-signalling blocks and NIL for ordinary ones."
  :cases (("signal-error" (list (make-vm-signal-error :error-reg :r0)) t)
          ("ordinary"     (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)) nil))
  (instructions expect-cold)
  (let ((blk (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions blk) instructions)
    (if expect-cold
        (assert-true  (cl-cc/optimize::%cfg-block-cold-p blk))
        (assert-false (cl-cc/optimize::%cfg-block-cold-p blk)))))

(deftest cfg-block-hotter-p-cold-vs-warm
  "%cfg-block-hotter-p returns NIL when A is cold; returns T when A is warm and B is cold."
  (let ((cold (%make-test-basic-block))
        (warm (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions cold) (list (make-vm-signal-error :error-reg :r0))
          (cl-cc/optimize:bb-instructions warm) (list (make-vm-const :dst :r0 :value 1)))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p cold warm))
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p warm cold))))

(deftest cfg-block-hotter-p-loop-depth
  "%cfg-block-hotter-p returns T when A has greater loop depth than B."
  (let ((deep    (%make-test-basic-block))
        (shallow (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-loop-depth  deep)    3
          (cl-cc/optimize:bb-loop-depth  shallow) 1
          (cl-cc/optimize:bb-instructions deep)    nil
          (cl-cc/optimize:bb-instructions shallow) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p deep shallow))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p shallow deep))))

(deftest cfg-block-hotter-p-rpo-index-tiebreaker
  "%cfg-block-hotter-p uses RPO index as a tiebreaker when loop depths are equal."
  (let ((first  (%make-test-basic-block))
        (second (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-loop-depth first)   1
          (cl-cc/optimize:bb-loop-depth second)  1
          (cl-cc/optimize:bb-rpo-index  first)   0
          (cl-cc/optimize:bb-rpo-index  second)  1
          (cl-cc/optimize:bb-instructions first)  nil
          (cl-cc/optimize:bb-instructions second) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p first second))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p second first))))

(deftest branch-correlation-propagates-through-forwarder
  "opt-pass-branch-correlation propagates fact through a jump-only forwarder block."
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (join   (make-vm-label :name "join"))
         (p2     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp join p2 ret2))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)
                  (eql (cl-cc/vm::vm-value i) 0)))
           out))))

(deftest branch-correlation-forwarder-does-not-propagate-on-mismatch
  "opt-pass-branch-correlation does not propagate when a forwarder edge disagrees."
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (otherp (make-vm-label :name "otherp"))
         (p2     (cl-cc:make-vm-integer-p :dst :r3 :src :r4))
         (br2    (make-vm-jump-zero :reg :r3 :label "join"))
         (join   (make-vm-label :name "join"))
         (p3     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp otherp p2 br2 join p3 ret2))))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)))
           out))))

(deftest tail-duplication-duplicates-small-shared-tail
  "opt-pass-tail-duplication duplicates a small shared tail into predecessors."
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "p2")
                      (make-vm-label :name "p2")
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (ret-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-ret)) out)))
    (assert-= 1 jump-to-tail)
    (assert-= 1 ret-count)))

(deftest tail-duplication-skips-large-tail
  "opt-pass-tail-duplication leaves jumps when tail block exceeds duplication budget."
  (let* ((large-tail (append (loop for i from 0 below 13
                                   collect (make-vm-const :dst (intern (format nil "R~D" i) :keyword)
                                                          :value i))
                             (list (make-vm-ret :reg :r12))))
         (insts (append (list (make-vm-jump-zero :reg :cond :label "p2")
                              (make-vm-label :name "p1")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "p2")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "tail"))
                        large-tail))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                  out)))
    (assert-= 2 jump-to-tail)))

(deftest tail-duplication-duplicates-larger-tail-up-to-threshold
  "opt-pass-tail-duplication duplicates shared tail blocks up to the 12-instruction budget."
  (let* ((tail (append (loop for i from 0 below 11
                             collect (make-vm-const :dst (intern (format nil "T~D" i) :keyword)
                                                    :value i))
                       (list (make-vm-ret :reg :t10))))
         (insts (append (list (make-vm-jump-zero :reg :cond :label "p2")
                              (make-vm-label :name "p1")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "p2")
                              (make-vm-jump :label "tail")
                              (make-vm-label :name "tail"))
                        tail))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (ret-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-ret)) out)))
    (assert-= 0 jump-to-tail)
    (assert-true (>= ret-count 2))))

(deftest tail-duplication-handles-conditional-predecessor-target
  "opt-pass-tail-duplication duplicates a shared tail through a conditional edge pad."
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-jump-zero :reg :cond :label "tail")
                      (make-vm-label :name "p2")
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-zero-to-tail (count-if (lambda (i)
                                        (and (typep i 'cl-cc/vm::vm-jump-zero)
                                             (equal (cl-cc/vm::vm-label-name i) "tail")))
                                      out))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-= 0 jump-zero-to-tail)
    (assert-= 0 jump-to-tail)
    (assert-true (>= add-count 2))))
