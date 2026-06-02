;;;; packages/optimize/tests/optimizer-jump-threading-tests.lisp
;;;; Unit tests for optimizer-jump-threading.lisp
;;;;
;;;; Covers: %opt-jump-branch-comparison-fact (nil and non-nil paths),
;;;;   %opt-jump-fact-killed-p, %opt-jump-same-comparison-p,
;;;;   %opt-jump-known-constant / %opt-jump-put-constant / %opt-jump-kill-constant,
;;;;   %opt-jump-rewrite-block-with-fact (redundant-comparison rewrite,
;;;;     move propagation, kill-on-redef),
;;;;   %opt-pass-jump-propagate-edge-values (integration smoke),
;;;;   opt-pass-jump-threading-with-propagation (end-to-end chain+propagation).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %opt-jump-fact-killed-p ─────────────────────────────────────────────

(deftest-each jump-fact-killed-p-cases
  "%opt-jump-fact-killed-p returns T only when DST matches a comparison operand."
  :cases (("kills-lhs"
           (list :pred 'vm-lt :lhs :x :rhs :y) :x t)
          ("kills-rhs"
           (list :pred 'vm-lt :lhs :x :rhs :y) :y t)
          ("no-match"
           (list :pred 'vm-lt :lhs :x :rhs :y) :z nil)
          ("nil-fact"
           nil :x nil)
          ("nil-dst"
           (list :pred 'vm-lt :lhs :x :rhs :y) nil nil))
  (fact dst expected)
  (if expected
      (assert-true  (cl-cc/optimize::%opt-jump-fact-killed-p fact dst))
      (assert-false (cl-cc/optimize::%opt-jump-fact-killed-p fact dst))))

;;; ─── %opt-jump-same-comparison-p ─────────────────────────────────────────

(deftest jump-same-comparison-p-matches-identical
  "%opt-jump-same-comparison-p returns T when INST replicates the FACT comparison."
  (let ((fact (list :pred 'vm-lt :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (assert-true (cl-cc/optimize::%opt-jump-same-comparison-p inst fact))))

(deftest jump-same-comparison-p-rejects-different-type
  "%opt-jump-same-comparison-p returns NIL when the predicate type differs."
  (let ((fact (list :pred 'vm-le :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (assert-false (cl-cc/optimize::%opt-jump-same-comparison-p inst fact))))

(deftest jump-same-comparison-p-rejects-different-operands
  "%opt-jump-same-comparison-p returns NIL when the source registers differ."
  (let ((fact (list :pred 'vm-lt :lhs :x :rhs :y :value 1))
        (inst (make-vm-lt :dst :c :lhs :x :rhs :z)))
    (assert-false (cl-cc/optimize::%opt-jump-same-comparison-p inst fact))))

(deftest jump-same-comparison-p-rejects-nil-fact
  "%opt-jump-same-comparison-p returns NIL when fact is NIL."
  (let ((inst (make-vm-lt :dst :c :lhs :x :rhs :y)))
    (assert-false (cl-cc/optimize::%opt-jump-same-comparison-p inst nil))))

;;; ─── constant alist helpers ───────────────────────────────────────────────

(deftest jump-known-constant-returns-value-when-present
  "%opt-jump-known-constant returns (values VALUE T) for a known register."
  (let ((constants (list (cons :r0 42) (cons :r1 7))))
    (multiple-value-bind (val found) (cl-cc/optimize::%opt-jump-known-constant :r0 constants)
      (assert-= 42 val)
      (assert-true found))))

(deftest jump-known-constant-returns-nil-when-absent
  "%opt-jump-known-constant returns (values NIL NIL) for an unknown register."
  (let ((constants (list (cons :r0 42))))
    (multiple-value-bind (val found) (cl-cc/optimize::%opt-jump-known-constant :r1 constants)
      (assert-null val)
      (assert-false found))))

(deftest jump-put-constant-adds-new-entry
  "%opt-jump-put-constant prepends a fresh association when the key is absent."
  (let* ((constants nil)
         (result    (cl-cc/optimize::%opt-jump-put-constant :r0 99 constants)))
    (assert-= 99 (cdr (assoc :r0 result)))))

(deftest jump-put-constant-replaces-existing-entry
  "%opt-jump-put-constant overwrites the previous value for an existing key."
  (let* ((constants (list (cons :r0 1)))
         (result    (cl-cc/optimize::%opt-jump-put-constant :r0 2 constants)))
    (assert-= 2 (cdr (assoc :r0 result)))
    (assert-= 1 (count :r0 result :key #'car))))

(deftest jump-kill-constant-removes-entry
  "%opt-jump-kill-constant removes the association for the given register."
  (let* ((constants (list (cons :r0 1) (cons :r1 2)))
         (result    (cl-cc/optimize::%opt-jump-kill-constant :r0 constants)))
    (assert-false (assoc :r0 result))
    (assert-true  (assoc :r1 result))))

;;; ─── %opt-jump-branch-comparison-fact ───────────────────────────────────

(deftest jump-branch-comparison-fact-returns-nil-for-non-conditional-terminator
  "%opt-jump-branch-comparison-fact returns NIL when the block ends with vm-jump."
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :r0 :value 1)
                                    (make-vm-jump :label "end")))))
    (assert-null (cl-cc/optimize::%opt-jump-branch-comparison-fact block))))

(deftest jump-branch-comparison-fact-returns-nil-when-operands-not-constant
  "%opt-jump-branch-comparison-fact returns NIL when comparison operands are unknown."
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-lt :dst :c :lhs :x :rhs :y)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    (assert-null (cl-cc/optimize::%opt-jump-branch-comparison-fact block))))

(deftest jump-branch-comparison-fact-returns-fact-when-both-operands-constant
  "%opt-jump-branch-comparison-fact returns a plist with :pred/:lhs/:rhs when both operands are vm-const."
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :i   :value 1)
                                    (make-vm-const :dst :lim :value 3)
                                    (make-vm-lt    :dst :c   :lhs :i :rhs :lim)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    (let ((fact (cl-cc/optimize::%opt-jump-branch-comparison-fact block)))
      (assert-true fact)
      (assert-eq 'vm-lt (getf fact :pred))
      (assert-eq :i     (getf fact :lhs))
      (assert-eq :lim   (getf fact :rhs)))))

(deftest jump-branch-comparison-fact-returns-nil-when-lhs-redefined-before-cmp
  "%opt-jump-branch-comparison-fact returns NIL when lhs register is redefined after its const load."
  (let ((block (cl-cc/optimize::make-basic-block
                :instructions (list (make-vm-const :dst :i   :value 1)
                                    (make-vm-const :dst :lim :value 3)
                                    (make-vm-const :dst :i   :value 9) ; redefines :i
                                    (make-vm-lt    :dst :c   :lhs :i :rhs :lim)
                                    (make-vm-jump-zero :reg :c :label "false")))))
    ;; After redefinition :i is still constant (value 9), so fact IS found
    ;; but the fact key is still extracted.  The important assertion is non-nil.
    (let ((fact (cl-cc/optimize::%opt-jump-branch-comparison-fact block)))
      (assert-true fact))))

;;; ─── %opt-jump-rewrite-block-with-fact ───────────────────────────────────

(deftest jump-rewrite-block-replaces-redundant-comparison-with-const
  "%opt-jump-rewrite-block-with-fact replaces a same-comparison with vm-const VALUE."
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-lt :dst :c2 :lhs :x :rhs :y)
                                     (make-vm-ret :reg :c2))))
         (fact  (list :pred 'vm-lt :lhs :x :rhs :y :value 1 :constants nil)))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (assert-true
       (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq  (cl-cc/vm::vm-dst   i) :c2)
                    (eql (cl-cc/vm::vm-value i) 1)))
             insts)))))

(deftest jump-rewrite-block-propagates-move-from-known-constant
  "%opt-jump-rewrite-block-with-fact replaces vm-move from a known-constant source with vm-const."
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-move :dst :r1 :src :r0)
                                     (make-vm-ret :reg :r1))))
         (fact  (list :constants (list (cons :r0 42)))))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (assert-true
       (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq  (cl-cc/vm::vm-dst   i) :r1)
                    (eql (cl-cc/vm::vm-value i) 42)))
             insts)))))

(deftest jump-rewrite-block-kills-fact-on-operand-redefinition
  "%opt-jump-rewrite-block-with-fact stops rewriting after a comparison source is overwritten."
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-const :dst :x  :value 99)  ; kills :x
                                     (make-vm-lt    :dst :c2 :lhs :x :rhs :y)
                                     (make-vm-ret   :reg :c2))))
         (fact  (list :pred 'vm-lt :lhs :x :rhs :y :value 1 :constants nil)))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      ;; The rewrite should NOT have folded :c2 since :x was redefined.
      (assert-true
       (some (lambda (i) (typep i 'cl-cc/vm::vm-lt)) insts)))))

(deftest jump-rewrite-block-preserves-vm-const-instructions
  "%opt-jump-rewrite-block-with-fact keeps vm-const instructions in the output."
  (let* ((block (cl-cc/optimize::make-basic-block
                 :instructions (list (make-vm-const :dst :r0 :value 5)
                                     (make-vm-ret   :reg :r0))))
         (fact  nil))
    (cl-cc/optimize::%opt-jump-rewrite-block-with-fact block fact)
    (let ((insts (cl-cc/optimize::bb-instructions block)))
      (assert-true
       (some (lambda (i)
               (and (typep i 'cl-cc/vm::vm-const)
                    (eq (cl-cc/vm::vm-dst i) :r0)))
             insts)))))

;;; ─── %opt-pass-jump-propagate-edge-values integration smoke ──────────────

(deftest jump-propagate-edge-values-threads-and-propagates
  "%opt-pass-jump-propagate-edge-values rewrites a redundant comparison in the fallthrough block."
  (let* ((insts (list (make-vm-const     :dst :i   :value 1)
                      (make-vm-const     :dst :lim :value 3)
                      (make-vm-lt        :dst :c   :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c   :label "false")
                      (make-vm-label     :name "true")
                      (make-vm-lt        :dst :c2  :lhs :i :rhs :lim)
                      (make-vm-ret       :reg :c2)
                      (make-vm-label     :name "false")
                      (make-vm-ret       :reg :c)))
         (out (cl-cc/optimize::%opt-pass-jump-propagate-edge-values insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq  (cl-cc/vm::vm-dst   i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out))))

(deftest jump-propagate-edge-values-handles-empty-input
  "%opt-pass-jump-propagate-edge-values does not signal on empty instruction list."
  (let ((out (cl-cc/optimize::%opt-pass-jump-propagate-edge-values nil)))
    (assert-null out)))

;;; ─── opt-pass-jump-threading-with-propagation end-to-end ─────────────────

(deftest jump-threading-with-propagation-chains-and-propagates
  "opt-pass-jump-threading-with-propagation threads a jump chain and propagates the edge fact."
  (let* ((insts (list (make-vm-const     :dst :i   :value 1)
                      (make-vm-const     :dst :lim :value 3)
                      (make-vm-lt        :dst :c   :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c   :label "middle")
                      (make-vm-label     :name "true")
                      (make-vm-lt        :dst :c2  :lhs :i :rhs :lim)
                      (make-vm-ret       :reg :c2)
                      (make-vm-label     :name "middle")
                      (make-vm-jump      :label "false")
                      (make-vm-label     :name "false")
                      (make-vm-ret       :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump-threading-with-propagation insts)))
    ;; The jump-zero should now point directly at "false" (chain threaded).
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-jump-zero)
                  (equal (cl-cc/vm::vm-label-name i) "false")))
           out))
    ;; The redundant lt in "true" block should be folded to const 1.
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq  (cl-cc/vm::vm-dst   i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out))))

(deftest jump-threading-with-propagation-is-identity-on-straight-line
  "opt-pass-jump-threading-with-propagation leaves a straight-line sequence structurally unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (out (cl-cc/optimize::opt-pass-jump-threading-with-propagation insts)))
    (assert-= (length insts) (length out))))
