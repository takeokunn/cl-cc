;;;; tests/optimizer-cfg-inline-tests.lisp — CFG Reachability and Inlining Pass Tests
;;; Extracted from optimizer-tests.lisp (> 700 lines).

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; ── CFG Reachability / Label Cleanup / Block Merge ───────────────────────

(deftest unreachable-code-after-jump
  "opt-pass-unreachable drops dead code between an unconditional jump and the next label."
  (let* ((j    (make-vm-jump  :label "end"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl  (make-vm-label :name "end"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list j dead lbl ret))))
    (assert-false (member dead out))
    (assert-true  (member j    out))
    (assert-true  (member lbl  out))
    (assert-true  (member ret  out))))

(deftest unreachable-code-after-ret
  "opt-pass-unreachable drops dead code after a ret; the next label revives reachability."
  (let* ((ret1 (make-vm-ret   :reg :r0))
         (dead (make-vm-const :dst :r1 :value 0))
         (lbl  (make-vm-label :name "after"))
         (ret2 (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-unreachable (list ret1 dead lbl ret2))))
    (assert-false (member dead out))
    (assert-true  (member ret1 out))
    (assert-true  (member lbl  out))))

(deftest unreachable-label-revives-reachability
  "A vm-label after a jump revives reachability."
  (let* ((j   (make-vm-jump  :label "lbl"))
         (lbl (make-vm-label :name "lbl"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-unreachable (list j lbl c ret))))
    (assert-true (member c out))))

(deftest dead-labels-removes-unreferenced-label
  "A label with no jumps pointing to it is removed."
  (let* ((lbl (make-vm-label :name "ghost"))
         (c   (make-vm-const :dst :r0 :value 1))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list lbl c ret))))
    (assert-false (member lbl out))
    (assert-true  (member c   out))
    (assert-true  (member ret out))))

(deftest-each dead-labels-preserves-referenced-labels
  "Labels referenced by jumps, closures, or handler instrs are preserved by opt-pass-dead-labels."
  :cases (("jump-target"
            (make-vm-jump :label "live") "live")
          ("closure-entry"
           (make-vm-closure :dst :r0 :label "fn" :params nil :captured nil
                            :optional-params nil :rest-param nil :key-params nil) "fn")
          ("handler-label"
           (cl-cc::make-vm-establish-handler :handler-label "err-handler"
                                              :result-reg :r0 :error-type 'error)
           "err-handler"))
  (ref-inst lbl-name)
  (let* ((lbl (make-vm-label :name lbl-name))
         (ret (make-vm-ret   :reg :r0))
         (out (cl-cc/optimize::opt-pass-dead-labels (list ref-inst lbl ret))))
    (assert-true (member lbl out))))

(deftest dead-basic-blocks-eliminated
  "opt-pass-dead-basic-blocks removes an unreachable labeled block, not just its label."
  (let* ((j    (make-vm-jump  :label "exit"))
         (lbl1 (make-vm-label :name "dead"))
         (dead (make-vm-const :dst :r1 :value 99))
         (lbl2 (make-vm-label :name "exit"))
         (ret  (make-vm-ret   :reg :r0))
         (out  (cl-cc/optimize::opt-pass-dead-basic-blocks (list j lbl1 dead lbl2 ret))))
    (assert-true  (member j out))
    (assert-false (member lbl1 out))
    (assert-false (member dead out))
    (assert-true  (member lbl2 out))
    (assert-true  (member ret out))))

(deftest block-merge-eliminates-single-pred-label
  "opt-pass-block-merge removes a mergeable successor label and its jump."
  (let* ((start (make-vm-label :name "start"))
         (c1    (make-vm-const :dst :r1 :value 1))
         (jmp   (make-vm-jump :label "mid"))
         (mid   (make-vm-label :name "mid"))
         (c2    (make-vm-const :dst :r2 :value 2))
         (ret   (make-vm-ret :reg :r2))
         (out   (cl-cc/optimize::opt-pass-block-merge (list start c1 jmp mid c2 ret))))
    (assert-false (member mid out))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) out))
    (assert-true  (member c1 out))
    (assert-true  (member c2 out))
    (assert-true  (member ret out))))

(deftest tail-merge-merges-identical-blocks
  "opt-pass-tail-merge merges duplicate CFG blocks with identical bodies."
  (let* ((entry (make-vm-label :name "entry"))
         (seed  (make-vm-const :dst :r0 :value nil))
         (br    (make-vm-jump-zero :reg :r0 :label "dup2"))
         (dup1  (make-vm-label :name "dup1"))
         (a1    (make-vm-const :dst :r1 :value 1))
         (j1    (make-vm-jump :label "exit"))
         (dup2  (make-vm-label :name "dup2"))
         (a2    (make-vm-const :dst :r1 :value 1))
         (j2    (make-vm-jump :label "exit"))
         (exit  (make-vm-label :name "exit"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-tail-merge (list entry seed br dup1 a1 j1 dup2 a2 j2 exit ret))))
    (assert-true  (member dup1 out))
    (assert-false (member dup2 out))
    (assert-equal 1 (count-if (lambda (i)
                                (and (typep i 'cl-cc/vm::vm-const)
                                     (eq :r1 (cl-cc/vm::vm-dst i))))
                              out))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))))

(deftest constant-hoist-moves-loop-constant-to-preheader
  "opt-pass-constant-hoist hoists a loop-invariant constant before the loop header."
  (let* ((start (make-vm-label :name "start"))
         (seed  (make-vm-const :dst :r0 :value 0))
         (jmp1  (make-vm-jump :label "loop"))
         (loop  (make-vm-label :name "loop"))
         (hoist (make-vm-const :dst :r1 :value 99))
         (jmp2  (make-vm-jump :label "body"))
         (body  (make-vm-label :name "body"))
         (back  (make-vm-jump :label "loop"))
         (ret   (make-vm-ret :reg :r1))
         (out   (cl-cc/optimize::opt-pass-constant-hoist
                 (list start seed jmp1 loop hoist jmp2 body back ret))))
    (assert-true (member hoist out))
    (assert-true (member loop out))
    (assert-true (< (position hoist out :test #'eq)
                    (position loop out :test #'eq)))))

(deftest-each opt-convergence-pass-membership
  "All expected optimization passes are registered in *opt-convergence-passes*."
  :cases (("constant-hoist" #'cl-cc/optimize::opt-pass-constant-hoist)
          ("global-dce"     #'cl-cc/optimize::opt-pass-global-dce)
          ("inline"         #'cl-cc/optimize::opt-pass-inline-iterative)
          ("pre"            #'cl-cc/optimize::opt-pass-pre)
          ("bswap"          #'cl-cc/optimize::opt-pass-bswap-recognition)
          ("rotate"         #'cl-cc/optimize::opt-pass-rotate-recognition))
  (pass-fn)
  (assert-true (member pass-fn cl-cc/optimize::*opt-convergence-passes* :test #'eq)))

(deftest optimizer-batch-concatenate
  "Adjacent vm-concatenate instructions are packed into one parts-list form."
  (let* ((i1 (cl-cc::make-vm-const :dst :R0 :value "a"))
         (i2 (cl-cc::make-vm-const :dst :R1 :value "b"))
         (i3 (cl-cc::make-vm-const :dst :R2 :value "c"))
         (c1 (cl-cc::make-vm-concatenate :dst :R3 :str1 :R0 :str2 :R1))
         (c2 (cl-cc::make-vm-concatenate :dst :R4 :str1 :R3 :str2 :R2))
         (out (cl-cc/optimize::opt-pass-batch-concatenate (list i1 i2 i3 c1 c2)))
         (inst (find-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out)))
    (assert-equal 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-concatenate)) out))
    (assert-true inst)
    (assert-equal '(:R0 :R1 :R2) (cl-cc/vm::vm-parts inst))
    (assert-equal :R4 (cl-cc/vm::vm-dst inst))))

