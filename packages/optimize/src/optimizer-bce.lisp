;;;; optimizer-bce.lisp — FR-611 Bounds Check Elimination
(in-package :cl-cc/optimize)

(defun %opt-bce-record-hoist-metadata (inst block)
  "Mark INST as using one loop-entry bounds proof when possible."
  (let ((metadata (opt-bounds-check-eliminable-metadata inst)))
    (when (and metadata block (> (bb-loop-depth block) 0))
      (setf (gethash inst *opt-bounds-check-eliminable-metadata*)
            (append metadata (list :hoisted-check t
                                   :reason :loop-invariant-bounds-proof))))))

(unless (fboundp 'opt-pass-bounds-check-elimination)
  (defun opt-pass-bounds-check-elimination (instructions)
    "FR-611: annotate provably safe array accesses for bounds-check elimination.

For each `vm-aref`/`vm-aset`, this pass proves INDEX ∈ [0, LENGTH-1] using the
FR-610 path-sensitive VRP facts.  It also records a loop-hoist marker for accesses
inside counted loops: codegen keeps the single dominating proof/check and suppresses
the repeated inner access guard.  Unproven accesses remain fully checked."
    (let* ((cfg (cfg-build instructions))
           (length-regs (%opt-bce-array-length-regs instructions))
           (inst->block (%opt-bce-instruction-blocks cfg)))
      (clrhash *opt-bounds-check-eliminable-metadata*)
      (cfg-compute-dominators cfg)
      (cfg-compute-loop-depths cfg)
      (let ((ranges (opt-compute-path-sensitive-ranges instructions)))
        (dolist (inst instructions instructions)
          (when (typep inst '(or vm-aref vm-aset))
            (let* ((array-reg (%opt-bce-inst-array-reg inst))
                   (index-reg (%opt-bce-inst-index-reg inst))
                   (length-reg (and array-reg (gethash array-reg length-regs)))
                   (block (gethash inst inst->block)))
              (when (and index-reg length-reg
                         (opt-array-bounds-check-eliminable-p
                          index-reg length-reg ranges block))
                (opt-mark-bounds-check-eliminable inst
                                                   :array-reg array-reg
                                                   :index-reg index-reg
                                                   :length-reg length-reg
                                                   :block block)
                (%opt-bce-record-hoist-metadata inst block)))))))))
