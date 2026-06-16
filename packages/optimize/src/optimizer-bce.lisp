;;;; optimizer-bce.lisp — FR-611 Bounds Check Elimination
(in-package :cl-cc/optimize)

(defun %opt-bce-record-hoist-metadata (inst block)
  "Mark INST as using one loop-entry bounds proof when possible."
  (let ((metadata (opt-bounds-check-eliminable-metadata inst)))
    (when (and metadata block (> (bb-loop-depth block) 0))
      (setf (gethash inst *opt-bounds-check-eliminable-metadata*)
            (append metadata (list :hoisted-check t
                                   :reason :loop-invariant-bounds-proof))))))
