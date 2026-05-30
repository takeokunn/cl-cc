(in-package :cl-cc/optimize)
;;;; ─── Branch Prediction Weight Analysis ─────────────────────────────────

(defparameter *opt-cold-block-instruction-types*
  '(vm-signal vm-error-instruction vm-cerror vm-warn)
  "Instruction types that make their containing basic block cold/unlikely.")

(defstruct (vm-branch-weighted-jump-zero
            (:include vm-jump-zero)
            (:constructor make-vm-branch-weighted-jump-zero
                (&key reg label branch-weight)))
  "A vm-jump-zero carrying optimizer-only branch prediction metadata."
  (branch-weight nil))

(defun opt-branch-weight (inst)
  "Return branch prediction metadata for INST, or NIL when it is unannotated."
  (when (typep inst 'vm-branch-weighted-jump-zero)
    (vm-branch-weighted-jump-zero-branch-weight inst)))

(defun %opt-cold-block-inst-p (inst)
  "Return T when INST identifies an error/diagnostic cold path."
  (member (type-of inst) *opt-cold-block-instruction-types* :test #'eq))

(defun %opt-label-positions (instructions)
  "Return a label-name → instruction-index table for INSTRUCTIONS."
  (let ((positions (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) positions) i))
    positions))

(defun %opt-target-block-has-terminal-p (vec start-index predicate)
  "Return T when target block starting at START-INDEX contains a matching terminator."
  (when start-index
    (loop for i from start-index below (length vec)
          for inst = (aref vec i)
          do (cond
               ((and (> i start-index) (typep inst 'vm-label))
                (return nil))
               ((funcall predicate inst)
                (return t)))
          finally (return nil))))

(defun %opt-cold-labels (instructions)
  "Return a table of labels whose basic blocks contain cold instructions."
  (let ((cold-labels (make-hash-table :test #'equal))
        (current-label nil)
        (current-cold-p nil))
    (labels ((flush-current ()
               (when (and current-label current-cold-p)
                 (setf (gethash current-label cold-labels) t))))
      (dolist (inst instructions cold-labels)
        (cond
          ((typep inst 'vm-label)
           (flush-current)
           (setf current-label (vm-name inst)
                 current-cold-p nil))
          ((%opt-cold-block-inst-p inst)
           (setf current-cold-p t))))
      (flush-current))))

(defun %opt-branch-weight-for-jump-zero (inst index vec label-positions cold-labels)
  "Return static branch metadata for INST at INDEX.

Values:
  :UNLIKELY     taken target is a cold diagnostics/error block
  :LIKELY-TAKEN taken target is a loop back-edge
  :NOT-TAKEN    taken target is a return/exit edge"
  (let* ((target-label (vm-label-name inst))
         (target-index (gethash target-label label-positions)))
    (cond
      ((gethash target-label cold-labels)
       :unlikely)
      ((and target-index (< target-index index))
       :likely-taken)
      ((%opt-target-block-has-terminal-p
        vec target-index
        (lambda (target-inst)
          (or (typep target-inst 'vm-ret)
              (typep target-inst 'vm-halt))))
       :not-taken)
      (t nil))))

(defun %opt-annotate-jump-zero (inst weight)
  "Return INST with BRANCH-WEIGHT metadata when WEIGHT is non-NIL."
  (if weight
      (make-vm-branch-weighted-jump-zero :reg (vm-reg inst)
                                         :label (vm-label-name inst)
                                         :branch-weight weight)
      inst))

(defun opt-analyze-branch-weights (instructions)
  "Annotate conditional branches with static branch-prediction metadata.

The analysis is intentionally non-transforming: instruction order and control-flow
targets are preserved.  It marks branches to blocks containing VM condition/error
signaling instructions as cold/unlikely, loop back-edge branches as likely taken,
and branches directly targeting return/exit blocks as not-taken."
  (let* ((vec (coerce instructions 'vector))
         (label-positions (%opt-label-positions instructions))
         (cold-labels (%opt-cold-labels instructions)))
    (loop for inst in instructions
          for i from 0
          collect (if (typep inst 'vm-jump-zero)
                      (%opt-annotate-jump-zero
                       inst
                       (%opt-branch-weight-for-jump-zero
                        inst i vec label-positions cold-labels))
                       inst))))
