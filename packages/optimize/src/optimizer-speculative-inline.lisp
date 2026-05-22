;;;; packages/optimize/src/optimizer-speculative-inline.lisp — FR-523
(in-package :cl-cc/optimize)

(defparameter *opt-speculative-inline-dominance-threshold* 0.9
  "Minimum IC type-frequency dominance required for speculative inlining.")

(defun opt-ic-dominant-type (counter-table &key (threshold *opt-speculative-inline-dominance-threshold*))
  "Return (values TYPE COUNT TOTAL RATIO) when one IC type dominates.

Flat distributions deliberately return NIL so the optimizer does not speculate
when multiple receiver types have similar frequency."
  (when (hash-table-p counter-table)
    (let ((total 0)
          (best-key nil)
          (best-count 0))
      (maphash (lambda (key count)
                 (when (plusp count)
                   (incf total count)
                   (when (> count best-count)
                     (setf best-key key
                           best-count count))))
               counter-table)
      (when (plusp total)
        (let ((ratio (/ (float best-count) total)))
          (when (> ratio threshold)
            (values best-key best-count total ratio)))))))

(defun opt-speculative-inline-eligible-p (inst)
  "Return dominant IC key for INST when FR-523 speculation is profitable."
  (and (typep inst 'cl-cc/vm:vm-generic-call)
       (multiple-value-bind (key count total ratio)
           (opt-ic-dominant-type (cl-cc/vm::vm-ic-type-counters inst))
         (declare (ignore count total ratio))
         key)))

(defun opt-annotate-speculative-inline (inst key pc)
  "Annotate INST with a guarded inline-cache fast path.

The VM's IC fast path already emits the runtime type guard by comparing the
current specializer key to VM-PGO-SPECIALIZER and directly calling the cached
method on success; guard failure falls back to the normal IC slow path, which is
also a safe FR-522 deopt/fallback boundary."
  (setf (cl-cc/vm::vm-pgo-specializer inst) key)
  (setf (getf (cl-cc/vm::vm-generic-call-metadata inst) :speculative-inline)
        (list :pc pc
              :dominant-type key
              :guard :typep-equivalent-specializer-key
              :failure :ic-slow-path-or-deopt))
  inst)

(defun opt-pass-speculative-inline (instructions)
  "FR-523: use IC type frequencies to install guarded speculative inlines.

Only monomorphic-dominant sites (>90%) are annotated.  Sites with flat type
distributions are left untouched."
  (loop for inst in instructions
        for pc from 0
        for key = (opt-speculative-inline-eligible-p inst)
        when key
          do (opt-annotate-speculative-inline inst key pc)
        collect inst))
