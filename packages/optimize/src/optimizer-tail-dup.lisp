;;;; optimizer-tail-dup.lisp --- FR-608 Tail Duplication

(in-package :cl-cc/optimize)

(defvar *opt-tail-dup-max-instructions* 5
  "Maximum number of instructions in a successor block eligible for FR-608.")

(defun %opt-tail-dup-control-inst-p (inst)
  "Return T when INST is a block terminator safe to copy with its block."
  (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))

(defun %opt-tail-dup-safe-inst-p (inst last-p)
  "Return T when duplicating INST preserves execution semantics."
  (if last-p
      (%opt-tail-dup-control-inst-p inst)
      (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq)))

(defun %tail-dup-candidate-p (succ)
  "Return T when SUCC is small and side-effect safe for tail duplication."
  (let* ((insts (bb-instructions succ))
         (n (length insts)))
    (and (plusp n)
         (<= n *opt-tail-dup-max-instructions*)
         (notany #'vm-label-p insts)
         (loop for inst in insts
               for index from 0
               always (%opt-tail-dup-safe-inst-p inst (= index (1- n)))))))

(defun %opt-tail-dup-fold-if-available (instructions)
  "Run constant folding after duplication when the fold pass is available."
  (if (fboundp 'opt-pass-fold)
      (opt-pass-fold instructions)
      instructions))

(unless (fboundp 'opt-pass-tail-duplication)
  (defun opt-pass-tail-duplication (instructions)
    "FR-608: duplicate small shared successor blocks into predecessor contexts.

Blocks with multiple predecessors are duplicated only when they contain at most
`*OPT-TAIL-DUP-MAX-INSTRUCTIONS*` instructions and have no side effects except
their terminator.  This prevents semantic changes and unbounded code growth.
After duplication, constant folding is run once so predecessor-local copies can
benefit from constants exposed by the transformation."
    (let* ((cfg (cfg-build instructions))
           (candidates (%tail-dup-linear-candidates cfg)))
      (multiple-value-bind (out changed)
          (%tail-dup-linear instructions candidates)
        (if changed
            (%opt-tail-dup-fold-if-available out)
            instructions)))))
