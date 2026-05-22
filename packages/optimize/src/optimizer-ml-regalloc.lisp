;;;; optimizer-ml-regalloc.lisp — conservative ML register-allocation hints

(in-package :cl-cc/optimize)

(defun opt-pass-ml-regalloc (instructions)
  "Infrastructure hook for ML-guided register allocation; leaves VM IR unchanged."
  instructions)
