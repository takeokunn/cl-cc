;;;; optimizer-overflow.lisp — FR-613 Integer Overflow Check Elimination
(in-package :cl-cc/optimize)

(defun opt-pass-overflow-check-elimination (instructions)
  "FR-613: replace checked integer arithmetic when VRP proves fixnum safety.

`vm-add-checked` and `vm-mul-checked` are rewritten to their unchecked integer
forms only when interval arithmetic proves the result stays within the host fixnum
range.  Otherwise the checked instruction is preserved."
  (opt-pass-elide-proven-overflow-checks instructions))
