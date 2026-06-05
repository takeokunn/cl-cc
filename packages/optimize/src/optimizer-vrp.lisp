;;;; optimizer-vrp.lisp — FR-610 Value Range Propagation
(in-package :cl-cc/optimize)

(defparameter *min-inf* +opt-range-negative-infinity+
  "Conservative lower-bound sentinel used by the VRP interval domain.")

(defparameter *max-inf* +opt-range-positive-infinity+
  "Conservative upper-bound sentinel used by the VRP interval domain.")

(defvar *opt-vrp-ranges* (make-hash-table :test #'equal)
  "Latest path-sensitive VRP facts keyed as (BASIC-BLOCK . REG) -> (LO . HI).")

(defun opt-vrp-top-interval ()
  "Return the unknown integer interval used by FR-610."
  (opt-make-interval *min-inf* *max-inf*))

(defun opt-vrp-fixnum-interval ()
  "Return the host fixnum interval used to seed typed fixnum facts."
  (opt-make-interval +opt-range-negative-infinity+ +opt-range-positive-infinity+))

(defun opt-vrp-constant-interval (value)
  "Return VALUE's singleton interval when it is an integer, else top."
  (if (integerp value)
      (opt-make-interval value value)
      (opt-vrp-top-interval)))

(defun opt-vrp-range (block reg &optional (ranges *opt-vrp-ranges*))
  "Return REG's latest path-sensitive interval at BLOCK, or NIL if unknown."
  (gethash (cons block reg) ranges))

(defun opt-pass-value-range-propagation (instructions)
  "FR-610: compute path-sensitive value ranges for virtual registers.

The interval domain is the existing optimizer interval representation `(LO . HI)`,
with `*MIN-INF*` and `*MAX-INF*` as conservative finite sentinels.  The transfer
function handles constants, moves, arithmetic (including `vm-add`), bitwise masks,
and branch-condition refinement through `vm-jump-zero` fed by comparison
instructions such as `(> x 0)` (`vm-gt`).  This pass records facts for following
passes and leaves the instruction stream unchanged."
  (setf *opt-vrp-ranges* (opt-compute-path-sensitive-ranges instructions))
  instructions)
