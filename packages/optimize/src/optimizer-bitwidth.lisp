;;;; optimizer-bitwidth.lisp — FR-614 Bitwidth Reduction
(in-package :cl-cc/optimize)

(defvar *opt-bitwidth-reduction-metadata* (make-hash-table :test #'eq)
  "EQ side table mapping instructions to proven narrow integer width metadata.")

(defun opt-bitwidth-reduction-metadata (inst)
  "Return bitwidth-reduction metadata for INST, or NIL."
  (gethash inst *opt-bitwidth-reduction-metadata*))

(defun opt-bitwidth-reduced-p (inst)
  "Return T when INST is proven safe to narrow."
  (getf (opt-bitwidth-reduction-metadata inst) :bitwidth-reduced))

(defun %opt-narrow-type-for-interval (interval)
  "Return the narrowest unsigned machine type proven for INTERVAL."
  (when (and interval (<= 0 (opt-interval-lo interval)))
    (cond
      ((<= (opt-interval-hi interval) #xff) :i8)
      ((<= (opt-interval-hi interval) (1- (ash 1 31))) :i32)
      (t :i64))))

(defun %opt-mark-bitwidth-reduced (inst interval narrow-type)
  (setf (gethash inst *opt-bitwidth-reduction-metadata*)
        (list :bitwidth-reduced t
              :narrow-type narrow-type
              :range (opt-make-interval (opt-interval-lo interval)
                                        (opt-interval-hi interval))
              :bit-width (opt-interval-bit-width interval)))
  inst)

(defun opt-pass-bitwidth-reduction (instructions)
  "FR-614: record safe i64→i32→i8 narrowing decisions after VRP.

The VM instruction set has no separate i8/i32 arithmetic opcodes, so this pass
attaches conservative metadata consumed by backends.  No type is narrowed unless
the proven interval is non-negative and fits the narrower range at runtime."
  (clrhash *opt-bitwidth-reduction-metadata*)
  (let ((intervals (opt-compute-value-ranges instructions)))
    (dolist (inst instructions instructions)
      (let* ((dst (opt-inst-dst inst))
             (interval (and dst (gethash dst intervals)))
             (narrow-type (%opt-narrow-type-for-interval interval)))
        (when (and narrow-type (not (eq narrow-type :i64)))
          (%opt-mark-bitwidth-reduced inst interval narrow-type))))))
