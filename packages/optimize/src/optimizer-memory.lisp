(in-package :cl-cc/optimize)
;;; ─── Alias Analysis + Memory Passes ─────────────────────────────────────────

(defun opt-heap-root-inst-p (inst)
  "Return T when INST produces a fresh heap-like object identity."
  (typep inst '(or vm-cons vm-make-array vm-closure vm-make-closure)))

(defparameter *opt-heap-root-kind-table*
  '((vm-cons . :cons)
    (vm-make-array . :array)
    (vm-closure . :closure)
    (vm-make-closure . :closure))
  "Maps heap-producing instruction types to symbolic heap kind keywords.")

(defun opt-heap-root-kind (inst)
  "Return a symbolic heap kind for fresh heap-producing INST."
  (loop for (type . kind) in *opt-heap-root-kind-table*
        when (typep inst type)
        return kind))

(defun %opt-build-root-map (instructions)
  "Build a conservative EQ hash-table mapping registers to their canonical heap root.

Fresh heap producers (opt-heap-root-inst-p) start a new root at their destination.
vm-move propagates the source root. Any other destination write kills the root fact."
  (let ((roots (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (typecase inst
        (vm-move
         (let ((dst (vm-move-dst inst)))
           (when dst
             (multiple-value-bind (root found-p)
                 (gethash (vm-move-src inst) roots)
               (if found-p
                   (setf (gethash dst roots) root)
                   (remhash dst roots))))))
        (vm-get-global
         (let ((dst (cl-cc/vm::vm-get-global-dst inst)))
           (when dst (remhash dst roots))))
        (vm-slot-read
         (let ((dst (cl-cc/vm::vm-slot-read-dst inst)))
           (when dst (remhash dst roots))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (cond
             ((and dst (opt-heap-root-inst-p inst))
              (setf (gethash dst roots) dst))
             (dst
              (remhash dst roots)))))))))

(defun opt-compute-heap-aliases (instructions)
  "Compute a conservative EQ hash-table reg -> canonical heap root.
This is a small FR-115 style oracle intended for downstream passes."
  (%opt-build-root-map instructions))

(defun opt-compute-points-to (instructions)
  "Compute conservative flow-sensitive points-to roots for INSTRUCTIONS.

This FR-018 helper intentionally models a single canonical fresh heap root per
register in straight-line code. Fresh heap producers create roots, vm-move
propagates them, and later non-heap writes kill stale facts. Branch joins and
field-sensitive object graphs remain out of scope for this helper."
  (opt-compute-heap-aliases instructions))

(defun opt-memory-def-inst-p (inst)
  "Return T when INST defines memory state for the conservative Memory-SSA model."
  (typep inst '(or vm-set-global vm-slot-write vm-cons)))

(defun opt-memory-use-inst-p (inst)
  "Return T when INST reads memory state for the conservative Memory-SSA model."
  (typep inst '(or vm-get-global vm-slot-read)))

(defun %opt-memory-location-key (inst alias-roots)
  "Return a canonical memory location key for INST, or NIL when not modeled."
  (typecase inst
    (vm-set-global (list :global (cl-cc/vm::vm-set-global-name inst)))
    (vm-get-global (list :global (cl-cc/vm::vm-get-global-name inst)))
    (vm-slot-write (opt-slot-alias-key (cl-cc/vm::vm-slot-write-obj-reg inst)
                                       (cl-cc/vm::vm-slot-write-slot-name inst)
                                       alias-roots))
    (vm-slot-read (opt-slot-alias-key (cl-cc/vm::vm-slot-read-obj-reg inst)
                                      (cl-cc/vm::vm-slot-read-slot-name inst)
                                      alias-roots))
    (vm-cons (list :alloc (vm-dst inst)))
    (t nil)))

(defun opt-compute-memory-ssa-snapshot (instructions)
  "Compute a straight-line Memory-SSA snapshot table for INSTRUCTIONS.

Returns an EQ hash-table mapping each modeled instruction to a plist:
  :kind     one of :def or :use
  :location canonical location key
  :in       incoming memory version
  :out      outgoing memory version

This intentionally models only straight-line versioning (no MemoryPhi)."
  (let ((annotations (make-hash-table :test #'eq))
        (version 0)
        (alias-roots (opt-compute-heap-aliases instructions)))
    (dolist (inst instructions annotations)
      (let ((loc (%opt-memory-location-key inst alias-roots)))
        (cond
          ((and loc (opt-memory-def-inst-p inst))
           (let ((vin version))
             (incf version)
             (setf (gethash inst annotations)
                   (list :kind :def :location loc :in vin :out version))))
          ((and loc (opt-memory-use-inst-p inst))
           (setf (gethash inst annotations)
                 (list :kind :use :location loc :in version :out version))))))))

(defun opt-memory-ssa-version-at (inst annotations &key (point :in))
  "Return memory version for INST in ANNOTATIONS at POINT (:in or :out)."
  (let ((entry (gethash inst annotations)))
    (when entry
      (ecase point
        (:in (getf entry :in))
        (:out (getf entry :out))))))

(defun opt-points-to-root (reg points-to)
  "Return REG's canonical root under POINTS-TO as two values: root and found-p."
  (gethash reg points-to))


(defun opt-make-interval (lo hi)
  "Construct a closed integer interval [LO, HI]."
  (cons lo hi))

(defun opt-interval-lo (interval)
  (car interval))

(defun opt-interval-hi (interval)
  (cdr interval))

(defun opt-interval-singleton-p (interval)
  "Return T when INTERVAL is a singleton [N, N]."
  (and interval
       (= (opt-interval-lo interval)
          (opt-interval-hi interval))))

(defun opt-interval-singleton-value (interval)
  "Return INTERVAL's single value, or NIL when it is not a singleton."
  (when (opt-interval-singleton-p interval)
    (opt-interval-lo interval)))

(defun opt-interval-add (a b)
  "Add two intervals conservatively."
  (opt-make-interval (+ (opt-interval-lo a) (opt-interval-lo b))
                     (+ (opt-interval-hi a) (opt-interval-hi b))))

(defun opt-interval-sub (a b)
  "Subtract interval B from A conservatively."
  (opt-make-interval (- (opt-interval-lo a) (opt-interval-hi b))
                     (- (opt-interval-hi a) (opt-interval-lo b))))

(defun opt-interval-mul (a b)
  "Multiply two intervals conservatively."
  (let* ((p1 (* (opt-interval-lo a) (opt-interval-lo b)))
         (p2 (* (opt-interval-lo a) (opt-interval-hi b)))
         (p3 (* (opt-interval-hi a) (opt-interval-lo b)))
         (p4 (* (opt-interval-hi a) (opt-interval-hi b))))
    (opt-make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(defun opt-interval-neg (a)
  "Negate interval A conservatively."
  (opt-make-interval (- (opt-interval-hi a))
                     (- (opt-interval-lo a))))

(defun opt-interval-abs (a)
  "Return a conservative interval for ABS over A."
  (cond
    ((>= (opt-interval-lo a) 0) a)
    ((<= (opt-interval-hi a) 0) (opt-interval-neg a))
    (t (opt-make-interval 0 (max (abs (opt-interval-lo a))
                                 (abs (opt-interval-hi a)))))))

(defun opt-interval-bit-width (interval)
  "Return a conservative unsigned bit-width upper bound for INTERVAL.

Only non-negative intervals are assigned a width. Mixed-sign or fully-negative
intervals return NIL rather than pretending the result is narrower than proven."
  (when (opt-interval-nonnegative-p interval)
    (integer-length (max 0 (opt-interval-hi interval)))))

(defun opt-interval-known-bits-mask (interval)
  "Return a conservative bit mask covering every bit that may be set.

For a non-negative interval with width W, the result is 2^W-1. Bits outside
that mask are therefore known zero for every value in INTERVAL. Returns NIL
when INTERVAL has no proven non-negative width bound."
  (let ((width (opt-interval-bit-width interval)))
    (when width
      (1- (ash 1 width)))))

(defun opt-interval-fits-fixnum-width-p (interval &optional (limit-width (integer-length most-positive-fixnum)))
  "Return T when INTERVAL's unsigned width is strictly below LIMIT-WIDTH."
  (let ((width (opt-interval-bit-width interval)))
    (and width (< width limit-width))))

(defun opt-interval-fits-fixnum-p (interval)
  "Return T when INTERVAL is proven to stay within the host fixnum range."
  (and interval
       (<= most-negative-fixnum (opt-interval-lo interval))
       (<= (opt-interval-hi interval) most-positive-fixnum)))

(defun %opt-nonnegative-mask-value (interval)
  "Return INTERVAL's non-negative singleton value, or NIL."
  (let ((value (opt-interval-singleton-value interval)))
    (when (and (integerp value) (not (minusp value)))
      value)))

(defun opt-interval-logand (a b)
  "Return a conservative interval for LOGAND over A and B.

If either operand is a known non-negative singleton mask, the result is bounded
to [0, mask] even when the other operand is unknown. When both operands are
proven non-negative, the result is also bounded above by the smaller upper
bound. Returns NIL when no safe bound is known."
  (let ((mask-a (%opt-nonnegative-mask-value a))
        (mask-b (%opt-nonnegative-mask-value b)))
    (cond
      (mask-a
       (opt-make-interval 0 (if (opt-interval-nonnegative-p b)
                                (min mask-a (opt-interval-hi b))
                                mask-a)))
      (mask-b
       (opt-make-interval 0 (if (opt-interval-nonnegative-p a)
                                (min mask-b (opt-interval-hi a))
                                mask-b)))
      ((and (opt-interval-nonnegative-p a)
            (opt-interval-nonnegative-p b))
       (opt-make-interval 0 (min (opt-interval-hi a)
                                 (opt-interval-hi b))))
       (t nil))))

(defun opt-interval-logior (a b)
  "Return a conservative interval for LOGIOR over A and B.

When both operands are proven non-negative, the result is non-negative and
bounded above by OR-ing their known-bits masks."
  (when (and (opt-interval-nonnegative-p a)
             (opt-interval-nonnegative-p b))
    (let ((mask-a (opt-interval-known-bits-mask a))
          (mask-b (opt-interval-known-bits-mask b)))
      (when (and mask-a mask-b)
        (opt-make-interval 0 (logior mask-a mask-b))))))

(defun opt-interval-logxor (a b)
  "Return a conservative interval for LOGXOR over A and B.

For proven non-negative operands, every set bit in the result must come from a
set bit in either operand, so the same known-bits upper mask as LOGIOR applies."
  (when (and (opt-interval-nonnegative-p a)
             (opt-interval-nonnegative-p b))
    (let ((mask-a (opt-interval-known-bits-mask a))
          (mask-b (opt-interval-known-bits-mask b)))
      (when (and mask-a mask-b)
        (opt-make-interval 0 (logior mask-a mask-b))))))

(defun opt-interval-ash (value-interval shift-interval)
  "Return a conservative interval for ASH over VALUE-INTERVAL by SHIFT-INTERVAL.

Only singleton integer shifts are handled. Unknown shifts return NIL.
For a fixed integer shift K, ASH is monotone over integers, so bounds can be
shifted directly."
  (let ((k (opt-interval-singleton-value shift-interval)))
    (when (integerp k)
      (opt-make-interval (ash (opt-interval-lo value-interval) k)
                         (ash (opt-interval-hi value-interval) k)))))

(defun opt-interval-contains-p (interval value)
  "Return T when VALUE is proven to be inside INTERVAL."
  (and interval
       (<= (opt-interval-lo interval) value)
       (<= value (opt-interval-hi interval))))

(defun opt-interval-nonnegative-p (interval)
  "Return T when INTERVAL is proven to contain only non-negative integers."
  (and interval (<= 0 (opt-interval-lo interval))))

(defun opt-interval-subset-p (inner outer)
  "Return T when INNER is proven to be a subset of OUTER."
  (and inner outer
       (<= (opt-interval-lo outer) (opt-interval-lo inner))
       (<= (opt-interval-hi inner) (opt-interval-hi outer))))

(defun opt-interval-valid-index-p (index-interval length-interval)
  "Return T when INDEX-INTERVAL is proven valid for any length in LENGTH-INTERVAL.

This is the conservative BCE predicate used by FR-039 style array bounds checks:
the index lower bound must be non-negative, and the index upper bound must be
strictly below the minimum possible array length."
  (and (opt-interval-nonnegative-p index-interval)
       length-interval
       (< (opt-interval-hi index-interval)
          (opt-interval-lo length-interval))))

(defparameter *opt-interval-binop-table*
  '((vm-add . opt-interval-add)
    (vm-integer-add . opt-interval-add)
    (vm-sub . opt-interval-sub)
    (vm-integer-sub . opt-interval-sub)
    (vm-mul . opt-interval-mul)
    (vm-integer-mul . opt-interval-mul))
  "Maps binary arithmetic instruction types to their interval combinator functions.")

(defparameter *opt-interval-unary-table*
  `((vm-neg . ,#'opt-interval-neg)
    (vm-abs . ,#'opt-interval-abs)
    (vm-inc . ,(lambda (a) (opt-interval-add a (opt-make-interval 1 1))))
    (vm-dec . ,(lambda (a) (opt-interval-sub a (opt-make-interval 1 1)))))
  "Maps unary arithmetic instruction types to interval transformers.")

(defun %opt-copy-interval-state (intervals)
  "Return a deep EQ hash-table copy of INTERVALS."
  (let ((copy (make-hash-table :test #'eq)))
    (when intervals
      (maphash (lambda (reg interval)
                 (setf (gethash reg copy)
                       (opt-make-interval (opt-interval-lo interval)
                                          (opt-interval-hi interval))))
               intervals))
    copy))

(defun %opt-interval-equal-p (a b)
  "Return T when intervals A and B have identical closed bounds."
  (and (= (opt-interval-lo a) (opt-interval-lo b))
       (= (opt-interval-hi a) (opt-interval-hi b))))

(defun %opt-interval-state-equal-p (a b)
  "Return T when A and B contain the same reg -> interval facts."
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for reg being the hash-keys of a
             for interval = (gethash reg a)
             always (multiple-value-bind (other found-p) (gethash reg b)
                      (and found-p
                           (%opt-interval-equal-p interval other))))))

(defun %opt-merge-interval-states (states)
  "Meet interval STATES at a CFG join.

Only registers known on every incoming path are preserved. Their intervals are
unioned conservatively as [min lo, max hi]."
  (if (null states)
      (make-hash-table :test #'eq)
      (let* ((merged (%opt-copy-interval-state (first states)))
             (rest-states (rest states))
             (keys (loop for reg being the hash-keys of merged collect reg)))
        (dolist (reg keys merged)
          (let* ((base (gethash reg merged))
                 (lo (opt-interval-lo base))
                 (hi (opt-interval-hi base))
                 (keep-p t))
            (dolist (state rest-states)
              (multiple-value-bind (other found-p) (gethash reg state)
                (unless found-p
                  (setf keep-p nil)
                  (return))
                (setf lo (min lo (opt-interval-lo other))
                      hi (max hi (opt-interval-hi other)))))
            (if keep-p
                (setf (gethash reg merged) (opt-make-interval lo hi))
                (remhash reg merged)))))))

(defun %opt-update-interval-binop (inst intervals fn)
  "Update INTERVALS for binary arithmetic INST using interval combinator FN.
If either operand has no known interval, conservatively kills the destination."
  (let ((lhs (gethash (vm-lhs inst) intervals))
        (rhs (gethash (vm-rhs inst) intervals)))
    (if (and lhs rhs)
        (setf (gethash (vm-dst inst) intervals) (funcall fn lhs rhs))
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-unary (inst intervals fn)
  "Update INTERVALS for unary arithmetic INST using interval transformer FN."
  (let ((src (gethash (vm-src inst) intervals)))
    (if src
        (setf (gethash (vm-dst inst) intervals) (funcall fn src))
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logand (inst intervals)
  "Update INTERVALS for LOGAND, preserving non-negative mask bounds when known."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logand lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logior (inst intervals)
  "Update INTERVALS for LOGIOR with non-negative known-bits bounds when provable."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logior lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-logxor (inst intervals)
  "Update INTERVALS for LOGXOR with non-negative known-bits bounds when provable."
  (let* ((lhs (gethash (vm-lhs inst) intervals))
         (rhs (gethash (vm-rhs inst) intervals))
         (interval (opt-interval-logxor lhs rhs)))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-update-interval-ash (inst intervals)
  "Update INTERVALS for ASH when both source and shift ranges are known.

The shift range must be a singleton integer interval."
  (let* ((value (gethash (vm-lhs inst) intervals))
         (shift (gethash (vm-rhs inst) intervals))
         (interval (and value shift (opt-interval-ash value shift))))
    (if interval
        (setf (gethash (vm-dst inst) intervals) interval)
        (remhash (vm-dst inst) intervals))))

(defun %opt-interval-binop-entry (inst)
  "Return the interval binary transformer symbol for INST, or NIL."
  (loop for (type . fn-sym) in *opt-interval-binop-table*
        when (typep inst type)
        return fn-sym))

(defun %opt-interval-unary-entry (inst)
  "Return the interval unary transformer designator for INST, or NIL."
  (loop for (type . fn) in *opt-interval-unary-table*
        when (typep inst type)
        return fn))

(defun %opt-interval-function (designator)
  "Resolve an interval transformer DESIGNATOR to a function."
  (etypecase designator
    (symbol (symbol-function designator))
    (function designator)))

(defun %opt-self-referential-range-update-p (inst)
  "Return T when INST reads and overwrites the same destination register."
  (let ((dst (opt-inst-dst inst)))
    (and dst
         (not (typep inst 'vm-move))
         (member dst (opt-inst-read-regs inst) :test #'eq))))

(defun %opt-transfer-interval-inst (inst intervals &key kill-self-updates)
  "Apply INST to INTERVALS conservatively and return INTERVALS.

With KILL-SELF-UPDATES, instructions that read their own destination kill that
fact instead of expanding ranges indefinitely across loop backedges."
  (cond
    ((typep inst 'vm-const)
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) intervals)
               (opt-make-interval (vm-value inst) (vm-value inst)))
         (remhash (vm-dst inst) intervals)))
    ((typep inst 'vm-move)
     (let ((src (gethash (vm-src inst) intervals)))
       (if src
           (setf (gethash (vm-dst inst) intervals) src)
           (remhash (vm-dst inst) intervals))))
    ((and kill-self-updates (%opt-self-referential-range-update-p inst))
     (let ((dst (opt-inst-dst inst)))
       (when dst
         (remhash dst intervals))))
    (t
     (let ((binop-entry (%opt-interval-binop-entry inst))
           (unary-entry (%opt-interval-unary-entry inst)))
         (cond
           ((typep inst 'vm-logand)
            (%opt-update-interval-logand inst intervals))
           ((typep inst 'vm-logior)
            (%opt-update-interval-logior inst intervals))
           ((typep inst 'vm-logxor)
            (%opt-update-interval-logxor inst intervals))
           ((typep inst 'vm-ash)
            (%opt-update-interval-ash inst intervals))
           (binop-entry
            (%opt-update-interval-binop inst intervals
                                        (%opt-interval-function binop-entry)))
         (unary-entry
          (%opt-update-interval-unary inst intervals
                                      (%opt-interval-function unary-entry)))
         (t
          (let ((dst (opt-inst-dst inst)))
            (when dst
              (remhash dst intervals))))))))
  intervals)

(defparameter *opt-checked-arithmetic-elision-table*
  '((vm-add-checked . (opt-interval-add . make-vm-integer-add))
    (vm-sub-checked . (opt-interval-sub . make-vm-integer-sub))
    (vm-mul-checked . (opt-interval-mul . make-vm-integer-mul)))
  "Maps checked arithmetic instruction types to interval proof and unchecked constructors.")

(defun %opt-checked-arithmetic-elision-entry (inst)
  "Return the checked-arithmetic elision entry for INST, or NIL."
  (loop for (type . entry) in *opt-checked-arithmetic-elision-table*
        when (typep inst type)
        return entry))

(defun %opt-rewrite-checked-arithmetic-if-safe (inst intervals)
  "Rewrite checked arithmetic INST to unchecked integer arithmetic if ranges prove safety."
  (let ((entry (%opt-checked-arithmetic-elision-entry inst)))
    (when entry
      (destructuring-bind (interval-fn . constructor) entry
        (let ((lhs (gethash (vm-lhs inst) intervals))
              (rhs (gethash (vm-rhs inst) intervals)))
          (when (and lhs rhs)
            (let ((result (funcall (symbol-function interval-fn) lhs rhs)))
              (when (opt-interval-fits-fixnum-p result)
                (funcall (symbol-function constructor)
                         :dst (vm-dst inst)
                         :lhs (vm-lhs inst)
                         :rhs (vm-rhs inst))))))))))

(defun opt-pass-elide-proven-overflow-checks (instructions)
  "Elide FR-303 checked arithmetic when interval analysis proves fixnum safety."
  (let ((intervals (make-hash-table :test #'eq))
        (result nil))
    (dolist (inst instructions (nreverse result))
      (let ((replacement (%opt-rewrite-checked-arithmetic-if-safe inst intervals)))
        (push (or replacement inst) result))
      (%opt-transfer-interval-inst inst intervals))))
