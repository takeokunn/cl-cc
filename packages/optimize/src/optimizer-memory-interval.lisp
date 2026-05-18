(in-package :cl-cc/optimize)

(defun %opt-memory-access-index-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-index-reg inst))
    (t nil)))

(defun %opt-memory-access-array-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-array-reg inst))
    (t nil)))

(defun %opt-memory-access-kind (inst)
  (typecase inst
    (vm-aref :load)
    (vm-aset :store)
    (t nil)))

(defun %opt-copy-constant-env (env)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %opt-update-memory-pattern-constants (inst env)
  (typecase inst
    (vm-const
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) env) (vm-value inst))
         (remhash (vm-dst inst) env)))
    (vm-move
     (multiple-value-bind (value found-p) (gethash (vm-src inst) env)
       (if found-p
           (setf (gethash (vm-dst inst) env) value)
           (remhash (vm-dst inst) env))))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst (remhash dst env))))))

(defun %opt-memory-pattern-class (stride)
  (cond
    ((null stride) :random)
    ((= (abs stride) 1) :sequential)
    (t :strided)))

(defun %opt-memory-pattern-record (metadata inst block array-reg index-reg stride &key memory-entry)
  (let ((entry (list :kind (%opt-memory-access-kind inst)
                     :array-reg array-reg
                     :index-reg index-reg
                     :stride stride
                     :pattern (%opt-memory-pattern-class stride)
                     :block block
                     :memory-ssa memory-entry
                     :prefetch-candidate (and stride (<= (abs stride) 4))
                     :tiling-candidate (and stride (/= stride 0)))))
    (setf (gethash inst metadata) entry)
    (push inst (gethash :accesses metadata))
    entry))

;;; FR-309: Memory Access Pattern Analysis — classifies memory accesses as sequential/strided/random; provides data for prefetch insertion and loop tiling
(defun opt-analyze-memory-access-patterns (cfg-or-instructions memory-ssa)
  "Analyze array memory accesses and classify their access patterns.

Returns an EQ hash-table keyed by memory access instruction.  Each value is a
plist containing `:stride', `:pattern' (`:sequential', `:strided', or `:random'),
and metadata flags consumed by future prefetch (FR-289) and loop-tiling (FR-287)
passes.  Constant-stride evidence is derived from consecutive array accesses,
constant index registers, and simple loop induction summaries."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (instructions (loop for block across (cfg-blocks cfg)
                             append (bb-instructions block)))
         (inductions (opt-compute-loop-inductions cfg))
         (metadata (make-hash-table :test #'eq)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (loop for block across (cfg-blocks cfg)
          when block
          do (let ((constants (make-hash-table :test #'eq))
                   (last-by-array (make-hash-table :test #'eq)))
               (dolist (inst (bb-instructions block))
                 (let ((array-reg (%opt-memory-access-array-reg inst))
                       (index-reg (%opt-memory-access-index-reg inst)))
                   (if (and array-reg index-reg)
                       (let* ((last (gethash array-reg last-by-array))
                              (index-value (gethash index-reg constants))
                              (iv-step (loop for ivs being the hash-values of inductions
                                             for iv = (gethash index-reg ivs)
                                             when iv return (opt-iv-step iv)))
                              (stride (cond
                                        (iv-step iv-step)
                                        ((and index-value last (getf last :index-value))
                                         (- index-value (getf last :index-value)))
                                        (t nil))))
                         (%opt-memory-pattern-record metadata inst block array-reg index-reg stride
                                                     :memory-entry (and memory-ssa (gethash inst memory-ssa)))
                         (setf (gethash array-reg last-by-array)
                               (list :inst inst :index-reg index-reg :index-value index-value)))
                       (%opt-update-memory-pattern-constants inst constants))))))
    (setf (gethash :cfg metadata) cfg
          (gethash :instructions metadata) instructions)
    metadata))

;;;; Safepoint optimization foundation (FR-090, FR-091)

(defun %opt-mir-accessor (name)
  "Return MIR accessor NAME when the MIR package is loaded."
  (let* ((pkg (find-package :cl-cc/mir))
         (sym (and pkg (find-symbol name pkg))))
    (and sym (fboundp sym) (symbol-function sym))))

(defun opt-safepoint-inst-p (inst)
  "Return T when INST represents a MIR/sexp safepoint."
  (or (and (consp inst) (eq (first inst) :safepoint))
      (let ((op-accessor (%opt-mir-accessor "MIRI-OP")))
        (and op-accessor (eq (funcall op-accessor inst) :safepoint)))))

(defun %opt-safepoint-default-roots (inst)
  (cond
    ((and (consp inst) (eq (first inst) :safepoint))
     (rest inst))
    (t
     (let ((srcs-accessor (%opt-mir-accessor "MIRI-SRCS")))
       (and srcs-accessor (funcall srcs-accessor inst))))))

(defun %opt-safepoint-roots (inst root-set-analysis)
  "Return a canonical root set for INST."
  (let ((roots (cond
                 ((hash-table-p root-set-analysis)
                  (multiple-value-bind (value found-p) (gethash inst root-set-analysis)
                    (if found-p value (%opt-safepoint-default-roots inst))))
                 ((functionp root-set-analysis)
                  (funcall root-set-analysis inst))
                 (t
                  (%opt-safepoint-default-roots inst)))))
    (sort (copy-list roots) #'string< :key #'prin1-to-string)))

(defun %opt-filter-safepoints (instructions active root-set-analysis removed)
  "Filter dominated safepoints in a single instruction list."
  (let ((result nil)
        (local-active (copy-list active)))
    (dolist (inst instructions (values (nreverse result) local-active))
      (if (opt-safepoint-inst-p inst)
          (let ((key (%opt-safepoint-roots inst root-set-analysis)))
            (if (member key local-active :test #'equal)
                (setf (gethash inst removed) t)
                (progn
                  (push key local-active)
                  (push inst result))))
          (push inst result)))))

;;; FR-090: Safepoint Dominance Pruning — removes safepoints whose root sets are dominated by other safepoints
(defun opt-prune-dominated-safepoints (cfg root-set-analysis)
  "Remove safepoint B when dominated by safepoint A with the same root set."
  (cfg-compute-dominators cfg)
  (let ((removed (make-hash-table :test #'eq)))
    (labels ((walk (block active)
               (multiple-value-bind (new-insts new-active)
                   (%opt-filter-safepoints (bb-instructions block) active root-set-analysis removed)
                 (setf (bb-instructions block) new-insts)
                 (dolist (child (bb-dom-children block))
                   (walk child new-active)))))
      (when (cfg-entry cfg)
        (walk (cfg-entry cfg) nil)))
    cfg))

(defun %opt-safepoint-motion-barrier-p (inst)
  "Return T when a safepoint must not be moved across INST."
  (or (typep inst '(or vm-set-global vm-slot-write vm-aset))
      (member (vm-inst-effect-kind inst)
              '(:write-global :io :control :unknown)
              :test #'eq)))

(defun %opt-block-terminator-position (instructions)
  (position-if (lambda (inst) (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))
               instructions))

(defun %opt-remove-safepoints-hoistable-to-tail (block)
  "Remove tail-block safepoints that can be reinserted at the back-edge poll site."
  (let* ((insts (bb-instructions block))
         (term-pos (or (%opt-block-terminator-position insts) (length insts)))
         (prefix (subseq insts 0 term-pos))
         (suffix (subseq insts term-pos))
         (kept nil)
         (hoisted nil))
    (loop for rest on prefix
          for inst = (car rest)
          do (cond
               ((and (opt-safepoint-inst-p inst)
                     (notany #'%opt-safepoint-motion-barrier-p (cdr rest)))
                (push inst hoisted))
               (t
                (push inst kept))))
    (setf (bb-instructions block) (append (nreverse kept) suffix))
    (nreverse hoisted)))

(defun %opt-insert-before-terminator (block insts)
  (when insts
    (let* ((old (bb-instructions block))
           (pos (or (%opt-block-terminator-position old) (length old))))
      (setf (bb-instructions block)
            (append (subseq old 0 pos) insts (subseq old pos))))))

;;; FR-091: Safepoint Hoisting to Loop Back-Edges — moves safepoints from loop bodies to back edges, reducing polling frequency
(defun opt-hoist-safepoints-to-back-edges (cfg)
  "Move safely-hoistable loop safepoints in back-edge blocks to back-edge polls."
  (cfg-compute-dominators cfg)
  (cfg-compute-loop-depths cfg)
  (loop for tail across (cfg-blocks cfg)
        when tail
        do (dolist (header (bb-successors tail))
             (when (and (> (bb-loop-depth tail) 0)
                        (cfg-dominates-p header tail))
               (let ((hoisted (%opt-remove-safepoints-hoistable-to-tail tail)))
                 (%opt-insert-before-terminator tail hoisted)))))
  cfg)

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
