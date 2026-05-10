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
    (vm-sub . opt-interval-sub)
    (vm-mul . opt-interval-mul))
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
