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
      (let ((dst (opt-inst-dst inst)))
        (typecase inst
          (vm-move
           (when dst
             (multiple-value-bind (root found-p)
                 (gethash (vm-move-src inst) roots)
               (if found-p
                   (setf (gethash dst roots) root)
                   (remhash dst roots)))))
          (t
           (cond
             ((and dst (opt-heap-root-inst-p inst))
              (setf (gethash dst roots) dst))
             (dst
              (remhash dst roots)))))))))

(defun opt-compute-heap-aliases (instructions)
  "Compute a conservative EQ hash-table reg -> canonical heap root.
This is a small FR-115 style oracle intended for downstream passes."
  (%opt-build-root-map instructions))


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

(defun opt-compute-value-ranges (instructions)
  "Compute conservative integer value ranges for a straight-line instruction list.

This extends the older constant-only interval analysis with vm-move propagation and
small unary arithmetic. It intentionally remains path-insensitive: unknown writes
kill the destination fact instead of guessing."
  (let ((intervals (make-hash-table :test #'eq)))
    (dolist (inst instructions intervals)
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
                (when dst (remhash dst intervals)))))))))))

(defun opt-compute-constant-intervals (instructions)
  "Compute a conservative interval map from straight-line constant arithmetic.

Handles vm-const and interval propagation through vm-add/vm-sub/vm-mul when
both operands already have known intervals."
  (opt-compute-value-ranges instructions))

(defun opt-array-bounds-check-eliminable-p (index-reg length-reg intervals)
  "Return T when INTERVALS prove INDEX-REG is within LENGTH-REG bounds."
  (opt-interval-valid-index-p (gethash index-reg intervals)
                              (gethash length-reg intervals)))

(defstruct (opt-induction-var (:conc-name opt-iv-))
  "Minimal scalar-evolution summary for a single affine induction variable."
  reg
  init
  step
  update-inst)

(defun %opt-constant-reg-value (reg constants)
  "Return (values VALUE T) when REG has a known integer constant in CONSTANTS."
  (multiple-value-bind (value found-p) (gethash reg constants)
    (if (and found-p (integerp value))
        (values value t)
        (values nil nil))))

(defun %opt-simple-induction-step (inst constants)
  "Return (values REG STEP T) when INST updates REG by a constant step."
  (let ((dst (opt-inst-dst inst)))
    (cond
      ((and dst (typep inst 'vm-add))
       (cond
         ((eq dst (vm-lhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))
         ((eq dst (vm-rhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-lhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))))
      ((and dst (typep inst 'vm-sub) (eq dst (vm-lhs inst)))
       (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
         (when (and ok (not (zerop c))) (values dst (- c) t)))))))

(defun %opt-copy-constant-fact (inst constants)
  "Propagate a constant fact through a vm-move, or kill the destination fact."
  (multiple-value-bind (value found-p) (gethash (vm-src inst) constants)
    (if found-p
        (setf (gethash (vm-dst inst) constants) value)
        (remhash (vm-dst inst) constants))))

(defun opt-compute-simple-inductions (instructions)
  "Return reg -> opt-induction-var summaries for simple affine updates.

Recognized patterns are intentionally conservative: a register must first hold a
known integer constant, then be updated by `(add dst dst const)`,
`(add dst const dst)`, or `(sub dst dst const)`. This supplies the small SCEV
facts needed by loop unrolling, peeling, and bounds-check reasoning without
changing program instructions."
  (let ((constants (make-hash-table :test #'eq))
        (inductions (make-hash-table :test #'eq)))
    (dolist (inst instructions inductions)
      (cond
        ((typep inst 'vm-const)
         (remhash (vm-dst inst) inductions)
         (if (integerp (vm-value inst))
             (setf (gethash (vm-dst inst) constants) (vm-value inst))
             (remhash (vm-dst inst) constants)))
        ((typep inst 'vm-move)
         (remhash (vm-dst inst) inductions)
         (%opt-copy-constant-fact inst constants))
        (t
         (multiple-value-bind (reg step ok)
             (%opt-simple-induction-step inst constants)
           (if ok
               (multiple-value-bind (init found-p) (gethash reg constants)
                 (if found-p
                     (setf (gethash reg inductions)
                           (make-opt-induction-var :reg reg
                                                   :init init
                                                   :step step
                                                   :update-inst inst))
                     (remhash reg inductions))
                 (remhash reg constants))
               (let ((dst (opt-inst-dst inst)))
                 (when dst
                   (remhash dst constants)
                   (remhash dst inductions))))))))))

(defun opt-induction-trip-count (init limit step &key inclusive-p)
  "Return a conservative integer trip count for an affine induction variable.

The loop condition is interpreted as `< limit` for positive STEP and `> limit`
for negative STEP. With INCLUSIVE-P, the corresponding boundary is `<=` or `>=`.
Returns NIL when STEP is zero."
  (cond
    ((zerop step) nil)
    ((plusp step)
     (cond
       ((if inclusive-p (> init limit) (>= init limit)) 0)
       (inclusive-p (1+ (floor (- limit init) step)))
       (t (ceiling (- limit init) step))))
    (t
     (let ((magnitude (- step)))
       (cond
         ((if inclusive-p (< init limit) (<= init limit)) 0)
         (inclusive-p (1+ (floor (- init limit) magnitude)))
         (t (ceiling (- init limit) magnitude)))))))

(defun opt-compute-heap-kinds (instructions)
  "Compute a conservative root -> heap-kind table for TBAA-style checks."
  (let ((points-to (opt-compute-heap-aliases instructions))
        (kinds (make-hash-table :test #'eq)))
    (dolist (inst instructions kinds)
      (let ((dst (opt-inst-dst inst)))
        (when (and dst (opt-heap-root-inst-p inst))
          (let ((root (gethash dst points-to)))
            (when root
              (setf (gethash root kinds) (opt-heap-root-kind inst)))))))
    kinds))

(defun opt-may-alias-by-type-p (reg-a reg-b points-to heap-kinds)
  "Return T if REG-A and REG-B may alias under type-based heap classification.

If both roots and kinds are known and the kinds differ, return NIL. Otherwise
stay conservative and return T."
  (let ((root-a (gethash reg-a points-to))
        (root-b (gethash reg-b points-to)))
    (cond
      ((or (null root-a) (null root-b)) t)
      ((eq root-a root-b) t)
      (t (let ((kind-a (gethash root-a heap-kinds))
               (kind-b (gethash root-b heap-kinds)))
           (if (and kind-a kind-b)
               (eq kind-a kind-b)
               t))))))

(defun opt-must-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B definitely alias under ALIAS-ROOTS."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (and found-a found-b (eq root-a root-b)))))

(defun opt-may-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B may alias under ALIAS-ROOTS.

Unknown roots remain conservative and therefore return T."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (or (not found-a)
          (not found-b)
          (eq root-a root-b)))))

(defun opt-slot-alias-key (obj-reg slot-name alias-roots)
  "Return a canonical slot key for OBJ-REG/SLOT-NAME using ALIAS-ROOTS."
  (multiple-value-bind (root found-p) (gethash obj-reg alias-roots)
    (list :slot (if found-p root obj-reg) slot-name)))

(defun opt-rewrite-inst-regs (inst copies)
  "Return INST with all source registers replaced by their canonical copies.
   Uses sexp roundtrip: instruction->sexp rewrites all register-keyword leaves
   except the destination slot (position 1 for instructions with a dst), then
   reconstructs via sexp->instruction.  Falls back to INST unchanged on error."
  (let ((c (lambda (x) (if (opt-register-keyword-p x) (or (gethash x copies) x) x))))
    (handler-case
        (let* ((sexp     (instruction->sexp inst))
               (has-dst  (not (null (opt-inst-dst inst))))
               ;; Rewrite all leaves; for instructions with a dst, the dst sits at
               ;; position 1 (immediately after the opcode tag) — leave it intact.
               (new-sexp (if has-dst
                             (list* (first sexp) (second sexp) (opt-map-tree c (cddr sexp)))
                             (cons  (first sexp) (opt-map-tree c (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

;; opt-pass-dead-store-elim and opt-pass-store-to-load-forward are in
;; optimizer-memory-passes.lisp (loaded next).
