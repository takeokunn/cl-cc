(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Implementations
;;;
;;; All optimization passes over the VM instruction sequence.
;;; Data tables and derived predicates live in optimizer-tables.lisp.
;;;
;;; Pipeline (two iterations for convergence):
;;;   opt-pass-fold        constant folding + algebraic simplification
;;;                        + unary folding + type predicate folding
;;;                        + constant branch elimination
;;;   opt-pass-dce         global-usedness dead code elimination (pure insts only)
;;;   opt-pass-jump        jump threading + dead jump elimination
;;;   opt-pass-unreachable remove instructions after unconditional transfers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Pass 1: Constant Folding + Algebraic Simplification ─────────────────

(defun opt-fold-binop-value (inst lval rval)
  "Fold binary INST with numeric constants LVAL and RVAL.
   Returns (values folded-value t) on success, or (values nil nil) if not foldable.
   Dispatch is data-driven via *opt-binary-fold-table* and *opt-binary-cmp-fold-table*."
  (let* ((tp (type-of inst))
         (arith-fn (gethash tp *opt-binary-fold-table*))
         (cmp-fn   (gethash tp *opt-binary-cmp-fold-table*)))
    (cond
      ;; Instructions that must not be folded (values-list side-channel)
      ((member tp *opt-binary-no-fold-types* :test #'eq)
       (values nil nil))
      ;; Zero-guarded arithmetic (div/mod/rem)
      ((and arith-fn (member tp *opt-binary-zero-guard-types* :test #'eq))
       (if (zerop rval)
           (values nil nil)
           (values (funcall arith-fn lval rval) t)))
      ;; Normal arithmetic fold
      (arith-fn
       (values (funcall arith-fn lval rval) t))
      ;; Comparison fold → 1/0
      (cmp-fn
       (values (if (funcall cmp-fn lval rval) 1 0) t))
      (t (values nil nil)))))

(defun %opt-known-constant-p (v)
  "Return T if V is a known concrete constant (not the :unknown sentinel)."
  (and (not (eq v :unknown)) (numberp v)))

(defun %opt-copy-derived-fact (src dst table)
  "Propagate SRC's derived fact to DST within TABLE, or clear DST."
  (multiple-value-bind (fact found-p) (gethash src table)
    (if found-p
        (setf (gethash dst table) fact)
        (remhash dst table))))

(defun %opt-logand-low-bit-source (lhs rhs lval lfound rval rfound)
  "Return the original source when LOGAND is known to mask with literal 1."
  (cond
    ((and rfound (eql rval 1)) lhs)
    ((and lfound (eql lval 1)) rhs)
    (t nil)))

(defun %opt-rewrite-logand-low-bit-test (inst lhs rhs lval lfound rval rfound low-bit-facts)
  "Rewrite (= (logand x 1) 0/1) style tests into vm-evenp/vm-oddp when known."
  (labels ((rewrite-from (reg constant)
             (multiple-value-bind (source found-p) (gethash reg low-bit-facts)
               (when found-p
                 (case constant
                   (0 (make-vm-evenp :dst (vm-dst inst) :src source))
                   (1 (make-vm-oddp  :dst (vm-dst inst) :src source)))))))
    (or (and lfound (integerp lval) (rewrite-from rhs lval))
        (and rfound (integerp rval) (rewrite-from lhs rval)))))

(defun %opt-apply-algebraic-action (action dst lhs-reg rhs-reg)
  "Produce an instruction from an algebraic simplification ACTION.
   Returns NIL if ACTION is unrecognized."
  (case action
    (:move-lhs (make-vm-move :dst dst :src lhs-reg))
    (:move-rhs (make-vm-move :dst dst :src rhs-reg))
    (otherwise
     (when (consp action)
       (case (car action)
         (:const (make-vm-const :dst dst :value (cadr action)))
         (:neg   (make-vm-neg  :dst dst
                               :src (if (eq (cadr action) :lhs) lhs-reg rhs-reg))))))))

(defun opt-simplify-binop (inst dst lhs-reg rhs-reg lval rval)
  "Algebraic simplification of binary INST via rule table lookup.
   LVAL/RVAL are known constant values or :unknown.
   Returns a simplified instruction, or NIL if no simplification applies."
  (dolist (rule (gethash (type-of inst) *opt-algebraic-identity-rules*) nil)
    (let ((cond   (car rule))
          (action (cdr rule)))
      (when (cond
              ((eq cond :same-reg)
               (eq lhs-reg rhs-reg))
              ((and (consp cond) (eq (car cond) :rconst))
               (and (%opt-known-constant-p rval) (eql rval (cadr cond))))
              ((and (consp cond) (eq (car cond) :lconst))
               (and (%opt-known-constant-p lval) (eql lval (cadr cond)))))
        (return (%opt-apply-algebraic-action action dst lhs-reg rhs-reg))))))

(defun %opt-branch-target-labels (instructions)
  "Return a hash table of labels that are explicit branch targets."
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (inst instructions targets)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (setf (gethash (vm-label-name inst) targets) t))))))

(defun %fold-vm-label (inst env low-bit-facts emit target-labels)
  "Flush constant knowledge only at labels that are explicit branch targets."
  (when (gethash (vm-name inst) target-labels)
    (clrhash env)
    (clrhash low-bit-facts))
  (funcall emit inst))

(defun %fold-vm-const (inst env low-bit-facts emit)
  "Record the constant value of INST's destination in ENV, then emit."
  (remhash (vm-dst inst) low-bit-facts)
  (setf (gethash (vm-dst inst) env) (vm-value inst))
  (funcall emit inst))

(defun %fold-vm-move (inst env low-bit-facts emit emit-const clear)
  "Propagate constant if src is known in ENV; eliminate self-moves silently."
  (let ((src (vm-src inst)) (dst (vm-dst inst)))
    (cond
      ((eq src dst)) ; self-move: drop silently
      (t
       (multiple-value-bind (sval found) (gethash src env)
          (if found
              (progn
                (remhash dst low-bit-facts)
                (funcall emit-const dst sval))
              (progn
                (funcall clear dst)
                (%opt-copy-derived-fact src dst low-bit-facts)
                (funcall emit inst))))))))

(defun %fold-binary-inst (inst env low-bit-facts emit emit-const clear)
  "Binary arithmetic/comparison: full fold or algebraic simplification.
   Dispatch derived from opt-binary-lhs-rhs-p (covers *opt-binary-lhs-rhs-types*).
   vm-floor-inst/vm-ceiling-inst/vm-truncate are excluded (values-list side-channel)."
  (let* ((dst (vm-dst inst))
         (lhs (vm-lhs inst))
         (rhs (vm-rhs inst)))
    (multiple-value-bind (lval lfound) (gethash lhs env)
      (multiple-value-bind (rval rfound) (gethash rhs env)
        (cond
          ;; Both operands are known numeric constants → full fold
          ((and lfound rfound (numberp lval) (numberp rval))
           (multiple-value-bind (folded ok)
               (opt-fold-binop-value inst lval rval)
              (if ok
                  (progn
                    (remhash dst low-bit-facts)
                    (funcall emit-const dst folded))
                  (progn
                    (remhash dst low-bit-facts)
                    (funcall clear dst)
                    (funcall emit inst)))))
          ;; Try algebraic identity simplification
          (t
           (let ((parity-rewrite
                   (and (typep inst '(or vm-num-eq vm-eq))
                        (%opt-rewrite-logand-low-bit-test
                         inst lhs rhs lval lfound rval rfound low-bit-facts)))
                 (simp (opt-simplify-binop inst dst lhs rhs
                                           (if lfound lval :unknown)
                                           (if rfound rval :unknown))))
             (cond
               (parity-rewrite
                (remhash dst low-bit-facts)
                (funcall clear dst)
                (funcall emit parity-rewrite))
               (simp
                (when (vm-const-p simp)
                  (setf (gethash dst env) (vm-const-value simp)))
                (cond
                  ((vm-const-p simp)
                   (remhash dst low-bit-facts))
                  ((typep simp 'vm-move)
                   (funcall clear dst)
                   (%opt-copy-derived-fact (vm-src simp) dst low-bit-facts))
                  (t
                   (funcall clear dst)
                   (remhash dst low-bit-facts)))
                (funcall emit simp))
               (t
                (funcall clear dst)
                (let ((source (%opt-logand-low-bit-source lhs rhs lval lfound rval rfound)))
                  (if (and (typep inst 'vm-logand) source)
                      (setf (gethash dst low-bit-facts) source)
                      (remhash dst low-bit-facts)))
                (funcall emit inst))))))))))

(defun %fold-unary-constant-eligible-p (inst value)
  "Return T when unary INST can be safely folded for constant VALUE."
  (or (numberp value)
      (eq (type-of inst) 'vm-not)
      (and (member (type-of inst) '(vm-car vm-cdr) :test #'eq)
           (or (consp value) (null value)))))

(defun %fold-unary-inst (inst env low-bit-facts emit emit-const clear)
  "Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found fold-fn
                (%fold-unary-constant-eligible-p inst sval))
          (progn
            (remhash dst low-bit-facts)
            (funcall emit-const dst (funcall fold-fn sval)))
          (progn
            (funcall clear dst)
            (remhash dst low-bit-facts)
            (funcall emit inst))))))

(defun %fold-type-pred-inst (inst env low-bit-facts emit emit-const clear)
  "Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found pred-fn)
          (progn
            (remhash dst low-bit-facts)
            (funcall emit-const dst (if (funcall pred-fn sval) 1 0)))
          (progn
            (funcall clear dst)
            (remhash dst low-bit-facts)
            (funcall emit inst))))))

(defun %fold-vm-jump-zero (inst env emit)
  "Constant branch folding: known-false → unconditional jump; known-true → drop."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (if found
        (if (opt-falsep val)
            (funcall emit (make-vm-jump :label (vm-label-name inst)))
            nil) ; condition is always true → branch never taken → drop
        (funcall emit inst))))

(defun %fold-default-inst (inst env low-bit-facts emit clear)
  "Default: invalidate any written destination register, then emit."
  (declare (ignore env))
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (funcall clear dst)
      (remhash dst low-bit-facts)))
  (funcall emit inst))

(defun opt-pass-fold (instructions)
  "Forward pass: constant folding, algebraic simplification, constant branch elimination."
  (let ((env (make-hash-table :test #'eq)) ; reg → known constant value
        (low-bit-facts (make-hash-table :test #'eq)) ; reg → original src for (logand src 1)
        (target-labels (%opt-branch-target-labels instructions))
        (result nil))
    (flet ((emit (inst) (push inst result))
           (emit-const (dst val)
             (setf (gethash dst env) val)
             (push (make-vm-const :dst dst :value val) result))
            (clear (reg) (remhash reg env)))
      (dolist (inst instructions)
        (typecase inst
          (vm-label    (%fold-vm-label      inst env low-bit-facts #'emit target-labels))
          (vm-const    (%fold-vm-const      inst env low-bit-facts #'emit))
          (vm-move     (%fold-vm-move       inst env low-bit-facts #'emit #'emit-const #'clear))
          (vm-jump-zero (%fold-vm-jump-zero inst env #'emit))
          (t
           (cond
              ((opt-binary-lhs-rhs-p inst)      (%fold-binary-inst    inst env low-bit-facts #'emit #'emit-const #'clear))
              ((opt-foldable-unary-arith-p inst) (%fold-unary-inst     inst env low-bit-facts #'emit #'emit-const #'clear))
              ((opt-foldable-type-pred-p inst)   (%fold-type-pred-inst inst env low-bit-facts #'emit #'emit-const #'clear))
              (t                                 (%fold-default-inst   inst env low-bit-facts #'emit #'clear))))))
    (nreverse result))))

;;; ─── Top-Level Optimizer ─────────────────────────────────────────────────
