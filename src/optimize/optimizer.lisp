(in-package :cl-cc)
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

(defun opt-simplify-binop (inst dst lhs-reg rhs-reg lval rval)
  "Algebraic simplification of binary INST via rule table lookup.
   LVAL/RVAL are known constant values or :unknown.
   Returns a simplified instruction, or NIL if no simplification applies."
  (let ((rules (gethash (type-of inst) *opt-algebraic-identity-rules*)))
    (flet ((const-p (v) (and (not (eq v :unknown)) (numberp v)))
           (apply-action (action)
             (case action
               (:move-lhs (make-vm-move :dst dst :src lhs-reg))
               (:move-rhs (make-vm-move :dst dst :src rhs-reg))
               (otherwise
                (if (consp action)
                    (case (car action)
                      (:const (make-vm-const :dst dst :value (cadr action)))
                      (:neg   (if (eq (cadr action) :lhs)
                                  (make-vm-neg :dst dst :src lhs-reg)
                                  (make-vm-neg :dst dst :src rhs-reg))))
                    nil)))))
      (dolist (rule rules nil)
        (let ((cond (car rule))
              (action (cdr rule)))
          (when (cond
                  ((eq cond :same-reg) (eq lhs-reg rhs-reg))
                  ((and (consp cond) (eq (car cond) :rconst))
                   (and (const-p rval) (eql rval (cadr cond))))
                  ((and (consp cond) (eq (car cond) :lconst))
                   (and (const-p lval) (eql lval (cadr cond)))))
            (return (apply-action action))))))))

(defun %opt-branch-target-labels (instructions)
  "Return a hash table of labels that are explicit branch targets."
  (let ((targets (make-hash-table :test #'equal)))
    (dolist (inst instructions targets)
      (typecase inst
        ((or vm-jump vm-jump-zero)
         (setf (gethash (vm-label-name inst) targets) t))))))

(defun %fold-vm-label (inst env emit target-labels)
  "Flush constant knowledge only at labels that are explicit branch targets."
  (when (gethash (vm-name inst) target-labels)
    (clrhash env))
  (funcall emit inst))

(defun %fold-vm-const (inst env emit)
  "Record the constant value of INST's destination in ENV, then emit."
  (setf (gethash (vm-dst inst) env) (vm-value inst))
  (funcall emit inst))

(defun %fold-vm-move (inst env emit emit-const clear)
  "Propagate constant if src is known in ENV; eliminate self-moves silently."
  (let ((src (vm-src inst)) (dst (vm-dst inst)))
    (cond
      ((eq src dst)) ; self-move: drop silently
      (t
       (multiple-value-bind (sval found) (gethash src env)
         (if found
             (funcall emit-const dst sval)
             (progn (funcall clear dst) (funcall emit inst))))))))

(defun %fold-binary-inst (inst env emit emit-const clear)
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
                 (funcall emit-const dst folded)
                 (progn (funcall clear dst) (funcall emit inst)))))
          ;; Try algebraic identity simplification
          (t
           (let ((simp (opt-simplify-binop inst dst lhs rhs
                                           (if lfound lval :unknown)
                                           (if rfound rval :unknown))))
             (cond
               (simp
                (when (vm-const-p simp)
                  (setf (gethash dst env) (vm-const-value simp)))
                (unless (vm-const-p simp) (funcall clear dst))
                (funcall emit simp))
               (t (funcall clear dst) (funcall emit inst))))))))))

(defun %fold-unary-inst (inst env emit emit-const clear)
  "Unary arithmetic: data-driven via opt-foldable-unary-arith-p → *opt-unary-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (fold-fn (gethash (type-of inst) *opt-unary-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found fold-fn
               (or (numberp sval)
                   ;; vm-not handles non-numeric values (nil → t)
                   (eq (type-of inst) 'vm-not)))
          (funcall emit-const dst (funcall fold-fn sval))
          (progn (funcall clear dst) (funcall emit inst))))))

(defun %fold-type-pred-inst (inst env emit emit-const clear)
  "Type predicates: data-driven via opt-foldable-type-pred-p → *opt-type-pred-fold-table*."
  (let* ((dst (vm-dst inst)) (src (vm-src inst))
         (pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
    (multiple-value-bind (sval found) (gethash src env)
      (if (and found pred-fn)
          (funcall emit-const dst (if (funcall pred-fn sval) 1 0))
          (progn (funcall clear dst) (funcall emit inst))))))

(defun %fold-vm-jump-zero (inst env emit)
  "Constant branch folding: known-false → unconditional jump; known-true → drop."
  (multiple-value-bind (val found) (gethash (vm-reg inst) env)
    (if found
        (if (opt-falsep val)
            (funcall emit (make-vm-jump :label (vm-label-name inst)))
            nil) ; condition is always true → branch never taken → drop
        (funcall emit inst))))

(defun %fold-default-inst (inst env emit clear)
  "Default: invalidate any written destination register, then emit."
  (declare (ignore env))
  (let ((dst (opt-inst-dst inst)))
    (when dst (funcall clear dst)))
  (funcall emit inst))

(defun opt-pass-fold (instructions)
  "Forward pass: constant folding, algebraic simplification, constant branch elimination."
  (let ((env (make-hash-table :test #'eq)) ; reg → known constant value
        (target-labels (%opt-branch-target-labels instructions))
        (result nil))
    (flet ((emit (inst) (push inst result))
           (emit-const (dst val)
             (setf (gethash dst env) val)
             (push (make-vm-const :dst dst :value val) result))
           (clear (reg) (remhash reg env)))
      (dolist (inst instructions)
        (cond
          ((typep inst 'vm-label)         (%fold-vm-label      inst env #'emit target-labels))
          ((typep inst 'vm-const)         (%fold-vm-const      inst env #'emit))
          ((typep inst 'vm-move)          (%fold-vm-move       inst env #'emit #'emit-const #'clear))
          ((opt-binary-lhs-rhs-p inst)    (%fold-binary-inst   inst env #'emit #'emit-const #'clear))
          ((opt-foldable-unary-arith-p inst) (%fold-unary-inst inst env #'emit #'emit-const #'clear))
          ((opt-foldable-type-pred-p inst)(%fold-type-pred-inst inst env #'emit #'emit-const #'clear))
          ((typep inst 'vm-jump-zero)     (%fold-vm-jump-zero  inst env #'emit))
          (t                              (%fold-default-inst  inst env #'emit #'clear)))))
    (nreverse result)))

;;; ─── Pass 1b: Sparse Conditional Constant Propagation ────────────────────

(defun %sccp-env-copy (env)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %sccp-env-equal-p (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (let ((same t))
         (maphash (lambda (k v)
                    (unless (multiple-value-bind (bv found) (gethash k b)
                              (and found (equal v bv)))
                      (setf same nil)))
                  a)
         same)))

(defun %sccp-env-merge (envs)
  "Intersect constant bindings across all ENVs."
  (cond
    ((null envs) (make-hash-table :test #'eq))
    ((null (cdr envs)) (%sccp-env-copy (car envs)))
    (t (let ((merged (%sccp-env-copy (car envs))))
         (maphash (lambda (k v)
                    (dolist (env (cdr envs))
                      (multiple-value-bind (ov found) (gethash k env)
                        (unless (and found (equal ov v))
                          (remhash k merged)
                          (return))))
                    )
                  merged)
         merged))))

(defun %sccp-fold-inst (inst env)
  (labels ((known (reg)
             (multiple-value-bind (val found) (gethash reg env)
               (if found (values val t) (values nil nil)))))
    (typecase inst
      (vm-const inst)
      (vm-label inst)
      (vm-jump-zero
       (multiple-value-bind (val found) (known (vm-reg inst))
         (cond
           ((and found (opt-falsep val)) (make-vm-jump :label (vm-label-name inst)))
           (found nil)
           (t inst))))
      (vm-move
       (multiple-value-bind (val found) (known (vm-src inst))
         (if found
             (make-vm-const :dst (vm-dst inst) :value val)
             inst)))
      ((or vm-add vm-integer-add vm-float-add
           vm-sub vm-integer-sub vm-float-sub
           vm-mul vm-integer-mul vm-float-mul
           vm-mod vm-rem vm-min vm-max
           vm-logand vm-logior vm-logxor vm-logeqv vm-ash vm-div vm-cl-div vm-float-div
           vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq)
       (multiple-value-bind (lval lfound) (known (vm-lhs inst))
         (multiple-value-bind (rval rfound) (known (vm-rhs inst))
           (if (and lfound rfound (numberp lval) (numberp rval))
               (multiple-value-bind (folded ok) (opt-fold-binop-value inst lval rval)
                 (if ok (make-vm-const :dst (vm-dst inst) :value folded) inst))
               inst))))
      ((or vm-neg vm-abs vm-inc vm-dec vm-lognot vm-bswap)
       (multiple-value-bind (sval found) (known (vm-src inst))
         (if (and found (numberp sval))
             (let* ((fold-fn (gethash (type-of inst) *opt-unary-fold-table*))
                    (dst (vm-dst inst)))
               (if fold-fn
                   (make-vm-const :dst dst :value (funcall fold-fn sval))
                   inst))
             inst)))
      ((or vm-null-p vm-cons-p vm-symbol-p vm-number-p vm-integer-p vm-function-p)
       (multiple-value-bind (sval found) (known (vm-src inst))
         (if found
             (let ((pred-fn (gethash (type-of inst) *opt-type-pred-fold-table*)))
               (if pred-fn
                   (make-vm-const :dst (vm-dst inst) :value (if (funcall pred-fn sval) 1 0))
                   inst))
             inst)))
      (vm-concatenate
       (let ((parts (or (vm-parts inst) (list (vm-str1 inst) (vm-str2 inst)))))
         (if (every (lambda (reg)
                      (multiple-value-bind (val found) (known reg)
                        (and found (stringp val))))
                    parts)
             (make-vm-const :dst (vm-dst inst)
                            :value (apply #'concatenate 'string
                                          (mapcar (lambda (reg) (gethash reg env)) parts)))
             inst)))
      (t inst))))

(defun opt-pass-sccp (instructions)
  "Sparse conditional constant propagation over the CFG.
   Propagates constants across blocks and folds constant branches."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (let ((in-envs  (make-hash-table :test #'eq))
            (out-envs (make-hash-table :test #'eq))
            (worklist (list (cfg-entry cfg))))
        (setf (gethash (cfg-entry cfg) in-envs) (make-hash-table :test #'eq))
        (labels ((pred-out-envs (block)
                   (loop for p in (bb-predecessors block)
                         for e = (gethash p out-envs)
                         when e collect e))
                 (redirect-successors (block new-succs)
                   (dolist (old (bb-successors block))
                     (setf (bb-predecessors old)
                           (remove block (bb-predecessors old) :test #'eq)))
                   (setf (bb-successors block) new-succs)
                   (dolist (succ new-succs)
                     (pushnew block (bb-predecessors succ) :test #'eq)))
                 (merge-in-env (block)
                   (let ((pred-envs (pred-out-envs block)))
                     (cond
                       ((null pred-envs) (make-hash-table :test #'eq))
                       ((null (cdr pred-envs)) (%sccp-env-copy (car pred-envs)))
                       (t (let* ((first (%sccp-env-copy (car pred-envs)))
                                 (keep (make-hash-table :test #'eq)))
                            (maphash (lambda (k v)
                                       (when (every (lambda (env)
                                                      (multiple-value-bind (ov found) (gethash k env)
                                                        (and found (equal ov v))))
                                                    (cdr pred-envs))
                                         (setf (gethash k keep) v)))
                                     first)
                            keep)))))
                 (process-block (block in-env)
                    (let ((env (%sccp-env-copy in-env))
                          (new-insts nil))
                      (dolist (inst (bb-instructions block))
                        (let ((folded (%sccp-fold-inst inst env)))
                          (cond
                            ((and (typep inst 'vm-jump-zero) (null folded))
                             (let ((succs (bb-successors block)))
                               (redirect-successors block (if (second succs)
                                                             (list (second succs))
                                                             nil))))
                            ((and (typep inst 'vm-jump-zero) (typep folded 'vm-jump))
                             (let ((succs (bb-successors block)))
                               (redirect-successors block (list (first succs))))
                             (push folded new-insts)
                             (let ((dst (opt-inst-dst folded)))
                               (when dst
                                 (typecase folded
                                   (vm-const (setf (gethash dst env) (vm-value folded)))
                                   (t (remhash dst env))))))
                            ((null folded) nil)
                            (t
                             (push folded new-insts)
                             (let ((dst (opt-inst-dst folded)))
                               (when dst
                                 (typecase folded
                                  (vm-const (setf (gethash dst env) (vm-value folded)))
                                  (t (remhash dst env)))))))))
                     (setf (bb-instructions block) (nreverse new-insts))
                     env))
                 (process-worklist ()
                   (when worklist
                     (let* ((block (pop worklist))
                            (new-in (merge-in-env block))
                            (old-in (gethash block in-envs)))
                       (when (or (null old-in)
                                 (null (gethash block out-envs))
                                 (not (%sccp-env-equal-p old-in new-in)))
                         (setf (gethash block in-envs) new-in)
                         (let ((new-out (process-block block new-in)))
                           (let ((old-out (gethash block out-envs)))
                             (unless (and old-out (%sccp-env-equal-p old-out new-out))
                               (setf (gethash block out-envs) new-out)
                               (dolist (succ (bb-successors block))
                                 (pushnew succ worklist :test #'eq)))))))
                     (process-worklist))))
          (process-worklist)))
      (let ((linear (loop for b across (cfg-blocks cfg)
                          when b append (append (when (bb-label b)
                                                    (list (bb-label b)))
                                                (copy-list (bb-instructions b))))))
        (cfg-flatten (cfg-build linear))))))

;;; ─── Tree Walker (shared utility) ────────────────────────────────────────

(defun opt-map-tree (fn tree)
  "Apply FN to every leaf of TREE (a possibly-improper nested cons tree)."
  (if (consp tree)
      (cons (opt-map-tree fn (car tree))
            (opt-map-tree fn (cdr tree)))
      (funcall fn tree)))

(defun %opt-copy-prop-env-copy (env)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %opt-copy-prop-env-equal-p (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (let ((same t))
         (maphash (lambda (k v)
                    (unless (multiple-value-bind (bv found) (gethash k b)
                              (and found (eq v bv)))
                      (setf same nil)))
                  a)
         same)))

(defun %opt-copy-prop-canonical (reg copies)
  (loop with seen = (make-hash-table :test #'eq)
        for current = reg then next
        for next = (gethash current copies)
        while (and next (not (gethash current seen)))
        do (setf (gethash current seen) t)
        finally (return current)))

(defun %opt-copy-prop-build-reverse (copies)
  (let ((reverse (make-hash-table :test #'eq)))
    (maphash (lambda (dst src)
               (push dst (gethash src reverse)))
             copies)
    reverse))

(defun %opt-copy-prop-add (dst src copies reverse)
  (setf (gethash dst copies) src)
  (push dst (gethash src reverse)))

(defun %opt-copy-prop-kill (reg copies reverse)
  (multiple-value-bind (src found) (gethash reg copies)
    (when found
      (setf (gethash src reverse)
            (delete reg (gethash src reverse) :test #'eq))
      (remhash reg copies)))
  (let ((dependents (copy-list (gethash reg reverse))))
    (dolist (dst dependents)
      (remhash dst copies))
    (remhash reg reverse)))

(defun %opt-value-rank (value)
  (typecase value
    (null 0)
    (number 1)
    (character 2)
    (string 3)
    (symbol 4)
    (cons 5)
    (vector 6)
    (t 7)))

(defun %opt-value< (a b)
  "Deterministic structural ordering without printed-string allocation."
  (let ((ra (%opt-value-rank a))
        (rb (%opt-value-rank b)))
    (cond
      ((< ra rb) t)
      ((> ra rb) nil)
      ((null a) nil)
      ((numberp a) (and (/= a b) (< a b)))
      ((characterp a) (< (char-code a) (char-code b)))
      ((stringp a) (string< a b))
      ((symbolp a)
       (let ((apkg (symbol-package a))
             (bpkg (symbol-package b)))
         (cond
           ((and apkg bpkg
                 (not (string= (package-name apkg) (package-name bpkg))))
            (string< (package-name apkg) (package-name bpkg)))
           ((and apkg (null bpkg)) nil)
           ((and (null apkg) bpkg) t)
           (t (string< (symbol-name a) (symbol-name b))))))
      ((consp a)
       (if (equal (car a) (car b))
           (%opt-value< (cdr a) (cdr b))
           (%opt-value< (car a) (car b))))
      ((vectorp a)
       (loop for av across a
             for bv across b
             do (unless (equal av bv)
                  (return (%opt-value< av bv)))
             finally (return (< (length a) (length b)))))
      (t
       (let ((ta (type-of a))
             (tb (type-of b)))
         (cond
           ((not (eq ta tb)) (%opt-value< ta tb))
           ((equal a b) nil)
           (t (< (sxhash a) (sxhash b)))))))))

(defun %opt-copy-prop-merge (envs)
  (cond
    ((null envs) (make-hash-table :test #'eq))
    ((null (cdr envs)) (%opt-copy-prop-env-copy (car envs)))
    (t (let ((merged (%opt-copy-prop-env-copy (car envs))))
         (maphash (lambda (k v)
                    (dolist (env (cdr envs))
                      (multiple-value-bind (ov found) (gethash k env)
                        (unless (and found (eq ov v))
                          (remhash k merged)
                          (return)))))
                  merged)
         merged))))

(defun %opt-copy-prop-transfer-block (block in-env)
  (let ((copies (%opt-copy-prop-env-copy in-env))
        (reverse (%opt-copy-prop-build-reverse in-env)))
    (dolist (inst (bb-instructions block))
      (typecase inst
        (vm-move
         (let* ((dst (vm-move-dst inst))
                 (src (%opt-copy-prop-canonical (vm-move-src inst) copies)))
            (%opt-copy-prop-kill dst copies reverse)
            (unless (eq dst src)
              (%opt-copy-prop-add dst src copies reverse))))
        (t
          (let ((dst (opt-inst-dst inst)))
            (when dst
              (%opt-copy-prop-kill dst copies reverse))))))
    copies))

(defun %opt-copy-prop-rewrite-inst (inst copies)
  (let ((sexp (instruction->sexp inst)))
    (flet ((rewrite (x)
             (if (opt-register-keyword-p x)
                 (%opt-copy-prop-canonical x copies)
                 x)))
      (handler-case
          (let* ((has-dst (not (null (opt-inst-dst inst))))
                 (new-sexp (if has-dst
                               (list* (first sexp)
                                      (second sexp)
                                      (opt-map-tree #'rewrite (cddr sexp)))
                               (cons (first sexp)
                                     (opt-map-tree #'rewrite (cdr sexp))))))
            (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
        (error () inst)))))

(defun %opt-copy-prop-rewrite-block (block in-env)
  (let ((copies (%opt-copy-prop-env-copy in-env))
        (reverse (%opt-copy-prop-build-reverse in-env))
        (result nil))
    (dolist (inst (bb-instructions block))
      (typecase inst
        (vm-move
         (let* ((dst (vm-move-dst inst))
                 (src (%opt-copy-prop-canonical (vm-move-src inst) copies)))
            (%opt-copy-prop-kill dst copies reverse)
            (unless (eq dst src)
              (%opt-copy-prop-add dst src copies reverse)
              (push (if (eq src (vm-move-src inst))
                        inst
                        (make-vm-move :dst dst :src src))
                    result))))
        (t
          (let ((rewritten (%opt-copy-prop-rewrite-inst inst copies))
                (dst (opt-inst-dst inst)))
            (when dst
              (%opt-copy-prop-kill dst copies reverse))
            (push rewritten result)))))
    (nreverse result)))

(defun opt-pass-copy-prop (instructions)
  "Global copy propagation over the CFG using a forward reaching-copy analysis.

   Copy facts are intersected at CFG joins, so only copies that are valid on all
   incoming paths are propagated. The final rewrite phase then substitutes the
   stabilized canonical registers inside each reachable basic block."
  (let ((cfg (cfg-build instructions)))
    (if (null (cfg-entry cfg))
        instructions
        (progn
          (let ((in-envs (make-hash-table :test #'eq))
                (out-envs (make-hash-table :test #'eq))
                (worklist (list (cfg-entry cfg)))
                (queued (make-hash-table :test #'eq)))
            (setf (gethash (cfg-entry cfg) queued) t)
            (labels ((enqueue (block)
                       (unless (gethash block queued)
                         (setf (gethash block queued) t)
                         (push block worklist)))
                     (process-block (block)
                       (let* ((preds (bb-predecessors block))
                              (incoming (cond
                                         ((null preds) (make-hash-table :test #'eq))
                                         ((null (cdr preds))
                                          (let ((pred-out (gethash (first preds) out-envs)))
                                            (if pred-out
                                                (%opt-copy-prop-env-copy pred-out)
                                                (make-hash-table :test #'eq))))
                                         (t (%opt-copy-prop-merge
                                             (mapcar (lambda (pred)
                                                       (or (gethash pred out-envs)
                                                           (make-hash-table :test #'eq)))
                                                     preds)))))
                              (old-in (gethash block in-envs))
                              (changed nil))
                         (unless (and old-in (%opt-copy-prop-env-equal-p old-in incoming))
                           (setf (gethash block in-envs) incoming
                                 changed t))
                         (let ((new-out (%opt-copy-prop-transfer-block block incoming))
                               (old-out (gethash block out-envs)))
                           (unless (and old-out (%opt-copy-prop-env-equal-p old-out new-out))
                             (setf (gethash block out-envs) new-out
                                   changed t)
                             (dolist (succ (bb-successors block))
                               (enqueue succ))))
                         changed)))
              (loop while worklist
                    for block = (pop worklist)
                    do (remhash block queued)
                       (process-block block)))

            (loop for block across (cfg-blocks cfg)
                  do (setf (bb-instructions block)
                           (%opt-copy-prop-rewrite-block
                            block
                            (or (gethash block in-envs)
                                (make-hash-table :test #'eq))))))
           (cfg-flatten cfg)))))

(defun opt-heap-root-inst-p (inst)
  "Return T when INST produces a fresh heap-like object identity."
  (typep inst '(or vm-cons vm-make-array vm-closure vm-make-closure)))

(defun opt-compute-heap-aliases (instructions)
  "Compute a conservative EQ hash-table reg -> canonical heap root.

Fresh heap producers start a new alias root at their destination register.
vm-move preserves the source root. Any other destination write kills alias info.
This is a small FR-115 style oracle intended for downstream passes." 
  (let ((roots (make-hash-table :test #'eq)))
    (dolist (inst instructions roots)
      (let ((dst (opt-inst-dst inst)))
        (cond
          ((and dst (opt-heap-root-inst-p inst))
           (setf (gethash dst roots) dst))
          ((typep inst 'vm-move)
           (multiple-value-bind (root found-p)
               (gethash (vm-move-src inst) roots)
             (if found-p
                 (setf (gethash dst roots) root)
                 (remhash dst roots))))
          (dst
           (remhash dst roots)))))
    roots))

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
  (flet ((c (x) (if (opt-register-keyword-p x) (or (gethash x copies) x) x)))
    (handler-case
        (let* ((sexp      (instruction->sexp inst))
               (has-dst   (not (null (opt-inst-dst inst))))
               ;; Rewrite all leaves; for instructions with a dst, the dst sits at
               ;; position 1 (immediately after the opcode tag) — leave it intact.
               (new-sexp  (if has-dst
                              (list* (first sexp) (second sexp)
                                     (opt-map-tree #'c (cddr sexp)))
                              (cons  (first sexp)
                                     (opt-map-tree #'c (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

;;; ─── Pass 2: Dead Code Elimination ──────────────────────────────────────

(defun opt-pass-dce (instructions)
  "Dead code elimination via global usedness analysis.
   Pass 1: collect every register that appears as a source operand anywhere
   in the program into a 'used' set (ignoring control flow).
   Pass 2: remove pure instructions (vm-const, vm-move) whose destination
   register never appears in the used set.
   This is safe across branches/labels: a register defined in both branches
   is preserved as long as it is read anywhere -- the linear-order issue that
   plagued the previous backward-liveness DCE is entirely avoided."
  (let ((used (make-hash-table :test #'eq)))
    ;; Pass 1: mark every register that is read by any instruction
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    ;; Pass 2: drop DCE-eligible instructions (pure + alloc) whose dst is never read
    (remove-if (lambda (inst)
                 (and (opt-inst-dce-eligible-p inst)
                      (let ((dst (ignore-errors (vm-dst inst))))
                        (and dst (not (gethash dst used))))))
               instructions)))

;;; ─── Pass 3: Jump Threading + Dead Jump Elimination ─────────────────────

(defun opt-build-label-index (instructions)
  "Return (values vector label→index-ht) for threading analysis."
  (let ((vec (coerce instructions 'vector))
        (idx (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          when (vm-label-p (aref vec i))
          do (setf (gethash (vm-name (aref vec i)) idx) i))
    (values vec idx)))

(defun opt-thread-label (label idx vec &optional (seen (make-hash-table :test #'equal)))
  "Follow jump chains starting at LABEL. Returns the ultimate jump target label.

   Cycle detection prevents infinite loops in pathological (cyclic) code while
   still allowing long but valid jump chains to fully thread."
  (when (gethash label seen)
    (return-from opt-thread-label label))
  (setf (gethash label seen) t)
  (let ((pos (gethash label idx)))
    (unless pos (return-from opt-thread-label label))
    ;; Scan forward past any labels to find the first real instruction
    (loop for i from pos below (length vec)
          for inst = (aref vec i)
          do (typecase inst
                (vm-label nil) ; skip label markers
                (vm-jump ; found a chained jump → follow it
                 (let ((next (vm-label-name inst)))
                   (return-from opt-thread-label
                     (opt-thread-label next idx vec seen))))
                (t (return-from opt-thread-label label)))))
  label)

(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return nil)))

(defun opt-pass-jump (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
            do (typecase inst
                 (vm-jump
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (unless (opt-falls-through-to-p vec i threaded)
                      (push (if (equal threaded (vm-label-name inst))
                                inst
                                (make-vm-jump :label threaded))
                            result))))
                 (vm-jump-zero
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (push (if (equal threaded (vm-label-name inst))
                              inst
                              (make-vm-jump-zero :reg (vm-reg inst) :label threaded))
                          result)))
                 (t (push inst result))))
      (nreverse result))))

;;; ─── Pass 4: Unreachable Code Elimination ────────────────────────────────

(defun opt-pass-unreachable (instructions)
  "Remove instructions that follow unconditional control transfers (jump/ret)
   and precede the next label — they can never be executed."
  (let ((result nil)
        (dead nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (setf dead nil)
         (push inst result))
        (t
         (unless dead
           (push inst result))
         ;; Mark subsequent instructions as unreachable after unconditional transfer
          (when (or (vm-jump-p inst) (vm-ret-p inst))
            (setf dead t)))))
    (nreverse result)))

(defun opt-pass-dead-basic-blocks (instructions)
  "Eliminate unreachable basic blocks by round-tripping through the CFG.
   Reachability is computed from the entry block; unreachable blocks and their
   instructions are dropped when the CFG is flattened back to a linear list."
  (cfg-flatten (cfg-build instructions)))

(defun opt-pass-dominated-type-check-elim (instructions)
  "Eliminate redundant pure type predicates dominated by an earlier identical
    predicate on the same source register. Nil checks via vm-not are treated
    the same way. The first check is kept; later checks are replaced with
    vm-move from the dominating result register."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (labels ((copy-facts (facts)
                 (copy-list facts))
               (forget-def (facts reg)
                 (remove-if (lambda (fact)
                              (or (eq (getf fact :src) reg)
                                  (eq (getf fact :dst) reg)))
                            facts))
               (lookup-fact (facts pred src)
                 (find-if (lambda (fact)
                            (and (eq (getf fact :pred) pred)
                                 (eq (getf fact :src) src)))
                          facts))
               (process-block (block facts)
                 (let ((local-facts (copy-facts facts))
                       (new-insts nil))
                    (dolist (inst (bb-instructions block))
                      (let ((dst (opt-inst-dst inst)))
                        (when dst
                          (setf local-facts (forget-def local-facts dst))))
                      (cond
                        ((and (or (opt-foldable-type-pred-p inst)
                                  (typep inst 'vm-not))
                              (vm-src inst)
                              (vm-dst inst))
                         (let* ((pred (type-of inst))
                                (src  (vm-src inst))
                                (dst  (vm-dst inst))
                                (fact (lookup-fact local-facts pred src)))
                          (if (and fact (not (eq dst (getf fact :dst))))
                              (push (make-vm-move :dst dst :src (getf fact :dst)) new-insts)
                              (progn
                                (push inst new-insts)
                                (push (list :pred pred :src src :dst dst) local-facts)))))
                       (t
                        (push inst new-insts))))
                   (setf (bb-instructions block) (nreverse new-insts))
                   (dolist (child (bb-dom-children block))
                     (process-block child local-facts)))))
         (process-block (cfg-entry cfg) nil)))
     (cfg-flatten cfg)))

(defun opt-pass-nil-check-elim (instructions)
  "Eliminate redundant nil checks dominated by an earlier identical check.

   This is a thin specialization of dominated type-check elimination so FR-040
   exists as a named optimizer pass in the pipeline."
  (opt-pass-dominated-type-check-elim instructions))

(defun %opt-branch-predicate-fact-for-block (block)
  "Return a branch fact plist for BLOCK, or NIL.

The fact is inferred only when BLOCK has a single predecessor ending in a
vm-jump-zero whose condition register is defined by a foldable type predicate
or vm-not in that predecessor block. The returned plist contains:
  :pred  instruction type
  :src   predicate source register
  :value replacement constant (1 on fallthrough, 0 on taken branch)."
  (when (= 1 (length (bb-predecessors block)))
    (let* ((pred (first (bb-predecessors block)))
           (term (car (last (bb-instructions pred))))
           (target-label (and (typep term 'vm-jump-zero)
                              (vm-label-name term)))
           (block-label (and (bb-label block) (vm-name (bb-label block)))))
      (when target-label
        (let ((cond-reg (vm-reg term)))
          (loop for inst in (reverse (bb-instructions pred))
                do (let ((dst (opt-inst-dst inst)))
                     (when (eq dst cond-reg)
                       (return
                         (when (or (opt-foldable-type-pred-p inst)
                                   (typep inst 'vm-not))
                           (list :pred (type-of inst)
                                 :src (vm-src inst)
                                 :value (if (and block-label
                                                 (equal block-label target-label))
                                            0
                                            1))))))))))))

(defun opt-pass-branch-correlation (instructions)
  "Propagate known predicate outcomes from a dominating conditional edge.

This is a conservative FR-168 style pass: when a block has exactly one
predecessor ending in vm-jump-zero over a foldable predicate, repeated tests of
the same predicate on the same source register inside the successor block are
replaced with vm-const 1/0." 
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg)
          do (let ((fact (%opt-branch-predicate-fact-for-block block)))
               (when fact
                 (let ((live-fact fact)
                       (new-insts nil))
                   (dolist (inst (bb-instructions block))
                     (let ((dst (opt-inst-dst inst)))
                       (when (and live-fact dst
                                  (eq dst (getf live-fact :src)))
                         (setf live-fact nil)))
                     (cond
                       ((and live-fact
                             (or (opt-foldable-type-pred-p inst)
                                 (typep inst 'vm-not))
                             (eq (type-of inst) (getf live-fact :pred))
                             (eq (vm-src inst) (getf live-fact :src))
                             (opt-inst-dst inst))
                        (push (make-vm-const :dst (opt-inst-dst inst)
                                             :value (getf live-fact :value))
                              new-insts))
                       (t
                        (push inst new-insts))))
                   (setf (bb-instructions block) (nreverse new-insts))))))
    (cfg-flatten cfg)))

(defun opt-pass-block-merge (instructions)
  "Merge linear CFG chains where a block has exactly one successor and that
   successor has exactly one predecessor. This removes redundant labels/jumps
   on straight-line code paths without changing branching structure."
  (let ((cfg (cfg-build instructions)))
    (labels ((mergeable-successor-p (block)
               (let ((succs (bb-successors block)))
                 (and (= (length succs) 1)
                      (let ((succ (first succs)))
                        (and (= (length (bb-predecessors succ)) 1)
                             (eq (first (bb-predecessors succ)) block))))))
             (strip-merge-jump (insts target-label)
               (if (and insts
                        (vm-jump-p (car (last insts)))
                        (equal (vm-label-name (car (last insts))) target-label))
                   (butlast insts)
                   insts))
            (emit-block (block visited suppress-label)
                (when (or (null block) (gethash block visited))
                  (return-from emit-block nil))
                (setf (gethash block visited) t)
                (let ((result nil))
                 (unless suppress-label
                   (when (bb-label block)
                     (push (bb-label block) result)))
                 (let ((insts (bb-instructions block)))
                   (if (mergeable-successor-p block)
                       (let* ((succ (first (bb-successors block)))
                              (succ-label (and (bb-label succ)
                                               (vm-name (bb-label succ)))))
                         (setf insts (if succ-label
                                         (strip-merge-jump insts succ-label)
                                         insts))
                         (setf result (nconc result insts))
                         (setf result (nconc result (emit-block succ visited t))))
                       (progn
                         (setf result (nconc result insts))
                         (dolist (succ (bb-successors block))
                           (setf result (nconc result (emit-block succ visited nil)))))))
                result)))
        (if (cfg-entry cfg)
            (emit-block (cfg-entry cfg) (make-hash-table :test #'eq) nil)
            instructions))))

(defun opt-pass-tail-merge (instructions)
  "Merge CFG blocks with identical bodies and identical successor labels.

   This is a conservative tail-merging pass: it only merges whole basic blocks
   whose instruction sequences and outgoing edges are exactly the same. That
   keeps the transformation safe while still removing duplicated block tails."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (labels ((succ-labels (block)
                 (mapcar (lambda (succ)
                           (and (bb-label succ) (vm-name (bb-label succ))))
                         (bb-successors block)))
               (block-signature (block)
                 (list (mapcar #'instruction->sexp (bb-instructions block))
                       (succ-labels block)))
               (replace-successor (block old new)
                 (setf (bb-successors block)
                       (mapcar (lambda (succ) (if (eq succ old) new succ))
                               (bb-successors block))))
                (rewrite-terminator (block old-label new-label)
                  (let ((cell (last (bb-instructions block))))
                    (when cell
                      (let ((last (car cell)))
                        (cond
                          ((and (typep last 'vm-jump)
                                (equal (vm-label-name last) old-label))
                           (setf (car cell) (make-vm-jump :label new-label)))
                          ((and (typep last 'vm-jump-zero)
                                (equal (vm-label-name last) old-label))
                           (setf (car cell)
                                 (make-vm-jump-zero :reg (vm-reg last)
                                                    :label new-label))))))))
               (merge-duplicate-blocks ()
                 (let ((canonical-by-sig (make-hash-table :test #'equal)))
                    (dolist (block (coerce (cfg-blocks cfg) 'list))
                     (let ((label (and (bb-label block) (vm-name (bb-label block)))))
                       (when label
                         (let* ((sig (block-signature block))
                                (canon (gethash sig canonical-by-sig)))
                           (if (null canon)
                               (setf (gethash sig canonical-by-sig) block)
                               (unless (eq canon block)
                                 (let* ((canon-label (and (bb-label canon)
                                                          (vm-name (bb-label canon))))
                                        (block-label label))
                                   (when (and canon-label block-label)
                                     (dolist (pred (copy-list (bb-predecessors block)))
                                       (replace-successor pred block canon)
                                       (rewrite-terminator pred block-label canon-label)
                                       (pushnew pred (bb-predecessors canon) :test #'eq))
                                     (setf (bb-predecessors block) nil))))))))))))
        (merge-duplicate-blocks)
        (cfg-flatten cfg)))))

(defun opt-pass-dead-store-elim (instructions)
  "Eliminate overwritten vm-set-global stores in straight-line code.

   Conservative rules:
   - a later store to the same global kills the earlier pending store
   - vm-get-global forces the matching pending store to be emitted first
   - labels and non-pure instructions flush all pending stores"
  (let ((result nil)
        (pending-order nil)
        (pending-by-name (make-hash-table :test #'equal)))
    (labels ((emit (inst)
               (push inst result))
             (pending-store (name)
               (gethash name pending-by-name))
             (remember-store (name inst)
               (unless (gethash name pending-by-name)
                 (push name pending-order))
               (setf (gethash name pending-by-name) inst))
             (drop-pending (name)
               (remhash name pending-by-name)
               (setf pending-order (remove name pending-order :test #'equal)))
             (flush-one (name)
               (let ((inst (pending-store name)))
                 (when inst
                   (emit inst)
                   (drop-pending name))))
             (flush-all ()
               (dolist (name (nreverse pending-order))
                 (flush-one name))))
       (dolist (inst instructions)
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (dolist (name (copy-list pending-order))
               (let ((pending (pending-store name)))
                 (when (and pending (eq (vm-src pending) dst))
                   (flush-one name))))))
         (typecase inst
           (vm-label
            (flush-all)
            (emit inst))
          ((or vm-jump vm-jump-zero vm-ret vm-halt)
           (flush-all)
           (emit inst))
          (vm-get-global
           (let ((name (vm-global-name inst)))
             (flush-one name)
             (emit inst)))
          (vm-set-global
           (remember-store (vm-global-name inst) inst))
          (t
           (unless (opt-inst-pure-p inst)
             (flush-all))
           (emit inst))))
       (flush-all)
       (nreverse result))))

(defun opt-pass-store-to-load-forward (instructions)
  "Forward pending vm-set-global values into matching vm-get-global loads.

   Conservative rules:
   - only straight-line code is considered
   - a vm-get-global/vm-slot-read is replaced with vm-move from the latest
     pending store for the same location
   - labels and non-pure instructions flush all pending stores
   - the store itself is still emitted later, preserving side effects"
  (let ((result nil)
         (pending-order nil)
         (pending-by-name (make-hash-table :test #'equal))
         (alias-roots (opt-compute-heap-aliases instructions)))
    (labels ((emit (inst)
               (push inst result))
             (pending-store (key)
               (gethash key pending-by-name))
             (remember-store (key inst)
               (unless (gethash key pending-by-name)
                 (push key pending-order))
               (setf (gethash key pending-by-name) inst))
             (drop-pending (key)
               (remhash key pending-by-name)
               (setf pending-order (remove key pending-order :test #'equal)))
             (flush-one (key)
               (let ((inst (pending-store key)))
                 (when inst
                   (emit inst)
                   (drop-pending key))))
             (flush-all ()
               (dolist (key (nreverse pending-order))
                 (flush-one key)))
             (flush-dependent-on-reg (reg &key exclude-key)
               (dolist (key (copy-list pending-order))
                 (let ((pending (pending-store key)))
                   (when (and pending (not (equal key exclude-key)))
                     (typecase pending
                       (vm-set-global
                        (when (eq (vm-src pending) reg)
                          (flush-one key)))
                       (vm-slot-write
                        (when (or (eq (vm-obj-reg pending) reg)
                                  (eq (vm-value-reg pending) reg))
                          (flush-one key)))))))))
       (dolist (inst instructions)
          (typecase inst
            (vm-label
             (flush-all)
             (emit inst))
           ((or vm-jump vm-jump-zero vm-ret vm-halt)
            (flush-all)
            (emit inst))
           (vm-get-global
            (let* ((key (list :global (vm-global-name inst)))
                   (store (pending-store key)))
              (if store
                  (progn
                    (emit (make-vm-move :dst (vm-dst inst) :src (vm-src store)))
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst) :exclude-key key)))
                  (progn
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst)))
                    (flush-one key)
                    (emit inst)))))
            (vm-slot-read
            (let* ((key (opt-slot-alias-key (vm-obj-reg inst)
                                            (vm-slot-name-sym inst)
                                            alias-roots))
                   (store (pending-store key)))
              (if store
                  (progn
                    (emit (make-vm-move :dst (vm-dst inst) :src (vm-value-reg store)))
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst) :exclude-key key)))
                  (progn
                    (when (vm-dst inst)
                      (flush-dependent-on-reg (vm-dst inst)))
                    (flush-one key)
                    (emit inst)))))
            (vm-set-global
             (remember-store (list :global (vm-global-name inst)) inst))
            (vm-slot-write
             (remember-store (opt-slot-alias-key (vm-obj-reg inst)
                                                 (vm-slot-name-sym inst)
                                                 alias-roots)
                             inst))
            (t
            (let ((dst (opt-inst-dst inst)))
              (when dst
                (flush-dependent-on-reg dst)))
            (unless (opt-inst-pure-p inst)
              (flush-all))
            (emit inst))))
      (flush-all)
      (nreverse result))))

;;; ─── Pass: Strength Reduction ────────────────────────────────────────────

(defun opt-power-of-2-p (n)
  "T if N is a positive integer that is a power of 2 (>= 2)."
  (and (integerp n) (>= n 2) (zerop (logand n (1- n)))))

(defun opt-pass-strength-reduce (instructions)
  "Forward pass: replace multiply/divide by powers of 2 with arithmetic shifts.
   - (* x 2^k) → (ash x k)
   - (* 2^k x) → (ash x k)   [commutative]
   - (/ x 2^k) → (ash x -k)  [floor semantics: (floor x 2^k) = (ash x -k)]
   - (mod x 2^k) → (logand x (2^k - 1))
   - (* x N)   → shift/add decomposition for small integer constants N
   At vm-label boundaries, flush the constant environment."
  (let* ((env     (make-hash-table :test #'eq))
         (base    (1+ (opt-max-reg-index instructions)))
         (counter base)
         (result  nil))
    (labels ((new-shift-reg ()
               (prog1 (intern (format nil "R~A" counter) :keyword)
                 (incf counter)))
             (const-val (reg) (gethash reg env))
             (emit (i) (push i result))
              (emit-seq (insts)
                (dolist (i insts)
                  (push i result)))
             (mul-by-const-seq (dst src n)
               "Emit a shift/add decomposition for SRC * N into DST."
               (let* ((negp (minusp n))
                      (absn (abs n)))
                 (cond
                   ((zerop absn)
                    (list (make-vm-const :dst dst :value 0)))
                   ((= absn 1)
                    (append (list (make-vm-move :dst dst :src src))
                            (when negp (list (make-vm-neg :dst dst :src dst)))))
                   (t
                    (let ((bits  (loop for bit from 0 below (integer-length absn)
                                       when (logbitp bit absn)
                                         collect bit))
                          (seq   nil)
                          (terms nil))
                      (dolist (bit bits)
                        (if (zerop bit)
                            (push src terms)
                            (let ((shift-count (new-shift-reg))
                                  (shifted (new-shift-reg)))
                              (push (make-vm-const :dst shift-count :value bit) seq)
                              (push (make-vm-ash :dst shifted :lhs src :rhs shift-count) seq)
                              (push shifted terms))))
                      (setf terms (nreverse terms))
                      (let ((first (car terms)))
                        (unless (eq first dst)
                          (push (make-vm-move :dst dst :src first) seq))
                        (dolist (term (cdr terms))
                          (push (make-vm-add :dst dst :lhs dst :rhs term) seq))
                        (when negp
                          (push (make-vm-neg :dst dst :src dst) seq))
                        (nreverse seq))))))))
      (dolist (inst instructions)
        (typecase inst
          (vm-label
           (clrhash env)
           (emit inst))
          (vm-const
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (emit inst))
          (vm-mul
           (let* ((dst (vm-dst inst))
                  (lhs (vm-lhs inst))
                  (rhs (vm-rhs inst))
                  (rv  (const-val rhs))
                  (lv  (const-val lhs)))
             (cond
               ((and rv (opt-power-of-2-p rv))
                (let* ((k         (1- (integer-length rv)))
                       (shift-reg (new-shift-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))
               ((and lv (opt-power-of-2-p lv))
                (let* ((k         (1- (integer-length lv)))
                       (shift-reg (new-shift-reg)))
                  (remhash dst env)
                  (emit (make-vm-const :dst shift-reg :value k))
                  (emit (make-vm-ash   :dst dst :lhs rhs :rhs shift-reg))))
                ((and rv (integerp rv) (not (zerop rv)) (<= (logcount (abs rv)) 2))
                 (remhash dst env)
                 (emit-seq (mul-by-const-seq dst lhs rv)))
                ((and lv (integerp lv) (not (zerop lv)) (<= (logcount (abs lv)) 2))
                 (remhash dst env)
                 (emit-seq (mul-by-const-seq dst rhs lv)))
               (t (emit inst)))))
           (vm-div
            (let* ((dst (vm-dst inst))
                   (lhs (vm-lhs inst))
                   (rhs (vm-rhs inst))
                   (rv  (const-val rhs)))
              (cond
                ((and rv (opt-power-of-2-p rv))
                 (let* ((k         (- (1- (integer-length rv))))
                        (shift-reg (new-shift-reg)))
                   (remhash dst env)
                   (emit (make-vm-const :dst shift-reg :value k))
                   (emit (make-vm-ash   :dst dst :lhs lhs :rhs shift-reg))))
                (t
                 (let ((dstreg (opt-inst-dst inst)))
                   (when dstreg (remhash dstreg env)))
                 (emit inst)))))
           (vm-mod
            (let* ((dst (vm-dst inst))
                   (lhs (vm-lhs inst))
                   (rhs (vm-rhs inst))
                   (rv  (const-val rhs)))
              (cond
                ((and rv (opt-power-of-2-p rv))
                 (let ((mask-reg (new-shift-reg)))
                   (remhash dst env)
                   (emit (make-vm-const :dst mask-reg :value (1- rv)))
                   (emit (make-vm-logand :dst dst :lhs lhs :rhs mask-reg))))
                (t
                 (let ((dstreg (opt-inst-dst inst)))
                   (when dstreg (remhash dstreg env)))
                 (emit inst)))))
           (t
            (let ((dst (opt-inst-dst inst)))
              (when dst (remhash dst env)))
            (emit inst)))))
    (nreverse result)))

(defun opt-bswap-recognition-match-at (instructions pos)
  (let ((end (+ pos 19)))
    (when (<= end (length instructions))
      (let* ((c0 (nth (+ pos 0) instructions))
             (a0 (nth (+ pos 1) instructions))
             (s0 (nth (+ pos 2) instructions))
             (b0 (nth (+ pos 3) instructions))
             (c1 (nth (+ pos 4) instructions))
             (a1 (nth (+ pos 5) instructions))
             (s1 (nth (+ pos 6) instructions))
             (b1 (nth (+ pos 7) instructions))
             (c2 (nth (+ pos 8) instructions))
             (a2 (nth (+ pos 9) instructions))
             (s2 (nth (+ pos 10) instructions))
             (b2 (nth (+ pos 11) instructions))
             (c3 (nth (+ pos 12) instructions))
             (a3 (nth (+ pos 13) instructions))
             (s3 (nth (+ pos 14) instructions))
             (b3 (nth (+ pos 15) instructions))
             (o0 (nth (+ pos 16) instructions))
             (o1 (nth (+ pos 17) instructions))
             (o2 (nth (+ pos 18) instructions)))
        (when (and (typep c0 'vm-const)
                   (eql (vm-value c0) #xFF)
                   (typep a0 'vm-logand)
                   (typep s0 'vm-const)
                   (typep b0 'vm-ash)
                   (typep c1 'vm-const)
                   (eql (vm-value c1) #xFF00)
                   (typep a1 'vm-logand)
                   (typep s1 'vm-const)
                   (typep b1 'vm-ash)
                   (typep c2 'vm-const)
                   (eql (vm-value c2) #xFF0000)
                   (typep a2 'vm-logand)
                   (typep s2 'vm-const)
                   (typep b2 'vm-ash)
                   (typep c3 'vm-const)
                   (eql (vm-value c3) #xFF000000)
                   (typep a3 'vm-logand)
                   (typep s3 'vm-const)
                   (typep b3 'vm-ash)
                   (typep o0 'vm-logior)
                   (typep o1 'vm-logior)
                   (typep o2 'vm-logior))
            (let* ((src (vm-lhs a0))
                   (dst (vm-dst o2)))
              (when (and (eq (vm-lhs a0) src)
                         (eq (vm-rhs a0) (vm-dst c0))
                         (eq (vm-lhs a1) src)
                         (eq (vm-rhs a1) (vm-dst c1))
                         (eq (vm-lhs a2) src)
                         (eq (vm-rhs a2) (vm-dst c2))
                         (eq (vm-lhs a3) src)
                         (eq (vm-rhs a3) (vm-dst c3))
                         (equal (mapcar #'vm-value (list s0 s1 s2 s3)) '(24 8 -8 -24))
                         (eq (vm-lhs b0) (vm-dst a0))
                         (eq (vm-rhs b0) (vm-dst s0))
                         (eq (vm-lhs b1) (vm-dst a1))
                         (eq (vm-rhs b1) (vm-dst s1))
                         (eq (vm-lhs b2) (vm-dst a2))
                         (eq (vm-rhs b2) (vm-dst s2))
                         (eq (vm-lhs b3) (vm-dst a3))
                         (eq (vm-rhs b3) (vm-dst s3))
                         (eq (vm-lhs o0) (vm-dst b0))
                         (eq (vm-rhs o0) (vm-dst b1))
                         (eq (vm-lhs o1) (vm-dst b2))
                         (eq (vm-rhs o1) (vm-dst b3))
                         (eq (vm-lhs o2) (vm-dst o0))
                         (eq (vm-rhs o2) (vm-dst o1)))
                (values (make-vm-bswap :dst dst :src src) 19))))))))

(defun opt-pass-bswap-recognition (instructions)
  "Collapse explicit byte-swap bit-manipulation trees into vm-bswap.

   Recognizes the canonical 32-bit reverse-bytes tree built from four masked
   extracts, four shifts, and three logior nodes, matching the bswap pattern
   documented for the backend."
  (let ((result nil)
        (i 0)
        (n (length instructions)))
    (do () ((>= i n) (nreverse result))
      (multiple-value-bind (rewritten consumed)
          (opt-bswap-recognition-match-at instructions i)
        (if rewritten
            (progn
              (push rewritten result)
              (incf i consumed))
              (progn
                (push (nth i instructions) result)
                 (incf i 1)))))))

(defun opt-rotate-recognition-match-at (instructions pos)
  (let ((end (+ pos 5)))
    (when (<= end (length instructions))
      (let* ((c0 (nth (+ pos 0) instructions))
             (a0 (nth (+ pos 1) instructions))
             (c1 (nth (+ pos 2) instructions))
             (a1 (nth (+ pos 3) instructions))
             (o0 (nth (+ pos 4) instructions)))
        (when (and (typep c0 'vm-const)
                   (typep a0 'vm-ash)
                   (typep c1 'vm-const)
                   (typep a1 'vm-ash)
                   (typep o0 'vm-logior))
          (let* ((k0 (vm-value c0))
                 (k1 (vm-value c1))
                 (src0 (vm-lhs a0))
                 (src1 (vm-lhs a1))
                 (count0 (vm-rhs a0))
                 (count1 (vm-rhs a1))
                 (dst0 (vm-dst a0))
                 (dst1 (vm-dst a1))
                 (out-dst (vm-dst o0)))
            (when (and (integerp k0)
                       (integerp k1)
                       (eq src0 src1)
                       (eq count0 (vm-dst c0))
                       (eq count1 (vm-dst c1))
                       (or (and (eq (vm-lhs o0) dst0) (eq (vm-rhs o0) dst1))
                           (and (eq (vm-lhs o0) dst1) (eq (vm-rhs o0) dst0))))
              (cond
                ((and (plusp k0) (= k1 (- k0 64)))
                 (values (list (make-vm-const :dst (vm-dst c1)
                                              :value (mod (- 64 k0) 64))
                               (make-vm-rotate :dst out-dst
                                               :lhs src0
                                               :rhs (vm-dst c1)))
                         5))
                ((and (plusp k1) (= k0 (- k1 64)))
                 (values (list (make-vm-const :dst (vm-dst c0)
                                              :value (mod (- 64 k1) 64))
                               (make-vm-rotate :dst out-dst
                                               :lhs src0
                                               :rhs (vm-dst c0)))
                         5))))))))))

(defun opt-pass-rotate-recognition (instructions)
  "Collapse rotate idioms into vm-rotate.

   Recognizes the classic two-shift + OR tree that implements a 64-bit rotate
   and replaces it with a single vm-rotate plus the normalized count constant."
  (let ((result nil)
        (i 0)
        (n (length instructions)))
    (do () ((>= i n) (nreverse result))
        (multiple-value-bind (rewritten consumed)
            (opt-rotate-recognition-match-at instructions i)
        (if rewritten
            (progn
              (dolist (inst (reverse rewritten))
                (push inst result))
              (incf i consumed))
            (progn
              (push (nth i instructions) result)
              (incf i 1)))))))

;;; ─── Pass: Arithmetic Reassociation ──────────────────────────────────────

(defun opt-reassociate-commutative-p (inst)
  (member (type-of inst)
          '(vm-add vm-integer-add vm-mul vm-integer-mul
            vm-logand vm-logior vm-logxor)
          :test #'eq))

(defun opt-copy-commutative-binop (inst dst lhs rhs)
  (typecase inst
    (vm-integer-add  (make-vm-integer-add :dst dst :lhs lhs :rhs rhs))
    (vm-add          (make-vm-add :dst dst :lhs lhs :rhs rhs))
    (vm-integer-mul  (make-vm-integer-mul :dst dst :lhs lhs :rhs rhs))
    (vm-mul          (make-vm-mul :dst dst :lhs lhs :rhs rhs))
    (vm-logand       (make-vm-logand :dst dst :lhs lhs :rhs rhs))
    (vm-logior       (make-vm-logior :dst dst :lhs lhs :rhs rhs))
    (vm-logxor       (make-vm-logxor :dst dst :lhs lhs :rhs rhs))
    (otherwise inst)))

(defun opt-pass-reassociate (instructions)
  "Reassociate commutative associative ops to move constants inward.

   Canonicalizes adjacent chains of +, *, logand, logior, and logxor so that
   compile-time constants drift toward the tail of the tree, exposing more
   folding opportunities without changing semantics for exact integer ops."
  (let ((use-counts (make-hash-table :test #'eq))
        (env        (make-hash-table :test #'eq))
        (result     nil))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (incf (gethash reg use-counts 0))))
    (labels ((const-known-p (reg)
             (multiple-value-bind (val found) (gethash reg env)
               (declare (ignore val))
               found))
           (sorted-triplet (a b c)
             (append (loop for term in (list a b c)
                           unless (const-known-p term)
                           collect term)
                     (loop for term in (list a b c)
                           when (const-known-p term)
                           collect term)))
           (maybe-reassociate (prev cur)
             (let* ((dst (vm-dst prev))
                    (lhs (vm-lhs prev))
                    (rhs (vm-rhs prev))
                    (uses-prev-p (or (eq (vm-lhs cur) dst)
                                     (eq (vm-rhs cur) dst)))
                    (other (if (eq (vm-lhs cur) dst)
                               (vm-rhs cur)
                               (vm-lhs cur))))
               (when (and uses-prev-p
                          (eq (type-of prev) (type-of cur))
                          (opt-reassociate-commutative-p prev)
                          (= (gethash dst use-counts 0) 1))
                 (let* ((sorted (sorted-triplet lhs rhs other))
                        (orig   (list lhs rhs other)))
                   (when (and (not (equal sorted orig))
                              (or (const-known-p lhs)
                                  (const-known-p rhs)
                                  (const-known-p other)))
                     (values (opt-copy-commutative-binop prev dst
                                                         (second sorted)
                                                         (third sorted))
                             (opt-copy-commutative-binop cur (vm-dst cur)
                                                         (first sorted)
                                                         dst))))))))
      (dolist (inst instructions)
        (cond
          ((typep inst 'vm-label)
           (clrhash env)
           (push inst result))
          ((typep inst 'vm-const)
           (setf (gethash (vm-dst inst) env) (vm-value inst))
           (push inst result))
          ((and result
                (opt-reassociate-commutative-p inst)
                (opt-reassociate-commutative-p (car result)))
            (multiple-value-bind (new-prev new-cur)
                (maybe-reassociate (car result) inst)
              (if new-prev
                  (progn
                    (setf (car result) new-prev)
                   (push new-cur result)
                   (remhash (vm-dst new-prev) env)
                   (remhash (vm-dst new-cur) env))
                 (progn
                    (when (opt-inst-dst inst)
                      (remhash (opt-inst-dst inst) env))
                    (push inst result)))))
          (t
           (when (opt-inst-dst inst)
             (remhash (opt-inst-dst inst) env))
           (push inst result)))))
    (nreverse result)))

;;; ─── Pass: Batch Concatenation Packing ───────────────────────────────────

(defun %concat-parts (inst)
  "Return the register list represented by a vm-concatenate instruction."
  (or (vm-parts inst)
      (list (vm-str1 inst) (vm-str2 inst))))

(defun %make-packed-concatenate (dst parts)
  (make-vm-concatenate :dst dst
                       :str1 (first parts)
                       :str2 (car (last parts))
                       :parts parts))

(defun opt-pass-batch-concatenate (instructions)
  "Pack linear vm-concatenate chains into one instruction with a PARTS list."
  (let ((use-counts (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (incf (gethash reg use-counts 0))))
    (labels ((concat-chain-p (current next)
               (and (typep current 'vm-concatenate)
                    (typep next 'vm-concatenate)
                    (= (gethash (vm-dst current) use-counts 0) 1)
                    (eq (vm-str1 next) (vm-dst current))))
             (pack-chain (rest)
               (when rest
                 (let ((inst (car rest)))
                   (if (typep inst 'vm-concatenate)
                       (let ((parts (%concat-parts inst))
                             (dst (vm-dst inst))
                             (tail (cdr rest))
                             (current inst)
                             (merged nil))
                         (loop while (and tail (concat-chain-p current (car tail))) do
                          (let ((next (car tail)))
                            (setf merged t
                                    parts (append parts (rest (%concat-parts next)))
                                    dst (vm-dst next)
                                    current next
                                    tail (cdr tail))))
                         (cons (if merged
                                   (%make-packed-concatenate dst parts)
                                   inst)
                               (pack-chain tail)))
                       (cons inst (pack-chain (cdr rest))))))))
      (pack-chain instructions))))

;;; ─── Pass: Common Subexpression Elimination ──────────────────────────────

(defun opt-pass-cse (instructions)
  "Common subexpression elimination via generation-numbered value numbering.
   Replaces redundant computations with vm-move from the first computing register.
   At vm-label boundaries, all knowledge is conservatively flushed."
  (let ((gen      (make-hash-table :test #'eq))    ; reg → generation count
        (val-env  (make-hash-table :test #'eq))    ; reg → canonical-value
        (memo     (make-hash-table :test #'equal)) ; canonical-value → canonical-reg
        (target-labels (%opt-branch-target-labels instructions))
        (result   nil))
    (labels ((get-val (reg)
               ;; Canonical value: from val-env if known, else (reg . gen)
               (or (gethash reg val-env)
                   (cons reg (or (gethash reg gen) 0))))
             (bump-gen (dst)
               ;; Overwriting dst: remove stale memo entry if dst was canonical
               (let ((old-val (gethash dst val-env)))
                 (when old-val
                   (when (eq (gethash old-val memo) dst)
                     (remhash old-val memo))
                   (remhash dst val-env)))
               (setf (gethash dst gen) (1+ (or (gethash dst gen) 0))))
             (record (dst key)
               (setf (gethash dst val-env) key)
               (unless (gethash key memo)
                 (setf (gethash key memo) dst)))
              (commutative-p (inst)
                (member (type-of inst) *opt-commutative-inst-types* :test #'eq))
             (try-cse (key)
               ;; Return existing reg if key is memoized, else nil
               (gethash key memo))
             (emit-or-cse (inst dst key)
               (let ((existing (try-cse key)))
                 (if existing
                     (progn
                       (bump-gen dst)
                       (setf (gethash dst val-env) key)
                       (push (make-vm-move :dst dst :src existing) result))
                     (progn
                       (bump-gen dst)
                       (record dst key)
                       (push inst result))))))
      (dolist (inst instructions)
        (cond
          ((typep inst 'vm-label)
            (when (gethash (vm-name inst) target-labels)
              (clrhash gen) (clrhash val-env) (clrhash memo))
            (push inst result))
          ((typep inst 'vm-const)
           ;; Never replace vm-const with vm-move: doing so creates a
           ;; fold<->CSE oscillation that DCE can turn into a dangling
           ;; register reference (first canonical reg gets removed by
           ;; DCE while later moves still point to it, leaving the
           ;; register uninitialized = 0 instead of NIL).
           ;; The fold pass already propagates constant values; CSE is
           ;; only needed for computed expressions.
           (let ((dst (vm-dst inst))
                 (key (list :const (vm-value inst))))
             (bump-gen dst)
             (record dst key)
             (push inst result)))
          ((typep inst 'vm-move)
           (let* ((dst     (vm-move-dst inst))
                  (src-val (get-val (vm-move-src inst))))
             (bump-gen dst)
             (setf (gethash dst val-env) src-val)
             (push inst result)))
          ;; CSE for binary ops: derived from opt-binary-lhs-rhs-p
          ((opt-binary-lhs-rhs-p inst)
           (let* ((dst (vm-dst inst))
                  (lv  (get-val (vm-lhs inst)))
                  (rv  (get-val (vm-rhs inst)))
                  (op  (type-of inst))
                   (key (if (commutative-p inst)
                           (list op (if (%opt-value< lv rv) lv rv)
                                    (if (%opt-value< lv rv) rv lv))
                            (list op lv rv))))
             (emit-or-cse inst dst key)))
          ;; CSE for unary ops + type predicates: derived from opt-unary-src-p
          ((opt-unary-src-p inst)
           (let* ((dst (vm-dst inst))
                  (sv  (get-val (vm-src inst)))
                  (key (list (type-of inst) sv)))
             (emit-or-cse inst dst key)))
          (t
           (let ((dst (opt-inst-dst inst)))
             (when dst (bump-gen dst)))
            (push inst result)))))
    (nreverse result)))

;;; ─── Pass: Global Value Numbering ────────────────────────────────────────

(defun %gvn-process-block (block gen val-env memo)
  "Process BLOCK for global value numbering using GEN/VAL-ENV/MEMO state."
  (labels ((copy-env (env &key (test #'eq))
            (let ((copy (make-hash-table :test test)))
             (maphash (lambda (k v) (setf (gethash k copy) v)) env)
             copy))
         (kill (reg gen val-env memo)
           (remhash reg val-env)
           (maphash (lambda (k v)
                      (when (eq v reg)
                        (remhash k memo)))
                    memo)
           (setf (gethash reg gen) (1+ (or (gethash reg gen) 0))))
          (commutative-p (inst)
            (member (type-of inst) *opt-commutative-inst-types* :test #'eq))
         (get-val (reg gen val-env)
           (or (gethash reg val-env)
               (cons reg (or (gethash reg gen) 0))))
         (record (dst key gen val-env memo)
           (setf (gethash dst val-env) key)
           (unless (gethash key memo)
             (setf (gethash key memo) dst))
           (setf (gethash dst gen) (1+ (or (gethash dst gen) 0))))
         (gvn-key (inst gen val-env)
           (let ((reads (opt-inst-read-regs inst)))
             (cond
               ((typep inst 'vm-const)
                (list :const (vm-value inst)))
               ((typep inst 'vm-move)
                (get-val (vm-src inst) gen val-env))
               ((and (opt-inst-pure-p inst) reads)
                (let ((vals (mapcar (lambda (reg) (get-val reg gen val-env)) reads)))
                   (if (commutative-p inst)
                       (destructuring-bind (a b) vals
                        (if (%opt-value< a b)
                            (list (type-of inst) a b)
                            (list (type-of inst) b a)))
                       (cons (type-of inst) vals))))
                (t nil)))))
    (let ((local-gen (copy-env gen))
          (local-val-env (copy-env val-env))
          (local-memo (copy-env memo :test #'equal))
          (new-insts nil))
      (dolist (inst (bb-instructions block))
        (let ((dst (opt-inst-dst inst)))
          (when dst
            (kill dst local-gen local-val-env local-memo)))
        (cond
          ((typep inst 'vm-const)
           (let* ((dst (vm-dst inst))
                  (key (gvn-key inst local-gen local-val-env)))
             (record dst key local-gen local-val-env local-memo)
             (push inst new-insts)))
          ((typep inst 'vm-move)
           (let* ((dst (vm-dst inst))
                  (key (gvn-key inst local-gen local-val-env))
                  (existing (gethash key local-memo)))
             (if existing
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push (make-vm-move :dst dst :src existing) new-insts))
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push inst new-insts)))))
          ((and (opt-inst-pure-p inst) (opt-inst-dst inst))
           (let* ((dst (opt-inst-dst inst))
                  (key (gvn-key inst local-gen local-val-env))
                  (existing (and key (gethash key local-memo))))
             (if existing
                 (progn
                   (record dst key local-gen local-val-env local-memo)
                   (push (make-vm-move :dst dst :src existing) new-insts))
                 (progn
                   (when key
                     (record dst key local-gen local-val-env local-memo))
                   (push inst new-insts)))))
          (t
           (push inst new-insts))))
      (setf (bb-instructions block) (nreverse new-insts))
      (dolist (child (bb-dom-children block))
        (%gvn-process-block child local-gen local-val-env local-memo)))))

(defun opt-pass-gvn (instructions)
  "Conservative global value numbering over the dominator tree.

   Unlike opt-pass-cse, knowledge is propagated into dominated basic blocks
   instead of being flushed at every label. This lets a computation in a
   dominator block replace an equivalent computation in a dominated block,
   while still avoiding cross-join speculation."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (%gvn-process-block (cfg-entry cfg)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq))
      (cfg-flatten-hot-cold cfg))))

;;; ─── Unused Label Elimination ────────────────────────────────────────────

(defun opt-pass-dead-labels (instructions)
  "Remove labels that are not referenced by any jump or closure instruction."
  (let ((used (make-hash-table :test #'equal)))
    ;; Collect all label references: jumps AND closure body labels
    (dolist (inst instructions)
      (let ((accessor (gethash (type-of inst) *opt-label-ref-table*)))
        (when accessor
          (setf (gethash (funcall accessor inst) used) t))))
    ;; Filter out unreferenced labels
      (remove-if (lambda (inst)
                   (and (vm-label-p inst)
                        (not (gethash (vm-name inst) used))))
                instructions)))

;;; ─── Leaf Function Detection ────────────────────────────────────────────

(defparameter *opt-leaf-call-types*
  '(vm-call vm-generic-call vm-tail-call vm-apply vm-call-next-method)
  "Instruction types that disqualify a function from being a leaf function.")

(defun opt-pass-leaf-detect (instructions)
  "Return INSTRUCTIONS unchanged and a leaf-function flag.

   A program is a leaf function when it contains no call-like instructions."
  (let ((leaf-p (not (some (lambda (inst)
                             (member (type-of inst) *opt-leaf-call-types* :test #'eq))
                           instructions))))
    (values instructions leaf-p)))

;;; ─── Pass: Loop Invariant Code Motion (LICM) ──────────────────────────────

(defun opt-pass-licm (instructions)
  "Loop Invariant Code Motion: hoist loop-invariant instructions to preheaders.

   Algorithm:
     1. Build CFG and compute loop depths via cfg-compute-loop-depths
     2. Identify loop headers (blocks with back-edge predecessors)
       3. For each loop, collect invariant instructions:
         - Pure (opt-inst-pure-p)
         - All operand registers defined outside the loop
     4. Create preheader block before each loop header
     5. Move invariant instructions to preheader

   This pass activates the dormant bb-loop-depth field computed by cfg.lisp.
   It is FR-003 from docs/optimize-passes.md."
  (when (null instructions)
    (return-from opt-pass-licm instructions))

  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)

       (let ((loop-blocks (remove-if (lambda (b) (= (bb-loop-depth b) 0))
                                  (coerce (cfg-blocks cfg) 'list))))
      (when (null loop-blocks)
        (return-from opt-pass-licm instructions))

      (let ((def-sites (make-hash-table :test #'eq))
            (header-to-members (make-hash-table :test #'eq))
            (header-to-invariants (make-hash-table :test #'eq))
            (header-to-tails (make-hash-table :test #'eq))
            (loop-headers nil))
        (loop for b across (cfg-blocks cfg)
              do (dolist (inst (bb-instructions b))
                   (let ((dst (opt-inst-dst inst)))
                     (when dst
                       (pushnew b (gethash dst def-sites) :test #'eq)))))

        (loop for b across (cfg-blocks cfg)
              do (dolist (succ (bb-successors b))
                   (when (cfg-dominates-p succ b)
                     (pushnew succ loop-headers :test #'eq)
                     (pushnew b (gethash succ header-to-tails) :test #'eq))))

        (dolist (header loop-headers)
          (let ((members (make-hash-table :test #'eq)))
            (dolist (tail (gethash header header-to-tails))
              (dolist (member (cfg-collect-natural-loop header tail))
                (setf (gethash member members) t)))
            (setf (gethash header header-to-members) members)

            (let ((loop-def-regs (make-hash-table :test #'eq)))
              (maphash (lambda (member _)
                         (dolist (inst (bb-instructions member))
                           (let ((dst (opt-inst-dst inst)))
                             (when dst
                               (setf (gethash dst loop-def-regs) t)))))
                       members)

                (maphash (lambda (member _)
                           (dolist (inst (bb-instructions member))
                             (when (opt-inst-loop-invariant-p inst loop-def-regs members def-sites)
                               (pushnew inst (gethash header header-to-invariants) :test #'eq))))
                        members))))

        (when (every (lambda (header)
                       (null (gethash header header-to-invariants)))
                     loop-headers)
          (return-from opt-pass-licm instructions))

        (opt-licm-emit-with-preheaders cfg header-to-members header-to-invariants)))))

(defun opt-inst-loop-invariant-p (inst loop-def-regs loop-members def-sites)
  "Return T if INST is loop-invariant:
     1. Pure (no observable side effects)
     2. All read registers are defined outside the loop"
  ;; Must be pure to be safely hoisted
  (unless (opt-inst-pure-p inst)
    (return-from opt-inst-loop-invariant-p nil))

  ;; Check all read registers
  (let ((reads (opt-inst-read-regs inst)))
    (dolist (reg reads t)
      ;; If reg is defined inside the loop, not invariant
      (when (gethash reg loop-def-regs)
        (return-from opt-inst-loop-invariant-p nil))
      ;; If all known definitions of REG are inside the loop, it is not invariant.
      (let ((sites (gethash reg def-sites)))
        (when (and sites
                   (every (lambda (block)
                            (gethash block loop-members))
                          sites))
          (return-from opt-inst-loop-invariant-p nil))))))

(defun opt-licm-emit-with-preheaders (cfg header-to-members header-to-invariants)
  "Mutate CFG to insert preheaders, then flatten it back to instructions."
  (let ((preheader-counter 0))
    (labels ((header-member-p (header block)
               (gethash block (gethash header header-to-members)))
             (redirect-successor (block old new)
               (setf (bb-successors block)
                     (mapcar (lambda (succ) (if (eq succ old) new succ))
                             (bb-successors block)))
               (setf (bb-predecessors old)
                     (remove block (bb-predecessors old) :test #'eq))
               (pushnew block (bb-predecessors new) :test #'eq))
             (rewrite-terminator (block old-label new-label)
               (let ((cell (last (bb-instructions block))))
                 (when cell
                   (let ((term (car cell)))
                     (typecase term
                       (vm-jump
                        (when (equal (vm-label-name term) old-label)
                          (setf (car cell) (make-vm-jump :label new-label))))
                       (vm-jump-zero
                        (when (equal (vm-label-name term) old-label)
                          (setf (car cell)
                                (make-vm-jump-zero :reg (vm-reg term)
                                                   :label new-label)))))))))
             (hoist-header (header)
               (let ((invariants (gethash header header-to-invariants)))
                 (when (and invariants (bb-label header))
                   (let* ((header-label (vm-name (bb-label header)))
                          (preheader-label-name (format nil "LICM_PREHEADER_~A" preheader-counter))
                          (preheader-label (make-vm-label :name preheader-label-name))
                          (preheader (cfg-new-block cfg :label preheader-label))
                          (outside-preds (remove-if (lambda (pred)
                                                      (header-member-p header pred))
                                                    (copy-list (bb-predecessors header)))))
                     (incf preheader-counter)
                     (setf (bb-instructions preheader)
                           (append (copy-list invariants)
                                   (list (make-vm-jump :label header-label))))
                     (setf (bb-successors preheader) (list header))
                     (pushnew preheader (bb-predecessors header) :test #'eq)
                     (dolist (pred outside-preds)
                       (redirect-successor pred header preheader)
                       (rewrite-terminator pred header-label preheader-label-name))
                     (dolist (member (loop for k being the hash-keys of (gethash header header-to-members)
                                           collect k))
                       (setf (bb-instructions member)
                             (remove-if (lambda (inst)
                                          (member inst invariants :test #'eq))
                                        (bb-instructions member)))))))))
      (maphash (lambda (header _)
                 (hoist-header header))
               header-to-invariants)
      (cfg-flatten cfg))))

(defun %opt-pre-expression-key (inst)
  "Return a structural key for a pure instruction INST, or NIL."
  (when (opt-inst-pure-p inst)
    (cond
      ((typep inst 'vm-const)
       (list :const (vm-value inst)))
      (t
       (let* ((reads (opt-inst-read-regs inst))
              (normalized (if (member (type-of inst) *opt-commutative-inst-types* :test #'eq)
                              (sort (copy-list reads)
                                    #'string<
                                    :key #'prin1-to-string)
                              reads)))
         (and normalized (cons (type-of inst) normalized)))))))

(defun %opt-pre-block-out-env (block)
  "Return key→dst availability after BLOCK for PRE join-point analysis."
  (let ((env (make-hash-table :test #'equal)))
    (dolist (inst (bb-instructions block) env)
      (let ((dst (opt-inst-dst inst)))
        (when dst
          (let ((stale nil))
            (maphash (lambda (k v)
                       (when (eq v dst)
                         (push k stale)))
                     env)
            (dolist (k stale)
              (remhash k env)))))
      (let ((key (%opt-pre-expression-key inst)))
        (when key
          (setf (gethash key env) (opt-inst-dst inst)))))))

(defun %opt-pre-splice-before-terminator (insts additions)
  "Insert ADDITIONS before INSTS' terminator, preserving order."
  (let ((term (car (last insts))))
    (if (and term (typep term '(or vm-jump vm-jump-zero vm-ret vm-halt)))
        (append (butlast insts) additions (list term))
        (append insts additions))))

(defun %opt-pre-join-elim (cfg)
  "Do a small join-point PRE pass over CFG blocks."
  (let ((pred-inserts (make-hash-table :test #'eq))
        (changed nil))
    (labels ((insert-for-pred (pred inst)
               (push inst (gethash pred pred-inserts))))
      (loop for block across (cfg-blocks cfg)
            do (let ((preds (bb-predecessors block)))
                 (when (> (length preds) 1)
                   (let ((pred-envs (mapcar (lambda (pred)
                                              (cons pred (%opt-pre-block-out-env pred)))
                                            preds))
                         (new-insts nil))
                     (dolist (inst (bb-instructions block))
                       (let* ((dst (opt-inst-dst inst))
                              (key (%opt-pre-expression-key inst)))
                         (when dst
                           (dolist (pair pred-envs)
                             (let ((env (cdr pair)))
                               (maphash (lambda (k v)
                                          (when (eq v dst)
                                            (remhash k env)))
                                        env))))
                         (if (and dst key)
                             (let ((available nil))
                               (dolist (pair pred-envs)
                                 (when (gethash key (cdr pair))
                                   (setf available t)))
                               (if available
                                   (progn
                                     (setf changed t)
                                     (dolist (pair pred-envs)
                                       (let* ((pred (car pair))
                                              (env (cdr pair))
                                              (src (gethash key env)))
                                         (if src
                                             (unless (eq src dst)
                                               (insert-for-pred pred
                                                                (make-vm-move :dst dst :src src)))
                                             (insert-for-pred pred
                                                              (handler-case
                                                                  (sexp->instruction (instruction->sexp inst))
                                                                (error () inst))))
                                         (setf (gethash key env) dst)))
                                     nil)
                                   (push inst new-insts)))
                             (push inst new-insts))))
                     (setf (bb-instructions block) (nreverse new-insts))))))
      (maphash (lambda (pred insts)
                 (setf (bb-instructions pred)
                       (%opt-pre-splice-before-terminator (bb-instructions pred)
                                                          (nreverse insts))))
               pred-inserts)
      changed)))

(defun opt-pass-constant-hoist (instructions)
  "Hoist loop-invariant constants into loop preheaders.

   This is a narrow constant-only specialization of LICM that reuses the same
   CFG/loop analysis and therefore stays within the existing optimizer model.
   It exists to make FR-166 a named pass while preserving the same lowering
   behavior for constant literals already covered by LICM."
  (opt-pass-licm instructions))

(defun opt-pass-pre (instructions)
  "Partial Redundancy Elimination (PRE).

   This combines LICM with a small join-point PRE step and CSE so partially
   redundant pure computations are exposed and removed."
  (let* ((licm1 (opt-pass-licm instructions))
         (cse1 (opt-pass-cse licm1))
         (cfg (cfg-build cse1)))
    (%opt-pre-join-elim cfg)
    (opt-pass-cse (opt-pass-licm (cfg-flatten cfg)))))

(defun opt-pass-egraph (instructions)
  "Run equality-saturation lowering over the current instruction list.

   This is a thin wrapper around optimize-with-egraph so the pass can live in
   the main optimizer pipeline without changing the standalone e-graph API."
  (optimize-with-egraph instructions))

;;; ─── Top-Level Optimizer ─────────────────────────────────────────────────

(defun opt-pass-inline-iterative (instructions)
  "Thresholded inline pass used inside the convergence loop."
  (opt-pass-inline instructions :threshold 15))

(defparameter *opt-convergence-passes*
  (list #'opt-pass-inline-iterative
        #'opt-pass-fold
        #'opt-pass-sccp
        #'opt-pass-strength-reduce
        #'opt-pass-bswap-recognition
        #'opt-pass-rotate-recognition
        #'opt-pass-reassociate
        #'opt-pass-copy-prop
        #'opt-pass-gvn
        #'opt-pass-batch-concatenate
        #'opt-pass-cse
        #'opt-pass-jump
        #'opt-pass-unreachable
        #'opt-pass-dead-basic-blocks
        #'opt-pass-store-to-load-forward
        #'opt-pass-dead-store-elim
        #'opt-pass-nil-check-elim
        #'opt-pass-dominated-type-check-elim
        #'opt-pass-branch-correlation
        #'opt-pass-block-merge
        #'opt-pass-tail-merge
        #'opt-pass-pre
        #'opt-pass-egraph
        #'opt-pass-constant-hoist
        #'opt-pass-global-dce
        #'opt-pass-dead-labels
        #'opt-pass-dce)
  "Ordered list of passes run to convergence in optimize-instructions.
    Each pass is a function (instructions) -> instructions.
    Add new passes here; the convergence loop requires no other changes.")

(defparameter *opt-pass-registry*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry `((:inline . ,#'opt-pass-inline-iterative)
                     (:fold . ,#'opt-pass-fold)
                     (:sccp . ,#'opt-pass-sccp)
                     (:strength-reduce . ,#'opt-pass-strength-reduce)
                     (:bswap-recognition . ,#'opt-pass-bswap-recognition)
                     (:rotate-recognition . ,#'opt-pass-rotate-recognition)
                     (:reassociate . ,#'opt-pass-reassociate)
                     (:copy-prop . ,#'opt-pass-copy-prop)
                     (:gvn . ,#'opt-pass-gvn)
                     (:batch-concatenate . ,#'opt-pass-batch-concatenate)
                     (:cse . ,#'opt-pass-cse)
                     (:jump . ,#'opt-pass-jump)
                     (:unreachable . ,#'opt-pass-unreachable)
                     (:dead-basic-blocks . ,#'opt-pass-dead-basic-blocks)
                     (:store-to-load-forward . ,#'opt-pass-store-to-load-forward)
                     (:dead-store-elim . ,#'opt-pass-dead-store-elim)
                     (:nil-check-elim . ,#'opt-pass-nil-check-elim)
                     (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
                     (:block-merge . ,#'opt-pass-block-merge)
                     (:tail-merge . ,#'opt-pass-tail-merge)
                     (:pre . ,#'opt-pass-pre)
                     (:egraph . ,#'opt-pass-egraph)
                     (:constant-hoist . ,#'opt-pass-constant-hoist)
                     (:dead-labels . ,#'opt-pass-dead-labels)
                     (:dce . ,#'opt-pass-dce)))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Keyword pass name -> optimizer function mapping for configurable pipelines.")

(defun opt-parse-pass-pipeline-string (text)
  "Parse a comma-separated optimizer pipeline string into keyword pass names."
  (labels ((trim (s)
             (string-trim '(#\Space #\Tab #\Newline #\Return) s)))
    (remove nil
            (mapcar (lambda (part)
                      (let ((name (trim part)))
                        (and (> (length name) 0)
                             (intern (string-upcase name) :keyword))))
                    (uiop:split-string text :separator '(#\,))))))

(defun opt-resolve-pass-pipeline (pipeline)
  "Resolve PIPELINE into a list of pass functions.
PIPELINE may be NIL (use *opt-convergence-passes*), a list of functions,
or a list of keyword pass names present in *opt-pass-registry*."
  (cond
    ((null pipeline) *opt-convergence-passes*)
    ((stringp pipeline) (opt-resolve-pass-pipeline (opt-parse-pass-pipeline-string pipeline)))
    ((every #'functionp pipeline) pipeline)
    (t
     (mapcar (lambda (entry)
               (or (and (keywordp entry) (gethash entry *opt-pass-registry*))
                   (error "Unknown optimizer pass ~S" entry)))
             pipeline))))

(defun opt-run-passes-once-with-timings (prog passes stream)
  "Apply PASSES once, writing timing lines to STREAM."
  (reduce (lambda (p f)
            (let ((start (get-internal-real-time)))
              (prog1 (funcall f p)
                (let ((elapsed (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second)))
                  (format stream "~A: ~,6Fs~%" f elapsed)))))
          passes
          :initial-value prog))

(defun opt-run-passes-once-with-remarks (prog passes stream &key (mode :all))
  "Apply PASSES once, writing simple optimization remarks to STREAM.
MODE is one of :all, :changed, or :missed."
  (reduce (lambda (p f)
            (let* ((next (funcall f p))
                   (changed (not (opt-converged-p p next))))
              (when (or (eq mode :all)
                        (and changed (eq mode :changed))
                        (and (not changed) (eq mode :missed)))
                (format stream "~A: ~A~%"
                        f
                        (if changed "changed" "missed")))
              next))
          passes
          :initial-value prog))

(defun opt-run-passes-once (prog)
  "Apply every convergence pass in *opt-convergence-passes* once, left to right."
  (reduce (lambda (p f) (funcall f p)) *opt-convergence-passes* :initial-value prog))

(defun opt-converged-p (prev next)
  "T if a pass-cycle produced no change (same length and all instructions eq)."
  (and (= (length prev) (length next))
       (every #'eq prev next)))

(defun optimize-instructions (instructions &key (max-iterations 20) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all))
  "Run the full multi-pass optimization pipeline on a VM instruction sequence.
   Runs until no changes or MAX-ITERATIONS reached."
  (let ((prog instructions)
        (passes (opt-resolve-pass-pipeline pass-pipeline))
        (timing-stream (or timing-stream *standard-output*))
        (opt-remarks-stream (or opt-remarks-stream *standard-output*)))
    (loop for iteration from 0 below max-iterations
          for prev = prog
          do (setf prog (cond
                          (print-pass-timings
                           (opt-run-passes-once-with-timings prog passes timing-stream))
                          (print-opt-remarks
                           (opt-run-passes-once-with-remarks prog passes opt-remarks-stream :mode opt-remarks-mode))
                          (t
                           (reduce (lambda (p f) (funcall f p)) passes :initial-value prog))))
          when (opt-converged-p prev prog)
          return prog)
    (when *enable-prolog-peephole*
      (setf prog (mapcar #'sexp->instruction
                         (apply-prolog-peephole (mapcar #'instruction->sexp prog)))))
    (opt-pass-leaf-detect prog)))
