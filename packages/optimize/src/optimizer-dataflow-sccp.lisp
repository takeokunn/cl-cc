;;;; optimizer-dataflow-sccp.lisp — Sparse Conditional Constant Propagation (SCCP) pass
(in-package :cl-cc/optimize)

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
  (let ((tp (type-of inst)))
    (typecase inst
      (vm-const inst)
      (vm-label inst)
      (vm-jump-zero
       (multiple-value-bind (val found) (gethash (vm-reg inst) env)
         (cond
           ((and found (opt-falsep val)) (make-vm-jump :label (vm-label-name inst)))
           (found nil)
           (t inst))))
      (vm-move
       (multiple-value-bind (val found) (gethash (vm-src inst) env)
         (if found
             (make-vm-const :dst (vm-dst inst) :value val)
             inst)))
      (vm-concatenate
       (let ((parts (or (vm-parts inst) (list (vm-str1 inst) (vm-str2 inst)))))
         (if (every (lambda (reg)
                      (multiple-value-bind (val found) (gethash reg env)
                        (and found (stringp val))))
                    parts)
             (make-vm-const :dst (vm-dst inst)
                            :value (apply #'concatenate 'string
                                          (mapcar (lambda (reg) (gethash reg env)) parts)))
             inst)))
      (t
       (cond
         ;; Binary arithmetic/comparison — data-driven
         ((or (gethash tp *opt-binary-fold-table*)
              (gethash tp *opt-binary-cmp-fold-table*))
          (multiple-value-bind (lval lfound) (gethash (vm-lhs inst) env)
            (multiple-value-bind (rval rfound) (gethash (vm-rhs inst) env)
              (if (and lfound rfound (numberp lval) (numberp rval))
                  (multiple-value-bind (folded ok) (opt-fold-binop-value inst lval rval)
                    (if ok (make-vm-const :dst (vm-dst inst) :value folded) inst))
                  inst))))
         ;; Unary arithmetic — data-driven
         ((gethash tp *opt-unary-fold-table*)
           (multiple-value-bind (sval found) (gethash (vm-src inst) env)
             (if (and found (%fold-unary-constant-eligible-p inst sval))
                 (make-vm-const :dst (vm-dst inst)
                                :value (funcall (gethash tp *opt-unary-fold-table*) sval))
                 inst)))
         ;; Type predicates — data-driven
         ((gethash tp *opt-type-pred-fold-table*)
          (multiple-value-bind (sval found) (gethash (vm-src inst) env)
            (if found
                (make-vm-const :dst (vm-dst inst)
                               :value (if (funcall (gethash tp *opt-type-pred-fold-table*) sval) 1 0))
                inst)))
         (t inst))))))

(defun %sccp-redirect-successors (block new-succs)
  "Update BLOCK's CFG edges to use NEW-SUCCS as its successors."
  (dolist (old (bb-successors block))
    (setf (bb-predecessors old)
          (remove block (bb-predecessors old) :test #'eq)))
  (setf (bb-successors block) new-succs)
  (dolist (succ new-succs)
    (pushnew block (bb-predecessors succ) :test #'eq)))

(defun %sccp-update-env-for-inst (inst env)
  "Update ENV by binding or killing the destination of INST after folding."
  (let ((dst (opt-inst-dst inst)))
    (when dst
      (typecase inst
        (vm-const (setf (gethash dst env) (vm-value inst)))
        (t (remhash dst env))))))

(defun %sccp-process-block (block in-env)
  "Fold BLOCK's instructions under IN-ENV; return the resulting out-env."
  (let ((env       (%sccp-env-copy in-env))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let ((folded (%sccp-fold-inst inst env)))
        (cond
          ((and (typep inst 'vm-jump-zero) (null folded))
           (%sccp-redirect-successors block
                                      (let ((succs (bb-successors block)))
                                        (if (second succs) (list (second succs)) nil))))
          ((and (typep inst 'vm-jump-zero) (typep folded 'vm-jump))
           (%sccp-redirect-successors block (list (first (bb-successors block))))
           (push folded new-insts)
           (%sccp-update-env-for-inst folded env))
          ((null folded) nil)
          (t
           (push folded new-insts)
           (%sccp-update-env-for-inst folded env)))))
    (setf (bb-instructions block) (nreverse new-insts))
    env))

(defun opt-pass-sccp (instructions)
  "Sparse conditional constant propagation over the CFG.
   Propagates constants across blocks and folds constant branches."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (let ((in-envs  (make-hash-table :test #'eq))
            (out-envs (make-hash-table :test #'eq))
            (worklist (list (cfg-entry cfg))))
        (setf (gethash (cfg-entry cfg) in-envs) (make-hash-table :test #'eq))
        (loop while worklist do
          (let* ((block   (pop worklist))
                 (new-in  (%sccp-env-merge
                           (loop for p in (bb-predecessors block)
                                 for e = (gethash p out-envs)
                                 when e collect e)))
                 (old-in  (gethash block in-envs)))
            (when (or (null old-in)
                      (null (gethash block out-envs))
                      (not (%sccp-env-equal-p old-in new-in)))
              (setf (gethash block in-envs) new-in)
              (let ((new-out (%sccp-process-block block new-in))
                    (old-out (gethash block out-envs)))
                (unless (and old-out (%sccp-env-equal-p old-out new-out))
                  (setf (gethash block out-envs) new-out)
                  (dolist (succ (bb-successors block))
                    (pushnew succ worklist :test #'eq)))))))))
    (let ((linear (loop for b across (cfg-blocks cfg)
                        when b append (append (when (bb-label b) (list (bb-label b)))
                                              (copy-list (bb-instructions b))))))
      (cfg-flatten (cfg-build linear)))))

;;; (opt-map-tree, %opt-copy-prop-* helpers, and opt-pass-copy-prop
;;;  are in optimizer-copyprop.lisp which loads after this file.)
