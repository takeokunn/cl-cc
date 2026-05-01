(in-package :cl-cc/optimize)
;;; ─── Pass: Partial Redundancy Elimination (PRE) ───────────────────────────

;;; ─── PRE support helpers ──────────────────────────────────────────────────

(defun %opt-pre-expression-key (inst)
  "Return a structural key for a pure instruction INST, or NIL."
  (when (opt-inst-pure-p inst)
    (typecase inst
      (vm-const (list :const (vm-value inst)))
      (t
       (let* ((reads (opt-inst-read-regs inst))
              (normalized (if (member (type-of inst) *opt-commutative-inst-types* :test #'eq)
                              (sort (copy-list reads)
                                    #'string<
                                    :key #'prin1-to-string)
                              reads)))
         (and normalized (cons (type-of inst) normalized)))))))

(defun %opt-pre-env-evict-dst (env dst)
  "Remove all entries in ENV whose value is DST (dst was overwritten)."
  (let ((stale nil))
    (maphash (lambda (k v) (when (eq v dst) (push k stale))) env)
    (dolist (k stale) (remhash k env))))

(defun %opt-pre-block-out-env (block)
  "Return key→dst availability after BLOCK for PRE join-point analysis."
  (let ((env (make-hash-table :test #'equal)))
    (dolist (inst (bb-instructions block) env)
      (let ((dst (opt-inst-dst inst)))
        (when dst (%opt-pre-env-evict-dst env dst)))
      (let ((key (%opt-pre-expression-key inst)))
        (when key
          (setf (gethash key env) (opt-inst-dst inst)))))))

(defun %opt-pre-splice-before-terminator (insts additions)
  "Insert ADDITIONS before INSTS' terminator, preserving order."
  (let ((term (car (last insts))))
    (if (and term (typep term '(or vm-jump vm-jump-zero vm-ret vm-halt)))
        (append (butlast insts) additions (list term))
        (append insts additions))))

(defun %opt-pre-reconstruct-inst (inst)
  "Round-trip INST through sexp form, returning INST unchanged on error."
  (handler-case (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %opt-pre-available-in-any-p (key pred-envs)
  "Return T if KEY is available in at least one predecessor environment."
  (some (lambda (pair) (gethash key (cdr pair))) pred-envs))

(defun %opt-pre-emit-compensating (pair key dst inst pred-inserts)
  "Emit a compensating instruction into PAIR's predecessor block, updating its availability env."
  (let* ((pred (car pair))
         (env  (cdr pair))
         (src  (gethash key env)))
    (when (or (null src) (not (eq src dst)))
      (push (if src
                (make-vm-move :dst dst :src src)
                (%opt-pre-reconstruct-inst inst))
            (gethash pred pred-inserts)))
    (setf (gethash key env) dst)))

(defun %opt-pre-join-elim (cfg)
  "Do a small join-point PRE pass over CFG blocks."
  (let ((pred-inserts (make-hash-table :test #'eq))
        (changed nil))
    (loop for block across (cfg-blocks cfg)
          do (when (> (length (bb-predecessors block)) 1)
               (let ((pred-envs (mapcar (lambda (pred)
                                          (cons pred (%opt-pre-block-out-env pred)))
                                        (bb-predecessors block)))
                     (new-insts nil))
                 (dolist (inst (bb-instructions block))
                   (let* ((dst (opt-inst-dst inst))
                          (key (%opt-pre-expression-key inst)))
                     (when dst
                       (dolist (pair pred-envs)
                         (%opt-pre-env-evict-dst (cdr pair) dst)))
                     (if (and dst key (%opt-pre-available-in-any-p key pred-envs))
                         (progn
                           (setf changed t)
                           (dolist (pair pred-envs)
                             (%opt-pre-emit-compensating pair key dst inst pred-inserts)))
                         (push inst new-insts))))
                 (setf (bb-instructions block) (nreverse new-insts)))))
    (maphash (lambda (pred insts)
               (setf (bb-instructions pred)
                     (%opt-pre-splice-before-terminator (bb-instructions pred)
                                                        (nreverse insts))))
             pred-inserts)
    changed))

(defun opt-pass-pre (instructions)
  "Partial Redundancy Elimination (PRE).

   This combines LICM with a small join-point PRE step and CSE so partially
   redundant pure computations are exposed and removed."
  (let* ((licm1 (opt-pass-licm instructions))
         (cse1 (opt-pass-cse licm1))
         (cfg (cfg-build cse1)))
    (%opt-pre-join-elim cfg)
    (opt-pass-cse (opt-pass-licm (cfg-flatten cfg)))))
