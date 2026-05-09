(in-package :cl-cc/optimize)

;;; ─── Generic Dataflow Framework ───────────────────────────────────────────

(defstruct (opt-dataflow-result (:conc-name opt-dataflow-result-))
  "Stores per-block IN and OUT states for a dataflow analysis."
  cfg
  direction
  (in  (make-hash-table :test #'eq) :type hash-table)
  (out (make-hash-table :test #'eq) :type hash-table))

(defun %opt-ensure-cfg (cfg-or-instructions)
  "Return CFG-OR-INSTRUCTIONS as a CFG, building one when needed."
  (if (cfg-p cfg-or-instructions)
      cfg-or-instructions
      (cfg-build cfg-or-instructions)))

(defun %opt-dataflow-copy-state (state copy-state)
  "Copy STATE with COPY-STATE when provided, else return STATE unchanged."
  (if copy-state
      (funcall copy-state state)
      state))

(defun %opt-dataflow-state-set-equal-p (a b &key (test #'equal))
  "Return T when A and B contain the same set of elements under TEST."
  (and (= (length a) (length b))
       (subsetp a b :test test)
       (subsetp b a :test test)))

(defun %opt-dataflow-boundary-block (cfg direction)
  "Return the entry/exit boundary block for DIRECTION in CFG."
  (ecase direction
    (:forward  (cfg-entry cfg))
    (:backward (cfg-exit cfg))))

(defun %opt-dataflow-neighbors (block direction)
  "Return the predecessor/successor blocks that flow into BLOCK."
  (ecase direction
    (:forward  (bb-predecessors block))
    (:backward (bb-successors block))))

(defun %opt-dataflow-users (block direction)
  "Return the blocks whose input depends on BLOCK's output."
  (ecase direction
    (:forward  (bb-successors block))
    (:backward (bb-predecessors block))))

(defun %opt-dataflow-merge-states (states meet initial-state copy-state)
  "Merge STATES with MEET, or copy INITIAL-STATE when STATES is empty."
  (if states
      (funcall meet states)
      (%opt-dataflow-copy-state initial-state copy-state)))

(defun opt-run-dataflow (cfg &key
                               (direction :forward)
                               meet
                               transfer
                               (state-equal #'equal)
                               (initial-state nil)
                               (boundary-state initial-state)
                               (copy-state #'identity))
  "Run a generic worklist dataflow analysis over CFG.

MEET merges a list of predecessor/successor states. TRANSFER computes the new
output state for BLOCK from its incoming state. STATE-EQUAL compares states for
convergence. INITIAL-STATE seeds unreachable edges; BOUNDARY-STATE is used at
the entry block for forward analyses and exit block for backward analyses."
  (let* ((graph          (%opt-ensure-cfg cfg))
         (boundary-block (%opt-dataflow-boundary-block graph direction))
         (blocks         (loop for block across (cfg-blocks graph)
                               when block collect block))
         (result         (make-opt-dataflow-result :cfg graph :direction direction))
         (in-map         (opt-dataflow-result-in result))
         (out-map        (opt-dataflow-result-out result))
         (worklist       (copy-list blocks)))
    (dolist (block blocks)
      (setf (gethash block in-map)  (%opt-dataflow-copy-state initial-state copy-state)
            (gethash block out-map) (%opt-dataflow-copy-state initial-state copy-state)))
    (when boundary-block
      (ecase direction
        (:forward
         (setf (gethash boundary-block in-map)
               (%opt-dataflow-copy-state boundary-state copy-state)))
        (:backward
         (setf (gethash boundary-block out-map)
               (%opt-dataflow-copy-state boundary-state copy-state)))))
    (loop while worklist
          do (let* ((block     (pop worklist))
                    (incoming  (%opt-dataflow-neighbors block direction))
                    (state-in  (loop for other in incoming
                                     collect (ecase direction
                                               (:forward  (gethash other out-map))
                                               (:backward (gethash other in-map))))))
               (ecase direction
                 (:forward
                  (let* ((new-in (if (eq block boundary-block)
                                     (%opt-dataflow-copy-state boundary-state copy-state)
                                     (%opt-dataflow-merge-states state-in meet initial-state copy-state)))
                         (old-out (gethash block out-map))
                         (new-out (funcall transfer block new-in)))
                    (setf (gethash block in-map) new-in)
                    (unless (funcall state-equal old-out new-out)
                      (setf (gethash block out-map) new-out)
                      (dolist (user (%opt-dataflow-users block direction))
                        (pushnew user worklist :test #'eq)))))
                 (:backward
                  (let* ((new-out (if (eq block boundary-block)
                                      (%opt-dataflow-copy-state boundary-state copy-state)
                                      (%opt-dataflow-merge-states state-in meet initial-state copy-state)))
                         (old-in (gethash block in-map))
                         (new-in (funcall transfer block new-out)))
                    (setf (gethash block out-map) new-out)
                    (unless (funcall state-equal old-in new-in)
                      (setf (gethash block in-map) new-in)
                      (dolist (user (%opt-dataflow-users block direction))
                        (pushnew user worklist :test #'eq))))))))
    result))

(defmacro define-dataflow-pass (name lambda-list &rest options)
  "Define NAME as a thin wrapper around OPT-RUN-DATAFLOW."
  (let* ((cfg-arg        (car lambda-list))
         (missing        (gensym "MISSING"))
         (direction      (or (getf options :direction) :forward))
         (meet           (getf options :meet))
         (transfer       (getf options :transfer))
         (state-equal    (or (getf options :state-equal) '#'equal))
         (initial-state  (getf options :initial-state))
         (boundary-state (let ((value (getf options :boundary-state missing)))
                           (if (eq value missing)
                               initial-state
                               value)))
         (copy-state     (or (getf options :copy-state) '#'identity))
         (documentation  (or (getf options :documentation)
                             (format nil "Run the ~A dataflow analysis." name))))
    `(defun ,name ,lambda-list
       ,documentation
       (let ((cfg (%opt-ensure-cfg ,cfg-arg)))
         (opt-run-dataflow cfg
                           :direction ,direction
                           :meet ,meet
                           :transfer ,transfer
                           :state-equal ,state-equal
                           :initial-state ,initial-state
                           :boundary-state ,boundary-state
                           :copy-state ,copy-state)))))

;;; ─── Available Expressions ────────────────────────────────────────────────

(defun %available-expression-inst-p (inst)
  "Return T when INST contributes a conservative available expression."
  (let ((dst   (opt-inst-dst inst))
        (reads (opt-inst-read-regs inst)))
    (and dst reads (opt-inst-pure-p inst))))

(defun %available-expression-entry (inst block instruction-index)
  "Build the available-expression entry recorded for INST."
  (let ((reads (copy-list (opt-inst-read-regs inst))))
    (list (cons (type-of inst) reads)
          reads
          (opt-inst-dst inst)
          (bb-id block)
          instruction-index
          inst)))

(defun %available-expression-entry-key (entry)
  "Return the stable expression key stored in ENTRY."
  (first entry))

(defun %available-expression-entry-reads (entry)
  "Return the read-register list stored in ENTRY."
  (second entry))

(defun %available-expression-state-equal (a b)
  "Return T when A and B contain the same available-expression keys."
  (%opt-dataflow-state-set-equal-p
   (mapcar #'%available-expression-entry-key a)
   (mapcar #'%available-expression-entry-key b)
   :test #'equal))

(defun %available-expression-meet (states)
  "Intersect available expressions across STATES."
  (cond
    ((null states) nil)
    ((null (cdr states)) (copy-list (car states)))
    (t (let ((others (cdr states)))
         (loop for entry in (car states)
               for key = (%available-expression-entry-key entry)
               when (every (lambda (state)
                             (find key state
                                   :test #'equal
                                   :key #'%available-expression-entry-key))
                           others)
               collect entry)))))

(defun %available-expression-transfer (block in-state)
  "Transfer available expressions through BLOCK."
  (let ((state (copy-list in-state)))
    (loop for inst in (bb-instructions block)
          for instruction-index from 0
          do (let ((dst (opt-inst-dst inst)))
               (when dst
                 (setf state
                       (remove-if (lambda (entry)
                                    (member dst (%available-expression-entry-reads entry)
                                            :test #'eq))
                                  state)))
               (when (%available-expression-inst-p inst)
                 (let* ((entry (%available-expression-entry inst block instruction-index))
                        (key   (%available-expression-entry-key entry)))
                   (setf state
                         (cons entry
                               (remove key state
                                       :test #'equal
                                       :key #'%available-expression-entry-key)))))))
    state))

(define-dataflow-pass opt-compute-available-expressions (cfg-or-instructions)
  :direction :forward
  :meet #'%available-expression-meet
  :transfer #'%available-expression-transfer
  :state-equal #'%available-expression-state-equal
  :initial-state nil
  :boundary-state nil
  :copy-state #'copy-list
  :documentation "Compute conservative available expressions for CFG-OR-INSTRUCTIONS.")

;;; ─── Reaching Definitions ────────────────────────────────────────────────

(defun %reaching-definition-entry (reg block instruction-index inst)
  "Return the reaching-definition tuple for REG at BLOCK/INSTRUCTION-INDEX."
  (list reg (bb-id block) instruction-index inst))

(defun %reaching-definition-reg (definition)
  "Return the destination register described by DEFINITION."
  (first definition))

(defun %reaching-definitions-meet (states)
  "Union reaching definitions across STATES."
  (let ((merged nil))
    (dolist (state states)
      (dolist (definition state)
        (pushnew definition merged :test #'equal)))
    (nreverse merged)))

(defun %reaching-definitions-state-equal (a b)
  "Return T when A and B contain the same reaching-definition tuples."
  (%opt-dataflow-state-set-equal-p a b :test #'equal))

(defun %reaching-definitions-transfer (block in-state)
  "Transfer reaching definitions through BLOCK."
  (let ((state (copy-list in-state)))
    (loop for inst in (bb-instructions block)
          for instruction-index from 0
          do (let ((dst (opt-inst-dst inst)))
               (when dst
                 (setf state
                       (remove dst state :test #'eq :key #'%reaching-definition-reg))
                 (push (%reaching-definition-entry dst block instruction-index inst)
                       state))))
    state))

(define-dataflow-pass opt-compute-reaching-definitions (cfg-or-instructions)
  :direction :forward
  :meet #'%reaching-definitions-meet
  :transfer #'%reaching-definitions-transfer
  :state-equal #'%reaching-definitions-state-equal
  :initial-state nil
  :boundary-state nil
  :copy-state #'copy-list
  :documentation "Compute reaching definitions for CFG-OR-INSTRUCTIONS.")

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
            (if (and found (numberp sval))
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
