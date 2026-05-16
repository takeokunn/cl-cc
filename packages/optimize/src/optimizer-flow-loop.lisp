;;;; optimizer-flow-loop.lisp — Loop unrolling, code sinking, dead-block elimination, type-check elim
(in-package :cl-cc/optimize)


(defparameter *opt-loop-unroll-max-trip* 8
  "Maximum compile-time trip count for conservative full unrolling.")

(defparameter *opt-loop-unroll-factor* 2
  "Conservative partial-unroll factor for counted loops that are too large or unknown.")

(defparameter *opt-loop-unroll-max-body* 8
  "Maximum loop body instruction count eligible for conservative partial unrolling.")

(defun %opt-build-const-env-up-to (vec end)
  "Return reg->integer constant env from VEC[0,END)."
  (let ((env (make-hash-table :test #'eq)))
    (loop for i from 0 below end
          for inst = (aref vec i)
          do (cond
               ((typep inst 'vm-const)
                (if (integerp (vm-value inst))
                    (setf (gethash (vm-dst inst) env) (vm-value inst))
                    (remhash (vm-dst inst) env)))
               (t
                (let ((dst (opt-inst-dst inst)))
                  (when dst (remhash dst env))))))
    env))

(defun %opt-has-external-jump-to-label-p (vec label-name start end)
  "Return T when any jump outside [START,END] targets LABEL-NAME."
  (loop for i from 0 below (length vec)
        thereis (and (not (and (<= start i) (<= i end)))
                     (let ((inst (aref vec i)))
                       (and (typep inst '(or vm-jump vm-jump-zero))
                             (equal (vm-label-name inst) label-name))))))

(defun %opt-loop-unroll-cmp-inst-p (inst)
  "Return T when INST is a comparison supported by loop unrolling."
  (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq)))

(defun %opt-loop-unroll-trip-count (cmp-inst init limit step)
  "Return conservative trip count for CMP-INST and affine induction values."
  (opt-induction-trip-count init limit step :predicate cmp-inst))

(defun %opt-loop-unroll-emit-partial (body-insts cmp-inst jz-inst result)
  "Emit a guarded partial unroll of BODY-INSTS into RESULT.
Returns the updated RESULT list. Each extra copy is guarded by the original
condition so odd or short trip counts fall through to the original exit."
  (dotimes (_ *opt-loop-unroll-factor* result)
    (push cmp-inst result)
    (push jz-inst result)
    (dolist (b body-insts)
      (push b result))))

(defun opt-pass-loop-unrolling (instructions)
  "Unroll conservative counted loops.

Conservative subset. Matches this linear shape:
  Lh: (<cmp> rcond riv rlim) (vm-jump-zero rcond Lexit) body... step (vm-jump Lh) Lexit:
where STEP is (vm-add riv riv rstep) and riv/rlim/rstep are integer constants
known at compile time before Lh. Full unrolling applies when 0 < trip <=
*opt-loop-unroll-max-trip*. Larger or unknown trips get guarded partial
unrolling when the body is small."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0))
    (loop while (< i n)
          do (let ((cur (aref vec i)))
               (if (and (vm-label-p cur)
                        (<= (+ i 5) (1- n)))
                   (let* ((header cur)
                          (cmp-inst (aref vec (+ i 1)))
                          (jz-inst  (aref vec (+ i 2)))
                          (header-name (vm-name header)))
                      (if (and (%opt-loop-unroll-cmp-inst-p cmp-inst)
                               (typep jz-inst 'vm-jump-zero)
                               (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
                         (let* ((exit-name (vm-label-name jz-inst))
                                (exit-pos  (cfg-find-label-position vec n exit-name))
                                (back-pos  (and exit-pos (1- exit-pos)))
                                (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                           (if (and exit-pos
                                    (> exit-pos (+ i 4))
                                    (typep back-inst 'vm-jump)
                                    (equal (vm-label-name back-inst) header-name)
                                    (not (%opt-has-external-jump-to-label-p vec header-name i exit-pos)))
                               (let* ((body-insts (loop for j from (+ i 3) below back-pos
                                                        collect (aref vec j)))
                                      (step-inst (car (last body-insts)))
                                      (const-env (%opt-build-const-env-up-to vec i)))
                                 (if (and (typep step-inst 'vm-add)
                                          (eq (vm-dst step-inst) (vm-lhs step-inst))
                                          (eq (vm-dst step-inst) (vm-lhs cmp-inst)))
                                     (let* ((iv-reg   (vm-lhs cmp-inst))
                                            (lim-reg  (vm-rhs cmp-inst))
                                            (step-reg (vm-rhs step-inst))
                                            (init     (gethash iv-reg const-env))
                                            (limit    (gethash lim-reg const-env))
                                            (step     (gethash step-reg const-env))
                                             (trip     (and init limit step
                                                            (%opt-loop-unroll-trip-count cmp-inst init limit step))))
                                        (cond
                                          ((and trip (> trip 0) (<= trip *opt-loop-unroll-max-trip*))
                                           (dotimes (_ trip)
                                             (dolist (b body-insts)
                                               (push b result)))
                                           ;; keep exit label and continue
                                           (setf i exit-pos)
                                           (push (aref vec i) result)
                                           (incf i))
                                           ((and (<= (length body-insts) *opt-loop-unroll-max-body*)
                                                 (plusp *opt-loop-unroll-factor*))
                                            (setf result (%opt-loop-unroll-emit-partial body-insts cmp-inst jz-inst result))
                                            (loop for j from i below (1+ exit-pos)
                                                  do (push (aref vec j) result))
                                            (setf i (1+ exit-pos)))
                                          (t
                                           (push cur result)
                                           (incf i))))
                                     (progn
                                       (push cur result)
                                       (incf i))))
                               (progn
                                 (push cur result)
                                 (incf i))))
                         (progn
                           (push cur result)
                           (incf i))))
                   (progn
                     (push cur result)
                     (incf i)))))
    (nreverse result)))

;;; ─── Conservative code sinking (FR-163 subset) ──────────────────────────

(defun %opt-reg-read-count (instructions reg)
  "Count reads of REG in INSTRUCTIONS."
  (let ((count 0))
    (dolist (inst instructions count)
      (dolist (r (opt-inst-read-regs inst))
        (when (eq r reg)
          (incf count))))))

(defun %opt-copy-inst (inst)
  "Return a structural copy of INST when it can be sexp-round-tripped."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %opt-code-sinking-constant-regs (blocks)
  "Return a hash table of registers defined by vm-const instructions."
  (let ((constants (make-hash-table :test #'eq)))
    (dolist (block blocks constants)
      (dolist (inst (bb-instructions block))
        (let ((dst (opt-inst-dst inst)))
          (when dst (remhash dst constants)))
        (when (typep inst 'vm-const)
          (setf (gethash (vm-dst inst) constants) t))))))

(defun %opt-code-sinking-candidate-p (inst constant-regs sunk-regs)
  "Return T when INST is safe and profitable enough to sink.

The pass keeps allocation sinking for vm-cons, adds constants/copies, admits
constant-operand arithmetic, and otherwise allows pure instructions whose
operands are known loop-invariant by this local analysis."
  (let ((reads (opt-inst-read-regs inst)))
    (cond
      ((typep inst 'vm-const) t)
      ((typep inst 'vm-cons) t)
      ((typep inst 'vm-move)
       (or (gethash (vm-src inst) constant-regs)
           (gethash (vm-src inst) sunk-regs)))
      ((typep inst '(or vm-add vm-sub vm-mul))
       (every (lambda (reg) (gethash reg constant-regs)) reads))
      ((opt-inst-pure-p inst)
       (every (lambda (reg)
                (or (gethash reg constant-regs)
                    (gethash reg sunk-regs)))
              reads))
      (t nil))))

(defun %opt-first-inst-after-label-index (vec label-index)
  "Return first index after LABEL-INDEX that is not a vm-label, or NIL."
  (loop for i from (1+ label-index) below (length vec)
        for inst = (aref vec i)
        unless (vm-label-p inst)
        do (return i)))

(defun %opt-block-terminator-index (insts)
  "Return the index of the first terminator in INSTS, or NIL."
  (position-if (lambda (inst)
                 (typep inst '(or vm-jump vm-jump-zero vm-ret vm-halt)))
               insts))

(defun %opt-insert-before-index (list index values)
  "Return LIST with VALUES inserted before INDEX."
  (append (subseq list 0 index) values (nthcdr index list)))

(defun %opt-remove-nth (list index)
  "Return LIST without the element at INDEX."
  (append (subseq list 0 index) (nthcdr (1+ index) list)))

(defun %opt-code-sinking-locations (cfg target-reg)
  "Return (BLOCK INDEX INST) locations where TARGET-REG is read."
  (let ((locations nil))
    (loop for block across (cfg-blocks cfg)
          do (loop for inst in (bb-instructions block)
                   for index from 0
                   when (member target-reg (opt-inst-read-regs inst) :test #'eq)
                     do (push (list block index inst) locations)))
    (nreverse locations)))

(defun %opt-code-sinking-definition-count (cfg target-reg)
  "Count definitions of TARGET-REG in CFG."
  (let ((count 0))
    (loop for block across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions block))
               (when (eq (opt-inst-dst inst) target-reg)
                 (incf count))))
    count))

(defun %opt-common-dominator (blocks)
  "Return the nearest common dominator of BLOCKS."
  (when blocks
    (reduce #'cfg-intersect blocks)))

(defun %opt-first-use-index-in-block (block reg)
  "Return the first instruction index in BLOCK that reads REG, or NIL."
  (position-if (lambda (inst)
                 (member reg (opt-inst-read-regs inst) :test #'eq))
               (bb-instructions block)))

(defun %opt-cheap-duplicable-sink-p (inst)
  "Return T for instructions cheap enough to duplicate onto conditional edges."
  (typep inst 'vm-const))

(defun %opt-sink-duplicate-into-conditional-successors (def-block def-index inst reg uses)
  "Duplicate cheap INST into both conditional successors when each edge uses REG."
  (let* ((term (car (last (bb-instructions def-block))))
         (succs (bb-successors def-block)))
    (when (and (%opt-cheap-duplicable-sink-p inst)
               (typep term 'vm-jump-zero)
               (= (length succs) 2)
               (= (length uses) 2)
               (every (lambda (succ)
                        (some (lambda (use)
                                (cfg-dominates-p succ (first use)))
                              uses))
                      succs))
      (dolist (succ succs)
        (let ((pos (%opt-first-use-index-in-block succ reg)))
          (when pos
            (setf (bb-instructions succ)
                  (%opt-insert-before-index (bb-instructions succ) pos
                                            (list (%opt-copy-inst inst)))))))
      (setf (bb-instructions def-block)
            (%opt-remove-nth (bb-instructions def-block) def-index))
      t)))

(defun %opt-sink-inst-before-uses (def-block def-index inst reg uses)
  "Move INST from DEF-BLOCK to the nearest common dominator of USES."
  (let* ((use-blocks (remove-duplicates (mapcar #'first uses) :test #'eq))
         (sink-block (%opt-common-dominator use-blocks)))
    (when (and sink-block
               (not (eq sink-block def-block)))
      (let* ((first-use (%opt-first-use-index-in-block sink-block reg))
             (term-index (%opt-block-terminator-index (bb-instructions sink-block)))
             (insert-index (or first-use term-index (length (bb-instructions sink-block)))))
        (setf (bb-instructions def-block)
              (%opt-remove-nth (bb-instructions def-block) def-index))
        (setf (bb-instructions sink-block)
              (%opt-insert-before-index (bb-instructions sink-block) insert-index (list inst)))
        t))))

(defun opt-pass-code-sinking (instructions)
  "Sink selected pure values closer to their CFG uses.

The pass builds a CFG, computes dominance, finds all uses of each candidate
definition, and moves it to the nearest common dominator of those uses.  Cheap
constants may be duplicated into both arms of a conditional when both arms use
the value.  Side-effecting instructions are never moved."
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (let* ((blocks (coerce (cfg-blocks cfg) 'list))
           (constant-regs (%opt-code-sinking-constant-regs blocks))
           (sunk-regs (make-hash-table :test #'eq))
           (changed nil))
      (dolist (block blocks)
        (let ((insts (bb-instructions block)))
          (loop for index downfrom (1- (length insts)) to 0
                for inst = (nth index insts)
                for dst = (opt-inst-dst inst)
                do (when (and dst
                              (= (%opt-code-sinking-definition-count cfg dst) 1)
                              (%opt-code-sinking-candidate-p inst constant-regs sunk-regs))
                     (let ((uses (%opt-code-sinking-locations cfg dst)))
                       (when uses
                         (when (or (%opt-sink-duplicate-into-conditional-successors
                                    block index inst dst uses)
                                   (and (= (length uses) 1)
                                        (%opt-sink-inst-before-uses block index inst dst uses)))
                           (setf (gethash dst sunk-regs) t
                                 changed t)
                           (return))))))))
      (if changed
          (cfg-flatten (cfg-build (cfg-flatten cfg)))
          (cfg-flatten cfg)))))

(defun opt-pass-dead-basic-blocks (instructions)
  "Eliminate unreachable basic blocks by round-tripping through the CFG.
   Reachability is computed from the entry block; unreachable blocks and their
   instructions are dropped when the CFG is flattened back to a linear list."
  (cfg-flatten (cfg-build instructions)))

(defun %type-check-elim-forget-def (facts reg)
  "Remove all facts whose :src or :dst is REG (killed by a def of REG)."
  (remove-if (lambda (fact)
               (or (eq (getf fact :src) reg)
                   (eq (getf fact :dst) reg)))
             facts))

(defun %type-check-elim-lookup-fact (facts pred src)
  "Find the first fact matching predicate PRED applied to source register SRC."
  (find-if (lambda (fact)
             (and (eq (getf fact :pred) pred)
                  (eq (getf fact :src) src)))
           facts))

(defun %type-check-elim-process-block (block facts)
  "Walk BLOCK under dominator FACTS; rewrite redundant type checks; recurse into dom-children."
  (let ((local-facts (copy-list facts))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let ((dst (opt-inst-dst inst)))
        (when dst
          (setf local-facts (%type-check-elim-forget-def local-facts dst))))
      (cond
        ((and (or (opt-foldable-type-pred-p inst) (typep inst 'vm-not))
              (vm-src inst) (vm-dst inst))
         (let* ((pred (type-of inst))
                (src  (vm-src inst))
                (dst  (vm-dst inst))
                (fact (%type-check-elim-lookup-fact local-facts pred src)))
           (if (and fact (not (eq dst (getf fact :dst))))
               (push (make-vm-move :dst dst :src (getf fact :dst)) new-insts)
               (progn
                 (push inst new-insts)
                 (push (list :pred pred :src src :dst dst) local-facts)))))
        (t (push inst new-insts))))
    (setf (bb-instructions block) (nreverse new-insts))
    (dolist (child (bb-dom-children block))
      (%type-check-elim-process-block child local-facts))))

(defun opt-pass-dominated-type-check-elim (instructions)
  "Eliminate redundant pure type predicates dominated by an earlier identical
    predicate on the same source register. Nil checks via vm-not are treated
    the same way. The first check is kept; later checks are replaced with
    vm-move from the dominating result register."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (%type-check-elim-process-block (cfg-entry cfg) nil))
    (cfg-flatten cfg)))

;; %opt-branch-predicate-fact-for-block, opt-pass-branch-correlation,
;; opt-pass-block-merge, and opt-pass-tail-merge are in optimizer-flow-passes.lisp (loaded next).
