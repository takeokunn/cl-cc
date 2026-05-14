;;;; optimizer-flow-loop.lisp — Loop unrolling, code sinking, dead-block elimination, type-check elim
(in-package :cl-cc/optimize)


(defparameter *opt-loop-unroll-max-trip* 4
  "Maximum compile-time trip count for conservative full unrolling.")

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

(defun opt-pass-loop-unrolling (instructions)
  "Fully unroll tiny counted loops with compile-time trip count.

Conservative subset. Matches this linear shape:
  Lh: (vm-lt rcond riv rlim) (vm-jump-zero rcond Lexit) body... step (vm-jump Lh) Lexit:
where STEP is (vm-add riv riv rstep) and riv/rlim/rstep are integer constants
known at compile time before Lh. Only applies when 0 < trip <=
*opt-loop-unroll-max-trip* and no external jumps target Lh."
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
                     (if (and (typep cmp-inst 'vm-lt)
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
                                                           (opt-induction-trip-count init limit step))))
                                       (if (and trip (> trip 0) (<= trip *opt-loop-unroll-max-trip*))
                                           (progn
                                             (dotimes (_ trip)
                                               (dolist (b body-insts)
                                                 (push b result)))
                                             ;; keep exit label and continue
                                             (setf i exit-pos)
                                             (push (aref vec i) result)
                                             (incf i))
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

(defun %opt-code-sinking-candidate-p (inst)
  "Return T when INST is safe for the conservative code-sinking subset.

Current candidates:
- vm-const (existing behavior)
- vm-cons  (allocation sinking for immediate consumers)"
  (or (typep inst 'vm-const)
      (typep inst 'vm-cons)))

(defun %opt-first-inst-after-label-index (vec label-index)
  "Return first index after LABEL-INDEX that is not a vm-label, or NIL."
  (loop for i from (1+ label-index) below (length vec)
        for inst = (aref vec i)
        unless (vm-label-p inst)
        do (return i)))

(defun opt-pass-code-sinking (instructions)
  "Sink selected pure values into the unique use block.

Conservative subset:
- source shape: (vm-const|vm-cons producing dst) followed by (vm-jump label)
- dst has exactly one read in whole function
- target label exists and target block reads dst

Transform moves the selected producer from source block to target block entry.
No duplication, no control-dependent sinking, no side-effect motion."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (remove-at (make-hash-table :test #'eql))
         (insertions (make-hash-table :test #'eql)))
    (loop for i from 0 below (- n 1)
          for inst = (aref vec i)
          for nxt = (aref vec (1+ i))
          do (when (and (%opt-code-sinking-candidate-p inst)
                         (typep nxt 'vm-jump))
               (let* ((dst (vm-dst inst))
                      (reads (%opt-reg-read-count instructions dst))
                      (label-name (vm-label-name nxt))
                      (label-pos (cfg-find-label-position vec n label-name)))
                 (when (and (= reads 1) label-pos)
                   (let ((target-inst-pos (%opt-first-inst-after-label-index vec label-pos)))
                     (when (and target-inst-pos
                                (member dst (opt-inst-read-regs (aref vec target-inst-pos)) :test #'eq))
                       (setf (gethash i remove-at) t)
                       (push inst (gethash label-pos insertions))))))))
    (let ((out nil))
      (loop for i from 0 below n
            for inst = (aref vec i)
            do (unless (gethash i remove-at)
                 (push inst out))
               (dolist (ins (nreverse (gethash i insertions)))
                 (push ins out)))
      (nreverse out))))

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
