(in-package :cl-cc/optimize)
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

(defun %opt-rewrite-block-terminator (block old-label new-label)
  "Rewrite the jump terminator of BLOCK from OLD-LABEL to NEW-LABEL when it matches.
Handles both vm-jump (unconditional) and vm-jump-zero (conditional)."
  (let ((cell (last (bb-instructions block))))
    (when cell
      (let ((term (car cell)))
        (when (equal (vm-label-name term) old-label)
          (setf (car cell)
                (typecase term
                  (vm-jump
                   (make-vm-jump :label new-label))
                  (vm-jump-zero
                   (make-vm-jump-zero :reg (vm-reg term) :label new-label))
                  (t (return-from %opt-rewrite-block-terminator)))))))))

(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return nil)))

(defun %opt-thread-jump (inst vec i idx)
  "Thread unconditional INST; return the (possibly relabeled) jump, or NIL if it falls through."
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (unless (opt-falls-through-to-p vec i threaded)
      (if (equal threaded (vm-label-name inst))
          inst
          (make-vm-jump :label threaded)))))

(defun %opt-thread-jump-zero (inst vec i idx)
  "Thread conditional INST; always return an instruction (never elide conditionals)."
  (declare (ignore i))
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (if (equal threaded (vm-label-name inst))
        inst
        (make-vm-jump-zero :reg (vm-reg inst) :label threaded))))

(defparameter *opt-jump-thread-table*
  (list (cons 'vm-jump      #'%opt-thread-jump)
        (cons 'vm-jump-zero #'%opt-thread-jump-zero))
  "Maps jump instruction types to their threading handlers.")

(defun opt-pass-jump (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
             do (let ((handler (loop for entry in *opt-jump-thread-table*
                                     for type = (car entry)
                                     for fn = (cdr entry)
                                     when (typep inst type) return fn)))
                 (if handler
                     (let ((new (funcall handler inst vec i idx)))
                       (when new (push new result)))
                     (push inst result))))
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

;;; ─── Conservative loop rotation (FR-169 subset) ─────────────────────────

(defun %opt-loop-rotation-fresh-label (base used)
  "Return a fresh label name derived from BASE not present in USED hash-table." 
  (let ((i 0)
        (name nil))
    (loop
      do (setf name (if (= i 0)
                        base
                        (format nil "~A_~D" base i)))
         (if (gethash name used)
             (incf i)
             (return name)))))

(defun opt-pass-loop-rotation (instructions)
  "Rotate simple while-shaped loops into guard+do-while form.

Conservative subset only. Matches this linear shape:
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:
and rewrites to:
  (vm-jump Lguard) Lbody: <body...> Lguard: <cond-inst>
  (vm-jump-zero reg Lexit) (vm-jump Lbody) Lexit:

The transform is skipped unless all structural checks pass."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (used-labels (make-hash-table :test #'equal)))
    (loop for i from 0 below n
          for inst = (aref vec i)
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) used-labels) t))
    (let ((result nil)
          (i 0))
      (loop while (< i n)
            do (let ((cur (aref vec i)))
                 (if (and (vm-label-p cur)
                          (<= (+ i 4) (1- n)))
                     (let* ((header       cur)
                            (cond-inst    (aref vec (+ i 1)))
                            (jz-inst      (aref vec (+ i 2)))
                            (header-name  (vm-name header)))
                       (if (and (typep jz-inst 'vm-jump-zero)
                                (not (vm-label-p cond-inst)))
                           (let* ((exit-name (vm-label-name jz-inst))
                                  (exit-pos  (cfg-find-label-position vec n exit-name))
                                  (back-pos  (and exit-pos (1- exit-pos)))
                                  (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                             (if (and exit-pos
                                      (> exit-pos (+ i 3))
                                      (typep back-inst 'vm-jump)
                                      (equal (vm-label-name back-inst) header-name))
                                 (let* ((body-insts (loop for j from (+ i 3) below back-pos
                                                          collect (aref vec j)))
                                        (body-label-name  (%opt-loop-rotation-fresh-label
                                                           (format nil "~A_body" header-name)
                                                           used-labels))
                                        (guard-label-name (%opt-loop-rotation-fresh-label
                                                           (format nil "~A_guard" header-name)
                                                           used-labels))
                                        (body-label  (make-vm-label :name body-label-name))
                                        (guard-label (make-vm-label :name guard-label-name)))
                                   (setf (gethash body-label-name used-labels) t
                                         (gethash guard-label-name used-labels) t)
                                   (push (make-vm-jump :label guard-label-name) result)
                                   (push body-label result)
                                   (dolist (b body-insts) (push b result))
                                   (push guard-label result)
                                   (push cond-inst result)
                                   (push jz-inst result)
                                   (push (make-vm-jump :label body-label-name) result)
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
                       (incf i)))))
      (nreverse result))))

;;; ─── Conservative loop peeling (FR-170 subset) ──────────────────────────

(defun opt-pass-loop-peeling (instructions)
  "Peel the first iteration of a simple while-shaped loop.

Conservative subset only. Matches this linear shape:
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:
and rewrites to:
  <cond-inst> (vm-jump-zero reg Lexit) <body...>
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:

This duplicates only one first-iteration body before the original loop header."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0)
         (peeled nil))
    (loop while (< i n)
          do (let ((cur (aref vec i)))
               (if (and (not peeled)
                        (vm-label-p cur)
                        (<= (+ i 4) (1- n)))
                   (let* ((header      cur)
                          (cond-inst   (aref vec (+ i 1)))
                          (jz-inst     (aref vec (+ i 2)))
                          (header-name (vm-name header)))
                     (if (and (typep jz-inst 'vm-jump-zero)
                              (not (vm-label-p cond-inst)))
                         (let* ((exit-name (vm-label-name jz-inst))
                                (exit-pos  (cfg-find-label-position vec n exit-name))
                                (back-pos  (and exit-pos (1- exit-pos)))
                                (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                           (if (and exit-pos
                                    (> exit-pos (+ i 3))
                                    (typep back-inst 'vm-jump)
                                    (equal (vm-label-name back-inst) header-name))
                               (let ((body-insts (loop for j from (+ i 3) below back-pos
                                                       collect (aref vec j))))
                                 ;; peeled first iteration (without header label)
                                 (push cond-inst result)
                                 (push jz-inst result)
                                 (dolist (b body-insts) (push b result))
                                 ;; then keep original loop unchanged
                                 (loop for j from i below (1+ exit-pos)
                                       do (push (aref vec j) result))
                                 (setf i (1+ exit-pos)
                                       peeled t))
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

;;; ─── Conservative loop unrolling (FR-021 subset) ────────────────────────

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

(defun %opt-first-inst-after-label-index (vec label-index)
  "Return first index after LABEL-INDEX that is not a vm-label, or NIL." 
  (loop for i from (1+ label-index) below (length vec)
        for inst = (aref vec i)
        unless (vm-label-p inst)
        do (return i)))

(defun opt-pass-code-sinking (instructions)
  "Sink selected constants into the unique use block.

Conservative subset:
- source shape: (vm-const dst v) followed by (vm-jump label)
- dst has exactly one read in whole function
- target label exists and target block reads dst

Transform moves vm-const from source block to target block entry.
No duplication, no control-dependent sinking, no side-effect motion." 
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (remove-at (make-hash-table :test #'eql))
         (insertions (make-hash-table :test #'eql)))
    (loop for i from 0 below (- n 1)
          for inst = (aref vec i)
          for nxt = (aref vec (1+ i))
          do (when (and (typep inst 'vm-const)
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
