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
            do (let ((handler (loop for (type . fn) in *opt-jump-thread-table*
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
