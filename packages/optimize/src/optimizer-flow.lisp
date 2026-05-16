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
  "Thread jump chains, remove fallthrough jumps, and propagate edge facts.

The value-propagation part is deliberately conservative: it only carries facts
derived from vm-jump-zero conditions whose comparison operands are both known
constants in the predecessor block, only into single-predecessor successors,
and never across CFG back-edges."
  (opt-pass-jump-threading-with-propagation instructions))

(defun %opt-pass-jump-chain-only (instructions)
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

(defun %opt-jump-comparison-p (inst)
  "Return T when INST is a comparison supported by jump fact propagation."
  (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq)))

(defun %opt-jump-fact-killed-p (fact dst)
  "Return T when a write to DST invalidates FACT's comparison operands."
  (and fact dst
       (or (eq dst (getf fact :lhs))
           (eq dst (getf fact :rhs)))))

(defun %opt-jump-same-comparison-p (inst fact)
  "Return T when INST repeats the comparison described by FACT."
  (and fact
       (%opt-jump-comparison-p inst)
       (eq (type-of inst) (getf fact :pred))
       (eq (vm-lhs inst) (getf fact :lhs))
       (eq (vm-rhs inst) (getf fact :rhs))
       (opt-inst-dst inst)))

(defun %opt-jump-branch-comparison-fact (block)
  "Return a comparison fact for BLOCK's vm-jump-zero terminator, or NIL.

The comparison must define the branch condition register, and both comparison
operands must be known constants at that point in the same block. The returned
fact does not include the edge value; callers add :VALUE for taken/fallthrough
successors."
  (let* ((insts (bb-instructions block))
         (term (car (last insts))))
    (unless (typep term 'vm-jump-zero)
      (return-from %opt-jump-branch-comparison-fact nil))
    (let ((env (make-hash-table :test #'eq))
          (cond-reg (vm-reg term))
          (fact nil))
      (dolist (inst insts fact)
        (when (eq inst term)
          (return fact))
        (let ((dst (opt-inst-dst inst)))
          (when dst
            (cond
              ((typep inst 'vm-const)
               (setf (gethash dst env) (vm-value inst)))
              (t
               (remhash dst env))))
          (when (and dst (eq dst cond-reg))
            (setf fact nil)
            (when (%opt-jump-comparison-p inst)
              (multiple-value-bind (lval lfound) (gethash (vm-lhs inst) env)
                (declare (ignore lval))
                (multiple-value-bind (rval rfound) (gethash (vm-rhs inst) env)
                  (declare (ignore rval))
                  (when (and lfound rfound)
                    (setf fact (list :pred (type-of inst)
                                     :lhs (vm-lhs inst)
                                     :rhs (vm-rhs inst)))))))))))))

(defun %opt-jump-back-edge-p (pred succ)
  "Return T when PRED -> SUCC is a CFG back-edge."
  (and (bb-idom pred)
       (cfg-dominates-p succ pred)))

(defun %opt-jump-edge-fact (pred succ cfg live-fact)
  "Return the fact propagated on edge PRED -> SUCC, or NIL."
  (unless (and (= (length (bb-predecessors succ)) 1)
               (not (%opt-jump-back-edge-p pred succ)))
    (return-from %opt-jump-edge-fact nil))
  (let* ((term (car (last (bb-instructions pred))))
         (branch-fact (%opt-jump-branch-comparison-fact pred)))
    (if (and branch-fact (typep term 'vm-jump-zero))
        (let ((target (cfg-get-block-by-label cfg (vm-label-name term))))
          (append branch-fact (list :value (if (eq succ target) 0 1))))
        live-fact)))

(defun %opt-jump-rewrite-block-with-fact (block fact)
  "Rewrite redundant comparisons in BLOCK using incoming FACT.
Returns the still-live fact after scanning the block."
  (let ((live-fact fact)
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let ((dst (opt-inst-dst inst)))
        (when (%opt-jump-fact-killed-p live-fact dst)
          (setf live-fact nil))
        (cond
          ((%opt-jump-same-comparison-p inst live-fact)
           (push (make-vm-const :dst dst :value (getf live-fact :value))
                 new-insts))
          (t
           (push inst new-insts)))))
    (setf (bb-instructions block) (nreverse new-insts))
    live-fact))

(defun %opt-pass-jump-propagate-edge-values (instructions)
  "Propagate known comparison values from conditional CFG edges."
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (let ((rpo (cfg-compute-rpo cfg))
          (facts (make-hash-table :test #'eq)))
      (dolist (block rpo)
        (let ((live-fact (%opt-jump-rewrite-block-with-fact
                          block (gethash block facts))))
          (dolist (succ (bb-successors block))
            (let ((edge-fact (%opt-jump-edge-fact block succ cfg live-fact)))
              (when edge-fact
                (setf (gethash succ facts) edge-fact))))))
      (cfg-flatten cfg))))

(defun opt-pass-jump-threading-with-propagation (instructions)
  "Run jump-chain threading, then conservative comparison-value propagation."
  (%opt-pass-jump-propagate-edge-values
   (%opt-pass-jump-chain-only instructions)))

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

(defun %opt-loop-label-positions (vec)
  "Return a label-name -> vector-position table for VEC."
  (let ((positions (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          for inst = (aref vec i)
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) positions) i))
    positions))

(defun %opt-loop-inst-position (vec inst)
  "Return the position of INST in VEC using object identity."
  (loop for i from 0 below (length vec)
        when (eq (aref vec i) inst)
        return i))

(defun %opt-loop-last-jump-to (block label-name)
  "Return BLOCK's final vm-jump when it jumps to LABEL-NAME."
  (let ((term (car (last (bb-instructions block)))))
    (and (typep term 'vm-jump)
         (equal (vm-label-name term) label-name)
         term)))

(defun %opt-loop-cfg-candidate (instructions)
  "Find a conservative single-latch natural loop candidate using the CFG.
Returns a plist with linear positions needed by the existing transforms."
  (handler-case
      (let* ((vec (coerce instructions 'vector))
             (label-pos (%opt-loop-label-positions vec))
             (cfg (cfg-build instructions)))
        (cfg-compute-dominators cfg)
        (cfg-compute-loop-depths cfg)
        (loop for latch across (cfg-blocks cfg)
              thereis
              (loop for header in (bb-successors latch)
                    thereis
                    (and (cfg-dominates-p header latch)
                         (bb-label header)
                         (let* ((header-name (vm-name (bb-label header)))
                                (loop-blocks (cfg-collect-natural-loop header latch))
                                (latches (remove-if-not
                                          (lambda (b) (%opt-loop-last-jump-to b header-name))
                                          loop-blocks)))
                           (when (= (length latches) 1)
                             (let* ((header-insts (bb-instructions header))
                                    (cond-inst (first header-insts))
                                    (jz-inst (second header-insts))
                                    (back-inst (%opt-loop-last-jump-to latch header-name))
                                    (header-pos (gethash header-name label-pos))
                                    (exit-pos (and (typep jz-inst 'vm-jump-zero)
                                                   (gethash (vm-label-name jz-inst) label-pos)))
                                    (back-pos (%opt-loop-inst-position vec back-inst)))
                               (when (and cond-inst
                                          (typep jz-inst 'vm-jump-zero)
                                          (not (vm-label-p cond-inst))
                                          header-pos exit-pos back-pos
                                          (< header-pos back-pos exit-pos))
                                 (list :header-name header-name
                                       :header-pos header-pos
                                       :exit-pos exit-pos
                                       :back-pos back-pos
                                       :cond-inst cond-inst
                                       :jz-inst jz-inst)))))))))
    (error () nil)))

(defun %opt-loop-rotation-apply-candidate (instructions candidate)
  "Apply loop rotation for a CFG-derived CANDIDATE and return a fresh list."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-pos (getf candidate :header-pos))
         (exit-pos (getf candidate :exit-pos))
         (back-pos (getf candidate :back-pos))
         (header-name (getf candidate :header-name))
         (cond-inst (getf candidate :cond-inst))
         (jz-inst (getf candidate :jz-inst))
         (used-labels (make-hash-table :test #'equal))
         (result nil))
    (loop for inst across vec
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) used-labels) t))
    (let* ((body-label-name (%opt-loop-rotation-fresh-label
                             (format nil "~A_body" header-name) used-labels))
           (guard-label-name (%opt-loop-rotation-fresh-label
                              (format nil "~A_guard" header-name) used-labels)))
      (loop for i from 0 below header-pos
            for inst = (aref vec i)
            do (push (if (and (typep inst 'vm-jump)
                              (equal (vm-label-name inst) header-name))
                         (make-vm-jump :label guard-label-name)
                         inst)
                     result))
      (push (make-vm-jump :label guard-label-name) result)
      (push (make-vm-label :name body-label-name) result)
      (loop for i from (+ header-pos 3) below back-pos do (push (aref vec i) result))
      (push (make-vm-label :name guard-label-name) result)
      (push cond-inst result)
      (push jz-inst result)
      (push (make-vm-jump :label body-label-name) result)
      (loop for i from exit-pos below n do (push (aref vec i) result))
      (nreverse result))))

(defun %opt-pass-loop-rotation-linear (instructions)
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

(defun opt-pass-loop-rotation (instructions)
  "Rotate loops using CFG natural-loop detection, falling back to linear matching."
  (let ((candidate (%opt-loop-cfg-candidate instructions)))
    (if candidate
        (%opt-loop-rotation-apply-candidate instructions candidate)
        (%opt-pass-loop-rotation-linear instructions))))

;;; ─── Conservative loop peeling (FR-170 subset) ──────────────────────────

(defun %opt-loop-peeling-apply-candidate (instructions candidate)
  "Apply one-iteration peeling for a CFG-derived CANDIDATE."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-pos (getf candidate :header-pos))
         (back-pos (getf candidate :back-pos))
         (cond-inst (getf candidate :cond-inst))
         (jz-inst (getf candidate :jz-inst))
         (body-insts (loop for i from (+ header-pos 3) below back-pos
                           collect (aref vec i)))
         (result nil))
    (when (some #'vm-label-p body-insts)
      (return-from %opt-loop-peeling-apply-candidate nil))
    (loop for i from 0 below header-pos
          for inst = (aref vec i)
          unless (and (= i (1- header-pos))
                      (typep inst 'vm-jump)
                      (equal (vm-label-name inst) (getf candidate :header-name)))
            do (push inst result))
    (push cond-inst result)
    (push jz-inst result)
    (dolist (b body-insts) (push b result))
    (loop for i from header-pos below n do (push (aref vec i) result))
    (cfg-flatten (cfg-build (nreverse result)))))

(defun %opt-pass-loop-peeling-linear (instructions)
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

(defun opt-pass-loop-peeling (instructions)
  "Peel loops using CFG natural-loop detection, falling back to linear matching."
  (let* ((candidate (%opt-loop-cfg-candidate instructions))
         (peeled (and candidate (%opt-loop-peeling-apply-candidate instructions candidate))))
    (or peeled (%opt-pass-loop-peeling-linear instructions))))

;;; ─── Conservative loop unrolling (FR-021 subset) ────────────────────────
