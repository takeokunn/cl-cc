(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CFG — Flat Instruction Layout and Block Ordering
;;;
;;; Contains: cfg-flatten (RPO order), cfg-flatten-hot-cold (branch-aware
;;; hot/cold layout), opt-pass-hot-cold-layout, cfg-block-count.
;;;
;;; CFG construction, dominator tree, post-dominators, critical-edge
;;; splitting, dominance frontiers, and IDF are in cfg.lisp (loads before).
;;;
;;; Load order: after cfg.lisp, before optimizer-strength.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── CFG → Flat Instruction List ─────────────────────────────────────────

(defun cfg-flatten (cfg)
  "Emit a flat instruction list from the CFG (for round-trip testing).
   Blocks are emitted in RPO order.  Each block's opening label (if any)
   is prepended to the block's instruction list."
  (let ((rpo (cfg-compute-rpo cfg))
        (result nil))
    (dolist (b rpo)
      (when (bb-label b)
        (push (bb-label b) result))
      (dolist (inst (bb-instructions b))
        (push inst result)))
    (nreverse result)))

(defparameter *cfg-cold-inst-types*
  '(vm-signal vm-error-instruction vm-cerror vm-warn
    vm-signal-error vm-establish-handler vm-remove-handler
    vm-establish-catch vm-throw)
  "Instruction types that mark a basic block as a cold (exception/handler) path.")

(defun %cfg-block-cold-p (block)
  "Return T when BLOCK contains any cold-path instruction."
  (some (lambda (inst) (member (type-of inst) *cfg-cold-inst-types* :test #'eq))
        (bb-instructions block)))

(defun %cfg-block-terminator (block)
  "Return BLOCK's final control-flow instruction, or NIL."
  (let ((last-inst (car (last (bb-instructions block)))))
    (and (typep last-inst '(or vm-jump vm-jump-zero vm-ret vm-halt))
         last-inst)))

(defun %cfg-jump-target-block (cfg inst)
  "Return INST's explicit jump target block, if INST names one."
  (and (typep inst '(or vm-jump vm-jump-zero))
       (cfg-get-block-by-label cfg (vm-label-name inst))))

(defun %cfg-conditional-fallthrough-successor (cfg block)
  "Return BLOCK's fall-through successor when it ends in vm-jump-zero.

For vm-jump-zero, the explicit label is the cold/taken edge and the other
successor is the hot fall-through edge inherited from the original instruction
stream."
  (let* ((term   (%cfg-block-terminator block))
         (target (and (typep term 'vm-jump-zero)
                      (%cfg-jump-target-block cfg term))))
    (when target
      (find-if (lambda (succ) (not (eq succ target)))
               (bb-successors block)))))

(defun %cfg-implicit-fallthrough-successor (block)
  "Return BLOCK's implicit fall-through successor for non-branch blocks."
  (let ((term (%cfg-block-terminator block)))
    (when (and (null term) (= (length (bb-successors block)) 1))
      (first (bb-successors block)))))

(defun %cfg-layout-successor-hotter-p (a b)
  "Stable layout ordering predicate for non-primary successors."
  (cond
    ((and (%cfg-block-cold-p a) (not (%cfg-block-cold-p b))) nil)
    ((and (%cfg-block-cold-p b) (not (%cfg-block-cold-p a))) t)
    ((> (bb-loop-depth a) (bb-loop-depth b)) t)
    ((< (bb-loop-depth a) (bb-loop-depth b)) nil)
    (t (< (bb-rpo-index a) (bb-rpo-index b)))))

(defun %cfg-block-hotter-p (a b)
  "Return T when block A is hotter than B (loop-depth + RPO tie-break)."
  (cond
    ((and (%cfg-block-cold-p a) (not (%cfg-block-cold-p b))) nil)
    ((and (%cfg-block-cold-p b) (not (%cfg-block-cold-p a))) t)
    ((> (bb-loop-depth a) (bb-loop-depth b)) t)
    ((< (bb-loop-depth a) (bb-loop-depth b)) nil)
    (t (< (bb-rpo-index a) (bb-rpo-index b)))))

(defun %cfg-emit-block (block result)
  "Prepend BLOCK's label and instructions to RESULT and return the new list."
  (when (bb-label block)
    (push (bb-label block) result))
  (dolist (inst (bb-instructions block) result)
    (push inst result)))

(defun %cfg-layout-order (cfg)
  "Return basic blocks in hot/cold layout order.

The primary successor of a vm-jump-zero block is its fall-through edge, so that
the likely non-zero path is emitted contiguously after the conditional branch.
Explicit jump targets and blocks containing condition/error instructions are
deferred to the cold tail when possible."
  (let* ((rpo       (cfg-compute-rpo cfg))
         (entry     (cfg-entry cfg))
         (reachable (make-hash-table :test #'eq))
         (visited   (make-hash-table :test #'eq))
         (cold-tail nil)
         (ordered   nil))
    (dolist (block rpo)
      (setf (gethash block reachable) t))
    (labels ((defer-cold-p (block force)
               (and block (not force) (not (eq block entry)) (%cfg-block-cold-p block)))
             (successors-after (block primary)
               (stable-sort (remove primary (copy-list (bb-successors block)) :test #'eq)
                            #'%cfg-layout-successor-hotter-p))
             (visit (block &optional force)
               (when (and block (not (gethash block visited)))
                 (if (defer-cold-p block force)
                     (pushnew block cold-tail :test #'eq)
                     (progn
                       (setf (gethash block visited) t)
                       (push block ordered)
                       (let ((primary (or (%cfg-conditional-fallthrough-successor cfg block)
                                          (%cfg-implicit-fallthrough-successor block))))
                         (when primary
                           (visit primary nil))
                         (dolist (succ (successors-after block primary))
                           (visit succ nil))))))))
      (visit entry t)
      (dolist (block rpo)
        (visit block nil))
      (dolist (block (stable-sort (copy-list cold-tail) #'%cfg-layout-successor-hotter-p))
        (visit block t))
      (loop for block across (cfg-blocks cfg)
            unless (gethash block reachable)
              do (visit block t)))
    (nreverse ordered)))

(defun %cfg-layout-append-required-jump (cfg block next-block)
  "Append an explicit jump when reordering would otherwise break fall-through."
  (let ((fallthrough (%cfg-implicit-fallthrough-successor block)))
    (when (and fallthrough (not (eq fallthrough next-block)))
      (let ((label (%cfg-ensure-label fallthrough cfg "LAYOUT_TARGET_")))
        (setf (bb-instructions block)
              (append (bb-instructions block)
                      (list (make-vm-jump :label (vm-name label)))))))))

(defun %cfg-layout-prepare-blocks (cfg ordered)
  "Patch implicit fall-through edges after block reordering."
  (loop for (block . rest) on ordered
        do (%cfg-layout-append-required-jump cfg block (first rest)))
  ordered)

(defun cfg-flatten-layout-order (cfg ordered)
  "Emit CFG using ORDERED basic blocks, preserving labels and instructions."
  (let ((result nil))
    (dolist (block ordered)
      (setf result (%cfg-emit-block block result)))
    (nreverse result)))

(defun cfg-flatten-hot-cold (cfg)
  "Emit a branch-aware hot/cold flat instruction list from CFG.

For vm-jump-zero, the fall-through successor is treated as the hot path and is
placed immediately after the conditional block.  Explicit jump targets and
condition/error blocks are placed later, with cold blocks deferred to the end of
the function.  Any non-branch fall-through edge broken by the new order is made
explicit with a vm-jump so label references and control flow remain valid."
  (cfg-flatten-layout-order cfg (%cfg-layout-prepare-blocks cfg (%cfg-layout-order cfg))))

(defun opt-pass-hot-cold-layout (instructions)
  "Reorder basic blocks for I-cache locality using a hot/cold CFG layout.

This is a layout-only optimization: it preserves instruction objects and label
targets, builds a CFG, emits vm-jump-zero fall-through paths contiguously, and
moves cold error/signalling blocks to the function tail."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (cfg-compute-loop-depths cfg))
    (cfg-flatten-hot-cold cfg)))

;;; ─── Accessors / Utilities ───────────────────────────────────────────────

(defun cfg-block-count (cfg)
  "Return the number of basic blocks in CFG."
  (length (cfg-blocks cfg)))
