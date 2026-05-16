(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CFG — Flat Instruction Layout and Block Ordering
;;;
;;; Contains: cfg-flatten (RPO order), cfg-flatten-hot-cold (loop-depth
;;; heuristic with cold-path markers), cfg-block-count.
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

(defun cfg-flatten-hot-cold (cfg)
  "Emit a flat instruction list using loop depth as a hot/cold heuristic.

   Reachable blocks keep the entry block first, then place non-cold blocks in
   descending loop-depth order so loop bodies are contiguous.  Blocks containing
   condition/error-handler instructions are placed last, preserving RPO within
   equal hotness bands."
  (let* ((rpo     (cfg-compute-rpo cfg))
         (entry   (cfg-entry cfg))
         (rest    (remove entry rpo :test #'eq))
         (hot     (remove-if #'%cfg-block-cold-p rest))
         (cold    (remove-if-not #'%cfg-block-cold-p rest))
         (ordered (append (and entry (list entry))
                          (stable-sort (copy-list hot) #'%cfg-block-hotter-p)
                          (stable-sort (copy-list cold) #'%cfg-block-hotter-p)))
         (result  nil))
    (dolist (b ordered)
      (setf result (%cfg-emit-block b result)))
    (nreverse result)))

;;; ─── Accessors / Utilities ───────────────────────────────────────────────

(defun cfg-block-count (cfg)
  "Return the number of basic blocks in CFG."
  (length (cfg-blocks cfg)))
