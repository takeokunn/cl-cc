;;;; packages/optimize/src/ssa-construction.lisp — SSA Construction and Destruction Entry Points
;;;;
;;;; Contains:
;;;;   ssa-construct — build SSA form from flat VM instructions
;;;;   ssa-destroy — deconstruct SSA back to VM instructions
;;;;   ssa-sequentialize-copies — parallel copy sequentialization
;;;;   ssa-round-trip — round-trip utility
;;;;
;;;; Data structures (ssa-rename-state, ssa-phi), phi placement (ssa-place-phis),
;;;; renaming (ssa-rename), and elimination are in ssa.lisp (loads before).
;;;;
;;;; Load order: after ssa.lisp.

(in-package :cl-cc/optimize)

(defun ssa-construct (instructions)
  "Construct SSA form from a flat VM INSTRUCTIONS list.
   Returns (values cfg phi-map renamed-map) where:
     cfg         — the CFG with dominator information
     phi-map     — hash-table block → list of ssa-phi
     renamed-map — hash-table block → list of renamed vm-instructions"
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-dominance-frontiers cfg)
    (let ((phi-map (ssa-place-phis cfg)))
      (setf phi-map (ssa-place-lcssa-phis cfg phi-map))
      (multiple-value-bind (renamed phi-map)
          (ssa-rename cfg phi-map)
        (multiple-value-bind (phi-map renamed)
            (ssa-eliminate-trivial-phis phi-map renamed)
          (values cfg phi-map renamed))))))

;;; ─── SSA Destruction ─────────────────────────────────────────────────────
;;;
;;; Converts SSA form back to a conventional flat instruction list by:
;;;   1. Replacing phi-nodes with copy instructions in predecessor blocks
;;;   2. Sequentializing parallel copies (handling swap cycles via temps)
;;;
;;; The result should be semantically equivalent to the original instructions
;;; under any correct SSA construction (used for round-trip testing).

(defun ssa-destroy (cfg phi-map renamed-map)
  "Destroy SSA form: replace phi-nodes with parallel copies in predecessors.
   Returns a flat instruction list in RPO order.
   Uses the same-RPO ordering for deterministic output."
  (let ((copies-to-insert (make-hash-table :test #'eq)) ; pred → succ → list of (dst . src)
        (edge-pads nil))

    (labels ((edge-copies-table (pred)
               (or (gethash pred copies-to-insert)
                   (setf (gethash pred copies-to-insert)
                         (make-hash-table :test #'eq))))
             (add-edge-copy (pred succ dst src)
               (push (cons dst src) (gethash succ (edge-copies-table pred))))
             (edge-copies (pred succ)
               (let ((succ-table (gethash pred copies-to-insert)))
                 (and succ-table (gethash succ succ-table))))
             (final-terminator (block-insts)
               (and block-insts
                    (let ((last-inst (car (last block-insts))))
                      (and (typep last-inst '(or vm-jump vm-jump-zero vm-ret vm-halt))
                           last-inst))))
             (branch-target-block (term)
               (and (typep term 'vm-jump-zero)
                    (cfg-get-block-by-label cfg (vm-label-name term))))
             (fallthrough-block (block target)
               (find-if (lambda (succ) (not (eq succ target)))
                        (bb-successors block)))
             (fresh-edge-label-name (pred succ)
               (format nil "SSA_EDGE_~D_~D_~D"
                       (bb-id pred) (bb-id succ) (length edge-pads)))
             (emit-copies (copies result)
               (dolist (copy (ssa-sequentialize-copies copies) result)
                 (push copy result)))
             (make-target-pad (pred succ copies)
               (let ((pad-name (fresh-edge-label-name pred succ))
                     (target-label (bb-label succ)))
                 (push (list pad-name copies (vm-name target-label)) edge-pads)
                 pad-name)))

    ;; Step 1: for each phi-node, schedule copies in predecessor blocks
      (loop for b across (cfg-blocks cfg)
            do (dolist (phi (gethash b phi-map))
                 (dolist (arg (phi-args phi))
                   (let ((pred (car arg))
                         (src  (cdr arg))
                         (dst  (phi-dst phi)))
                     (add-edge-copy pred b dst src)))))

    ;; Step 2: emit flat instruction list in RPO order
      (let ((rpo (cfg-compute-rpo cfg))
            (result nil))
        (dolist (b rpo)
          ;; Emit block label
          (when (bb-label b)
            (push (bb-label b) result))
          (let* ((block-insts (gethash b renamed-map))
                 (terminator  (final-terminator block-insts))
                 (prefix      (if terminator (butlast block-insts) block-insts)))
            (dolist (inst prefix)
              (push inst result))
            (cond
              ((typep terminator 'vm-jump)
               (let* ((target (cfg-get-block-by-label cfg (vm-label-name terminator)))
                      (copies (and target (edge-copies b target))))
                 (when copies
                   (setf result (emit-copies copies result)))
                 (push terminator result)))
              ((typep terminator 'vm-jump-zero)
               (let* ((target (branch-target-block terminator))
                      (fallthrough (fallthrough-block b target))
                      (target-copies (and target (edge-copies b target)))
                      (fallthrough-copies (and fallthrough (edge-copies b fallthrough)))
                      (branch-inst terminator))
                 (when target-copies
                   (setf branch-inst
                         (make-vm-jump-zero
                          :reg (vm-reg terminator)
                          :label (make-target-pad b target target-copies))))
                 (push branch-inst result)
                 (when fallthrough-copies
                   (setf result (emit-copies fallthrough-copies result)))))
              ((typep terminator '(or vm-ret vm-halt))
               (push terminator result))
              (t
               (let ((successor (first (bb-successors b))))
                 (when successor
                   (let ((copies (edge-copies b successor)))
                     (when copies
                       (setf result (emit-copies copies result))))))))))
        (dolist (pad (nreverse edge-pads))
          (destructuring-bind (pad-name copies target-name) pad
            (push (make-vm-label :name pad-name) result)
            (setf result (emit-copies copies result))
            (push (make-vm-jump :label target-name) result)))
        (nreverse result)))))

(defun ssa-sequentialize-copies (parallel-copies)
  "Convert a list of parallel copies (dst . src) to a sequential list of
   vm-move instructions that produces the same effect.

   Handles the swap problem: if A←B and B←A appear simultaneously, we use
   a temporary register to break the cycle.

   Algorithm: topological sort of the copy graph; cycles require a temp."
  (when (null parallel-copies) (return-from ssa-sequentialize-copies nil))

  (let* ((copies   (copy-list parallel-copies)) ; mutable working set
         (result   nil)
         ;; Build: src → dst (reverse map for cycle detection)
         (src->dst (make-hash-table :test #'eq)))

    (dolist (c copies)
      (setf (gethash (cdr c) src->dst) (car c)))

    ;; Emit copies that are ready (their dst is not also a src in any copy)
    (let ((ready-q (loop for c in copies
                         unless (gethash (car c) src->dst)
                         collect c)))
      (loop while (or ready-q copies)
            do (loop while ready-q
                     do (let ((c (pop ready-q)))
                          (push (make-vm-move :dst (car c) :src (cdr c)) result)
                          (setq copies (remove c copies :test #'equal))
                          ;; Check if this dst was blocking another copy
                          (let ((unblocked (find-if (lambda (cc)
                                                      (eq (cdr cc) (car c)))
                                                    copies)))
                            (when unblocked
                              (push unblocked ready-q)))))
               ;; If no ready copies remain but copies exist: we have a cycle
               (when (and (null ready-q) copies)
                 ;; Break cycle: use a fresh temp for the first copy's dst
                 (let* ((c    (car copies))
                        (temp (gensym "SSATMP"))
                        (tmp-kw (intern (symbol-name temp) :keyword)))
                   (push (make-vm-move :dst tmp-kw :src (cdr c)) result)
                   ;; Replace all uses of (cdr c) as a src in copies
                   (setq copies
                         (mapcar (lambda (cc)
                                   (if (eq (cdr cc) (cdr c))
                                       (cons (car cc) tmp-kw)
                                       cc))
                                 copies))
                   ;; Now (cdr c) is free: put in ready-q with its original dst
                   (push (cons (car c) tmp-kw) ready-q)
                   (setq copies (cdr copies))))))

    (nreverse result)))

;;; ─── Round-Trip Utility ──────────────────────────────────────────────────

(defun ssa-round-trip (instructions)
  "Construct and immediately destruct SSA form.
   Returns a flat instruction list that should be semantically equivalent
   to the input.  Used for integration testing."
  (multiple-value-bind (cfg phi-map renamed)
      (ssa-construct instructions)
    (ssa-destroy cfg phi-map renamed)))
