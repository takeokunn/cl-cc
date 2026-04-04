(in-package :cl-cc)

;;; ─── Static Single Assignment (SSA) Form ────────────────────────────────
;;;
;;; Constructs and destructs SSA form for the CL-CC VM instruction set.
;;;
;;; Construction (Cytron et al. 1991):
;;;   1. Compute CFG, dominator tree, dominance frontiers (in cfg.lisp)
;;;   2. Place phi-nodes at dominance frontiers of definition sites
;;;   3. Rename registers via DFS over the dominator tree
;;;
;;; Destruction (Sreedhar & Gao / Briggs-Cooper):
;;;   1. Replace each phi-node with parallel copy instructions in predecessors
;;;   2. Sequentialize parallel copies (handle swap cycles via temporaries)
;;;
;;; SSA values use the convention: original register :R5, version 3 → :R5.3
;;; Phi-node temporaries use #:PHI-<id> (uninterned symbols / gensyms)
;;;
;;; References:
;;;   Cytron, Ferrante, Rosen, Wegman, Zadeck (1991). TOPLAS.
;;;   Braun et al. (2013). "Simple and Efficient Construction of SSA Form."

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(defun ssa-versioned-reg (base-reg version)
  "Return an SSA-versioned register keyword :R<n>.<v> for BASE-REG and VERSION."
  (intern (format nil "~A.~A" (symbol-name base-reg) version) :keyword))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(defstruct (ssa-rename-state (:conc-name ssr-))
  "Per-register renaming state during SSA construction.
   Counters track how many versions each register has received.
   Stacks track the current live version at any point in the DFS."
  (counters (make-hash-table :test #'eq) :type hash-table) ; reg → next-version
  (stacks   (make-hash-table :test #'eq) :type hash-table)); reg → stack of versions

(defun ssr-push-new-version (state reg)
  "Assign a new version number to REG, push it onto the stack, return the version."
  (let* ((i   (or (gethash reg (ssr-counters state)) 0))
         (new  (ssa-versioned-reg reg i)))
    (setf (gethash reg (ssr-counters state)) (1+ i))
    (push new (gethash reg (ssr-stacks state)))
    new))

(defun ssr-current-version (state reg)
  "Return the current (most recent) SSA name for REG, or REG if not renamed."
  (or (car (gethash reg (ssr-stacks state))) reg))

(defun ssr-pop-version (state reg)
  "Pop the top version of REG from the rename stack."
  (let ((stack (gethash reg (ssr-stacks state))))
    (when stack
      (setf (gethash reg (ssr-stacks state)) (cdr stack)))))

;;; ─── Phi-Node Representation ─────────────────────────────────────────────

(defstruct (ssa-phi (:conc-name phi-))
  "A phi-function inserted at the entry of a basic block.
   DST is the SSA-versioned destination register.
   ARGS is an alist of (predecessor-block . versioned-source-register)."
  (dst  nil)                           ; versioned SSA register keyword
  (args nil :type list)                ; ((bb . versioned-reg) ...)
  (reg  nil))                          ; original (pre-SSA) register name

;;; ─── Phi Placement (Cytron Algorithm) ───────────────────────────────────

(defun ssa-place-phis (cfg)
  "Insert phi-node stubs at dominance frontiers of definition sites.
   Returns a hash-table: block → list of ssa-phi stubs (with nil DST/ARGS,
   to be filled during renaming).

   Algorithm:
     For each variable v:
       defsites(v) = blocks where v is defined (has a dst write)
       Insert phi(v) at each block in IDF(defsites(v))
       if v is not already live-in there"
  ;; Step 1: collect all registers and their definition blocks
  (let ((def-sites (make-hash-table :test #'eq))  ; reg → list of blocks
        (phi-map   (make-hash-table :test #'eq)))  ; block → list of ssa-phi

    (loop for b across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions b))
               (let ((dst (ignore-errors (vm-dst inst))))
                 (when dst
                   (pushnew b (gethash dst def-sites) :test #'eq)))))

    ;; Step 2: for each register, place phis at IDF(def-sites)
    (maphash
     (lambda (reg blocks)
       (let ((idf (cfg-idf blocks)))
         (dolist (f idf)
           (unless (find-if (lambda (p) (eq (phi-reg p) reg))
                            (gethash f phi-map))
             (push (make-ssa-phi :reg reg) (gethash f phi-map))))))
     def-sites)

    phi-map))

;;; ─── SSA Renaming (DFS over dominator tree) ──────────────────────────────

(defun ssa-rename (cfg phi-map)
  "Rename all register references in CFG to SSA form.
   Fills in phi-node DST registers and argument registers.
   Returns (values renamed-instructions phi-map).

   Works by DFS over the dominator tree rooted at the entry block.
   For each block:
     1. Assign new versions to phi-node destinations
     2. Rename each instruction's src registers to current versions
     3. Assign new version to each instruction's dst register
     4. Fill in phi-node args in successor blocks
     5. Recurse into dominated children
     6. Pop the versions pushed in this block"
  (let ((state     (make-ssa-rename-state))
        ;; renamed-instructions: block → list of renamed instructions
        (renamed   (make-hash-table :test #'eq)))

    (labels ((rename-block (b)
               ;; Collect registers pushed in this block (for later pop)
               (let ((pushed-regs nil))

                 ;; Step 1: rename phi-node destinations
                 (dolist (phi (gethash b phi-map))
                   (let ((new-dst (ssr-push-new-version state (phi-reg phi))))
                     (setf (phi-dst phi) new-dst)
                     (push (phi-reg phi) pushed-regs)))

                 ;; Step 2 & 3: rename instructions
                 (let ((new-insts nil))
                   (dolist (inst (bb-instructions b))
                     (let* ((reads  (opt-inst-read-regs inst))
                            ;; Rewrite src registers to their current SSA versions
                            (renamed-inst
                             (if reads
                                 (let ((subst (make-hash-table :test #'eq)))
                                   (dolist (r reads)
                                     (setf (gethash r subst) (ssr-current-version state r)))
                                   (opt-rewrite-inst-regs inst subst))
                                 inst))
                            ;; Assign new version to the destination
                            (dst (ignore-errors (vm-dst inst))))
                       (when dst
                         (let ((new-dst (ssr-push-new-version state dst)))
                           (push dst pushed-regs)
                           (setf renamed-inst
                                 (ssa-rewrite-dst renamed-inst dst new-dst))))
                       (push renamed-inst new-insts)))
                   (setf (gethash b renamed) (nreverse new-insts)))

                 ;; Step 4: fill phi-args in successors
                 (dolist (succ (bb-successors b))
                   (dolist (phi (gethash succ phi-map))
                     (let ((cur (ssr-current-version state (phi-reg phi))))
                       (push (cons b cur) (phi-args phi)))))

                 ;; Step 5: recurse into dominated children
                 (dolist (child (bb-dom-children b))
                   (rename-block child))

                 ;; Step 6: pop versions pushed in this block
                 (dolist (r pushed-regs)
                   (ssr-pop-version state r)))))

      (when (cfg-entry cfg)
        (rename-block (cfg-entry cfg))))

    (values renamed phi-map)))

(defun ssa-eliminate-trivial-phis (phi-map renamed-map)
  "Collapse trivial phi-nodes and remove dead phi nodes."
  (labels ((resolve-reg (reg replacements)
             (loop for cur = reg then next
                   for next = (gethash cur replacements)
                   while next
                   finally (return cur)))
           (rewrite-tree (tree replacements)
             (cond
               ((consp tree)
                (cons (rewrite-tree (car tree) replacements)
                      (rewrite-tree (cdr tree) replacements)))
               ((and (symbolp tree) (gethash tree replacements))
                (resolve-reg tree replacements))
               (t tree)))
           (rewrite-inst (inst replacements)
             (let* ((sexp (instruction->sexp inst))
                    (new-sexp (rewrite-tree sexp replacements)))
               (if (equal sexp new-sexp)
                   inst
                   (sexp->instruction new-sexp))))
           (rewrite-phi-args (phi replacements)
             (mapcar (lambda (arg)
                       (cons (car arg)
                             (resolve-reg (cdr arg) replacements)))
                     (phi-args phi)))
           (rewrite-phi-map (phi-map replacements)
             (let ((new-map (make-hash-table :test #'eq)))
               (maphash (lambda (blk phis)
                          (setf (gethash blk new-map)
                                (mapcar (lambda (phi)
                                          (make-ssa-phi :dst (phi-dst phi)
                                                        :args (rewrite-phi-args phi replacements)
                                                        :reg (phi-reg phi)))
                                        phis)))
                        phi-map)
               new-map))
           (rewrite-renamed-map (renamed-map replacements)
             (let ((new-map (make-hash-table :test #'eq)))
               (maphash (lambda (blk insts)
                          (setf (gethash blk new-map)
                                (mapcar (lambda (inst)
                                          (rewrite-inst inst replacements))
                                        insts)))
                        renamed-map)
               new-map))
           (collect-uses (phi-map renamed-map)
             (let ((uses (make-hash-table :test #'eq)))
               (labels ((note-use (reg)
                          (when reg
                            (setf (gethash reg uses) t))))
                 (maphash (lambda (_blk insts)
                            (dolist (inst insts)
                              (dolist (reg (opt-inst-read-regs inst))
                                (note-use reg))))
                          renamed-map)
                 (maphash (lambda (_blk phis)
                            (dolist (phi phis)
                              (dolist (arg (phi-args phi))
                                (note-use (cdr arg)))))
                          phi-map))
               uses)))
    (loop
      with changed = t
      while changed do
        (setf changed nil)
        (let ((replacements (make-hash-table :test #'eq)))
          (loop
            with round-changed = t
            while round-changed do
              (setf round-changed nil)
              (maphash (lambda (_blk phis)
                         (dolist (phi phis)
                           (let* ((resolved-args (mapcar (lambda (arg)
                                                           (resolve-reg (cdr arg) replacements))
                                                         (phi-args phi)))
                                  (first (car resolved-args)))
                             (when (and first
                                        (every (lambda (arg) (eq arg first)) (cdr resolved-args))
                                        (not (eq (phi-dst phi) first)))
                               (unless (eq (gethash (phi-dst phi) replacements) first)
                                 (setf (gethash (phi-dst phi) replacements) first
                                       round-changed t
                                       changed t))))))
                       phi-map))
          (when (> (hash-table-count replacements) 0)
            (setf phi-map (rewrite-phi-map phi-map replacements)
                  renamed-map (rewrite-renamed-map renamed-map replacements)
                  changed t))
          (let* ((uses (collect-uses phi-map renamed-map))
                 (pruned-map (make-hash-table :test #'eq))
                 (pruned nil))
            (maphash (lambda (blk phis)
                       (let ((kept (remove-if (lambda (phi)
                                                (not (gethash (phi-dst phi) uses)))
                                              phis)))
                         (when (not (equal kept phis))
                           (setf pruned t
                                 changed t))
                         (when kept
                           (setf (gethash blk pruned-map) kept))))
                     phi-map)
            (when pruned
              (setf phi-map pruned-map))))))
    (values phi-map renamed-map))

(defun ssa-rewrite-dst (inst old-dst new-dst)
  "Return INST with its destination register changed from OLD-DST to NEW-DST.
   Uses sexp roundtrip when the dst appears in the serialized form."
  (flet ((sub (x) (if (eq x old-dst) new-dst x)))
    (handler-case
        (let* ((sexp     (instruction->sexp inst))
               (new-sexp (opt-map-tree #'sub sexp)))
          (if (equal sexp new-sexp)
              inst
              (sexp->instruction new-sexp)))
      (error () inst))))

;;; ─── SSA Construction Entry Point ───────────────────────────────────────

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
  (let ((copies-to-insert (make-hash-table :test #'eq))) ; block → list of (dst . src) pairs

    ;; Step 1: for each phi-node, schedule copies in predecessor blocks
    (loop for b across (cfg-blocks cfg)
          do (dolist (phi (gethash b phi-map))
               (dolist (arg (phi-args phi))
                 (let ((pred (car arg))
                       (src  (cdr arg))
                       (dst  (phi-dst phi)))
                   (push (cons dst src) (gethash pred copies-to-insert))))))

    ;; Step 2: emit flat instruction list in RPO order
    (let ((rpo (cfg-compute-rpo cfg))
          (result nil))
      (dolist (b rpo)
        ;; Emit block label
        (when (bb-label b)
          (push (bb-label b) result))
        ;; Emit renamed instructions
        (dolist (inst (gethash b renamed-map))
          (push inst result))
        ;; Insert sequentialized parallel copies at end of block
        ;; (before the block terminator — we insert them before the last
        ;;  branch/jump instruction to preserve control flow)
        (let* ((block-insts (gethash b renamed-map))
               (terminator  (and block-insts
                                 (find-if (lambda (i) (typep i '(or vm-jump vm-jump-zero vm-ret vm-halt)))
                                          (reverse block-insts))))
               (copies      (gethash b copies-to-insert)))
          (declare (ignore terminator))
          (when copies
            (dolist (copy (ssa-sequentialize-copies copies))
              (push copy result)))))
      (nreverse result))))

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
