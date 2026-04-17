(in-package :cl-cc/optimize)

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


;;; (ssa-construct, ssa-destroy, ssa-sequentialize-copies, ssa-round-trip
;;;  are in ssa-construction.lisp which loads after this file.)
