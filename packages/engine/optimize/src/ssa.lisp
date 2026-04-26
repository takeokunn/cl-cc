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

(defun %ssa-rename-block (b state phi-map renamed)
  "DFS rename pass for one block B, mutating STATE, PHI-MAP, and RENAMED."
  (let ((pushed-regs nil))
    (dolist (phi (gethash b phi-map))
      (let ((new-dst (ssr-push-new-version state (phi-reg phi))))
        (setf (phi-dst phi) new-dst)
        (push (phi-reg phi) pushed-regs)))
    (let ((new-insts nil))
      (dolist (inst (bb-instructions b))
        (let* ((reads (opt-inst-read-regs inst))
               (renamed-inst
                (if reads
                    (let ((subst (make-hash-table :test #'eq)))
                      (dolist (r reads)
                        (setf (gethash r subst) (ssr-current-version state r)))
                      (opt-rewrite-inst-regs inst subst))
                    inst))
               (dst (ignore-errors (vm-dst inst))))
          (when dst
            (let ((new-dst (ssr-push-new-version state dst)))
              (push dst pushed-regs)
              (setf renamed-inst (ssa-rewrite-dst renamed-inst dst new-dst))))
          (push renamed-inst new-insts)))
      (setf (gethash b renamed) (nreverse new-insts)))
    (dolist (succ (bb-successors b))
      (dolist (phi (gethash succ phi-map))
        (push (cons b (ssr-current-version state (phi-reg phi))) (phi-args phi))))
    (dolist (child (bb-dom-children b))
      (%ssa-rename-block child state phi-map renamed))
    (dolist (r pushed-regs)
      (ssr-pop-version state r))))

(defun ssa-rename (cfg phi-map)
  "Rename all register references in CFG to SSA form.
   Fills in phi-node DST registers and argument registers.
   Returns (values renamed-instructions phi-map)."
  (let ((state   (make-ssa-rename-state))
        (renamed (make-hash-table :test #'eq)))
    (when (cfg-entry cfg)
      (%ssa-rename-block (cfg-entry cfg) state phi-map renamed))
    (values renamed phi-map)))

;;; Trivial phi elimination (ssa-eliminate-trivial-phis) and ssa-rewrite-dst are in ssa-phi-elim.lisp.
