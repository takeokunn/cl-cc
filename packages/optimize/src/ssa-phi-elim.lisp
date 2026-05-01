;;;; ssa-phi-elim.lisp — SSA Trivial Phi Elimination and Destination Rewriting
;;;;
;;;; Post-construction cleanup: collapse trivial phi-nodes, prune dead phis,
;;;; and rewrite instruction destinations for SSA destruction.
;;;;
;;;; Called by ssa-construction.lisp's ssa-destroy after phi placement and renaming.
;;;; Load order: after ssa.lisp, before ssa-construction.lisp.

(in-package :cl-cc/optimize)

;;; ─── SSA phi-elimination helpers ─────────────────────────────────────────

(defun %ssa-resolve-reg (reg replacements)
  "Follow the replacement chain for REG through REPLACEMENTS to its final value."
  (loop for cur = reg then next
        for next = (gethash cur replacements)
        while next
        finally (return cur)))

(defun %ssa-rewrite-tree (tree replacements)
  "Walk TREE, replacing every symbol that has a mapping in REPLACEMENTS."
  (cond
    ((consp tree)
     (cons (%ssa-rewrite-tree (car tree) replacements)
           (%ssa-rewrite-tree (cdr tree) replacements)))
    ((and (symbolp tree) (gethash tree replacements))
     (%ssa-resolve-reg tree replacements))
    (t tree)))

(defun %ssa-rewrite-inst (inst replacements)
  "Return INST with all register references resolved through REPLACEMENTS."
  (let* ((sexp     (instruction->sexp inst))
         (new-sexp (%ssa-rewrite-tree sexp replacements)))
    (if (equal sexp new-sexp) inst (sexp->instruction new-sexp))))

(defun %ssa-rewrite-phi-args (phi replacements)
  "Return new phi argument alist with all source registers resolved."
  (mapcar (lambda (arg)
            (cons (car arg) (%ssa-resolve-reg (cdr arg) replacements)))
          (phi-args phi)))

(defun %ssa-rewrite-phi-map (phi-map replacements)
  "Return a fresh phi-map with all phi arguments resolved through REPLACEMENTS."
  (let ((new-map (make-hash-table :test #'eq)))
    (maphash (lambda (blk phis)
               (setf (gethash blk new-map)
                     (mapcar (lambda (phi)
                               (make-ssa-phi :dst (phi-dst phi)
                                             :args (%ssa-rewrite-phi-args phi replacements)
                                             :reg (phi-reg phi)))
                             phis)))
             phi-map)
    new-map))

(defun %ssa-rewrite-renamed-map (renamed-map replacements)
  "Return a fresh renamed-map with all instruction registers resolved."
  (let ((new-map (make-hash-table :test #'eq)))
    (maphash (lambda (blk insts)
               (setf (gethash blk new-map)
                     (mapcar (lambda (inst) (%ssa-rewrite-inst inst replacements)) insts)))
             renamed-map)
    new-map))

(defun %ssa-collect-uses (phi-map renamed-map)
  "Return a hash-set of all registers read in RENAMED-MAP instructions and phi args."
  (let ((uses (make-hash-table :test #'eq)))
    (maphash (lambda (_blk insts)
               (dolist (inst insts)
                 (dolist (reg (opt-inst-read-regs inst))
                   (when reg (setf (gethash reg uses) t)))))
             renamed-map)
    (maphash (lambda (_blk phis)
               (dolist (phi phis)
                 (dolist (arg (phi-args phi))
                   (when (cdr arg) (setf (gethash (cdr arg) uses) t)))))
             phi-map)
    uses))

(defun ssa-eliminate-trivial-phis (phi-map renamed-map)
  "Collapse trivial phi-nodes and remove dead phi nodes."
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
                                                         (%ssa-resolve-reg (cdr arg) replacements))
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
          (setf phi-map    (%ssa-rewrite-phi-map phi-map replacements)
                renamed-map (%ssa-rewrite-renamed-map renamed-map replacements)
                changed t))
        (let* ((uses (%ssa-collect-uses phi-map renamed-map))
               (pruned-map (make-hash-table :test #'eq))
               (pruned nil))
          (maphash (lambda (blk phis)
                     (let ((kept (remove-if (lambda (phi)
                                              (not (gethash (phi-dst phi) uses)))
                                            phis)))
                       (when (not (equal kept phis))
                         (setf pruned t changed t))
                       (when kept
                         (setf (gethash blk pruned-map) kept))))
                   phi-map)
          (when pruned
            (setf phi-map pruned-map)))))
  (values phi-map renamed-map))

(defun ssa-rewrite-dst (inst old-dst new-dst)
  "Return INST with its destination register changed from OLD-DST to NEW-DST.
   Uses sexp roundtrip when the dst appears in the serialized form."
  (handler-case
      (let* ((sexp     (instruction->sexp inst))
             (new-sexp (opt-map-tree (lambda (x) (if (eq x old-dst) new-dst x)) sexp)))
        (if (equal sexp new-sexp)
            inst
            (sexp->instruction new-sexp)))
    (error () inst)))

;;; (ssa-construct, ssa-destroy, ssa-sequentialize-copies, ssa-round-trip
;;;  are in ssa-construction.lisp which loads after this file.)
