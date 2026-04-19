;;;; ssa-phi-elim.lisp — SSA Trivial Phi Elimination and Destination Rewriting
;;;;
;;;; Post-construction cleanup: collapse trivial phi-nodes, prune dead phis,
;;;; and rewrite instruction destinations for SSA destruction.
;;;;
;;;; Called by ssa-construction.lisp's ssa-destroy after phi placement and renaming.
;;;; Load order: after ssa.lisp, before ssa-construction.lisp.

(in-package :cl-cc/optimize)

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
