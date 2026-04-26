;;;; packages/engine/optimize/src/optimizer-copyprop.lisp — Copy Propagation Pass
;;;
;;; Contains:
;;;   - opt-map-tree (shared tree-walker utility)
;;;   - %opt-copy-prop-* helpers (env-copy, env-equal, canonical, build-reverse,
;;;                               add, kill, value-rank, value<, merge,
;;;                               transfer-block, rewrite-inst, rewrite-block)
;;;   - opt-pass-copy-prop — global copy propagation over CFG
;;;
;;; SCCP (sparse conditional constant propagation) is in optimizer-dataflow.lisp
;;; (loads before).
;;;
;;; Load order: after optimizer-dataflow.lisp.
(in-package :cl-cc/optimize)

;;; ─── Tree Walker (shared utility) ────────────────────────────────────────

(defun opt-map-tree (fn tree)
  "Apply FN to every leaf of TREE (a possibly-improper nested cons tree)."
  (if (consp tree)
      (cons (opt-map-tree fn (car tree))
            (opt-map-tree fn (cdr tree)))
      (funcall fn tree)))

;;; ─── Copy Propagation Helpers ────────────────────────────────────────────

(defun %opt-copy-prop-env-copy (env)
  (let ((copy (make-hash-table :test #'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %opt-copy-prop-env-equal-p (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (let ((same t))
         (maphash (lambda (k v)
                    (unless (multiple-value-bind (bv found) (gethash k b)
                              (and found (eq v bv)))
                      (setf same nil)))
                  a)
         same)))

(defun %opt-copy-prop-canonical (reg copies)
  (loop with seen = (make-hash-table :test #'eq)
        for current = reg then next
        for next = (gethash current copies)
        while (and next (not (gethash current seen)))
        do (setf (gethash current seen) t)
        finally (return current)))

(defun %opt-copy-prop-build-reverse (copies)
  (let ((reverse (make-hash-table :test #'eq)))
    (maphash (lambda (dst src)
               (push dst (gethash src reverse)))
             copies)
    reverse))

(defun %opt-copy-prop-add (dst src copies reverse)
  (setf (gethash dst copies) src)
  (push dst (gethash src reverse)))

(defun %opt-copy-prop-kill (reg copies reverse)
  (multiple-value-bind (src found) (gethash reg copies)
    (when found
      (setf (gethash src reverse)
            (delete reg (gethash src reverse) :test #'eq))
      (remhash reg copies)))
  (let ((dependents (copy-list (gethash reg reverse))))
    (dolist (dst dependents)
      (remhash dst copies))
    (remhash reg reverse)))

(defparameter *opt-type-rank-table*
  '((null . 0) (number . 1) (character . 2) (string . 3) (symbol . 4) (cons . 5) (vector . 6))
  "Type → rank mapping for deterministic structural ordering; unlisted types rank as 7.")

(defun %opt-value-rank (value)
  (loop for (type . rank) in *opt-type-rank-table*
        when (typep value type)
        return rank
        finally (return 7)))

(defun %opt-value< (a b)
  "Deterministic structural ordering without printed-string allocation."
  (let ((ra (%opt-value-rank a))
        (rb (%opt-value-rank b)))
    (cond
      ((< ra rb) t)
      ((> ra rb) nil)
      ((null a) nil)
      ((numberp a) (and (/= a b) (< a b)))
      ((characterp a) (< (char-code a) (char-code b)))
      ((stringp a) (string< a b))
      ((symbolp a)
       (let ((apkg (symbol-package a))
             (bpkg (symbol-package b)))
         (cond
           ((and apkg bpkg
                 (not (string= (package-name apkg) (package-name bpkg))))
            (string< (package-name apkg) (package-name bpkg)))
           ((and apkg (null bpkg)) nil)
           ((and (null apkg) bpkg) t)
           (t (string< (symbol-name a) (symbol-name b))))))
      ((consp a)
       (if (equal (car a) (car b))
           (%opt-value< (cdr a) (cdr b))
           (%opt-value< (car a) (car b))))
      ((vectorp a)
       (loop for av across a
             for bv across b
             do (unless (equal av bv)
                  (return (%opt-value< av bv)))
             finally (return (< (length a) (length b)))))
      (t
       (let ((ta (type-of a))
             (tb (type-of b)))
         (cond
           ((not (eq ta tb)) (%opt-value< ta tb))
           ((equal a b) nil)
           (t (< (sxhash a) (sxhash b)))))))))

(defun %opt-copy-prop-merge (envs)
  (cond
    ((null envs) (make-hash-table :test #'eq))
    ((null (cdr envs)) (%opt-copy-prop-env-copy (car envs)))
    (t (let ((merged (%opt-copy-prop-env-copy (car envs))))
         (maphash (lambda (k v)
                    (dolist (env (cdr envs))
                      (multiple-value-bind (ov found) (gethash k env)
                        (unless (and found (eq ov v))
                          (remhash k merged)
                          (return)))))
                  merged)
         merged))))

(defun %opt-copy-prop-transfer-block (block in-env)
  (let ((copies (%opt-copy-prop-env-copy in-env))
        (reverse (%opt-copy-prop-build-reverse in-env)))
    (dolist (inst (bb-instructions block))
      (typecase inst
        (vm-move
         (let* ((dst (vm-move-dst inst))
                 (src (%opt-copy-prop-canonical (vm-move-src inst) copies)))
            (%opt-copy-prop-kill dst copies reverse)
            (unless (eq dst src)
              (%opt-copy-prop-add dst src copies reverse))))
        (t
          (let ((dst (opt-inst-dst inst)))
            (when dst
              (%opt-copy-prop-kill dst copies reverse))))))
    copies))

(defun %opt-copy-prop-rewrite-inst (inst copies)
  (let* ((sexp    (instruction->sexp inst))
         (rewrite (lambda (x) (if (opt-register-keyword-p x) (%opt-copy-prop-canonical x copies) x))))
    (handler-case
        (let* ((has-dst  (not (null (opt-inst-dst inst))))
               (new-sexp (if has-dst
                             (list* (first sexp) (second sexp) (opt-map-tree rewrite (cddr sexp)))
                             (cons (first sexp) (opt-map-tree rewrite (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

(defun %opt-copy-prop-rewrite-block (block in-env)
  (let ((copies (%opt-copy-prop-env-copy in-env))
        (reverse (%opt-copy-prop-build-reverse in-env))
        (result nil))
    (dolist (inst (bb-instructions block))
      (typecase inst
        (vm-move
         (let* ((dst (vm-move-dst inst))
                 (src (%opt-copy-prop-canonical (vm-move-src inst) copies)))
            (%opt-copy-prop-kill dst copies reverse)
            (unless (eq dst src)
              (%opt-copy-prop-add dst src copies reverse)
              (push (if (eq src (vm-move-src inst))
                        inst
                        (make-vm-move :dst dst :src src))
                    result))))
        (t
          (let ((rewritten (%opt-copy-prop-rewrite-inst inst copies))
                (dst (opt-inst-dst inst)))
            (when dst
              (%opt-copy-prop-kill dst copies reverse))
            (push rewritten result)))))
    (nreverse result)))

;;; ─── Copy propagation pass state ────────────────────────────────────────

(defstruct (copyprop-pass-state (:conc-name cpps-))
  "Mutable worklist state for the global copy propagation pass."
  (in-envs  (make-hash-table :test #'eq) :type hash-table)
  (out-envs (make-hash-table :test #'eq) :type hash-table)
  (worklist nil                          :type list)
  (queued   (make-hash-table :test #'eq) :type hash-table))

(defun %copyprop-enqueue (block state)
  "Add BLOCK to the STATE worklist if not already queued."
  (unless (gethash block (cpps-queued state))
    (setf (gethash block (cpps-queued state)) t)
    (push block (cpps-worklist state))))

(defun %copyprop-process-block (block state)
  "Compute the in/out copy environments for BLOCK, enqueue changed successors."
  (let* ((preds (bb-predecessors block))
         (incoming
           (cond
             ((null preds) (make-hash-table :test #'eq))
             ((null (cdr preds))
              (let ((pred-out (gethash (first preds) (cpps-out-envs state))))
                (if pred-out
                    (%opt-copy-prop-env-copy pred-out)
                    (make-hash-table :test #'eq))))
             (t (%opt-copy-prop-merge
                 (mapcar (lambda (pred)
                           (or (gethash pred (cpps-out-envs state))
                               (make-hash-table :test #'eq)))
                         preds)))))
         (old-in  (gethash block (cpps-in-envs state)))
         (changed nil))
    (unless (and old-in (%opt-copy-prop-env-equal-p old-in incoming))
      (setf (gethash block (cpps-in-envs state)) incoming
            changed t))
    (let ((new-out (%opt-copy-prop-transfer-block block incoming))
          (old-out (gethash block (cpps-out-envs state))))
      (unless (and old-out (%opt-copy-prop-env-equal-p old-out new-out))
        (setf (gethash block (cpps-out-envs state)) new-out
              changed t)
        (dolist (succ (bb-successors block))
          (%copyprop-enqueue succ state))))
    changed))

;;; ─── Pass: Copy Propagation ──────────────────────────────────────────────

(defun opt-pass-copy-prop (instructions)
  "Global copy propagation over the CFG using a forward reaching-copy analysis.

   Copy facts are intersected at CFG joins, so only copies that are valid on all
   incoming paths are propagated. The final rewrite phase then substitutes the
   stabilized canonical registers inside each reachable basic block."
  (let ((cfg (cfg-build instructions)))
    (if (null (cfg-entry cfg))
        instructions
        (let ((state (make-copyprop-pass-state :worklist (list (cfg-entry cfg)))))
          (setf (gethash (cfg-entry cfg) (cpps-queued state)) t)
          (loop while (cpps-worklist state)
                for block = (pop (cpps-worklist state))
                do (remhash block (cpps-queued state))
                   (%copyprop-process-block block state))
          (loop for block across (cfg-blocks cfg)
                do (setf (bb-instructions block)
                         (%opt-copy-prop-rewrite-block
                          block
                          (or (gethash block (cpps-in-envs state))
                              (make-hash-table :test #'eq)))))
          (cfg-flatten cfg)))))
