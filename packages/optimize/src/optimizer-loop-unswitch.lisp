;;;; optimizer-loop-unswitch.lisp — FR-602 loop unswitching

(in-package :cl-cc/optimize)

(defvar *opt-enable-loop-unswitch* nil
  "When true, enable FR-602 loop unswitching in the default pipeline.")

(defun %loop-unswitch-label-table (instructions)
  "Return a hash table of label names used in INSTRUCTIONS."
  (let ((used (make-hash-table :test #'equal)))
    (dolist (inst instructions used)
      (when (vm-label-p inst)
        (setf (gethash (vm-name inst) used) t)))))

(defun %loop-unswitch-fresh (base used)
  "Return a fresh label based on BASE and record it in USED."
  (let ((name (%opt-loop-rotation-fresh-label base used)))
    (setf (gethash name used) t)
    name))

(defun %loop-unswitch-rewrite-atom (atom rename exit-name)
  (cond
    ((and (stringp atom) (gethash atom rename))
     (gethash atom rename))
    ((and (stringp atom) (equal atom exit-name))
     exit-name)
    (t atom)))

(defun %loop-unswitch-rewrite-sexp (sexp rename exit-name)
  "Rewrite label names in SEXP according to RENAME, preserving EXIT-NAME."
  (cond
    ((atom sexp) (%loop-unswitch-rewrite-atom sexp rename exit-name))
    (t (mapcar (lambda (x) (%loop-unswitch-rewrite-sexp x rename exit-name)) sexp))))

(defun %loop-unswitch-copy-inst (inst rename exit-name)
  "Copy INST while renaming internal loop labels."
  (handler-case
      (sexp->instruction
       (%loop-unswitch-rewrite-sexp (instruction->sexp inst) rename exit-name))
    (error () inst)))

(defun %loop-unswitch-dst-set (insts)
  "Return registers defined by INSTS."
  (let ((defs (make-hash-table :test #'eq)))
    (dolist (inst insts defs)
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst defs) t))))))

(defun %loop-unswitch-def-index (body reg)
  "Return the first index in BODY that defines REG, if any."
  (position-if (lambda (inst) (eq (opt-inst-dst inst) reg)) body))

(defun %loop-unswitch-loop-candidate-at (vec index)
  "Return a simple counted-loop candidate suitable for unswitching."
  (let ((n (length vec)))
    (when (and (<= (+ index 5) (1- n))
               (vm-label-p (aref vec index)))
      (let* ((header (aref vec index))
             (cmp-inst (aref vec (+ index 1)))
             (jz-inst (aref vec (+ index 2)))
             (header-name (vm-name header)))
        (when (and (%opt-loop-unroll-cmp-inst-p cmp-inst)
                   (typep jz-inst 'vm-jump-zero)
                   (eq (vm-reg jz-inst) (vm-dst cmp-inst)))
          (let* ((exit-name (vm-label-name jz-inst))
                 (exit-pos (cfg-find-label-position vec n exit-name))
                 (back-pos (and exit-pos (1- exit-pos)))
                 (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
            (when (and exit-pos
                       (> exit-pos (+ index 4))
                       (typep back-inst 'vm-jump)
                       (equal (vm-label-name back-inst) header-name)
                       (not (%opt-has-external-jump-to-label-p vec header-name index exit-pos)))
              (let* ((body (loop for j from (+ index 3) below back-pos
                                 collect (aref vec j)))
                     (step-inst (car (last body))))
                (when (%loop-unroll-final-step-p step-inst cmp-inst)
                  (list :header-pos index
                        :exit-pos exit-pos
                        :back-pos back-pos
                        :header-name header-name
                        :exit-name exit-name
                        :body body))))))))))

(defun %loop-unswitch-branch-internal-p (body branch)
  "Return T when BRANCH's target label is inside BODY."
  (let ((target (vm-label-name branch)))
    (some (lambda (inst)
            (and (vm-label-p inst) (equal (vm-name inst) target)))
          body)))

(defun %loop-unswitch-invariant-branch (body)
  "Find an invariant conditional branch in BODY.

The branch condition may be a value defined before the loop, or a pure
instruction inside the loop whose operands are all loop-invariant. Impure
condition computations are rejected to avoid changing side-effect behavior."
  (let ((defs (%loop-unswitch-dst-set body)))
    (loop for inst in body
          for index from 0
          when (and (typep inst 'vm-jump-zero)
                    (%loop-unswitch-branch-internal-p body inst))
            do (let* ((reg (vm-reg inst))
                      (def-index (%loop-unswitch-def-index body reg))
                      (def-inst (and def-index (nth def-index body))))
                 (cond
                   ((null def-index)
                    (return (list :branch-index index :branch inst :condition-inst nil)))
                   ((and (< def-index index)
                         (eq (vm-inst-effect-kind def-inst) :pure)
                         (every (lambda (read) (not (gethash read defs)))
                                (opt-inst-read-regs def-inst)))
                    (return (list :branch-index index
                                  :branch inst
                                  :condition-index def-index
                                  :condition-inst def-inst))))))))

(defun %loop-unswitch-collect-loop-labels (vec start back-pos)
  "Return labels in the copied loop range START..BACK-POS."
  (let (labels)
    (loop for i from start to back-pos
          for inst = (aref vec i)
          when (vm-label-p inst)
            do (pushnew (vm-name inst) labels :test #'equal))
    labels))

(defun %loop-unswitch-rename-table (labels suffix used)
  "Build original-label -> fresh-label table for one loop version."
  (let ((rename (make-hash-table :test #'equal)))
    (dolist (label labels rename)
      (setf (gethash label rename)
            (%loop-unswitch-fresh (format nil "~A~A" label suffix) used)))))

(defun %loop-unswitch-renamed-target (target rename exit-name)
  (cond
    ((gethash target rename) (gethash target rename))
    ((equal target exit-name) exit-name)
    (t target)))

(defun %loop-unswitch-copy-loop-version (vec start back-pos branch-info rename exit-name true-version-p)
  "Copy one specialized loop version."
  (let ((branch-index (+ start 3 (getf branch-info :branch-index)))
        (condition-index (let ((idx (getf branch-info :condition-index)))
                           (and idx (+ start 3 idx))))
        (branch (getf branch-info :branch))
        (out nil))
    (loop for i from start to back-pos
          for inst = (aref vec i)
          do (cond
               ((and condition-index (= i condition-index))
                nil)
               ((= i branch-index)
                (unless true-version-p
                  (push (make-vm-jump
                         :label (%loop-unswitch-renamed-target (vm-label-name branch)
                                                               rename exit-name))
                        out)))
               (t
                (push (%loop-unswitch-copy-inst inst rename exit-name) out))))
    (nreverse out)))

(defun %loop-unswitch-apply-candidate (instructions candidate branch-info)
  "Apply loop unswitching for CANDIDATE and BRANCH-INFO."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (start (getf candidate :header-pos))
         (exit-pos (getf candidate :exit-pos))
         (back-pos (getf candidate :back-pos))
         (exit-name (getf candidate :exit-name))
         (condition-inst (getf branch-info :condition-inst))
         (branch (getf branch-info :branch))
         (condition-reg (vm-reg branch))
         (used (%loop-unswitch-label-table instructions))
         (labels (%loop-unswitch-collect-loop-labels vec start back-pos))
         (true-rename (%loop-unswitch-rename-table labels "_unsw_true" used))
         (false-rename (%loop-unswitch-rename-table labels "_unsw_false" used))
         (true-entry (gethash (getf candidate :header-name) true-rename))
         (false-entry (gethash (getf candidate :header-name) false-rename))
         (out nil))
    (loop for i from 0 below start do (push (aref vec i) out))
    (when condition-inst
      (push (%loop-unroll-copy-inst condition-inst) out))
    (push (make-vm-jump-zero :reg condition-reg :label false-entry) out)
    (push (make-vm-jump :label true-entry) out)
    (dolist (inst (%loop-unswitch-copy-loop-version vec start back-pos branch-info
                                                    true-rename exit-name t))
      (push inst out))
    (dolist (inst (%loop-unswitch-copy-loop-version vec start back-pos branch-info
                                                    false-rename exit-name nil))
      (push inst out))
    (loop for i from exit-pos below n do (push (aref vec i) out))
    (nreverse out)))

(defun opt-pass-loop-unswitch (instructions)
  "FR-602: hoist loop-invariant conditionals out of conservative counted loops.

The pass duplicates one loop into true/false specialized versions and emits an
outer conditional dispatch. It is intentionally code-size increasing and is
therefore policy-gated in the default pipeline by SPEED=3 and SPACE=0."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec)))
    (loop for i from 0 below n
          for candidate = (%loop-unswitch-loop-candidate-at vec i)
          when candidate
            do (let ((branch-info (%loop-unswitch-invariant-branch
                                   (getf candidate :body))))
                 (when branch-info
                   (return-from opt-pass-loop-unswitch
                     (%loop-unswitch-apply-candidate instructions candidate branch-info)))))
    instructions))
