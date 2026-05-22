(in-package :cl-cc/optimize)

;;; ─── FR-683: Loop Rotation ───────────────────────────────────────────────
;;;
;;; Rotate conservative while-header loops into a guard + do-while shape:
;;;
;;;   H:  <cond>
;;;       (jump-zero cond EXIT)
;;;       <body>
;;;       (jump H)
;;;   EXIT:
;;;
;;; becomes:
;;;
;;;       (jump H_guard)
;;;   H_body:
;;;       <body>
;;;   H_guard:
;;;       <cond>
;;;       (jump-zero cond EXIT)
;;;       (jump H_body)
;;;   EXIT:
;;;
;;; The pass deliberately skips multi-latch loops and loops whose body contains
;;; labels, because preserving arbitrary internal branch targets would require
;;; more invasive CFG surgery.

(defun %loop-rotate-label-table (instructions)
  "Return a table containing all label names in INSTRUCTIONS."
  (let ((labels (make-hash-table :test #'equal)))
    (dolist (inst instructions labels)
      (when (vm-label-p inst)
        (setf (gethash (vm-name inst) labels) t)))))

(defun %loop-rotate-position (vec inst)
  "Return INST's vector index in VEC using EQ identity."
  (loop for i from 0 below (length vec)
        when (eq (aref vec i) inst)
          return i))

(defun %loop-rotate-last-jump-to (block label-name)
  "Return BLOCK's final unconditional jump to LABEL-NAME, if present."
  (let ((term (car (last (bb-instructions block)))))
    (and (typep term 'vm-jump)
         (equal (vm-label-name term) label-name)
         term)))

(defun %loop-rotate-candidate (instructions)
  "Find one safe while-header natural loop rotation candidate.

The result is a plist containing linear positions. NIL means no safe candidate."
  (handler-case
      (let* ((vec (coerce instructions 'vector))
             (label-pos (%opt-loop-label-positions vec))
             (cfg (cfg-build instructions)))
        (cfg-compute-dominators cfg)
        (loop for latch across (cfg-blocks cfg)
              thereis
              (loop for header in (bb-successors latch)
                    thereis
                    (when (and (cfg-dominates-p header latch)
                               (bb-label header))
                      (let* ((header-name (vm-name (bb-label header)))
                             (members (cfg-collect-natural-loop header latch))
                             (latches (remove-if-not
                                       (lambda (block)
                                         (%loop-rotate-last-jump-to block header-name))
                                       members)))
                        (when (= (length latches) 1)
                          (let* ((header-insts (bb-instructions header))
                                 (cond-inst (first header-insts))
                                 (branch-inst (second header-insts))
                                 (back-inst (%loop-rotate-last-jump-to latch header-name))
                                 (header-pos (gethash header-name label-pos))
                                 (exit-pos (and (typep branch-inst 'vm-jump-zero)
                                                (gethash (vm-label-name branch-inst) label-pos)))
                                 (back-pos (%loop-rotate-position vec back-inst)))
                            (when (and cond-inst
                                       (not (vm-label-p cond-inst))
                                       (typep branch-inst 'vm-jump-zero)
                                       header-pos exit-pos back-pos
                                       (< header-pos back-pos exit-pos)
                                       (notany #'vm-label-p
                                               (loop for i from (+ header-pos 3) below back-pos
                                                     collect (aref vec i))))
                              (list :header-name header-name
                                    :header-pos header-pos
                                    :exit-pos exit-pos
                                    :back-pos back-pos
                                    :cond-inst cond-inst
                                    :branch-inst branch-inst)))))))))
    (error () nil)))

(defun %loop-rotate-rewrite-entry-jump (inst header-name guard-name)
  "Redirect an outside jump to HEADER-NAME so it enters the rotated guard."
  (if (and (typep inst 'vm-jump)
           (equal (vm-label-name inst) header-name))
      (make-vm-jump :label guard-name)
      inst))

(defun %loop-rotate-apply-candidate (instructions candidate)
  "Apply CANDIDATE to INSTRUCTIONS and return a fresh instruction list."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-name (getf candidate :header-name))
         (header-pos (getf candidate :header-pos))
         (exit-pos (getf candidate :exit-pos))
         (back-pos (getf candidate :back-pos))
         (cond-inst (getf candidate :cond-inst))
         (branch-inst (getf candidate :branch-inst))
         (used-labels (%loop-rotate-label-table instructions))
         (body-name (%opt-loop-rotation-fresh-label
                     (format nil "~A_body" header-name) used-labels))
         (guard-name (%opt-loop-rotation-fresh-label
                      (format nil "~A_guard" header-name) used-labels))
         (result nil))
    (loop for i from 0 below header-pos
          for inst = (aref vec i)
          do (push (%loop-rotate-rewrite-entry-jump inst header-name guard-name) result))
    ;; Explicit pre-header: one-time entry check jumps to the bottom guard.
    (push (make-vm-jump :label guard-name) result)
    (push (make-vm-label :name body-name) result)
    (loop for i from (+ header-pos 3) below back-pos
          do (push (aref vec i) result))
    (push (make-vm-label :name guard-name) result)
    (push cond-inst result)
    (push branch-inst result)
    (push (make-vm-jump :label body-name) result)
    (loop for i from exit-pos below n
          do (push (aref vec i) result))
    (nreverse result)))

(defun opt-pass-loop-rotate (instructions)
  "FR-683 loop rotation pass.

Only single-latch while-header loops are rotated. Multi-back-edge loops and
loops with internal labels are left untouched to avoid correctness hazards."
  (or (let ((candidate (%loop-rotate-candidate instructions)))
        (and candidate (%loop-rotate-apply-candidate instructions candidate)))
      instructions))
