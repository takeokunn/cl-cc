(in-package :cl-cc/optimize)

(defun opt-pass-unreachable (instructions)
  "Remove instructions that follow unconditional control transfers (jump/ret)
   and precede the next label — they can never be executed."
  (let ((result nil)
        (dead nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (setf dead nil)
         (push inst result))
        (t
         (unless dead
           (push inst result))
         ;; Mark subsequent instructions as unreachable after unconditional transfer
          (when (or (vm-jump-p inst) (vm-ret-p inst))
            (setf dead t)))))
    (nreverse result)))

;;; ─── Conservative loop rotation (FR-169 subset) ─────────────────────────

(defun %opt-loop-rotation-fresh-label (base used)
  "Return a fresh label name derived from BASE not present in USED hash-table." 
  (let ((i 0)
        (name nil))
    (loop
      do (setf name (if (= i 0)
                        base
                        (format nil "~A_~D" base i)))
         (if (gethash name used)
              (incf i)
              (return name)))))

(defun %opt-loop-label-positions (vec)
  "Return a label-name -> vector-position table for VEC."
  (let ((positions (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          for inst = (aref vec i)
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) positions) i))
    positions))

(defun %opt-loop-inst-position (vec inst)
  "Return the position of INST in VEC using object identity."
  (loop for i from 0 below (length vec)
        when (eq (aref vec i) inst)
        return i))

(defun %opt-loop-last-jump-to (block label-name)
  "Return BLOCK's final vm-jump when it jumps to LABEL-NAME."
  (let ((term (car (last (bb-instructions block)))))
    (and (typep term 'vm-jump)
         (equal (vm-label-name term) label-name)
         term)))

(defun %opt-loop-cfg-candidate (instructions)
  "Find a conservative single-latch natural loop candidate using the CFG.
Returns a plist with linear positions needed by the existing transforms."
  (handler-case
      (let* ((vec (coerce instructions 'vector))
             (label-pos (%opt-loop-label-positions vec))
             (cfg (cfg-build instructions)))
        (cfg-compute-dominators cfg)
        (cfg-compute-loop-depths cfg)
        (loop for latch across (cfg-blocks cfg)
              thereis
              (loop for header in (bb-successors latch)
                    thereis
                    (and (cfg-dominates-p header latch)
                         (bb-label header)
                         (let* ((header-name (vm-name (bb-label header)))
                                (loop-blocks (cfg-collect-natural-loop header latch))
                                (latches (remove-if-not
                                          (lambda (b) (%opt-loop-last-jump-to b header-name))
                                          loop-blocks)))
                           (when (= (length latches) 1)
                             (let* ((header-insts (bb-instructions header))
                                    (cond-inst (first header-insts))
                                    (jz-inst (second header-insts))
                                    (back-inst (%opt-loop-last-jump-to latch header-name))
                                    (header-pos (gethash header-name label-pos))
                                    (exit-pos (and (typep jz-inst 'vm-jump-zero)
                                                   (gethash (vm-label-name jz-inst) label-pos)))
                                    (back-pos (%opt-loop-inst-position vec back-inst)))
                               (when (and cond-inst
                                          (typep jz-inst 'vm-jump-zero)
                                          (not (vm-label-p cond-inst))
                                          header-pos exit-pos back-pos
                                          (< header-pos back-pos exit-pos))
                                 (list :header-name header-name
                                       :header-pos header-pos
                                       :exit-pos exit-pos
                                       :back-pos back-pos
                                       :cond-inst cond-inst
                                       :jz-inst jz-inst)))))))))
    (error () nil)))

(defun %opt-loop-rotation-apply-candidate (instructions candidate)
  "Apply loop rotation for a CFG-derived CANDIDATE and return a fresh list."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-pos (getf candidate :header-pos))
         (exit-pos (getf candidate :exit-pos))
         (back-pos (getf candidate :back-pos))
         (header-name (getf candidate :header-name))
         (cond-inst (getf candidate :cond-inst))
         (jz-inst (getf candidate :jz-inst))
         (used-labels (make-hash-table :test #'equal))
         (result nil))
    (loop for inst across vec
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) used-labels) t))
    (let* ((body-label-name (%opt-loop-rotation-fresh-label
                             (format nil "~A_body" header-name) used-labels))
           (guard-label-name (%opt-loop-rotation-fresh-label
                              (format nil "~A_guard" header-name) used-labels)))
      (loop for i from 0 below header-pos
            for inst = (aref vec i)
            do (push (if (and (typep inst 'vm-jump)
                              (equal (vm-label-name inst) header-name))
                         (make-vm-jump :label guard-label-name)
                         inst)
                     result))
      (push (make-vm-jump :label guard-label-name) result)
      (push (make-vm-label :name body-label-name) result)
      (loop for i from (+ header-pos 3) below back-pos do (push (aref vec i) result))
      (push (make-vm-label :name guard-label-name) result)
      (push cond-inst result)
      (push jz-inst result)
      (push (make-vm-jump :label body-label-name) result)
      (loop for i from exit-pos below n do (push (aref vec i) result))
      (nreverse result))))

(defun %opt-pass-loop-rotation-linear (instructions)
  "Rotate simple while-shaped loops into guard+do-while form.

Conservative subset only. Matches this linear shape:
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:
and rewrites to:
  (vm-jump Lguard) Lbody: <body...> Lguard: <cond-inst>
  (vm-jump-zero reg Lexit) (vm-jump Lbody) Lexit:

The transform is skipped unless all structural checks pass."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (used-labels (make-hash-table :test #'equal)))
    (loop for i from 0 below n
          for inst = (aref vec i)
          when (vm-label-p inst)
          do (setf (gethash (vm-name inst) used-labels) t))
    (let ((result nil)
          (i 0))
      (loop while (< i n)
            do (let ((cur (aref vec i)))
                 (if (and (vm-label-p cur)
                          (<= (+ i 4) (1- n)))
                     (let* ((header       cur)
                            (cond-inst    (aref vec (+ i 1)))
                            (jz-inst      (aref vec (+ i 2)))
                            (header-name  (vm-name header)))
                       (if (and (typep jz-inst 'vm-jump-zero)
                                (not (vm-label-p cond-inst)))
                           (let* ((exit-name (vm-label-name jz-inst))
                                  (exit-pos  (cfg-find-label-position vec n exit-name))
                                  (back-pos  (and exit-pos (1- exit-pos)))
                                  (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                             (if (and exit-pos
                                      (> exit-pos (+ i 3))
                                      (typep back-inst 'vm-jump)
                                      (equal (vm-label-name back-inst) header-name))
                                 (let* ((body-insts (loop for j from (+ i 3) below back-pos
                                                          collect (aref vec j)))
                                        (body-label-name  (%opt-loop-rotation-fresh-label
                                                           (format nil "~A_body" header-name)
                                                           used-labels))
                                        (guard-label-name (%opt-loop-rotation-fresh-label
                                                           (format nil "~A_guard" header-name)
                                                           used-labels))
                                        (body-label  (make-vm-label :name body-label-name))
                                        (guard-label (make-vm-label :name guard-label-name)))
                                   (setf (gethash body-label-name used-labels) t
                                         (gethash guard-label-name used-labels) t)
                                   (push (make-vm-jump :label guard-label-name) result)
                                   (push body-label result)
                                   (dolist (b body-insts) (push b result))
                                   (push guard-label result)
                                   (push cond-inst result)
                                   (push jz-inst result)
                                   (push (make-vm-jump :label body-label-name) result)
                                   (setf i exit-pos)
                                   (push (aref vec i) result)
                                   (incf i))
                                 (progn
                                   (push cur result)
                                   (incf i))))
                           (progn
                             (push cur result)
                             (incf i))))
                     (progn
                       (push cur result)
                       (incf i)))))
      (nreverse result))))

(defun opt-pass-loop-rotation (instructions)
  "Rotate loops using CFG natural-loop detection, falling back to linear matching."
  (let ((candidate (%opt-loop-cfg-candidate instructions)))
    (if candidate
        (%opt-loop-rotation-apply-candidate instructions candidate)
        (%opt-pass-loop-rotation-linear instructions))))

;;; ─── Conservative loop peeling (FR-170 subset) ──────────────────────────

(defun %opt-loop-peeling-apply-candidate (instructions candidate)
  "Apply one-iteration peeling for a CFG-derived CANDIDATE."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (header-pos (getf candidate :header-pos))
         (back-pos (getf candidate :back-pos))
         (cond-inst (getf candidate :cond-inst))
         (jz-inst (getf candidate :jz-inst))
         (body-insts (loop for i from (+ header-pos 3) below back-pos
                           collect (aref vec i)))
         (result nil))
    (when (some #'vm-label-p body-insts)
      (return-from %opt-loop-peeling-apply-candidate nil))
    (loop for i from 0 below header-pos
          for inst = (aref vec i)
          unless (and (= i (1- header-pos))
                      (typep inst 'vm-jump)
                      (equal (vm-label-name inst) (getf candidate :header-name)))
            do (push inst result))
    (push cond-inst result)
    (push jz-inst result)
    (dolist (b body-insts) (push b result))
    (loop for i from header-pos below n do (push (aref vec i) result))
    (cfg-flatten (cfg-build (nreverse result)))))

(defun %opt-pass-loop-peeling-linear (instructions)
  "Peel the first iteration of a simple while-shaped loop.

Conservative subset only. Matches this linear shape:
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:
and rewrites to:
  <cond-inst> (vm-jump-zero reg Lexit) <body...>
  Lh: <cond-inst> (vm-jump-zero reg Lexit) <body...> (vm-jump Lh) Lexit:

This duplicates only one first-iteration body before the original loop header."
  (let* ((vec (coerce instructions 'vector))
         (n (length vec))
         (result nil)
         (i 0)
         (peeled nil))
    (loop while (< i n)
          do (let ((cur (aref vec i)))
               (if (and (not peeled)
                        (vm-label-p cur)
                        (<= (+ i 4) (1- n)))
                   (let* ((header      cur)
                          (cond-inst   (aref vec (+ i 1)))
                          (jz-inst     (aref vec (+ i 2)))
                          (header-name (vm-name header)))
                     (if (and (typep jz-inst 'vm-jump-zero)
                              (not (vm-label-p cond-inst)))
                         (let* ((exit-name (vm-label-name jz-inst))
                                (exit-pos  (cfg-find-label-position vec n exit-name))
                                (back-pos  (and exit-pos (1- exit-pos)))
                                (back-inst (and back-pos (>= back-pos 0) (aref vec back-pos))))
                           (if (and exit-pos
                                    (> exit-pos (+ i 3))
                                    (typep back-inst 'vm-jump)
                                    (equal (vm-label-name back-inst) header-name))
                               (let ((body-insts (loop for j from (+ i 3) below back-pos
                                                       collect (aref vec j))))
                                 ;; peeled first iteration (without header label)
                                 (push cond-inst result)
                                 (push jz-inst result)
                                 (dolist (b body-insts) (push b result))
                                 ;; then keep original loop unchanged
                                 (loop for j from i below (1+ exit-pos)
                                       do (push (aref vec j) result))
                                 (setf i (1+ exit-pos)
                                       peeled t))
                               (progn
                                 (push cur result)
                                 (incf i))))
                         (progn
                           (push cur result)
                           (incf i))))
                   (progn
                     (push cur result)
                     (incf i)))))
    (nreverse result)))

(defun opt-pass-loop-peeling (instructions)
  "Peel loops using CFG natural-loop detection, falling back to linear matching."
  (let* ((candidate (%opt-loop-cfg-candidate instructions))
         (peeled (and candidate (%opt-loop-peeling-apply-candidate instructions candidate))))
    (or peeled (%opt-pass-loop-peeling-linear instructions))))

;;; ─── Conservative loop unrolling (FR-021 subset) ────────────────────────
