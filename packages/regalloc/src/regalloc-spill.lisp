(in-package :cl-cc/regalloc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Regalloc — Spill Code Insertion and Live-Range Splitting
;;;
;;; Contains: vm-spill-store/vm-spill-load instruction structs,
;;; %regalloc-map-tree, %regalloc-rewrite-inst,
;;; %regalloc-rewrite-inst-dst, %regalloc-rematerialize-inst,
;;; %regalloc-reserved-scratch-regs, %regalloc-scratch-candidates,
;;; %split-vreg-at-position, %copy-float-vreg-map-with-splits,
;;; %split-live-range-rewrite-map, %rewrite-inst-for-split-position,
;;; %collect-live-range-splits, %assign-live-range-split-slots,
;;; %boundaries-by-position, split-live-ranges,
;;; %finalize-split-spill-registers, insert-spill-code.
;;;
;;; Depends on: regalloc-allocate.lisp (linear-scan-allocate,
;;;   %regalloc-scratch-candidates, regalloc-target-fp-registers).
;;;
;;; Load order: after regalloc-allocate.lisp, before regalloc-color.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Spill Instructions

(define-vm-instruction vm-spill-store (vm-instruction)
  "Store register to spill slot [RBP - slot*8]."
  (src-reg nil :reader vm-spill-src)
  (slot nil :reader vm-spill-slot))

(define-vm-instruction vm-spill-load (vm-instruction)
  "Load spill slot [RBP - slot*8] into register."
  (dst-reg nil :reader vm-spill-dst)
  (slot nil :reader vm-spill-slot))

(defparameter *live-range-split-minimum-hole-size* 8
  "Minimum instruction-position gap that triggers live range splitting.")

(defmethod instruction-uses ((inst vm-spill-store))
  (list (vm-spill-src inst)))

(defmethod instruction-defs ((inst vm-spill-load))
  (list (vm-spill-dst inst)))

;;; Register Rewriting

(defun %regalloc-map-tree (fn tree)
  "Recursively map FN over all leaves of TREE."
  (if (consp tree)
      (cons (%regalloc-map-tree fn (car tree))
            (%regalloc-map-tree fn (cdr tree)))
      (funcall fn tree)))

(defun %regalloc-rewrite-inst (inst reg-map)
  "Return INST with register keywords substituted per REG-MAP."
  (flet ((rewrite-reg (reg)
           (if (and (keywordp reg) (gethash reg reg-map))
               (gethash reg reg-map)
               reg)))
    (cond
      ((typep inst 'vm-spill-store)
       (make-vm-spill-store :src-reg (rewrite-reg (vm-spill-src inst))
                            :slot (vm-spill-slot inst)))
      ((typep inst 'vm-spill-load)
       (make-vm-spill-load :dst-reg (rewrite-reg (vm-spill-dst inst))
                           :slot (vm-spill-slot inst)))
      (t
       (handler-case
           (sexp->instruction
            (%regalloc-map-tree #'rewrite-reg (instruction->sexp inst)))
         (error () inst))))))

(defun %regalloc-rewrite-inst-dst (inst dst)
  "Return INST cloned with its single destination rewritten to DST."
  (let ((old-dst (opt-inst-dst inst))
        (reg-map (make-hash-table :test #'eq)))
    (when old-dst
      (setf (gethash old-dst reg-map) dst))
    (%regalloc-rewrite-inst inst reg-map)))

(defun %regalloc-rematerialize-inst (remat scratch)
  "Return instruction(s) that rematerialize REMAT into SCRATCH."
  (cond
    ((and (consp remat) (eq (car remat) :const))
     (list (make-vm-const :dst scratch :value (cadr remat))))
    ((and (consp remat) (eq (car remat) :inst))
     (list (%regalloc-rewrite-inst-dst (cadr remat) scratch)))
    ((or (integerp remat) (symbolp remat) (characterp remat) (stringp remat))
     (list (make-vm-const :dst scratch :value remat)))
    (t nil)))

;;; Scratch Register Selection

(defun %regalloc-reserved-scratch-regs (inst cc)
  "Return scratch registers reserved internally by INST on CC, if any."
  (when (and (eq (target-name cc) :x86-64)
             (or (typep inst 'vm-integer-mul-high-u)
                 (typep inst 'vm-integer-mul-high-s)
                 (typep inst 'vm-truncate)
                 (typep inst 'vm-rem)
                 (typep inst 'vm-div)
                 (typep inst 'vm-mod)))
    (list (first (target-scratch-regs cc)))))

(defun %regalloc-scratch-candidates (cc used-phys &optional inst fp-p)
  "Return allocatable scratch registers excluding USED-PHYS and reserved registers."
  (let ((reserved (remove nil (%regalloc-reserved-scratch-regs inst cc))))
    (remove-if
     (lambda (reg)
       (or (null reg)
           (member reg used-phys :test #'eq)
           (member reg reserved :test #'eq)))
     (if fp-p
         (regalloc-target-fp-registers cc)
         (remove-duplicates
          (append (list (first (target-scratch-regs cc))
                        (target-ret-reg cc))
                  (target-caller-saved cc)
                  (target-allocatable-regs cc))
          :test #'eq)))))

;;; Live Range Splitting

(defun %split-vreg-at-position (children position)
  "Return the child vreg live at POSITION, or NIL."
  (let ((match (find-if (lambda (child)
                          (and (<= (interval-start child) position)
                               (<= position (interval-end child))))
                        children)))
    (and match (interval-vreg match))))

(defun %copy-float-vreg-map-with-splits (float-vregs child-groups)
  "Copy FLOAT-VREGS and mark split child virtual registers."
  (let ((result (make-hash-table :test #'eq)))
    (when float-vregs
      (maphash (lambda (vreg value)
                 (setf (gethash vreg result) value))
               float-vregs))
    (dolist (group child-groups result)
      (dolist (child (cdr group))
        (when (interval-fp-p child)
          (setf (gethash (interval-vreg child) result) t))))))

(defun %split-live-range-rewrite-map (child-groups position)
  "Build an original-vreg -> child-vreg map for POSITION."
  (let ((map (make-hash-table :test #'eq)))
    (dolist (group child-groups map)
      (destructuring-bind (original-vreg . children) group
        (let ((child-vreg (%split-vreg-at-position children position)))
          (when (and child-vreg (not (eq child-vreg original-vreg)))
            (setf (gethash original-vreg map) child-vreg)))))))

(defun %rewrite-inst-for-split-position (inst child-groups position)
  "Rewrite INST virtual registers to the split children live at POSITION."
  (let ((reg-map (%split-live-range-rewrite-map child-groups position)))
    (if (zerop (hash-table-count reg-map))
        inst
        (%regalloc-rewrite-inst inst reg-map))))

(defun %collect-live-range-splits (intervals minimum-hole-size)
  "Return child interval groups and split boundaries for INTERVALS."
  (let ((child-groups nil)
        (boundaries nil))
    (dolist (interval intervals)
      (multiple-value-bind (children interval-boundaries)
          (split-live-interval interval minimum-hole-size)
        (push (cons (interval-vreg interval) children) child-groups)
        (setf boundaries (append boundaries interval-boundaries))))
    (values (nreverse child-groups)
            (sort boundaries #'< :key #'split-boundary-before-position))))

(defun %assign-live-range-split-slots (boundaries)
  "Assign fixed stack slots to split BOUNDARIES."
  (loop for boundary in boundaries
        for slot from 1
        do (setf (split-boundary-slot boundary) slot)
        finally (return (1- slot))))

(defun %boundaries-by-position (boundaries keyfn)
  "Index BOUNDARIES by position returned by KEYFN."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (boundary boundaries table)
      (push boundary (gethash (funcall keyfn boundary) table)))))

(defun split-live-ranges (instructions intervals float-vregs &optional
                          (minimum-hole-size *live-range-split-minimum-hole-size*))
  "Rewrite INSTRUCTIONS and intervals by splitting long live-range holes.

Returns four values: rewritten instructions, recomputed child intervals, the
number of fixed split spill slots reserved at the bottom of the frame, and the
float-vreg map updated with split children."
  (multiple-value-bind (child-groups boundaries)
      (%collect-live-range-splits intervals minimum-hole-size)
    (let ((split-slot-count (%assign-live-range-split-slots boundaries)))
      (if (zerop split-slot-count)
          (values instructions intervals 0 float-vregs)
          (let ((loads-by-position (%boundaries-by-position boundaries #'split-boundary-before-position))
                (stores-by-position (%boundaries-by-position boundaries #'split-boundary-after-position))
                (rewritten nil))
            (loop for inst in instructions
                  for position from 0
                  do (dolist (boundary (reverse (gethash position loads-by-position)))
                       (push (make-vm-spill-load :dst-reg (split-boundary-to-vreg boundary)
                                                 :slot (split-boundary-slot boundary))
                             rewritten))
                     (push (%rewrite-inst-for-split-position inst child-groups position) rewritten)
                     (dolist (boundary (reverse (gethash position stores-by-position)))
                       (push (make-vm-spill-store :src-reg (split-boundary-from-vreg boundary)
                                                  :slot (split-boundary-slot boundary))
                             rewritten)))
            (let* ((split-instructions (nreverse rewritten))
                   (split-float-vregs (%copy-float-vreg-map-with-splits float-vregs child-groups)))
              (values split-instructions
                      (%compute-live-intervals-raw split-instructions split-float-vregs)
                      split-slot-count
                      split-float-vregs)))))))

(defun %finalize-split-spill-registers (instructions assignment)
  "Rewrite split spill helper instructions from allocated vregs to physical regs."
  (mapcar (lambda (inst)
            (cond
              ((typep inst 'vm-spill-store)
               (let ((phys (gethash (vm-spill-src inst) assignment)))
                 (if phys
                     (make-vm-spill-store :src-reg phys :slot (vm-spill-slot inst))
                     inst)))
              ((typep inst 'vm-spill-load)
               (let ((phys (gethash (vm-spill-dst inst) assignment)))
                 (if phys
                     (make-vm-spill-load :dst-reg phys :slot (vm-spill-slot inst))
                     inst)))
              (t inst)))
          instructions))

;;; Spill Code Insertion

(defun insert-spill-code (instructions assignment spill-map cc &optional remat-map float-vregs)
  "Insert spill loads/stores around instructions that use spilled registers.
   Returns a new instruction list with spill code inserted."
  (let ((result nil))
    (dolist (inst instructions)
      (let* ((used-phys (remove nil
                                (mapcar (lambda (vreg)
                                          (and vreg
                                               (not (gethash vreg spill-map))
                                               (gethash vreg assignment)))
                                         (append (instruction-uses inst)
                                                 (instruction-defs inst)))))
             (available-gpr (%regalloc-scratch-candidates cc used-phys inst nil))
             (available-fp (%regalloc-scratch-candidates cc used-phys inst t))
             (reg-map (make-hash-table :test #'eq)))
        (flet ((float-vreg-p (vreg)
                 (and float-vregs (gethash vreg float-vregs)))
               (alloc-scratch (fp-p)
                 (or (if fp-p (pop available-fp) (pop available-gpr))
                     (error "insert-spill-code: no scratch register available for ~S" inst))))
          (flet ((ensure-scratch (vreg)
                   (or (gethash vreg reg-map)
                       (setf (gethash vreg reg-map)
                             (alloc-scratch (float-vreg-p vreg))))))
          ;; Load spilled uses before the instruction.
          (dolist (vreg (instruction-uses inst))
            (when (and vreg (gethash vreg spill-map))
              (let ((scratch (ensure-scratch vreg)))
                (let ((remat (and remat-map (gethash vreg remat-map))))
                  (if remat
                      (dolist (remat-inst (reverse (%regalloc-rematerialize-inst remat scratch)))
                        (push remat-inst result))
                      (push (make-vm-spill-load :dst-reg scratch
                                                :slot (gethash vreg spill-map))
                            result))))))
          ;; Ensure spilled defs have scratch registers unless the definition is
          ;; rematerializable and can be dropped instead of stored.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map)
                       (not (and remat-map (gethash vreg remat-map))))
              (ensure-scratch vreg)))
          ;; Rewrite instruction once with complete reg-map, then push.
          (unless (and (instruction-defs inst)
                       (every (lambda (vreg)
                                (and vreg
                                     (gethash vreg spill-map)
                                     remat-map
                                     (gethash vreg remat-map)))
                              (instruction-defs inst)))
            (push (if (zerop (hash-table-count reg-map))
                      inst
                      (%regalloc-rewrite-inst inst reg-map))
                  result))
          ;; Store spilled defs after the instruction.
          (dolist (vreg (instruction-defs inst))
            (when (and vreg (gethash vreg spill-map)
                       (not (and remat-map (gethash vreg remat-map))))
              (push (make-vm-spill-store :src-reg (gethash vreg reg-map)
                                          :slot (gethash vreg spill-map))
                    result)))))))
    (nreverse result)))
