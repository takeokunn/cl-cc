(in-package :cl-cc/compile)

;;; ── FR-138 exception table + OSR/deopt infrastructure ─────────────────────
;;;
;;; Provides: compile-exception-entry defstruct,
;;;           %record-exception-table-entry,
;;;           %instruction-pc-index,
;;;           %build-exception-table,
;;;           %insert-osr-entry-markers,
;;;           %build-deopt-info

(defstruct compile-exception-entry
  start-inst
  end-inst
  handler-label
  condition-type
  result-reg
  order)

(defun %record-exception-table-entry (start-inst end-inst handler-label condition-type result-reg)
  "Record a protected instruction range for final PC→handler table creation."
  (when (and start-inst end-inst handler-label)
    (push (make-compile-exception-entry
           :start-inst start-inst
           :end-inst end-inst
           :handler-label handler-label
           :condition-type condition-type
           :result-reg result-reg
           :order (prog1 *next-exception-table-entry-order*
                    (incf *next-exception-table-entry-order*)))
          *pending-exception-table-entries*)))

(defun %instruction-pc-index (instructions)
  "Return an EQ hash table mapping instruction objects to their final PCs."
  (let ((index (make-hash-table :test #'eq)))
    (loop for inst in instructions
          for pc from 0
          do (setf (gethash inst index) pc))
    index))

(defun %build-exception-table (instructions)
  "Resolve pending exception ranges into concrete VM exception-table entries."
  (let ((pending (nreverse (copy-list *pending-exception-table-entries*))))
    (when pending
      (let ((pc-index (%instruction-pc-index instructions))
            (labels   (cl-cc/vm::build-label-table instructions))
            (entries  nil))
        (dolist (entry pending)
          (let* ((start-pc (gethash (compile-exception-entry-start-inst entry) pc-index))
                 (last-pc  (gethash (compile-exception-entry-end-inst entry) pc-index))
                 (handler-pc (cl-cc/vm::vm-label-table-lookup
                              labels (compile-exception-entry-handler-label entry))))
            (when (and start-pc last-pc handler-pc (<= start-pc last-pc))
              (push (cl-cc/vm::make-vm-exception-entry
                     start-pc
                     (1+ last-pc)
                     handler-pc
                     (compile-exception-entry-condition-type entry)
                     (compile-exception-entry-result-reg entry)
                     (compile-exception-entry-order entry))
                    entries))))
        (coerce (nreverse entries) 'vector)))))

(defun %insert-osr-entry-markers (instructions)
  "Insert lightweight OSR markers immediately before loop back-edge jumps."
  (if cl-cc/vm:*osr-enabled*
      (let ((labels (cl-cc/vm::build-label-table instructions))
            (marked nil))
        (loop for inst in instructions
              for pc from 0
              do (when (typep inst 'vm-jump)
                   (let* ((label (vm-label-name inst))
                          (target-pc (cl-cc/vm::vm-label-table-lookup labels label)))
                     (when (and target-pc (<= target-pc pc))
                       (push (cl-cc/vm::make-vm-osr-entry
                              :label label
                              :id (list :loop-header label :back-edge-pc pc))
                             marked))))
                 (push inst marked))
        (nreverse marked))
      instructions))

(defun %build-deopt-info (instructions)
  "Build PC -> interpreter reconstruction metadata for FR-155 checkpoints."
  (let ((table (make-hash-table :test #'eql))
        (osr nil))
    (loop for inst in instructions
          for pc from 0
          do (cond
                ((and cl-cc/vm:*deopt-enabled*
                      (typep inst 'cl-cc/vm::vm-type-check))
                 (setf (gethash pc table)
                       (cl-cc/vm::make-vm-deopt-info
                        :pc pc
                        :label (cl-cc/vm::vm-type-check-deopt-label inst)
                        :live-regs (list (vm-src inst))
                        :vreg->preg (list (cons (vm-src inst) :p0))
                        :description (list :type-check (cl-cc/vm::vm-type-name inst)
                                           :id (cl-cc/vm::vm-type-check-deopt-id inst)))))
                ((and cl-cc/vm:*deopt-enabled*
                      (typep inst 'cl-cc/vm::vm-deopt))
                 (setf (gethash pc table)
                       (cl-cc/vm::make-vm-deopt-info
                        :pc pc
                        :label (cl-cc/vm::vm-deopt-label inst)
                        :vreg->preg nil
                        :inline-stack nil
                        :description (list :deopt (cl-cc/vm::vm-deopt-reason inst)
                                           :id (cl-cc/vm::vm-deopt-id inst)))))
                ((and cl-cc/vm:*osr-enabled*
                      (typep inst 'cl-cc/vm::vm-osr-entry))
                 (push (list :pc pc
                             :label (cl-cc/vm::vm-osr-label inst)
                             :id (cl-cc/vm::vm-osr-id inst))
                      osr))))
    (values table (nreverse osr))))
