;;;; packages/optimize/src/optimizer-closure.lisp
;;;; Closure allocation optimizations.

(in-package :cl-cc/optimize)

(defun %opt-closure-env-regs (inst)
  "Return the environment registers captured by closure allocation INST."
  (cond
    ((vm-closure-p inst) (mapcar #'cdr (vm-captured-vars inst)))
    ((vm-make-closure-p inst) (vm-env-regs inst))
    (t nil)))

(defun %opt-closure-captured-vars (inst)
  "Return a capture alist suitable for cl-cc/ast closure grouping helpers."
  (cond
    ((vm-closure-p inst) (vm-captured-vars inst))
    ((vm-make-closure-p inst)
     (mapcar (lambda (reg) (cons reg reg)) (vm-env-regs inst)))
    (t nil)))

(defun %opt-simple-closure-inst-p (inst)
  "T when INST can be represented by vm-make-closure without losing metadata."
  (or (vm-make-closure-p inst)
      (and (vm-closure-p inst)
           (vm-captured-vars inst)
           (null (vm-closure-optional-params inst))
           (null (vm-closure-rest-param inst))
           (null (vm-closure-key-params inst))
           (null (vm-closure-rest-stack-alloc-p inst))
           (null (vm-closure-inline-policy inst))
           (null (vm-closure-inst-dispatch-tag inst)))))

(defun %opt-closure-descriptor (inst index)
  "Build a plist descriptor for shareable closure INST at INDEX."
  (when (%opt-simple-closure-inst-p inst)
    (let ((captures (%opt-closure-captured-vars inst)))
      (when captures
        (list :index index
              :inst inst
              :entry-label (vm-label-name inst)
              :captured-vars captures)))))

(defun %opt-closure-groups (instructions)
  "Return shareable closure descriptor groups keyed by AST closure analysis."
  (let ((descriptors nil))
    (loop for inst in instructions
          for index from 0
          for desc = (%opt-closure-descriptor inst index)
          when desc do (push desc descriptors))
    (cl-cc/ast:group-shareable-closures (nreverse descriptors))))

(defun %opt-range-has-write-to-reg-p (instructions start end reg)
  "T when any instruction in [START, END) writes REG."
  (loop for i from start below end
        for inst = (nth i instructions)
        thereis (eq (opt-inst-dst inst) reg)))

(defun %opt-range-has-write-to-any-reg-p (instructions start end regs)
  "T when any instruction in [START, END) writes one of REGS."
  (some (lambda (reg) (%opt-range-has-write-to-reg-p instructions start end reg)) regs))

(defun %opt-shareable-closure-use-p (instructions root-desc desc)
  "T when DESC can safely reuse ROOT-DESC's closure register."
  (let* ((root-index (getf root-desc :index))
         (index (getf desc :index))
         (root-inst (getf root-desc :inst))
         (env-regs (%opt-closure-env-regs root-inst))
         (root-reg (vm-dst root-inst)))
    (and (< root-index index)
         (not (%opt-range-has-write-to-reg-p instructions (1+ root-index) index root-reg))
         (not (%opt-range-has-write-to-any-reg-p instructions (1+ root-index) index env-regs)))))

(defun %opt-make-closure-from-inst (inst)
  "Lower a simple closure allocation INST to vm-make-closure."
  (if (vm-make-closure-p inst)
      inst
      (make-vm-make-closure :dst (vm-dst inst)
                            :label (vm-label-name inst)
                            :params (vm-closure-params inst)
                            :env-regs (%opt-closure-env-regs inst))))

(defun %opt-record-closure-group-rewrites (instructions group rewrites)
  "Record FR-330 rewrites for one closure descriptor GROUP into REWRITES."
  (let* ((root-desc (first group))
         (root-inst (getf root-desc :inst))
         (root-index (getf root-desc :index))
         (root-reg (vm-dst root-inst)))
    (setf (gethash root-index rewrites) (%opt-make-closure-from-inst root-inst))
    (dolist (desc (rest group))
      (let* ((index (getf desc :index))
             (inst (getf desc :inst)))
        (when (%opt-shareable-closure-use-p instructions root-desc desc)
          (setf (gethash index rewrites)
                (make-vm-move :dst (vm-dst inst) :src root-reg)))))))

(defun opt-pass-closure-capture-dedup (instructions)
  "FR-330: share duplicate closure environments with one vm-make-closure.

Closures are grouped with cl-cc/ast:group-shareable-closures by code label and
capture set.  The first closure in each safe group performs the allocation;
later closure sites become vm-move aliases to the first closure register."
  (let ((groups (%opt-closure-groups instructions))
        (rewrites (make-hash-table :test #'eql)))
    (maphash (lambda (_key group)
               (declare (ignore _key))
               (%opt-record-closure-group-rewrites instructions group rewrites))
             groups)
    (loop for inst in instructions
          for index from 0
          collect (or (gethash index rewrites) inst))))

(defun opt-pass-closure-thunk-sharing (instructions)
  "FR-079 closure thunk sharing: eliminate redundant closure allocations.

When two simple closure instructions share both entry-label and capture set, and
the first closure register + its captured environment registers are not
overwritten between the two sites, the second allocation is replaced by a
vm-move to the first closure register.  This is a conservative partial
implementation; same-code-body-with-different-environments requires VM-level
support for separate code-pointer + environment-record (FR-079 extension)."

  (let* ((groups (%opt-closure-groups instructions))
         (rewrites (make-hash-table :test #'eql)))
    (maphash (lambda (_key group)
               (declare (ignore _key))
               (when (>= (length group) 2)
                 (let ((root-desc (first group)))
                   (dolist (desc (rest group))
                     (when (%opt-shareable-closure-use-p instructions root-desc desc)
                       (let* ((dup-index (getf desc :index))
                              (dup-dst (vm-dst (nth dup-index instructions)))
                              (root-dst (vm-dst (getf root-desc :inst)))
                              (move (make-vm-move :dst dup-dst :src root-dst)))
                         (setf (gethash dup-index rewrites) move)))))))
              groups)
    (loop for inst in instructions
          for index from 0
          collect (or (gethash index rewrites) inst))))
