;;;; pipeline-pgo.lisp — PGO counter-plan construction and type-feedback annotation
;;; Extracted from pipeline.lisp. Defines all %pgo-* helpers used by
;;; %make-direct-compilation-result and compile-string*.
(in-package :cl-cc/pipeline)

;;; ─────────────────────────────────────────────────────────────────────────
;;; PGO counter-plan construction and type-feedback annotation
;;; ─────────────────────────────────────────────────────────────────────────

(defun %pgo-label-position-map (instructions)
  "Return hash table label-name -> PC for INSTRUCTIONS."
  (let ((table (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'cl-cc/vm:vm-label)
               (setf (gethash (cl-cc/vm::vm-name inst) table) pc)))
    table))

(defun %pgo-block-start-pc (block instructions label->pc)
  "Resolve BLOCK's start PC in INSTRUCTIONS, preferring explicit labels."
  (or (let ((label (cl-cc/optimize:bb-label block)))
        (and label (gethash (cl-cc/vm::vm-name label) label->pc)))
      (let ((first-inst (car (cl-cc/optimize:bb-instructions block))))
        (and first-inst
             (position first-inst instructions :test #'eq)))
      0))

;;; The set of VM instruction types that are tracked as edge terminators.
;;; Extend this list to add new edge kinds without touching the dispatcher.
(defparameter *pgo-edge-kind-types*
  '(cl-cc/vm:vm-jump
    cl-cc/vm:vm-jump-zero
    cl-cc/vm:vm-call
    cl-cc/vm:vm-tail-call
    cl-cc/vm:vm-ret)
  "VM instruction types that classify a basic-block terminator for PGO edge tracking.")

(defun %pgo-edge-kind (terminator-instruction)
  "Return the edge-kind symbol for TERMINATOR-INSTRUCTION, or NIL if not tracked."
  (find-if (lambda (kind) (typep terminator-instruction kind))
           *pgo-edge-kind-types*))

(defun %pgo-plan-with-runtime-keys (instructions counter-plan)
  "Attach runtime key maps (:bb-runtime-keys/:edge-runtime-keys) to COUNTER-PLAN.

The runtime keys map logical plan IDs onto VM profile keys:
- bb-runtime-keys:   (bb-id . pc)
- edge-runtime-keys: (edge-id . (kind from-pc to-pc))"
  (if (or (null instructions) (null counter-plan))
      counter-plan
      (let* ((cfg (cl-cc/optimize:cfg-build instructions))
             (label->pc (%pgo-label-position-map instructions))
             (id->block (make-hash-table :test #'eql))
             (block->pc (make-hash-table :test #'eq))
             (inst->pc (make-hash-table :test #'eq)))
        (loop for inst in instructions
              for pc from 0
              do (setf (gethash inst inst->pc) pc))
        (dolist (block (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))
          (setf (gethash (cl-cc/optimize:bb-id block) id->block) block)
          (setf (gethash block block->pc)
                (%pgo-block-start-pc block instructions label->pc)))
        (let ((bb-runtime-keys
                (loop for (block-id . bb-id) in (getf counter-plan :bb-counters)
                      for block = (gethash block-id id->block)
                      for pc = (and block (gethash block block->pc))
                      when (and block (integerp pc))
                        collect (cons bb-id pc)))
              (edge-runtime-keys
                (loop for ((from-id . to-id) . edge-id) in (getf counter-plan :edge-counters)
                      for from-block = (gethash from-id id->block)
                      for to-block = (gethash to-id id->block)
                      for from-pc = (and from-block (gethash from-block block->pc))
                      for to-pc = (and to-block (gethash to-block block->pc))
                      for term = (and from-block
                                      (car (last (cl-cc/optimize:bb-instructions from-block))))
                      for term-pc = (and term (gethash term inst->pc))
                      for kind = (%pgo-edge-kind term)
                      when (and from-block to-block
                                kind
                                (integerp term-pc)
                                (integerp to-pc))
                        collect (cons edge-id (list kind term-pc to-pc)))))
          (append counter-plan
                  (list :bb-runtime-keys bb-runtime-keys
                        :edge-runtime-keys edge-runtime-keys))))))

(defun %build-pgo-counter-plan-from-instructions (instructions)
  "Derive a deterministic BB/edge counter plan from flat VM INSTRUCTIONS."
  (when instructions
    (let* ((cfg (cl-cc/optimize:cfg-build instructions))
           (entry (and (cl-cc/optimize:cfg-entry cfg)
                       (cl-cc/optimize:bb-id (cl-cc/optimize:cfg-entry cfg))))
            (successors
              (mapcar (lambda (block)
                        (cons (cl-cc/optimize:bb-id block)
                              (mapcar #'cl-cc/optimize:bb-id
                                      (cl-cc/optimize:bb-successors block))))
                      (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
      (%pgo-plan-with-runtime-keys
       instructions
       (cl-cc/optimize:opt-pgo-build-counter-plan entry successors)))))

(defun %pgo-type-feedback-rows (profile-data)
  "Return FR-058 type-feedback rows from PROFILE-DATA."
  (when (consp profile-data)
    (getf profile-data :type-feedback)))

(defun %pgo-dominant-types-by-pc (profile-data)
  "Return an alist of PC -> dominant specializer key for >90% type feedback sites."
  (let ((by-pc (make-hash-table :test #'eql)))
    (dolist (row (%pgo-type-feedback-rows profile-data))
      (destructuring-bind (site-key . count) row
        (when (and (consp site-key)
                   (eq (first site-key) :generic-call)
                   (integerp (second site-key))
                   (plusp count))
          (push (cons (third site-key) count)
                (gethash (second site-key) by-pc)))))
    (let ((dominant nil))
      (maphash
       (lambda (pc rows)
         (let* ((total (reduce #'+ rows :key #'cdr :initial-value 0))
                (best (car (sort (copy-list rows) #'> :key #'cdr))))
           (when (and (plusp total)
                      best
                      (> (/ (float (cdr best)) total)
                         cl-cc/vm::+ic-pgo-dominance-threshold+))
             (push (cons pc (car best)) dominant))))
       by-pc)
      dominant)))

(defun %pgo-apply-type-feedback-to-instructions (instructions profile-data)
  "Annotate generic-call instructions with PGO-dominant type keys from PROFILE-DATA."
  (when (and instructions profile-data)
    (let ((dominant (%pgo-dominant-types-by-pc profile-data)))
      (loop for inst in instructions
            for pc from 0
            for key = (cdr (assoc pc dominant :test #'eql))
            when (and key (typep inst 'cl-cc/vm:vm-generic-call))
              do (setf (cl-cc/vm::vm-pgo-specializer inst) key))))
  instructions)

(defun %pgo-apply-type-feedback-to-result (result opts)
  "Apply FR-058 type-feedback PGO annotations to RESULT in-place."
  (let ((profile-data (pipeline-opts-pgo-profile-data opts)))
    (when profile-data
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/compile:compilation-result-vm-instructions result)
       profile-data)
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/compile:compilation-result-optimized-instructions result)
       profile-data)
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/vm:vm-program-instructions
        (cl-cc/compile:compilation-result-program result))
       profile-data)))
  result)
