;;;; optimizer-inline.lisp — Function Inlining Pass
;;;;
;;;; Inlines small, non-capturing, linear functions at call sites.
;;;; Eligibility: vm-closure with no captured vars, body ≤ threshold insts,
;;;;              no internal jumps (linear body ending in vm-ret).
;;;;
;;;; Algorithm:
;;;;   1. Scan once to map label → (params, linear-body).
;;;;   2. Forward scan: when a known small function's register is called,
;;;;      replace vm-call with: arg-moves + renamed body + result-move.
;;;;   Register renaming uses fresh :R<N> indices above the program's max.

(in-package :cl-cc)

(defun opt-max-reg-index (instructions)
  "Return the maximum integer N where :RN is used in INSTRUCTIONS, or -1."
  (let ((max-idx -1))
    (dolist (inst instructions max-idx)
      (dolist (reg (cons (opt-inst-dst inst) (opt-inst-read-regs inst)))
        (when (and reg (opt-register-keyword-p reg))
          (let* ((name (symbol-name reg))
                 (idx (ignore-errors (parse-integer name :start 1))))
            (when (and idx (> idx max-idx))
              (setf max-idx idx))))))))

(defun opt-make-renaming (body-instructions base-index)
  "Build renaming table: existing-register → fresh :R<N> (starting at BASE-INDEX).
   Uses opt-inst-dst + opt-inst-read-regs to discover ALL registers, including
   those not serialized by instruction->sexp (e.g., vm-make-obj initarg-regs)."
  (let ((seen (make-hash-table :test #'eq))
        (counter base-index)
        (renaming (make-hash-table :test #'eq)))
    (flet ((add (r)
             (when (and r (opt-register-keyword-p r))
               (unless (gethash r seen)
                 (setf (gethash r seen) t)
                 (setf (gethash r renaming)
                       (intern (format nil "R~A" counter) :keyword))
                 (incf counter)))))
      (dolist (inst body-instructions)
        (add (opt-inst-dst inst))
        (dolist (r (opt-inst-read-regs inst)) (add r))))
    renaming))

(defun opt-can-safely-rename-p (body-instructions)
  "T if all instructions in BODY can be safely register-renamed via sexp roundtrip.
   An instruction is safe when instruction->sexp captures all its registers —
   i.e., opt-inst-read-regs reports the same registers as appear in the sexp.
   Instructions with custom sexp methods (vm-make-obj, vm-slot-read, etc.) that
   omit registers from their sexp representation will cause this to return NIL."
  (dolist (inst body-instructions t)
    (let ((explicit-regs (remove nil
                                 (cons (opt-inst-dst inst)
                                       (opt-inst-read-regs inst))))
          (sexp-regs nil))
      (labels ((visit (x)
                 (cond ((and (keywordp x) (opt-register-keyword-p x)) (push x sexp-regs))
                       ((consp x) (visit (car x)) (visit (cdr x))))))
        (handler-case (visit (instruction->sexp inst))
          (error () (return-from opt-can-safely-rename-p nil))))
      (unless (every (lambda (r) (member r sexp-regs)) explicit-regs)
        (return-from opt-can-safely-rename-p nil)))))

(defun opt-rename-regs-in-inst (inst renaming)
  "Return INST with all VM register keywords substituted per RENAMING.
   Uses instruction→sexp roundtrip; returns INST unchanged on any error."
  (flet ((sub (x)
           (if (and (keywordp x) (opt-register-keyword-p x))
               (or (gethash x renaming) x)
               x)))
    (handler-case
        (sexp->instruction (opt-map-tree #'sub (instruction->sexp inst)))
      (error () inst))))

(defun opt-collect-function-defs (instructions)
  "Return hash-table label → (:params params :body body-insts).
   Only includes functions that are:
   - Registered via vm-closure with known params
   - Linear: no internal jumps; body ends with exactly one vm-ret"
  (let ((label-to-params (make-hash-table :test #'equal))
        (label-to-body   (make-hash-table :test #'equal))
        (in-fn nil) (cur-label nil) (cur-body nil) (has-jump nil))
    ;; Collect params from vm-closure instructions
    (dolist (inst instructions)
      (when (and (vm-closure-p inst) (vm-closure-params inst))
        (setf (gethash (vm-label-name inst) label-to-params)
              (vm-closure-params inst))))
    ;; Collect linear bodies (label → instructions ending in vm-ret)
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         ;; Nested label: abandon any in-progress body (non-linear)
         (when in-fn
           (setf in-fn nil cur-label nil cur-body nil has-jump nil))
         (setf in-fn t cur-label (vm-name inst) cur-body nil has-jump nil))
        ((or vm-jump vm-jump-zero)
         (setf has-jump t)
         (when in-fn (push inst cur-body)))
        (vm-ret
         (when (and in-fn (not has-jump))
           (push inst cur-body)
           (setf (gethash cur-label label-to-body) (nreverse cur-body)))
         (setf in-fn nil cur-label nil cur-body nil has-jump nil))
        (t (when in-fn (push inst cur-body)))))
    ;; Combine: label must appear in both tables
    ;; Also build reverse map: label → the vm-closure instruction (for captured-var check)
    (let ((label-to-closure (make-hash-table :test #'equal))
          (result (make-hash-table :test #'equal)))
      (dolist (inst instructions)
        (when (vm-closure-p inst)
          (setf (gethash (vm-label-name inst) label-to-closure) inst)))
      (maphash (lambda (lbl params)
                 (let ((body    (gethash lbl label-to-body))
                       (closure (gethash lbl label-to-closure)))
                   (when (and body closure)
                     (setf (gethash lbl result)
                           (list :closure closure :params params :body body)))))
               label-to-params)
      result)))

(defun opt-body-has-global-refs-p (body-instructions params)
  "Return T if BODY-INSTRUCTIONS read any register that is neither in PARAMS
   nor defined as a DST by a prior body instruction.  Such 'global registers'
   (e.g., class descriptors set by defclass at the top level) would be renamed
   to fresh uninitialized registers if the function were inlined, breaking
   correctness.  Functions with global refs must not be inlined."
  (let ((safe (make-hash-table :test #'eq)))
    (dolist (p params) (setf (gethash p safe) t))
    (dolist (inst body-instructions nil)
      (dolist (r (opt-inst-read-regs inst))
        (unless (gethash r safe)
          (return-from opt-body-has-global-refs-p t)))
      (let ((dst (opt-inst-dst inst)))
        (when dst (setf (gethash dst safe) t))))))

(defun opt-build-function-name-map (instructions)
  "Return symbol → function-label mapping for top-level function registrations."
  (let ((reg-track (make-hash-table :test #'eq))
        (name-to-label (make-hash-table :test #'eq)))
    (dolist (inst instructions)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (when label
             (setf (gethash (vm-dst inst) reg-track) label))))
        (vm-register-function
         (let ((label (or (gethash (vm-src inst) reg-track)
                          (dolist (i instructions)
                            (when (and (vm-closure-p i)
                                       (eq (vm-dst i) (vm-src inst)))
                              (return (vm-label-name i)))))))
            (when label
              (setf (gethash (vm-func-name inst) name-to-label) label))))))
    name-to-label))

(defun opt-known-callee-labels (instructions)
  "Return reg -> known callee label mapping tracked through simple designators." 
  (let ((name-to-label (opt-build-function-name-map instructions))
        (reg-track (make-hash-table :test #'eq)))
    (dolist (inst instructions reg-track)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (setf (gethash (vm-dst inst) reg-track) (vm-label-name inst)))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if label
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label found-p)
             (gethash (vm-move-src inst) reg-track)
           (if found-p
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track))))))))

(defun opt-known-callee-label (reg known-callee-labels)
  "Return the known callee label tracked for REG, or NIL." 
  (gethash reg known-callee-labels))

(defun opt-build-call-graph (instructions func-defs name-to-label)
  "Return label → direct callees graph for the linear function bodies in FUNC-DEFS."
  (let ((graph (make-hash-table :test #'equal)))
    (maphash (lambda (label def)
               (let ((body (getf def :body))
                     (reg-track (make-hash-table :test #'eq))
                     (callees nil))
                 (dolist (inst body)
                   (typecase inst
                     ((or vm-closure vm-func-ref)
                      (let ((callee (vm-label-name inst)))
                        (when (gethash callee func-defs)
                          (setf (gethash (vm-dst inst) reg-track) callee))))
                     (vm-const
                      (let ((callee (and (symbolp (vm-value inst))
                                         (gethash (vm-value inst) name-to-label))))
                        (when callee
                          (setf (gethash (vm-dst inst) reg-track) callee))))
                     (vm-call
                      (let ((callee (gethash (vm-func-reg inst) reg-track)))
                        (when callee
                          (pushnew callee callees :test #'equal))))
                     (t
                      (let ((dst (opt-inst-dst inst)))
                        (when dst (remhash dst reg-track))))))
                 (setf (gethash label graph) callees)))
             func-defs)
    graph))

(defun opt-call-graph-recursive-labels (graph)
  "Return the set of labels that are in a recursive SCC of GRAPH."
  (let ((recursive (make-hash-table :test #'equal)))
    (labels ((reaches-self-p (start node seen)
               (dolist (next (gethash node graph) nil)
                 (cond
                   ((equal next start) (return-from reaches-self-p t))
                   ((not (gethash next seen))
                    (setf (gethash next seen) t)
                    (when (reaches-self-p start next seen)
                      (return-from reaches-self-p t)))))))
      (maphash (lambda (label _callees)
                 (when (reaches-self-p label label (make-hash-table :test #'equal))
                   (setf (gethash label recursive) t)))
                graph))
    recursive))

(defun opt-function-body-transitively-pure-p (body func-defs name-to-label pure-labels)
  "Return T if BODY is transitively pure under PURE-LABELS.

Known direct calls are allowed only when their callee label is already marked
pure in PURE-LABELS. Unknown calls remain conservative and therefore impure."
  (let ((reg-track (make-hash-table :test #'eq)))
    (dolist (inst body t)
      (typecase inst
        ((or vm-closure vm-func-ref)
         (let ((label (vm-label-name inst)))
           (if (gethash label func-defs)
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-const
         (let ((label (and (symbolp (vm-value inst))
                           (gethash (vm-value inst) name-to-label))))
           (if (and label (gethash label func-defs))
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        (vm-move
         (multiple-value-bind (label present-p)
             (gethash (vm-src inst) reg-track)
           (if present-p
               (setf (gethash (vm-dst inst) reg-track) label)
               (remhash (vm-dst inst) reg-track))))
        ((or vm-call vm-tail-call)
         (let ((callee (gethash (vm-func-reg inst) reg-track)))
           (unless (and callee (gethash callee pure-labels))
             (return-from opt-function-body-transitively-pure-p nil))
           (when (opt-inst-dst inst)
             (remhash (opt-inst-dst inst) reg-track))))
        (t
         (unless (or (typep inst 'vm-ret)
                     (typep inst 'vm-label)
                     (opt-inst-pure-p inst))
           (return-from opt-function-body-transitively-pure-p nil))
         (let ((dst (opt-inst-dst inst)))
           (when dst
             (remhash dst reg-track))))))))

(defun opt-infer-transitive-function-purity (instructions)
  "Infer a conservative set of transitively pure function labels.

The inference covers known direct-call edges between collected linear function
bodies. Recursive SCCs remain conservative and are not marked pure."
  (let* ((func-defs (opt-collect-function-defs instructions))
         (name-to-label (opt-build-function-name-map instructions))
         (graph (opt-build-call-graph instructions func-defs name-to-label))
         (recursive-labels (opt-call-graph-recursive-labels graph))
         (pure-labels (make-hash-table :test #'equal))
         (changed t))
    (loop while changed
          do (setf changed nil)
             (maphash (lambda (label def)
                        (unless (or (gethash label pure-labels)
                                    (gethash label recursive-labels))
                          (when (opt-function-body-transitively-pure-p
                                 (getf def :body) func-defs name-to-label pure-labels)
                            (setf (gethash label pure-labels) t
                                  changed t))))
                      func-defs))
    pure-labels))

