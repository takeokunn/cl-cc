;;;; optimizer-purity.lisp — Transitive Function Purity Analysis
;;;;
;;;; Call graph construction and recursive SCC detection for inlining eligibility.
;;;; These functions are called by optimizer-inline-pass.lisp to determine which
;;;; functions can be safely inlined (non-recursive, transitively pure).
;;;;
;;;; Load order: after optimizer-inline.lisp.

(in-package :cl-cc/optimize)

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

(defun %opt-call-graph-reaches-self-p (graph start node seen)
  "Return T if NODE can reach START via GRAPH edges, using SEEN to avoid cycles."
  (dolist (next (gethash node graph) nil)
    (cond
      ((equal next start) (return-from %opt-call-graph-reaches-self-p t))
      ((not (gethash next seen))
       (setf (gethash next seen) t)
       (when (%opt-call-graph-reaches-self-p graph start next seen)
         (return-from %opt-call-graph-reaches-self-p t))))))

(defun opt-call-graph-recursive-labels (graph)
  "Return the set of labels that are in a recursive SCC of GRAPH."
  (let ((recursive (make-hash-table :test #'equal)))
    (maphash (lambda (label _callees)
               (when (%opt-call-graph-reaches-self-p graph label label (make-hash-table :test #'equal))
                 (setf (gethash label recursive) t)))
             graph)
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
