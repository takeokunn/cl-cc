(in-package :cl-cc/compile)

;;; ── Codegen bookkeeping shared across emitters and loaders ──────────────

(defvar *pending-exception-table-entries* nil
  "Compile-time handler range descriptors awaiting final PC resolution.")

(defvar *next-exception-table-entry-order* 0
  "Monotonic source-order counter for exception-table clause tie breaking.")

(defvar *load-time-value-cells* nil
  "Load-time-value cells recorded while compiling one toplevel program.")

(defvar *next-load-time-value-cell-id* 0
  "Monotonic load-time-value cell id for the current compilation unit.")

(defvar *forward-reference-patch-table* (make-hash-table :test #'equal)
  "FR-920 table mapping unresolved reference names to pending fixup plists.")

(defvar *forward-declared-functions* nil
  "Function names declared by `(declare (forward-reference ...))' in this compilation.")

(defvar *unresolved-forward-refs* nil
  "Compile-time mirror of forward-declared function names awaiting DEFUN.")

(defun %forward-reference-names-from-form (form)
  (when (and (consp form)
             (eq (car form) 'declare))
    (loop for clause in (cdr form)
          when (and (consp clause) (eq (car clause) 'forward-reference))
            append (remove-if-not #'symbolp (cdr clause)))))

(defun %declare-forward-references-in-context (names ctx)
  "Record NAMES as forward references in CTX and emit runtime cell allocation."
  (dolist (name names)
    (pushnew name *forward-declared-functions* :test #'eq)
    (pushnew (cons name nil) *unresolved-forward-refs* :key #'car :test #'eq)
    (setf (gethash name (ctx-global-functions ctx)) :forward-reference)
    (emit ctx (make-vm-declare-forward-reference :name name)))
  names)

(defun %record-defun-resolves-forward-reference (name)
  (when (member name *forward-declared-functions* :test #'eq)
    (setf *unresolved-forward-refs*
          (remove name *unresolved-forward-refs* :key #'car :test #'eq))))

(define-condition unresolved-forward-reference-error (error)
  ((references :initarg :references :reader unresolved-forward-reference-error-references))
  (:report (lambda (c s)
             (format s "Unresolved forward references at EOF: ~{~S~^, ~}"
                     (unresolved-forward-reference-error-references c)))))

(defun record-forward-reference (name location &key kind patch-fn metadata)
  "Record a pending forward reference NAME at LOCATION.
PATCH-FN, when supplied, is called as (PATCH-FN NAME VALUE FIXUP) by
RESOLVE-FORWARD-REFERENCES once NAME is defined."
  (let ((fixup (list :name name
                     :location location
                     :kind kind
                     :patch-fn patch-fn
                     :metadata metadata)))
    (push fixup (gethash name *forward-reference-patch-table*))
    fixup))

(defun %forward-reference-value (resolver name)
  (cond
    ((hash-table-p resolver) (multiple-value-bind (value present-p) (gethash name resolver)
                              (values value present-p)))
    ((functionp resolver) (multiple-value-bind (value present-p) (funcall resolver name)
                          (values value present-p)))
    ((listp resolver) (let ((entry (assoc name resolver :test #'equal)))
                        (values (cdr entry) (not (null entry)))))
    ((null resolver) (values nil nil))
    (t (values resolver t))))

(defun resolve-forward-references (&optional resolver &key (errorp t))
  "Resolve all pending forward references using RESOLVER.
RESOLVER may be a hash table, alist, function, or a single replacement value.
Signals UNRESOLVED-FORWARD-REFERENCE-ERROR when ERRORP and refs remain."
  (when (and (null resolver)
             (zerop (hash-table-count *forward-reference-patch-table*))
             (boundp 'cl-cc/vm::*vm-current-state*)
             cl-cc/vm::*vm-current-state*)
    (return-from resolve-forward-references
      (cl-cc/vm:vm-resolve-forward-references cl-cc/vm::*vm-current-state*)))
  (let ((unresolved nil)
        (resolved nil))
    (maphash
     (lambda (name fixups)
       (multiple-value-bind (value present-p) (%forward-reference-value resolver name)
         (if present-p
             (progn
               (dolist (fixup fixups)
                 (let ((patch-fn (getf fixup :patch-fn)))
                   (when patch-fn
                     (funcall patch-fn name value fixup))))
               (push name resolved))
             (push name unresolved))))
     *forward-reference-patch-table*)
    (dolist (name resolved)
      (remhash name *forward-reference-patch-table*))
    (when (and errorp unresolved)
      (error 'unresolved-forward-reference-error :references (nreverse unresolved)))
    (values (nreverse resolved) (nreverse unresolved))))

(defun %record-load-time-value-cell (form read-only-p)
  "Record FORM for load-time execution and return its cell id."
  (let* ((id (prog1 *next-load-time-value-cell-id*
               (incf *next-load-time-value-cell-id*)))
         (cell (cl-cc/vm::make-vm-load-time-value-cell
                :id id
                :form form
                :read-only-p read-only-p)))
    (push cell *load-time-value-cells*)
    id))

(defun %compile-load-time-value-call (args result-reg ctx)
  "Compile (LOAD-TIME-VALUE form &optional read-only-p) as a constant cell load."
  (when (and (>= (length args) 1) (<= (length args) 2))
    (let* ((form (ast-to-sexp (first args)))
           (read-only-p (and (second args)
                             (let ((sexp (ast-to-sexp (second args))))
                               (not (null sexp)))))
           (cell-id (%record-load-time-value-cell form read-only-p)))
      (emit ctx (make-vm-load-time-value :dst result-reg :cell-id cell-id))
      result-reg)))

(defun %compile-nth-value-call (args result-reg ctx)
  "Compile (NTH-VALUE n form) with O(1) MV buffer access when N is constant."
  (when (= (length args) 2)
    (let ((index-sexp (ast-to-sexp (first args))))
      (when (and (integerp index-sexp) (<= 0 index-sexp))
        (emit ctx (make-vm-clear-values))
        (let ((primary-reg (compile-ast (second args) ctx)))
          (emit ctx (make-vm-ensure-values :src primary-reg))
          (emit ctx (make-vm-nth-value :dst result-reg :index index-sexp))
          result-reg)))))

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
    (when cl-cc/vm:*deopt-enabled*
      (loop for inst in instructions
            for pc from 0
            do (cond
                 ((typep inst 'cl-cc/vm::vm-type-check)
                  (setf (gethash pc table)
                        (cl-cc/vm::make-vm-deopt-info
                         :pc pc
                         :label (cl-cc/vm::vm-type-check-deopt-label inst)
                         :live-regs (list (vm-src inst))
                         :vreg->preg (list (cons (vm-src inst) :p0))
                         :description (list :type-check (cl-cc/vm::vm-type-name inst)
                                            :id (cl-cc/vm::vm-type-check-deopt-id inst)))))
                 ((typep inst 'cl-cc/vm::vm-deopt)
                  (setf (gethash pc table)
                        (cl-cc/vm::make-vm-deopt-info
                         :pc pc
                         :label (cl-cc/vm::vm-deopt-label inst)
                         :vreg->preg nil
                         :inline-stack nil
                         :description (list :deopt (cl-cc/vm::vm-deopt-reason inst)
                                            :id (cl-cc/vm::vm-deopt-id inst))))))))
    (when cl-cc/vm:*osr-enabled*
      (loop for inst in instructions
            for pc from 0
            do (when (typep inst 'cl-cc/vm::vm-osr-entry)
                 (push (list :pc pc
                             :label (cl-cc/vm::vm-osr-label inst)
                             :id (cl-cc/vm::vm-osr-id inst))
                       osr))))
    (values table (nreverse osr))))
