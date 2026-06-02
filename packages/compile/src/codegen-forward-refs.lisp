(in-package :cl-cc/compile)

;;; ── FR-138 / FR-920 forward-reference machinery ───────────────────────────
;;;
;;; Provides: *pending-exception-table-entries*, *next-exception-table-entry-order*,
;;;           *forward-reference-patch-table*, *forward-declared-functions*,
;;;           *unresolved-forward-refs*, *load-time-value-cells*,
;;;           *next-load-time-value-cell-id*,
;;;           %forward-reference-declaration-form-p,
;;;           %forward-reference-names-from-form,
;;;           %declare-forward-references-in-context,
;;;           %record-defun-resolves-forward-reference,
;;;           wasm-fixnum-range-annotation,
;;;           unresolved-forward-reference-error,
;;;           record-forward-reference,
;;;           %forward-reference-value,
;;;           resolve-forward-references,
;;;           %record-load-time-value-cell,
;;;           %compile-load-time-value-call,
;;;           %compile-nth-value-call

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

(defun %forward-reference-declaration-form-p (form)
  (and (consp form)
       (eq (car form) 'declare)
       (some (lambda (clause)
               (and (consp clause)
                    (eq (car clause) 'forward-reference)))
             (cdr form))))

(defun %forward-reference-names-from-form (form)
  (loop for clause in (cdr form)
        when (and (consp clause) (eq (car clause) 'forward-reference))
          append (remove-if-not #'symbolp (cdr clause))))

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

(defgeneric wasm-fixnum-range-annotation (object &optional context)
  (:documentation
   "FR-145 hook for Wasm fixnum range metadata.

Backends may specialize this generic function to return either NIL or a
(min . max) cons usable by the Wasm backend to skip redundant fixnum
box/unbox operations without changing the core compilation pipeline."))


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
  "Return (values value found-p) for NAME using RESOLVER.
RESOLVER may be a hash-table, function (returning 2 values), alist, NIL, or a literal value."
  (cond
    ((hash-table-p resolver) (gethash name resolver))
    ((functionp resolver)    (funcall resolver name))
    ((listp resolver)        (let ((entry (assoc name resolver :test #'equal)))
                               (values (cdr entry) (not (null entry)))))
    ((null resolver)         (values nil nil))
    (t                       (values resolver t))))

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
