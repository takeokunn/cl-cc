(in-package :cl-cc/compile)

(declaim (special cl-cc/target:*x86-64-target*))
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Entry Points + Calls + Exception Handling + Multiple Values
;;;
;;; Contains: compile-toplevel-forms, ast-call (Phase 1 + Phase 2 dispatch),
;;; exception handling (catch/throw/unwind-protect/handler-case),
;;; multiple values (values/mvb/apply/mv-call/mv-prog1),
;;; ast-function, ast-flet, ast-labels, and assembly utilities.
;;;
;;; Load order: after codegen-phase2. compile-ast defgeneric and all
;;; methods for primitives, CLOS, functions, and Phase 2 handlers live in
;;; the preceding files.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Top-level compilation entry point ────────────────────────────────────
;;; (Fold machinery is in codegen-fold.lisp, which loads before this file.)

(defstruct compilation-result
  "Result of compiling expressions or top-level forms."
  (program nil)
  (assembly nil)
  (globals nil)
  (type nil)
  (type-env nil)
  (cps nil)
  (ast nil)
  (vm-instructions nil)
  (optimized-instructions nil)
  (pgo-counter-plan nil)
  (branch-probability-hints nil :type list)
  (code-placement-hints nil :type list)
  (warnings nil :type list)
  (errors nil :type list))

;;; ── FR-138 zero-cost exception table bookkeeping ───────────────────────────

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

(defun %make-toplevel-recovery-error (form-index form condition)
  "Return a plist describing a recovered top-level compilation CONDITION."
  (list :form-index form-index
        :form form
        :condition condition
        :message (princ-to-string condition)))

(defun %copy-compiler-hash-table (table)
  "Return a shallow copy of compiler context hash TABLE."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :rehash-size (hash-table-rehash-size table)
                               :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defstruct toplevel-compilation-snapshot
  "Mutable compiler state saved before compiling one top-level form."
  instructions
  next-register
  next-label
  env
  type-env
  safety
  block-env
  tagbody-env
  global-functions
  global-function-mv-arities
  function-conventions
  global-variables
  global-var-cache
  global-classes
  global-generics
  global-generic-params
  current-function-name
  current-function-label
  current-function-params
  current-function-simple-p
  pending-inline-policy
  top-level-p
  boxed-vars
  noescape-cons-bindings
  noescape-array-bindings
  noescape-instance-bindings
  noescape-closure-bindings
  hash-table-test-bindings
  tail-position
  diagnostics
  compile-time-value-env
  compile-time-function-env
  opts)

(defun %snapshot-toplevel-compilation-state (ctx opts)
  "Snapshot CTX and dynamic top-level compilation state before one FORM."
  (make-toplevel-compilation-snapshot
   :instructions (copy-list (ctx-instructions ctx))
   :next-register (ctx-next-register ctx)
   :next-label (ctx-next-label ctx)
   :env (copy-tree (ctx-env ctx))
   :type-env (ctx-type-env ctx)
   :safety (ctx-safety ctx)
   :block-env (copy-tree (ctx-block-env ctx))
   :tagbody-env (copy-tree (ctx-tagbody-env ctx))
    :global-functions (%copy-compiler-hash-table (ctx-global-functions ctx))
    :global-function-mv-arities (%copy-compiler-hash-table (ctx-global-function-mv-arities ctx))
    :function-conventions (%copy-compiler-hash-table (ctx-function-conventions ctx))
    :global-variables (%copy-compiler-hash-table (ctx-global-variables ctx))
    :global-var-cache (copy-list (ctx-global-var-cache ctx))
    :global-classes (%copy-compiler-hash-table (ctx-global-classes ctx))
   :global-generics (%copy-compiler-hash-table (ctx-global-generics ctx))
   :global-generic-params (%copy-compiler-hash-table (ctx-global-generic-params ctx))
   :current-function-name (ctx-current-function-name ctx)
   :current-function-label (ctx-current-function-label ctx)
   :current-function-params (copy-list (ctx-current-function-params ctx))
   :current-function-simple-p (ctx-current-function-simple-p ctx)
   :pending-inline-policy (ctx-pending-inline-policy ctx)
   :top-level-p (ctx-top-level-p ctx)
   :boxed-vars (copy-list (ctx-boxed-vars ctx))
   :noescape-cons-bindings (copy-tree (ctx-noescape-cons-bindings ctx))
   :noescape-array-bindings (copy-tree (ctx-noescape-array-bindings ctx))
   :noescape-instance-bindings (copy-tree (ctx-noescape-instance-bindings ctx))
   :noescape-closure-bindings (copy-tree (ctx-noescape-closure-bindings ctx))
    :hash-table-test-bindings (copy-tree (ctx-hash-table-test-bindings ctx))
    :tail-position (ctx-tail-position ctx)
    :diagnostics (copy-list (ctx-diagnostics ctx))
    :compile-time-value-env (copy-list *compile-time-value-env*)
   :compile-time-function-env (copy-list *compile-time-function-env*)
   :opts (copy-list opts)))

(defun %restore-toplevel-compilation-state (ctx snapshot)
  "Restore CTX and dynamic state from SNAPSHOT. Returns restored compile opts."
  (setf (ctx-instructions ctx) (copy-list (toplevel-compilation-snapshot-instructions snapshot)))
  (setf (ctx-next-register ctx) (toplevel-compilation-snapshot-next-register snapshot))
  (setf (ctx-next-label ctx) (toplevel-compilation-snapshot-next-label snapshot))
  (setf (ctx-env ctx) (copy-tree (toplevel-compilation-snapshot-env snapshot)))
  (setf (ctx-type-env ctx) (toplevel-compilation-snapshot-type-env snapshot))
  (setf (ctx-safety ctx) (toplevel-compilation-snapshot-safety snapshot))
  (setf (ctx-block-env ctx) (copy-tree (toplevel-compilation-snapshot-block-env snapshot)))
  (setf (ctx-tagbody-env ctx) (copy-tree (toplevel-compilation-snapshot-tagbody-env snapshot)))
  (setf (ctx-global-functions ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-functions snapshot)))
  (setf (ctx-global-function-mv-arities ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-function-mv-arities snapshot)))
  (setf (ctx-function-conventions ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-function-conventions snapshot)))
  (setf (ctx-global-variables ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-variables snapshot)))
  (setf (ctx-global-var-cache ctx) (copy-list (toplevel-compilation-snapshot-global-var-cache snapshot)))
  (setf (ctx-global-classes ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-classes snapshot)))
  (setf (ctx-global-generics ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-generics snapshot)))
  (setf (ctx-global-generic-params ctx) (%copy-compiler-hash-table (toplevel-compilation-snapshot-global-generic-params snapshot)))
  (setf (ctx-current-function-name ctx) (toplevel-compilation-snapshot-current-function-name snapshot))
  (setf (ctx-current-function-label ctx) (toplevel-compilation-snapshot-current-function-label snapshot))
  (setf (ctx-current-function-params ctx) (copy-list (toplevel-compilation-snapshot-current-function-params snapshot)))
  (setf (ctx-current-function-simple-p ctx) (toplevel-compilation-snapshot-current-function-simple-p snapshot))
  (setf (ctx-pending-inline-policy ctx) (toplevel-compilation-snapshot-pending-inline-policy snapshot))
  (setf (ctx-top-level-p ctx) (toplevel-compilation-snapshot-top-level-p snapshot))
  (setf (ctx-boxed-vars ctx) (copy-list (toplevel-compilation-snapshot-boxed-vars snapshot)))
  (setf (ctx-noescape-cons-bindings ctx) (copy-tree (toplevel-compilation-snapshot-noescape-cons-bindings snapshot)))
  (setf (ctx-noescape-array-bindings ctx) (copy-tree (toplevel-compilation-snapshot-noescape-array-bindings snapshot)))
  (setf (ctx-noescape-instance-bindings ctx) (copy-tree (toplevel-compilation-snapshot-noescape-instance-bindings snapshot)))
  (setf (ctx-noescape-closure-bindings ctx) (copy-tree (toplevel-compilation-snapshot-noescape-closure-bindings snapshot)))
  (setf (ctx-hash-table-test-bindings ctx) (copy-tree (toplevel-compilation-snapshot-hash-table-test-bindings snapshot)))
  (setf (ctx-tail-position ctx) (toplevel-compilation-snapshot-tail-position snapshot))
  (setf (ctx-diagnostics ctx) (copy-list (toplevel-compilation-snapshot-diagnostics snapshot)))
  (setf *compile-time-value-env* (copy-list (toplevel-compilation-snapshot-compile-time-value-env snapshot)))
  (setf *compile-time-function-env* (copy-list (toplevel-compilation-snapshot-compile-time-function-env snapshot)))
  (copy-list (toplevel-compilation-snapshot-opts snapshot)))

(defun %type-check-form (ctx ast type-env type-check)
  "Run the type checker on AST in TYPE-ENV, honoring TYPE-CHECK strictness.
Returns the inferred type, or NIL on failure (structured warning recorded unless :strict)."
  (handler-case
      (type-check-ast ast type-env)
    (error (e)
      (if (eq type-check :strict)
          (error e)
          (progn
            (%emit-compile-warning ctx
                                   (format nil "Type check warning: ~A" e)
                                   :error-code "W0002")
            nil)))))

(defun %extend-type-env-for-defvar (ast type-env best-effort-type)
  "If AST is a defvar with an initializer, extend TYPE-ENV with its inferred type."
  (if (and (typep ast 'ast-defvar) (ast-defvar-value ast))
      (let ((value-type (funcall best-effort-type (ast-defvar-value ast) type-env)))
        (if value-type
            (type-env-extend (ast-defvar-name ast)
                                        (type-to-scheme value-type)
                                        type-env)
            type-env))
      type-env))

(defun %extend-type-env-for-defun (ast type-env best-effort-type)
  "If AST is a defun, extend TYPE-ENV with its inferred function type."
  (if (typep ast 'ast-defun)
      (let ((fn-type (funcall best-effort-type ast type-env)))
        (if fn-type
            (type-env-extend (ast-defun-name ast)
                                        (type-to-scheme fn-type)
                                        type-env)
            type-env))
      type-env))

(defun %maybe-extend-ct-value-env (ast)
  "If AST is a defvar with a statically evaluable initializer, push it onto
*compile-time-value-env* (side-effecting; no return value)."
  (when (and (typep ast 'ast-defvar) (ast-defvar-value ast))
    (multiple-value-bind (value ok)
        (%evaluate-ast (ast-defvar-value ast) *compile-time-eval-depth-limit*)
      (when ok
        (push (cons (ast-defvar-name ast) value) *compile-time-value-env*)))))

(defun %make-compile-opts (&key pass-pipeline speed (safety 1) (space 0) (debug 0) (inline-threshold-scale 1) block-compile 
                                    opt-bisect-limit
                                    print-pass-timings timing-stream
                                    print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                    print-pass-stats stats-stream trace-json-stream werror werror-categories)
  "Build a compilation options plist suitable for APPLYing to compile-*/optimize-* functions."
  (let ((resolved-speed (or speed (%global-optimize-quality 'speed))))
  (list :pass-pipeline       pass-pipeline
        :speed               resolved-speed
        :safety              safety
        :space               space
        :debug               debug
        :inline-threshold-scale inline-threshold-scale
        :block-compile       block-compile
        :opt-bisect-limit    opt-bisect-limit
        :print-pass-timings  print-pass-timings
        :timing-stream       timing-stream
        :print-opt-remarks   print-opt-remarks
        :opt-remarks-stream  opt-remarks-stream
        :opt-remarks-mode    opt-remarks-mode
        :print-pass-stats    print-pass-stats
        :stats-stream        stats-stream
        :trace-json-stream   trace-json-stream
        :werror              werror
        :werror-categories   werror-categories)))

(defun %ast-declarations-for-optimize-policy (ast)
  "Return declaration list associated with AST when available."
  (cond
    ((typep ast 'ast-defun) (ast-defun-declarations ast))
    ((typep ast 'ast-lambda) (ast-lambda-declarations ast))
    ((typep ast 'ast-let) (ast-let-declarations ast))
    (t nil)))

(defun %ast-forward-reference-names (ast)
  "Return forward-reference declaration names carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl) (eq (car decl) 'forward-reference))
          append (remove-if-not #'symbolp (cdr decl))))

(defun %ast-branch-probability-hints (ast)
  "Return :LIKELY/:UNLIKELY declaration hints carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl)
                  (eq (car decl) 'branch-probability)
                  (member (cadr decl) '(:likely :unlikely) :test #'eq))
          collect (cadr decl)))

(defun %ast-code-placement-hints (ast)
  "Return :HOT/:COLD code placement declarations carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl)
                  (member (car decl) '(hot cold) :test #'eq))
          collect (ecase (car decl)
                    (hot :hot)
                    (cold :cold))))

(defun %diagnostics-after-werror (diagnostics opts)
  "Split DIAGNOSTICS into warnings and promoted errors using OPTS."
  (let ((cl-cc/parse:*werror-p* (getf opts :werror))
        (cl-cc/parse:*werror-categories*
          (mapcar (lambda (category) (string-downcase (string category)))
                  (getf opts :werror-categories)))
        (warnings nil)
        (errors nil))
    (dolist (diag diagnostics)
      (let ((effective (cl-cc/parse::%maybe-promote-warning-diagnostic diag)))
        (if (eq (cl-cc/parse:diagnostic-severity effective) :error)
            (push effective errors)
            (push effective warnings))))
    (values (nreverse warnings) (nreverse errors))))

(defun %maybe-bump-opts-speed-from-ast (opts ast)
  "Merge local `(declare (optimize (speed ...)))` into OPTS conservatively.

Uses max(current-speed, local-speed) when local speed is an integer."
  (let* ((decls (%ast-declarations-for-optimize-policy ast))
         (local-speed (and decls (%local-optimize-quality decls 'speed)))
         (current-speed (getf opts :speed)))
    (when (integerp local-speed)
      (setf (getf opts :speed)
            (if (integerp current-speed)
                (max current-speed local-speed)
                local-speed)))
    opts))

(defun %maybe-compile-toplevel-form-via-cps (ast type-check safety opts)
  "Compile AST through the CPS entry path when the VM-safe subset allows it.
Returns a compilation-result or NIL when AST should stay on the direct path."
  (when (and *enable-cps-vm-primary-path*
             (not *compile-expression-cps-recursion-guard*)
             (%cps-vm-compile-safe-ast-p ast))
    (let ((cps (cps-transform-ast* ast)))
      (let ((*compile-expression-cps-recursion-guard* t))
        (apply #'compile-expression (%cps-identity-entry-form cps)
               :target :vm :type-check type-check :safety safety
               opts)))))

(defun %result-vm-instructions-without-halt (result)
  "Return RESULT's VM instructions without its terminal halt instruction."
  (labels ((scan (instructions)
             (if (consp instructions)
                 (if (consp (cdr instructions))
                     (cons (car instructions) (scan (cdr instructions)))
                     (if (typep (car instructions) 'vm-halt)
                         nil
                         (cons (car instructions) nil)))
                  nil)))
    (scan (compilation-result-vm-instructions result))))

(defun %merge-result-warnings-into-context (ctx result)
  "Merge RESULT warnings back into CTX diagnostic storage."
  (setf (ctx-diagnostics ctx)
        (append (reverse (compilation-result-warnings result))
                (ctx-diagnostics ctx))))

(defun %lower-toplevel-form-to-ast (form)
  "Expand FORM and lower it into an optimized AST node."
  (let* ((expanded (if (typep form 'ast-node)
                       form
                       (cl-cc/expand:compiler-macroexpand-all form)))
         (ast (if (typep expanded 'ast-node)
                  expanded
                  (lower-sexp-to-ast expanded))))
    (optimize-ast ast)))

(defun %record-toplevel-defun-for-ct-env (ast)
  "Register top-level function ASTs for compile-time evaluation helpers."
  (when (typep ast 'ast-defun)
    (push (cons (ast-defun-name ast) ast) *compile-time-function-env*)))

(defun %update-toplevel-type-state (ctx ast type-env type-check best-effort-type)
  "Return updated type metadata after visiting AST as a top-level form.
Values: inferred-type, updated-type-env."
  (let ((last-type nil)
        (next-type-env type-env))
    (when type-check
      (setf last-type (%type-check-form ctx ast type-env type-check)))
    (setf next-type-env (%extend-type-env-for-defvar ast next-type-env best-effort-type))
    (setf next-type-env (%extend-type-env-for-defun ast next-type-env best-effort-type))
    (values last-type next-type-env)))

(defun %compile-toplevel-ast-into-context (ast ctx target type-check opts)
  "Compile AST into CTX, preferring the CPS VM path when allowed.
Returns two values: result register and CPS form used for the AST."
  (let* ((last-cps (and (eq target :vm)
                        (%cps-vm-compile-safe-ast-p ast)
                        (cps-transform-ast* ast)))
         (cps-result (and (eq target :vm)
                          (%maybe-compile-toplevel-form-via-cps
                           ast type-check (ctx-safety ctx) opts))))
    (if cps-result
        (progn
          (setf (ctx-instructions ctx)
                (append (reverse (%result-vm-instructions-without-halt cps-result))
                        (ctx-instructions ctx)))
          (%merge-result-warnings-into-context ctx cps-result)
          (values (vm-program-result-register
                   (compilation-result-program cps-result))
                  last-cps))
        (values (compile-ast ast ctx) last-cps))))

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case with a PC→handler side table instead of push/pop ops."
  (setf (ctx-tail-position ctx) nil)
  (let* ((clauses           (ast-handler-case-clauses node))
         (result-reg        (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos     (loop repeat (length clauses)
                                   collect (list (make-label ctx "handler")
                                                 (make-register ctx))))
         (protected-form    (ast-handler-case-form node))
         (prefix            (ctx-instructions ctx))
         (form-result       (compile-ast protected-form ctx))
         (body-rev          (ldiff (ctx-instructions ctx) prefix))
         (body-instructions (nreverse (copy-list body-rev)))
         (elide-handlers-p  (%handler-case-can-elide-handlers-p
                             protected-form body-instructions)))
    (emit ctx (make-vm-move :dst result-reg :src form-result))
    (unless elide-handlers-p
      (when body-instructions
        (loop for clause in clauses
              for info in handler-infos
              do (%record-exception-table-entry
                  (first body-instructions)
                  (car (last body-instructions))
                  (first info)
                  (first clause)
                  (second info))))
      (emit ctx (make-vm-jump :label normal-exit-label))
      (loop for clause in clauses
            for info   in handler-infos
            do (let* ((var           (second clause))
                      (body          (cddr clause))
                      (handler-label (first info))
                      (error-reg     (second info))
                      (old-env       (ctx-env ctx)))
                 (emit ctx (make-vm-label :name handler-label))
                 (when var
                   (setf (ctx-env ctx) (cons (cons var error-reg) (ctx-env ctx))))
                 (let ((last-reg (if body
                                     (loop for form in body
                                           for r = (compile-ast form ctx)
                                           finally (return r))
                                     error-reg)))
                   (emit ctx (make-vm-move :dst result-reg :src last-reg)))
                 (setf (ctx-env ctx) old-env)
                 (emit ctx (make-vm-jump :label normal-exit-label))))
      (emit ctx (make-vm-label :name normal-exit-label)))
    result-reg))

(defun %top-level-in-package-form-p (form)
  "Return T when FORM is an in-package declaration skipped by top-level compilation."
  (and (consp form)
       (eq (car form) 'in-package)))

(defun %best-effort-type-check (ast env)
  "Return type of AST in ENV, or NIL when type-checking signals an error."
  (ignore-errors (type-check-ast ast env)))

(defun %process-toplevel-form (form ctx target type-env type-check safety opts compiled-asts)
  "Compile one top-level FORM and return updated compilation state.
Values: last-reg, last-type, last-cps, updated-type-env, updated-compiled-asts."
  (when (%forward-reference-declaration-form-p form)
    (%declare-forward-references-in-context
     (%forward-reference-names-from-form form) ctx)
    (return-from %process-toplevel-form
      (values nil nil nil type-env compiled-asts)))
  (let* ((ast (%lower-toplevel-form-to-ast form))
          (last-reg nil) (last-type nil) (last-cps nil))
     (setf (ctx-safety ctx) (or (%global-optimize-quality 'safety) safety))
     (%maybe-bump-opts-speed-from-ast opts ast)
    (when (typep ast 'ast-defvar)
      (setf (gethash (ast-defvar-name ast) (ctx-global-variables ctx)) t))
    (let ((forward-names (%ast-forward-reference-names ast)))
      (when forward-names
        (%declare-forward-references-in-context forward-names ctx)))
    (%record-toplevel-defun-for-ct-env ast)
    (push ast compiled-asts)
    (multiple-value-setq (last-type type-env)
      (%update-toplevel-type-state ctx ast type-env type-check #'%best-effort-type-check))
    (setf (ctx-type-env ctx) type-env)
     (%maybe-extend-ct-value-env ast)
     (multiple-value-setq (last-reg last-cps)
       (%compile-toplevel-ast-into-context ast ctx target type-check opts))
     (values last-reg last-type last-cps type-env compiled-asts)))

(defun %finalize-toplevel-compilation (ctx target last-reg last-type last-cps compiled-asts opts errors compilation-tier)
  "Finalize CTX after all top-level forms have been compiled."
  (when last-reg
    (emit ctx (make-vm-halt :reg last-reg)))
  (when *repl-capture-label-counter*
    (setf *repl-capture-label-counter* (ctx-next-label ctx)))
  (let* ((function-conventions (ctx-function-conventions ctx))
         (program-convention
           (let ((has-external-p nil)
                 (has-internal-p nil))
             (maphash (lambda (_ convention)
                        (declare (ignore _))
                        (case convention
                          (:external (setf has-external-p t))
                          (:internal (setf has-internal-p t))))
                      function-conventions)
             (if (and has-internal-p (not has-external-p)) :internal :external)))
         (raw-instructions (nreverse (ctx-instructions ctx)))
         (instructions (if (eq target :vm)
                           (%insert-osr-entry-markers raw-instructions)
                           raw-instructions))
         (optimized nil)
         (leaf-p    nil)
         (program   nil)
         (deopt-info nil)
         (osr-entry-points nil))
    (multiple-value-setq (optimized leaf-p)
      (let ((cl-cc/optimize:*skip-optimizer-passes*
               (or cl-cc/optimize:*skip-optimizer-passes*
                   (zerop compilation-tier))))
        (apply #'optimize-instructions instructions opts)))
    (multiple-value-setq (deopt-info osr-entry-points)
      (%build-deopt-info instructions))
    (setf program (make-vm-program :instructions (if (or (eq target :vm) (eq target :wasm))
                                                       instructions
                                                       (or optimized instructions))
                                    :result-register last-reg
                                    :leaf-p          leaf-p
                                     :calling-convention program-convention
                                     :function-conventions function-conventions
                                      :deopt-info deopt-info
                                      :osr-entry-points osr-entry-points
                                      :load-time-value-cells (nreverse (copy-list *load-time-value-cells*))
                                      :compilation-tier compilation-tier))
    (cl-cc/vm::vm-register-program-exception-table
     program
     (%build-exception-table (vm-program-instructions program)))
    (multiple-value-bind (warnings promoted-errors)
        (%diagnostics-after-werror (nreverse (ctx-diagnostics ctx)) opts)
      (let ((compiled-ast-list (nreverse compiled-asts)))
        (make-compilation-result :program                program
                               :assembly               (emit-assembly program :target target)
                               :globals                (ctx-global-functions ctx)
                               :type                   last-type
                               :type-env               (ctx-type-env ctx)
                                :cps                    last-cps
                                :ast                    compiled-ast-list
                                 :vm-instructions        instructions
                                 :optimized-instructions optimized
                                 :branch-probability-hints (mapcan #'%ast-branch-probability-hints (copy-list compiled-ast-list))
                                 :code-placement-hints (mapcan #'%ast-code-placement-hints (copy-list compiled-ast-list))
                                 :warnings               warnings
                                 :errors                 (append (nreverse errors) promoted-errors))))))


(defun compile-toplevel-forms (forms &key (target :x86_64) type-check (safety 1)
                                          speed (inline-threshold-scale 1)
                                          block-compile opt-bisect-limit
                                         pass-pipeline print-pass-timings timing-stream coverage
                                        print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                          print-pass-stats stats-stream trace-json-stream
                                           retpoline spectre-mitigations stack-protector shadow-stack
                                           asan msan tsan ubsan hwasan verify-transforms werror werror-categories (compilation-tier 1)
                                           &allow-other-keys)
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (declare (ignore coverage verify-transforms retpoline spectre-mitigations stack-protector shadow-stack
                    asan msan tsan ubsan hwasan))
  (let ((ctx           (make-instance 'compiler-context :safety safety :target target))
        (last-reg      nil)
        (last-type     nil)
        (last-cps      nil)
        (compiled-asts nil)
        (errors        nil)
        (*string-literal-pool* (make-hash-table :test #'equal))
         (type-env      (type-env-empty))
         (opts          (%make-compile-opts :pass-pipeline pass-pipeline
                                              :speed speed
                                              :opt-bisect-limit opt-bisect-limit
                                              :inline-threshold-scale inline-threshold-scale
                                              :block-compile block-compile
                                            :print-pass-timings print-pass-timings
                                          :timing-stream timing-stream
                                          :print-opt-remarks print-opt-remarks
                                          :opt-remarks-stream opt-remarks-stream
                                           :opt-remarks-mode opt-remarks-mode
                                             :print-pass-stats print-pass-stats
                                             :stats-stream stats-stream
                                             :trace-json-stream trace-json-stream
                                             :werror werror
                                             :werror-categories werror-categories)))
    (let ((*compile-time-value-env*    nil)
           (*compile-time-function-env* nil)
            (*pending-exception-table-entries* nil)
             (*next-exception-table-entry-order* 0)
             (*forward-reference-patch-table* (make-hash-table :test #'equal))
             (*forward-declared-functions* nil)
             (*unresolved-forward-refs* nil)
             (*load-time-value-cells* nil)
             (*next-load-time-value-cell-id* 0)
             (cl-cc/parse:*werror-p* werror)
             (cl-cc/parse:*werror-categories*
               (mapcar (lambda (category) (string-downcase (string category)))
                       werror-categories)))
       (loop for form in forms
             for form-index from 0
             do (unless (%top-level-in-package-form-p form)
                   (let ((snapshot (%snapshot-toplevel-compilation-state ctx opts))
                         (string-literal-pool-snapshot
                           (%copy-string-literal-pool *string-literal-pool*)))
                     (handler-case
                         (multiple-value-setq (last-reg last-type last-cps type-env compiled-asts)
                           (%process-toplevel-form form ctx target type-env type-check safety opts compiled-asts))
                       (error (e)
                         (setf opts (%restore-toplevel-compilation-state ctx snapshot))
                          (setf *string-literal-pool* string-literal-pool-snapshot)
                         (push (%make-toplevel-recovery-error form-index form e) errors))))))
       (setf (ctx-type-env ctx) type-env)
        (resolve-forward-references (ctx-global-functions ctx) :errorp t)
          (%finalize-toplevel-compilation ctx target last-reg last-type last-cps compiled-asts opts errors compilation-tier))))

;;; Function call compilation (%resolve-func-sym-reg, %try-compile-*,
;;; %compile-normal-call, compile-ast (ast-call)) is in codegen-calls.lisp (loads next).

;;; ─── FR-860 / FR-861 Numeric Compile-Time Helpers ─────────────────────────
;;;
;;; Append-only helpers for numeric contagion inference and inline arithmetic
;;; dispatch planning.  The existing AST binop compiler remains untouched; these
;;; functions give later lowering passes and tests a stable codegen-facing API
;;; for selecting the VM dispatch-table entry introduced in primitives.lisp.

(defparameter *codegen-inline-arith-dispatch-enabled* t
  "When true, FR-861 planning helpers may choose VM-ARITH-DISPATCH for VM code.")

(defun %codegen-normalize-contagion-type (type-designator)
  "Map compiler/CL numeric type designators to FR-860 contagion symbols."
  (cond
    ((member type-designator '(fixnum bignum integer) :test #'eq) 'integer)
    ((member type-designator '(ratio rational) :test #'eq) 'rational)
    ((eq type-designator 'single-float) 'single-float)
    ((member type-designator '(double-float float) :test #'eq) 'double-float)
    ((eq type-designator 'complex) 'complex)
    ((and (consp type-designator) (eq (first type-designator) 'complex)) 'complex)
    (t nil)))

(defun codegen-infer-numeric-contagion-type (left-type right-type)
  "Return the FR-860 result type for LEFT-TYPE × RIGHT-TYPE, or NIL if unknown."
  (let ((left (%codegen-normalize-contagion-type left-type))
        (right (%codegen-normalize-contagion-type right-type)))
    (and left right (cl-cc/vm:infer-numeric-result-type left right))))

(defun %codegen-arith-type-tag-from-type (type-designator)
  "Return the FR-861 inline dispatch type tag implied by TYPE-DESIGNATOR."
  (cond
    ((eq type-designator 'fixnum) 0)
    ((member type-designator '(bignum integer) :test #'eq) 1)
    ((member type-designator '(ratio rational) :test #'eq) 2)
    ((member type-designator '(single-float double-float float) :test #'eq) 3)
    ((or (eq type-designator 'complex)
         (and (consp type-designator) (eq (first type-designator) 'complex))) 4)
    (t nil)))

(defun codegen-inline-arith-dispatch-index (op left-type right-type)
  "Return the flattened FR-861 dispatch-table index for OP and operand types."
  (let ((left-tag (%codegen-arith-type-tag-from-type left-type))
        (right-tag (%codegen-arith-type-tag-from-type right-type)))
    (and left-tag right-tag
         (cl-cc/vm:arithmetic-dispatch-index
          (cl-cc/vm:arithmetic-op-tag op) left-tag right-tag))))

(defun codegen-inline-arith-dispatch-entry (op left-type right-type)
  "Return the VM inline arithmetic dispatch entry selected at compile time."
  (let ((index (codegen-inline-arith-dispatch-index op left-type right-type)))
    (and index (aref cl-cc/vm:*arith-dispatch-table* index))))

(defun codegen-inline-arith-dispatch-plan (op left-type right-type &key (target :vm))
  "Return a plist describing the FR-861 codegen plan for OP.

The plan records the flattened dispatch-table INDEX and selected ENTRY.  When a
specialized entry is present, VM backends can lower to VM-ARITH-DISPATCH; when
absent, callers should keep the existing generic arithmetic lowering."
  (let* ((index (codegen-inline-arith-dispatch-index op left-type right-type))
         (entry (and index (aref cl-cc/vm:*arith-dispatch-table* index)))
         (result-type (codegen-infer-numeric-contagion-type left-type right-type)))
    (list :op op
          :target target
          :left-type left-type
          :right-type right-type
          :result-type result-type
          :index index
          :entry entry
          :instruction (and *codegen-inline-arith-dispatch-enabled*
                            (eq target :vm)
                            entry
                            'cl-cc/vm:vm-arith-dispatch))))

(defun emit-inline-arith-dispatch (ctx op dst lhs-reg rhs-reg)
  "Emit a VM-ARITH-DISPATCH instruction for OP and return DST.
This helper is intentionally explicit so existing binop lowering is not changed
unless a caller opts into FR-861 dispatch lowering."
  (emit ctx (cl-cc/vm:make-vm-arith-dispatch
             :dst dst :lhs lhs-reg :rhs rhs-reg :op op))
  dst)

(export '(codegen-infer-numeric-contagion-type
          codegen-inline-arith-dispatch-index
          codegen-inline-arith-dispatch-entry
          codegen-inline-arith-dispatch-plan
           emit-inline-arith-dispatch
            *forward-reference-patch-table*
            record-forward-reference
             resolve-forward-references
             unresolved-forward-reference-error
             *unresolved-forward-refs*
             *codegen-inline-arith-dispatch-enabled*
             ;; FR-542: hot/cold code annotations
             declare-hot
             declare-cold
             cold-path))

;;; ── FR-542: Hot/Cold Code Annotations ─────────────────────────────────

(defvar *code-temperature-registry* (make-hash-table :test 'eq)
  "Maps function names to :hot or :cold temperature hints for code placement.")

(defun %declare-function-temperature (name temperature)
  "Register NAME with TEMPERATURE (:hot or :cold) for code placement.
Hot functions go to .text.hot section; cold functions to .text.cold."
  (setf (gethash name *code-temperature-registry*) temperature))

(defmacro declare-hot ()
  "FR-542: Declare the current function as hot-path.
Equivalent to GCC __attribute__((hot)). Hot functions are placed in
the .text.hot section for I-cache locality."
  `(push (cons :code-placement :hot) (compilation-result-code-placement-hints *compilation-result*)))

(defmacro declare-cold ()
  "FR-542: Declare the current function as cold-path.
Equivalent to GCC __attribute__((cold)). Cold functions are placed in
the .text.cold section, away from hot code to improve I-cache density."
  `(push (cons :code-placement :cold) (compilation-result-code-placement-hints *compilation-result*)))

(defmacro cold-path (&body body)
  "FR-542: Mark BODY as a cold execution path.
Used for error handlers and rarely-taken branches. The compiler
may outline this code to .text.cold and place it away from the
hot instruction stream."
  `(progn
     (push (cons :code-placement :cold) (compilation-result-code-placement-hints *compilation-result*))
     ,@body))
