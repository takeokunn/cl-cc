(in-package :cl-cc/codegen)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Parameter-List Helpers
;;;
;;; Extracted from codegen-functions.lisp.
;;; Contains:
;;;   - *non-constant-default-sentinel*   — sentinel for deferred defaults
;;;   - extract-constant-value            — extract compile-time constant from AST
;;;   - allocate-defaulting-params        — allocate &optional/&key registers + metadata
;;;   - allocate-extended-params          — full extended lambda list allocation
;;;   - *rest-stack-alloc-safe-consumers* — whitelist for dynamic-extent &rest
;;;   - rest-param-stack-alloc-p          — heuristic for safe stack allocation
;;;   - dynamic-extent-declared-p         — check for (declare (dynamic-extent ...))
;;;   - emit-supplied-p-checks            — emit sentinel comparison for supplied-p vars
;;;   - emit-non-constant-defaults        — emit inline default computation code
;;;   - build-all-param-bindings          — combine all parameter name→register pairs
;;;   - function-param-type-bindings      — look up typed signature for a function
;;;   - compile-function-body             — bind params + emit body + restore env
;;;
;;; Typed parameter machinery and defmacro compilation are in
;;; codegen-functions.lisp (loads before this file).
;;; Load order: after codegen-functions.lisp, before codegen-functions-emit.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Parameter-list helpers ───────────────────────────────────────────────

(defvar *non-constant-default-sentinel* :__unsupplied__
  "Sentinel value used for non-constant &optional/&key defaults.
When the VM sees this as the default, the actual default is computed inline.")

(defun extract-constant-value (ast-node)
  "Extract a constant value from an AST node for use as a default parameter value.
Returns (values value is-constant-p).  IS-CONSTANT-P is true for compile-time constants."
  (typecase ast-node
    (ast-int (values (ast-int-value ast-node) t))
    (ast-quote (values (ast-quote-value ast-node) t))
    (ast-var (let ((name (ast-var-name ast-node)))
               (cond ((eq name 't) (values t t))
                     ((eq name 'nil) (values nil t))
                     (t (values nil nil)))))
    (t (values nil nil))))

(defun allocate-defaulting-params (ctx params make-entry)
  "Allocate registers for PARAMS with optional defaults.
MAKE-ENTRY is called as (name reg default-val) and returns the closure-data entry.
Returns (values closure-data bindings non-constant-defaults supplied-p-entries)."
  (let ((closure-data nil) (bindings nil) (non-constant-defaults nil)
        (supplied-p-entries nil))
    (dolist (param params)
      (let* ((name (first param))
             (default-ast (second param))
             (supplied-p-name (third param))  ; FR-696: supplied-p variable
             (reg (make-register ctx)))
        (multiple-value-bind (default-val is-constant)
            (if default-ast (extract-constant-value default-ast) (values nil t))
          (if (and default-ast (not is-constant))
              (progn
                (push (funcall make-entry name reg *non-constant-default-sentinel*) closure-data)
                (push (cons reg default-ast) non-constant-defaults))
              (push (funcall make-entry name reg default-val) closure-data)))
        (push (cons name reg) bindings)
        ;; FR-696: allocate register for supplied-p variable
        (when supplied-p-name
          (let ((sp-reg (make-register ctx)))
            (push (cons supplied-p-name sp-reg) bindings)
            (push (cons sp-reg reg) supplied-p-entries)))))
    (values (nreverse closure-data) (nreverse bindings)
            (nreverse non-constant-defaults) (nreverse supplied-p-entries))))

(defun allocate-extended-params (ctx optional-params rest-param key-params)
  "Allocate registers and build metadata for extended lambda list parameters.
Returns (values opt-closure-data rest-reg key-closure-data opt-bindings
                 rest-binding key-bindings non-constant-defaults supplied-p-entries)."
  (let ((opt-closure-data nil) (opt-bindings nil)
        (rest-reg nil) (rest-binding nil)
        (key-closure-data nil) (key-bindings nil)
        (non-constant-defaults nil) (supplied-p-entries nil))
    (when optional-params
      (multiple-value-bind (cd binds ncds sp-entries)
          (allocate-defaulting-params ctx optional-params
                                      (lambda (name reg val)
                                        (declare (ignore name)) (list reg val)))
        (setf opt-closure-data cd  opt-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))
        (setf supplied-p-entries (nconc supplied-p-entries sp-entries))))
    (when rest-param
      (setf rest-reg (make-register ctx))
      (setf rest-binding (cons rest-param rest-reg)))
    (when key-params
      (multiple-value-bind (cd binds ncds sp-entries)
          (allocate-defaulting-params ctx key-params
                                      (lambda (name reg val)
                                        (list (intern (symbol-name name) "KEYWORD") reg val)))
        (setf key-closure-data cd  key-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))
        (setf supplied-p-entries (nconc supplied-p-entries sp-entries))))
    (values opt-closure-data rest-reg key-closure-data
            opt-bindings rest-binding key-bindings non-constant-defaults
            supplied-p-entries)))

(defparameter *rest-stack-alloc-safe-consumers*
  '("CAR" "CDR" "CAAR" "CADR" "CDAR" "CDDR" "CAAAR" "CAADR" "CADAR" "CADDR"
    "CDAAR" "CDADR" "CDDAR" "CDDDR" "FIRST" "SECOND" "THIRD" "FOURTH" "FIFTH"
    "SIXTH" "SEVENTH" "EIGHTH" "NINTH" "TENTH" "CONS" "LIST" "LIST*" "LENGTH"
    "NULL" "CONSP" "LISTP" "ATOM" "ENDP" "NOT" "EQ" "EQL" "EQUAL" "EQUALP"
    "MEMBER" "ASSOC" "FIND" "POSITION" "COUNT" "REMOVE" "REMOVE-IF"
    "REMOVE-IF-NOT" "REMOVE-DUPLICATES" "REVERSE" "NREVERSE" "SORT"
    "STABLE-SORT" "REDUCE" "EVERY" "SOME" "NOTANY" "NOTEVERY"
    "MAPCAR" "MAPC" "MAPCAN" "MAPL" "MAPLIST" "MAPCON")
  "Functions that consume a &rest list without causing it to escape.")

(defun rest-param-stack-alloc-p (body-forms rest-name)
  "Return T when REST-NAME is only used in stack-safe contexts within BODY-FORMS.
This is a conservative heuristic used to mark &rest lists dynamic-extent."
  (and (consp body-forms)
        (not (binding-escapes-in-body-p body-forms rest-name
                                        :safe-consumers *rest-stack-alloc-safe-consumers*))))

(defun dynamic-extent-declared-p (declarations name)
  "Return T when DECLARATIONS contain (dynamic-extent NAME)."
  (some (lambda (decl)
          (and (consp decl)
               (eq (car decl) 'dynamic-extent)
               (member name (cdr decl) :test #'eq)))
        declarations))

(defun emit-supplied-p-checks (ctx supplied-p-entries)
  "Emit code to set supplied-p registers based on sentinel comparison.
Each entry is (supplied-p-reg . param-reg). Emits: sp-reg = (param-reg != sentinel)."
  (when supplied-p-entries
    (let ((sentinel-reg (make-register ctx)))
      (emit ctx (make-vm-const :dst sentinel-reg :value *non-constant-default-sentinel*))
      (dolist (entry supplied-p-entries)
        (let* ((sp-reg (car entry))
               (param-reg (cdr entry))
               (eq-reg (make-register ctx))
               (was-supplied-label (make-label ctx "SP_YES"))
               (sp-done-label (make-label ctx "SP_DONE")))
          ;; Compare param-reg with sentinel
          (emit ctx (make-vm-eq :dst eq-reg :lhs param-reg :rhs sentinel-reg))
          ;; If eq=0 (not equal to sentinel = was supplied), jump to SP_YES
          (emit ctx (make-vm-jump-zero :reg eq-reg :label was-supplied-label))
          ;; Was sentinel → not supplied → sp-reg = NIL
          (emit ctx (make-vm-const :dst sp-reg :value nil))
          (emit ctx (make-vm-jump :label sp-done-label))
          ;; Was supplied → sp-reg = T
          (emit ctx (make-vm-label :name was-supplied-label))
          (emit ctx (make-vm-const :dst sp-reg :value t))
          (emit ctx (make-vm-label :name sp-done-label)))))))

(defun emit-non-constant-defaults (ctx non-constant-defaults)
  "Emit inline code to compute non-constant default parameter values.
For each (register . ast-node), emits: if register eq sentinel, register = compile(ast)."
  (dolist (entry non-constant-defaults)
    (let* ((param-reg (car entry))
           (default-ast (cdr entry))
           (sentinel-reg (make-register ctx))
           (cmp-reg (make-register ctx))
           (skip-label (make-label ctx "DEFAULT_SKIP")))
      (emit ctx (make-vm-const :dst sentinel-reg :value *non-constant-default-sentinel*))
      (emit ctx (make-vm-eq :dst cmp-reg :lhs param-reg :rhs sentinel-reg))
      (emit ctx (make-vm-jump-zero :reg cmp-reg :label skip-label))
      (let ((default-reg (compile-ast default-ast ctx)))
        (emit ctx (make-vm-move :dst param-reg :src default-reg)))
      (emit ctx (make-vm-label :name skip-label)))))

(defun build-all-param-bindings (params param-regs opt-bindings rest-binding key-bindings)
  "Build the combined alist of (name . register) for all lambda list parameters."
  (let ((bindings nil))
    (loop for param in params
          for param-reg in param-regs
          do (push (cons param param-reg) bindings))
    (dolist (b opt-bindings) (push b bindings))
    (when rest-binding (push rest-binding bindings))
    (dolist (b key-bindings) (push b bindings))
    (nreverse bindings)))

(defun function-param-type-bindings (name params)
  "Return an alist of (param . type-scheme) for NAME's typed parameters.
Falls back to NIL when NAME was not registered with a typed signature."
  (multiple-value-bind (signature found-p)
      (gethash name cl-cc/expand:*function-type-registry*)
    (when found-p
      (let ((param-types (car signature)))
        (loop for param in params
              for param-type in param-types
              collect (cons param (cl-cc/type:type-to-scheme param-type)))))))

(defun compile-function-body (ctx params param-regs opt-bindings rest-binding
                              key-bindings non-constant-defaults body
                              &optional supplied-p-entries type-bindings
                              current-function-name current-function-label)
  "Bind parameters, emit supplied-p checks, non-constant defaults, compile BODY, emit vm-ret.
Saves and restores the compiler environment around the body via unwind-protect."
  (let ((old-env (ctx-env ctx))
        (old-type-env (ctx-type-env ctx))
        (old-tail (ctx-tail-position ctx))
        (old-current-function-name (ctx-current-function-name ctx))
        (old-current-function-label (ctx-current-function-label ctx))
        (old-current-function-params (ctx-current-function-params ctx))
        (old-current-function-simple-p (ctx-current-function-simple-p ctx)))
    (unwind-protect
         (progn
           (setf (ctx-env ctx)
                 (append (build-all-param-bindings params param-regs
                                                    opt-bindings rest-binding key-bindings)
                         (ctx-env ctx)))
           (setf (ctx-current-function-name ctx) current-function-name
                 (ctx-current-function-label ctx) current-function-label
                 (ctx-current-function-params ctx) params
                 (ctx-current-function-simple-p ctx)
                 (and current-function-name
                      (null opt-bindings)
                      (null rest-binding)
                      (null key-bindings)))
           (when type-bindings
             (setf (ctx-type-env ctx)
                   (cl-cc/type:type-env-extend* type-bindings (ctx-type-env ctx))))
           (when supplied-p-entries
             (emit-supplied-p-checks ctx supplied-p-entries))
           (when non-constant-defaults
             (emit-non-constant-defaults ctx non-constant-defaults))
           (let ((last-reg nil))
             (dolist (form body)
               (setf (ctx-tail-position ctx)
                     (if (eq form (car (last body))) t nil))
               (setf last-reg (compile-ast form ctx)))
             (setf (ctx-tail-position ctx) nil)
             (emit ctx (make-vm-ret :reg last-reg))))
        (setf (ctx-env ctx) old-env)
        (setf (ctx-type-env ctx) old-type-env)
        (setf (ctx-tail-position ctx) old-tail)
        (setf (ctx-current-function-name ctx) old-current-function-name
              (ctx-current-function-label ctx) old-current-function-label
              (ctx-current-function-params ctx) old-current-function-params
              (ctx-current-function-simple-p ctx) old-current-function-simple-p))))
