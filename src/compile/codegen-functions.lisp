(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Function Compilation
;;;
;;; Contains: defmacro, lambda, defun, defvar/defparameter compilation, plus
;;; all parameter-list helpers (optional/rest/key allocation, boxing, defaults)
;;; and the typed-parameter machinery (type registry, strip-typed-params).
;;;
;;; Load order: after codegen-clos, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Typed parameter machinery ────────────────────────────────────────────
;;;
;;; These are also called by expand/expander.lisp (expand-typed-defun-or-lambda).
;;; They live here because they describe compile-time type semantics.

(defvar *function-type-registry* (make-hash-table :test #'eq)
  "Maps function names to their declared type signatures.
Each entry is (param-types . return-type) where param-types is a list of
type-node objects and return-type is a type-node.")

(defun register-function-type (name param-types return-type)
  "Register a typed function signature for type checking."
  (setf (gethash name *function-type-registry*) (cons param-types return-type)))

(defun lambda-list-has-typed-p (params)
  "Return T if required PARAMS contain typed syntax like ((x fixnum) (y string)).
Only checks params before &optional/&rest/&key.
Also recognizes registered type aliases."
  (and (listp params)
       (loop for p in params
             when (and (symbolp p)
                       (member p '(&optional &rest &key &body &allow-other-keys)))
               return nil
             thereis (and (consp p)
                          (= (length p) 2)
                          (symbolp (first p))
                          (let ((type-spec (second p)))
                            (or (and (symbolp type-spec)
                                     (or (member type-spec
                                                 '(fixnum integer int string boolean bool
                                                   symbol cons null t number float
                                                   character list vector array
                                                   hash-table function sequence))
                                         (cl-cc/type:lookup-type-alias type-spec)))
                                (and (consp type-spec)
                                     (member (car type-spec)
                                             '(or and function values cons list vector array)))))))))

(defun expand-type-alias (type-spec)
  "Expand type aliases in TYPE-SPEC recursively."
  (if (symbolp type-spec)
      (let ((expanded (cl-cc/type:lookup-type-alias type-spec)))
        (if expanded (expand-type-alias expanded) type-spec))
      type-spec))

(defun strip-typed-params (params)
  "Strip type annotations from typed params.
((x fixnum) (y string) z) → (values (x y z) ((x . fixnum) (y . string)))"
  (let ((plain nil) (type-alist nil))
    (dolist (p params)
      (cond
        ((and (symbolp p)
              (member p '(&optional &rest &key &body &allow-other-keys)))
         (push p plain))
        ((and (consp p) (= (length p) 2) (symbolp (first p)))
         (push (first p) plain)
         (push (cons (first p) (expand-type-alias (second p))) type-alist))
        ((symbolp p) (push p plain))
        (t (push p plain))))
    (values (nreverse plain) (nreverse type-alist))))

;;; ── defmacro ─────────────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-defmacro) ctx)
  "Compile a top-level macro definition.
Registers the macro expander at compile time so subsequent forms can use it.
At runtime, defmacro evaluates to the macro name."
  (let ((name (ast-defmacro-name node))
        (lambda-list (ast-defmacro-lambda-list node))
        (body (ast-defmacro-body node)))
    (register-macro name (make-macro-expander lambda-list body))
    (let ((dst (make-register ctx)))
      (emit ctx (make-vm-const :dst dst :value name))
      dst)))

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
Returns (values closure-data bindings non-constant-defaults)."
  (let ((closure-data nil) (bindings nil) (non-constant-defaults nil))
    (dolist (param params)
      (let* ((name (first param))
             (default-ast (second param))
             (reg (make-register ctx)))
        (multiple-value-bind (default-val is-constant)
            (if default-ast (extract-constant-value default-ast) (values nil t))
          (if (and default-ast (not is-constant))
              (progn
                (push (funcall make-entry name reg *non-constant-default-sentinel*) closure-data)
                (push (cons reg default-ast) non-constant-defaults))
              (push (funcall make-entry name reg default-val) closure-data)))
        (push (cons name reg) bindings)))
    (values (nreverse closure-data) (nreverse bindings) (nreverse non-constant-defaults))))

(defun allocate-extended-params (ctx optional-params rest-param key-params)
  "Allocate registers and build metadata for extended lambda list parameters.
Returns (values opt-closure-data rest-reg key-closure-data opt-bindings
                rest-binding key-bindings non-constant-defaults)."
  (let ((opt-closure-data nil) (opt-bindings nil)
        (rest-reg nil) (rest-binding nil)
        (key-closure-data nil) (key-bindings nil)
        (non-constant-defaults nil))
    (when optional-params
      (multiple-value-bind (cd binds ncds)
          (allocate-defaulting-params ctx optional-params
                                      (lambda (name reg val)
                                        (declare (ignore name)) (list reg val)))
        (setf opt-closure-data cd  opt-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))))
    (when rest-param
      (setf rest-reg (make-register ctx))
      (setf rest-binding (cons rest-param rest-reg)))
    (when key-params
      (multiple-value-bind (cd binds ncds)
          (allocate-defaulting-params ctx key-params
                                      (lambda (name reg val)
                                        (list (intern (symbol-name name) "KEYWORD") reg val)))
        (setf key-closure-data cd  key-bindings binds)
        (setf non-constant-defaults (nconc non-constant-defaults ncds))))
    (values opt-closure-data rest-reg key-closure-data
            opt-bindings rest-binding key-bindings non-constant-defaults)))

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

(defun compile-function-body (ctx params param-regs opt-bindings rest-binding
                              key-bindings non-constant-defaults body)
  "Bind parameters, emit non-constant defaults, compile BODY, emit vm-ret.
Saves and restores the compiler environment around the body via unwind-protect."
  (let ((old-env (ctx-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-env ctx)
                 (append (build-all-param-bindings params param-regs
                                                   opt-bindings rest-binding key-bindings)
                         (ctx-env ctx)))
           (when non-constant-defaults
             (emit-non-constant-defaults ctx non-constant-defaults))
           (let ((last-reg nil))
             (dolist (form body)
               (setf last-reg (compile-ast form ctx)))
             (emit ctx (make-vm-ret :reg last-reg))))
      (setf (ctx-env ctx) old-env))))

;;; ── Lambda and defun ─────────────────────────────────────────────────────

(defun %emit-closure-body (ctx func-label end-label params param-regs
                           opt-bindings rest-binding key-bindings
                           non-constant-defaults body)
  "Emit the jump-over + label + body + end-label pattern common to lambda and defun."
  (emit ctx (make-vm-jump :label end-label))
  (emit ctx (make-vm-label :name func-label))
  (compile-function-body ctx params param-regs opt-bindings rest-binding
                         key-bindings non-constant-defaults body)
  (emit ctx (make-vm-label :name end-label)))

(defmethod compile-ast ((node ast-lambda) ctx)
  (let* ((params (ast-lambda-params node))
         (body (ast-lambda-body node))
         (func-label (make-label ctx "lambda"))
         (end-label (make-label ctx "lambda_end"))
         (closure-reg (make-register ctx))
         (free-vars (find-free-variables node))
         (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                               free-vars)))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings non-constant-defaults)
        (allocate-extended-params ctx
                                  (ast-lambda-optional-params node)
                                  (ast-lambda-rest-param node)
                                  (ast-lambda-key-params node))
      (emit ctx (make-vm-closure
                 :dst closure-reg :label func-label :params param-regs
                 :optional-params opt-closure-data :rest-param rest-reg
                 :key-params key-closure-data :captured captured-vars))
      (%emit-closure-body ctx func-label end-label params param-regs
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults body)
      closure-reg)))

(defmethod compile-ast ((node ast-defun) ctx)
  "Compile a top-level function definition.
Generates a closure at the function's label and registers it globally."
  (let* ((name (ast-defun-name node))
         (params (ast-defun-params node))
         (body (ast-defun-body node))
         ;; Pre-make the label before emitting so recursive calls can find it
         (func-label (make-label ctx (format nil "DEFUN_~A" name)))
         (end-label (make-label ctx (format nil "DEFUN_~A_END" name)))
         (closure-reg (make-register ctx))
         (free-vars (let ((temp-ast (make-ast-lambda :params params :body body)))
                      (find-free-variables temp-ast)))
         (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                               free-vars)))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    ;; Pre-register for recursion support
    (setf (gethash name (ctx-global-functions ctx)) func-label)
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings non-constant-defaults)
        (allocate-extended-params ctx
                                  (ast-defun-optional-params node)
                                  (ast-defun-rest-param node)
                                  (ast-defun-key-params node))
      (emit ctx (make-vm-closure
                 :dst closure-reg :label func-label :params param-regs
                 :optional-params opt-closure-data :rest-param rest-reg
                 :key-params key-closure-data :captured captured-vars))
      (push (cons name closure-reg) (ctx-env ctx))
      (emit ctx (make-vm-register-function :name name :src closure-reg))
      (let ((*compiling-typed-fn* (or name t)))
        (%emit-closure-body ctx func-label end-label params param-regs
                            opt-bindings rest-binding key-bindings
                            non-constant-defaults body))
      closure-reg)))

;;; ── defvar / defparameter ────────────────────────────────────────────────

(defmethod compile-ast ((node ast-defvar) ctx)
  "Compile a top-level variable definition.
Global variables are stored in the VM's global variable store so they
persist across function calls."
  (let* ((name (ast-defvar-name node))
         (value-form (ast-defvar-value node))
         (value-reg (if value-form
                        (compile-ast value-form ctx)
                        (let ((nil-reg (make-register ctx)))
                          (emit ctx (make-vm-const :dst nil-reg :value nil))
                          nil-reg))))
    (emit ctx (make-vm-set-global :name name :src value-reg))
    (setf (gethash name (ctx-global-variables ctx)) t)
    value-reg))
