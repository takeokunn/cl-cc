(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Core: Basic Forms + Control Flow
;;;
;;; Contains: defgeneric, *typed-binop-ctors*, and compile-ast methods for
;;; primitive/control-flow AST node types.
;;; The let-binding optimization subsystem is in codegen-core-let.lisp.
;;;
;;; Load order: before codegen-core-let, codegen-clos, codegen-functions, codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; NOTE: optimize-instructions (cl-cc/optimize) and emit-assembly (defined in
;; codegen-locals.lisp) are called via the umbrella use-package bridge.
;; No declaim needed — it would intern zombie symbols in this package.

(defgeneric compile-ast (node ctx))

;;; ── Binary operator dispatch table (data layer) ──────────────────────────
;;;
;;; One table carries every constructor variant for a numeric/comparison operator.
;;; This keeps the policy declarative: adding a new operator means adding one
;;; entry, not updating 3 parallel tables plus dispatch code.

(defparameter *numeric-binop-ctor-specs*
  '((+  :generic make-vm-add    :fixnum make-vm-integer-add :float make-vm-float-add)
    (-  :generic make-vm-sub    :fixnum make-vm-integer-sub :float make-vm-float-sub)
    (*  :generic make-vm-mul    :fixnum make-vm-integer-mul :float make-vm-float-mul)
    (/  :generic make-vm-cl-div :float  make-vm-float-div)
    (=  :generic make-vm-num-eq :fixnum make-vm-num-eq)
    (<  :generic make-vm-lt     :fixnum make-vm-lt)
    (>  :generic make-vm-gt     :fixnum make-vm-gt)
    (<= :generic make-vm-le     :fixnum make-vm-le)
    (>= :generic make-vm-ge     :fixnum make-vm-ge))
  "(operator keyword constructor ...) specs for generic/fixnum/float binop codegen.")

(defun %lookup-numeric-binop-ctor-symbol (op kind)
  "Return the constructor symbol for OP/KIND, or NIL when no specialization exists."
  (let ((entry (assoc op *numeric-binop-ctor-specs* :test #'eq)))
    (when entry (getf (cdr entry) kind))))

(defun %numeric-binop-ctor-function (op kind)
  "Return the constructor function for OP/KIND or NIL when absent."
  (let ((symbol (%lookup-numeric-binop-ctor-symbol op kind)))
    (when symbol
      (symbol-function symbol))))

(defun binop-ctor (op)
  "Return the generic instruction constructor for OP."
  (or (%numeric-binop-ctor-function op :generic)
      (error "Unknown binary operator: ~S" op)))

(defparameter +codegen-fixnum-type+
  (parse-type-specifier 'fixnum))

(defparameter +codegen-float-type+
  (parse-type-specifier 'float))

(defun %ast-proven-type (ctx ast)
  "Return the currently proven type for AST, if any."
  (cond
    ((typep ast 'ast-int)
     (if (typep (ast-int-value ast) 'fixnum)
         +codegen-fixnum-type+
         nil))
    ((typep ast 'ast-the)
     (or (let ((declared (ast-the-type ast)))
           (if declared (parse-type-specifier declared) nil))
         (%ast-proven-type ctx (ast-the-value ast))))
    ((typep ast 'ast-var)
     (multiple-value-bind (scheme found-p)
         (type-env-lookup (ast-var-name ast) (ctx-type-env ctx))
       (if found-p (instantiate scheme) nil)))
    (t nil)))

(defun %proven-fixnum-type-p (ty)
  "Return T if TY is a proven subtype of fixnum (or nil when type is unknown)."
  (and ty (is-subtype-p ty +codegen-fixnum-type+)))

(defun %proven-float-type-p (ty)
  "Return T if TY is a proven subtype of float."
  (and ty (is-subtype-p ty +codegen-float-type+)))

(defun %float-literal-node-p (node)
  "Return T if NODE is a quoted float literal."
  (and (typep node 'ast-quote) (floatp (ast-quote-value node))))

(defun %numeric-binop-constructor (op lhs rhs ctx)
  "Select a numeric-specialized constructor for OP/LHS/RHS when possible.
Falls back to the generic binop-ctor when no specialization applies."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (cond
      ((and (%proven-fixnum-type-p lhs-type) (%proven-fixnum-type-p rhs-type))
       (or (%numeric-binop-ctor-function op :fixnum) (binop-ctor op)))
      ((or (and (%proven-float-type-p lhs-type) (%proven-float-type-p rhs-type))
           (and (%float-literal-node-p lhs) (%float-literal-node-p rhs)))
       (or (%numeric-binop-ctor-function op :float) (binop-ctor op)))
      (t (binop-ctor op)))))

;;; ── Primitive literal forms ──────────────────────────────────────────────

(defmethod compile-ast ((node ast-int) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-int-value node)))
    dst))

(defmethod compile-ast ((node ast-hole) ctx)
  (declare (ignore ctx))
  (ast-error node "Typed hole '_' must be filled before compilation."))

(defmethod compile-ast ((node ast-var) ctx)
  (let ((name (ast-var-name node)))
    (when (or (eq name t) (eq name nil) (keywordp name))
      (let ((dst (make-register ctx)))
        (emit ctx (make-vm-const :dst dst :value name))
        (return-from compile-ast dst)))
    (let ((local-entry (%assoc-eq name (ctx-env ctx))))
      (when (and local-entry (%member-eq-p name (ctx-boxed-vars ctx)))
        (let ((dst (make-register ctx)))
          (emit ctx (make-vm-car :dst dst :src (cdr local-entry)))
          (return-from compile-ast dst)))
      (when local-entry
        (return-from compile-ast (cdr local-entry))))
    (multiple-value-bind (constant-value constant-present-p)
        (gethash name *constant-table*)
      (when constant-present-p
        (let ((dst (make-register ctx)))
          (emit ctx (make-vm-const :dst dst :value constant-value))
          (return-from compile-ast dst))))
    (when (or (gethash name (ctx-global-variables ctx)) (boundp name))
      (let ((dst (make-register ctx)))
        (emit ctx (make-vm-get-global :dst dst :name name))
        (return-from compile-ast dst)))
    (error "Unbound variable: ~S" name)))

(defmethod compile-ast ((node ast-binop) ctx)
  ;; binop is never in tail position itself; clear to prevent sub-expression leakage
  (setf (ctx-tail-position ctx) nil)
  (let* ((lhs-reg (compile-ast (ast-binop-lhs node) ctx))
         (rhs-reg (compile-ast (ast-binop-rhs node) ctx))
         (dst (make-register ctx))
         (ctor (%numeric-binop-constructor (ast-binop-op node)
                                           (ast-binop-lhs node)
                                           (ast-binop-rhs node)
                                           ctx)))
    (emit ctx (funcall ctor :dst dst :lhs lhs-reg :rhs rhs-reg))
    dst))

(defmethod compile-ast ((node ast-progn) ctx)
  (let ((tail (ctx-tail-position ctx))
        (last nil))
    (loop for rest on (ast-progn-forms node)
          do (setf (ctx-tail-position ctx) (if (null (cdr rest)) tail nil))
             (setf last (compile-ast (car rest) ctx)))
    last))

(defmethod compile-ast ((node ast-print) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((reg (compile-ast (ast-print-expr node) ctx)))
    (emit ctx (make-vm-print :reg reg))
    reg))

(defun %branch-type-env (ctx guard-var guard-type branch)
  "Return a branch-specialized type environment for GUARD-VAR/GUARD-TYPE."
  (let ((base-env (ctx-type-env ctx)))
    (if guard-var
        (if (eq branch :then)
            (type-env-extend guard-var (type-to-scheme guard-type) base-env)
            (if (eq branch :else)
                (multiple-value-bind (scheme found-p)
                    (type-env-lookup guard-var base-env)
                  (let ((current-type (if found-p (instantiate scheme) nil)))
                    (if (and current-type (typep current-type 'type-union))
                        (type-env-extend guard-var
                                         (type-to-scheme
                                          (narrow-union-type current-type guard-type))
                                         base-env)
                        base-env)))
                base-env))
        base-env)))

(defun %case-of-case-collapse-node (node outer-cond thenp)
  "Recursively skip NODE when it is an ast-if whose condition equals OUTER-COND.
Returns the matching branch (THEN or ELSE per THENP) until the condition differs."
  (if (and (typep node 'ast-if)
           (equal (ast-to-sexp (ast-if-cond node))
                  (ast-to-sexp outer-cond)))
      (%case-of-case-collapse-node (if thenp (ast-if-then node) (ast-if-else node))
                                   outer-cond thenp)
      node))

(defun %case-of-case-collapse-branch (outer-cond branch thenp)
  "Collapse nested IF branches that repeat the same outer condition.
Only removes the inner test when the condition matches exactly."
  (%case-of-case-collapse-node branch outer-cond thenp))

(defun %compile-if-branch (ast ctx dst tail guard-var guard-type branch &optional jump-label)
  "Compile one branch of an IF into CTX with type-env narrowing.
Emits a move from the branch result into DST; optionally emits a jump to JUMP-LABEL."
  (setf (ctx-tail-position ctx) tail)
  (let ((saved-type-env (ctx-type-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-type-env ctx) (%branch-type-env ctx guard-var guard-type branch))
           (let ((result-reg (compile-ast ast ctx)))
             (setf (ctx-tail-position ctx) nil)
             (emit ctx (make-vm-move :dst dst :src result-reg))
             (when jump-label
               (emit ctx (make-vm-jump :label jump-label)))))
      (setf (ctx-type-env ctx) saved-type-env))))

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((tail       (ctx-tail-position ctx))
         (cond-ast   (ast-if-cond node))
         (then-ast   (%case-of-case-collapse-branch cond-ast (ast-if-then node) t))
         (else-ast   (%case-of-case-collapse-branch cond-ast (ast-if-else node) nil))
         (guard-info (multiple-value-list (extract-type-guard cond-ast)))
         (guard-var  (first guard-info))
         (guard-type (second guard-info))
         (cond-reg   (progn (setf (ctx-tail-position ctx) nil)
                            (compile-ast cond-ast ctx)))
         (dst        (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label  (make-label ctx "ifend")))
    (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
    (%compile-if-branch then-ast ctx dst tail guard-var guard-type :then end-label)
    (emit ctx (make-vm-label :name else-label))
    (%compile-if-branch else-ast ctx dst tail guard-var guard-type :else nil)
    (emit ctx (make-vm-label :name end-label))
    dst))
