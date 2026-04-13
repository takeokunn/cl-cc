(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Core: Basic Forms + Control Flow
;;;
;;; Contains: defgeneric, *typed-binop-ctors*, and compile-ast methods for
;;; primitive/control-flow AST node types.
;;; The let-binding optimization subsystem is in codegen-core-let.lisp.
;;;
;;; Load order: before codegen-core-let, codegen-clos, codegen-functions, codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; Forward declarations (defined in optimize/ and emit/ loaded after this file)
(declaim (ftype function optimize-instructions emit-assembly))

(defgeneric compile-ast (node ctx))

;;; ── Binary operator dispatch table (data layer) ──────────────────────────
;;;
;;; Adding a new arithmetic/comparison operator requires exactly one entry
;;; here — binop-ctor derives the constructor at runtime.

(defparameter *typed-binop-ctors*
  '((+  . make-vm-add)
    (-  . make-vm-sub)
    (*  . make-vm-mul)
    (/  . make-vm-cl-div)
    (=  . make-vm-num-eq)
    (<  . make-vm-lt)
    (>  . make-vm-gt)
    (<= . make-vm-le)
    (>= . make-vm-ge))
  "Maps arithmetic/comparison operators to typed (fixnum) VM instruction constructors.")

(defun binop-ctor (op)
  "Return the instruction constructor for OP."
  (let ((entry (assoc op *typed-binop-ctors*)))
    (unless entry
      (error "Unknown binary operator: ~S" op))
    (symbol-function (cdr entry))))

(defparameter +codegen-fixnum-type+
  (cl-cc/type:parse-type-specifier 'fixnum))

(defparameter +codegen-float-type+
  (cl-cc/type:parse-type-specifier 'float))

(defun %ast-proven-type (ctx ast)
  "Return the currently proven type for AST, if any."
  (typecase ast
    (ast-int
     (when (typep (ast-int-value ast) 'fixnum)
       +codegen-fixnum-type+))
    (ast-the
     (or (let ((declared (ast-the-type ast)))
           (when declared
             (cl-cc/type:parse-type-specifier declared)))
         (%ast-proven-type ctx (ast-the-value ast))))
    (ast-var
     (multiple-value-bind (scheme found-p)
         (cl-cc/type:type-env-lookup (ast-var-name ast) (ctx-type-env ctx))
       (when found-p
         (cl-cc/type::instantiate scheme))))
     (t nil)))

(defparameter *fixnum-binop-ctors*
  '((+  . make-vm-integer-add)
    (-  . make-vm-integer-sub)
    (*  . make-vm-integer-mul)
    (<  . make-vm-lt)
    (>  . make-vm-gt)
    (=  . make-vm-num-eq)
    (<= . make-vm-le)
    (>= . make-vm-ge))
  "Alist mapping operators to fixnum-specialized VM instruction constructors.
Parallels *typed-binop-ctors* and *float-binop-ctors*.")

(defparameter *float-binop-ctors*
  '((+ . make-vm-float-add)
    (- . make-vm-float-sub)
    (* . make-vm-float-mul)
    (/ . make-vm-float-div))
  "Alist mapping operators to float-specialized VM instruction constructors.
Parallels *typed-binop-ctors* and *fixnum-binop-ctors*.")

(defun %lookup-specialized-ctor (op table)
  "Look up OP in TABLE alist; return its function or nil."
  (let ((entry (assoc op table)))
    (when entry (symbol-function (cdr entry)))))

(defun %numeric-binop-constructor (op lhs rhs ctx)
  "Select a numeric-specialized constructor for OP/LHS/RHS when possible.
Falls back to the generic binop-ctor when no specialization applies."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (labels ((fixnum-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-fixnum-type+)))
             (float-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-float-type+)))
             (float-literal-p (node)
               (and (typep node 'ast-quote) (floatp (ast-quote-value node)))))
      (cond
        ((and (fixnum-type-p lhs-type) (fixnum-type-p rhs-type))
         (or (%lookup-specialized-ctor op *fixnum-binop-ctors*) (binop-ctor op)))
        ((or (and (float-type-p lhs-type) (float-type-p rhs-type))
             (and (float-literal-p lhs) (float-literal-p rhs)))
         (or (%lookup-specialized-ctor op *float-binop-ctors*) (binop-ctor op)))
        (t (binop-ctor op))))))

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
    (cond
      ;; Self-evaluating: t, nil, keywords are immediate values
      ((or (eq name t) (eq name nil) (keywordp name))
       (let ((dst (make-register ctx)))
         (emit ctx (make-vm-const :dst dst :value name))
         dst))
      (t
       (let ((local-entry (assoc name (ctx-env ctx))))
         (cond
           ;; Boxed local: mutated AND captured variable — unbox via (car box)
           ((and local-entry (member name (ctx-boxed-vars ctx)))
            (let ((dst (make-register ctx)))
              (emit ctx (make-vm-car :dst dst :src (cdr local-entry)))
              dst))
           ;; Plain local binding: return its register directly
           (local-entry
            (cdr local-entry))
           (t
            (multiple-value-bind (constant-value constant-present-p)
                (gethash name *constant-table*)
              (cond
                ;; Compile-time constant: inline the value directly.
                (constant-present-p
                 (let ((dst (make-register ctx)))
                   (emit ctx (make-vm-const :dst dst :value constant-value))
                   dst))
                ;; Global variable: load from the persistent global store
                ((gethash name (ctx-global-variables ctx))
                 (let ((dst (make-register ctx)))
                   (emit ctx (make-vm-get-global :dst dst :name name))
                   dst))
                ;; Host-bound special variable (e.g. *package*, most-positive-fixnum)
                ((boundp name)
                 (let ((dst (make-register ctx)))
                   (emit ctx (make-vm-get-global :dst dst :name name))
                   dst))
                (t
                 (error "Unbound variable: ~S" name)))))))))))

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
  (let ((forms (ast-progn-forms node))
        (last nil)
        (tail (ctx-tail-position ctx)))
    (dolist (form forms)
      (setf (ctx-tail-position ctx)
            (if (eq form (car (last forms))) tail nil))
      (setf last (compile-ast form ctx)))
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
        (case branch
          (:then
           (cl-cc/type:type-env-extend guard-var
                                       (cl-cc/type:type-to-scheme guard-type)
                                       base-env))
          (:else
           (multiple-value-bind (scheme found-p)
               (cl-cc/type:type-env-lookup guard-var base-env)
             (let ((current-type (and found-p
                                      (cl-cc/type::instantiate scheme))))
               (if (and current-type
                        (typep current-type 'cl-cc/type::type-union))
                   (cl-cc/type:type-env-extend guard-var
                                               (cl-cc/type:type-to-scheme
                                                (cl-cc/type::narrow-union-type
                                                 current-type guard-type))
                                               base-env)
                   base-env))))
          (t base-env))
        base-env)))

(defun %case-of-case-collapse-branch (outer-cond branch thenp)
  "Collapse nested IF branches that repeat the same outer condition.

When BRANCH is an IF whose condition is structurally identical to OUTER-COND,
the inner test is redundant.  This is a conservative case-of-case style
optimization: we only remove the inner test when the condition matches
exactly, preserving semantics for overlapping or reordered tests."
  (labels ((collapse (node)
             (if (and (typep node 'ast-if)
                      (equal (ast-to-sexp (ast-if-cond node))
                             (ast-to-sexp outer-cond)))
                 (collapse (if thenp
                               (ast-if-then node)
                               (ast-if-else node)))
                 node)))
    (collapse branch)))

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((tail (ctx-tail-position ctx))
         (cond-ast (ast-if-cond node))
         (then-ast (%case-of-case-collapse-branch cond-ast
                                                  (ast-if-then node)
                                                  t))
         (else-ast (%case-of-case-collapse-branch cond-ast
                                                  (ast-if-else node)
                                                  nil))
         (guard-info (multiple-value-list
                      (cl-cc/type::extract-type-guard cond-ast)))
         (guard-var (first guard-info))
         (guard-type (second guard-info))
         (cond-reg (progn (setf (ctx-tail-position ctx) nil)
                           (compile-ast cond-ast ctx)))
         (dst (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label (make-label ctx "ifend")))
      (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
     (setf (ctx-tail-position ctx) tail)
    (let ((old-type-env (ctx-type-env ctx)))
      (unwind-protect
           (progn
             (setf (ctx-type-env ctx) (%branch-type-env ctx guard-var guard-type :then))
              (let ((then-reg (compile-ast then-ast ctx)))
                (setf (ctx-tail-position ctx) nil)
                (emit ctx (make-vm-move :dst dst :src then-reg))
                (emit ctx (make-vm-jump :label end-label))))
         (setf (ctx-type-env ctx) old-type-env)))
    (emit ctx (make-vm-label :name else-label))
    (setf (ctx-tail-position ctx) tail)
    (let ((old-type-env (ctx-type-env ctx)))
      (unwind-protect
           (progn
             (setf (ctx-type-env ctx) (%branch-type-env ctx guard-var guard-type :else))
              (let ((else-reg (compile-ast else-ast ctx)))
                (setf (ctx-tail-position ctx) nil)
                (emit ctx (make-vm-move :dst dst :src else-reg))))
         (setf (ctx-type-env ctx) old-type-env)))
    (emit ctx (make-vm-label :name end-label))
    dst))

