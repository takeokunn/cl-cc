(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Core: Basic Forms + Control Flow
;;;
;;; Contains: defgeneric, *typed-binop-ctors*, and compile-ast methods for
;;; the 12 primitive AST node types that have no dependencies on CLOS,
;;; closures, or builtin dispatch.
;;;
;;; Load order: before codegen-clos, codegen-functions, and codegen.
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

(defun %ast-pure-p (ast)
  "T when AST is inferred to have no observable effects."
  (cl-cc/type:effect-row-subset-p (cl-cc/type:infer-effects ast nil)
                                  cl-cc/type:+pure-effect-row+))

(defun %numeric-binop-constructor (op lhs rhs ctx)
  "Select a numeric-specialized constructor for OP/LHS/RHS when possible."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (labels ((fixnum-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-fixnum-type+)))
              (float-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-float-type+)))
             (float-literal-p (node)
               (and (typep node 'ast-quote)
                    (floatp (ast-quote-value node)))))
       (cond
         ((and (fixnum-type-p lhs-type) (fixnum-type-p rhs-type))
          (case op
            (+ 'make-vm-integer-add)
            (- 'make-vm-integer-sub)
            (* 'make-vm-integer-mul)
            (< 'make-vm-lt)
            (> 'make-vm-gt)
            (= 'make-vm-num-eq)
            (<= 'make-vm-le)
            (>= 'make-vm-ge)
            (otherwise (binop-ctor op))))
         ((or (and (float-type-p lhs-type) (float-type-p rhs-type))
              (and (float-literal-p lhs) (float-literal-p rhs)))
           (case op
             (+ 'make-vm-float-add)
             (- 'make-vm-float-sub)
           (* 'make-vm-float-mul)
           (/ 'make-vm-float-div)
           (otherwise (binop-ctor op))))
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

(defun %ast-var-proven-type (ctx ast)
  "Return the currently proven type for AST if it is a variable reference."
  (when (typep ast 'ast-var)
    (multiple-value-bind (scheme found-p)
        (cl-cc/type:type-env-lookup (ast-var-name ast) (ctx-type-env ctx))
      (when found-p
        (cl-cc/type::instantiate scheme)))))

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

(defun %ast-let-binding-ignored-p (name declarations)
  (some (lambda (decl)
          (and (consp decl)
               (member (car decl) '(ignore ignorable))
               (member name (cdr decl) :test #'eq)))
        declarations))

(defun %ast-cons-call-p (node)
  (and (typep node 'ast-call)
       (let ((func (ast-call-func node)))
         (or (eq func 'cons)
             (and (typep func 'ast-var)
                  (eq (ast-var-name func) 'cons))))
       (= (length (ast-call-args node)) 2)))

(defun %ast-make-array-call-p (node)
  (and (typep node 'ast-call)
       (let ((func (ast-call-func node)))
         (or (eq func 'make-array)
             (and (typep func 'ast-var)
                  (eq (ast-var-name func) 'make-array))))
       (= (length (ast-call-args node)) 1)))

(defun %ast-make-array-int-call-p (node)
  (and (%ast-make-array-call-p node)
       (typep (first (ast-call-args node)) 'ast-int)))

(defun %binding-mentioned-in-body-p (body-forms binding-name)
  (and (listp body-forms)
       (find binding-name
             (find-free-variables (make-ast-progn :forms body-forms))
             :test #'eq)))

(defun %ast-lambda-bound-names (node)
  (append (copy-list (ast-lambda-params node))
          (loop for spec in (ast-lambda-optional-params node)
                collect (if (consp spec) (first spec) spec))
          (when (ast-lambda-rest-param node)
            (list (ast-lambda-rest-param node)))
          (loop for spec in (ast-lambda-key-params node)
                collect (let ((name (if (consp spec) (first spec) spec)))
                          (if (consp name) (second name) name)))))

(defun %ast-as-body-forms (node)
  (if (typep node 'ast-progn)
      (ast-progn-forms node)
      (list node)))

(defun %ast-wrap-bindings (bindings body)
  (if bindings
      (make-ast-let :bindings bindings :body body)
      (if (= (length body) 1)
          (first body)
          (make-ast-progn :forms body))))

(defun %ast-let-sink-if-candidate (node)
  (let* ((bindings (ast-let-bindings node))
         (body (ast-let-body node)))
    (when (and (= (length body) 1)
               (typep (first body) 'ast-if))
      (let* ((if-node (first body))
             (then-forms (%ast-as-body-forms (ast-if-then if-node)))
             (else-forms (%ast-as-body-forms (ast-if-else if-node))))
        (loop for binding in bindings
              for idx from 0
              for name = (car binding)
              for expr = (cdr binding)
              for then-uses = (%binding-mentioned-in-body-p then-forms name)
              for else-uses = (%binding-mentioned-in-body-p else-forms name)
              for outer-bindings = (append (subseq bindings 0 idx)
                                           (subseq bindings (1+ idx)))
               do (labels ((sink-then ()
                             (%ast-wrap-bindings
                              outer-bindings
                              (list (make-ast-if :cond (ast-if-cond if-node)
                                                :then (make-ast-let :bindings (list binding)
                                                                    :body then-forms)
                                                :else (ast-if-else if-node)))))
                          (sink-else ()
                            (%ast-wrap-bindings
                             outer-bindings
                             (list (make-ast-if :cond (ast-if-cond if-node)
                                                :then (ast-if-then if-node)
                                                :else (make-ast-let :bindings (list binding)
                                                                    :body else-forms))))))
                   (when (not (and then-uses else-uses))
                     (cond
                       ((%ast-make-array-call-p expr)
                        (cond
                          ((and then-uses (not else-uses)
                                (%array-binding-static-access-p then-forms name nil))
                           (return-from %ast-let-sink-if-candidate (sink-then)))
                          ((and else-uses (not then-uses)
                                (%array-binding-static-access-p else-forms name nil))
                           (return-from %ast-let-sink-if-candidate (sink-else)))))
                       ((typep expr 'ast-make-instance)
                        (cond
                          ((and then-uses (not else-uses)
                                (%instance-binding-static-slot-only-p then-forms name))
                           (return-from %ast-let-sink-if-candidate (sink-then)))
                          ((and else-uses (not then-uses)
                                (%instance-binding-static-slot-only-p else-forms name))
                           (return-from %ast-let-sink-if-candidate (sink-else)))))
                       ((%ast-cons-call-p expr)
                        (cond
                          ((and then-uses (not else-uses)
                                (not (binding-escapes-in-body-p then-forms name
                                                                :safe-consumers '("CAR" "CDR"))))
                           (return-from %ast-let-sink-if-candidate (sink-then)))
                          ((and else-uses (not then-uses)
                                (not (binding-escapes-in-body-p else-forms name
                                                                :safe-consumers '("CAR" "CDR"))))
                           (return-from %ast-let-sink-if-candidate (sink-else))))))))
               finally (return nil))))))

(defun %array-binding-static-access-p (body-forms binding-name size)
  (labels ((okp (node)
             (typecase node
               (ast-var (not (eq (ast-var-name node) binding-name)))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
               (ast-progn (every #'okp (ast-progn-forms node)))
               (ast-block (every #'okp (ast-block-body node)))
               (ast-if (and (okp (ast-if-cond node))
                            (okp (ast-if-then node))
                            (okp (ast-if-else node))))
                (ast-let
                 (let ((bound-names (mapcar #'car (ast-let-bindings node))))
                   (and (every #'okp (mapcar #'cdr (ast-let-bindings node)))
                        (or (member binding-name bound-names :test #'eq)
                            (every #'okp (ast-let-body node))))))
               (ast-setq (okp (ast-setq-value node)))
               (ast-return-from (okp (ast-return-from-value node)))
               (ast-the (okp (ast-the-value node)))
               (ast-values (every #'okp (ast-values-forms node)))
               (ast-catch (and (okp (ast-catch-tag node))
                               (every #'okp (ast-catch-body node))))
               (ast-throw (and (okp (ast-throw-tag node))
                               (okp (ast-throw-value node))))
               (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                        (every #'okp (ast-unwind-cleanup node))))
               (ast-handler-case (and (okp (ast-handler-case-form node))
                                      (every (lambda (clause)
                                               (every #'okp (cddr clause)))
                                             (ast-handler-case-clauses node))))
               (ast-multiple-value-call (and (okp (ast-mv-call-func node))
                                             (every #'okp (ast-mv-call-args node))))
               (ast-multiple-value-prog1 (and (okp (ast-mv-prog1-first node))
                                              (every #'okp (ast-mv-prog1-forms node))))
                (ast-multiple-value-bind
                 (and (okp (ast-mvb-values-form node))
                      (or (member binding-name (ast-mvb-vars node) :test #'eq)
                          (every #'okp (ast-mvb-body node)))))
                (ast-apply (and (okp (ast-apply-func node))
                                (every #'okp (ast-apply-args node))))
                (ast-lambda
                 (and (not (find binding-name (find-captured-in-children (ast-lambda-body node)
                                                                         (list binding-name))))
                      (or (member binding-name (%ast-lambda-bound-names node) :test #'eq)
                          (every #'okp (ast-lambda-body node)))))
                (ast-defun (and (not (find binding-name (find-captured-in-children (ast-defun-body node)
                                                                                    (list binding-name))))
                                (or (member binding-name (ast-defun-params node) :test #'eq)
                                    (every #'okp (ast-defun-body node)))))
                (ast-defmethod (and (not (find binding-name (find-captured-in-children (ast-defmethod-body node)
                                                                                        (list binding-name))))
                                    (or (member binding-name (ast-defmethod-params node) :test #'eq)
                                        (every #'okp (ast-defmethod-body node)))))
               (ast-local-fns (and (not (find binding-name (find-captured-in-children (ast-local-fns-body node)
                                                                                       (list binding-name))))
                                   (every #'okp (ast-local-fns-body node))))
               (ast-call
                (let ((func (ast-call-func node))
                      (args (ast-call-args node)))
                  (cond
                    ((and (or (and (symbolp func) (string= (symbol-name func) "ARRAY-LENGTH"))
                              (and (typep func 'ast-var) (string= (symbol-name (ast-var-name func)) "ARRAY-LENGTH")))
                          (= (length args) 1)
                          (typep (first args) 'ast-var)
                          (eq (ast-var-name (first args)) binding-name))
                     t)
                    ((and (or (and (symbolp func) (string= (symbol-name func) "ASET"))
                              (and (typep func 'ast-var) (string= (symbol-name (ast-var-name func)) "ASET")))
                          (= (length args) 3)
                          (typep (first args) 'ast-var)
                          (eq (ast-var-name (first args)) binding-name)
                          (okp (second args))
                          (okp (third args)))
                     t)
                    ((and (or (eq func 'aref)
                              (and (typep func 'ast-var) (eq (ast-var-name func) 'aref)))
                          (= (length args) 2)
                          (typep (first args) 'ast-var)
                          (eq (ast-var-name (first args)) binding-name)
                          (okp (second args)))
                     t)
                    (t (and (if (typep func 'ast-node) (okp func) t)
                            (every #'okp args))))))
               (t (every #'okp (ast-children node))))))
    (and (listp body-forms) (every #'okp body-forms))))

(defun %instance-binding-static-slot-only-p (body-forms binding-name)
  (labels ((okp (node)
             (typecase node
               (ast-var (not (eq (ast-var-name node) binding-name)))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
               (ast-progn (every #'okp (ast-progn-forms node)))
               (ast-block (every #'okp (ast-block-body node)))
               (ast-if (and (okp (ast-if-cond node))
                            (okp (ast-if-then node))
                            (okp (ast-if-else node))))
                (ast-let
                 (let ((bound-names (mapcar #'car (ast-let-bindings node))))
                   (and (every #'okp (mapcar #'cdr (ast-let-bindings node)))
                        (or (member binding-name bound-names :test #'eq)
                            (every #'okp (ast-let-body node))))))
               (ast-setq (okp (ast-setq-value node)))
               (ast-return-from (okp (ast-return-from-value node)))
               (ast-the (okp (ast-the-value node)))
               (ast-values (every #'okp (ast-values-forms node)))
               (ast-catch (and (okp (ast-catch-tag node))
                               (every #'okp (ast-catch-body node))))
               (ast-throw (and (okp (ast-throw-tag node))
                               (okp (ast-throw-value node))))
               (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                        (every #'okp (ast-unwind-cleanup node))))
               (ast-handler-case (and (okp (ast-handler-case-form node))
                                      (every (lambda (clause)
                                               (every #'okp (cddr clause)))
                                             (ast-handler-case-clauses node))))
               (ast-multiple-value-call (and (okp (ast-mv-call-func node))
                                             (every #'okp (ast-mv-call-args node))))
               (ast-multiple-value-prog1 (and (okp (ast-mv-prog1-first node))
                                              (every #'okp (ast-mv-prog1-forms node))))
                (ast-multiple-value-bind
                 (and (okp (ast-mvb-values-form node))
                      (or (member binding-name (ast-mvb-vars node) :test #'eq)
                          (every #'okp (ast-mvb-body node)))))
                (ast-apply (and (okp (ast-apply-func node))
                                (every #'okp (ast-apply-args node))))
                (ast-lambda
                 (and (not (find binding-name (find-captured-in-children (ast-lambda-body node)
                                                                         (list binding-name))))
                      (or (member binding-name (%ast-lambda-bound-names node) :test #'eq)
                          (every #'okp (ast-lambda-body node)))))
                (ast-defun (and (not (find binding-name (find-captured-in-children (ast-defun-body node)
                                                                                    (list binding-name))))
                                (or (member binding-name (ast-defun-params node) :test #'eq)
                                    (every #'okp (ast-defun-body node)))))
                (ast-defmethod (and (not (find binding-name (find-captured-in-children (ast-defmethod-body node)
                                                                                        (list binding-name))))
                                    (or (member binding-name (ast-defmethod-params node) :test #'eq)
                                        (every #'okp (ast-defmethod-body node)))))
               (ast-local-fns (and (not (find binding-name (find-captured-in-children (ast-local-fns-body node)
                                                                                       (list binding-name))))
                                   (every #'okp (ast-local-fns-body node))))
               (ast-slot-value
                (and (typep (ast-slot-value-object node) 'ast-var)
                     (eq (ast-var-name (ast-slot-value-object node)) binding-name)))
               (ast-set-slot-value
                (and (typep (ast-set-slot-value-object node) 'ast-var)
                     (eq (ast-var-name (ast-set-slot-value-object node)) binding-name)
                     (okp (ast-set-slot-value-value node))))
               (t (every #'okp (ast-children node))))))
    (and (listp body-forms) (every #'okp body-forms))))

(defun %closure-binding-direct-call-only-p (body-forms binding-name arity)
  (labels ((okp (node)
             (typecase node
               (ast-var (not (eq (ast-var-name node) binding-name)))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
               (ast-progn (every #'okp (ast-progn-forms node)))
               (ast-block (every #'okp (ast-block-body node)))
               (ast-if (and (okp (ast-if-cond node))
                            (okp (ast-if-then node))
                            (okp (ast-if-else node))))
               (ast-let (and (every #'okp (mapcar #'cdr (ast-let-bindings node)))
                             (every #'okp (ast-let-body node))))
               (ast-setq (okp (ast-setq-value node)))
               (ast-return-from (okp (ast-return-from-value node)))
               (ast-the (okp (ast-the-value node)))
               (ast-values (every #'okp (ast-values-forms node)))
               (ast-catch (and (okp (ast-catch-tag node))
                               (every #'okp (ast-catch-body node))))
               (ast-throw (and (okp (ast-throw-tag node))
                               (okp (ast-throw-value node))))
               (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                        (every #'okp (ast-unwind-cleanup node))))
               (ast-handler-case (and (okp (ast-handler-case-form node))
                                      (every (lambda (clause)
                                               (every #'okp (cddr clause)))
                                             (ast-handler-case-clauses node))))
               (ast-multiple-value-call (and (okp (ast-mv-call-func node))
                                             (every #'okp (ast-mv-call-args node))))
               (ast-multiple-value-prog1 (and (okp (ast-mv-prog1-first node))
                                              (every #'okp (ast-mv-prog1-forms node))))
               (ast-multiple-value-bind (and (okp (ast-mvb-values-form node))
                                             (every #'okp (ast-mvb-body node))))
               (ast-apply (and (okp (ast-apply-func node))
                               (every #'okp (ast-apply-args node))))
               (ast-lambda (and (not (find binding-name (find-captured-in-children (ast-lambda-body node)
                                                                                    (list binding-name))))
                                (every #'okp (ast-lambda-body node))))
               (ast-defun (and (not (find binding-name (find-captured-in-children (ast-defun-body node)
                                                                                   (list binding-name))))
                               (every #'okp (ast-defun-body node))))
               (ast-defmethod (and (not (find binding-name (find-captured-in-children (ast-defmethod-body node)
                                                                                       (list binding-name))))
                                   (every #'okp (ast-defmethod-body node))))
               (ast-local-fns (and (not (find binding-name (find-captured-in-children (ast-local-fns-body node)
                                                                                       (list binding-name))))
                                   (every #'okp (ast-local-fns-body node))))
               (ast-call
                (let ((func (ast-call-func node))
                      (args (ast-call-args node)))
                  (cond
                    ((and (typep func 'ast-var)
                          (eq (ast-var-name func) binding-name)
                          (= (length args) arity)
                          (every #'okp args))
                     t)
                    (t (and (if (typep func 'ast-node) (okp func) t)
                            (every #'okp args))))))
               (t (every #'okp (ast-children node))))))
    (and (listp body-forms) (every #'okp body-forms))))

(defmethod compile-ast ((node ast-let) ctx)
  (let ((sunk (%ast-let-sink-if-candidate node)))
    (when sunk
      (return-from compile-ast (compile-ast sunk ctx))))
  (let ((old-env (ctx-env ctx))
         (old-boxed (ctx-boxed-vars ctx))
         (old-noescape-cons (ctx-noescape-cons-bindings ctx))
         (old-noescape-arrays (ctx-noescape-array-bindings ctx))
         (old-noescape-instances (ctx-noescape-instance-bindings ctx))
         (old-noescape-closures (ctx-noescape-closure-bindings ctx))
         (old-type-env (ctx-type-env ctx)))
    (unwind-protect
         (progn
           ;; A variable needs boxing if it's both:
           ;;   1. Captured by an inner lambda/defun
           ;;   2. Mutated via setq anywhere in the let body
            (let* ((binding-names (mapcar #'car (ast-let-bindings node)))
                   (declarations (ast-let-declarations node))
                   (body-forms (ast-let-body node))
                   (mutated (reduce #'union (mapcar #'find-mutated-variables body-forms)
                                    :initial-value nil))
                   (captured (find-captured-in-children body-forms binding-names))
                   (needs-boxing (intersection mutated captured))
                   (new-bindings nil))
              (dolist (binding (ast-let-bindings node))
                (let* ((name (car binding))
                       (expr (cdr binding))
                       (noescape-closure
                         (and (typep expr 'ast-lambda)
                              (null (ast-lambda-optional-params expr))
                              (null (ast-lambda-rest-param expr))
                              (null (ast-lambda-key-params expr))
                              (not (member name mutated))
                              (not (member name captured))
                              (%closure-binding-direct-call-only-p body-forms name
                                                                  (length (ast-lambda-params expr)))
                              expr))
                       (noescape-instance-slots
                         (and (typep expr 'ast-make-instance)
                              (not (member name mutated))
                              (not (member name captured))
                              (%instance-binding-static-slot-only-p body-forms name)
                              (loop for (key . value-ast) in (ast-make-instance-initargs expr)
                                    collect (cons (symbol-name key)
                                                  (compile-ast value-ast ctx)))))
                       (noescape-array-size (and (%ast-make-array-int-call-p expr)
                                                 (not (member name mutated))
                                                 (not (member name captured))
                                                 (%array-binding-static-access-p body-forms name
                                                                                (ast-int-value (first (ast-call-args expr))))
                                                 (ast-int-value (first (ast-call-args expr)))))
                       (noescape-cons-p (and (%ast-cons-call-p expr)
                                             (not (member name mutated))
                                             (not (member name captured))
                                             (not (binding-escapes-in-body-p body-forms name
                                                                             :safe-consumers '("CAR" "CDR"))))))
                  (cond
                    (noescape-closure
                     (push (cons name noescape-closure)
                           (ctx-noescape-closure-bindings ctx)))
                    (noescape-instance-slots
                     (push (cons name noescape-instance-slots)
                           (ctx-noescape-instance-bindings ctx)))
                    (noescape-array-size
                     (let* ((zero-reg (make-register ctx))
                            (element-regs nil))
                       (emit ctx (make-vm-const :dst zero-reg :value 0))
                       (dotimes (_ noescape-array-size)
                         (push zero-reg element-regs))
                       (push (cons name (cons noescape-array-size (nreverse element-regs)))
                             (ctx-noescape-array-bindings ctx))))
                    (noescape-cons-p
                     (let* ((car-reg (compile-ast (first (ast-call-args expr)) ctx))
                            (cdr-reg (compile-ast (second (ast-call-args expr)) ctx)))
                       (push (cons name (cons car-reg cdr-reg))
                             (ctx-noescape-cons-bindings ctx))))
                    ((member name needs-boxing)
                     (let ((val-reg (compile-ast expr ctx)))
                       (let ((own-reg (make-register ctx))
                             (box-reg (make-register ctx))
                             (nil-reg (make-register ctx)))
                         (emit ctx (make-vm-move :dst own-reg :src val-reg))
                         (emit ctx (make-vm-const :dst nil-reg :value nil))
                         (emit ctx (make-vm-cons :dst box-reg :car-src own-reg :cdr-src nil-reg))
                         (push (cons name box-reg) new-bindings))))
                    ((%ast-let-binding-ignored-p name declarations)
                     (let ((val-reg (compile-ast expr ctx)))
                       (push (cons name val-reg) new-bindings)))
                    (t
                     (let ((val-reg (compile-ast expr ctx)))
                       (let ((own-reg (make-register ctx)))
                         (emit ctx (make-vm-move :dst own-reg :src val-reg))
                         (push (cons name own-reg) new-bindings)))))))
              (setf (ctx-env ctx) (append (nreverse new-bindings) (ctx-env ctx)))
              (setf (ctx-boxed-vars ctx) (union needs-boxing (ctx-boxed-vars ctx))))
           (dolist (binding (ast-let-bindings node))
             (let ((binding-type (%ast-proven-type ctx (cdr binding))))
               (when binding-type
                 (setf (ctx-type-env ctx)
                       (cl-cc/type:type-env-extend
                        (car binding)
                        (cl-cc/type:type-to-scheme binding-type)
                        (ctx-type-env ctx))))))
           (let ((last nil)
                 (tail (ctx-tail-position ctx))
                 (body-forms (ast-let-body node)))
             (dolist (form body-forms)
               (setf (ctx-tail-position ctx)
                     (if (eq form (car (last body-forms))) tail nil))
               (setf last (compile-ast form ctx)))
             last))
       (setf (ctx-env ctx) old-env)
       (setf (ctx-boxed-vars ctx) old-boxed)
       (setf (ctx-noescape-cons-bindings ctx) old-noescape-cons)
       (setf (ctx-noescape-array-bindings ctx) old-noescape-arrays)
       (setf (ctx-noescape-instance-bindings ctx) old-noescape-instances)
       (setf (ctx-noescape-closure-bindings ctx) old-noescape-closures)
       (setf (ctx-type-env ctx) old-type-env))))

;;; ── Control flow: block / return-from ────────────────────────────────────

(defun lookup-block (ctx name)
  "Look up a block by name, returning (exit-label . result-reg) or error."
  (let ((entry (assoc name (ctx-block-env ctx))))
    (unless entry
      (error "Unknown block: ~S" name))
    (cdr entry)))

(defmethod compile-ast ((node ast-block) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((block-name (ast-block-name node))
         (exit-label (make-label ctx "block_exit"))
         (result-reg (make-register ctx))
         (old-block-env (ctx-block-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-block-env ctx)
                 (cons (cons block-name (cons exit-label result-reg))
                       (ctx-block-env ctx)))
           (let ((body-result (let ((last nil))
                                (dolist (form (ast-block-body node))
                                  (setf last (compile-ast form ctx)))
                                last)))
             (emit ctx (make-vm-move :dst result-reg :src body-result))))
      (setf (ctx-block-env ctx) old-block-env))
    (emit ctx (make-vm-label :name exit-label))
    result-reg))

(defmethod compile-ast ((node ast-return-from) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((block-info (lookup-block ctx (ast-return-from-name node)))
         (exit-label (car block-info))
         (result-reg (cdr block-info))
         (value-reg (compile-ast (ast-return-from-value node) ctx)))
    (emit ctx (make-vm-move :dst result-reg :src value-reg))
    (emit ctx (make-vm-jump :label exit-label))
    result-reg))

;;; ── Control flow: tagbody / go ───────────────────────────────────────────

(defun lookup-tag (ctx tag)
  "Look up a tag within the current tagbody, returning its label or error."
  (let ((entry (assoc tag (ctx-tagbody-env ctx))))
    (unless entry
      (error "Unknown tag: ~S" tag))
    (cdr entry)))

(defmethod compile-ast ((node ast-tagbody) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((tags (ast-tagbody-tags node))
         (end-label (make-label ctx "tagbody_end"))
         (result-reg (make-register ctx))
         (old-tagbody-env (ctx-tagbody-env ctx))
         (tag-labels (mapcar (lambda (tag-entry)
                               (cons (car tag-entry) (make-label ctx "tag")))
                             tags)))
    (unwind-protect
         (progn
           (setf (ctx-tagbody-env ctx)
                 (append tag-labels (ctx-tagbody-env ctx)))
           (if tag-labels
               (emit ctx (make-vm-jump :label (cdar tag-labels)))
               (emit ctx (make-vm-const :dst result-reg :value nil)))
           (dolist (tag-entry tags)
             (let* ((tag (car tag-entry))
                    (forms (cdr tag-entry))
                    (label (cdr (assoc tag tag-labels))))
               (emit ctx (make-vm-label :name label))
               (when forms
                 (dolist (form forms)
                   (compile-ast form ctx))
                 (emit ctx (make-vm-jump :label end-label))))))
      (setf (ctx-tagbody-env ctx) old-tagbody-env))
    (emit ctx (make-vm-label :name end-label))
    (emit ctx (make-vm-const :dst result-reg :value nil))
    result-reg))

(defmethod compile-ast ((node ast-go) ctx)
  (emit ctx (make-vm-jump :label (lookup-tag ctx (ast-go-tag node))))
  (make-register ctx))

;;; ── Assignment: setq / quote / the ──────────────────────────────────────

(defmethod compile-ast ((node ast-setq) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((var-name (ast-setq-var node))
         (value-reg (compile-ast (ast-setq-value node) ctx))
         (local-entry (assoc var-name (ctx-env ctx))))
    (cond
      ((and local-entry (member var-name (ctx-boxed-vars ctx)))
       ;; Boxed variable: write via (rplaca box new-val)
       (emit ctx (make-vm-rplaca :cons (cdr local-entry) :val value-reg))
       value-reg)
      (local-entry
       (emit ctx (make-vm-move :dst (cdr local-entry) :src value-reg))
       (cdr local-entry))
      ((gethash var-name (ctx-global-variables ctx))
       (emit ctx (make-vm-set-global :name var-name :src value-reg))
       value-reg)
      (t
       (error "Unbound variable for setq: ~S" var-name)))))

(defmethod compile-ast ((node ast-quote) ctx)
  (let ((dst (make-register ctx)))
    (emit ctx (make-vm-const :dst dst :value (ast-quote-value node)))
    dst))

(defun type-error-message-from-mismatch (e)
  "Extract a human-readable message from a type-mismatch-error condition."
  (format nil "expected ~A but got ~A"
          (cl-cc/type:type-to-string (cl-cc/type:type-mismatch-error-expected e))
          (cl-cc/type:type-to-string (cl-cc/type:type-mismatch-error-actual e))))

(defun %emit-the-runtime-assertion (ctx value-reg declared-spec &key (emit-failure-p t))
  "Emit a runtime assertion for (the DECLARED-TYPE VALUE-REG).
When EMIT-FAILURE-P is NIL, keep the lightweight type check but omit failure handling."
  (unless (or (null declared-spec)
              (eq declared-spec 't)
              (typep declared-spec 'cl-cc/type:type-unknown))
    (when (> (ctx-safety ctx) 0)
      (let ((check-reg (make-register ctx)))
        (emit ctx (make-vm-typep :dst check-reg :src value-reg :type-name declared-spec))
        (when emit-failure-p
          (let ((fail-label (make-label ctx "the_fail"))
                (done-label (make-label ctx "the_done"))
                (error-reg (make-register ctx)))
            (emit ctx (make-vm-jump-zero :reg check-reg :label fail-label))
            (emit ctx (make-vm-jump :label done-label))
            (emit ctx (make-vm-label :name fail-label))
            (emit ctx (make-vm-const :dst error-reg
                                     :value (format nil "Type assertion failed: expected ~A"
                                                    declared-spec)))
            (emit ctx (make-vm-signal-error :error-reg error-reg))
            (emit ctx (make-vm-label :name done-label)))))))
  value-reg)

(defmethod compile-ast ((node ast-the) ctx)
  "Compile a type declaration. In typed-function mode, verifies the type at compile time."
  (let ((reg (compile-ast (ast-the-value node) ctx)))
    (let* ((declared (ast-the-type node))
           (declared-spec (and declared (cl-cc/type:parse-type-specifier declared))))
      (when *compiling-typed-fn*
        (when (and declared-spec
                   (not (and (typep (ast-the-value node) 'ast-var)
                             (let ((proven (%ast-proven-type ctx (ast-the-value node))))
                               (and proven
                                    (cl-cc/type:type-equal-p proven declared-spec))))))
          (handler-case
              (cl-cc/type:check (ast-the-value node) declared-spec
                                (or (ctx-type-env ctx)
                                    (cl-cc/type:type-env-empty)))
            (cl-cc/type:type-mismatch-error (e)
              (error 'ast-compilation-error
                     :location (format nil "~A:~A"
                                       (ast-source-file node)
                                       (ast-source-line node))
                     :format-control "Type error in ~A: ~A"
                     :format-arguments (list *compiling-typed-fn*
                                             (type-error-message-from-mismatch e))))
            (cl-cc/type:type-inference-error () nil))))
      (if (and declared-spec
               (typep (ast-the-value node) 'ast-var)
               (let ((proven (%ast-proven-type ctx (ast-the-value node))))
                 (and proven (cl-cc/type:type-equal-p proven declared-spec))))
           (%emit-the-runtime-assertion ctx reg declared :emit-failure-p nil)
           (%emit-the-runtime-assertion ctx reg declared))
      reg)))
