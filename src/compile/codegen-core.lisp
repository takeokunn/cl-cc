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

(defun %numeric-binop-constructor (op lhs rhs ctx)
  "Select a numeric-specialized constructor for OP/LHS/RHS when possible."
  (let ((lhs-type (%ast-proven-type ctx lhs))
        (rhs-type (%ast-proven-type ctx rhs)))
    (labels ((fixnum-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-fixnum-type+)))
             (float-type-p (ty)
               (and ty (cl-cc/type::is-subtype-p ty +codegen-float-type+))))
      (cond
        ((and (fixnum-type-p lhs-type) (fixnum-type-p rhs-type))
         (case op
           (+ 'make-vm-integer-add)
           (- 'make-vm-integer-sub)
           (* 'make-vm-integer-mul)
           (otherwise (binop-ctor op))))
        ((and (float-type-p lhs-type) (float-type-p rhs-type))
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
            (error "Unbound variable: ~S" name))))))))

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

(defmethod compile-ast ((node ast-if) ctx)
  (let* ((tail (ctx-tail-position ctx))
         (guard-info (multiple-value-list
                      (cl-cc/type::extract-type-guard (ast-if-cond node))))
         (guard-var (first guard-info))
         (guard-type (second guard-info))
         (cond-reg (progn (setf (ctx-tail-position ctx) nil)
                          (compile-ast (ast-if-cond node) ctx)))
         (dst (make-register ctx))
         (else-label (make-label ctx "else"))
         (end-label (make-label ctx "ifend")))
    (emit ctx (make-vm-jump-zero :reg cond-reg :label else-label))
    (setf (ctx-tail-position ctx) tail)
    (let ((old-type-env (ctx-type-env ctx)))
      (unwind-protect
           (progn
             (setf (ctx-type-env ctx) (%branch-type-env ctx guard-var guard-type :then))
             (let ((then-reg (compile-ast (ast-if-then node) ctx)))
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
             (let ((else-reg (compile-ast (ast-if-else node) ctx)))
               (setf (ctx-tail-position ctx) nil)
               (emit ctx (make-vm-move :dst dst :src else-reg))))
        (setf (ctx-type-env ctx) old-type-env)))
    (emit ctx (make-vm-label :name end-label))
    dst))

(defmethod compile-ast ((node ast-let) ctx)
  (let ((old-env (ctx-env ctx))
        (old-boxed (ctx-boxed-vars ctx))
        (old-type-env (ctx-type-env ctx)))
    (unwind-protect
         (progn
           ;; A variable needs boxing if it's both:
           ;;   1. Captured by an inner lambda/defun
           ;;   2. Mutated via setq anywhere in the let body
           (let* ((binding-names (mapcar #'car (ast-let-bindings node)))
                  (body-forms (ast-let-body node))
                  (mutated (reduce #'union (mapcar #'find-mutated-variables body-forms)
                                   :initial-value nil))
                  (captured (find-captured-in-children body-forms binding-names))
                  (needs-boxing (intersection mutated captured))
                  (new-bindings nil))
             (dolist (binding (ast-let-bindings node))
               (let* ((name (car binding))
                      (expr (cdr binding))
                      (val-reg (compile-ast expr ctx))
                      (own-reg (make-register ctx)))
                 (emit ctx (make-vm-move :dst own-reg :src val-reg))
                 (if (member name needs-boxing)
                     (let ((box-reg (make-register ctx))
                           (nil-reg (make-register ctx)))
                       (emit ctx (make-vm-const :dst nil-reg :value nil))
                       (emit ctx (make-vm-cons :dst box-reg :car-src own-reg :cdr-src nil-reg))
                       (push (cons name box-reg) new-bindings))
                     (push (cons name own-reg) new-bindings))))
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

(defun %emit-the-runtime-assertion (ctx value-reg declared-type &key (emit-failure-p t))
  "Emit a runtime assertion for (the DECLARED-TYPE VALUE-REG)."
  (unless (or (null declared-type)
              (eq declared-type 't)
              (typep declared-type 'cl-cc/type:type-unknown))
    (when (> (ctx-safety ctx) 0)
      (let ((check-reg (make-register ctx))
            (fail-label (make-label ctx "the_fail"))
            (done-label (make-label ctx "the_done"))
            (error-reg (make-register ctx)))
        (emit ctx (make-vm-typep :dst check-reg :src value-reg :type-name declared-type))
        (when emit-failure-p
          (emit ctx (make-vm-jump-zero :reg check-reg :label fail-label))
          (emit ctx (make-vm-jump :label done-label))
          (emit ctx (make-vm-label :name fail-label))
          (emit ctx (make-vm-const :dst error-reg
                                   :value (format nil "Type assertion failed: expected ~A"
                                                  declared-type)))
          (emit ctx (make-vm-signal-error :error-reg error-reg))
          (emit ctx (make-vm-label :name done-label))))))
  value-reg)

(defmethod compile-ast ((node ast-the) ctx)
  "Compile a type declaration. In typed-function mode, verifies the type at compile time."
  (let ((reg (compile-ast (ast-the-value node) ctx)))
    (when *compiling-typed-fn*
      (let ((declared (ast-the-type node)))
        (when (and declared
                   (not (consp declared))
                   (not (typep declared 'cl-cc/type:type-unknown)))
          (handler-case
            (cl-cc/type:check (ast-the-value node) declared (cl-cc/type:type-env-empty))
              (cl-cc/type:type-mismatch-error (e)
               (error 'ast-compilation-error
                      :location (format nil "~A:~A"
                                        (ast-source-file node)
                                        (ast-source-line node))
                      :format-control "Type error in ~A: ~A"
                      :format-arguments (list *compiling-typed-fn*
                                              (type-error-message-from-mismatch e))))
              (cl-cc/type:type-inference-error () nil)))))
    (let ((declared (ast-the-type node))
          (declared-type (and (ast-the-type node)
                              (cl-cc/type:parse-type-specifier (ast-the-type node)))))
      (if (and declared-type
               (typep (ast-the-value node) 'ast-var)
               (let ((proven (%ast-proven-type ctx (ast-the-value node))))
                 (and proven (cl-cc/type:type-equal-p proven declared-type))))
          (%emit-the-runtime-assertion ctx reg declared :emit-failure-p nil)
          (%emit-the-runtime-assertion ctx reg declared))
      reg)))
