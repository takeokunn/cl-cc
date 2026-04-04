(in-package :cl-cc)

(declaim (special *x86-64-calling-convention*))
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

(defun %ast-constant-number-value (node)
  "Return NODE's integer value when NODE is a constant AST integer."
  (typecase node
    (ast-int (ast-int-value node))
    (ast-quote (let ((value (ast-quote-value node)))
                 (when (integerp value)
                   value)))
    (t nil)))

(defun %same-ast-binop (node lhs rhs)
  (make-ast-binop :op (ast-binop-op node)
                  :lhs lhs
                  :rhs rhs
                  :source-file (ast-source-file node)
                  :source-line (ast-source-line node)
                  :source-column (ast-source-column node)))

(defun %fold-ast-binop (node lhs rhs)
  (let ((lv (%ast-constant-number-value lhs))
        (rv (%ast-constant-number-value rhs)))
    (if (and lv rv)
        (let ((value (case (ast-binop-op node)
                       (+ (+ lv rv))
                       (- (- lv rv))
                       (* (* lv rv))
                       (/ (when (not (zerop rv))
                            (let ((quot (/ lv rv)))
                              (when (integerp quot) quot))))
                       (otherwise nil))))
          (if (integerp value)
              (make-ast-int :value value
                            :source-file (ast-source-file node)
                            :source-line (ast-source-line node)
                            :source-column (ast-source-column node))
              (%same-ast-binop node lhs rhs)))
        (%same-ast-binop node lhs rhs))))

(defvar *compile-time-value-env* nil
  "Alist of compile-time constant bindings available to optimize-ast.")

(defvar *compile-time-function-env* nil
  "Alist of compile-time known function definitions available to optimize-ast.")

(defvar *compile-time-block-env* nil
  "Alist of compile-time block tags available to optimize-ast.")

(defvar *compile-time-eval-depth-limit* 64
  "Maximum recursion depth for compile-time partial evaluation.")

(defun %compile-time-falsep (value)
  (opt-falsep value))

(defun %ast-constant-node-p (node)
  (or (typep node 'ast-int)
      (typep node 'ast-quote)))

(defun %ast->compile-time-value (node)
  (typecase node
    (ast-int (ast-int-value node))
    (ast-quote (ast-quote-value node))
    (t nil)))

(defun %compile-time-value->ast (value node)
  (cond
    ((integerp value)
     (make-ast-int :value value
                   :source-file (ast-source-file node)
                   :source-line (ast-source-line node)
                   :source-column (ast-source-column node)))
    (t
     (make-ast-quote :value value
                     :source-file (ast-source-file node)
                     :source-line (ast-source-line node)
                     :source-column (ast-source-column node)))))

(defun %compile-time-lookup (name env)
  (let ((entry (assoc name env)))
    (when entry
      (values (cdr entry) t))))

(defun %compile-time-lookup-block (name env)
  (let ((entry (assoc name env)))
    (when entry
      (values (cdr entry) t))))

(defun %compile-time-eval-binop (op lhs rhs)
  (case op
    (+ (+ lhs rhs))
    (- (- lhs rhs))
    (* (* lhs rhs))
    (/ (when (not (zerop rhs))
         (let ((quot (/ lhs rhs)))
           (when (integerp quot) quot))))
    (1+ (when (and (integerp lhs) (null rhs)) (1+ lhs)))
    (1- (when (and (integerp lhs) (null rhs)) (1- lhs)))
    (otherwise nil)))

(defun %compile-time-eval-call (func args depth)
  (cond
    ((and (typep func 'ast-var)
           (= (length args) 1)
           (string= (symbol-name (ast-var-name func)) "STRING-LENGTH")
           (stringp (first args)))
      (values (length (first args)) t))
    ((and (typep func 'ast-var)
          (let ((name (ast-var-name func)))
            (member name '(+ - * / = < <= > >= not zerop plusp minusp oddp evenp
                              numberp integerp consp null symbolp stringp functionp)
                    :test #'eq)))
      (let ((name (ast-var-name func)))
        (case name
          (+ (values (apply #'+ args) t))
          (- (values (apply #'- args) t))
          (* (values (apply #'* args) t))
          (/ (let ((value (ignore-errors (apply #'/ args))))
               (when value
                 (values value t))))
          (= (values (if (apply #'= args) t nil) t))
          (< (values (if (apply #'< args) t nil) t))
          (<= (values (if (apply #'<= args) t nil) t))
          (> (values (if (apply #'> args) t nil) t))
          (>= (values (if (apply #'>= args) t nil) t))
          (not (values (not (first args)) t))
          (zerop (values (zerop (first args)) t))
          (plusp (values (plusp (first args)) t))
          (minusp (values (minusp (first args)) t))
          (oddp (values (oddp (first args)) t))
          (evenp (values (evenp (first args)) t))
          (numberp (values (numberp (first args)) t))
          (integerp (values (integerp (first args)) t))
          (consp (values (consp (first args)) t))
          (null (values (null (first args)) t))
          (symbolp (values (symbolp (first args)) t))
          (stringp (values (stringp (first args)) t))
          (functionp (values (functionp (first args)) t))
          (otherwise nil))))
    ((and (typep func 'ast-var)
          (<= 0 depth)
          (multiple-value-bind (entry found-p)
              (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
            (declare (ignore entry))
            found-p))
     (multiple-value-bind (defun-node found-p)
         (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
       (when (and found-p (typep defun-node 'ast-defun)
                  (null (ast-defun-optional-params defun-node))
                  (null (ast-defun-rest-param defun-node))
                  (null (ast-defun-key-params defun-node)))
         (let ((param-bindings (mapcar #'cons (ast-defun-params defun-node) args)))
           (%evaluate-ast-sequence (ast-defun-body defun-node)
                                   param-bindings
                                   *compile-time-function-env*
                                   (1- depth))))))
    ((typep func 'ast-lambda)
     (let ((param-bindings (mapcar #'cons (ast-lambda-params func) args)))
       (%evaluate-ast-sequence (ast-lambda-body func)
                               param-bindings
                               *compile-time-function-env*
                               (1- depth))))
    (t nil)))

(defun %evaluate-ast-sequence (forms value-env function-env depth)
  (let ((*compile-time-value-env* value-env)
        (*compile-time-function-env* function-env))
    (loop with result = nil
          for form in forms
          do (multiple-value-bind (value ok)
                 (%evaluate-ast form depth)
               (unless ok
                 (return-from %evaluate-ast-sequence (values nil nil)))
               (setf result value))
          finally (return (values result t)))))

(defun %evaluate-ast (node depth)
  (when (minusp depth)
    (return-from %evaluate-ast (values nil nil)))
  (typecase node
    (ast-int (values (ast-int-value node) t))
    (ast-quote (values (ast-quote-value node) t))
    (ast-var
     (multiple-value-bind (value found-p)
         (%compile-time-lookup (ast-var-name node) *compile-time-value-env*)
       (if found-p
           (values value t)
           (values nil nil))))
    (ast-binop
     (multiple-value-bind (lhs lhs-ok) (%evaluate-ast (ast-binop-lhs node) (1- depth))
       (multiple-value-bind (rhs rhs-ok) (%evaluate-ast (ast-binop-rhs node) (1- depth))
         (if (and lhs-ok rhs-ok)
             (let ((value (%compile-time-eval-binop (ast-binop-op node) lhs rhs)))
               (if (and value (integerp value))
                   (values value t)
                   (values nil nil)))
             (values nil nil)))))
    (ast-if
     (multiple-value-bind (cond cond-ok) (%evaluate-ast (ast-if-cond node) (1- depth))
       (if cond-ok
           (%evaluate-ast (if (%compile-time-falsep cond)
                              (ast-if-else node)
                              (ast-if-then node))
                          (1- depth))
           (values nil nil))))
    (ast-progn
     (%evaluate-ast-sequence (ast-progn-forms node)
                              *compile-time-value-env*
                              *compile-time-function-env*
                              (1- depth)))
    (ast-block
     (let ((tag (gensym "BLOCK-")))
       (let ((*compile-time-block-env* (acons (ast-block-name node)
                                              tag
                                              *compile-time-block-env*)))
         (catch tag
           (%evaluate-ast-sequence (ast-block-body node)
                                   *compile-time-value-env*
                                   *compile-time-function-env*
                                   (1- depth))))))
    (ast-return-from
     (multiple-value-bind (tag found-p)
         (%compile-time-lookup-block (ast-return-from-name node) *compile-time-block-env*)
       (if found-p
           (multiple-value-bind (value ok)
               (%evaluate-ast (ast-return-from-value node) (1- depth))
             (if ok
                 (throw tag value)
                 (values nil nil)))
           (values nil nil))))
    (ast-let
     (let ((bindings nil))
       (dolist (binding (ast-let-bindings node))
         (multiple-value-bind (value ok)
             (%evaluate-ast (cdr binding) (1- depth))
           (unless ok (return-from %evaluate-ast (values nil nil)))
           (push (cons (car binding) value) bindings)))
       (%evaluate-ast-sequence (ast-let-body node)
                               (append (nreverse bindings) *compile-time-value-env*)
                               *compile-time-function-env*
                               (1- depth))))
    (ast-the
     (%evaluate-ast (ast-the-value node) (1- depth)))
    (ast-call
     (let ((func (ast-call-func node)))
       (multiple-value-bind (args ok)
           (loop for arg in (ast-call-args node)
                 collect (multiple-value-list (%evaluate-ast arg (1- depth))) into results
                 finally (return (values (mapcar #'first results)
                                         (every #'second results))))
         (if ok
             (%compile-time-eval-call func args depth)
             (values nil nil)))))
    (t (values nil nil))))

(defun optimize-ast (node)
  "Fold small pure constant expressions before VM lowering."
  (typecase node
    (ast-binop
     (%fold-ast-binop node
                      (optimize-ast (ast-binop-lhs node))
                      (optimize-ast (ast-binop-rhs node))))
    (ast-call
      (let* ((func (optimize-ast (ast-call-func node)))
             (args (mapcar #'optimize-ast (ast-call-args node)))
             (call-node (make-ast-call :func func
                                       :args args
                                       :source-file (ast-source-file node)
                                       :source-line (ast-source-line node)
                                       :source-column (ast-source-column node))))
        (multiple-value-bind (value ok)
            (let ((*compile-time-value-env* *compile-time-value-env*)
                  (*compile-time-function-env* *compile-time-function-env*))
              (%evaluate-ast call-node *compile-time-eval-depth-limit*))
          (if ok
              (%compile-time-value->ast value node)
              call-node))))
    (ast-progn
     (make-ast-progn :forms (mapcar #'optimize-ast (ast-progn-forms node))
                     :source-file (ast-source-file node)
                     :source-line (ast-source-line node)
                     :source-column (ast-source-column node)))
    (ast-let
     (make-ast-let :bindings (mapcar (lambda (binding)
                                       (cons (car binding)
                                             (optimize-ast (cdr binding))))
                                     (ast-let-bindings node))
                   :declarations (ast-let-declarations node)
                   :body (mapcar #'optimize-ast (ast-let-body node))
                   :source-file (ast-source-file node)
                   :source-line (ast-source-line node)
                   :source-column (ast-source-column node)))
    (ast-if
     (make-ast-if :cond (optimize-ast (ast-if-cond node))
                  :then (optimize-ast (ast-if-then node))
                  :else (optimize-ast (ast-if-else node))
                  :source-file (ast-source-file node)
                  :source-line (ast-source-line node)
                  :source-column (ast-source-column node)))
    (ast-lambda
     (make-ast-lambda :params (ast-lambda-params node)
                      :optional-params (ast-lambda-optional-params node)
                      :rest-param (ast-lambda-rest-param node)
                      :key-params (ast-lambda-key-params node)
                      :declarations (ast-lambda-declarations node)
                      :body (mapcar #'optimize-ast (ast-lambda-body node))
                      :env (ast-lambda-env node)
                      :source-file (ast-source-file node)
                      :source-line (ast-source-line node)
                      :source-column (ast-source-column node)))
    (ast-defun
     (make-ast-defun :name (ast-defun-name node)
                     :params (ast-defun-params node)
                     :optional-params (ast-defun-optional-params node)
                     :rest-param (ast-defun-rest-param node)
                     :key-params (ast-defun-key-params node)
                     :declarations (ast-defun-declarations node)
                     :body (mapcar #'optimize-ast (ast-defun-body node))
                     :source-file (ast-source-file node)
                     :source-line (ast-source-line node)
                     :source-column (ast-source-column node)))
    (ast-defvar
     (make-ast-defvar :name (ast-defvar-name node)
                      :value (optimize-ast (ast-defvar-value node))
                      :kind (ast-defvar-kind node)
                      :source-file (ast-source-file node)
                      :source-line (ast-source-line node)
                      :source-column (ast-source-column node)))
    (ast-block
     (make-ast-block :name (ast-block-name node)
                     :body (mapcar #'optimize-ast (ast-block-body node))
                     :source-file (ast-source-file node)
                     :source-line (ast-source-line node)
                     :source-column (ast-source-column node)))
    (ast-return-from
     (make-ast-return-from :name (ast-return-from-name node)
                           :value (optimize-ast (ast-return-from-value node))
                           :source-file (ast-source-file node)
                           :source-line (ast-source-line node)
                           :source-column (ast-source-column node)))
    (ast-setq
     (make-ast-setq :var (ast-setq-var node)
                    :value (optimize-ast (ast-setq-value node))
                    :source-file (ast-source-file node)
                    :source-line (ast-source-line node)
                    :source-column (ast-source-column node)))
    (ast-the
     (make-ast-the :type (ast-the-type node)
                   :value (optimize-ast (ast-the-value node))
                   :source-file (ast-source-file node)
                   :source-line (ast-source-line node)
                   :source-column (ast-source-column node)))
    (t node)))

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
  (optimized-instructions nil))

(defun compile-toplevel-forms (forms &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
  Returns a compilation-result struct with program, assembly, and globals."
  (let* ((ctx (make-instance 'compiler-context :safety safety))
          (last-reg nil)
          (last-type nil)
          (last-cps nil)
          (compiled-asts nil)
          (type-env (cl-cc/type:type-env-empty)))
     (labels ((best-effort-type (ast env)
                (ignore-errors (type-check-ast ast env))))
       (let ((*compile-time-value-env* nil)
             (*compile-time-function-env* nil))
         (dolist (form forms)
           (unless (and (consp form) (eq (car form) 'in-package))
             (let* ((expanded (if (typep form 'ast-node)
                                  form
                                  (compiler-macroexpand-all form)))
                    (ast (if (typep expanded 'ast-node)
                             expanded
                             (lower-sexp-to-ast expanded))))
               (when (typep ast 'ast-defun)
                 (push (cons (ast-defun-name ast) ast) *compile-time-function-env*))
               (setf ast (optimize-ast ast))
               (push ast compiled-asts)
               (setf last-cps (maybe-cps-transform ast))
               (when type-check
                 (setf last-type
                       (handler-case
                          (type-check-ast ast type-env)
                        (error (e)
                          (if (eq type-check :strict)
                              (error e)
                              (progn
                                (warn "Type check warning: ~A" e)
                                nil))))))
               (when (and (typep ast 'cl-cc:ast-defvar)
                          (cl-cc:ast-defvar-value ast))
                 (let ((value-type (best-effort-type (cl-cc:ast-defvar-value ast) type-env)))
                   (when value-type
                     (setf type-env
                          (cl-cc/type:type-env-extend
                           (cl-cc:ast-defvar-name ast)
                           (cl-cc/type:type-to-scheme value-type)
                           type-env)))))
              (when (typep ast 'cl-cc:ast-defun)
                (let ((fn-type (best-effort-type ast type-env)))
                  (when fn-type
                    (setf type-env
                          (cl-cc/type:type-env-extend
                           (cl-cc:ast-defun-name ast)
                            (cl-cc/type:type-to-scheme fn-type)
                            type-env)))))
               (when (and (typep ast 'ast-defvar)
                          (ast-defvar-value ast))
                 (multiple-value-bind (value ok)
                     (%evaluate-ast (ast-defvar-value ast) *compile-time-eval-depth-limit*)
                   (when ok
                     (push (cons (ast-defvar-name ast) value) *compile-time-value-env*))))
               (setf last-reg (compile-ast ast ctx))))))
    (setf (ctx-type-env ctx) type-env)
    (when last-reg
      (emit ctx (make-vm-halt :reg last-reg)))
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
     (let* ((instructions (nreverse (ctx-instructions ctx)))
             (optimized nil)
             (leaf-p nil)
             (program nil))
          (multiple-value-setq (optimized leaf-p)
            (optimize-instructions instructions
                                   :pass-pipeline pass-pipeline
                                   :print-pass-timings print-pass-timings
                                   :timing-stream timing-stream
                                   :print-pass-stats print-pass-stats
                                   :stats-stream stats-stream
                                   :trace-json-stream trace-json-stream
                                   :print-opt-remarks print-opt-remarks
                                   :opt-remarks-stream opt-remarks-stream
                                   :opt-remarks-mode opt-remarks-mode))
        (setf program (make-vm-program
                       :instructions instructions
                       :result-register last-reg
                       :leaf-p leaf-p))
        (make-compilation-result :program program
                                 :assembly (emit-assembly program :target target)
                                 :globals (ctx-global-functions ctx)
                                 :type last-type
                                 :type-env (ctx-type-env ctx)
                                  :cps last-cps
                                  :ast (nreverse compiled-asts)
                                  :vm-instructions instructions
                                  :optimized-instructions optimized)))))

;;; ── Function call compilation ────────────────────────────────────────────
;;; Phase 2 (*phase2-builtin-handlers*) is defined in codegen-phase2.lisp.

(defun %resolve-func-sym-reg (sym ctx)
  "Return the register holding the value of function symbol SYM.
   Uses local env binding unless the symbol is a global function, in which
   case it loads the symbol itself into a fresh register."
  (let ((entry (assoc sym (ctx-env ctx)))
        (is-global (gethash sym (ctx-global-functions ctx))))
    (if (and entry (not is-global))
        (cdr entry)
        (let ((reg (make-register ctx)))
          (emit ctx (make-vm-const :dst reg :value sym))
          reg))))

(defmethod compile-ast ((node ast-call) ctx)
  (let* ((tail (ctx-tail-position ctx))
         (func-expr (progn (setf (ctx-tail-position ctx) nil)
                           (ast-call-func node)))
         (func-sym (cond ((symbolp func-expr) func-expr)
                         ((typep func-expr 'ast-var) (ast-var-name func-expr))
                         (t nil)))
         (args (ast-call-args node))
         (result-reg (make-register ctx)))
    (when (typep func-expr 'ast-var)
      (let ((entry (assoc (ast-var-name func-expr) (ctx-noescape-closure-bindings ctx))))
        (when entry
          (return-from compile-ast
            (%compile-inline-lambda-call (cdr entry) args tail ctx)))))
    (when (and func-sym
               (= (length args) 1)
               (typep (first args) 'ast-var)
               (member func-sym '(car cdr) :test #'eq))
      (let* ((arg-name (ast-var-name (first args)))
             (entry (assoc arg-name (ctx-noescape-cons-bindings ctx))))
        (when entry
          (emit ctx (make-vm-move :dst result-reg
                                  :src (if (eq func-sym 'car)
                                           (cadr entry)
                                           (cddr entry))))
          (return-from compile-ast result-reg))))
    (when func-sym
      (cond
        ((and func-sym
              (string= (symbol-name func-sym) "ARRAY-LENGTH")
              (= (length args) 1)
              (typep (first args) 'ast-var))
         (let* ((arg-name (ast-var-name (first args)))
                (entry (assoc arg-name (ctx-noescape-array-bindings ctx))))
           (when entry
             (emit ctx (make-vm-const :dst result-reg :value (cadr entry)))
             (return-from compile-ast result-reg))))
        ((and (eq func-sym 'aref)
              (= (length args) 2)
              (typep (first args) 'ast-var))
         (let* ((arg-name (ast-var-name (first args)))
                (entry (assoc arg-name (ctx-noescape-array-bindings ctx))))
           (when entry
             (if (typep (second args) 'ast-int)
                 (let ((index (ast-int-value (second args))))
                   (when (and (<= 0 index) (< index (cadr entry)))
                     (emit ctx (make-vm-move :dst result-reg
                                             :src (nth index (cddr entry))))))
                 (%emit-noescape-array-read entry (second args) result-reg ctx))
             (return-from compile-ast result-reg))))
        ((and func-sym
              (string= (symbol-name func-sym) "ASET")
              (= (length args) 3)
              (typep (first args) 'ast-var))
         (let* ((arg-name (ast-var-name (first args)))
                (entry (assoc arg-name (ctx-noescape-array-bindings ctx))))
           (when entry
             (if (typep (second args) 'ast-int)
                 (let ((index (ast-int-value (second args))))
                   (when (and (<= 0 index) (< index (cadr entry)))
                     (let ((val-reg (compile-ast (third args) ctx)))
                       (setf (nth index (cddr entry)) val-reg)
                       (emit ctx (make-vm-move :dst result-reg :src val-reg)))))
                 (%emit-noescape-array-write entry (second args) (third args) result-reg ctx))
             (return-from compile-ast result-reg)))))
    ;; ── Phase 1: Table-driven builtin dispatch ──────────────────────────
    ;; ~160 calling conventions via a single hash-table lookup + generic emitter.
    (when func-sym
      (let ((entry (gethash (symbol-name func-sym) *builtin-registry*)))
        (when entry
          (let ((result (emit-registered-builtin entry args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Phase 2: AST-introspecting builtins — Prolog-style dispatch ─────
    ;; Each handler returns result-reg on success, nil to fall through.
    (when func-sym
      (let ((handler (gethash (symbol-name func-sym) *phase2-builtin-handlers*)))
        (when handler
          (let ((result (funcall handler args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Normal function call ─────────────────────────────────────────────
    (let* ((raw-func-reg
             (cond ((symbolp func-expr) (%resolve-func-sym-reg func-expr ctx))
                   ((typep func-expr 'ast-var)
                    (%resolve-func-sym-reg (ast-var-name func-expr) ctx))
                   (t (compile-ast func-expr ctx))))
           ;; Unbox labels functions: extract closure from cons cell box
           (func-reg (if (and func-sym (assoc func-sym *labels-boxed-fns*))
                          (let ((unboxed (make-register ctx)))
                            (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                            unboxed)
                          raw-func-reg))
           (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
        (cond
          ;; Simple self-recursive tail calls lower to a loop: update the
          ;; current function parameter registers and jump back to the entry
          ;; label instead of emitting vm-tail-call.
          ((and tail
                (ctx-current-function-simple-p ctx)
                (symbolp func-expr)
                func-sym
                (eq func-sym (ctx-current-function-name ctx))
                (= (length args) (length (ctx-current-function-params ctx))))
            (setf arg-regs
                  (mapcar (lambda (arg-reg)
                            (let ((temp-reg (make-register ctx)))
                              (emit ctx (make-vm-move :dst temp-reg :src arg-reg))
                              temp-reg))
                          arg-regs))
            (dolist (param (ctx-current-function-params ctx))
              (let ((arg-reg (pop arg-regs)))
                (emit ctx (make-vm-move :dst (lookup-var ctx param) :src arg-reg))))
            (emit ctx (make-vm-jump :label (ctx-current-function-label ctx))))
         ((and func-sym (gethash func-sym (ctx-global-generics ctx)))
          (emit ctx (make-vm-generic-call :dst result-reg :gf-reg func-reg :args arg-regs)))
         (tail
          (emit ctx (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs)))
         (t
          (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs))))
        result-reg))))

;;; ── Function references and local function bindings ──────────────────────

(defun %compile-body-with-tail (body tail ctx)
  "Compile BODY forms with tail-position tracking. Returns the last result register."
  (let ((last-reg nil))
    (dolist (form body)
      (setf (ctx-tail-position ctx)
            (if (eq form (car (last body))) tail nil))
      (setf last-reg (compile-ast form ctx)))
    last-reg))

(defun %compile-inline-lambda-call (lambda-node arg-asts tail ctx)
  "Compile a direct call to a non-escaping lambda without emitting vm-closure."
  (let ((old-env (ctx-env ctx))
        (old-tail (ctx-tail-position ctx)))
    (unwind-protect
         (let ((arg-bindings (loop for param in (ast-lambda-params lambda-node)
                                   for arg-ast in arg-asts
                                   collect (cons param (compile-ast arg-ast ctx)))))
           (setf (ctx-env ctx) (append arg-bindings old-env))
            (%compile-body-with-tail (ast-lambda-body lambda-node) tail ctx))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-tail-position ctx) old-tail))))

(defun %emit-noescape-array-read (entry index-ast result-reg ctx)
  (let* ((size (cadr entry))
         (elements (cddr entry))
         (index-reg (compile-ast index-ast ctx))
         (end-label (make-label ctx "noescape_array_read_end"))
         (error-label (make-label ctx "noescape_array_read_error"))
         (err-reg (make-register ctx)))
    (dotimes (i size)
      (let ((idx-const (make-register ctx))
            (eq-reg (make-register ctx))
            (next-label (make-label ctx "noescape_array_read_next")))
        (emit ctx (make-vm-const :dst idx-const :value i))
        (emit ctx (make-vm-num-eq :dst eq-reg :lhs index-reg :rhs idx-const))
        (emit ctx (make-vm-jump-zero :reg eq-reg :label next-label))
        (emit ctx (make-vm-move :dst result-reg :src (nth i elements)))
        (emit ctx (make-vm-jump :label end-label))
        (emit ctx (make-vm-label :name next-label))))
    (emit ctx (make-vm-jump :label error-label))
    (emit ctx (make-vm-label :name error-label))
    (emit ctx (make-vm-const :dst err-reg :value "no-escape array index out of bounds"))
    (emit ctx (make-vm-signal-error :error-reg err-reg))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defun %emit-noescape-array-write (entry index-ast value-ast result-reg ctx)
  (let* ((size (cadr entry))
         (elements (cddr entry))
         (index-reg (compile-ast index-ast ctx))
         (val-reg (compile-ast value-ast ctx))
         (end-label (make-label ctx "noescape_array_write_end"))
         (error-label (make-label ctx "noescape_array_write_error"))
         (err-reg (make-register ctx)))
    (dotimes (i size)
      (let ((idx-const (make-register ctx))
            (eq-reg (make-register ctx))
            (next-label (make-label ctx "noescape_array_write_next")))
        (emit ctx (make-vm-const :dst idx-const :value i))
        (emit ctx (make-vm-num-eq :dst eq-reg :lhs index-reg :rhs idx-const))
        (emit ctx (make-vm-jump-zero :reg eq-reg :label next-label))
        (setf (nth i elements) val-reg)
        (emit ctx (make-vm-move :dst result-reg :src val-reg))
        (emit ctx (make-vm-jump :label end-label))
        (emit ctx (make-vm-label :name next-label))))
    (emit ctx (make-vm-jump :label error-label))
    (emit ctx (make-vm-label :name error-label))
    (emit ctx (make-vm-const :dst err-reg :value "no-escape array index out of bounds"))
    (emit ctx (make-vm-signal-error :error-reg err-reg))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defun %compile-closure-body (ctx params param-regs body-forms env)
  "Emit the body of a local function (flet/labels closure).
   Binds PARAMS to PARAM-REGS in CTX-ENV under base ENV, compiles BODY-FORMS
   in tail position, emits vm-ret with the last result register.
   Restores CTX-ENV to ENV on exit."
  (let ((param-bindings (loop for param in params
                               for param-reg in param-regs
                               collect (cons param param-reg))))
    (setf (ctx-env ctx) (append param-bindings env))
    (let ((last-reg nil))
      (dolist (form body-forms)
        (setf (ctx-tail-position ctx)
              (if (eq form (car (last body-forms))) t nil))
        (setf last-reg (compile-ast form ctx)))
      (setf (ctx-tail-position ctx) nil)
      (emit ctx (make-vm-ret :reg last-reg)))))

(defmethod compile-ast ((node ast-function) ctx)
  "Compile #'name — look up function by name, returning a closure reference."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-function-name node))
         (dst (make-register ctx)))
    (cond
      ((symbolp name)
        (let ((entry (assoc name (ctx-env ctx))))
          (if entry
              (if (assoc name *labels-boxed-fns*)
                  (emit ctx (make-vm-car :dst dst :src (cdr entry)))
                  (emit ctx (make-vm-move :dst dst :src (cdr entry))))
              (if (gethash name (ctx-global-generics ctx))
                  (emit ctx (make-vm-get-global :dst dst :name name))
                  (emit ctx (make-vm-func-ref :dst dst :label (format nil "~A" name)))))))
      ((and (consp name) (eq (car name) 'setf))
        (emit ctx (make-vm-func-ref :dst dst :label (format nil "SETF_~A" (second name)))))
      (t
       (error "Invalid function name: ~S" name)))
    dst))

(defmethod compile-ast ((node ast-flet) ctx)
  "Compile flet: non-recursive local function bindings."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-flet-bindings node))
         (body (ast-flet-body node))
         (old-env (ctx-env ctx)))
    (unwind-protect
         (progn
           (let ((func-bindings nil))
             (dolist (binding bindings)
               (let* ((name (first binding))
                      (params (second binding))
                      (body-forms (cddr binding))
                      (func-label (make-label ctx "flet_fn"))
                      (closure-reg (make-register ctx)))
                 (let* ((body-ast (make-ast-progn :forms body-forms))
                        (free-vars (find-free-variables body-ast))
                        (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                               (remove-if-not
                                                (lambda (v) (assoc v old-env))
                                                (set-difference free-vars params)))))
                    (let ((param-regs (loop for i from 0 below (length params)
                                            collect (make-register ctx)))
                          (skip-label (make-label ctx "flet_skip")))
                      (if captured-vars
                          (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                                     :params param-regs :captured captured-vars))
                          (emit ctx (make-vm-func-ref :dst closure-reg :label func-label)))
                      (push (cons name closure-reg) func-bindings)
                      (emit ctx (make-vm-jump :label skip-label))
                      (emit ctx (make-vm-label :name func-label))
                     (%compile-closure-body ctx params param-regs body-forms old-env)
                     (emit ctx (make-vm-label :name skip-label))))))
             (setf (ctx-env ctx) (append (nreverse func-bindings) old-env)))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx) old-env))))

(defmethod compile-ast ((node ast-labels) ctx)
  "Compile labels: mutually recursive local function bindings via cons-cell boxing."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-labels-bindings node))
         (body (ast-labels-body node))
         (old-env (ctx-env ctx))
         (old-labels-boxes *labels-boxed-fns*))
    (let ((body-result-reg
            (unwind-protect
                 (progn
                   ;; Phase 1: Create mutable boxes for each function
                   (let ((func-infos nil)
                         (nil-reg (make-register ctx)))
                     (emit ctx (make-vm-const :dst nil-reg :value nil))
                     (dolist (binding bindings)
                       (let* ((name (first binding))
                              (params (second binding))
                              (func-label (make-label ctx "labels_fn"))
                              (closure-reg (make-register ctx))
                              (box-reg (make-register ctx)))
                         (emit ctx (make-vm-cons :dst box-reg
                                                 :car-src nil-reg :cdr-src nil-reg))
                         (push (list name func-label closure-reg box-reg params) func-infos)))
                     (let ((forward-env (mapcar (lambda (info) (cons (first info) (fourth info)))
                                                func-infos)))
                       (setf *labels-boxed-fns* (append forward-env old-labels-boxes))
                       (setf (ctx-env ctx) (append forward-env old-env))
                       ;; Phase 2: Create closures and fill boxes
                       (dolist (info (nreverse func-infos))
                         (destructuring-bind (name func-label closure-reg box-reg params) info
                           (let* ((binding-data (find name bindings :key #'first))
                                  (body-forms (cddr binding-data))
                                  (body-ast (make-ast-progn :forms body-forms))
                                  (free-vars (set-difference (find-free-variables body-ast) params))
                                  (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                                         (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                                        free-vars)))
                                  (param-regs (loop for i from 0 below (length params)
                                                    collect (make-register ctx)))
                                  (skip-label (make-label ctx "labels_skip")))
                             (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                                        :params param-regs :captured captured-vars))
                             (emit ctx (make-vm-rplaca :cons box-reg :val closure-reg))
                             (emit ctx (make-vm-jump :label skip-label))
                             (emit ctx (make-vm-label :name func-label))
                             (%compile-closure-body ctx params param-regs body-forms
                                                   (append forward-env old-env))
                             (emit ctx (make-vm-label :name skip-label)))))
                       ;; Phase 3: Compile body with box-resolved env
                       (let ((func-env (mapcar (lambda (binding)
                                                 (cons (first binding)
                                                       (lookup-var ctx (first binding))))
                                               bindings)))
                         (setf (ctx-env ctx) (append func-env old-env)))
                       (%compile-body-with-tail body tail ctx))))
              (setf (ctx-env ctx) old-env)
              (setf *labels-boxed-fns* old-labels-boxes))))
      body-result-reg)))

;;; ── Assembly and type utilities ───────────────────────────────────────────

(defun target-instance (target)
  (ecase target
    (:x86_64 (make-instance 'x86-64-target))
    (:aarch64 (make-instance 'aarch64-target))
    (:vm nil)))

(defun emit-assembly (program &key (target :x86_64))
  (when (eq target :vm)
    (return-from emit-assembly ""))
  (when (eq target :wasm)
    (return-from emit-assembly (compile-to-wasm-wat program)))
  (let ((target-object (target-instance target)))
    (when (and (typep target-object 'x86-64-target)
               (vm-program-instructions program))
      (let* ((instructions (vm-program-instructions program))
             (ra (allocate-registers instructions *x86-64-calling-convention*)))
        (setf (target-regalloc target-object) ra)
        (setf (target-spill-base-reg target-object)
              (if (cl-cc::x86-64-red-zone-spill-p (vm-program-leaf-p program)
                                                 (regalloc-spill-count ra))
                  :rsp
                  :rbp))
        (setf program (make-vm-program
                       :instructions (regalloc-instructions ra)
                       :result-register (vm-program-result-register program)
                       :leaf-p (vm-program-leaf-p program)))))
    (with-output-to-string (s)
      (format s "; CL-CC bootstrap assembly (~A)~%" target)
      (format s "clcc_entry:~%")
      (dolist (inst (vm-program-instructions program))
        (emit-instruction target-object inst s)))))

(defun type-check-ast (ast &optional (env (cl-cc/type:type-env-empty)))
  "Run type inference on AST. Returns inferred type or signals type error."
  (cl-cc/type:reset-type-vars!)
  (multiple-value-bind (type subst)
      (cl-cc/type:infer ast env)
    (cl-cc/type:type-substitute type subst)))
