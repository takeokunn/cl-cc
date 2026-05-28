(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Function Call Compilation
;;;
;;; Extracted from codegen.lisp.
;;; Contains:
;;;   - %resolve-func-sym-reg     — resolve a symbol to a function register
;;;   - %try-compile-funcall      — handle (funcall fn arg...)
;;;   - %try-compile-apply        — handle (apply fn arg...)
;;;   - %try-compile-noescape-closure — inline noescape lambda closures
;;;   - %try-compile-noescape-cons    — optimise (car/cdr noescape-cons) → move
;;;   - %try-compile-noescape-array   — optimise aref/aset on noescape arrays
;;;   - %try-compile-builtin      — Phase-1 registry + Phase-2 Prolog dispatch
;;;   - %compile-normal-call      — general call: self-recursive, GF, tail, normal
;;;   - compile-ast (ast-call)    — priority-ordered dispatch over the above
;;;
;;; Load order: after codegen.lisp, before codegen-locals.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Function call compilation ────────────────────────────────────────────
;;; Phase 2 (*phase2-builtin-handlers*) is defined in codegen-phase2.lisp.


(defun %codegen-call-assoc (key alist)
  (assoc key alist :test #'eq))

(defun %resolve-func-sym-reg (sym ctx)
  "Return the register holding the callable value for function symbol SYM.
   Local function bindings come from CTX-ENV. Global generic functions must be
   loaded from the persistent global store, while ordinary global functions are
   represented by their symbol and resolved later by the VM."
  (let ((entry (%codegen-call-assoc sym (ctx-env ctx)))
        (is-global-function (gethash sym (ctx-global-functions ctx)))
        (is-global-generic (gethash sym (ctx-global-generics ctx))))
    (cond
      ((and entry (not is-global-function) (not is-global-generic))
       (cdr entry))
      (is-global-generic
       (let ((reg (make-register ctx)))
         (emit ctx (make-vm-get-global :dst reg :name sym))
         reg))
      (t
       (let ((reg (make-register ctx)))
          (emit ctx (make-vm-const :dst reg :value sym))
          reg)))))

(defun %ast-var-function-value-p (name ctx)
  "Return T when NAME in function position should be treated as a variable value.
Global functions/generics stay symbolic; only local lexical bindings count as
first-class function values here."
  (let ((entry (%codegen-call-assoc name (ctx-env ctx)))
        (is-global-function (gethash name (ctx-global-functions ctx)))
        (is-global-generic (gethash name (ctx-global-generics ctx))))
    (and entry (not is-global-function) (not is-global-generic))))

(defun %compile-call-arg-registers (args ctx)
  "Compile ARGS left-to-right and return their result registers."
  (mapcar (lambda (arg) (compile-ast arg ctx)) args))

(defun %emit-call-like-instruction (tail result-reg func-reg arg-regs ctx)
  "Emit either a normal call or a tail call and return RESULT-REG."
  (if tail
      (emit ctx (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs))
      (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs)))
  result-reg)

(defun %known-call-live-regs (ctx func-reg arg-regs)
  "Return caller registers that must survive a known non-tail call."
  (let* ((prefix (nreverse (copy-list (ctx-instructions ctx))))
         (call-index (length prefix))
         (synthetic-call (make-vm-call :dst :%known-call-result
                                       :func func-reg
                                       :args arg-regs))
         (intervals (or (ignore-errors
                          (compute-live-intervals (append prefix (list synthetic-call))))
                        nil))
         (env-regs (loop for binding in (ctx-env ctx)
                         for reg = (cdr binding)
                         when (keywordp reg)
                           collect reg))
         (noescape-regs
           (append
            (loop for binding in (ctx-noescape-cons-bindings ctx)
                  append (list (cadr binding) (cddr binding)))
            (loop for binding in (ctx-noescape-array-bindings ctx)
                  append (cddr binding))
            (loop for binding in (ctx-noescape-instance-bindings ctx)
                  append (loop for slot in (cdr binding) collect (cdr slot))))))
    (flet ((interval-live-p (reg)
             (some (lambda (interval)
                     (and (eq (interval-vreg interval) reg)
                          (<= (interval-start interval) call-index)
                          (>= (interval-end interval) call-index)))
                   intervals)))
      (remove-duplicates
       (remove-if-not (lambda (reg)
                        (and (keywordp reg)
                             (or (member reg env-regs :test #'eq)
                                 (member reg noescape-regs :test #'eq)
                                 (interval-live-p reg))))
                      (append env-regs noescape-regs (list func-reg) arg-regs))
       :test #'eq))))

;;; ── ast-call fast-path helpers ──────────────────────────────────────────────
;;; Each returns RESULT-REG on success, NIL to fall through to the next handler.
;;; This "try-each" pattern is the Lisp equivalent of Prolog's clause selection.

(defun %try-compile-funcall (func-sym args result-reg tail ctx)
  "Handle (funcall FN ARG...) — emit call/tail-call on the fn value."
  (when (and func-sym (eq func-sym 'funcall) args)
    (let* ((func-arg (first args))
           (local-func-name (and (typep func-arg 'ast-var) (ast-var-name func-arg)))
           (raw-func-reg (compile-ast func-arg ctx))
           (func-reg0 (if (and local-func-name (%codegen-call-assoc local-func-name *labels-boxed-fns*))
                          (let ((unboxed (make-register ctx)))
                            (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                            unboxed)
                          raw-func-reg))
           (func-reg (make-register ctx)))
      (emit ctx (make-vm-move :dst func-reg :src func-reg0))
      (let ((arg-regs (%compile-call-arg-registers (rest args) ctx)))
        (%emit-call-like-instruction tail result-reg func-reg arg-regs ctx)))))

(defun %try-compile-apply (func-sym args result-reg tail ctx)
  "Handle (apply FN ARG...) — use flat calls when the spread arity is known."
  (when (and func-sym (eq func-sym 'apply) args)
    (let* ((func-arg (first args))
           (apply-args (rest args))
           (local-func-name (and (typep func-arg 'ast-var) (ast-var-name func-arg)))
           (raw-func-reg (cond
                            ((typep func-arg 'ast-function)
                             (compile-ast func-arg ctx))
                           ((typep func-arg 'ast-var)
                            (compile-ast func-arg ctx))
                           ((symbolp func-arg)
                            (%resolve-func-sym-reg func-arg ctx))
                           (t
                            (compile-ast func-arg ctx))))
           (func-reg0 (if (and local-func-name (%codegen-call-assoc local-func-name *labels-boxed-fns*))
                          (let ((unboxed (make-register ctx)))
                            (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                            unboxed)
                           raw-func-reg))
            (func-reg (make-register ctx)))
      (emit ctx (make-vm-move :dst func-reg :src func-reg0))
      (let* ((plan (%apply-argument-plan apply-args))
             (leading-args (getf plan :leading))
             (spread-arg (getf plan :spread)))
        (multiple-value-bind (literal-spread-p spread-values)
            (%literal-apply-spread-values spread-arg)
          (if literal-spread-p
               (%compile-apply-literal-spread leading-args spread-values func-reg result-reg tail ctx)
               (multiple-value-bind (list-spread-p spread-forms)
                   (%list-call-apply-spread-forms spread-arg ctx)
                 (if list-spread-p
                     (%compile-apply-list-call-spread leading-args spread-forms func-reg result-reg tail ctx)
                    (%compile-apply-dynamic-spread apply-args func-reg result-reg tail ctx)))))))))

(defun %try-compile-noescape-closure (func-expr args tail ctx)
  "Inline a noescape lambda closure — no vm-closure emitted."
  (when (typep func-expr 'ast-var)
    (let ((entry (%codegen-call-assoc (ast-var-name func-expr) (ctx-noescape-closure-bindings ctx))))
      (when entry
        (%compile-inline-lambda-call (cdr entry) args tail ctx)))))

(defun %try-compile-noescape-cons (func-sym args result-reg ctx)
  "Optimise (car/cdr NOESCAPE-CONS) to a register move."
  (when (and func-sym
             (= (length args) 1)
             (typep (first args) 'ast-var)
             (or (eq func-sym 'car) (eq func-sym 'cdr)))
    (let* ((arg-name (ast-var-name (first args)))
           (entry    (%codegen-call-assoc arg-name (ctx-noescape-cons-bindings ctx))))
      (when entry
        (emit ctx (make-vm-move :dst result-reg
                                :src (if (eq func-sym 'car) (cadr entry) (cddr entry))))
        result-reg))))

(defun %noescape-array-entry-size (entry)
  (cadr entry))

(defun %noescape-array-entry-element-regs (entry)
  "Return element register list from typed noescape-array ENTRY payload."
  (cdr (cddr entry)))

(defun %try-compile-noescape-array (func-sym args result-reg ctx)
  "Optimise ARRAY-LENGTH / AREF / ASET on noescape arrays to moves/consts."
  (when func-sym
    (let ((sname (symbol-name func-sym)))
      (cond
        ((and (string= sname "ARRAY-LENGTH")
              (= (length args) 1)
              (typep (first args) 'ast-var))
           (let* ((arg-name (ast-var-name (first args)))
                  (entry    (%codegen-call-assoc arg-name (ctx-noescape-array-bindings ctx))))
             (when entry
              (emit ctx (make-vm-const :dst result-reg :value (%noescape-array-entry-size entry)))
               result-reg)))
        ((and (eq func-sym 'aref)
              (= (length args) 2)
              (typep (first args) 'ast-var))
          (let* ((arg-name (ast-var-name (first args)))
                 (entry    (%codegen-call-assoc arg-name (ctx-noescape-array-bindings ctx))))
             (when entry
               (if (typep (second args) 'ast-int)
                   (let ((index (ast-int-value (second args))))
                     (when (and (<= 0 index) (< index (%noescape-array-entry-size entry)))
                       (emit ctx (make-vm-move :dst result-reg
                                               :src (nth index (%noescape-array-entry-element-regs entry))))))
                   (%emit-noescape-array-read entry (second args) result-reg ctx))
               result-reg)))
        ((and (string= sname "ASET")
              (= (length args) 3)
              (typep (first args) 'ast-var))
          (let* ((arg-name (ast-var-name (first args)))
                 (entry    (%codegen-call-assoc arg-name (ctx-noescape-array-bindings ctx))))
             (when entry
               (if (typep (second args) 'ast-int)
                   (let ((index (ast-int-value (second args))))
                     (when (and (<= 0 index) (< index (%noescape-array-entry-size entry)))
                       (let ((val-reg (compile-ast (third args) ctx)))
                        (setf (nth index (%noescape-array-entry-element-regs entry)) val-reg)
                         (emit ctx (make-vm-move :dst result-reg :src val-reg)))))
                   (%emit-noescape-array-write entry (second args) (third args) result-reg ctx))
                result-reg)))))))

(defun %emit-argument-rebinds-and-jump (arg-regs params param-regs target-label ctx)
  "Snapshot ARG-REGS, move them into PARAM-REGS/PARAMS, then jump to TARGET-LABEL."
  (let ((temps (loop for reg in arg-regs
                     collect (let ((t-reg (make-register ctx)))
                               (emit ctx (make-vm-move :dst t-reg :src reg))
                               t-reg))))
    (loop for param in params
          for temp in temps
          for param-reg = (and param-regs (pop param-regs))
          do (emit ctx (make-vm-move :dst (or param-reg (lookup-var ctx param))
                                     :src temp))))
  (emit ctx (make-vm-jump :label target-label)))

(defun %try-compile-local-tail-jump (func-sym args result-reg tail ctx)
  "Lower a tail call to an eligible local labels/flet binding into param moves + vm-jump."
  (when (and tail func-sym)
    (let ((entry (%codegen-call-assoc func-sym *local-tail-jump-fns*)))
      (when entry
        (destructuring-bind (target-label params param-regs) (cdr entry)
          (when (= (length args) (length params))
            (let ((arg-regs (%compile-call-arg-registers args ctx)))
              (%emit-argument-rebinds-and-jump arg-regs params param-regs target-label ctx)
                result-reg)))))))

(defun %try-compile-continuation-operator (func-sym args result-reg ctx)
  "Lower continuation operators to dedicated VM control instructions."
  (when func-sym
    (let ((name (symbol-name func-sym)))
      (cond
        ((and (member name '("CALL-WITH-CURRENT-CONTINUATION" "CALL/CC") :test #'string=)
              (= (length args) 1))
         (let ((fn-reg (compile-ast (first args) ctx)))
           (emit ctx (make-vm-call/cc :dst result-reg :func fn-reg))
           result-reg))
        ((and (string= name "CALL-WITH-CONTINUATION-PROMPT")
              (= (length args) 2))
         (let ((fn-reg (compile-ast (first args) ctx))
               (prompt-reg (compile-ast (second args) ctx)))
           (emit ctx (make-vm-call-with-prompt :dst result-reg
                                               :func fn-reg
                                               :prompt prompt-reg))
           result-reg))
        ((and (string= name "ABORT-TO-PROMPT")
              (= (length args) 2))
         (let ((prompt-reg (compile-ast (first args) ctx))
               (value-reg (compile-ast (second args) ctx)))
           (emit ctx (make-vm-abort-to-prompt :prompt prompt-reg :value value-reg))
           result-reg))))))

(defun %try-compile-equality-predicate (func-sym args result-reg ctx)
  "Lower EQ/EQL/EQUAL through a type-specialized equality constructor when possible."
  (when (and (= (length args) 2)
             (member func-sym '(eq eql equal) :test #'eq))
    (let ((ctor (%equality-predicate-constructor func-sym (first args) (second args) ctx)))
      (when ctor
        (let ((lhs-reg (compile-ast (first args) ctx))
              (rhs-reg (compile-ast (second args) ctx)))
          (emit ctx (funcall ctor :dst result-reg :lhs lhs-reg :rhs rhs-reg))
          result-reg)))))

(defun %try-compile-builtin (func-sym args result-reg ctx)
  "Phase-1 (table-driven) + Phase-2 (Prolog-style) builtin dispatch."
  (when (and func-sym
             (not (%ast-var-function-value-p func-sym ctx)))
    (let ((sname (symbol-name func-sym)))
      (let ((result (%try-compile-equality-predicate func-sym args result-reg ctx)))
        (when result (return-from %try-compile-builtin result)))
      ;; Phase 1: ~160 calling conventions via registry hash-table
      (let ((entry (gethash sname *builtin-registry*)))
        (when entry
          (let ((result (emit-registered-builtin entry args result-reg ctx)))
            (when result (return-from %try-compile-builtin result)))))
      ;; Phase 2: AST-introspecting handlers — each returns result-reg or NIL
      (let ((handler (gethash sname *phase2-builtin-handlers*)))
        (when handler
          (funcall handler args result-reg ctx))))))

(defun %compile-normal-call (func-expr func-sym args result-reg tail ctx)
  "Emit a normal (non-special-cased) function call."
  (let* ((local-func-name (and (typep func-expr 'ast-var) (ast-var-name func-expr)))
         (raw-func-reg
            (cond ((symbolp func-expr)        (%resolve-func-sym-reg func-expr ctx))
                  ((typep func-expr 'ast-var)
                   (if (%ast-var-function-value-p (ast-var-name func-expr) ctx)
                       (compile-ast func-expr ctx)
                       (%resolve-func-sym-reg (ast-var-name func-expr) ctx)))
                  (t                           (compile-ast func-expr ctx))))
          ;; Unbox labels-boxed functions: extract closure from cons-cell box
          (func-reg0 (if (let ((name (or func-sym local-func-name)))
                           (and name (%codegen-call-assoc name *labels-boxed-fns*)))
                         (let ((unboxed (make-register ctx)))
                           (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                           unboxed)
                         raw-func-reg))
          (func-reg (make-register ctx)))
    (emit ctx (make-vm-move :dst func-reg :src func-reg0))
    (let ((arg-regs (%compile-call-arg-registers args ctx)))
     (cond
      ;; Self-recursive simple tail call → update params + jump (loop lowering)
       ((and tail
             (ctx-current-function-simple-p ctx)
             func-sym
             (symbolp func-expr)
             (eq func-sym (ctx-current-function-name ctx))
             (= (length args) (length (ctx-current-function-params ctx))))
        (%emit-argument-rebinds-and-jump arg-regs
                                         (ctx-current-function-params ctx)
                                         nil
                                         (ctx-current-function-label ctx)
                                         ctx))
      ;; Generic function dispatch
      ((and func-sym (gethash func-sym (ctx-global-generics ctx)))
       (emit ctx (make-vm-generic-call :dst result-reg :gf-reg func-reg :args arg-regs)))
      ;; Tail call
       (tail
        (emit ctx (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs)))
        ;; Regular call
        (t
         (emit ctx (if (or func-sym local-func-name)
                       (make-vm-call :dst result-reg :func func-reg :args arg-regs
                                     :live-regs (%known-call-live-regs ctx func-reg arg-regs))
                       (make-vm-call :dst result-reg :func func-reg :args arg-regs)))))
      result-reg)))

(defmacro %define-call-fast-path-dispatcher (name lambda-list &body handler-forms)
  "Define a call fast-path dispatcher from ordered HANDLER-FORMS.
Each handler form must return RESULT-REG on success or NIL to continue."
  `(defun ,name ,lambda-list
     "Try ordered call fast paths. Returns RESULT-REG on success, NIL otherwise."
     (or ,@handler-forms)))

(%define-call-fast-path-dispatcher %try-compile-call-fast-paths
    (func-expr func-sym args result-reg tail ctx)
  (%try-compile-funcall          func-sym  args result-reg tail ctx)
  (%try-compile-apply            func-sym  args result-reg tail ctx)
  (%try-compile-noescape-closure func-expr args             tail ctx)
  (%try-compile-noescape-cons    func-sym  args result-reg      ctx)
  (%try-compile-noescape-array   func-sym  args result-reg      ctx)
  (%try-compile-local-tail-jump  func-sym  args result-reg tail ctx)
  (%try-compile-continuation-operator func-sym args result-reg ctx)
  (and (member (symbol-name func-sym) '("LOAD-TIME-VALUE" "%LOAD-TIME-VALUE") :test #'string=)
       (%compile-load-time-value-call args result-reg ctx))
  (and (member (symbol-name func-sym) '("NTH-VALUE" "%NTH-VALUE") :test #'string=)
       (%compile-nth-value-call args result-reg ctx))
  (%try-compile-builtin          func-sym  args result-reg      ctx))

(defmethod compile-ast ((node ast-call) ctx)
  "Compile a function call via priority-ordered fast-path dispatch.
   Each %try-compile-* returns RESULT-REG on success, NIL to fall through."
  (let* ((tail      (ctx-tail-position ctx))
         (func-expr (progn (setf (ctx-tail-position ctx) nil) (ast-call-func node)))
         (func-sym  (cond ((symbolp func-expr) func-expr)
                          ((and (typep func-expr 'ast-var)
                                (not (%ast-var-function-value-p (ast-var-name func-expr) ctx)))
                           (ast-var-name func-expr))
                          (t nil)))
         (args       (ast-call-args node))
         (result-reg (make-register ctx)))
    (or (%try-compile-call-fast-paths func-expr func-sym args result-reg tail ctx)
        (%compile-normal-call          func-expr func-sym args result-reg tail ctx))))

(defmethod compile-ast ((node ast-list) ctx)
  "Compile a runtime list-construction AST node through the normal LIST call path."
  (compile-ast (make-ast-call :func (make-ast-var :name 'list)
                              :args (ast-list-elements node))
               ctx))
