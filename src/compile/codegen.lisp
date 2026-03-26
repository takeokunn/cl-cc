(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Entry Points + Calls + Exception Handling + Multiple Values
;;;
;;; Contains: compile-toplevel-forms, ast-call (Phase 1 + Phase 2 dispatch),
;;; exception handling (catch/throw/unwind-protect/handler-case),
;;; multiple values (values/mvb/apply/mv-call/mv-prog1),
;;; ast-function, ast-flet, ast-labels, and assembly utilities.
;;;
;;; Load order: after codegen-functions. compile-ast defgeneric and all
;;; methods for primitives, CLOS, and functions live in the preceding files.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Top-level compilation entry point ────────────────────────────────────

(defstruct compilation-result
  "Result of compiling expressions or top-level forms."
  (program nil)
  (assembly nil)
  (globals nil)
  (type nil)
  (cps nil))

(defun compile-toplevel-forms (forms &key (target :x86_64))
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (let* ((ctx (make-instance 'compiler-context))
         (last-reg nil))
    (dolist (form forms)
      (let* ((expanded (if (typep form 'ast-node)
                           form
                           (compiler-macroexpand-all form)))
             (ast (if (typep expanded 'ast-node)
                      expanded
                      (lower-sexp-to-ast expanded))))
        (setf last-reg (compile-ast ast ctx))))
    (when last-reg
      (emit ctx (make-vm-halt :reg last-reg)))
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
    (let* ((instructions (nreverse (ctx-instructions ctx)))
           (optimized (optimize-instructions instructions))
           (program (make-vm-program
                     :instructions optimized
                     :result-register last-reg)))
      (make-compilation-result :program program
                               :assembly (emit-assembly program :target target)
                               :globals (ctx-global-functions ctx)))))

;;; ── Exception handling: catch / throw / unwind-protect / handler-case ────

(defmethod compile-ast ((node ast-catch) ctx)
  (let ((tag-reg (compile-ast (ast-catch-tag node) ctx))
        (result-reg (make-register ctx))
        (end-label (make-label ctx "catch_end")))
    (declare (ignore tag-reg))
    (emit ctx (make-vm-label :name (make-label ctx "catch_start")))
    (let ((body-result (let ((last nil))
                         (dolist (form (ast-catch-body node))
                           (setf last (compile-ast form ctx)))
                         last)))
      (emit ctx (make-vm-move :dst result-reg :src body-result)))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-throw) ctx)
  (let ((tag-reg (compile-ast (ast-throw-tag node) ctx))
        (value-reg (compile-ast (ast-throw-value node) ctx)))
    (declare (ignore tag-reg))
    value-reg))

(defmethod compile-ast ((node ast-unwind-protect) ctx)
  (let ((result-reg (make-register ctx))
        (error-reg (make-register ctx))
        (error-flag-reg (make-register ctx))
        (handler-label (make-label ctx "unwind_handler"))
        (cleanup-label (make-label ctx "unwind_cleanup"))
        (end-label (make-label ctx "unwind_end")))
    (emit ctx (make-vm-const :dst error-flag-reg :value nil))
    (emit ctx (make-vm-establish-handler
               :handler-label handler-label
               :result-reg error-reg
               :error-type 'error))
    (let ((protected-result (compile-ast (ast-unwind-protected node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src protected-result)))
    (emit ctx (make-vm-remove-handler))
    (emit ctx (make-vm-jump :label cleanup-label))
    (emit ctx (make-vm-label :name handler-label))
    (emit ctx (make-vm-const :dst error-flag-reg :value t))
    (emit ctx (make-vm-label :name cleanup-label))
    (dolist (form (ast-unwind-cleanup node))
      (compile-ast form ctx))
    (emit ctx (make-vm-jump-zero :reg error-flag-reg :label end-label))
    (emit ctx (make-vm-sync-handler-regs))
    (emit ctx (make-vm-signal-error :error-reg error-reg))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case: establish handlers, run protected form, handle errors."
  (let* ((clauses (ast-handler-case-clauses node))
         (result-reg (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos (mapcar (lambda (clause)
                                  (declare (ignore clause))
                                  (list (make-label ctx "handler")
                                        (make-register ctx)))
                                clauses)))
    ;; 1. Establish handlers in reverse so first clause is on top
    (loop for clause in (reverse clauses)
          for info in (reverse handler-infos)
          do (emit ctx (make-vm-establish-handler
                        :handler-label (first info)
                        :result-reg (second info)
                        :error-type (first clause))))
    ;; 2. Protected form
    (let ((form-result (compile-ast (ast-handler-case-form node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src form-result)))
    ;; 3. Remove all handlers
    (dotimes (i (length clauses))
      (emit ctx (make-vm-remove-handler)))
    (emit ctx (make-vm-jump :label normal-exit-label))
    ;; 4. Handler bodies
    (loop for clause in clauses
          for info in handler-infos
          do (let* ((var (second clause))
                    (body (cddr clause))
                    (handler-label (first info))
                    (error-reg (second info))
                    (old-env (ctx-env ctx)))
               (emit ctx (make-vm-label :name handler-label))
               (unwind-protect
                    (progn
                      (when var
                        (push (cons var error-reg) (ctx-env ctx)))
                      (let ((last-reg nil))
                        (if body
                            (dolist (form body)
                              (setf last-reg (compile-ast form ctx)))
                            (setf last-reg error-reg))
                        (emit ctx (make-vm-move :dst result-reg :src last-reg))))
                 (setf (ctx-env ctx) old-env))
               (emit ctx (make-vm-jump :label normal-exit-label))))
    (emit ctx (make-vm-label :name normal-exit-label))
    result-reg))

;;; ── Multiple values ───────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-values) ctx)
  (let* ((src-regs (mapcar (lambda (form) (compile-ast form ctx))
                           (ast-values-forms node)))
         (dst (make-register ctx)))
    (emit ctx (make-vm-values :dst dst :src-regs src-regs))
    dst))

(defmethod compile-ast ((node ast-multiple-value-bind) ctx)
  (let* ((vars (ast-mvb-vars node))
         (body (ast-mvb-body node))
         (old-env (ctx-env ctx)))
    (compile-ast (ast-mvb-values-form node) ctx)
    (let ((var-regs (loop for v in vars collect (make-register ctx))))
      (emit ctx (make-vm-mv-bind :dst-regs var-regs))
      (unwind-protect
           (progn
             (setf (ctx-env ctx)
                   (append (mapcar #'cons vars var-regs) (ctx-env ctx)))
             (let ((last nil))
               (dolist (form body)
                 (setf last (compile-ast form ctx)))
               last))
        (setf (ctx-env ctx) old-env)))))

(defmethod compile-ast ((node ast-apply) ctx)
  (let* ((func-reg (compile-ast (ast-apply-func node) ctx))
         (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx))
                           (ast-apply-args node)))
         (result-reg (make-register ctx)))
    (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-call) ctx)
  "Evaluate FUNC, collect ALL values from each arg form, apply FUNC to them."
  (let* ((func-reg (compile-ast (ast-mv-call-func node) ctx))
         (args (ast-mv-call-args node))
         (result-reg (make-register ctx)))
    (if (null args)
        (let ((empty-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst empty-reg :value nil))
          (emit ctx (make-vm-apply :dst result-reg :func func-reg :args (list empty-reg))))
        (let ((list-regs
               (mapcar (lambda (arg)
                         (emit ctx (make-vm-clear-values))
                         (let ((primary-reg (compile-ast arg ctx)))
                           (emit ctx (make-vm-ensure-values :src primary-reg))
                           (let ((lr (make-register ctx)))
                             (emit ctx (make-vm-values-to-list :dst lr))
                             lr)))
                       args)))
          (let ((combined-reg
                 (reduce (lambda (acc lr)
                           (let ((new-reg (make-register ctx)))
                             (emit ctx (make-vm-append :dst new-reg :src1 acc :src2 lr))
                             new-reg))
                         (cdr list-regs)
                         :initial-value (car list-regs))))
            (emit ctx (make-vm-apply :dst result-reg :func func-reg
                                     :args (list combined-reg))))))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  "Save result of first form, evaluate remaining forms, return saved result."
  (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
        (result-reg (make-register ctx)))
    (emit ctx (make-vm-move :dst result-reg :src first-reg))
    (dolist (form (ast-mv-prog1-forms node))
      (compile-ast form ctx))
    result-reg))

;;; ── Function calls ────────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-call) ctx)
  (let* ((func-expr (ast-call-func node))
         (func-sym (cond ((symbolp func-expr) func-expr)
                         ((typep func-expr 'ast-var) (ast-var-name func-expr))
                         (t nil)))
         (args (ast-call-args node))
         (result-reg (make-register ctx)))
    ;; ── Phase 1: Table-driven builtin dispatch ──────────────────────────
    ;; Look up func-sym in the unified builtin registry.  Handles ~160
    ;; calling conventions via a single hash-table lookup + generic emitter.
    (when func-sym
      (let ((entry (gethash (symbol-name func-sym) *builtin-registry*)))
        (when entry
          (let ((result (emit-registered-builtin entry args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Phase 2: Builtins requiring AST introspection ──────────────────
    ;; Cannot be data-driven: inspect AST node types at compile time
    ;; (quote guards, keyword arg parsing, variadic cons-chains, multi-path).
    (macrolet ((builtin-name-p (sym name-str)
                 `(and ,sym (string= (symbol-name ,sym) ,name-str))))
      ;; ─ Keyword arg parsing ─
      (when (builtin-name-p func-sym "MAKE-HASH-TABLE")
        (let ((test-reg nil))
          (when (>= (length args) 2)
            (let ((kw-arg (first args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :test))
                (let ((test-arg (second args)))
                  (let ((test-sym (cond ((and (typep test-arg 'ast-quote)
                                              (symbolp (ast-quote-value test-arg)))
                                         (ast-quote-value test-arg))
                                        ((and (typep test-arg 'ast-var)
                                              (member (ast-var-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-var-name test-arg))
                                        ((and (typep test-arg 'ast-function)
                                              (symbolp (ast-function-name test-arg))
                                              (member (ast-function-name test-arg)
                                                      '(eq eql equal equalp)))
                                         (ast-function-name test-arg))
                                        (t nil))))
                    (when test-sym
                      (setf test-reg (make-register ctx))
                      (emit ctx (make-vm-const :dst test-reg :value test-sym))))))))
          (emit ctx (make-vm-make-hash-table :dst result-reg :test test-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "GETHASH")
        (let ((key-reg (compile-ast (first args) ctx))
              (table-reg (compile-ast (second args) ctx))
              (default-reg (when (third args) (compile-ast (third args) ctx))))
          (emit ctx (make-vm-gethash :dst result-reg :found-dst nil
                                     :key key-reg :table table-reg :default default-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAPHASH")
        (let* ((fn-reg (compile-ast (first args) ctx))
               (table-reg (compile-ast (second args) ctx))
               (keys-reg (make-register ctx))
               (loop-start (make-label ctx "MAPHASH_START"))
               (loop-end (make-label ctx "MAPHASH_END"))
               (key-reg (make-register ctx))
               (val-reg (make-register ctx)))
          (emit ctx (make-vm-hash-table-keys :dst keys-reg :table table-reg))
          (emit ctx (make-vm-label :name loop-start))
          (emit ctx (make-vm-jump-zero :reg keys-reg :label loop-end))
          (emit ctx (make-vm-car :dst key-reg :src keys-reg))
          (emit ctx (make-vm-gethash :dst val-reg :key key-reg :table table-reg))
          (let ((call-dst (make-register ctx)))
            (emit ctx (make-vm-call :dst call-dst :func fn-reg :args (list key-reg val-reg))))
          (emit ctx (make-vm-cdr :dst keys-reg :src keys-reg))
          (emit ctx (make-vm-jump :label loop-start))
          (emit ctx (make-vm-label :name loop-end))
          (emit ctx (make-vm-const :dst result-reg :value nil))
          (return-from compile-ast result-reg)))
      ;; ─ Simple custom slots ─
      (when (builtin-name-p func-sym "MAKE-ARRAY")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                        :fill-pointer nil :adjustable nil))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAKE-ADJUSTABLE-VECTOR")
        (let ((size-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-array :dst result-reg :size-reg size-reg
                                        :fill-pointer t :adjustable t))
          (return-from compile-ast result-reg)))
      ;; ─ Variadic cons-chain builders ─
      (when (and (builtin-name-p func-sym "ARRAY-ROW-MAJOR-INDEX") (>= (length args) 1))
        (let ((arr-reg (compile-ast (first args) ctx))
              (subs-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst subs-reg :value nil))
          (dolist (sub (reverse (rest args)))
            (let ((sub-reg (compile-ast sub ctx))
                  (new-reg (make-register ctx)))
              (emit ctx (make-vm-cons :dst new-reg :car-src sub-reg :cdr-src subs-reg))
              (setf subs-reg new-reg)))
          (emit ctx (make-vm-array-row-major-index :dst result-reg :arr arr-reg :subs subs-reg))
          (return-from compile-ast result-reg)))
      (when (and (builtin-name-p func-sym "ENCODE-UNIVERSAL-TIME")
                 (>= (length args) 6) (<= (length args) 7))
        (let ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
              (list-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst list-reg :value nil))
          (dolist (r (reverse arg-regs))
            (let ((new-reg (make-register ctx)))
              (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
              (setf list-reg new-reg)))
          (emit ctx (make-vm-encode-universal-time :dst result-reg :args-reg list-reg))
          (return-from compile-ast result-reg)))
      ;; ─ Keyword arg: make-string optional :initial-element ─
      (when (builtin-name-p func-sym "MAKE-STRING")
        (let ((size-reg (compile-ast (first args) ctx)))
          (if (and (= (length args) 3)
                   (typep (second args) 'ast-var)
                   (eq (ast-var-name (second args)) :initial-element))
              (let ((char-reg (compile-ast (third args) ctx)))
                (emit ctx (make-vm-make-string :dst result-reg :src size-reg :char char-reg)))
              (emit ctx (make-vm-make-string :dst result-reg :src size-reg)))
          (return-from compile-ast result-reg)))
      ;; ─ Quote guards ─
      (when (and (builtin-name-p func-sym "TYPEP")
                 (= (length args) 2)
                 (typep (second args) 'ast-quote))
        (let ((val-reg (compile-ast (first args) ctx))
              (type-sym (ast-quote-value (second args))))
          (emit ctx (make-vm-typep :dst result-reg :src val-reg :type-name type-sym))
          (return-from compile-ast result-reg)))
      ;; CLOS slot predicates requiring quoted slot name
      (macrolet ((compile-slot-pred (name inst-maker)
                   `(when (and (builtin-name-p func-sym ,(symbol-name name))
                               (= (length args) 2)
                               (typep (second args) 'ast-quote))
                      (let ((obj-reg (compile-ast (first args) ctx))
                            (slot-sym (ast-quote-value (second args))))
                        (emit ctx (,inst-maker :dst result-reg :obj-reg obj-reg :slot-name-sym slot-sym))
                        (return-from compile-ast result-reg)))))
        (compile-slot-pred slot-boundp make-vm-slot-boundp)
        (compile-slot-pred slot-exists-p make-vm-slot-exists-p)
        (compile-slot-pred slot-makunbound make-vm-slot-makunbound))
      ;; ─ CLOS: call-next-method (variadic args → list) ─
      (when (builtin-name-p func-sym "CALL-NEXT-METHOD")
        (let ((args-reg (if args
                            (let* ((arg-regs (mapcar (lambda (a) (compile-ast a ctx)) args))
                                   (list-reg (make-register ctx)))
                              (emit ctx (make-vm-const :dst list-reg :value nil))
                              (dolist (r (reverse arg-regs))
                                (let ((new-reg (make-register ctx)))
                                  (emit ctx (make-vm-cons :dst new-reg :car-src r :cdr-src list-reg))
                                  (setf list-reg new-reg)))
                              list-reg)
                            nil)))
          (emit ctx (make-vm-call-next-method :dst result-reg :args-reg args-reg))
          (return-from compile-ast result-reg)))
      ;; ─ I/O with optional stream arg ─
      (when (builtin-name-p func-sym "WRITE-STRING")
        (let ((str-reg (compile-ast (first args) ctx)))
          (if (>= (length args) 2)
              (let ((stream-reg (compile-ast (second args) ctx)))
                (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
                (emit ctx (make-vm-move :dst result-reg :src str-reg)))
              (emit ctx (make-vm-princ :src str-reg)))
          (return-from compile-ast result-reg)))
      ;; format: (format nil/t/stream fmt-string args...)
      (when (and (builtin-name-p func-sym "FORMAT") (>= (length args) 2))
        (let* ((dest-arg (first args))
               (fmt-reg (compile-ast (second args) ctx))
               (format-arg-regs (mapcar (lambda (a) (compile-ast a ctx)) (cddr args)))
               (dest-is-nil (or (and (typep dest-arg 'ast-var)
                                     (eq (ast-var-name dest-arg) 'nil))
                                (and (typep dest-arg 'ast-quote)
                                     (null (ast-quote-value dest-arg)))))
               (dest-is-t (or (and (typep dest-arg 'ast-var)
                                   (eq (ast-var-name dest-arg) 't))
                              (and (typep dest-arg 'ast-quote)
                                   (eq (ast-quote-value dest-arg) t)))))
          (cond
            (dest-is-nil
             (emit ctx (make-vm-format-inst :dst result-reg :fmt fmt-reg
                                            :arg-regs format-arg-regs))
             (return-from compile-ast result-reg))
            (dest-is-t
             (let ((str-reg (make-register ctx)))
               (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                              :arg-regs format-arg-regs))
               (emit ctx (make-vm-princ :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg)))
            (t
             (let ((str-reg (make-register ctx))
                   (stream-reg (compile-ast dest-arg ctx)))
               (emit ctx (make-vm-format-inst :dst str-reg :fmt fmt-reg
                                              :arg-regs format-arg-regs))
               (emit ctx (make-vm-stream-write-string-inst :stream-reg stream-reg :src str-reg))
               (emit ctx (make-vm-const :dst result-reg :value nil))
               (return-from compile-ast result-reg))))))
      ;; open: keyword arg :direction
      (when (builtin-name-p func-sym "OPEN")
        (let* ((path-reg (compile-ast (first args) ctx))
               (direction :input))
          (when (>= (length args) 3)
            (let ((kw-arg (second args)))
              (when (and (typep kw-arg 'ast-var)
                         (eq (ast-var-name kw-arg) :direction))
                (let ((dir-arg (third args)))
                  (when (and (typep dir-arg 'ast-var)
                             (keywordp (ast-var-name dir-arg)))
                    (setf direction (ast-var-name dir-arg)))))))
          (emit ctx (make-vm-open-file :dst result-reg :path path-reg :direction direction))
          (return-from compile-ast result-reg)))
      ;; peek-char: (peek-char nil handle) or (peek-char handle)
      (when (builtin-name-p func-sym "PEEK-CHAR")
        (let ((handle-reg (if (>= (length args) 2)
                              (compile-ast (second args) ctx)
                              (compile-ast (first args) ctx))))
          (emit ctx (make-vm-peek-char :dst result-reg :handle handle-reg))
          (return-from compile-ast result-reg)))
      (when (builtin-name-p func-sym "MAKE-STRING-INPUT-STREAM")
        (let ((str-reg (compile-ast (first args) ctx)))
          (emit ctx (make-vm-make-string-stream :dst result-reg :direction :input
                                                :initial-string str-reg))
          (return-from compile-ast result-reg)))
      ;; concatenate: only (concatenate 'string a b)
      (when (and (builtin-name-p func-sym "CONCATENATE")
                 (>= (length args) 3)
                 (typep (first args) 'ast-quote)
                 (string= (symbol-name (ast-quote-value (first args))) "STRING"))
        (let ((str1-reg (compile-ast (second args) ctx))
              (str2-reg (compile-ast (third args) ctx)))
          (emit ctx (make-vm-concatenate :dst result-reg :str1 str1-reg :str2 str2-reg))
          (return-from compile-ast result-reg))))
    ;; ── Normal function call ─────────────────────────────────────────────
    (let* ((raw-func-reg
             (cond ((symbolp func-expr)
                    (let ((entry (assoc func-expr (ctx-env ctx)))
                          (is-global (gethash func-expr (ctx-global-functions ctx))))
                      (if (and entry (not is-global))
                          (cdr entry)
                          (let ((sym-reg (make-register ctx)))
                            (emit ctx (make-vm-const :dst sym-reg :value func-expr))
                            sym-reg))))
                   ((typep func-expr 'ast-var)
                    (let* ((name (ast-var-name func-expr))
                           (entry (assoc name (ctx-env ctx)))
                           (is-global (gethash name (ctx-global-functions ctx))))
                      (if (and entry (not is-global))
                          (cdr entry)
                          (let ((sym-reg (make-register ctx)))
                            (emit ctx (make-vm-const :dst sym-reg :value name))
                            sym-reg))))
                   (t (compile-ast func-expr ctx))))
           ;; Unbox labels functions: extract closure from cons cell box
           (func-reg (if (and func-sym (assoc func-sym *labels-boxed-fns*))
                         (let ((unboxed (make-register ctx)))
                           (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                           unboxed)
                         raw-func-reg))
           (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
      (if (and func-sym (gethash func-sym (ctx-global-generics ctx)))
          (emit ctx (make-vm-generic-call :dst result-reg :gf-reg func-reg :args arg-regs))
          (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs)))
      result-reg)))

;;; ── Function references and local function bindings ──────────────────────

(defmethod compile-ast ((node ast-function) ctx)
  "Compile #'name — look up function by name, returning a closure reference."
  (let* ((name (ast-function-name node))
         (dst (make-register ctx)))
    (cond
      ((symbolp name)
       (let ((entry (assoc name (ctx-env ctx))))
         (if entry
             (if (assoc name *labels-boxed-fns*)
                 (emit ctx (make-vm-car :dst dst :src (cdr entry)))
                 (emit ctx (make-vm-move :dst dst :src (cdr entry))))
             (emit ctx (make-vm-func-ref :dst dst :label (format nil "~A" name))))))
      ((and (consp name) (eq (car name) 'setf))
       (emit ctx (make-vm-func-ref :dst dst :label (format nil "SETF_~A" (second name)))))
      (t
       (error "Invalid function name: ~S" name)))
    dst))

(defmethod compile-ast ((node ast-flet) ctx)
  "Compile flet: non-recursive local function bindings."
  (let* ((bindings (ast-flet-bindings node))
         (body (ast-flet-body node))
         (end-label (make-label ctx "flet_end"))
         (old-env (ctx-env ctx)))
    (let ((body-result-reg
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
                             (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                                        :params param-regs :captured captured-vars))
                             (push (cons name closure-reg) func-bindings)
                             (emit ctx (make-vm-jump :label skip-label))
                             (emit ctx (make-vm-label :name func-label))
                             (let ((param-bindings nil))
                               (loop for param in params
                                     for param-reg in param-regs
                                     do (push (cons param param-reg) param-bindings))
                               (setf (ctx-env ctx) (append (nreverse param-bindings) old-env))
                               (let ((last-reg nil))
                                 (dolist (form body-forms)
                                   (setf last-reg (compile-ast form ctx)))
                                 (emit ctx (make-vm-ret :reg last-reg))))
                             (emit ctx (make-vm-label :name skip-label))))))
                     (setf (ctx-env ctx) (append (nreverse func-bindings) old-env)))
                   (let ((last-reg nil))
                     (dolist (form body)
                       (setf last-reg (compile-ast form ctx)))
                     last-reg))
              (setf (ctx-env ctx) old-env))))
      (emit ctx (make-vm-label :name end-label))
      body-result-reg)))

(defmethod compile-ast ((node ast-labels) ctx)
  "Compile labels: mutually recursive local function bindings via cons-cell boxing."
  (let* ((bindings (ast-labels-bindings node))
         (body (ast-labels-body node))
         (end-label (make-label ctx "labels_end"))
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
                       (setf *labels-boxed-fns*
                             (append (mapcar (lambda (info) (cons (first info) (fourth info)))
                                            func-infos)
                                     old-labels-boxes))
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
                             (let ((param-bindings nil))
                               (loop for param in params
                                     for param-reg in param-regs
                                     do (push (cons param param-reg) param-bindings))
                               (setf (ctx-env ctx) (append (nreverse param-bindings)
                                                           forward-env old-env))
                               (let ((last-reg nil))
                                 (dolist (form body-forms)
                                   (setf last-reg (compile-ast form ctx)))
                                 (emit ctx (make-vm-ret :reg last-reg))))
                             (emit ctx (make-vm-label :name skip-label)))))
                       ;; Phase 3: Compile body with box-resolved env
                       (let ((func-env (mapcar (lambda (binding)
                                                 (cons (first binding)
                                                       (lookup-var ctx (first binding))))
                                               bindings)))
                         (setf (ctx-env ctx) (append func-env old-env)))
                       (let ((last-reg nil))
                         (dolist (form body)
                           (setf last-reg (compile-ast form ctx)))
                         last-reg))))
              (setf (ctx-env ctx) old-env)
              (setf *labels-boxed-fns* old-labels-boxes))))
      (emit ctx (make-vm-label :name end-label))
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
        (setf program (make-vm-program
                       :instructions (regalloc-instructions ra)
                       :result-register (vm-program-result-register program)))))
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
