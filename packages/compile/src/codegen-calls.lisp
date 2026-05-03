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
  (if (consp alist)
      (let ((entry (car alist)))
        (if (eq (car entry) key)
            entry
            (%codegen-call-assoc key (cdr alist))))
      nil))

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

(defun %try-compile-apply (func-sym args result-reg ctx)
  "Handle (apply FN ARG...) — emit vm-apply."
  (when (and func-sym (eq func-sym 'apply) args)
    (let* ((func-arg (first args))
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
      (let ((arg-regs (%compile-call-arg-registers (rest args) ctx)))
        (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs))
        result-reg))))

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
             (emit ctx (make-vm-const :dst result-reg :value (cadr entry)))
             result-reg)))
        ((and (eq func-sym 'aref)
              (= (length args) 2)
              (typep (first args) 'ast-var))
         (let* ((arg-name (ast-var-name (first args)))
                (entry    (%codegen-call-assoc arg-name (ctx-noescape-array-bindings ctx))))
           (when entry
             (if (typep (second args) 'ast-int)
                 (let ((index (ast-int-value (second args))))
                   (when (and (<= 0 index) (< index (cadr entry)))
                     (emit ctx (make-vm-move :dst result-reg
                                             :src (nth index (cddr entry))))))
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
                   (when (and (<= 0 index) (< index (cadr entry)))
                     (let ((val-reg (compile-ast (third args) ctx)))
                       (setf (nth index (cddr entry)) val-reg)
                       (emit ctx (make-vm-move :dst result-reg :src val-reg)))))
                 (%emit-noescape-array-write entry (second args) (third args) result-reg ctx))
             result-reg)))))))

(defun %try-compile-builtin (func-sym args result-reg ctx)
  "Phase-1 (table-driven) + Phase-2 (Prolog-style) builtin dispatch."
  (when func-sym
    (let ((sname (symbol-name func-sym)))
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
       (let ((temps (mapcar (lambda (r)
                              (let ((t-reg (make-register ctx)))
                                (emit ctx (make-vm-move :dst t-reg :src r))
                                t-reg))
                            arg-regs)))
         (dolist (param (ctx-current-function-params ctx))
           (emit ctx (make-vm-move :dst (lookup-var ctx param) :src (pop temps)))))
       (emit ctx (make-vm-jump :label (ctx-current-function-label ctx))))
      ;; Generic function dispatch
      ((and func-sym (gethash func-sym (ctx-global-generics ctx)))
       (emit ctx (make-vm-generic-call :dst result-reg :gf-reg func-reg :args arg-regs)))
      ;; Tail call
      (tail
       (emit ctx (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs)))
       ;; Regular call
       (t
        (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs))))
      result-reg)))

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
    (or (%try-compile-funcall          func-sym  args result-reg tail ctx)
        (%try-compile-apply            func-sym  args result-reg      ctx)
        (%try-compile-noescape-closure func-expr args             tail ctx)
        (%try-compile-noescape-cons    func-sym  args result-reg      ctx)
        (%try-compile-noescape-array   func-sym  args result-reg      ctx)
        (%try-compile-builtin          func-sym  args result-reg      ctx)
        (%compile-normal-call          func-expr func-sym args result-reg tail ctx))))
