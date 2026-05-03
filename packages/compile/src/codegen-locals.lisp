(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Local Function Bindings and Assembly Utilities
;;;
;;; Contains: %compile-body-with-tail, %compile-inline-lambda-call,
;;; %emit-noescape-array-read/write, %compile-closure-body,
;;; compile-ast methods for ast-function/ast-flet/ast-labels,
;;; target-instance, emit-assembly, type-check-ast.
;;;
;;; Entry points (compile-toplevel-forms, ast-call, exception handling,
;;; multiple values) are in codegen.lisp (loads before this file).
;;;
;;; Load order: after codegen.lisp, before codegen-phase2.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Function references and local function bindings ──────────────────────


(defun %codegen-locals-append-env (prefix env)
  (if (consp prefix)
      (cons (car prefix) (%codegen-locals-append-env (cdr prefix) env))
      env))


(defun %codegen-locals-assoc-eq (key alist)
  (if (consp alist)
      (if (and (consp (car alist)) (eq key (caar alist)))
          (car alist)
          (%codegen-locals-assoc-eq key (cdr alist)))
      nil))

(defun %compile-local-arg-bindings (params arg-asts ctx)
  (if (and (consp params) (consp arg-asts))
      (cons (cons (car params) (compile-ast (car arg-asts) ctx))
            (%compile-local-arg-bindings (cdr params) (cdr arg-asts) ctx))
      nil))


(defun %compile-local-param-bindings (params param-regs)
  (if (and (consp params) (consp param-regs))
      (cons (cons (car params) (car param-regs))
            (%compile-local-param-bindings (cdr params) (cdr param-regs)))
      nil))

(defun %compile-body-with-tail (body tail ctx)
  "Compile BODY forms with tail-position tracking. Returns the last result register."
  (labels ((scan (forms last-reg)
             (if (consp forms)
                 (progn
                   (setf (ctx-tail-position ctx)
                         (if (null (cdr forms)) tail nil))
                   (scan (cdr forms) (compile-ast (car forms) ctx)))
                 last-reg)))
    (scan body nil)))

(defun %compile-inline-lambda-call (lambda-node arg-asts tail ctx)
  "Compile a direct call to a non-escaping lambda without emitting vm-closure."
  (let ((old-env (ctx-env ctx))
        (old-tail (ctx-tail-position ctx)))
    (unwind-protect
         (let ((arg-bindings (%compile-local-arg-bindings
                              (ast-lambda-params lambda-node)
                              arg-asts
                              ctx)))
           (setf (ctx-env ctx) (%codegen-locals-append-env arg-bindings old-env))
           (%compile-body-with-tail (ast-lambda-body lambda-node) tail ctx))
      (setf (ctx-env ctx) old-env)
      (setf (ctx-tail-position ctx) old-tail))))

(defun %emit-noescape-dispatch-loop (size index-reg label-prefix end-label hit-fn ctx)
  "Emit a runtime index-dispatch loop for a noescape array.
For each index I in [0, SIZE), tests INDEX-REG == I and calls (HIT-FN I) on match.
Falls through to an oob error when no index matches."
  (let ((error-label (make-label ctx (concatenate 'string label-prefix "_error")))
        (err-reg     (make-register ctx)))
    (dotimes (i size)
      (let ((idx-const  (make-register ctx))
            (eq-reg     (make-register ctx))
            (next-label (make-label ctx (concatenate 'string label-prefix "_next"))))
        (emit ctx (make-vm-const   :dst idx-const :value i))
        (emit ctx (make-vm-num-eq  :dst eq-reg :lhs index-reg :rhs idx-const))
        (emit ctx (make-vm-jump-zero :reg eq-reg :label next-label))
        (funcall hit-fn i)
        (emit ctx (make-vm-jump  :label end-label))
        (emit ctx (make-vm-label :name next-label))))
    (emit ctx (make-vm-jump  :label error-label))
    (emit ctx (make-vm-label :name error-label))
    (emit ctx (make-vm-const :dst err-reg :value "no-escape array index out of bounds"))
    (emit ctx (make-vm-signal-error :error-reg err-reg))))

(defun %emit-noescape-array-read (entry index-ast result-reg ctx)
  (let* ((size      (cadr entry))
         (elements  (cddr entry))
         (index-reg (compile-ast index-ast ctx))
         (end-label (make-label ctx "noescape_array_read_end")))
    (%emit-noescape-dispatch-loop
     size index-reg "noescape_array_read" end-label
     (lambda (i) (emit ctx (make-vm-move :dst result-reg :src (nth i elements))))
     ctx)
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defun %emit-noescape-array-write (entry index-ast value-ast result-reg ctx)
  (let* ((size      (cadr entry))
         (elements  (cddr entry))
         (index-reg (compile-ast index-ast ctx))
         (val-reg   (compile-ast value-ast ctx))
         (end-label (make-label ctx "noescape_array_write_end")))
    (%emit-noescape-dispatch-loop
     size index-reg "noescape_array_write" end-label
     (lambda (i)
       (setf (nth i elements) val-reg)
       (emit ctx (make-vm-move :dst result-reg :src val-reg)))
     ctx)
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defun %compile-closure-body (ctx params param-regs body-forms env)
  "Emit the body of a local function (flet/labels closure).
   Binds PARAMS to PARAM-REGS in CTX-ENV under base ENV, compiles BODY-FORMS
   in tail position, emits vm-ret with the last result register.
   Restores CTX-ENV to ENV on exit."
  (let ((param-bindings (%compile-local-param-bindings params param-regs)))
    (setf (ctx-env ctx) (%codegen-locals-append-env param-bindings env))
    (let ((last-reg (%compile-body-with-tail body-forms t ctx)))
      (setf (ctx-tail-position ctx) nil)
      (emit ctx (make-vm-ret :reg last-reg)))))

(defmethod compile-ast ((node ast-function) ctx)
  "Compile #'name — look up function by name, returning a closure reference."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-function-name node))
         (dst (make-register ctx)))
    (if (symbolp name)
        (let ((entry (%codegen-locals-assoc-eq name (ctx-env ctx))))
          (if entry
              (if (%codegen-locals-assoc-eq name *labels-boxed-fns*)
                  (emit ctx (make-vm-car :dst dst :src (cdr entry)))
                  (emit ctx (make-vm-move :dst dst :src (cdr entry))))
              (if (gethash name (ctx-global-generics ctx))
                  (emit ctx (make-vm-get-global :dst dst :name name))
                  (emit ctx (make-vm-func-ref :dst dst :label (format nil "~A" name))))))
        (if (and (consp name) (eq (car name) 'setf))
            (emit ctx (make-vm-func-ref :dst dst :label (format nil "SETF_~A" (second name))))
            (error "Invalid function name: ~S" name)))
    dst))

(defun %codegen-locals-member-eq (item items)
  (if (consp items)
      (if (eq item (car items))
          t
          (%codegen-locals-member-eq item (cdr items)))
      nil))

(defun %codegen-locals-set-difference-eq (items excluded)
  (if (consp items)
      (if (%codegen-locals-member-eq (car items) excluded)
          (%codegen-locals-set-difference-eq (cdr items) excluded)
          (cons (car items)
                (%codegen-locals-set-difference-eq (cdr items) excluded)))
      nil))

(defun %codegen-locals-captured-vars (vars env ctx)
  (if (consp vars)
      (if (%codegen-locals-assoc-eq (car vars) env)
          (cons (cons (car vars) (lookup-var ctx (car vars)))
                (%codegen-locals-captured-vars (cdr vars) env ctx))
          (%codegen-locals-captured-vars (cdr vars) env ctx))
      nil))

(defun %codegen-locals-param-registers (params ctx)
  (if (consp params)
      (cons (make-register ctx)
            (%codegen-locals-param-registers (cdr params) ctx))
      nil))

(defun %codegen-locals-binding-by-name (name bindings)
  (if (consp bindings)
      (if (and (consp (car bindings)) (eq name (caar bindings)))
          (car bindings)
          (%codegen-locals-binding-by-name name (cdr bindings)))
      nil))

(defun %codegen-locals-info-forward-env (infos)
  (if (consp infos)
      (cons (cons (caar infos) (cadddr (car infos)))
            (%codegen-locals-info-forward-env (cdr infos)))
      nil))

(defun %codegen-locals-bindings-env (bindings ctx)
  (if (consp bindings)
      (let ((name (caar bindings)))
        (cons (cons name (lookup-var ctx name))
              (%codegen-locals-bindings-env (cdr bindings) ctx)))
      nil))

(defun %compile-flet-bindings (bindings old-env ctx)
  (if (consp bindings)
      (let* ((binding (car bindings))
             (name (first binding))
             (params (second binding))
             (body-forms (cddr binding))
             (func-label (make-label ctx "flet_fn"))
             (closure-reg (make-register ctx))
             (free-vars (%codegen-locals-set-difference-eq
                         (free-vars-of-list body-forms)
                         params))
             (captured-vars (%codegen-locals-captured-vars free-vars old-env ctx))
             (param-regs (%codegen-locals-param-registers params ctx))
             (skip-label (make-label ctx "flet_skip")))
        (if captured-vars
            (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                       :params param-regs :captured captured-vars))
            (emit ctx (make-vm-func-ref :dst closure-reg :label func-label)))
        (emit ctx (make-vm-jump :label skip-label))
        (emit ctx (make-vm-label :name func-label))
        (%compile-closure-body ctx params param-regs body-forms old-env)
        (emit ctx (make-vm-label :name skip-label))
        (cons (cons name closure-reg)
              (%compile-flet-bindings (cdr bindings) old-env ctx)))
      nil))

(defmethod compile-ast ((node ast-flet) ctx)
  "Compile flet: non-recursive local function bindings."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-flet-bindings node))
         (body (ast-flet-body node))
         (old-env (ctx-env ctx)))
    (unwind-protect
         (progn
           (let ((func-bindings (%compile-flet-bindings bindings old-env ctx)))
             (setf (ctx-env ctx) (%codegen-locals-append-env func-bindings old-env)))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx) old-env))))

(defun %labels-create-boxes-scan (bindings ctx nil-reg box-id)
  (if (consp bindings)
      (let* ((binding (car bindings))
             (name (first binding))
             (params (second binding))
             (func-label (make-label ctx "labels_fn"))
             (closure-reg (make-register ctx))
             (box-reg (make-register ctx))
             (box-id-reg (make-register ctx)))
        (emit ctx (make-vm-const :dst box-id-reg :value box-id))
        (emit ctx (make-vm-cons :dst box-reg :car-src nil-reg :cdr-src box-id-reg))
        (cons (list name func-label closure-reg box-reg params)
              (%labels-create-boxes-scan (cdr bindings) ctx nil-reg (+ box-id 1))))
      nil))

(defun %labels-create-boxes (bindings ctx)
  "Phase 1 of labels compilation: allocate a cons-cell box for each binding.
Returns (func-infos forward-env) where func-infos is (name label closure-reg box-reg params)."
  (let ((nil-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst nil-reg :value nil))
    (let ((func-infos (%labels-create-boxes-scan bindings ctx nil-reg 0)))
      (values func-infos (%codegen-locals-info-forward-env func-infos)))))

(defun %labels-fill-closure (info bindings ctx forward-env old-env)
  "Phase 2 helper: emit closure creation + body code for one INFO entry."
  (let* ((name (first info))
         (func-label (second info))
         (closure-reg (third info))
         (box-reg (fourth info))
         (params (fifth info))
         (binding-data (%codegen-locals-binding-by-name name bindings))
         (body-forms (cddr binding-data))
         (free-vars (%codegen-locals-set-difference-eq
                     (free-vars-of-list body-forms)
                     params))
         (captured-vars (%codegen-locals-captured-vars free-vars (ctx-env ctx) ctx))
         (param-regs (%codegen-locals-param-registers params ctx))
         (skip-label (make-label ctx "labels_skip")))
    (emit ctx (make-vm-closure :dst closure-reg :label func-label
                               :params param-regs :captured captured-vars))
    (emit ctx (make-vm-rplaca :cons box-reg :val closure-reg))
    (emit ctx (make-vm-jump :label skip-label))
    (emit ctx (make-vm-label :name func-label))
    (%compile-closure-body ctx params param-regs body-forms
                           (%codegen-locals-append-env forward-env old-env))
    (emit ctx (make-vm-label :name skip-label))))

(defun %labels-fill-closures (infos bindings ctx forward-env old-env)
  (if (consp infos)
      (progn
        (%labels-fill-closure (car infos) bindings ctx forward-env old-env)
        (%labels-fill-closures (cdr infos) bindings ctx forward-env old-env))
      nil))

(defmethod compile-ast ((node ast-labels) ctx)
  "Compile labels: mutually recursive local function bindings via cons-cell boxing."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-labels-bindings node))
         (body (ast-labels-body node))
         (old-env (ctx-env ctx))
         (old-labels-boxes *labels-boxed-fns*))
    (unwind-protect
         (multiple-value-bind (func-infos forward-env)
             (%labels-create-boxes bindings ctx)
           (setf *labels-boxed-fns* (%codegen-locals-append-env forward-env old-labels-boxes))
           (setf (ctx-env ctx) (%codegen-locals-append-env forward-env old-env))
           (%labels-fill-closures func-infos bindings ctx forward-env old-env)
           (let ((func-env (%codegen-locals-bindings-env bindings ctx)))
             (setf (ctx-env ctx) (%codegen-locals-append-env func-env old-env)))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx) old-env)
      (setf *labels-boxed-fns* old-labels-boxes))))

;;; ── Assembly and type utilities ───────────────────────────────────────────

(defparameter *target-class-table*
  '((:x86_64  . x86-64-target)
    (:aarch64 . aarch64-target))
  "Maps backend keyword to the target class to instantiate for assembly emission.")

(defun target-instance (target)
  "Return a fresh target object for TARGET, or NIL for :vm."
  (let ((entry (%codegen-locals-assoc-eq target *target-class-table*)))
    (if entry
        (make-instance (cdr entry))
        nil)))

(defun %emit-x86-64-regalloc (program target-object)
  "Run register allocation for an x86-64 target, updating TARGET-OBJECT in place.
Returns the regalloc-rewritten program."
  (let* ((instructions (vm-program-instructions program))
         (ra (allocate-registers instructions (find-target :x86-64))))
    (setf (target-regalloc target-object) ra
          (target-spill-base-reg target-object)
          (if (x86-64-red-zone-spill-p (vm-program-leaf-p program) (regalloc-spill-count ra))
              :rsp
              :rbp))
    (make-vm-program :instructions       (regalloc-instructions ra)
                     :result-register    (vm-program-result-register program)
                     :leaf-p             (vm-program-leaf-p program))))

(defun %emit-assembly-instructions (instructions target-object stream)
  (if (consp instructions)
      (progn
        (emit-instruction target-object (car instructions) stream)
        (%emit-assembly-instructions (cdr instructions) target-object stream))
      nil))

(defun %maybe-emit-x86-64-regalloc (program target-object)
  (if (typep target-object 'x86-64-target)
      (if (vm-program-instructions program)
          (%emit-x86-64-regalloc program target-object)
          program)
      program))

(defun emit-assembly (program &key (target :x86_64))
  (if (eq target :vm)
      ""
      (if (eq target :wasm)
          (compile-to-wasm-wat program)
          (let ((target-object (target-instance target)))
            (setf program (%maybe-emit-x86-64-regalloc program target-object))
            (with-output-to-string (s)
              (format s "; CL-CC bootstrap assembly (~A)~%" target)
              (format s "clcc_entry:~%")
              (%emit-assembly-instructions
               (vm-program-instructions program)
               target-object
               s))))))

(defun type-check-ast (ast &optional (env (type-env-empty)))
  "Run type inference on AST. Returns inferred type or signals type error."
  (reset-type-vars!)
  (multiple-value-bind (type subst)
      (infer ast env)
    (zonk type subst)))
