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
  (let ((old-env  (ctx-env ctx))
        (old-tail (ctx-tail-position ctx)))
    (unwind-protect
         (let ((arg-bindings (mapcar (lambda (p a) (cons p (compile-ast a ctx)))
                                     (ast-lambda-params lambda-node)
                                     arg-asts)))
           (setf (ctx-env ctx) (append arg-bindings old-env))
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
  (setf (ctx-env ctx) (append (mapcar #'cons params param-regs) env))
  (let ((last-reg (%compile-body-with-tail body-forms t ctx)))
    (setf (ctx-tail-position ctx) nil)
    (emit ctx (make-vm-ret :reg last-reg))))

(defmethod compile-ast ((node ast-function) ctx)
  "Compile #'name — look up function by name, returning a closure reference."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-function-name node))
         (dst  (make-register ctx)))
    (if (symbolp name)
        (let ((entry (assoc name (ctx-env ctx) :test #'eq)))
          (if entry
              (if (assoc name *labels-boxed-fns* :test #'eq)
                  (emit ctx (make-vm-car  :dst dst :src (cdr entry)))
                  (emit ctx (make-vm-move :dst dst :src (cdr entry))))
              (if (gethash name (ctx-global-generics ctx))
                  (emit ctx (make-vm-get-global :dst dst :name name))
                  (emit ctx (make-vm-func-ref   :dst dst :label (format nil "~A" name))))))
        (if (and (consp name) (eq (car name) 'setf))
            (emit ctx (make-vm-func-ref :dst dst :label (format nil "SETF_~A" (second name))))
            (error "Invalid function name: ~S" name)))
    dst))

;;; ── flet ─────────────────────────────────────────────────────────────────

(defun %compile-flet-binding (binding old-env ctx)
  "Compile one flet binding and return (name . closure-reg)."
  (let* ((name        (first binding))
         (params      (second binding))
         (body-forms  (cddr binding))
         (func-label  (make-label ctx "flet_fn"))
         (closure-reg (make-register ctx))
         (free-vars   (remove-if (lambda (v) (member v params :test #'eq))
                                 (free-vars-of-list body-forms)))
         (captured    (loop for var in free-vars
                            when (assoc var old-env :test #'eq)
                              collect (cons var (lookup-var ctx var))))
         (param-regs  (loop repeat (length params) collect (make-register ctx)))
         (skip-label  (make-label ctx "flet_skip")))
    (if captured
        (emit ctx (make-vm-closure  :dst closure-reg :label func-label
                                    :params param-regs :captured captured))
        (emit ctx (make-vm-func-ref :dst closure-reg :label func-label)))
    (emit ctx (make-vm-jump  :label skip-label))
    (emit ctx (make-vm-label :name  func-label))
    (%compile-closure-body ctx params param-regs body-forms old-env)
    (emit ctx (make-vm-label :name  skip-label))
    (cons name closure-reg)))

(defmethod compile-ast ((node ast-flet) ctx)
  "Compile flet: non-recursive local function bindings."
  (let* ((tail     (ctx-tail-position ctx))
         (bindings (ast-flet-bindings node))
         (body     (ast-flet-body node))
         (old-env  (ctx-env ctx)))
    (unwind-protect
         (progn
           (setf (ctx-env ctx)
                 (append (mapcar (lambda (b) (%compile-flet-binding b old-env ctx)) bindings)
                         old-env))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx) old-env))))

;;; ── labels ───────────────────────────────────────────────────────────────

(defun %labels-create-boxes (bindings ctx)
  "Phase 1 of labels compilation: allocate a cons-cell box per binding.
Returns (func-infos forward-env) where each info is (name label closure-reg box-reg params)."
  (let ((nil-reg (make-register ctx)))
    (emit ctx (make-vm-const :dst nil-reg :value nil))
    (let ((func-infos
           (loop for binding in bindings
                 for box-id from 0
                 for name       = (first binding)
                 for params     = (second binding)
                 for func-label = (make-label ctx "labels_fn")
                 for closure-reg = (make-register ctx)
                 for box-reg    = (make-register ctx)
                 for box-id-reg = (make-register ctx)
                 do (emit ctx (make-vm-const :dst box-id-reg :value box-id))
                    (emit ctx (make-vm-cons  :dst box-reg :car-src nil-reg :cdr-src box-id-reg))
                 collect (list name func-label closure-reg box-reg params))))
      (values func-infos
              (mapcar (lambda (info) (cons (first info) (fourth info))) func-infos)))))

(defun %labels-fill-closure (info bindings ctx forward-env old-env)
  "Phase 2: emit closure creation + body code for one INFO entry."
  (let* ((name        (first info))
         (func-label  (second info))
         (closure-reg (third info))
         (box-reg     (fourth info))
         (params      (fifth info))
         (binding-data (assoc name bindings :key #'first :test #'eq))
         (body-forms  (cddr binding-data))
         (free-vars   (remove-if (lambda (v) (member v params :test #'eq))
                                 (free-vars-of-list body-forms)))
         (captured    (loop for var in free-vars
                            when (assoc var (ctx-env ctx) :test #'eq)
                              collect (cons var (lookup-var ctx var))))
         (param-regs  (loop repeat (length params) collect (make-register ctx)))
         (skip-label  (make-label ctx "labels_skip")))
    (emit ctx (make-vm-closure :dst closure-reg :label func-label
                               :params param-regs :captured captured))
    (emit ctx (make-vm-rplaca :cons box-reg :val closure-reg))
    (emit ctx (make-vm-jump   :label skip-label))
    (emit ctx (make-vm-label  :name  func-label))
    (%compile-closure-body ctx params param-regs body-forms (append forward-env old-env))
    (emit ctx (make-vm-label  :name  skip-label))))

(defmethod compile-ast ((node ast-labels) ctx)
  "Compile labels: mutually recursive local function bindings via cons-cell boxing."
  (let* ((tail             (ctx-tail-position ctx))
         (bindings         (ast-labels-bindings node))
         (body             (ast-labels-body node))
         (old-env          (ctx-env ctx))
         (old-labels-boxes *labels-boxed-fns*))
    (unwind-protect
         (multiple-value-bind (func-infos forward-env)
             (%labels-create-boxes bindings ctx)
           (setf *labels-boxed-fns* (append forward-env old-labels-boxes))
           (setf (ctx-env ctx)      (append forward-env old-env))
           (dolist (info func-infos)
             (%labels-fill-closure info bindings ctx forward-env old-env))
           (setf (ctx-env ctx)
                 (append (mapcar (lambda (b)
                                   (let ((n (first b)))
                                     (cons n (lookup-var ctx n))))
                                 bindings)
                         old-env))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx)      old-env)
      (setf *labels-boxed-fns* old-labels-boxes))))

;;; ── Assembly and type utilities ───────────────────────────────────────────

(defparameter *target-class-table*
  '((:x86_64  . x86-64-target)
    (:aarch64 . aarch64-target))
  "Maps backend keyword to the target class to instantiate for assembly emission.")

(defun target-instance (target)
  "Return a fresh target object for TARGET, or NIL for :vm."
  (let ((entry (assoc target *target-class-table*)))
    (when entry (make-instance (cdr entry)))))

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

(defun %maybe-emit-x86-64-regalloc (program target-object)
  (if (and (typep target-object 'x86-64-target)
           (vm-program-instructions program))
      (%emit-x86-64-regalloc program target-object)
      program))

(defun emit-assembly (program &key (target :x86_64))
  (cond
    ((eq target :vm)   "")
    ((eq target :wasm) (compile-to-wasm-wat program))
    (t
     (let ((target-object (target-instance target)))
       (setf program (%maybe-emit-x86-64-regalloc program target-object))
       (with-output-to-string (s)
         (format s "; CL-CC bootstrap assembly (~A)~%" target)
         (format s "clcc_entry:~%")
         (dolist (inst (vm-program-instructions program))
           (emit-instruction target-object inst s)))))))

(defun type-check-ast (ast &optional (env (type-env-empty)))
  "Run type inference on AST. Returns inferred type or signals type error."
  (reset-type-vars!)
  (multiple-value-bind (type subst)
      (infer ast env)
    (zonk type subst)))
