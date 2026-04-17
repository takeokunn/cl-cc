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
                      (loop for binding in bindings
                            for box-id from 0
                            do
                        (let* ((name (first binding))
                               (params (second binding))
                               (func-label (make-label ctx "labels_fn"))
                               (closure-reg (make-register ctx))
                               (box-reg (make-register ctx))
                               (box-id-reg (make-register ctx)))
                          (emit ctx (make-vm-const :dst box-id-reg :value box-id))
                          (emit ctx (make-vm-cons :dst box-reg
                                                  :car-src nil-reg :cdr-src box-id-reg))
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
              (if (x86-64-red-zone-spill-p (vm-program-leaf-p program)
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
