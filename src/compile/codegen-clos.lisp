(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — CLOS Compilation
;;;
;;; Contains compile-ast methods for all CLOS-related AST nodes:
;;; defclass, defgeneric, defmethod, make-instance, slot-value, set-gethash.
;;;
;;; Also contains compile-slot-accessor which generates reader/writer closures
;;; as a side-effect of defclass compilation.
;;;
;;; Load order: after codegen-core, before codegen-functions.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defmethod compile-ast ((node ast-defclass) ctx)
  "Compile a class definition.
Creates a class descriptor hash table with metadata about slots, superclasses,
and a method dispatch table.  The descriptor is registered globally."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defclass-name node))
         (supers (ast-defclass-superclasses node))
         (slots (ast-defclass-slots node))
         (dst (make-register ctx))
         (slot-names (mapcar #'ast-slot-name slots))
         (initarg-map (loop for slot in slots
                            when (ast-slot-initarg slot)
                              collect (cons (ast-slot-initarg slot)
                                            (ast-slot-name slot))))
         (initform-regs (loop for slot in slots
                              when (ast-slot-initform slot)
                                collect (cons (ast-slot-name slot)
                                              (compile-ast (ast-slot-initform slot) ctx))))
         ;; Compile :default-initargs value forms
         (default-initarg-regs (loop for (key . value-ast) in (ast-defclass-default-initargs node)
                                     collect (cons key (compile-ast value-ast ctx))))
         ;; Collect names of :allocation :class slots
         (class-slot-names (loop for slot in slots
                                 when (eq :class (ast-slot-allocation slot))
                                   collect (ast-slot-name slot))))
    (emit ctx (make-vm-class-def
               :dst dst
               :class-name name
               :superclasses supers
               :slot-names slot-names
               :slot-initargs initarg-map
               :slot-initform-regs initform-regs
               :default-initarg-regs default-initarg-regs
               :class-slots class-slot-names))
    (setf (gethash name (ctx-global-classes ctx)) dst)
    (push (cons name dst) (ctx-env ctx))
    (emit ctx (make-vm-set-global :name name :src dst))
    (setf (gethash name (ctx-global-variables ctx)) dst)
    ;; Compile reader/writer/accessor methods as closures
    (dolist (slot slots)
      (let ((slot-name (ast-slot-name slot)))
        (when (ast-slot-reader slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-reader slot) :reader))
        (when (ast-slot-writer slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-writer slot) :writer))
        (when (ast-slot-accessor slot)
          (compile-slot-accessor ctx dst name slot-name (ast-slot-accessor slot) :reader)
          (setf (gethash (ast-slot-accessor slot) *accessor-slot-map*)
                (cons name slot-name)))))
    dst))

(defun compile-slot-accessor (ctx class-reg class-name slot-name accessor-name kind)
  "Compile a slot reader or writer as a defun-style closure."
  (let* ((func-label (make-label ctx (format nil "~A_~A" kind accessor-name)))
         (end-label (make-label ctx (format nil "~A_~A_END" kind accessor-name)))
         (closure-reg (make-register ctx)))
    (ecase kind
      (:reader
       (let ((obj-reg (make-register ctx))
             (result-reg (make-register ctx)))
         (emit ctx (make-vm-closure
                    :dst closure-reg
                    :label func-label
                    :params (list obj-reg)
                    :captured nil))
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         (emit ctx (make-vm-register-function :name accessor-name :src closure-reg))
         (emit ctx (make-vm-jump :label end-label))
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-read
                    :dst result-reg
                    :obj-reg obj-reg
                    :slot-name slot-name))
         (emit ctx (make-vm-ret :reg result-reg))
         (emit ctx (make-vm-label :name end-label))))
      (:writer
       (let ((val-reg (make-register ctx))
             (obj-reg (make-register ctx)))
         (emit ctx (make-vm-closure
                    :dst closure-reg
                    :label func-label
                    :params (list val-reg obj-reg)
                    :captured nil))
         (push (cons accessor-name closure-reg) (ctx-env ctx))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         (emit ctx (make-vm-register-function :name accessor-name :src closure-reg))
         (emit ctx (make-vm-jump :label end-label))
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-write
                    :obj-reg obj-reg
                    :slot-name slot-name
                    :value-reg val-reg))
         (emit ctx (make-vm-ret :reg val-reg))
         (emit ctx (make-vm-label :name end-label)))))
    closure-reg))

;;; ── Generic functions ────────────────────────────────────────────────────

(defun %ensure-generic-function (ctx name)
  "Ensure generic function NAME exists (at compile time or runtime).
Returns the register holding the GF dispatch table."
  (let ((dst (make-register ctx))
        (sym-reg (make-register ctx))
        (check-reg (make-register ctx))
        (skip-label (gensym "GF-EXISTS-"))
        (end-label (gensym "GF-END-")))
    (emit ctx (make-vm-const :dst sym-reg :value name))
    (emit ctx (make-vm-boundp :dst check-reg :src sym-reg))
    (emit ctx (make-vm-not :dst check-reg :src check-reg))
    (emit ctx (make-vm-jump-zero :reg check-reg :label skip-label))
    ;; Not found — create new generic function dispatch table
    (emit ctx (make-vm-class-def
               :dst dst :class-name name :superclasses nil
               :slot-names nil :slot-initargs nil))
    (emit ctx (make-vm-set-global :name name :src dst))
    (emit ctx (make-vm-jump :label end-label))
    ;; Found — load existing
    (emit ctx (make-vm-label :name skip-label))
    (emit ctx (make-vm-get-global :dst dst :name name))
    (emit ctx (make-vm-label :name end-label))
    (setf (gethash name (ctx-global-generics ctx)) dst)
    dst))

(defmethod compile-ast ((node ast-defgeneric) ctx)
  "Compile a generic function definition.
Creates a dispatch table (hash table) that maps class names to method closures.
Idempotent: if already defined at compile-time or runtime, reuse existing.
If :method-combination is specified, stores it on the GF as :__method-combination__."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defgeneric-name node))
         (combination (ast-defgeneric-combination node))
         (existing (gethash name (ctx-global-generics ctx))))
    (if existing
        existing
        (let ((dst (%ensure-generic-function ctx name)))
          ;; Store method combination on the GF if specified
          (when combination
            (let ((combo-reg (make-register ctx))
                  (key-reg (make-register ctx)))
              (emit ctx (make-vm-const :dst key-reg :value :__method-combination__))
              (emit ctx (make-vm-const :dst combo-reg :value combination))
              (emit ctx (make-vm-sethash :key key-reg :table dst :value combo-reg))))
          (push (cons name dst) (ctx-env ctx))
          dst))))

(defmethod compile-ast ((node ast-defmethod) ctx)
  "Compile a method definition.
Compiles the method body as a closure and registers it on the generic function's
dispatch table, keyed by the composite specializer list for multiple dispatch."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defmethod-name node))
         (qualifier (ast-defmethod-qualifier node))
         (specializers (ast-defmethod-specializers node))
         (params (ast-defmethod-params node))
         (body (ast-defmethod-body node))
         ;; Look up or auto-create the generic function
         (gf-reg (or (gethash name (ctx-global-generics ctx))
                     (cdr (assoc name (ctx-env ctx)))
                     (%ensure-generic-function ctx name)))
         ;; Build composite dispatch key from ALL specializers
         ;; For eql specializers: (param . (eql 'val)) → (eql val) with quote unwrapped
         (dispatch-key (mapcar (lambda (spec)
                                 (if (and spec (consp spec))
                                     (let ((raw (cdr spec)))
                                       (if (and (consp raw) (eq (car raw) 'eql))
                                           (let ((val (second raw)))
                                             (list 'eql (if (and (consp val) (eq (car val) 'quote))
                                                            (second val)
                                                            val)))
                                           raw))
                                     (or spec t)))
                               specializers))
         (qual-str (if qualifier (format nil "_~A" qualifier) ""))
         (label-suffix (format nil "~{~A~^_~}" dispatch-key))
         (func-label (make-label ctx (format nil "METHOD_~A~A_~A" name qual-str label-suffix)))
         (end-label (make-label ctx (format nil "METHOD_~A~A_~A_END" name qual-str label-suffix)))
         (closure-reg (make-register ctx))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx))))
    (emit ctx (make-vm-closure
               :dst closure-reg :label func-label
               :params param-regs :captured nil))
    (emit ctx (make-vm-register-method
               :gf-reg gf-reg :specializer dispatch-key
               :qualifier qualifier :method-reg closure-reg))
    (emit ctx (make-vm-jump :label end-label))
    (emit ctx (make-vm-label :name func-label))
    (let ((old-env (ctx-env ctx)))
      (unwind-protect
           (progn
             (setf (ctx-env ctx)
                   (append (nreverse (loop for param in params
                                           for reg in param-regs
                                           collect (cons param reg)))
                           (ctx-env ctx)))
             (let ((last-reg nil))
               (dolist (form body)
                 (setf (ctx-tail-position ctx)
                       (if (eq form (car (last body))) t nil))
                 (setf last-reg (compile-ast form ctx)))
               (setf (ctx-tail-position ctx) nil)
               (emit ctx (make-vm-ret :reg last-reg))))
        (setf (ctx-env ctx) old-env)))
    (emit ctx (make-vm-label :name end-label))
    closure-reg))

;;; ── Object instantiation and slot access ─────────────────────────────────

(defmethod compile-ast ((node ast-make-instance) ctx)
  "Compile make-instance.
Handles both dynamic class names (ast-var) and static names (ast-quote/symbol)."
  (setf (ctx-tail-position ctx) nil)
  (let* ((class-ast (ast-make-instance-class node))
         (initargs (ast-make-instance-initargs node))
         (dst (make-register ctx)))
    (if (typep class-ast 'ast-var)
        (let* ((class-reg (compile-ast class-ast ctx))
               (initarg-regs (loop for (key . value-ast) in initargs
                                   collect (cons key (compile-ast value-ast ctx)))))
          (emit ctx (make-vm-make-obj :dst dst :class-reg class-reg :initarg-regs initarg-regs))
          dst)
        (let* ((class-name (etypecase class-ast
                             (ast-quote (ast-quote-value class-ast))
                             (symbol class-ast)))
               (class-reg (let ((r (make-register ctx)))
                            (emit ctx (make-vm-get-global :dst r :name class-name))
                            r))
               (initarg-regs (loop for (key . value-ast) in initargs
                                   collect (cons key (compile-ast value-ast ctx)))))
          (emit ctx (make-vm-make-obj :dst dst :class-reg class-reg :initarg-regs initarg-regs))
          dst))))

(defmethod compile-ast ((node ast-slot-value) ctx)
  "Compile slot-value access."
  (setf (ctx-tail-position ctx) nil)
  (let* ((obj-ast (ast-slot-value-object node))
         (slot-name (ast-slot-value-slot node))
         (dst (make-register ctx)))
    (when (typep obj-ast 'ast-var)
      (let* ((entry (assoc (ast-var-name obj-ast) (ctx-noescape-instance-bindings ctx)))
             (slot-entry (and entry
                              (assoc (symbol-name slot-name) (cdr entry) :test #'string=))))
        (when slot-entry
          (emit ctx (make-vm-move :dst dst :src (cdr slot-entry)))
          (return-from compile-ast dst))))
    (let ((obj-reg (compile-ast obj-ast ctx)))
      (emit ctx (make-vm-slot-read
                 :dst dst :obj-reg obj-reg :slot-name slot-name))
      dst)))

(defmethod compile-ast ((node ast-set-slot-value) ctx)
  "Compile (setf (slot-value obj 'slot) value)."
  (setf (ctx-tail-position ctx) nil)
  (let* ((obj-ast (ast-set-slot-value-object node))
         (slot-name (ast-set-slot-value-slot node))
         (val-reg (compile-ast (ast-set-slot-value-value node) ctx)))
    (when (typep obj-ast 'ast-var)
      (let* ((entry (assoc (ast-var-name obj-ast) (ctx-noescape-instance-bindings ctx)))
             (slot-entry (and entry
                              (assoc (symbol-name slot-name) (cdr entry) :test #'string=))))
        (when slot-entry
          (setf (cdr slot-entry) val-reg)
          (return-from compile-ast val-reg))))
    (let ((obj-reg (compile-ast obj-ast ctx)))
      (emit ctx (make-vm-slot-write
                 :obj-reg obj-reg
                 :slot-name slot-name
                 :value-reg val-reg))
      val-reg)))

(defmethod compile-ast ((node ast-set-gethash) ctx)
  "Compile (setf (gethash key table) value)."
  (setf (ctx-tail-position ctx) nil)
  (let* ((key-reg (compile-ast (ast-set-gethash-key node) ctx))
         (table-reg (compile-ast (ast-set-gethash-table node) ctx))
         (val-reg (compile-ast (ast-set-gethash-value node) ctx)))
    (emit ctx (make-vm-sethash :key key-reg :table table-reg :value val-reg))
    val-reg))
