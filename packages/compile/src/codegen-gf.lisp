(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Generic Function and Method Compilation
;;;
;;; Extracted from codegen-clos.lisp.
;;; Contains compile-ast methods for:
;;;   - ast-defgeneric   — create GF dispatch table (idempotent)
;;;   - ast-defmethod    — compile method body + register on GF by specializer
;;;   - ast-make-instance  — instantiate CLOS objects (dynamic or static class)
;;;   - ast-slot-value     — compile slot-value reads (with noescape opt)
;;;   - ast-set-slot-value — compile (setf (slot-value ...)) writes
;;;   - ast-set-gethash    — compile (setf (gethash ...))
;;;
;;; Also contains the %ensure-generic-function helper used by defgeneric
;;; and defmethod.
;;;
;;; The defclass and compile-slot-accessor are in codegen-clos.lisp (loads before).
;;; Load order: after codegen-clos.lisp, before codegen-functions.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
If :method-combination is specified, stores it on the GF as :__method-combination__.
ANSI CL 7.6.4: signals a warning if the lambda list is not congruent with an
existing generic function in the same compilation unit."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defgeneric-name node))
         (combination (ast-defgeneric-combination node))
         (params (ast-defgeneric-params node))
         (existing (gethash name (ctx-global-generics ctx)))
         (existing-params (gethash name (ctx-global-generic-params ctx))))
    ;; ANSI CL 7.6.4 lambda-list congruence check
    (when (and existing existing-params
               (not (equal (length params) (length existing-params))))
      (warn "DEFGENERIC ~A: lambda list ~A has ~A required parameter(s) but existing definition has ~A"
            name params (length params) (length existing-params)))
    (if existing
        existing
        (let ((dst (%ensure-generic-function ctx name)))
          ;; Store params for future congruence checking
          (setf (gethash name (ctx-global-generic-params ctx)) params)
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
  "Compile a method definition."
  (setf (ctx-tail-position ctx) nil)
  (let ((name (ast-defmethod-name node))
        (qualifier (ast-defmethod-qualifier node))
        (specializers (ast-defmethod-specializers node))
        (params (ast-defmethod-params node))
        (body (ast-defmethod-body node))
        (gf-reg (make-register ctx))
        (dispatch-key nil)
        (qual-str nil)
        (label-suffix nil)
        (func-label nil)
        (end-label nil)
        (closure-reg (make-register ctx))
        (param-regs nil))
    (or (gethash name (ctx-global-generics ctx))
        (cdr (%assoc-eq name (ctx-env ctx)))
        (%ensure-generic-function ctx name))
    (let ((keys nil)
          (xs specializers))
      (tagbody
       scan-specializers
         (if (null xs) (go done-specializers))
         (let* ((spec (car xs))
                (key (if (and spec (consp spec))
                         (let ((raw (cdr spec)))
                           (if (and (consp raw) (eq (car raw) (quote eql)))
                               (let ((val (second raw)))
                                 (list (quote eql)
                                       (if (and (consp val)
                                                (eq (car val) (quote quote)))
                                           (second val)
                                           val)))
                               raw))
                         (or spec t))))
           (setq keys (cons key keys)))
         (setq xs (cdr xs))
         (go scan-specializers)
       done-specializers)
      (setq keys (nreverse keys))
      (setq dispatch-key (if (= (length keys) 1) (first keys) keys)))
    (setq qual-str (if qualifier (format nil "_~A" qualifier) ""))
    (setq label-suffix
          (format nil "~{~A~^_~}"
                  (if (listp dispatch-key) dispatch-key (list dispatch-key))))
    (setq func-label
          (make-label ctx
                      (format nil "METHOD_~A~A_~A" name qual-str
                              label-suffix)))
    (setq end-label
          (make-label ctx
                      (format nil "METHOD_~A~A_~A_END" name qual-str
                              label-suffix)))
    (let ((xs params))
      (tagbody
       scan-param-regs
         (if (null xs) (go done-param-regs))
         (setq param-regs (cons (make-register ctx) param-regs))
         (setq xs (cdr xs))
         (go scan-param-regs)
       done-param-regs))
    (setq param-regs (nreverse param-regs))
    (emit ctx (make-vm-get-global :dst gf-reg :name name))
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
             (let ((pairs nil)
                   (ps params)
                   (rs param-regs))
               (tagbody
                scan-env
                  (if (or (null ps) (null rs)) (go done-env))
                  (setq pairs (cons (cons (car ps) (car rs)) pairs))
                  (setq ps (cdr ps))
                  (setq rs (cdr rs))
                  (go scan-env)
                done-env)
               (setf (ctx-env ctx)
                     (append (nreverse pairs) (ctx-env ctx))))
             (let ((last-reg nil)
                   (xs body))
               (tagbody
                scan-body
                  (if (null xs) (go done-body))
                  (let ((form (car xs)))
                    (setf (ctx-tail-position ctx)
                          (if (null (cdr xs)) t nil))
                    (setq last-reg (compile-ast form ctx)))
                  (setq xs (cdr xs))
                  (go scan-body)
                done-body)
               (setf (ctx-tail-position ctx) nil)
               (emit ctx (make-vm-ret :reg last-reg))))
        (setf (ctx-env ctx) old-env)))
    (emit ctx (make-vm-label :name end-label))
    closure-reg))

;;; ── Object instantiation and slot access ─────────────────────────────────

(defmethod compile-ast ((node ast-make-instance) ctx)
  "Compile make-instance."
  (setf (ctx-tail-position ctx) nil)
  (let ((class-ast (ast-make-instance-class node))
        (initargs (ast-make-instance-initargs node))
        (dst (make-register ctx))
        (initarg-regs nil))
    (let ((xs initargs))
      (tagbody
       scan-initargs
         (if (null xs) (go done-initargs))
         (let ((entry (car xs)))
           (setq initarg-regs
                 (cons (cons (car entry) (compile-ast (cdr entry) ctx))
                       initarg-regs)))
         (setq xs (cdr xs))
         (go scan-initargs)
       done-initargs))
    (setq initarg-regs (nreverse initarg-regs))
    (if (typep class-ast (quote ast-var))
        (let ((class-reg (compile-ast class-ast ctx)))
          (emit ctx (make-vm-make-obj :dst dst :class-reg class-reg
                                      :initarg-regs initarg-regs))
          dst)
        (let* ((class-name (cond
                             ((typep class-ast (quote ast-quote))
                              (ast-quote-value class-ast))
                             ((symbolp class-ast) class-ast)))
               (class-reg (let ((r (make-register ctx)))
                            (emit ctx (make-vm-get-global :dst r
                                                          :name class-name))
                            r)))
          (emit ctx (make-vm-make-obj :dst dst :class-reg class-reg
                                      :initarg-regs initarg-regs))
          dst))))

(defmethod compile-ast ((node ast-slot-value) ctx)
  "Compile slot-value access."
  (setf (ctx-tail-position ctx) nil)
  (let ((obj-ast (ast-slot-value-object node))
        (slot-name (ast-slot-value-slot node))
        (dst (make-register ctx)))
    (if (typep obj-ast (quote ast-var))
        (let* ((entry (%assoc-eq (ast-var-name obj-ast)
                                 (ctx-noescape-instance-bindings ctx)))
               (slot-entry nil))
          (if entry
              (let ((xs (cdr entry)))
                (tagbody
                 scan-slots
                   (if (null xs) (go done-slots))
                   (if (equal (symbol-name slot-name) (car (car xs)))
                       (progn
                         (setq slot-entry (car xs))
                         (go done-slots)))
                   (setq xs (cdr xs))
                   (go scan-slots)
                 done-slots)))
          (if slot-entry
              (progn
                (emit ctx (make-vm-move :dst dst :src (cdr slot-entry)))
                (return-from compile-ast dst)))))
    (let ((obj-reg (compile-ast obj-ast ctx)))
      (emit ctx (make-vm-slot-read
                 :dst dst :obj-reg obj-reg :slot-name slot-name))
      dst)))

(defmethod compile-ast ((node ast-set-slot-value) ctx)
  "Compile (setf (slot-value obj 'slot) value)."
  (setf (ctx-tail-position ctx) nil)
  (let ((obj-ast (ast-set-slot-value-object node))
        (slot-name (ast-set-slot-value-slot node))
        (val-reg (compile-ast (ast-set-slot-value-value node) ctx)))
    (if (typep obj-ast (quote ast-var))
        (let* ((entry (%assoc-eq (ast-var-name obj-ast)
                                 (ctx-noescape-instance-bindings ctx)))
               (slot-entry nil))
          (if entry
              (let ((xs (cdr entry)))
                (tagbody
                 scan-slots
                   (if (null xs) (go done-slots))
                   (if (equal (symbol-name slot-name) (car (car xs)))
                       (progn
                         (setq slot-entry (car xs))
                         (go done-slots)))
                   (setq xs (cdr xs))
                   (go scan-slots)
                 done-slots)))
          (if slot-entry
              (progn
                (setf (cdr slot-entry) val-reg)
                (return-from compile-ast val-reg)))))
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
