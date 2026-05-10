(in-package :cl-cc/compile)
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
  "Compile a class definition."
  (setf (ctx-tail-position ctx) nil)
  (let ((name (ast-defclass-name node))
        (supers (ast-defclass-superclasses node))
        (slots (ast-defclass-slots node))
        (dst (make-register ctx))
        (slot-names nil)
        (initarg-map nil)
        (initform-regs nil)
        (slot-types nil)
        (default-initarg-regs nil)
        (class-slot-names nil)
        (metaclass-reg nil))
    (loop for slot in slots
          collect (ast-slot-name slot)                                          into s-names
          when (ast-slot-initarg slot)
            collect (cons (ast-slot-initarg slot) (ast-slot-name slot))         into i-map
          when (ast-slot-initform slot)
            collect (cons (ast-slot-name slot)
                          (compile-ast (ast-slot-initform slot) ctx))           into i-regs
          when (ast-slot-type slot)
            collect (cons (ast-slot-name slot) (ast-slot-type slot))            into s-types
          when (eq :class (ast-slot-allocation slot))
            collect (ast-slot-name slot)                                        into c-slots
          finally (setq slot-names      s-names
                        initarg-map     i-map
                        initform-regs   i-regs
                        slot-types      s-types
                        class-slot-names c-slots))
    (setq default-initarg-regs
          (loop for entry in (ast-defclass-default-initargs node)
                collect (cons (car entry) (compile-ast (cdr entry) ctx))))
    (when (ast-defclass-metaclass node)
      (setq metaclass-reg (compile-ast (ast-defclass-metaclass node) ctx)))
    (emit ctx (make-vm-class-def
               :dst dst
               :class-name name
               :superclasses supers
               :slot-names slot-names
               :slot-initargs initarg-map
               :slot-initform-regs initform-regs
               :slot-types slot-types
               :default-initarg-regs default-initarg-regs
               :class-slots class-slot-names
               :metaclass-reg metaclass-reg))
    (setf (gethash name (ctx-global-classes ctx)) dst)
    (setf (ctx-env ctx) (cons (cons name dst) (ctx-env ctx)))
    (emit ctx (make-vm-set-global :name name :src dst))
    (setf (gethash name (ctx-global-variables ctx)) dst)
    (loop for slot in slots
          for slot-name = (ast-slot-name slot)
          do (when (ast-slot-reader slot)
               (compile-slot-accessor ctx dst name slot-name
                                      (ast-slot-reader slot) :reader))
             (when (ast-slot-writer slot)
               (compile-slot-accessor ctx dst name slot-name
                                      (ast-slot-writer slot) :writer))
             (when (ast-slot-accessor slot)
               (compile-slot-accessor ctx dst name slot-name
                                      (ast-slot-accessor slot) :reader)
               (setf (gethash (ast-slot-accessor slot) *accessor-slot-map*)
                     (cons name slot-name))))
    dst))

(defun compile-slot-accessor (ctx class-reg class-name slot-name accessor-name kind)
  "Compile a slot reader or writer as a defun-style closure."
  (declare (ignore class-reg class-name))
  (let ((func-label (make-label ctx (format nil "~A_~A" kind accessor-name)))
        (end-label (make-label ctx (format nil "~A_~A_END" kind accessor-name)))
        (closure-reg (make-register ctx)))
    (cond
      ((eq kind :reader)
       (let ((obj-reg (make-register ctx))
             (result-reg (make-register ctx)))
         (emit ctx (make-vm-closure
                    :dst closure-reg
                    :label func-label
                    :params (list obj-reg)
                    :captured nil))
         (setf (ctx-env ctx)
               (cons (cons accessor-name closure-reg) (ctx-env ctx)))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         (emit ctx (make-vm-register-function :name accessor-name
                                              :src closure-reg))
         (emit ctx (make-vm-jump :label end-label))
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-read
                    :dst result-reg
                    :obj-reg obj-reg
                    :slot-name slot-name))
         (emit ctx (make-vm-ret :reg result-reg))
         (emit ctx (make-vm-label :name end-label))))
      ((eq kind :writer)
       (let ((val-reg (make-register ctx))
             (obj-reg (make-register ctx)))
         (emit ctx (make-vm-closure
                    :dst closure-reg
                    :label func-label
                    :params (list val-reg obj-reg)
                    :captured nil))
         (setf (ctx-env ctx)
               (cons (cons accessor-name closure-reg) (ctx-env ctx)))
         (setf (gethash accessor-name (ctx-global-functions ctx)) func-label)
         (emit ctx (make-vm-register-function :name accessor-name
                                              :src closure-reg))
         (emit ctx (make-vm-jump :label end-label))
         (emit ctx (make-vm-label :name func-label))
         (emit ctx (make-vm-slot-write
                    :obj-reg obj-reg
                    :slot-name slot-name
                    :value-reg val-reg))
         (emit ctx (make-vm-ret :reg val-reg))
         (emit ctx (make-vm-label :name end-label)))))
    closure-reg))

;;; Generic function and method compilation (%ensure-generic-function,
;;; compile-ast for ast-defgeneric/defmethod/make-instance/slot-value/
;;; set-slot-value/set-gethash) is in codegen-gf.lisp (loads next).
