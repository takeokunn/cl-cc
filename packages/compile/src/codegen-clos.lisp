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
        (default-initarg-regs nil)
        (class-slot-names nil))
    (let ((xs slots))
      (tagbody
       scan-slots
         (if (null xs) (go done-slots))
         (let ((slot (car xs)))
           (setq slot-names (cons (ast-slot-name slot) slot-names))
           (if (ast-slot-initarg slot)
               (setq initarg-map
                     (cons (cons (ast-slot-initarg slot)
                                 (ast-slot-name slot))
                           initarg-map)))
           (if (ast-slot-initform slot)
               (setq initform-regs
                     (cons (cons (ast-slot-name slot)
                                 (compile-ast (ast-slot-initform slot) ctx))
                           initform-regs)))
           (if (eq :class (ast-slot-allocation slot))
               (setq class-slot-names
                     (cons (ast-slot-name slot) class-slot-names))))
         (setq xs (cdr xs))
         (go scan-slots)
       done-slots))
    (let ((xs (ast-defclass-default-initargs node)))
      (tagbody
       scan-defaults
         (if (null xs) (go done-defaults))
         (let ((entry (car xs)))
           (setq default-initarg-regs
                 (cons (cons (car entry) (compile-ast (cdr entry) ctx))
                       default-initarg-regs)))
         (setq xs (cdr xs))
         (go scan-defaults)
       done-defaults))
    (setq slot-names (nreverse slot-names))
    (setq initarg-map (nreverse initarg-map))
    (setq initform-regs (nreverse initform-regs))
    (setq default-initarg-regs (nreverse default-initarg-regs))
    (setq class-slot-names (nreverse class-slot-names))
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
    (setf (ctx-env ctx) (cons (cons name dst) (ctx-env ctx)))
    (emit ctx (make-vm-set-global :name name :src dst))
    (setf (gethash name (ctx-global-variables ctx)) dst)
    (let ((xs slots))
      (tagbody
       scan-accessors
         (if (null xs) (go done-accessors))
         (let* ((slot (car xs))
                (slot-name (ast-slot-name slot)))
           (if (ast-slot-reader slot)
               (compile-slot-accessor ctx dst name slot-name
                                      (ast-slot-reader slot) :reader))
           (if (ast-slot-writer slot)
               (compile-slot-accessor ctx dst name slot-name
                                      (ast-slot-writer slot) :writer))
           (if (ast-slot-accessor slot)
               (progn
                 (compile-slot-accessor ctx dst name slot-name
                                        (ast-slot-accessor slot) :reader)
                 (setf (gethash (ast-slot-accessor slot) *accessor-slot-map*)
                       (cons name slot-name)))))
         (setq xs (cdr xs))
         (go scan-accessors)
       done-accessors))
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

