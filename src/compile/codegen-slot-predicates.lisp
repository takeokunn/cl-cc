(in-package :cl-cc)

;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — CLOS slot predicates
;;;
;;; Very small cohesive cluster split from codegen-phase2.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;; CLOS slot predicates: (pred obj 'slot-name) with quoted slot name
(dolist (entry '(("SLOT-BOUNDP"    . make-vm-slot-boundp)
                 ("SLOT-EXISTS-P"  . make-vm-slot-exists-p)
                 ("SLOT-MAKUNBOUND". make-vm-slot-makunbound)))
  (let ((name-str (car entry)) (inst-ctor (cdr entry)))
    (setf (gethash name-str *phase2-builtin-handlers*)
          (lambda (args result-reg ctx)
            (when (and (= (length args) 2) (typep (second args) 'ast-quote))
              (let ((obj-reg  (compile-ast (first args) ctx))
                    (slot-sym (ast-quote-value (second args))))
                (emit ctx (funcall (symbol-function inst-ctor)
                                   :dst result-reg :obj-reg obj-reg :slot-name-sym slot-sym))
                result-reg))))))
