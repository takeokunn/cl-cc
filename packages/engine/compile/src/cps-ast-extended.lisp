(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CPS — OOP / Mutation AST Node Transforms
;;;
;;; Contains cps-transform-ast methods for imperative/OOP forms:
;;;   setq, defvar/defparameter, handler-case, make-instance,
;;;   slot-value, set-slot-value, defclass, defgeneric, defmethod,
;;;   set-gethash.
;;;
;;; Functional forms (quote, the, values, mvb, apply, mvc, mvprog1, ast-call)
;;; and entry points (cps-transform-ast*, cps-transform*, maybe-cps-transform,
;;; cps-transform-eval) are in cps-ast-functional.lisp (loads after this file).
;;;
;;; Core structural CPS transforms (ast-int through ast-labels) are in
;;; cps-ast.lisp (loads before this file).
;;;
;;; Load order: after cps-ast.lisp, before cps-ast-functional.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Mutation Forms ──────────────────────────────────────────────────────────

(defun %cps-lambda (vars &rest body)
  (cons 'lambda (cons vars body)))

(defun %cps-funcall (fn &rest args)
  (cons 'funcall (cons fn args)))

(defun %cps-progn (&rest forms)
  (cons 'progn forms))

(defmethod cps-transform-ast ((node ast-setq) k)
  "Transform setq assignment."
  (let* ((var (ast-setq-var node))
         (value (ast-setq-value node))
         (v (gensym "SETQ")))
    (cps-transform-ast value
                       (%cps-lambda (list v)
                                    (%cps-progn
                                     (list 'setq var v)
                                     (%cps-funcall k v))))))

(defmethod cps-transform-ast ((node ast-defvar) k)
  "Transform defvar/defparameter conservatively through host special forms."
  (let ((name (ast-defvar-name node))
        (kind (ast-defvar-kind node))
        (value (ast-defvar-value node))
        (v (gensym "DEFVAR")))
    (if value
        (cps-transform-ast value
                           (%cps-lambda (list v)
                                        (%cps-progn
                                         (list kind name v)
                                         (%cps-funcall k name))))
        (%cps-progn
         (list kind name)
         (%cps-funcall k name)))))

(defmethod cps-transform-ast ((node ast-handler-case) k)
  "Transform handler-case conservatively through host handler-case."
  (let ((result (gensym "HC-RESULT")))
    (cons 'handler-case
          (cons (cps-transform-ast (ast-handler-case-form node)
                                   (%cps-lambda (list result)
                                                (%cps-funcall k result)))
                (mapcar (lambda (clause)
                          (destructuring-bind (type var &rest body) clause
                            (list type (list var)
                                  (cps-transform-sequence body k))))
                        (ast-handler-case-clauses node))))))

(defmethod cps-transform-ast ((node ast-make-instance) k)
  "Transform make-instance conservatively through host MAKE-INSTANCE."
  (let* ((class-v (gensym "CLASS"))
         (init-pairs (loop for (key value) on (ast-make-instance-initargs node) by #'cddr
                           collect (list key value (gensym "INIT")))))
    (labels ((transform-initargs (remaining)
               (if (null remaining)
                   (let ((make-form
                           (append (list 'make-instance class-v)
                                   (mapcan (lambda (triple)
                                             (list (first triple) (third triple)))
                                           init-pairs))))
                     (%cps-funcall k make-form))
                   (destructuring-bind (_key value-form tmp) (car remaining)
                     (declare (ignore _key))
                     (cps-transform-ast value-form
                                        (%cps-lambda (list tmp)
                                                     (transform-initargs (cdr remaining))))))))
      (cps-transform-ast (ast-make-instance-class node)
                         (%cps-lambda (list class-v)
                                      (transform-initargs init-pairs))))))

(defmethod cps-transform-ast ((node ast-slot-value) k)
  "Transform slot-value conservatively through host SLOT-VALUE."
  (let ((obj-v (gensym "OBJ")))
    (cps-transform-ast (ast-slot-value-object node)
                       (%cps-lambda (list obj-v)
                                    (%cps-funcall k
                                                  (list 'slot-value obj-v
                                                        (list 'quote (ast-slot-value-slot node))))))))

(defmethod cps-transform-ast ((node ast-set-slot-value) k)
  "Transform slot mutation conservatively through host slot-value setf."
  (let ((obj-v (gensym "OBJ"))
        (val-v (gensym "VAL")))
    (cps-transform-ast (ast-set-slot-value-object node)
                       (%cps-lambda (list obj-v)
                                    (cps-transform-ast
                                     (ast-set-slot-value-value node)
                                     (%cps-lambda (list val-v)
                                                  (%cps-progn
                                                   (list 'setf
                                                         (list 'slot-value obj-v (list 'quote (ast-set-slot-value-slot node)))
                                                         val-v)
                                                   (%cps-funcall k val-v))))))))

(defmethod cps-transform-ast ((node ast-defclass) k)
  "Transform defclass conservatively through host DEFCLASS."
  (%cps-progn
   (list 'defclass (ast-defclass-name node)
         (ast-defclass-superclasses node)
         (ast-defclass-slots node))
   (%cps-funcall k (list 'quote (ast-defclass-name node)))))

(defmethod cps-transform-ast ((node ast-defgeneric) k)
  "Transform defgeneric conservatively through host DEFGENERIC."
  (%cps-progn
   (list 'defgeneric (ast-defgeneric-name node) (ast-defgeneric-params node))
   (%cps-funcall k (list 'quote (ast-defgeneric-name node)))))

(defmethod cps-transform-ast ((node ast-defmethod) k)
  "Transform defmethod conservatively through host DEFMETHOD."
  (%cps-progn
   (append (list 'defmethod
                 (ast-defmethod-name node)
                 (mapcar #'list (ast-defmethod-params node)
                         (ast-defmethod-specializers node)))
           (ast-defmethod-body node))
   (%cps-funcall k (list 'quote (ast-defmethod-name node)))))

(defmethod cps-transform-ast ((node ast-set-gethash) k)
  "Transform setf/gethash conservatively through host GETHASH setf."
  (let ((key-v (gensym "KEY"))
        (table-v (gensym "TABLE"))
        (value-v (gensym "VAL")))
    (cps-transform-ast (ast-set-gethash-key node)
                       (%cps-lambda (list key-v)
                                    (cps-transform-ast
                                     (ast-set-gethash-table node)
                                     (%cps-lambda (list table-v)
                                                  (cps-transform-ast
                                                   (ast-set-gethash-value node)
                                                   (%cps-lambda (list value-v)
                                                                (%cps-progn
                                                                 (list 'setf (list 'gethash key-v table-v) value-v)
                                                                 (%cps-funcall k value-v))))))))))

;;; Functional forms (ast-quote, ast-the, ast-values, ast-multiple-value-bind,
;;; ast-apply, ast-multiple-value-call, ast-multiple-value-prog1, ast-call)
;;; and entry points (cps-transform-ast*, cps-transform*, maybe-cps-transform,
;;; cps-transform-eval) are in cps-ast-functional.lisp (loads after this file).
