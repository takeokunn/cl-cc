(in-package :cl-cc)
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

(defmethod cps-transform-ast ((node ast-setq) k)
  "Transform setq assignment."
  (let* ((var (ast-setq-var node))
         (value (ast-setq-value node))
         (v (gensym "SETQ")))
    (cps-transform-ast value
                       (list 'lambda (list v)
                              (list 'progn
                                    (list 'setq var v)
                                    (list 'funcall k v))))))

(defmethod cps-transform-ast ((node ast-defvar) k)
  "Transform defvar/defparameter conservatively through host special forms."
  (let ((name (ast-defvar-name node))
        (kind (ast-defvar-kind node))
        (value (ast-defvar-value node))
        (v (gensym "DEFVAR")))
    (if value
        (cps-transform-ast value
                           `(lambda (,v)
                              (progn
                                (,kind ,name ,v)
                                (funcall ,k ,name))))
        `(progn
           (,kind ,name)
           (funcall ,k ,name)))))

(defmethod cps-transform-ast ((node ast-handler-case) k)
  "Transform handler-case conservatively through host handler-case."
  (let ((result (gensym "HC-RESULT")))
    `(handler-case
         ,(cps-transform-ast (ast-handler-case-form node)
                             `(lambda (,result)
                                (funcall ,k ,result)))
       ,@(mapcar (lambda (clause)
                    (destructuring-bind (type var &rest body) clause
                      `(,type (,var)
                         ,(cps-transform-sequence body k))))
                  (ast-handler-case-clauses node)))))

(defmethod cps-transform-ast ((node ast-make-instance) k)
  "Transform make-instance conservatively through host MAKE-INSTANCE."
  (let* ((class-v (gensym "CLASS"))
         (init-pairs (loop for (key value) on (ast-make-instance-initargs node) by #'cddr
                           collect (list key value (gensym "INIT")))))
    (labels ((transform-initargs (remaining)
               (if (null remaining)
                   `(funcall ,k
                             (make-instance ,class-v
                                            ,@(mapcan (lambda (triple)
                                                        (list (first triple) (third triple)))
                                                      init-pairs)))
                   (destructuring-bind (_key value-form tmp) (car remaining)
                     (declare (ignore _key))
                     (cps-transform-ast value-form
                                        `(lambda (,tmp)
                                           ,(transform-initargs (cdr remaining))))))))
      (cps-transform-ast (ast-make-instance-class node)
                         `(lambda (,class-v)
                            ,(transform-initargs init-pairs))))))

(defmethod cps-transform-ast ((node ast-slot-value) k)
  "Transform slot-value conservatively through host SLOT-VALUE."
  (let ((obj-v (gensym "OBJ")))
    (cps-transform-ast (ast-slot-value-object node)
                       `(lambda (,obj-v)
                          (funcall ,k (slot-value ,obj-v ',(ast-slot-value-slot node)))))))

(defmethod cps-transform-ast ((node ast-set-slot-value) k)
  "Transform slot mutation conservatively through host slot-value setf."
  (let ((obj-v (gensym "OBJ"))
        (val-v (gensym "VAL")))
    (cps-transform-ast (ast-set-slot-value-object node)
                       `(lambda (,obj-v)
                          ,(cps-transform-ast
                            (ast-set-slot-value-value node)
                             `(lambda (,val-v)
                                (progn
                                  (setf (slot-value ,obj-v ',(ast-set-slot-value-slot node)) ,val-v)
                                  (funcall ,k ,val-v))))))))

(defmethod cps-transform-ast ((node ast-defclass) k)
  "Transform defclass conservatively through host DEFCLASS."
  `(progn
     (defclass ,(ast-defclass-name node) ,(ast-defclass-superclasses node) ,(ast-defclass-slots node))
     (funcall ,k ',(ast-defclass-name node))))

(defmethod cps-transform-ast ((node ast-defgeneric) k)
  "Transform defgeneric conservatively through host DEFGENERIC."
  `(progn
     (defgeneric ,(ast-defgeneric-name node) ,(ast-defgeneric-params node))
     (funcall ,k ',(ast-defgeneric-name node))))

(defmethod cps-transform-ast ((node ast-defmethod) k)
  "Transform defmethod conservatively through host DEFMETHOD."
  `(progn
     (defmethod ,(ast-defmethod-name node)
         ,(mapcar #'list (ast-defmethod-params node) (ast-defmethod-specializers node))
       ,@(ast-defmethod-body node))
     (funcall ,k ',(ast-defmethod-name node))))

(defmethod cps-transform-ast ((node ast-set-gethash) k)
  "Transform setf/gethash conservatively through host GETHASH setf."
  (let ((key-v (gensym "KEY"))
        (table-v (gensym "TABLE"))
        (value-v (gensym "VAL")))
    (cps-transform-ast (ast-set-gethash-key node)
                       `(lambda (,key-v)
                          ,(cps-transform-ast
                            (ast-set-gethash-table node)
                            `(lambda (,table-v)
                               ,(cps-transform-ast
                                 (ast-set-gethash-value node)
                                 `(lambda (,value-v)
                                    (progn
                                      (setf (gethash ,key-v ,table-v) ,value-v)
                                      (funcall ,k ,value-v))))))))))

;;; Functional forms (ast-quote, ast-the, ast-values, ast-multiple-value-bind,
;;; ast-apply, ast-multiple-value-call, ast-multiple-value-prog1, ast-call)
;;; and entry points (cps-transform-ast*, cps-transform*, maybe-cps-transform,
;;; cps-transform-eval) are in cps-ast-functional.lisp (loads after this file).
