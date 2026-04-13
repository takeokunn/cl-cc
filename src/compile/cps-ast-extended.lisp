(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CPS — Extended and Imperative AST Node Transforms
;;;
;;; Contains: cps-transform-ast methods for extended/imperative forms:
;;;   setq, defvar/defparameter, handler-case, make-instance, slot-value,
;;;   set-slot-value, defclass, defgeneric, defmethod, set-gethash,
;;;   quote, the, values, multiple-value-bind, apply, multiple-value-call,
;;;   multiple-value-prog1, ast-call (general function calls);
;;;   plus entry points: maybe-cps-transform, cps-transform-ast*,
;;;   cps-transform*, cps-transform-eval.
;;;
;;; Core structural CPS transforms (ast-int through ast-labels) are in
;;; cps-ast.lisp (loads before this file).
;;;
;;; Load order: after cps-ast.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Additional AST Types

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

(defmethod cps-transform-ast ((node ast-quote) k)
  "Transform quoted literal."
  `(funcall ,k ',(ast-quote-value node)))

(defmethod cps-transform-ast ((node ast-the) k)
  "Transform type declaration (preserves type info)."
  (let* ((type (ast-the-type node))
         (value (ast-the-value node))
         (v (gensym "THE")))
    (cps-transform-ast value
                       `(lambda (,v)
                          (funcall ,k (the ,type ,v))))))

(defmethod cps-transform-ast ((node ast-values) k)
  "Transform explicit VALUES conservatively through host multiple-value-call."
  (let ((forms (ast-values-forms node)))
    (if (null forms)
        `(funcall ,k nil)
        (let ((temps (loop repeat (length forms) collect (gensym "MV"))))
          (labels ((transform-forms (remaining-forms remaining-temps)
                     (if (null remaining-forms)
                         `(multiple-value-call ,k (values ,@temps))
                         (cps-transform-ast
                          (car remaining-forms)
                          `(lambda (,(car remaining-temps))
                             ,(transform-forms (cdr remaining-forms)
                                               (cdr remaining-temps)))))))
            (transform-forms forms temps))))))

(defmethod cps-transform-ast ((node ast-multiple-value-bind) k)
  "Transform multiple-value-bind conservatively.

When the values form is explicit AST-VALUES, preserve host multiple-value-bind.
Otherwise bind only the primary value, which keeps the transformer total while
remaining conservative for currently uncovered producers."
  (let ((vars (ast-mvb-vars node))
        (values-form (ast-mvb-values-form node))
        (body (ast-mvb-body node)))
    (if (typep values-form 'ast-values)
        (let ((temps (loop repeat (length (ast-values-forms values-form))
                           collect (gensym "MVB"))))
          (labels ((transform-forms (remaining-forms remaining-temps)
                     (if (null remaining-forms)
                         `(multiple-value-bind ,vars (values ,@temps)
                            ,(cps-transform-sequence body k))
                         (cps-transform-ast
                          (car remaining-forms)
                          `(lambda (,(car remaining-temps))
                             ,(transform-forms (cdr remaining-forms)
                                               (cdr remaining-temps)))))))
            (transform-forms (ast-values-forms values-form) temps)))
        (let ((primary (gensym "MVB")))
          (cps-transform-ast
           values-form
           `(lambda (,primary)
              (let ((,(first vars) ,primary)
                    ,@(mapcar (lambda (v) `(,v nil)) (rest vars)))
                ,(cps-transform-sequence body k))))))))

(defmethod cps-transform-ast ((node ast-apply) k)
  "Transform apply conservatively via host APPLY."
  (let* ((func (ast-apply-func node))
         (args (ast-apply-args node))
         (f-v (gensym "FUNC"))
         (arg-vars (loop repeat (length args) collect (gensym "ARG"))))
    (cps-transform-ast
     func
     `(lambda (,f-v)
        ,(labels ((transform-args (remaining-args remaining-vars)
                    (if (null remaining-args)
                        `(funcall ,k (apply ,f-v ,@arg-vars))
                        (cps-transform-ast
                         (car remaining-args)
                         `(lambda (,(car remaining-vars))
                            ,(transform-args (cdr remaining-args)
                                             (cdr remaining-vars)))))))
           (transform-args args arg-vars))))))

(defmethod cps-transform-ast ((node ast-multiple-value-call) k)
  "Transform multiple-value-call.
Each arg form is CPS-transformed in sequence, collected as a list, then spread
to the function via apply.  This preserves multiple-values semantics because
host CL's multiple-value-call is only usable when args are known sexps."
  (let* ((func  (ast-mv-call-func node))
         (args  (ast-mv-call-args node))
         (f-v   (gensym "FUNC"))
         (results-v (gensym "RESULTS")))
    (cps-transform-ast func
                       `(lambda (,f-v)
                          ,(labels ((collect-args (remaining acc-var)
                                     (if (null remaining)
                                         `(funcall ,k (apply ,f-v (nreverse ,acc-var)))
                                         (let ((av (gensym "ARG")))
                                           (cps-transform-ast
                                            (car remaining)
                                            `(lambda (,av)
                                               (push ,av ,acc-var)
                                               ,(collect-args (cdr remaining) acc-var)))))))
                             `(let ((,results-v nil))
                                ,(collect-args args results-v)))))))

(defmethod cps-transform-ast ((node ast-multiple-value-prog1) k)
  "Transform multiple-value-prog1."
  (let* ((first-form (ast-mv-prog1-first node))
         (forms (ast-mv-prog1-forms node))
         (v (gensym "MV")))
    (cps-transform-ast first-form
                       `(lambda (,v)
                          ,@(mapcar (lambda (form)
                                     (cps-transform-ast form `(lambda (ignored)
                                                                (declare (ignore ignored))
                                                                nil)))
                                   forms)
                          (funcall ,k ,v)))))

(defmethod cps-transform-ast ((node ast-call) k)
  "Transform function call (non-special operator calls)."
  (let* ((func (ast-call-func node))
         (args (ast-call-args node)))
    (if (null args)
        `(funcall ,k (,func))
        (let ((arg-syms (loop for a in args collect (gensym "ARG"))))
          (labels ((transform-args (remaining-args remaining-syms)
                     (if (null remaining-args)
                         `(funcall ,k (,func ,@remaining-syms))
                         (cps-transform-ast (car remaining-args)
                                            `(lambda (,(car remaining-syms))
                                               ,(transform-args (cdr remaining-args)
                                                               (cdr remaining-syms)))))))
            (transform-args args arg-syms))))))

;;; Entry points

(defun maybe-cps-transform (thing)
  "Best-effort CPS transform for THING.
Returns a CPS form when supported, otherwise NIL."
  (handler-case
      (if (typep thing 'ast-node)
          (cps-transform-ast* thing)
          (cps-transform thing))
    (error (e)
      (declare (ignore e))
      nil)))

(defun cps-transform-ast* (node)
  "Transform an AST node to CPS, wrapping in a lambda for the outer continuation.
Returns (lambda (k) ...) form."
  (let ((k (gensym "K")))
    `(lambda (,k)
       ,(cps-transform-ast node k))))

(defun cps-transform* (expr)
  "Transform EXPR to CPS. Works with both S-expressions and AST nodes.
For S-expressions, uses the minimal bootstrap transformer.
For AST nodes, uses the full AST-based transformer."
  (typecase expr
    (ast-node (cps-transform-ast* expr))
    (t (cps-transform expr))))

(defun cps-transform-eval (expr)
  "CPS-transform an S-expression and evaluate it.
Returns the result of calling the CPS-transformed form with identity continuation."
  (let ((cps-form (cps-transform expr)))
    (eval cps-form)))
