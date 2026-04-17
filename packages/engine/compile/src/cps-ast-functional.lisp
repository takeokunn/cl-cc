(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CPS — Functional / Multi-Value AST Transforms and Entry Points
;;;
;;; Extracted from cps-ast-extended.lisp.
;;; Contains cps-transform-ast methods for:
;;;   ast-quote, ast-the, ast-values, ast-multiple-value-bind,
;;;   ast-apply, ast-multiple-value-call, ast-multiple-value-prog1,
;;;   ast-call (general function calls)
;;; Plus entry points:
;;;   maybe-cps-transform, cps-transform-ast*, cps-transform*, cps-transform-eval
;;;
;;; OOP/mutation transforms (setq, defvar, handler-case, make-instance,
;;; slot-value, set-slot-value, defclass, defgeneric, defmethod, set-gethash)
;;; are in cps-ast-extended.lisp (loads before this file).
;;;
;;; Load order: after cps-ast-extended.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Atomic / Type Forms ─────────────────────────────────────────────────────

(defmethod cps-transform-ast ((node ast-quote) k)
  "Transform quoted literal — immediately calls continuation with the value."
  `(funcall ,k ',(ast-quote-value node)))

(defmethod cps-transform-ast ((node ast-the) k)
  "Transform type declaration; the type annotation wraps the continuation result."
  (let ((type (ast-the-type node))
        (v    (gensym "THE")))
    (cps-transform-ast (ast-the-value node)
                       `(lambda (,v)
                          (funcall ,k (the ,type ,v))))))

;;; ─── Multiple-Value Forms ────────────────────────────────────────────────────

(defmethod cps-transform-ast ((node ast-values) k)
  "Transform explicit VALUES through host multiple-value-call.
CPS-threads each sub-form in sequence, then delivers all via (values ...)."
  (let ((forms (ast-values-forms node)))
    (if (null forms)
        `(funcall ,k nil)
        (let ((temps (loop repeat (length forms) collect (gensym "MV"))))
          (labels ((thread-forms (remaining-forms remaining-temps)
                     (if (null remaining-forms)
                         `(multiple-value-call ,k (values ,@temps))
                         (cps-transform-ast
                          (car remaining-forms)
                          `(lambda (,(car remaining-temps))
                             ,(thread-forms (cdr remaining-forms)
                                            (cdr remaining-temps)))))))
            (thread-forms forms temps))))))

(defmethod cps-transform-ast ((node ast-multiple-value-bind) k)
  "Transform multiple-value-bind.
When the values form is AST-VALUES, thread each sub-form then bind via host MVB.
Otherwise, bind the primary value and default remaining vars to NIL."
  (let ((vars        (ast-mvb-vars node))
        (values-form (ast-mvb-values-form node))
        (body        (ast-mvb-body node)))
    (if (typep values-form 'ast-values)
        (let ((temps (loop repeat (length (ast-values-forms values-form))
                           collect (gensym "MVB"))))
          (labels ((thread-forms (remaining-forms remaining-temps)
                     (if (null remaining-forms)
                         `(multiple-value-bind ,vars (values ,@temps)
                            ,(cps-transform-sequence body k))
                         (cps-transform-ast
                          (car remaining-forms)
                          `(lambda (,(car remaining-temps))
                             ,(thread-forms (cdr remaining-forms)
                                            (cdr remaining-temps)))))))
            (thread-forms (ast-values-forms values-form) temps)))
        (let ((primary (gensym "MVB")))
          (cps-transform-ast
           values-form
           `(lambda (,primary)
              (let ((,(first vars) ,primary)
                    ,@(mapcar (lambda (v) `(,v nil)) (rest vars)))
                ,(cps-transform-sequence body k))))))))

(defmethod cps-transform-ast ((node ast-multiple-value-prog1) k)
  "Transform multiple-value-prog1: evaluate first form, execute side-effect forms,
then deliver the first form's result."
  (let ((v (gensym "MV")))
    (cps-transform-ast
     (ast-mv-prog1-first node)
     `(lambda (,v)
        ,@(mapcar (lambda (form)
                    (cps-transform-ast form `(lambda (ignored)
                                               (declare (ignore ignored))
                                               nil)))
                  (ast-mv-prog1-forms node))
        (funcall ,k ,v)))))

(defmethod cps-transform-ast ((node ast-multiple-value-call) k)
  "Transform multiple-value-call: CPS-thread each arg form sequentially,
collect results, then spread to the function via apply."
  (let ((f-v       (gensym "FUNC"))
        (results-v (gensym "RESULTS")))
    (cps-transform-ast
     (ast-mv-call-func node)
     `(lambda (,f-v)
        ,(labels ((collect-args (remaining acc)
                    (if (null remaining)
                        `(funcall ,k (apply ,f-v (nreverse ,acc)))
                        (let ((av (gensym "ARG")))
                          (cps-transform-ast
                           (car remaining)
                           `(lambda (,av)
                              (push ,av ,acc)
                              ,(collect-args (cdr remaining) acc)))))))
           `(let ((,results-v nil))
              ,(collect-args (ast-mv-call-args node) results-v)))))))

;;; ─── Function Call Forms ─────────────────────────────────────────────────────

(defmethod cps-transform-ast ((node ast-apply) k)
  "Transform apply: CPS-thread function and all args, then deliver via host APPLY."
  (let ((f-v      (gensym "FUNC"))
        (arg-vars (loop repeat (length (ast-apply-args node)) collect (gensym "ARG"))))
    (cps-transform-ast
     (ast-apply-func node)
     `(lambda (,f-v)
        ,(labels ((thread-args (remaining-args remaining-vars)
                    (if (null remaining-args)
                        `(funcall ,k (apply ,f-v ,@arg-vars))
                        (cps-transform-ast
                         (car remaining-args)
                         `(lambda (,(car remaining-vars))
                            ,(thread-args (cdr remaining-args)
                                          (cdr remaining-vars)))))))
           (thread-args (ast-apply-args node) arg-vars))))))

(defmethod cps-transform-ast ((node ast-call) k)
  "Transform a general function call: CPS-thread each argument in order,
then call the function with all evaluated arguments."
  (let ((func (ast-call-func node))
        (args (ast-call-args node)))
    (if (null args)
        `(funcall ,k (,func))
        (let ((arg-syms (loop for a in args collect (gensym "ARG"))))
          (labels ((thread-args (remaining-args remaining-syms)
                     (if (null remaining-args)
                         `(funcall ,k (,func ,@remaining-syms))
                         (cps-transform-ast
                          (car remaining-args)
                          `(lambda (,(car remaining-syms))
                             ,(thread-args (cdr remaining-args)
                                           (cdr remaining-syms)))))))
            (thread-args args arg-syms))))))

;;; ─── Entry Points ────────────────────────────────────────────────────────────

(defun cps-transform-ast* (node)
  "Transform an AST node to CPS, wrapping in a lambda for the outer continuation.
Returns a (lambda (k) ...) form ready for evaluation."
  (let ((k (gensym "K")))
    `(lambda (,k)
       ,(cps-transform-ast node k))))

(defun cps-transform* (expr)
  "Transform EXPR to CPS. Dispatches on type:
- AST nodes → full AST-based transformer (cps-transform-ast*)
- S-expressions → minimal bootstrap transformer (cps-transform)"
  (typecase expr
    (ast-node (cps-transform-ast* expr))
    (t (cps-transform expr))))

(defun maybe-cps-transform (thing)
  "Best-effort CPS transform. Returns the CPS form on success, NIL on failure."
  (handler-case
      (if (typep thing 'ast-node)
          (cps-transform-ast* thing)
          (cps-transform thing))
    (error (e)
      (declare (ignore e))
      nil)))

(defun cps-transform-eval (expr)
  "CPS-transform EXPR (an S-expression) and evaluate it with an identity continuation."
  (eval (cps-transform expr)))
