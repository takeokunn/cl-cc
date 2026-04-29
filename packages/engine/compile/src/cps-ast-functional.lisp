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
;;;   cps-transform-ast*, cps-transform*, cps-transform-eval
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
  (%cps-funcall k (list 'quote (ast-quote-value node))))

(defmethod cps-transform-ast ((node ast-the) k)
  "Transform type declaration; the type annotation wraps the continuation result."
  (let ((type (ast-the-type node))
        (v    (gensym "THE")))
    (cps-transform-ast (ast-the-value node)
                       (%cps-lambda (list v)
                                    (%cps-funcall k (list 'the type v))))))

;;; ─── Multiple-Value Forms ────────────────────────────────────────────────────

(defun %cps-thread-values-forms (remaining-forms remaining-temps k temps)
  "Thread CPS through REMAINING-FORMS, binding each result to REMAINING-TEMPS.
When done, delivers all TEMPS via (multiple-value-call k (values ...))."
  (if (null remaining-forms)
      (list 'multiple-value-call k (cons 'values temps))
      (cps-transform-ast
       (car remaining-forms)
       (%cps-lambda (list (car remaining-temps))
                    (%cps-thread-values-forms (cdr remaining-forms)
                                              (cdr remaining-temps)
                                              k temps)))))

(defmethod cps-transform-ast ((node ast-values) k)
  "Transform explicit VALUES through host multiple-value-call.
CPS-threads each sub-form in sequence, then delivers all via (values ...)."
  (let ((forms (ast-values-forms node)))
    (if (null forms)
        (%cps-funcall k nil)
        (let ((temps (loop repeat (length forms) collect (gensym "MV"))))
          (%cps-thread-values-forms forms temps k temps)))))

(defun %cps-thread-mvb-forms (remaining-forms remaining-temps vars body k temps)
  "Thread CPS through REMAINING-FORMS for a multiple-value-bind.
When done, binds VARS to TEMPS and transforms BODY with continuation K."
  (if (null remaining-forms)
      (list 'multiple-value-bind vars (cons 'values temps)
            (cps-transform-sequence body k))
      (cps-transform-ast
       (car remaining-forms)
       (%cps-lambda (list (car remaining-temps))
                    (%cps-thread-mvb-forms (cdr remaining-forms)
                                           (cdr remaining-temps)
                                           vars body k temps)))))

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
          (%cps-thread-mvb-forms (ast-values-forms values-form) temps vars body k temps))
        (let ((primary (gensym "MVB")))
          (cps-transform-ast
           values-form
           (%cps-lambda (list primary)
                        (list 'let
                              (cons (list (first vars) primary)
                                    (mapcar (lambda (v) (list v nil)) (rest vars)))
                              (cps-transform-sequence body k))))))))

(defmethod cps-transform-ast ((node ast-multiple-value-prog1) k)
  "Transform multiple-value-prog1: evaluate first form, execute side-effect forms,
then deliver the first form's result."
  (let ((v (gensym "MV")))
    (cps-transform-ast
     (ast-mv-prog1-first node)
     (apply #'%cps-lambda
            (list* (list v)
                   (append (mapcar (lambda (form)
                                     (cps-transform-ast form
                                                        (list 'lambda '(ignored)
                                                              '(declare (ignore ignored))
                                                              nil)))
                                   (ast-mv-prog1-forms node))
                           (list (%cps-funcall k v))))))))

(defun %cps-collect-mv-call-args (remaining acc f-v k)
  "CPS-thread REMAINING arg-forms, pushing each result onto ACC.
When all args are processed, applies F-V to (nreverse acc) and delivers to K."
  (if (null remaining)
      (%cps-funcall k (list 'apply f-v (list 'nreverse acc)))
      (let ((av (gensym "ARG")))
        (cps-transform-ast
         (car remaining)
         (%cps-lambda (list av)
                      (%cps-progn
                       (list 'push av acc)
                       (%cps-collect-mv-call-args (cdr remaining) acc f-v k)))))))

(defmethod cps-transform-ast ((node ast-multiple-value-call) k)
  "Transform multiple-value-call: CPS-thread each arg form sequentially,
collect results, then spread to the function via apply."
  (let ((f-v       (gensym "FUNC"))
        (results-v (gensym "RESULTS")))
    (cps-transform-ast
     (ast-mv-call-func node)
     (%cps-lambda
      (list f-v)
      (list 'let (list (list results-v nil))
            (%cps-collect-mv-call-args (ast-mv-call-args node) results-v f-v k))))))

;;; ─── Shared CPS argument threading ──────────────────────────────────────────

(defun %cps-thread-args (arg-asts arg-syms k-thunk)
  "CPS-thread ARG-ASTS, binding each result to the corresponding symbol in ARG-SYMS.
When all args are processed, calls K-THUNK (a zero-argument closure) to produce
the continuation form."
  (if (null arg-asts)
      (funcall k-thunk)
      (cps-transform-ast
       (car arg-asts)
       (%cps-lambda (list (car arg-syms))
                    (%cps-thread-args (cdr arg-asts) (cdr arg-syms) k-thunk)))))

;;; ─── Function Call Forms ─────────────────────────────────────────────────────

(defmethod cps-transform-ast ((node ast-apply) k)
  "Transform apply: CPS-thread function and all args, then deliver via host APPLY."
  (let ((func-node (ast-apply-func node))
        (arg-vars  (loop repeat (length (ast-apply-args node)) collect (gensym "ARG"))))
    (labels ((emit-apply (func-form)
               (%cps-thread-args (ast-apply-args node) arg-vars
                                 (lambda ()
                                   (%cps-funcall k (cons 'apply (cons func-form arg-vars)))))))
      (cond
        ((typep func-node 'ast-function)
         (emit-apply (list 'function (ast-function-name func-node))))
        ((typep func-node 'ast-var)
         (emit-apply (list 'function (ast-var-name func-node))))
        ((and (typep func-node 'ast-quote)
              (symbolp (ast-quote-value func-node)))
         (emit-apply (list 'function (ast-quote-value func-node))))
        (t
         (let ((f-v (gensym "FUNC")))
           (cps-transform-ast
            func-node
            (%cps-lambda
             (list f-v)
             (emit-apply f-v)))))))))

(defmethod cps-transform-ast ((node ast-call) k)
  "Transform a general function call: CPS-thread each argument in order,
then call the function with all evaluated arguments."
  (let ((func     (ast-call-func node))
        (args     (ast-call-args node))
        (arg-syms (loop repeat (length (ast-call-args node)) collect (gensym "ARG"))))
    (%cps-thread-args args arg-syms
                      (lambda ()
                        (%cps-funcall k (cons (if (typep func 'ast-node)
                                                  (ast-to-sexp func)
                                                  func)
                                              arg-syms))))))

;;; ─── Entry Points ────────────────────────────────────────────────────────────

(defun cps-transform-ast* (node)
  "Transform an AST node to CPS, wrapping in a lambda for the outer continuation.
Returns a (lambda (k) ...) form ready for evaluation."
  (let ((k (gensym "K")))
    (%cps-lambda (list k)
                 (cps-transform-ast node k))))

(defun cps-transform* (expr)
  "Transform EXPR to CPS. Dispatches on type:
- AST nodes → full AST-based transformer (cps-transform-ast*)
- S-expressions → minimal bootstrap transformer (cps-transform)"
  (typecase expr
    (ast-node (cps-transform-ast* expr))
    (t (cps-transform expr))))

(defun cps-transform-eval (expr)
  "CPS-transform EXPR (an S-expression) and evaluate it with an identity continuation."
  (eval (cps-transform expr)))
