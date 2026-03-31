(in-package :cl-cc)

;;; AST-Based CPS Transformation

(defgeneric cps-transform-ast (node k)
  (:documentation "Transform an AST node to continuation-passing style.
NODE is an AST node, K is the continuation (a symbol or lambda expression).
Returns a CPS-transformed S-expression."))

;;; Helper: Transform a sequence of forms, passing result to continuation
(defun %cps-transform-sequence-step (forms k empty-result)
  "Shared CPS sequence walker used by sequence-like special forms."
  (if (null forms)
      empty-result
      (let ((tmp (gensym "SEQ")))
        (if (null (cdr forms))
            (cps-transform-ast (car forms) k)
            (cps-transform-ast (car forms)
                               `(lambda (,tmp)
                                  (declare (ignore ,tmp))
                                  ,(%cps-transform-sequence-step
                                    (cdr forms) k empty-result)))))))

(defun cps-transform-sequence (forms k)
  "Transform a sequence of forms in CPS. Returns the value of the last form."
  (%cps-transform-sequence-step forms k `(funcall ,k nil)))

;;; Core AST Types

(defmethod cps-transform-ast ((node ast-int) k)
  `(funcall ,k ,(ast-int-value node)))

(defmethod cps-transform-ast ((node ast-var) k)
  `(funcall ,k ,(ast-var-name node)))

(defmethod cps-transform-ast ((node ast-binop) k)
  (let ((op (ast-binop-op node))
        (lhs (ast-binop-lhs node))
        (rhs (ast-binop-rhs node))
        (va (gensym "A"))
        (vb (gensym "B")))
    (cps-transform-ast lhs
                       (list 'lambda (list va)
                             (cps-transform-ast rhs
                                                (list 'lambda (list vb)
                                                      (list 'funcall k
                                                            (list op va vb))))))))

(defmethod cps-transform-ast ((node ast-if) k)
  (let ((v (gensym "COND")))
    (cps-transform-ast (ast-if-cond node)
                       (list 'lambda (list v)
                             (list 'if (list 'zerop v)
                                   (cps-transform-ast (ast-if-else node) k)
                                   (cps-transform-ast (ast-if-then node) k))))))

(defmethod cps-transform-ast ((node ast-progn) k)
  (cps-transform-sequence (ast-progn-forms node) k))

(defmethod cps-transform-ast ((node ast-print) k)
  (let ((v (gensym "PRINT")))
    (cps-transform-ast (ast-print-expr node)
                       (list 'lambda (list v)
                             (list 'progn
                                   (list 'print v)
                                   (list 'funcall k v))))))

(defmethod cps-transform-ast ((node ast-let) k)
  (let ((bindings (ast-let-bindings node))
        (body (ast-let-body node)))
    (labels ((expand-bindings (rest env-k)
               (if (null rest)
                   (cps-transform-sequence body env-k)
                   (let* ((binding (car rest))
                          (sym (car binding))
                          (val (cdr binding))
                          (tmp (gensym (symbol-name sym))))
                     (cps-transform-ast val
                                        `(lambda (,tmp)
                                           (let ((,sym ,tmp))
                                             ,(expand-bindings (cdr rest) env-k))))))))
      (expand-bindings bindings k))))

;;; Lambda and Closures

(defmethod cps-transform-ast ((node ast-lambda) k)
  "Transform a lambda expression to CPS. The continuation becomes an extra parameter."
  (let* ((params (ast-lambda-params node))
         (body   (ast-lambda-body node))
         (k-var  (gensym "K")))
    `(funcall ,k
              (lambda (,@params ,k-var)
                ,(cps-transform-sequence body k-var)))))

(defmethod cps-transform-ast ((node ast-function) k)
  "Transform #'var to CPS (function reference)."
  `(funcall ,k (function ,(ast-function-name node))))

;;; Block and Return-From

(defmethod cps-transform-ast ((node ast-block) k)
  "Transform block with named exit point.
The block creates a catch tag for return-from."
  (let* ((name (ast-block-name node))
         (body (ast-block-body node))
         (result (gensym "RESULT")))
    ;; The continuation for the body is a special one that returns from the block
    `(block ,name
       ,(cps-transform-sequence body
                                `(lambda (,result)
                                   (return-from ,name
                                     (funcall ,k ,result)))))))

(defmethod cps-transform-ast ((node ast-return-from) k)
  "Transform return-from to exit the block with a value."
  (let* ((name (ast-return-from-name node))
         (value (ast-return-from-value node))
         (v (gensym "VAL")))
    ;; Evaluate the value, then return from the block
    ;; Note: k is ignored since we're doing a non-local exit
    (cps-transform-ast value
                       `(lambda (,v)
                          (return-from ,name ,v)))))

;;; Tagbody and Go

(defun cps-transform-tagbody-section (forms k)
  "Transform a section of tagbody forms between tags, passing NIL to K when done."
  (%cps-transform-sequence-step forms k `(funcall ,k nil)))

(defmethod cps-transform-ast ((node ast-tagbody) k)
  "Transform tagbody with labeled sections.
Uses a tag table to map tags to their continuations."
  (let* ((tags (ast-tagbody-tags node))
         (tag-k (gensym "TAG-K")))
    `(tagbody
       ,@(loop for entry in tags
               for tag = (car entry)
               for forms = (cdr entry)
               for result-k = (if (eq entry (car (last tags))) k tag-k)
               collect tag
               collect (cps-transform-tagbody-section forms result-k))
       (funcall ,k nil))))

(defmethod cps-transform-ast ((node ast-go) k)
  "Transform go to jump to a tag."
  (let ((tag (ast-go-tag node)))
    ;; k is ignored since go performs a non-local jump
    `(go ,tag)))

;;; Catch and Throw

(defmethod cps-transform-ast ((node ast-catch) k)
  "Transform catch with dynamic tag."
  (let* ((tag-expr (ast-catch-tag node))
         (body (ast-catch-body node))
         (tag-v (gensym "TAG"))
         (result (gensym "RESULT")))
    (cps-transform-ast tag-expr
                       (list 'lambda (list tag-v)
                             (list 'catch tag-v
                                   (cps-transform-sequence body
                                                           (list 'lambda (list result)
                                                                 (list 'funcall k result))))))))

(defmethod cps-transform-ast ((node ast-throw) k)
  "Transform throw to unwind to matching catch."
  (let* ((tag-expr (ast-throw-tag node))
         (value (ast-throw-value node))
         (tag-v (gensym "TAG"))
         (val-v (gensym "VAL")))
    (cps-transform-ast tag-expr
                       (list 'lambda (list tag-v)
                             (cps-transform-ast value
                                                (list 'lambda (list val-v)
                                                      (list 'throw tag-v val-v)))))))

;;; Unwind-Protect

(defmethod cps-transform-ast ((node ast-unwind-protect) k)
  "Transform unwind-protect with guaranteed cleanup.
The cleanup forms always run, even on non-local exit."
  (let* ((protected (ast-unwind-protected node))
         (cleanup (ast-unwind-cleanup node))
         (result (gensym "RESULT"))
         (cleanup-result (gensym "CLEANUP")))
    `(unwind-protect
         ,(cps-transform-ast protected
                             `(lambda (,result)
                                (funcall ,k ,result)))
       ,(if cleanup
            (cps-transform-sequence cleanup
                                    `(lambda (,cleanup-result)
                                       (declare (ignore ,cleanup-result))
                                       nil))
            nil))))

;;; Flet and Labels (Local Function Bindings)

(defun cps-transform-fn-binding (binding k-var)
  "Transform a function binding (name params . body) to CPS form."
  (let* ((name (first binding))
         (params (second binding))
         (body (cddr binding)))
    `(,name (lambda (,@params ,k-var)
              ,(cps-transform-sequence body k-var)))))

(defun cps-transform-local-fns (form-kw bindings body k)
  "Transform a flet/labels binding group to CPS.
FORM-KW is either 'flet or 'labels; they share identical CPS structure."
  (let ((fn-k (gensym (if (eq form-kw 'flet) "FLET-K" "LABELS-K"))))
    `(,form-kw ,(loop for binding in bindings
                      collect (cps-transform-fn-binding binding fn-k))
       ,(cps-transform-sequence body
                                `(lambda (,fn-k)
                                   (funcall ,k ,fn-k))))))

(defmethod cps-transform-ast ((node ast-flet) k)
  "Transform flet (non-recursive local functions)."
  (cps-transform-local-fns 'flet (ast-flet-bindings node) (ast-flet-body node) k))

(defmethod cps-transform-ast ((node ast-labels) k)
  "Transform labels (mutually recursive local functions)."
  (cps-transform-local-fns 'labels (ast-labels-bindings node) (ast-labels-body node) k))

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
                          ;; Collect each arg's single value into a list, then apply.
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

;;; Entry Point for AST-based CPS Transformation

(defun cps-transform-ast* (node)
  "Transform an AST node to CPS, wrapping in a lambda for the outer continuation.
Returns (lambda (k) ...) form."
  (let ((k (gensym "K")))
    `(lambda (,k)
       ,(cps-transform-ast node k))))

;;; Dispatcher: Choose between sexp and AST based transformation

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
