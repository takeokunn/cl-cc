;;;; compile/closure.lisp - Closure and Free Variable Analysis
(in-package :cl-cc)

(defun mutated-vars-of-list (nodes)
  "Union of setq-mutation targets across all AST NODES in a list."
  (reduce #'union (mapcar #'find-mutated-variables nodes) :initial-value nil))

(defun find-mutated-variables (ast)
  "Find all variable names that are targets of SETQ in AST."
  (typecase ast
    (ast-setq  (union (list (ast-setq-var ast))
                      (find-mutated-variables (ast-setq-value ast))))
    (ast-if    (union (find-mutated-variables (ast-if-cond ast))
                      (union (find-mutated-variables (ast-if-then ast))
                             (find-mutated-variables (ast-if-else ast)))))
    (ast-progn (mutated-vars-of-list (ast-progn-forms ast)))
    (ast-let   (mutated-vars-of-list
                (append (mapcar #'cdr (ast-let-bindings ast))
                        (ast-let-body ast))))
    (ast-lambda (mutated-vars-of-list (ast-lambda-body ast)))
    (ast-defun  (mutated-vars-of-list (ast-defun-body ast)))
    (ast-call   (mutated-vars-of-list
                 (cons (if (typep (ast-call-func ast) 'ast-node)
                           (ast-call-func ast)
                           nil)
                       (ast-call-args ast))))
    (ast-print  (find-mutated-variables (ast-print-expr ast)))
    (ast-binop  (union (find-mutated-variables (ast-binop-lhs ast))
                       (find-mutated-variables (ast-binop-rhs ast))))
    (t nil)))

(defun find-captured-in-children (body-forms params)
  "Find variables that inner lambdas/defuns in BODY-FORMS capture as free variables.
PARAMS are the current scope's bound variables — only those are candidates for boxing."
  (let ((captured nil))
    (flet ((add-captured! (forms)
             "Recurse into FORMS and union any newly-captured vars into CAPTURED."
             (setf captured (union captured (find-captured-in-children forms params))))
           (capture-free! (ast-node)
             "Intersect free vars of AST-NODE with PARAMS and add to CAPTURED."
             (setf captured (union captured
                                   (intersection (find-free-variables ast-node) params)))))
      (dolist (form body-forms captured)
        (typecase form
          (ast-lambda (capture-free! form))
          (ast-defun  (capture-free! (make-ast-lambda :params (ast-defun-params form)
                                                      :body   (ast-defun-body form))))
          ;; Recurse into compound forms to find nested lambdas
          (ast-if    (add-captured! (list (ast-if-cond form)
                                         (ast-if-then form)
                                         (ast-if-else form))))
          (ast-progn (add-captured! (ast-progn-forms form)))
          (ast-let   (add-captured! (append (mapcar #'cdr (ast-let-bindings form))
                                            (ast-let-body form))))
          (ast-call  (add-captured! (if (typep (ast-call-func form) 'ast-node)
                                        (cons (ast-call-func form) (ast-call-args form))
                                        (ast-call-args form))))
          (ast-print (add-captured! (list (ast-print-expr form))))
          (ast-binop (add-captured! (list (ast-binop-lhs form) (ast-binop-rhs form))))
          (ast-setq  (add-captured! (list (ast-setq-value form)))))))))

(defun free-vars-of-list (nodes)
  "Union of free variables across all AST NODES in a list."
  (reduce #'union (mapcar #'find-free-variables nodes) :initial-value nil))

(defun free-vars-of-defaults (param-list)
  "Union of free variables in the default expressions of an optional/key PARAM-LIST.
Each entry is (name default-ast); entries with no default contribute nothing."
  (free-vars-of-list (remove nil (mapcar #'second param-list))))

(defun find-free-variables (ast)
  "Find all free variables in AST that would need to be captured in a closure."
  (typecase ast
    (ast-int nil)
    (ast-var (list (ast-var-name ast)))
    (ast-binop (union (find-free-variables (ast-binop-lhs ast))
                      (find-free-variables (ast-binop-rhs ast))))
    (ast-if (union (find-free-variables (ast-if-cond ast))
                   (union (find-free-variables (ast-if-then ast))
                          (find-free-variables (ast-if-else ast)))))
    (ast-progn (free-vars-of-list (ast-progn-forms ast)))
    (ast-print (find-free-variables (ast-print-expr ast)))
    (ast-let
     (let ((binding-vars (mapcar #'car (ast-let-bindings ast)))
           (binding-free  (free-vars-of-list (mapcar #'cdr (ast-let-bindings ast))))
           (body-free     (free-vars-of-list (ast-let-body ast))))
       (set-difference (union binding-free body-free) binding-vars)))
    (ast-lambda
     (let* ((params (ast-lambda-params ast))
            (all-params (append params
                                (mapcar #'first (ast-lambda-optional-params ast))
                                (when (ast-lambda-rest-param ast)
                                  (list (ast-lambda-rest-param ast)))
                                (mapcar #'first (ast-lambda-key-params ast))))
            (body-free    (free-vars-of-list (ast-lambda-body ast)))
            (default-free (union (free-vars-of-defaults (ast-lambda-optional-params ast))
                                 (free-vars-of-defaults (ast-lambda-key-params ast)))))
       (set-difference (union body-free default-free) all-params)))
    (ast-setq (union (list (ast-setq-var ast))
                     (find-free-variables (ast-setq-value ast))))
    (ast-defun
     (set-difference (free-vars-of-list (ast-defun-body ast))
                     (ast-defun-params ast)))
    (ast-call
     (union (if (typep (ast-call-func ast) 'ast-node)
                (find-free-variables (ast-call-func ast))
                nil)
            (free-vars-of-list (ast-call-args ast))))
    (ast-quote nil)
    (t nil)))

