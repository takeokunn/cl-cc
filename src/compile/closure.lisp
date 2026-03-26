;;;; compile/closure.lisp - Closure and Free Variable Analysis
(in-package :cl-cc)

(defun mutated-vars-of-list (nodes)
  "Union of setq-mutation targets across all AST NODES in a list."
  (reduce #'union (mapcar #'find-mutated-variables nodes) :initial-value nil))

(defun find-mutated-variables (ast)
  "Find all variable names that are targets of SETQ in AST.
Uses ast-children for generic traversal — new node types work automatically."
  (if (typep ast 'ast-setq)
      (union (list (ast-setq-var ast))
             (mutated-vars-of-list (ast-children ast)))
      (mutated-vars-of-list (ast-children ast))))

(defun find-captured-in-children (body-forms params)
  "Find variables that inner lambdas/defuns in BODY-FORMS capture as free variables.
PARAMS are the current scope's bound variables — only those are candidates for boxing.
Uses ast-children for generic traversal."
  (let ((captured nil))
    (dolist (form body-forms captured)
      (typecase form
        ;; Lambda/defun boundaries: capture free vars that overlap with PARAMS
        (ast-lambda
         (setf captured (union captured (intersection (find-free-variables form) params))))
        (ast-defun
         (let ((pseudo (make-ast-lambda :params (ast-defun-params form)
                                        :body   (ast-defun-body form))))
           (setf captured (union captured (intersection (find-free-variables pseudo) params)))))
        ;; labels/flet: each binding body is a closure boundary
        (ast-local-fns
         (dolist (binding (ast-local-fns-bindings form))
           (let ((pseudo (make-ast-lambda :params (second binding) :body (cddr binding))))
             (setf captured (union captured (intersection (find-free-variables pseudo) params)))))
         (setf captured (union captured (find-captured-in-children (ast-local-fns-body form) params))))
        ;; All other compound forms: recurse via ast-children
        (t (setf captured (union captured (find-captured-in-children (ast-children form) params))))))))

(defun free-vars-of-list (nodes)
  "Union of free variables across all AST NODES in a list."
  (reduce #'union (mapcar #'find-free-variables nodes) :initial-value nil))

(defun free-vars-of-defaults (param-list)
  "Union of free variables in the default expressions of an optional/key PARAM-LIST.
Each entry is (name default-ast); entries with no default contribute nothing."
  (free-vars-of-list (remove nil (mapcar #'second param-list))))

(defun find-free-variables (ast)
  "Find all free variables in AST that would need to be captured in a closure.
Binding forms use ast-bound-names; others fall through to ast-children."
  (typecase ast
    ;; Leaves
    (ast-var (list (ast-var-name ast)))
    ;; Assignment: the var itself is free (it must exist in some enclosing scope)
    (ast-setq (union (list (ast-setq-var ast))
                     (find-free-variables (ast-setq-value ast))))
    ;; Binding forms: recurse into children, subtract bound names
    (ast-let
     (let ((binding-free (free-vars-of-list (mapcar #'cdr (ast-let-bindings ast))))
           (body-free    (free-vars-of-list (ast-let-body ast))))
       (set-difference (union binding-free body-free) (ast-bound-names ast))))
    (ast-lambda
     (let ((body-free    (free-vars-of-list (ast-lambda-body ast)))
           (default-free (union (free-vars-of-defaults (ast-lambda-optional-params ast))
                                (free-vars-of-defaults (ast-lambda-key-params ast)))))
       (set-difference (union body-free default-free) (ast-bound-names ast))))
    (ast-defun
     (set-difference (free-vars-of-list (ast-defun-body ast)) (ast-bound-names ast)))
    (ast-local-fns
     (let ((binding-free (free-vars-of-list
                          (mapcar (lambda (b)
                                    (make-ast-lambda :params (second b) :body (cddr b)))
                                  (ast-local-fns-bindings ast))))
           (body-free (free-vars-of-list (ast-local-fns-body ast))))
       (set-difference (union binding-free body-free) (ast-bound-names ast))))
    (ast-multiple-value-bind
     (set-difference (union (find-free-variables (ast-mvb-values-form ast))
                            (free-vars-of-list (ast-mvb-body ast)))
                     (ast-bound-names ast)))
    ;; All other nodes: generic traversal via ast-children
    (t (free-vars-of-list (ast-children ast)))))

