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

(defun binding-escapes-in-body-p (body-forms binding-name &key (safe-consumers nil))
  "Return T when BINDING-NAME escapes from BODY-FORMS.

This is a conservative intra-procedural escape analysis helper. A binding is
treated as escaping if it is returned directly, captured by an inner closure,
passed to APPLY/FUNCALL/unknown callees, or otherwise appears outside a known
safe consumer context. SAFE-CONSUMERS is a list of uppercase function names
that are allowed to consume the binding without causing escape."
  (labels ((escapes-p (node)
             (typecase node
               (ast-var (eq (ast-var-name node) binding-name))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) nil)
               (ast-progn (some #'escapes-p (ast-progn-forms node)))
               (ast-block (some #'escapes-p (ast-block-body node)))
               (ast-if (or (escapes-p (ast-if-cond node))
                           (escapes-p (ast-if-then node))
                           (escapes-p (ast-if-else node))))
               (ast-let (or (some #'escapes-p (mapcar #'cdr (ast-let-bindings node)))
                            (some #'escapes-p (ast-let-body node))))
               (ast-setq (escapes-p (ast-setq-value node)))
               (ast-return-from (escapes-p (ast-return-from-value node)))
               (ast-the (escapes-p (ast-the-value node)))
               (ast-values (some #'escapes-p (ast-values-forms node)))
               (ast-catch (or (escapes-p (ast-catch-tag node))
                              (some #'escapes-p (ast-catch-body node))))
               (ast-throw (or (escapes-p (ast-throw-tag node))
                              (escapes-p (ast-throw-value node))))
               (ast-unwind-protect (or (escapes-p (ast-unwind-protected node))
                                       (some #'escapes-p (ast-unwind-cleanup node))))
               (ast-handler-case (or (escapes-p (ast-handler-case-form node))
                                     (some (lambda (clause)
                                             (some #'escapes-p (cddr clause)))
                                           (ast-handler-case-clauses node))))
               (ast-multiple-value-call (or (escapes-p (ast-mv-call-func node))
                                            (some #'escapes-p (ast-mv-call-args node))))
               (ast-multiple-value-prog1 (or (escapes-p (ast-mv-prog1-first node))
                                             (some #'escapes-p (ast-mv-prog1-forms node))))
               (ast-multiple-value-bind (or (escapes-p (ast-mvb-values-form node))
                                            (some #'escapes-p (ast-mvb-body node))))
               (ast-apply (or (escapes-p (ast-apply-func node))
                              (some #'escapes-p (ast-apply-args node))))
               (ast-call
                (let ((func (ast-call-func node))
                      (args (ast-call-args node)))
                  (cond
                    ((and (symbolp func)
                          (member (symbol-name func) safe-consumers :test #'string=))
                     nil)
                    ((and (symbolp func)
                          (member (symbol-name func) '("APPLY" "FUNCALL") :test #'string=))
                     (some #'escapes-p args))
                    (t (or (and (typep func 'ast-node) (escapes-p func))
                           (some #'escapes-p args))))))
               (ast-lambda
                (or (not (null (find binding-name (find-captured-in-children (ast-lambda-body node)
                                                                             (list binding-name)))))
                    (some #'escapes-p (ast-lambda-body node))))
               (ast-defun
                (or (not (null (find binding-name (find-captured-in-children (ast-defun-body node)
                                                                             (list binding-name)))))
                    (some #'escapes-p (ast-defun-body node))))
               (ast-defmethod
                (or (not (null (find binding-name (find-captured-in-children (ast-defmethod-body node)
                                                                             (list binding-name)))))
                    (some #'escapes-p (ast-defmethod-body node))))
               (ast-local-fns
                (or (not (null (find binding-name (find-captured-in-children (ast-local-fns-body node)
                                                                             (list binding-name)))))
                    (some #'escapes-p (ast-local-fns-body node))))
               (ast-defclass (some #'escapes-p (ast-defclass-slots node)))
               (ast-defvar (escapes-p (ast-defvar-value node)))
               (ast-defmacro (some #'escapes-p (ast-defmacro-body node)))
               (ast-make-instance
                (some #'escapes-p
                      (loop for (_ value) on (ast-make-instance-initargs node) by #'cddr
                            collect value)))
               (ast-slot-value (escapes-p (ast-slot-value-object node)))
               (ast-set-slot-value (or (escapes-p (ast-set-slot-value-object node))
                                       (escapes-p (ast-set-slot-value-value node))))
               (ast-set-gethash (or (escapes-p (ast-set-gethash-key node))
                                    (escapes-p (ast-set-gethash-table node))
                                    (escapes-p (ast-set-gethash-value node))))
               (t (some #'escapes-p (ast-children node))))))
    (and (listp body-forms)
         (or (not (null (member binding-name (find-captured-in-children body-forms (list binding-name)))))
             (some #'escapes-p body-forms)))))
