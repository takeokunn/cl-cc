;;;; compile/closure.lisp - Closure and Free Variable Analysis
(in-package :cl-cc/compile)

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

(defun binding-escape-kinds-in-body (body-forms binding-name &key (safe-consumers nil))
  "Return a list of conservative escape kinds for BINDING-NAME in BODY-FORMS.

Currently reports the documented escape modes that this compiler slice can
reliably identify intra-procedurally:

- :return         the binding flows out as a direct value/result
- :capture        the binding is captured by an inner closure/local function
- :external-call  the binding is passed to APPLY/FUNCALL or an unknown callee

SAFE-CONSUMERS is a list of uppercase function names that may consume the
binding without constituting an escape."
  (labels ((add-kind (kind acc)
             (if (member kind acc) acc (cons kind acc)))
           (merge-kinds (&rest kind-lists)
             (reduce (lambda (acc ks) (reduce (lambda (a k) (add-kind k a)) ks :initial-value acc))
                     kind-lists :initial-value nil))
           (mentions-node-p (node)
             (typecase node
               (ast-var (eq (ast-var-name node) binding-name))
               (t (some #'mentions-node-p (ast-children node)))))
           (mentions-forms-p (forms)
             (and (listp forms) (some #'mentions-node-p forms)))
           (classify-children (node)
             (reduce #'merge-kinds (mapcar #'classify (ast-children node)) :initial-value nil))
           (%capture-kinds (body)
             ;; Returns (:capture) when binding appears in a closure body, plus
             ;; any inner escapes found by recursing into the body forms.
             (merge-kinds (when (mentions-forms-p body) (list :capture))
                          (reduce #'merge-kinds (mapcar #'classify body) :initial-value nil)))
           (classify (node)
             (typecase node
               ;; Leaf that triggers :return
               (ast-var (when (eq (ast-var-name node) binding-name) (list :return)))
               ;; Closures: add :capture when the binding appears in their specific body
               (ast-lambda     (%capture-kinds (ast-lambda-body node)))
               (ast-defun      (%capture-kinds (ast-defun-body node)))
               (ast-defmethod  (%capture-kinds (ast-defmethod-body node)))
               (ast-local-fns  (%capture-kinds (ast-local-fns-body node)))
               ;; apply always escapes
               (ast-apply
                (add-kind :external-call (classify-children node)))
               ;; call: safe-consumer, explicit apply/funcall, or unknown callee
               (ast-call
                (let ((func (ast-call-func node))
                      (args (ast-call-args node)))
                  (cond
                    ((and (symbolp func)
                          (member (symbol-name func) safe-consumers :test #'string=))
                     nil)
                    ((and (symbolp func)
                          (member (symbol-name func) '("APPLY" "FUNCALL") :test #'string=))
                     (add-kind :external-call
                               (reduce #'merge-kinds (mapcar #'classify args) :initial-value nil)))
                    (t
                     (let ((arg-kinds (reduce #'merge-kinds (mapcar #'classify args) :initial-value nil)))
                       (if arg-kinds
                           (add-kind :external-call
                                     (merge-kinds (when (typep func 'ast-node) (classify func))
                                                  arg-kinds))
                           (when (typep func 'ast-node) (classify func))))))))
               ;; All other nodes: generic traversal via ast-children
               (t (classify-children node)))))
    (if (listp body-forms)
        (reduce #'merge-kinds (mapcar #'classify body-forms) :initial-value nil)
        nil)))

(defun binding-escapes-in-body-p (body-forms binding-name &key (safe-consumers nil))
  "Return T when BINDING-NAME escapes from BODY-FORMS.

This is a conservative intra-procedural escape analysis helper layered on top
of BINDING-ESCAPE-KINDS-IN-BODY. SAFE-CONSUMERS is a list of uppercase
function names that are allowed to consume the binding without causing escape."
  (not (null (binding-escape-kinds-in-body body-forms binding-name
                                           :safe-consumers safe-consumers))))

(defun closure-capture-key (captured-vars)
  "Return a canonical key for CAPTURED-VARS.

CAPTURED-VARS is an alist of (var . reg). The key ignores order and groups by
captured variable names only, which is enough to detect sibling closures that
could share one environment record." 
  (sort (remove-duplicates (mapcar #'car captured-vars) :test #'eq)
        #'string< :key #'symbol-name))

(defun group-shared-sibling-captures (captured-var-lists)
  "Group sibling closure captures that share the same captured variable set.

Returns an EQUAL hash-table mapping canonical capture keys to the list of
captured-var alists using that key. Singleton groups are omitted." 
  (let ((all-groups (make-hash-table :test #'equal))
        (shared-only (make-hash-table :test #'equal)))
    (dolist (captures captured-var-lists)
      (push captures (gethash (closure-capture-key captures) all-groups)))
    (maphash (lambda (key group)
               (when (> (length group) 1)
                 (setf (gethash key shared-only) (nreverse group))))
             all-groups)
    shared-only))

(defun binding-direct-call-count-in-body (body-forms binding-name)
  "Count direct AST-CALL occurrences of BINDING-NAME in BODY-FORMS." 
  (labels ((count-node (node)
             (typecase node
               (ast-call
                (+ (if (eq (ast-call-func node) binding-name) 1 0)
                   (reduce #'+ (mapcar #'count-node (ast-call-args node)) :initial-value 0)))
               (t (reduce #'+ (mapcar #'count-node (ast-children node)) :initial-value 0)))))
    (if (listp body-forms)
        (reduce #'+ (mapcar #'count-node body-forms) :initial-value 0)
        0)))

(defun binding-one-shot-p (body-forms binding-name &key (safe-consumers nil))
  "Return T when BINDING-NAME is a non-escaping direct call used exactly once." 
  (and (= 1 (binding-direct-call-count-in-body body-forms binding-name))
       (not (binding-escapes-in-body-p body-forms binding-name
                                       :safe-consumers safe-consumers))))

(defun closure-sharing-key (entry-label captured-vars)
  "Return a canonical sharing key for closures with ENTRY-LABEL and CAPTURED-VARS." 
  (list entry-label (closure-capture-key captured-vars)))

(defun group-shareable-closures (closure-descriptors)
  "Group closures that share both code label and capture set.

CLOSURE-DESCRIPTORS is a list of plist-like records containing at least
  :entry-label and :captured-vars.
Singleton groups are omitted." 
  (let ((all-groups (make-hash-table :test #'equal))
        (shared-only (make-hash-table :test #'equal)))
    (dolist (desc closure-descriptors)
      (push desc (gethash (closure-sharing-key (getf desc :entry-label)
                                               (getf desc :captured-vars))
                          all-groups)))
    (maphash (lambda (key group)
               (when (> (length group) 1)
                 (setf (gethash key shared-only) (nreverse group))))
             all-groups)
    shared-only))
