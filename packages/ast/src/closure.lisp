;;;; compile/closure.lisp - Closure and Free Variable Analysis
(in-package :cl-cc/ast)

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

(defun %escape-add-kind (kind acc)
  "Return ACC with KIND added unless already present."
  (if (member kind acc) acc (cons kind acc)))

(defun %escape-merge-kinds (&rest kind-lists)
  "Merge multiple escape-kind lists into one deduplicated list."
  (reduce (lambda (acc ks)
            (reduce (lambda (a k) (%escape-add-kind k a)) ks :initial-value acc))
          kind-lists :initial-value nil))

(defun %escape-mentions-node-p (node binding-name)
  "Return T if NODE or any descendant references BINDING-NAME."
  (typecase node
    (ast-var (eq (ast-var-name node) binding-name))
    (t (some (lambda (child) (%escape-mentions-node-p child binding-name))
             (ast-children node)))))

(defun %escape-mentions-forms-p (forms binding-name)
  "Return T if any form in FORMS references BINDING-NAME."
  (and (listp forms)
       (some (lambda (f) (%escape-mentions-node-p f binding-name)) forms)))

(defun %escape-classify-children (node binding-name safe-consumers)
  "Merge escape kinds for all children of NODE."
  (reduce #'%escape-merge-kinds
          (mapcar (lambda (c) (%escape-classify c binding-name safe-consumers))
                  (ast-children node))
          :initial-value nil))

(defun %escape-capture-kinds (body binding-name safe-consumers)
  "Escape kinds when BODY is a closure boundary over BINDING-NAME."
  (%escape-merge-kinds
   (when (%escape-mentions-forms-p body binding-name) (list :capture))
   (reduce #'%escape-merge-kinds
           (mapcar (lambda (f) (%escape-classify f binding-name safe-consumers)) body)
           :initial-value nil)))

(defun %escape-classify (node binding-name safe-consumers)
  "Classify how BINDING-NAME escapes through NODE."
  (typecase node
    (ast-var        (when (eq (ast-var-name node) binding-name) (list :return)))
    (ast-lambda     (%escape-capture-kinds (ast-lambda-body node)    binding-name safe-consumers))
    (ast-defun      (%escape-capture-kinds (ast-defun-body node)     binding-name safe-consumers))
    (ast-defmethod  (%escape-capture-kinds (ast-defmethod-body node) binding-name safe-consumers))
    (ast-local-fns  (%escape-capture-kinds (ast-local-fns-body node) binding-name safe-consumers))
    (ast-apply
     (%escape-add-kind :external-call
                       (%escape-classify-children node binding-name safe-consumers)))
    (ast-call
     (let ((func (ast-call-func node))
           (args (ast-call-args node)))
       (cond
         ((and (symbolp func) (member (symbol-name func) safe-consumers :test #'string=))
          nil)
         ((and (symbolp func) (member (symbol-name func) '("APPLY" "FUNCALL") :test #'string=))
          (%escape-add-kind :external-call
                            (reduce #'%escape-merge-kinds
                                    (mapcar (lambda (a) (%escape-classify a binding-name safe-consumers)) args)
                                    :initial-value nil)))
         (t
          (let ((arg-kinds (reduce #'%escape-merge-kinds
                                   (mapcar (lambda (a) (%escape-classify a binding-name safe-consumers)) args)
                                   :initial-value nil)))
            (if arg-kinds
                (%escape-add-kind :external-call
                                  (%escape-merge-kinds
                                   (when (typep func 'ast-node) (%escape-classify func binding-name safe-consumers))
                                   arg-kinds))
                (when (typep func 'ast-node) (%escape-classify func binding-name safe-consumers))))))))
    (t (%escape-classify-children node binding-name safe-consumers))))

(defun binding-escape-kinds-in-body (body-forms binding-name &key (safe-consumers nil))
  "Return a list of conservative escape kinds for BINDING-NAME in BODY-FORMS.

Escape modes:
- :return         the binding flows out as a direct value/result
- :capture        the binding is captured by an inner closure/local function
- :external-call  the binding is passed to APPLY/FUNCALL or an unknown callee

SAFE-CONSUMERS is a list of uppercase function names that may consume the
binding without constituting an escape."
  (if (listp body-forms)
      (reduce #'%escape-merge-kinds
              (mapcar (lambda (f) (%escape-classify f binding-name safe-consumers))
                      body-forms)
              :initial-value nil)
      nil))

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

(defun %count-ast-calls (node binding-name)
  "Count direct AST-CALL occurrences of BINDING-NAME in NODE tree."
  (typecase node
    (ast-call
     (+ (if (eq (ast-call-func node) binding-name) 1 0)
        (reduce #'+ (mapcar (lambda (n) (%count-ast-calls n binding-name))
                            (ast-call-args node))
                :initial-value 0)))
    (t
     (reduce #'+ (mapcar (lambda (n) (%count-ast-calls n binding-name))
                         (ast-children node))
             :initial-value 0))))

(defun binding-direct-call-count-in-body (body-forms binding-name)
  "Count direct AST-CALL occurrences of BINDING-NAME in BODY-FORMS."
  (if (listp body-forms)
      (reduce #'+ (mapcar (lambda (n) (%count-ast-calls n binding-name)) body-forms)
              :initial-value 0)
      0))

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
