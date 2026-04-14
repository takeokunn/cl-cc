(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Let-Binding Analysis Layer
;;;
;;; Contains:
;;;   - AST predicate helpers (cons/array/ignored detection)
;;;   - Sink-if candidate analysis (%ast-let-sink-if-candidate)
;;;   - %define-binding-walker macro + three generated walkers:
;;;       %array-binding-static-access-p
;;;       %instance-binding-static-slot-only-p
;;;       %closure-binding-direct-call-only-p
;;;
;;; Classification predicates, binding emitters, and compile-ast (ast-let)
;;; are in codegen-core-let-emit.lisp (loads after).
;;;
;;; Load order: after codegen-core.lisp, before codegen-core-let-emit.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── AST predicate helpers ────────────────────────────────────────────────

(defun %ast-let-binding-ignored-p (name declarations)
  (some (lambda (decl)
          (and (consp decl)
               (member (car decl) '(ignore ignorable))
               (member name (cdr decl) :test #'eq)))
        declarations))

(defun %ast-cons-call-p (node)
  (and (typep node 'ast-call)
       (let ((func (ast-call-func node)))
         (or (eq func 'cons)
             (and (typep func 'ast-var)
                  (eq (ast-var-name func) 'cons))))
       (= (length (ast-call-args node)) 2)))

(defun %ast-make-array-call-p (node)
  (and (typep node 'ast-call)
       (let ((func (ast-call-func node)))
         (or (eq func 'make-array)
             (and (typep func 'ast-var)
                  (eq (ast-var-name func) 'make-array))))
       (= (length (ast-call-args node)) 1)))

(defun %ast-make-array-int-call-p (node)
  (and (%ast-make-array-call-p node)
       (typep (first (ast-call-args node)) 'ast-int)))

(defun %binding-mentioned-in-body-p (body-forms binding-name)
  (and (listp body-forms)
       (find binding-name
             (find-free-variables (make-ast-progn :forms body-forms))
             :test #'eq)))

(defun %ast-lambda-bound-names (node)
  (append (copy-list (ast-lambda-params node))
          (loop for spec in (ast-lambda-optional-params node)
                collect (if (consp spec) (first spec) spec))
          (when (ast-lambda-rest-param node)
            (list (ast-lambda-rest-param node)))
          (loop for spec in (ast-lambda-key-params node)
                collect (let ((name (if (consp spec) (first spec) spec)))
                          (if (consp name) (second name) name)))))

(defun %ast-as-body-forms (node)
  (if (typep node 'ast-progn)
      (ast-progn-forms node)
      (list node)))

(defun %ast-wrap-bindings (bindings body)
  (if bindings
      (make-ast-let :bindings bindings :body body)
      (if (= (length body) 1)
          (first body)
          (make-ast-progn :forms body))))

;;; ── Sink-if candidate analysis ───────────────────────────────────────────
;;;
;;; When a let has exactly one binding and exactly one body form that is an
;;; IF, and the binding is used in only one branch, we can sink the binding
;;; into that branch to reduce unnecessary allocation.

(defun %ast-let-sink-if-candidate (node)
  (let* ((bindings (ast-let-bindings node))
         (body (ast-let-body node)))
    (when (and (= (length body) 1)
               (typep (first body) 'ast-if))
      (let* ((if-node (first body))
             (then-forms (%ast-as-body-forms (ast-if-then if-node)))
             (else-forms (%ast-as-body-forms (ast-if-else if-node))))
        (loop for binding in bindings
              for idx from 0
              for name = (car binding)
              for expr = (cdr binding)
              for then-uses = (%binding-mentioned-in-body-p then-forms name)
              for else-uses = (%binding-mentioned-in-body-p else-forms name)
              for outer-bindings = (append (subseq bindings 0 idx)
                                           (subseq bindings (1+ idx)))
               do (labels ((sink-then ()
                             (%ast-wrap-bindings
                              outer-bindings
                              (list (make-ast-if :cond (ast-if-cond if-node)
                                                :then (make-ast-let :bindings (list binding)
                                                                    :body then-forms)
                                                :else (ast-if-else if-node)))))
                          (sink-else ()
                            (%ast-wrap-bindings
                             outer-bindings
                             (list (make-ast-if :cond (ast-if-cond if-node)
                                                :then (ast-if-then if-node)
                                                :else (make-ast-let :bindings (list binding)
                                                                    :body else-forms))))))
                   (when (not (and then-uses else-uses))
                     (cond
                       ((%ast-make-array-call-p expr)
                        (cond
                          ((and then-uses (not else-uses)
                                (%array-binding-static-access-p then-forms name nil))
                           (return-from %ast-let-sink-if-candidate (sink-then)))
                          ((and else-uses (not then-uses)
                                (%array-binding-static-access-p else-forms name nil))
                           (return-from %ast-let-sink-if-candidate (sink-else)))))
                       ((typep expr 'ast-make-instance)
                        (let ((slot-names (loop for (key . _value-ast) in (ast-make-instance-initargs expr)
                                                collect (symbol-name key))))
                          (cond
                            ((and then-uses (not else-uses)
                                  (%instance-binding-static-slot-only-p then-forms name slot-names))
                             (return-from %ast-let-sink-if-candidate (sink-then)))
                            ((and else-uses (not then-uses)
                                  (%instance-binding-static-slot-only-p else-forms name slot-names))
                             (return-from %ast-let-sink-if-candidate (sink-else))))))
                       ((%ast-cons-call-p expr)
                        (cond
                          ((and then-uses (not else-uses)
                                (not (binding-escapes-in-body-p then-forms name
                                                                :safe-consumers '("CAR" "CDR"))))
                           (return-from %ast-let-sink-if-candidate (sink-then)))
                          ((and else-uses (not then-uses)
                                (not (binding-escapes-in-body-p else-forms name
                                                                :safe-consumers '("CAR" "CDR"))))
                           (return-from %ast-let-sink-if-candidate (sink-else))))))))
               finally (return nil))))))

;;; ── Binding noescape walkers ─────────────────────────────────────────────
;;;
;;; All three noescape checks share ~35 identical typecase arms.  The macro
;;; below generates the shared skeleton; callers supply:
;;;   SHADOW-LET  — whether a let binding that re-binds BINDING-NAME is safe
;;;   SHADOW-MVB  — whether mvb vars that re-bind BINDING-NAME are safe
;;;   TERMINAL-CASES — unique typecase arms (ast-call / ast-slot-value / etc.)

(defmacro %define-binding-walker (name args docstring
                                  (&key (shadow-let t) (shadow-mvb t))
                                  &body terminal-cases)
  "Generate a binding-walker defun.
The first two elements of ARGS are (body-forms binding-name); remaining
elements are extra parameters available in TERMINAL-CASES."
  (let ((body-forms-var  (first  args))
        (binding-name-var (second args)))
    `(defun ,name ,args
       ,docstring
       (labels ((okp (node)
                  (typecase node
                    (ast-var (not (eq (ast-var-name node) ,binding-name-var)))
                    ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
                    (ast-progn  (every #'okp (ast-progn-forms node)))
                    (ast-block  (every #'okp (ast-block-body node)))
                    (ast-if     (and (okp (ast-if-cond node))
                                     (okp (ast-if-then node))
                                     (okp (ast-if-else node))))
                    (ast-let
                     ,(if shadow-let
                          `(let ((bound-names (mapcar #'car (ast-let-bindings node))))
                             (and (every #'okp (mapcar #'cdr (ast-let-bindings node)))
                                  (or (member ,binding-name-var bound-names :test #'eq)
                                      (every #'okp (ast-let-body node)))))
                          `(and (every #'okp (mapcar #'cdr (ast-let-bindings node)))
                                (every #'okp (ast-let-body node)))))
                    (ast-setq          (okp (ast-setq-value node)))
                    (ast-return-from   (okp (ast-return-from-value node)))
                    (ast-the           (okp (ast-the-value node)))
                    (ast-values        (every #'okp (ast-values-forms node)))
                    (ast-catch         (and (okp (ast-catch-tag node))
                                            (every #'okp (ast-catch-body node))))
                    (ast-throw         (and (okp (ast-throw-tag node))
                                            (okp (ast-throw-value node))))
                    (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                             (every #'okp (ast-unwind-cleanup node))))
                    (ast-handler-case  (and (okp (ast-handler-case-form node))
                                            (every (lambda (clause)
                                                     (every #'okp (cddr clause)))
                                                   (ast-handler-case-clauses node))))
                    (ast-multiple-value-call
                     (and (okp (ast-mv-call-func node))
                          (every #'okp (ast-mv-call-args node))))
                    (ast-multiple-value-prog1
                     (and (okp (ast-mv-prog1-first node))
                          (every #'okp (ast-mv-prog1-forms node))))
                    (ast-multiple-value-bind
                     ,(if shadow-mvb
                          `(and (okp (ast-mvb-values-form node))
                                (or (member ,binding-name-var (ast-mvb-vars node) :test #'eq)
                                    (every #'okp (ast-mvb-body node))))
                          `(and (okp (ast-mvb-values-form node))
                                (every #'okp (ast-mvb-body node)))))
                    (ast-apply (and (okp (ast-apply-func node))
                                   (every #'okp (ast-apply-args node))))
                    (ast-lambda
                     (and (not (find ,binding-name-var
                                     (find-captured-in-children (ast-lambda-body node)
                                                                (list ,binding-name-var))))
                          ,(if shadow-let
                               `(or (member ,binding-name-var (%ast-lambda-bound-names node) :test #'eq)
                                    (every #'okp (ast-lambda-body node)))
                               `(every #'okp (ast-lambda-body node)))))
                    (ast-defun
                     (and (not (find ,binding-name-var
                                     (find-captured-in-children (ast-defun-body node)
                                                                (list ,binding-name-var))))
                          ,(if shadow-let
                               `(or (member ,binding-name-var (ast-defun-params node) :test #'eq)
                                    (every #'okp (ast-defun-body node)))
                               `(every #'okp (ast-defun-body node)))))
                    (ast-defmethod
                     (and (not (find ,binding-name-var
                                     (find-captured-in-children (ast-defmethod-body node)
                                                                (list ,binding-name-var))))
                          ,(if shadow-let
                               `(or (member ,binding-name-var (ast-defmethod-params node) :test #'eq)
                                    (every #'okp (ast-defmethod-body node)))
                               `(every #'okp (ast-defmethod-body node)))))
                    (ast-local-fns
                     (and (not (find ,binding-name-var
                                     (find-captured-in-children (ast-local-fns-body node)
                                                                (list ,binding-name-var))))
                          (every #'okp (ast-local-fns-body node))))
                    ,@terminal-cases
                    (t (every #'okp (ast-children node))))))
         (and (listp ,body-forms-var) (every #'okp ,body-forms-var))))))

;;; Array noescape: binding may only appear as the array arg to ARRAY-LENGTH, ASET, or AREF.
(%define-binding-walker %array-binding-static-access-p
  (body-forms binding-name size)
  "True iff BINDING-NAME in BODY-FORMS is used only via array operations (aref/aset/array-length)."
  (:shadow-let t :shadow-mvb t)
  (ast-call
   (let ((func (ast-call-func node))
         (args (ast-call-args node)))
     (flet ((array-fn-named-p (name)
              (or (and (symbolp func) (string= (symbol-name func) name))
                  (and (typep func 'ast-var) (string= (symbol-name (ast-var-name func)) name))))
            (first-arg-is-binding-p ()
              (and (typep (first args) 'ast-var)
                   (eq (ast-var-name (first args)) binding-name))))
       (cond
         ((and (array-fn-named-p "ARRAY-LENGTH") (= (length args) 1) (first-arg-is-binding-p)) t)
         ((and (array-fn-named-p "ASET") (= (length args) 3) (first-arg-is-binding-p)
               (okp (second args)) (okp (third args))) t)
         ((and (or (eq func 'aref)
                   (and (typep func 'ast-var) (eq (ast-var-name func) 'aref)))
               (= (length args) 2) (first-arg-is-binding-p) (okp (second args))) t)
         (t (and (if (typep func 'ast-node) (okp func) t)
                 (every #'okp args))))))))

;;; Instance noescape: binding may only appear as the object arg to slot-value / set-slot-value.
(%define-binding-walker %instance-binding-static-slot-only-p
  (body-forms binding-name allowed-slot-names)
  "True iff BINDING-NAME in BODY-FORMS is used only as the object in slot-value forms
for statically materialized slots in ALLOWED-SLOT-NAMES."
  (:shadow-let t :shadow-mvb t)
  (ast-slot-value
   (and (typep (ast-slot-value-object node) 'ast-var)
        (eq (ast-var-name (ast-slot-value-object node)) binding-name)
        (member (symbol-name (ast-slot-value-slot node)) allowed-slot-names :test #'string=)))
  (ast-set-slot-value
   (and (typep (ast-set-slot-value-object node) 'ast-var)
        (eq (ast-var-name (ast-set-slot-value-object node)) binding-name)
        (member (symbol-name (ast-set-slot-value-slot node)) allowed-slot-names :test #'string=)
        (okp (ast-set-slot-value-value node)))))

;;; Closure noescape: binding may only appear as the called function with the correct arity.
;;; No shadowing — if the name is re-bound in a nested let, we conservatively bail.
(%define-binding-walker %closure-binding-direct-call-only-p
  (body-forms binding-name arity)
  "True iff BINDING-NAME in BODY-FORMS is used only in direct calls with matching ARITY."
  (:shadow-let nil :shadow-mvb nil)
  (ast-call
   (let ((func (ast-call-func node))
         (args (ast-call-args node)))
     (cond
       ((and (typep func 'ast-var)
             (eq (ast-var-name func) binding-name)
             (= (length args) arity)
             (every #'okp args))
        t)
       (t (and (if (typep func 'ast-node) (okp func) t)
               (every #'okp args)))))))

;;; (Classification predicates, binding emitters, and compile-ast (ast-let)
;;;  are in codegen-core-let-emit.lisp which loads after this file.)
