(in-package :cl-cc/compile)
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

(defun %ast-call-named-p (node fn-name nargs)
  "True if NODE is an ast-call to a function named FN-NAME with exactly NARGS arguments.
FN-NAME is compared case-insensitively via SYMBOL-NAME so both symbol and ast-var funcs match."
  (and (typep node 'ast-call)
       (= (length (ast-call-args node)) nargs)
       (let ((func (ast-call-func node)))
         (flet ((name= (sym) (string= (symbol-name sym) fn-name)))
           (or (and (symbolp func)             (name= func))
               (and (typep func 'ast-var) (name= (ast-var-name func))))))))

(defun %ast-cons-call-p (node)
  (%ast-call-named-p node "CONS" 2))

(defun %ast-make-array-call-p (node)
  (%ast-call-named-p node "MAKE-ARRAY" 1))

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

(defun %sink-if-branch-body (if-node branch)
  (ecase branch
    (:then (%ast-as-body-forms (ast-if-then if-node)))
    (:else (%ast-as-body-forms (ast-if-else if-node)))))

(defun %sink-if-wrap-branch (if-node branch binding branch-forms outer-bindings)
  (let ((sunken (make-ast-let :bindings (list binding) :body branch-forms)))
    (%ast-wrap-bindings
     outer-bindings
     (list (make-ast-if :cond (ast-if-cond if-node)
                        :then (if (eq branch :then) sunken (ast-if-then if-node))
                        :else (if (eq branch :else) sunken (ast-if-else if-node)))))))

(defun %sink-if-branch-uses-p (then-uses else-uses branch)
  (ecase branch
    (:then (and then-uses (not else-uses)))
    (:else (and else-uses (not then-uses)))))

(defun %sink-if-build-branch (if-node binding branch outer-bindings)
  (%sink-if-wrap-branch if-node
                        branch
                        binding
                        (%sink-if-branch-body if-node branch)
                        outer-bindings))

(defun %sink-if-instance-slot-names (expr)
  (loop for initarg in (ast-make-instance-initargs expr)
        collect (symbol-name (car initarg))))

(defun %sink-if-array-candidate-p (expr if-node branch name then-uses else-uses)
  (and (%ast-make-array-call-p expr)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (%array-binding-static-access-p (%sink-if-branch-body if-node branch) name nil)))

(defun %sink-if-instance-candidate-p (expr if-node branch name then-uses else-uses)
  (and (typep expr 'ast-make-instance)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (%instance-binding-static-slot-only-p (%sink-if-branch-body if-node branch)
                                             name
                                             (%sink-if-instance-slot-names expr))))

(defun %sink-if-cons-candidate-p (expr if-node branch name then-uses else-uses)
  (and (%ast-cons-call-p expr)
       (%sink-if-branch-uses-p then-uses else-uses branch)
       (not (binding-escapes-in-body-p (%sink-if-branch-body if-node branch)
                                       name
                                       :safe-consumers '("CAR" "CDR")))))

;;; Data table: predicates tried in order; each is tried for :then then :else.
;;; Separating predicates from the branch axis eliminates the 3×2 Cartesian enumeration.
(defparameter *sink-if-candidate-predicates*
  (list #'%sink-if-array-candidate-p
        #'%sink-if-instance-candidate-p
        #'%sink-if-cons-candidate-p)
  "Predicates for sinking a let binding into an if branch.")

(defun %sink-if-binding-candidate (if-node binding outer-bindings then-uses else-uses)
  (let ((name (car binding))
        (expr (cdr binding)))
    (loop for pred-fn in *sink-if-candidate-predicates*
          thereis (loop for branch in '(:then :else)
                        when (funcall pred-fn expr if-node branch name then-uses else-uses)
                          return (%sink-if-build-branch if-node binding branch outer-bindings)))))

;;; ── Sink-if candidate analysis ───────────────────────────────────────────
;;;
;;; When a let has exactly one binding and exactly one body form that is an
;;; IF, and the binding is used in only one branch, we can sink the binding
;;; into that branch to reduce unnecessary allocation.

(defun %ast-let-sink-if-candidate (node)
  (let ((bindings (ast-let-bindings node))
        (body     (ast-let-body node)))
    (unless (and (= (length body) 1) (typep (first body) 'ast-if))
      (return-from %ast-let-sink-if-candidate nil))
    (let* ((if-node    (first body))
           (then-forms (%ast-as-body-forms (ast-if-then if-node)))
           (else-forms (%ast-as-body-forms (ast-if-else if-node))))
      (loop for binding in bindings
            for idx from 0
            for name = (car binding)
            for then-uses = (%binding-mentioned-in-body-p then-forms name)
            for else-uses = (%binding-mentioned-in-body-p else-forms name)
            for outer-bindings = (append (subseq bindings 0 idx)
                                         (subseq bindings (1+ idx)))
            for candidate = (unless (and then-uses else-uses)
                              (%sink-if-binding-candidate if-node binding outer-bindings
                                                          then-uses else-uses))
            when candidate
              return candidate))))

;;; ── Binding noescape walkers ─────────────────────────────────────────────
;;;
;;; All three noescape checks share ~35 identical typecase arms.  The macro
;;; below generates the shared skeleton; callers supply:
;;;   SHADOW-LET  — whether a let binding that re-binds BINDING-NAME is safe
;;;   SHADOW-MVB  — whether mvb vars that re-bind BINDING-NAME are safe
;;;   TERMINAL-CASES — unique typecase arms (ast-call / ast-slot-value / etc.)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %binding-walker-every-okp (form)
    (list 'every '(function okp) form))

  (defun %binding-walker-captured-form (binding-name-var body-form)
    (list 'find binding-name-var
          (list 'find-captured-in-children body-form (list 'list binding-name-var))))

  (defun %binding-walker-shadowed-body-form (binding-name-var shadow-p bound-form body-form)
    (if shadow-p
        (list 'or
              (list 'member binding-name-var bound-form :test '(function eq))
              (%binding-walker-every-okp body-form))
        (%binding-walker-every-okp body-form)))

  (defun %binding-walker-let-clause (binding-name-var shadow-let)
    (if shadow-let
        (list 'let '((bound-names (mapcar (function car) (ast-let-bindings node))))
              (list 'and
                    (%binding-walker-every-okp '(mapcar (function cdr) (ast-let-bindings node)))
                    (list 'or
                          (list 'member binding-name-var 'bound-names :test '(function eq))
                          (%binding-walker-every-okp '(ast-let-body node)))))
        (list 'and
              (%binding-walker-every-okp '(mapcar (function cdr) (ast-let-bindings node)))
              (%binding-walker-every-okp '(ast-let-body node)))))

  (defun %binding-walker-mvb-clause (binding-name-var shadow-mvb)
    (if shadow-mvb
        (list 'and
              '(okp (ast-mvb-values-form node))
              (list 'or
                    (list 'member binding-name-var '(ast-mvb-vars node) :test '(function eq))
                    (%binding-walker-every-okp '(ast-mvb-body node))))
        (list 'and
              '(okp (ast-mvb-values-form node))
              (%binding-walker-every-okp '(ast-mvb-body node)))))

  (defun %binding-walker-noncapturing-clause (binding-name-var shadow-let bound-form body-form)
    (list 'and
          (list 'not (%binding-walker-captured-form binding-name-var body-form))
          (%binding-walker-shadowed-body-form binding-name-var shadow-let bound-form body-form)))

  (defun %binding-walker-base-clauses (binding-name-var shadow-let shadow-mvb terminal-cases)
    (append
     (list
      (list 'ast-var (list 'not (list 'eq '(ast-var-name node) binding-name-var)))
      (list '(or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
      (list 'ast-progn (%binding-walker-every-okp '(ast-progn-forms node)))
      (list 'ast-block (%binding-walker-every-okp '(ast-block-body node)))
      (list 'ast-if (list 'and '(okp (ast-if-cond node))
                          '(okp (ast-if-then node))
                          '(okp (ast-if-else node))))
      (list 'ast-let (%binding-walker-let-clause binding-name-var shadow-let))
      (list 'ast-setq '(okp (ast-setq-value node)))
      (list 'ast-return-from '(okp (ast-return-from-value node)))
      (list 'ast-the '(okp (ast-the-value node)))
      (list 'ast-values (%binding-walker-every-okp '(ast-values-forms node)))
      (list 'ast-catch (list 'and '(okp (ast-catch-tag node))
                             (%binding-walker-every-okp '(ast-catch-body node))))
      (list 'ast-throw (list 'and '(okp (ast-throw-tag node))
                             '(okp (ast-throw-value node))))
      (list 'ast-unwind-protect (list 'and '(okp (ast-unwind-protected node))
                                      (%binding-walker-every-okp '(ast-unwind-cleanup node))))
      (list 'ast-handler-case
            (list 'and
                  '(okp (ast-handler-case-form node))
                  (list 'every '(function (lambda (clause)
                                            (every #'okp (cddr clause))))
                        '(ast-handler-case-clauses node))))
      (list 'ast-multiple-value-call
            (list 'and '(okp (ast-mv-call-func node))
                  (%binding-walker-every-okp '(ast-mv-call-args node))))
      (list 'ast-multiple-value-prog1
            (list 'and '(okp (ast-mv-prog1-first node))
                  (%binding-walker-every-okp '(ast-mv-prog1-forms node))))
      (list 'ast-multiple-value-bind (%binding-walker-mvb-clause binding-name-var shadow-mvb))
      (list 'ast-apply
            (list 'and '(okp (ast-apply-func node))
                  (%binding-walker-every-okp '(ast-apply-args node))))
      (list 'ast-lambda
            (%binding-walker-noncapturing-clause binding-name-var shadow-let
                                                 '(%ast-lambda-bound-names node)
                                                 '(ast-lambda-body node)))
      (list 'ast-defun
            (%binding-walker-noncapturing-clause binding-name-var shadow-let
                                                 '(ast-defun-params node)
                                                 '(ast-defun-body node)))
      (list 'ast-defmethod
            (%binding-walker-noncapturing-clause binding-name-var shadow-let
                                                 '(ast-defmethod-params node)
                                                 '(ast-defmethod-body node)))
      (list 'ast-local-fns
            (list 'and
                  (list 'not (%binding-walker-captured-form binding-name-var '(ast-local-fns-body node)))
                  (%binding-walker-every-okp '(ast-local-fns-body node)))))
     terminal-cases
     (list (list 't (%binding-walker-every-okp '(ast-children node)))))))

(defmacro %define-binding-walker (name args docstring
                                  (&key (shadow-let t) (shadow-mvb t))
                                  &body terminal-cases)
  "Generate a binding-walker defun.
The first two elements of ARGS are (body-forms binding-name); remaining
elements are extra parameters available in TERMINAL-CASES."
  (let ((body-forms-var (first args))
        (binding-name-var (second args)))
    (list 'defun name args docstring
          (list 'labels
                (list (list 'okp '(node)
                            (cons 'typecase
                                  (cons 'node
                                        (%binding-walker-base-clauses
                                         binding-name-var shadow-let shadow-mvb terminal-cases)))))
                (list 'and (list 'listp body-forms-var)
                      (%binding-walker-every-okp body-forms-var))))))

;;; Array noescape: binding may only appear as the array arg to ARRAY-LENGTH, ASET, or AREF.
(%define-binding-walker %array-binding-static-access-p
  (body-forms binding-name size)
  "True iff BINDING-NAME in BODY-FORMS is used only via array operations (aref/aset/array-length)."
  (:shadow-let t :shadow-mvb t)
  (ast-call
   (let ((args (ast-call-args node)))
     (flet ((first-arg-is-binding-p ()
              (and (typep (first args) 'ast-var)
                   (eq (ast-var-name (first args)) binding-name)))
            (func-named-p (name n) (%ast-call-named-p node name n)))
       (cond
         ((and (func-named-p "ARRAY-LENGTH" 1) (first-arg-is-binding-p)) t)
         ((and (func-named-p "ASET" 3)         (first-arg-is-binding-p)
               (okp (second args)) (okp (third args))) t)
         ((and (func-named-p "AREF" 2)         (first-arg-is-binding-p)
               (okp (second args))) t)
         (t (let ((func (ast-call-func node)))
              (and (if (typep func 'ast-node) (okp func) t)
                   (every #'okp args)))))))))

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
