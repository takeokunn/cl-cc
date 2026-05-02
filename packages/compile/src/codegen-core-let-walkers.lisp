(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Let-Binding Walker Layer
;;;
;;; Contains:
;;;   - %define-binding-walker macro
;;;   - eval-when helper builders for walker clauses
;;;   - generated walkers:
;;;       %array-binding-static-access-p
;;;       %instance-binding-static-slot-only-p
;;;       %closure-binding-direct-call-only-p
;;;
;;; Analysis predicates and sink-if detection live in codegen-core-let.lisp.
;;; Let-binding emitters live in codegen-core-let-emit.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

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
  "Generate a binding-walker defun."
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

(%define-binding-walker %array-binding-static-access-p
  (body-forms binding-name size)
  "True iff BINDING-NAME in BODY-FORMS is used only via array operations (aref/aset/array-length)."
  (:shadow-let t :shadow-mvb t)
  (ast-call
   (let* ((args            (ast-call-args node))
          (first-arg-match (and (typep (first args) 'ast-var)
                                (eq (ast-var-name (first args)) binding-name))))
     (cond
       ((and (%ast-call-named-p node "ARRAY-LENGTH" 1) first-arg-match) t)
       ((and (%ast-call-named-p node "ASET" 3)         first-arg-match
             (okp (second args)) (okp (third args))) t)
       ((and (%ast-call-named-p node "AREF" 2)         first-arg-match
             (okp (second args))) t)
       (t (let ((func (ast-call-func node)))
            (and (if (typep func 'ast-node) (okp func) t)
                 (every #'okp args))))))))

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
