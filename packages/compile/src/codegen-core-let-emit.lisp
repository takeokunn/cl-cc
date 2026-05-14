(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Let-Binding Classification Predicates, Emitters, and compile-ast
;;;
;;; Contains:
;;;   Five classification predicates (%let-binding-special-p, %let-noescape-*)
;;;   Five binding emitters (%emit-let-*)
;;;   compile-ast (ast-let) dispatch method — the only public entry point
;;;
;;; AST predicate helpers, sink-if analysis, and the %define-binding-walker
;;; macro + generated walkers are in codegen-core-let.lisp (loads before).
;;;
;;; Load order: after codegen-core-let.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Let-binding classification predicates ────────────────────────────────
;;;
;;; Prolog-style or-chain: compile-ast (ast-let) tries each in order;
;;; first match wins, falling back to normal heap-allocated binding.

(defun %let-binding-special-p (name ctx)
  "T when NAME is a dynamic special (earmuffs + registered as global)."
  (and (gethash name (ctx-global-variables ctx))
       (let ((s (symbol-name name)))
         (and (> (length s) 1)
              (char= (char s 0) #\*)
              (char= (char s (1- (length s))) #\*)))))

(defun %let-dynamic-extent-declared-p (name declarations)
  "T when NAME is declared dynamic-extent in DECLARATIONS."
  (dynamic-extent-declared-p declarations name))

(defun %let-dynamic-extent-direct-use-p (body-forms binding-name call-ok-p)
  "T when BODY-FORMS use BINDING-NAME only in CALL-OK-P approved call shapes,
while still permitting those shapes through captured lambdas/local forms."
  (labels ((ok-list (forms)
             (and (listp forms) (every #'okp forms)))
           (shadowed-body-ok-p (bound-names body)
             (or (member binding-name bound-names :test #'eq)
                 (ok-list body)))
           (okp (node)
             (typecase node
               (ast-var (not (eq (ast-var-name node) binding-name)))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
               (ast-progn (ok-list (ast-progn-forms node)))
               (ast-block (ok-list (ast-block-body node)))
               (ast-if (and (okp (ast-if-cond node))
                            (okp (ast-if-then node))
                            (okp (ast-if-else node))))
               (ast-let
                (let ((bound-names (mapcar #'car (ast-let-bindings node))))
                  (and (ok-list (mapcar #'cdr (ast-let-bindings node)))
                       (or (member binding-name bound-names :test #'eq)
                           (ok-list (ast-let-body node))))))
               (ast-setq (okp (ast-setq-value node)))
               (ast-return-from (okp (ast-return-from-value node)))
               (ast-the (okp (ast-the-value node)))
               (ast-values (ok-list (ast-values-forms node)))
               (ast-catch (and (okp (ast-catch-tag node))
                               (ok-list (ast-catch-body node))))
               (ast-throw (and (okp (ast-throw-tag node))
                               (okp (ast-throw-value node))))
               (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                        (ok-list (ast-unwind-cleanup node))))
               (ast-handler-case
                (and (okp (ast-handler-case-form node))
                     (every (lambda (clause) (every #'okp (cddr clause)))
                            (ast-handler-case-clauses node))))
               (ast-multiple-value-call
                (and (okp (ast-mv-call-func node))
                     (ok-list (ast-mv-call-args node))))
               (ast-multiple-value-prog1
                (and (okp (ast-mv-prog1-first node))
                     (ok-list (ast-mv-prog1-forms node))))
               (ast-multiple-value-bind
                (and (okp (ast-mvb-values-form node))
                     (or (member binding-name (ast-mvb-vars node) :test #'eq)
                         (ok-list (ast-mvb-body node)))))
               (ast-apply (and (okp (ast-apply-func node))
                               (ok-list (ast-apply-args node))))
               (ast-lambda (shadowed-body-ok-p (%ast-lambda-bound-names node)
                                               (ast-lambda-body node)))
               (ast-defun (shadowed-body-ok-p (ast-defun-params node)
                                              (ast-defun-body node)))
               (ast-defmethod (shadowed-body-ok-p (ast-defmethod-params node)
                                                  (ast-defmethod-body node)))
               (ast-local-fns
                (if (member binding-name (ast-bound-names node) :test #'eq)
                    t
                    (ok-list (ast-children node))))
               (ast-call (funcall call-ok-p node #'okp))
               (t (ok-list (ast-children node))))))
    (ok-list body-forms)))

(defun %let-dynamic-extent-cons-direct-consumers-p (body-forms binding-name)
  "T when BODY-FORMS only use BINDING-NAME as the direct argument to CAR/CDR,
while still permitting those direct uses through captured lambdas/local forms."
  (%let-dynamic-extent-direct-use-p
   body-forms binding-name
   (lambda (node okp)
     (let* ((args (ast-call-args node))
            (direct-arg-match
              (and (= (length args) 1)
                   (typep (first args) 'ast-var)
                   (eq (ast-var-name (first args)) binding-name))))
       (cond
         ((and (%ast-call-named-p node "CAR" 1) direct-arg-match) t)
         ((and (%ast-call-named-p node "CDR" 1) direct-arg-match) t)
         (t
          (let ((func (ast-call-func node)))
            (and (if (typep func 'ast-node) (funcall okp func) t)
                 (every okp args)))))))))

(defun %let-dynamic-extent-array-direct-access-p (body-forms binding-name)
  "T when BODY-FORMS only use BINDING-NAME as the direct array operand of
ARRAY-LENGTH/AREF/ASET, while still permitting those direct uses through
captured lambdas/local forms."
  (%let-dynamic-extent-direct-use-p
   body-forms binding-name
   (lambda (node okp)
     (let* ((args (ast-call-args node))
            (first-arg-match
              (and (consp args)
                   (typep (first args) 'ast-var)
                   (eq (ast-var-name (first args)) binding-name))))
       (cond
         ((and (%ast-call-named-p node "ARRAY-LENGTH" 1) first-arg-match) t)
         ((and (%ast-call-named-p node "AREF" 2) first-arg-match
               (funcall okp (second args)))
          t)
         ((and (%ast-call-named-p node "ASET" 3) first-arg-match
               (funcall okp (second args))
               (funcall okp (third args)))
          t)
         (t
          (let ((func (ast-call-func node)))
            (and (if (typep func 'ast-node) (funcall okp func) t)
                 (every okp args)))))))))

(defun %let-dynamic-extent-closure-direct-call-only-p (body-forms binding-name arity)
  "T when BODY-FORMS only use BINDING-NAME via direct calls of ARITY, even across captures."
  (labels ((ok-list (forms)
             (and (listp forms) (every #'okp forms)))
           (shadowed-body-ok-p (bound-names body)
             (or (member binding-name bound-names :test #'eq)
                 (ok-list body)))
           (okp (node)
             (typecase node
               (ast-var (not (eq (ast-var-name node) binding-name)))
               ((or ast-int ast-quote ast-function ast-go ast-hole ast-defgeneric) t)
               (ast-progn (ok-list (ast-progn-forms node)))
               (ast-block (ok-list (ast-block-body node)))
               (ast-if (and (okp (ast-if-cond node))
                            (okp (ast-if-then node))
                            (okp (ast-if-else node))))
               (ast-let
                (let ((bound-names (mapcar #'car (ast-let-bindings node))))
                  (and (ok-list (mapcar #'cdr (ast-let-bindings node)))
                       (or (member binding-name bound-names :test #'eq)
                           (ok-list (ast-let-body node))))))
               (ast-setq (okp (ast-setq-value node)))
               (ast-return-from (okp (ast-return-from-value node)))
               (ast-the (okp (ast-the-value node)))
               (ast-values (ok-list (ast-values-forms node)))
               (ast-catch (and (okp (ast-catch-tag node))
                               (ok-list (ast-catch-body node))))
               (ast-throw (and (okp (ast-throw-tag node))
                               (okp (ast-throw-value node))))
               (ast-unwind-protect (and (okp (ast-unwind-protected node))
                                        (ok-list (ast-unwind-cleanup node))))
               (ast-handler-case
                (and (okp (ast-handler-case-form node))
                     (every (lambda (clause) (every #'okp (cddr clause)))
                            (ast-handler-case-clauses node))))
               (ast-multiple-value-call
                (and (okp (ast-mv-call-func node))
                     (ok-list (ast-mv-call-args node))))
               (ast-multiple-value-prog1
                (and (okp (ast-mv-prog1-first node))
                     (ok-list (ast-mv-prog1-forms node))))
               (ast-multiple-value-bind
                (and (okp (ast-mvb-values-form node))
                     (or (member binding-name (ast-mvb-vars node) :test #'eq)
                         (ok-list (ast-mvb-body node)))))
               (ast-apply (and (okp (ast-apply-func node))
                               (ok-list (ast-apply-args node))))
               (ast-lambda (shadowed-body-ok-p (%ast-lambda-bound-names node)
                                               (ast-lambda-body node)))
               (ast-defun (shadowed-body-ok-p (ast-defun-params node)
                                              (ast-defun-body node)))
               (ast-defmethod (shadowed-body-ok-p (ast-defmethod-params node)
                                                  (ast-defmethod-body node)))
               (ast-local-fns
                (if (member binding-name (ast-bound-names node) :test #'eq)
                    t
                    (ok-list (ast-children node))))
               (ast-call
                (let ((func (ast-call-func node))
                      (args (ast-call-args node)))
                  (cond
                    ((and (typep func 'ast-var)
                          (eq (ast-var-name func) binding-name)
                          (= (length args) arity)
                          (ok-list args))
                     t)
                    (t (and (if (typep func 'ast-node) (okp func) t)
                            (ok-list args))))))
               (t (ok-list (ast-children node))))))
    (ok-list body-forms)))

(defun %let-noescape-closure (name expr declarations mutated captured body-forms)
  "Return EXPR if the binding can be inlined as a noescape closure, else NIL."
  (let ((dynamic-extent-p (%let-dynamic-extent-declared-p name declarations)))
    (and (typep expr 'ast-lambda)
         (null (ast-lambda-optional-params expr))
         (null (ast-lambda-rest-param expr))
         (null (ast-lambda-key-params expr))
         (not (%member-eq-p name mutated))
         (or dynamic-extent-p
             (not (%member-eq-p name captured)))
         (if dynamic-extent-p
             (%let-dynamic-extent-closure-direct-call-only-p
              body-forms name (length (ast-lambda-params expr)))
             (%closure-binding-direct-call-only-p
              body-forms name (length (ast-lambda-params expr))))
         expr)))

(defun %let-noescape-instance-slots (name expr mutated captured body-forms ctx)
  "Return compiled slot alist when the binding can skip heap allocation, else NIL."
  (if (and (typep expr 'ast-make-instance)
           (not (%member-eq-p name mutated))
           (not (%member-eq-p name captured)))
      (let ((slot-names (loop for entry in (ast-make-instance-initargs expr)
                              collect (symbol-name (car entry)))))
        (when (%instance-binding-static-slot-only-p body-forms name slot-names)
          (loop for entry in (ast-make-instance-initargs expr)
                collect (cons (symbol-name (car entry))
                              (compile-ast (cdr entry) ctx)))))
      nil))

(defun %let-noescape-array-size (name expr declarations mutated captured body-forms)
  "Return the array size integer when the binding can skip heap allocation, else NIL."
  (let ((dynamic-extent-p (%let-dynamic-extent-declared-p name declarations)))
    (and (or (%ast-make-array-noescape-call-p expr)
             (and (typep expr 'ast-the)
                  (%ast-make-array-noescape-call-p (ast-the-value expr))))
         (not (%member-eq-p name mutated))
         (or dynamic-extent-p
             (not (%member-eq-p name captured)))
         (let* ((call-node (if (typep expr 'ast-the) (ast-the-value expr) expr))
                (size (ast-int-value (first (ast-call-args call-node)))))
             (and (if dynamic-extent-p
                     (%let-dynamic-extent-array-direct-access-p body-forms name)
                     (%array-binding-static-access-p body-forms name size))
                 size)))))

(defun %let-noescape-cons-p (name expr declarations mutated captured body-forms)
  "T when the cons binding never escapes (only CAR/CDR consumers)."
  (let ((dynamic-extent-p (%let-dynamic-extent-declared-p name declarations)))
    (and (%ast-cons-call-p expr)
         (not (%member-eq-p name mutated))
         (or dynamic-extent-p
             (not (%member-eq-p name captured)))
         (if dynamic-extent-p
             (%let-dynamic-extent-cons-direct-consumers-p body-forms name)
             (not (binding-escapes-in-body-p body-forms name
                                             :safe-consumers '("CAR" "CDR")))))))
