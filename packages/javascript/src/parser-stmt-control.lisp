;;;; packages/javascript/src/parser-stmt-control.lisp — JS Control-Flow Statement Parser
;;;;
;;;; Contains: for-statement (C-style, for-in, for-of), switch/case/default,
;;;; break, continue, return, throw, try/catch/finally, debugger, using
;;;; declaration, and the main statement dispatcher (js-parse-stmt / js-parse-stmt-list).
;;;;
;;;; Load order: after parser-stmt.lisp (loop/if/for lowering helpers,
;;;; *js-stmt-parsers* variable, and early statement registrations).

(in-package :cl-cc/javascript)

;;; ─── For Statement ───────────────────────────────────────────────────────────
;;;
;;; Handles: for(init;cond;update)body, for(var x in obj)body,
;;;          for(var x of iter)body, for await(var x of asyncIter)body.

(defun %js-lower-for-c-style (current init-ast)
  "Lower a C-style for(init;cond;update){body} loop from CURRENT (at ;).
INIT-AST is the already-parsed init form (or NIL for no-init).
Returns (values ast rest)."
  (let* ((rest2 (%js-consume-expected :T-SEMI current))
         (cond-expr (if (eq (js-peek-type rest2) :T-SEMI)
                        (make-ast-quote :value t)
                        (multiple-value-bind (e r) (js-parse-expr rest2)
                          (setf rest2 r) e)))
         (rest3 (%js-consume-expected :T-SEMI rest2))
         (update-expr (if (eq (js-peek-type rest3) :T-RPAREN)
                          (make-ast-quote :value nil)
                          (multiple-value-bind (e r) (js-parse-expr rest3)
                            (setf rest3 r) e)))
         (rest4 (%js-consume-expected :T-RPAREN rest3)))
    (let* ((loop-tag (gensym "FOR-"))
           (end-tag  (gensym "FOR-END-"))
           (*js-loop-continue-target* loop-tag)
           (*js-loop-break-target*    end-tag)
           (*js-break-targets*    (cons end-tag  *js-break-targets*))
           (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
      (multiple-value-bind (body-ast rest5) (%js-parse-stmt-body rest4)
        (let* ((body-stmts (if (ast-progn-p body-ast)
                               (ast-progn-forms body-ast)
                               (list body-ast)))
               (loop-ast (%js-lower-while-with-tags
                          (%js-truthy-call cond-expr)
                          (append body-stmts (list update-expr))
                          loop-tag end-tag)))
          (values (if init-ast
                      (make-ast-progn :forms (list init-ast loop-ast))
                      loop-ast)
                  rest5))))))

(defun %js-lower-for-of-in (binding iter-expr body-fn-name loop-tag end-tag)
  "Shared lowering for for-in and for-of loops.
ITER-EXPR is the call that produces the iteration list.
BINDING is either a plain symbol (simple) or an (:array-pattern/:object-pattern …)
destructuring pattern — in the latter case we emit full destructuring bindings inside
the loop so `for (let [a,b] of pairs)' correctly unpacks each element.
Returns a closure that accepts the parsed body-ast."
  (declare (ignore body-fn-name))
  (let ((var-sym  (%js-binding-to-sym binding))
        (iter-sym (gensym "FOR-ITER-")))
    (lambda (body-ast)
      (let* ((body-stmts (if (ast-progn-p body-ast)
                             (ast-progn-forms body-ast)
                             (list body-ast)))
             (elem-access (make-ast-call
                           :func (make-ast-var :name 'car)
                           :args (list (make-ast-var :name iter-sym))))
             ;; For destructuring patterns emit all inner bindings inside the loop.
             ;; %js-emit-destructure-bindings returns ((gensym . source) name1 name2 …).
             ;; The first binding is (gensym . (var gensym)) — a self-reference placeholder;
             ;; we replace its RHS with the actual (car iter-sym) element access.
             (loop-bindings
              (if (listp binding)
                  (multiple-value-bind (dest-bindings _)
                      (%js-emit-destructure-bindings binding (make-ast-var :name var-sym))
                    (declare (ignore _))
                    (cons (cons var-sym elem-access) (rest dest-bindings)))
                  (list (cons var-sym elem-access)))))
        (let* ((iter-advance (make-ast-setq
                               :var iter-sym
                               :value (make-ast-call
                                       :func (make-ast-var :name 'cdr)
                                       :args (list (make-ast-var :name iter-sym)))))
               (combined-body (append body-stmts (list iter-advance)))
               ;; For destructuring, build nested single-binding lets so each binding
               ;; sees all earlier ones (let* semantics).  %js-emit-destructure-bindings
               ;; returns bindings in dependency order, so folding from the inside out
               ;; is correct for any nesting depth.
               (inner-form
                (if (listp binding)
                    (let ((r combined-body))
                      (dolist (b (reverse loop-bindings) (first r))
                        (setf r (list (make-ast-let :bindings (list b) :body r)))))
                    (make-ast-let :bindings loop-bindings :body combined-body))))
          (make-ast-let
           :bindings (list (cons iter-sym iter-expr))
           :body (list (%js-lower-while-with-tags
                        (%js-truthy-call (make-ast-var :name iter-sym))
                        (list inner-form)
                        loop-tag end-tag))))))))

(defun js-parse-for-stmt (stream)
  "Handle for(init;cond;update){}, for(var x in obj){},
for(var x of iter){}, for await(var x of asyncIter){}.
Returns (values ast rest)."
  (let ((await-p nil)
        (current stream))
    (declare (ignorable await-p)) ; for-await-of currently iterates synchronously
    (when (eq (js-peek-type current) :T-AWAIT)
      (setf await-p t
            current (cdr current)))
    (setf current (%js-consume-expected :T-LPAREN current))
    (let ((init-type (js-peek-type current)))
      (cond
        ;; for (var/let/const binding in/of/; ...)
        ((member init-type '(:T-VAR :T-LET :T-CONST) :test #'eq)
         (let* ((kind (case init-type (:T-VAR :var) (:T-LET :let) (:T-CONST :const)))
                (rest (cdr current)))
           (multiple-value-bind (binding rest2) (%js-parse-binding-pattern rest)
             (let ((iter-kw (js-peek-type rest2)))
               (cond
                 ;; for (var x in obj) { }
                 ((eq iter-kw :T-IN)
                  (multiple-value-bind (obj-expr rest3) (js-parse-expr (cdr rest2))
                    (setf rest3 (%js-consume-expected :T-RPAREN rest3))
                    (let* ((loop-tag (gensym "FOR-IN-"))
                           (end-tag  (gensym "FOR-IN-END-"))
                           (*js-loop-continue-target* loop-tag)
                           (*js-loop-break-target*    end-tag)
                           (*js-break-targets*    (cons end-tag  *js-break-targets*))
                           (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
                      (multiple-value-bind (body-ast rest4) (%js-parse-stmt-body rest3)
                        (let ((lower (funcall (%js-lower-for-of-in
                                               binding
                                               (make-ast-call
                                                :func (make-ast-var :name '%js-iter-keys)
                                                :args (list obj-expr))
                                               nil loop-tag end-tag)
                                              body-ast)))
                          (setf (ast-let-declarations lower) (list kind))
                          (values lower rest4))))))
                 ;; for (var x of iter) { } / for await (var x of iter) { }
                 ((eq iter-kw :T-OF)
                  (multiple-value-bind (iter-expr rest3) (js-parse-expr (cdr rest2))
                    (setf rest3 (%js-consume-expected :T-RPAREN rest3))
                    (let* ((of-fn '%js-iter-values)
                           (loop-tag (gensym "FOR-OF-"))
                           (end-tag  (gensym "FOR-OF-END-"))
                           (*js-loop-continue-target* loop-tag)
                           (*js-loop-break-target*    end-tag)
                           (*js-break-targets*    (cons end-tag  *js-break-targets*))
                           (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
                      (multiple-value-bind (body-ast rest4) (%js-parse-stmt-body rest3)
                        (let ((lower (funcall (%js-lower-for-of-in
                                               binding
                                               (make-ast-call
                                                :func (make-ast-var :name of-fn)
                                                :args (list iter-expr))
                                               nil loop-tag end-tag)
                                              body-ast)))
                          (setf (ast-let-declarations lower) (list kind))
                          (values lower rest4))))))
                 ;; for (var x = init ; cond ; update) { }
                 (t
                  (let ((init-val (make-ast-quote :value nil))
                        (rest-at-semi rest2))
                    (when (and (eq (js-peek-type rest2) :T-OP)
                               (equal (js-peek-value rest2) "="))
                      (multiple-value-bind (e r) (js-parse-expr (cdr rest2))
                        (setf init-val e
                              rest-at-semi r)))
                    (let ((var-sym (%js-binding-to-sym binding))
                          (init-bindings nil))
                      (setf init-bindings
                            (make-ast-let
                             :bindings (list (cons var-sym init-val))
                             :declarations (list kind)
                             :body nil))
                      (multiple-value-bind (loop-ast rest6)
                          (%js-lower-for-c-style rest-at-semi nil)
                        (setf (ast-let-body init-bindings) (list loop-ast))
                        (values init-bindings rest6))))))))))
        ;; for (; cond ; update) { } — empty init
        ((eq init-type :T-SEMI)
         (%js-lower-for-c-style current nil))
        ;; for (expr ; cond ; update) { } — expression init
        (t
         (multiple-value-bind (init-expr rest2) (js-parse-expr current)
           (%js-lower-for-c-style rest2 init-expr)))))))

;;; ─── Switch Statement ────────────────────────────────────────────────────────

(defun %js-parse-switch-body (stream break-tag)
  "Parse switch case/default sections.
  Returns (values cases default-body rest) where cases is a list of
  (case-expr . body-stmts) conses and default-body is a list of stmts or NIL."
  (let ((current (%js-consume-expected :T-LBRACE stream))
        (cases nil)
        (default-body nil))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (cond
        ;; case expr:
        ((eq (js-peek-type current) :T-CASE)
         (setf current (cdr current))
         (multiple-value-bind (case-expr rest) (js-parse-expr current)
           (setf current (%js-consume-expected :T-COLON rest))
           (let ((body nil))
             (loop
               (setf current (js-skip-semis current))
               (when (or (js-at-eof-p current)
                         (eq (js-peek-type current) :T-RBRACE)
                         (eq (js-peek-type current) :T-CASE)
                         (eq (js-peek-type current) :T-DEFAULT))
                 (return))
               (let ((*js-loop-break-target* break-tag))
                 (multiple-value-bind (stmt rest2) (js-parse-stmt current)
                   (when stmt (push stmt body))
                   (setf current rest2))))
             (push (cons case-expr (nreverse body)) cases))))
        ;; default:
        ((eq (js-peek-type current) :T-DEFAULT)
         (setf current (cdr current))
         (setf current (%js-consume-expected :T-COLON current))
         (let ((body nil))
           (loop
             (setf current (js-skip-semis current))
             (when (or (js-at-eof-p current)
                       (eq (js-peek-type current) :T-RBRACE)
                       (eq (js-peek-type current) :T-CASE))
               (return))
             (let ((*js-loop-break-target* break-tag))
               (multiple-value-bind (stmt rest2) (js-parse-stmt current)
                 (when stmt (push stmt body))
                 (setf current rest2))))
           (setf default-body (nreverse body))))
        (t
         ;; Skip unexpected token
         (setf current (cdr current)))))
    (values (nreverse cases) default-body
            (%js-consume-expected :T-RBRACE current))))

(defun %js-lower-switch (switch-expr cases default-body break-tag)
  "Lower switch(expr){case x:...; default:...} to a let/tagbody dispatch form."
  (let* ((value-sym (gensym "SWITCH-VAL-"))
         (default-tag (when default-body (gensym "SWITCH-DEFAULT-")))
         (case-labels (loop repeat (length cases) collect (gensym "SWITCH-CASE-")))
         (dispatch-forms
          (append
           (loop for case in cases
                 for label in case-labels
                 collect (make-ast-if
                          :cond (make-ast-call
                                 :func (make-ast-var :name '%js-strict-eq)
                                 :args (list (make-ast-var :name value-sym)
                                             (car case)))
                          :then (make-ast-go :tag label)
                          :else (make-ast-quote :value nil)))
           (list (make-ast-go :tag (or default-tag break-tag)))))
         (case-forms
          (loop for case in cases
                for label in case-labels
                append (list* label (cdr case))))
         (default-forms
          (when default-body
            (list* default-tag default-body))))
    (make-ast-let
     :bindings (list (cons value-sym switch-expr))
     :body (list (make-ast-block :name nil
                   :body (list (%js-make-tagbody
                                (append dispatch-forms
                                        case-forms
                                        default-forms
                                        (list break-tag)))))))))

(defun js-parse-switch-stmt (stream)
  "Parse switch(expr){case x:...; default:...}.
  Lowers to ast-let + ast-if chain via %js-lower-switch.
  Returns (values ast rest)."
  (let* ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (switch-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (let* ((break-tag (gensym "SWITCH-END-"))
             (*js-loop-break-target* break-tag)
             (*js-break-targets* (cons break-tag *js-break-targets*)))
        (multiple-value-bind (cases default-body rest3)
            (%js-parse-switch-body rest2 break-tag)
          (values (%js-lower-switch switch-expr cases default-body break-tag)
                  rest3))))))


;;; Flow-control statements (break/continue/return/throw/try/debugger/using) and
;;; main statement dispatcher -> see parser-stmt-flow.lisp
