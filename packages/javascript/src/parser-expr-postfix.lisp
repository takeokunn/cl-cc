;;;; packages/javascript/src/parser-expr-postfix.lisp — Postfix, unary, template, new
;;;;
;;;; Second half of the expression parser: new/member expressions, postfix operators
;;;; (++/--, . [] () ?.),  template literals, and prefix unary operators.
;;;;
;;;; Depends on parser-expr.lisp (defines %js-call, %js-place-get-prop-p, etc.)
;;;; Load order: after parser-expr.lisp, before parser-expr-primary.lisp.

(in-package :cl-cc/javascript)

;;; ─── New Expression ──────────────────────────────────────────────────────────

(defun js-parse-new-expr (stream)
  "Parse new ClassName(args) or new.target. Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'new'
    (declare (ignore tok))
    ;; new.target
    (when (and (eq (js-peek-type rest) :T-DOT)
               (eq (js-peek-type (cdr rest)) :T-TARGET))
      (multiple-value-bind (dot-tok rest2) (js-consume rest)
        (declare (ignore dot-tok))
        (multiple-value-bind (target-tok rest3) (js-consume rest2)
          (declare (ignore target-tok))
          (return-from js-parse-new-expr
            (values (%js-call '%js-new-target) rest3)))))
    ;; new expr
    (multiple-value-bind (ctor-ast rest2) (js-parse-member-expr rest)
      ;; Optional argument list
      (if (eq (js-peek-type rest2) :T-LPAREN)
          (multiple-value-bind (args rest3) (js-parse-arguments rest2)
            (values (%js-call '%js-new ctor-ast
                              (make-ast-call :func (make-ast-var :name '%js-make-array)
                                             :args args))
                    rest3))
          (values (%js-call '%js-new ctor-ast
                            (%js-call '%js-make-array))
                  rest2)))))

(defun js-parse-member-expr (stream)
  "Parse a member expression for 'new' context (no call, only . and []).
Returns (values ast rest)."
  (multiple-value-bind (ast rest) (js-parse-primary stream)
    ;; Apply member accesses only (no calls — that would be CallExpression)
    (loop
      (let ((type (js-peek-type rest))
            (val  (js-peek-value rest)))
        (cond
          ;; obj.prop
          ((eq type :T-DOT)
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (prop-tok rest3) (js-consume rest2)
               (let* ((prop-str (js-tok-value prop-tok))
                      (key (make-ast-quote :value prop-str)))
                 (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                          :args (list ast key))
                       rest rest3)))))
          ;; obj[expr]
          ((eq type :T-LBRACKET)
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (idx-ast rest3) (js-parse-assignment-expr rest2)
               (multiple-value-bind (tok2 rest4) (js-expect :T-RBRACKET rest3)
                 (declare (ignore tok2))
                 (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                          :args (list ast idx-ast))
                       rest rest4)))))
          ;; optional chain ?.
          ((and (eq type :T-OP) (string= val "?."))
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (cond
               ;; ?.prop — if followed by ( emit optional-method-call, else optional-chain
               ((eq (js-peek-type rest2) :T-IDENT)
                (multiple-value-bind (prop-tok rest3) (js-consume rest2)
                  (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                    (if (eq (js-peek-type rest3) :T-LPAREN)
                        (multiple-value-bind (args rest4) (js-parse-arguments rest3)
                          (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-method-call)
                                                   :args (list* ast key args))
                                rest rest4))
                        (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                                 :args (list ast key))
                              rest rest3)))))
               ;; ?.[expr]
               ((eq (js-peek-type rest2) :T-LBRACKET)
                (multiple-value-bind (tok2 rest3) (js-consume rest2)
                  (declare (ignore tok2))
                  (multiple-value-bind (idx-ast rest4) (js-parse-assignment-expr rest3)
                    (multiple-value-bind (tok3 rest5) (js-expect :T-RBRACKET rest4)
                      (declare (ignore tok3))
                      (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                               :args (list ast idx-ast))
                            rest rest5)))))
               (t
                (setf rest rest2)
                (return)))))
          (t (return)))))
    (values ast rest)))

;;; ─── Increment / decrement on places (obj.x / arr[i]) ────────────────────────

(defun %js-place-get-prop-p (ast)
  "True when AST is an assignable property place — either a public property
access (%js-get-prop OBJ KEY) or a private field access
(%js-class-private-field-get OBJ KEY)."
  (and (ast-call-p ast)
       (ast-var-p (ast-call-func ast))
       (member (ast-var-name (ast-call-func ast))
               '(%js-get-prop %js-class-private-field-get))
       (= (length (ast-call-args ast)) 2)))

(defun %js-lower-place-incdec (place op return-new-p)
  "Lower ++/-- applied to a property/element PLACE.
PLACE may be a public (%js-get-prop) or private (%js-class-private-field-get) access.
OP is '+ or '-. RETURN-NEW-P true means prefix (yields updated value); false
means postfix (yields original). OBJ and KEY are captured in temps so the place
expression is evaluated exactly once."
  (let* ((obj     (first  (ast-call-args place)))
         (key     (second (ast-call-args place)))
         (get-fn  (ast-var-name (ast-call-func place)))
         (set-fn  (if (eq get-fn '%js-class-private-field-get)
                      '%js-class-private-field-set
                      '%js-set-prop))
         (obj-tmp (gensym "JS-OBJ-"))
         (key-tmp (gensym "JS-KEY-"))
         (old-tmp (gensym "JS-OLD-")))
    (flet ((bumped ()
             (make-ast-binop :op op
                             :lhs (make-ast-var :name old-tmp)
                             :rhs (make-ast-int :value 1))))
      (make-ast-let
       :bindings (list (cons obj-tmp obj) (cons key-tmp key))
       :body
       (list
        (make-ast-let
         :bindings (list (cons old-tmp (%js-call get-fn
                                                 (make-ast-var :name obj-tmp)
                                                 (make-ast-var :name key-tmp))))
         :body (list (%js-call set-fn
                               (make-ast-var :name obj-tmp)
                               (make-ast-var :name key-tmp)
                               (bumped))
                     (if return-new-p (bumped) (make-ast-var :name old-tmp)))))))))

;;; ─── Postfix ─────────────────────────────────────────────────────────────────

(defun js-parse-postfix (ast stream)
  "Apply postfix operations: ++ -- . [] () ?. to AST.
Returns (values ast rest). Loops until no more postfix ops."
  (loop
    (let ((type (js-peek-type stream))
          (val  (js-peek-value stream)))
      (cond
        ;; Postfix ++
        ((and (eq type :T-OP) (string= val "++"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (if (ast-var-p ast)
               (let* ((var-sym (ast-var-name ast))
                      (tmp (gensym "JS-POSTFIX-")))
                 (setf ast (make-ast-let
                            :bindings (list (cons tmp (make-ast-var :name var-sym)))
                            :body (list (make-ast-setq
                                         :var var-sym
                                         :value (make-ast-binop :op '+
                                                                :lhs (make-ast-var :name var-sym)
                                                                :rhs (make-ast-int :value 1)))
                                        (make-ast-var :name tmp)))
                       stream rest))
               (setf ast (if (%js-place-get-prop-p ast)
                             (%js-lower-place-incdec ast '+ nil)
                             (%js-call '%js-postfix-inc ast))
                     stream rest))))
        ;; Postfix --
        ((and (eq type :T-OP) (string= val "--"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (if (ast-var-p ast)
               (let* ((var-sym (ast-var-name ast))
                      (tmp (gensym "JS-POSTFIX-")))
                 (setf ast (make-ast-let
                            :bindings (list (cons tmp (make-ast-var :name var-sym)))
                            :body (list (make-ast-setq
                                         :var var-sym
                                         :value (make-ast-binop :op '-
                                                                :lhs (make-ast-var :name var-sym)
                                                                :rhs (make-ast-int :value 1)))
                                        (make-ast-var :name tmp)))
                       stream rest))
               (setf ast (if (%js-place-get-prop-p ast)
                             (%js-lower-place-incdec ast '- nil)
                             (%js-call '%js-postfix-dec ast))
                     stream rest))))
        ;; Property access: obj.prop
        ((eq type :T-DOT)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           ;; private field #name
           (if (eq (js-peek-type rest) :T-PRIVATE-IDENT)
               (multiple-value-bind (prop-tok rest2) (js-consume rest)
                 (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                   (setf ast (make-ast-call :func (make-ast-var :name '%js-class-private-field-get)
                                            :args (list ast key))
                         stream rest2)))
               (multiple-value-bind (prop-tok rest2) (js-consume rest)
                 (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                   (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                            :args (list ast key))
                         stream rest2))))))
        ;; Computed member: obj[expr]
        ((eq type :T-LBRACKET)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (idx-ast rest2) (js-parse-assignment-expr rest)
             (multiple-value-bind (tok2 rest3) (js-expect :T-RBRACKET rest2)
               (declare (ignore tok2))
               (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                        :args (list ast idx-ast))
                     stream rest3)))))
        ;; Call: fn(args)
        ((eq type :T-LPAREN)
         (multiple-value-bind (args rest) (js-parse-arguments stream)
           (setf ast (cond
                       ((%js-items-have-spread-p args)
                        (make-ast-apply :func ast :args (list (%js-spread-list-expr args))))
                       ((and (ast-var-p ast)
                             (gethash (ast-var-name ast) *js-coercion-call-helpers*))
                        (%js-lower-coercion-call
                         (gethash (ast-var-name ast) *js-coercion-call-helpers*) args))
                       (t (make-ast-call :func ast :args args)))
                 stream rest)))
        ;; Optional chain ?.
        ((and (eq type :T-OP) (string= val "?."))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (cond
             ;; ?.prop — if followed by ( emit optional-method-call, else optional-chain
             ((eq (js-peek-type rest) :T-IDENT)
              (multiple-value-bind (prop-tok rest2) (js-consume rest)
                (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                  (if (eq (js-peek-type rest2) :T-LPAREN)
                      (multiple-value-bind (args rest3) (js-parse-arguments rest2)
                        (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-method-call)
                                                 :args (list* ast key args))
                              stream rest3))
                      (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                               :args (list ast key))
                            stream rest2)))))
             ;; ?.[expr]
             ((eq (js-peek-type rest) :T-LBRACKET)
              (multiple-value-bind (tok2 rest2) (js-consume rest)
                (declare (ignore tok2))
                (multiple-value-bind (idx-ast rest3) (js-parse-assignment-expr rest2)
                  (multiple-value-bind (tok3 rest4) (js-expect :T-RBRACKET rest3)
                    (declare (ignore tok3))
                    (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                             :args (list ast idx-ast))
                          stream rest4)))))
             ;; ?.(args)
             ((eq (js-peek-type rest) :T-LPAREN)
              (multiple-value-bind (args rest2) (js-parse-arguments rest)
                (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-call)
                                         :args (cons ast args))
                      stream rest2)))
             (t
              (setf stream rest)
              (return)))))
        ;; Tagged template literal
        ((or (eq type :T-TEMPLATE-START)
             (eq type :T-TEMPLATE-PARTS))
         (multiple-value-bind (call-ast rest) (%js-parse-tagged-template ast stream)
           (setf ast call-ast stream rest)))
        (t (return)))))
  (values ast stream))
