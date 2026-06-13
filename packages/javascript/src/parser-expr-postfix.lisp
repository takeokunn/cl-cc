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
  "True when AST is a (%js-get-prop OBJ KEY) call — an assignable place."
  (and (ast-call-p ast)
       (ast-var-p (ast-call-func ast))
       (eq (ast-var-name (ast-call-func ast)) '%js-get-prop)
       (= (length (ast-call-args ast)) 2)))

(defun %js-lower-place-incdec (place op return-new-p)
  "Lower ++/-- applied to a property/element PLACE (a %js-get-prop call).
OP is '+ or '-. RETURN-NEW-P true means prefix (yields updated value); false
means postfix (yields original). OBJ and KEY are captured in temps so the place
expression is evaluated exactly once."
  (let ((obj     (first  (ast-call-args place)))
        (key     (second (ast-call-args place)))
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
         :bindings (list (cons old-tmp (%js-call '%js-get-prop
                                                 (make-ast-var :name obj-tmp)
                                                 (make-ast-var :name key-tmp))))
         :body (list (%js-call '%js-set-prop
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

;;; ─── Template Literal ────────────────────────────────────────────────────────

(defun %js-template-parts-and-rest (stream)
  "Return (values PARTS rest) for a template literal at STREAM."
  (let ((tok (js-peek stream)))
    (case (js-tok-type tok)
      (:T-STRING
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (list (js-tok-value str-tok)) rest)))
      (:T-TEMPLATE-PARTS
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (values (js-tok-value parts-tok) rest)))
      (:T-TEMPLATE-START
       (multiple-value-bind (tok2 rest) (js-consume stream)
         (declare (ignore tok2))
         (%js-template-parts-and-rest rest)))
      (t (error "JS parse error: expected template literal, got ~S" tok)))))

(defun %js-parse-tagged-template (tag-ast stream)
  "Lower a tagged template tag`...` to (call TAG strings-array value1 value2 ...),
per the TC39 tagged-template protocol."
  (multiple-value-bind (parts rest) (%js-template-parts-and-rest stream)
    (let ((cooked nil) (values nil) (current ""))
      (dolist (part parts)
        (cond
          ((stringp part)
           (setf current (concatenate 'string current part)))
          ((and (consp part) (eq (car part) :template-expr))
           (push (make-ast-quote :value current) cooked)
           (setf current "")
           (multiple-value-bind (expr _r) (js-parse-assignment-expr (cadr part))
             (declare (ignore _r))
             (push expr values)))))
      (push (make-ast-quote :value current) cooked)
      (values (make-ast-call
               :func tag-ast
               :args (cons (apply #'%js-call '%js-make-array (nreverse cooked))
                           (nreverse values)))
              rest))))

(defun %js-parse-template-literal (stream)
  "Parse a template literal starting at :T-TEMPLATE-START or :T-STRING.
Returns (values ast rest)."
  (let ((tok (js-peek stream)))
    (cond
      ;; Simple string template (no interpolation)
      ((eq (js-tok-type tok) :T-STRING)
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value str-tok)) rest)))
      ;; Template with parts: :T-TEMPLATE-PARTS
      ((eq (js-tok-type tok) :T-TEMPLATE-PARTS)
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (let ((parts (js-tok-value parts-tok))
               (segments nil))
           (dolist (part parts)
             (cond
               ((stringp part)
                (push (make-ast-quote :value part) segments))
               ((and (consp part) (eq (car part) :template-expr))
                (let ((inner-tokens (cadr part)))
                  (multiple-value-bind (expr _rest)
                      (js-parse-assignment-expr inner-tokens)
                    (declare (ignore _rest))
                    (push (%js-call '%js-to-string expr) segments))))
               (t
                (push (make-ast-quote :value (format nil "~A" part)) segments))))
           (let ((parts-list (nreverse segments)))
             (values (if (null (cdr parts-list))
                         (car parts-list)
                         (reduce (lambda (l r) (%js-call '%js-concat l r))
                                 parts-list))
                     rest)))))
      ;; :T-TEMPLATE-START — consumed by the template lexer inline
      ((eq (js-tok-type tok) :T-TEMPLATE-START)
       (multiple-value-bind (tok2 rest) (js-consume stream)
         (declare (ignore tok2))
         (%js-parse-template-literal rest)))
      (t
       (error "JS parse error: expected template literal, got ~S" tok)))))

;;; ─── Unary ───────────────────────────────────────────────────────────────────
;;;
;;; Data-driven dispatch for simple OP-string and keyword unary operators.
;;; Prefix ++/--, unary -, and yield have special logic and are handled explicitly.

;;; OP-string unaries: maps operator string -> (lambda (expr) -> ast).
;;; Excludes - (constant folding), ++ and -- (place/var dispatch).
(defparameter *js-unary-op-builders*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (entry
             `(("!" . ,(lambda (expr)
                         (make-ast-call :func (make-ast-var :name 'not)
                                        :args (list (%js-call '%js-truthy expr)))))
               ("~" . ,(lambda (expr) (%js-call '%js-bitwise-not expr)))
               ("+" . ,(lambda (expr) (%js-call '%js-unary-plus expr)))))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Data table: JS unary OP string -> AST builder (lambda (expr) -> ast).
Only for simple single-argument operators; -, ++, --, yield handled separately.")

;;; Keyword unaries: maps token type -> (lambda (expr) -> ast).
;;; typeof/void/delete/await all follow the same consume-one-token pattern.
(defparameter *js-unary-kw-builders*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry
             `((:T-TYPEOF . ,(lambda (expr) (%js-call '%js-typeof expr)))
               (:T-DELETE . ,(lambda (expr) (%js-call '%js-delete expr)))
               (:T-AWAIT  . ,(lambda (expr) (%js-call '%js-await expr)))
               (:T-VOID   . ,(lambda (expr)
                               (make-ast-progn
                                :forms (list expr (make-ast-quote :value :js-undefined)))))))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Data table: keyword unary token type -> AST builder (lambda (expr) -> ast).
typeof/void/delete/await all consume one token and wrap the sub-expression.")

(defun js-parse-unary (stream)
  "Parse prefix unary: ! ~ + - ++ -- typeof void delete await yield.
Returns (values ast rest)."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    ;; CPS helper: consume one token, recurse into sub-expression, apply BUILDER.
    (labels ((consume-and-build (builder)
               (multiple-value-bind (tok rest) (js-consume stream)
                 (declare (ignore tok))
                 (multiple-value-bind (expr rest2) (js-parse-unary rest)
                   (values (funcall builder expr) rest2)))))
      (cond
        ;; Table-driven: simple OP-string unary operators (!, ~, +)
        ((and (eq type :T-OP) (gethash val *js-unary-op-builders*))
         (consume-and-build (gethash val *js-unary-op-builders*)))
        ;; Unary - : constant-fold integer literals, else negate
        ((and (eq type :T-OP) (string= val "-"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-int-p expr)
                 (values (make-ast-int :value (- (ast-int-value expr))) rest2)
                 (values (make-ast-call :func (make-ast-var :name '-)
                                        :args (list expr))
                         rest2)))))
        ;; Table-driven: keyword unary operators (typeof, void, delete, await)
        ((gethash type *js-unary-kw-builders*)
         (consume-and-build (gethash type *js-unary-kw-builders*)))
        ;; Prefix ++ (var or place)
        ((and (eq type :T-OP) (string= val "++"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-var-p expr)
                 (let ((var-sym (ast-var-name expr)))
                   (values (make-ast-setq :var var-sym
                                          :value (make-ast-binop :op '+
                                                                 :lhs expr
                                                                 :rhs (make-ast-int :value 1)))
                           rest2))
                 (if (%js-place-get-prop-p expr)
                     (values (%js-lower-place-incdec expr '+ t) rest2)
                     (values (%js-call '%js-prefix-inc expr) rest2))))))
        ;; Prefix -- (var or place)
        ((and (eq type :T-OP) (string= val "--"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-var-p expr)
                 (let ((var-sym (ast-var-name expr)))
                   (values (make-ast-setq :var var-sym
                                          :value (make-ast-binop :op '-
                                                                 :lhs expr
                                                                 :rhs (make-ast-int :value 1)))
                           rest2))
                 (if (%js-place-get-prop-p expr)
                     (values (%js-lower-place-incdec expr '- t) rest2)
                     (values (%js-call '%js-prefix-dec expr) rest2))))))
        ;; yield (prefix usage)
        ((eq type :T-YIELD)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           ;; yield* iterable
           (if (and (eq (js-peek-type rest) :T-OP) (string= (js-peek-value rest) "*"))
               (multiple-value-bind (tok2 rest2) (js-consume rest)
                 (declare (ignore tok2))
                 (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                   (values (%js-call '%js-yield-from expr) rest3)))
               ;; yield expr or bare yield
               (if (or (js-at-eof-p rest)
                       (member (js-peek-type rest)
                               '(:T-SEMI :T-COMMA :T-RBRACE :T-RPAREN :T-RBRACKET) :test #'eq))
                   (values (%js-call '%js-yield) rest)
                   (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                     (values (%js-call '%js-yield expr) rest2))))))
        ;; Fall through to postfix
        (t
         (multiple-value-bind (ast rest) (js-parse-primary stream)
           (js-parse-postfix ast rest)))))))
