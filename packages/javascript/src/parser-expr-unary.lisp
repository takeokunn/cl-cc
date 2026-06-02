;;;; packages/javascript/src/parser-expr-unary.lisp — JS Unary/Primary Expression Parsers
;;;;
;;;; Contains: template literal parser, unary expression parser (typeof/void/delete/!/~/±),
;;;; and primary expression dispatcher (identifiers, literals, this, new, class, import()).
;;;;
;;;; Load order: after parser-expr.lisp (which provides postfix, operator lowering,
;;;; array/object literals, and function expression helpers).

(in-package :cl-cc/javascript)

;;; ─── Template Literal ────────────────────────────────────────────────────────

(defun %js-parse-template-literal (stream)
  "Parse a template literal starting at :T-TEMPLATE-START or :T-STRING.
Returns (values ast rest)."
  (let ((tok (js-peek stream)))
    (cond
      ;; Simple string template (no interpolation): lexer emitted :T-STRING
      ((eq (js-tok-type tok) :T-STRING)
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value str-tok)) rest)))
      ;; Template with parts: :T-TEMPLATE-PARTS (value = list of strings and (:template-expr tokens))
      ((eq (js-tok-type tok) :T-TEMPLATE-PARTS)
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (let ((parts (js-tok-value parts-tok))
               (segments nil))
           (dolist (part parts)
             (cond
               ((stringp part)
                (push (make-ast-quote :value part) segments))
               ((and (consp part) (eq (car part) :template-expr))
                ;; The inner tokens form an expression — parse it
                (let ((inner-tokens (cadr part)))
                  (multiple-value-bind (expr _rest)
                      (js-parse-assignment-expr inner-tokens)
                    (declare (ignore _rest))
                    (push (%js-call '%js-to-string expr) segments))))
               (t
                (push (make-ast-quote :value (format nil "~A" part)) segments))))
           ;; Build concat chain
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
         ;; After T-TEMPLATE-START, expect T-STRING or T-TEMPLATE-PARTS
         (%js-parse-template-literal rest)))
      (t
       (error "JS parse error: expected template literal, got ~S" tok)))))

;;; ─── Unary ───────────────────────────────────────────────────────────────────

(defun js-parse-unary (stream)
  "Parse prefix unary: ! ~ + - ++ -- typeof void delete await yield.
Returns (values ast rest)."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Prefix ++
      ((and (eq type :T-OP) (string= val "++"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (if (ast-var-p expr)
               (let ((var-sym (ast-var-name expr)))
                 (values (make-ast-setq :var var-sym
                                        :value (make-ast-binop :op :+
                                                               :lhs expr
                                                               :rhs (make-ast-int :value 1)))
                         rest2))
               (values (%js-call '%js-prefix-inc expr) rest2)))))
      ;; Prefix --
      ((and (eq type :T-OP) (string= val "--"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (if (ast-var-p expr)
               (let ((var-sym (ast-var-name expr)))
                 (values (make-ast-setq :var var-sym
                                        :value (make-ast-binop :op :-
                                                               :lhs expr
                                                               :rhs (make-ast-int :value 1)))
                         rest2))
               (values (%js-call '%js-prefix-dec expr) rest2)))))
      ;; Logical NOT !
      ((and (eq type :T-OP) (string= val "!"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (make-ast-call :func (make-ast-var :name 'not)
                                  :args (list (%js-call '%js-truthy expr)))
                   rest2))))
      ;; Bitwise NOT ~
      ((and (eq type :T-OP) (string= val "~"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-bitwise-not expr) rest2))))
      ;; Unary + (numeric coercion)
      ((and (eq type :T-OP) (string= val "+"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-unary-plus expr) rest2))))
      ;; Unary -
      ((and (eq type :T-OP) (string= val "-"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           ;; Constant folding for numeric literals
           (if (ast-int-p expr)
               (values (make-ast-int :value (- (ast-int-value expr))) rest2)
               (values (make-ast-call :func (make-ast-var :name '-)
                                      :args (list expr))
                       rest2)))))
      ;; typeof
      ((eq type :T-TYPEOF)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-typeof expr) rest2))))
      ;; void
      ((eq type :T-VOID)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (make-ast-progn :forms (list expr (make-ast-quote :value :undefined)))
                   rest2))))
      ;; delete
      ((eq type :T-DELETE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-delete expr) rest2))))
      ;; await
      ((eq type :T-AWAIT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-await expr) rest2))))
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
         (js-parse-postfix ast rest))))))

;;; ─── Primary Expression ──────────────────────────────────────────────────────

(defun js-parse-primary (stream)
  "Parse a primary expression. Returns (values ast rest).
Handles: numbers, strings, booleans, null, undefined, this, super,
identifiers, parenthesized expressions, array literals [...],
object literals {...}, function expressions, async functions,
generator functions, class expressions, new expr, template literals,
yield, await, import()."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Numeric literal (integer or float)
      ((eq type :T-NUMBER)
       (multiple-value-bind (tok rest) (js-consume stream)
         (let ((v (js-tok-value tok)))
           (if (integerp v)
               (values (make-ast-int :value v) rest)
               (values (make-ast-quote :value v) rest)))))
      ;; BigInt literal
      ((eq type :T-BIGINT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-int :value (js-tok-value tok)) rest)))
      ;; String literal
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value tok)) rest)))
      ;; Regex literal
      ((eq type :T-REGEX)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (%js-call '%js-make-regex
                           (make-ast-quote :value (js-tok-value tok)))
                 rest)))
      ;; Template literal
      ((or (eq type :T-TEMPLATE-START)
           (eq type :T-TEMPLATE-PARTS))
       (%js-parse-template-literal stream))
      ;; Boolean true
      ((eq type :T-TRUE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value t) rest)))
      ;; Boolean false
      ((eq type :T-FALSE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value nil) rest)))
      ;; null literal
      ((eq type :T-NULL)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :null) rest)))
      ;; undefined
      ((eq type :T-UNDEFINED)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :undefined) rest)))
      ;; this
      ((eq type :T-THIS)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name '%js-this) rest)))
      ;; super
      ((eq type :T-SUPER)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name '%js-super) rest)))
      ;; Array literal [...]
      ((eq type :T-LBRACKET)
       (js-parse-array-literal stream))
      ;; Object literal {...}
      ((eq type :T-LBRACE)
       (js-parse-object-literal stream))
      ;; Parenthesized expression or arrow function params
      ((eq type :T-LPAREN)
       (%js-parse-paren-or-arrow stream))
      ;; function expression
      ((eq type :T-FUNCTION)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (js-parse-function-expr rest)))
      ;; async function / async arrow
      ((eq type :T-ASYNC)
       (%js-parse-async-expr stream))
      ;; class expression
      ((eq type :T-CLASS)
       (%js-parse-class-expr stream))
      ;; new expression (including new.target)
      ((eq type :T-NEW)
       (js-parse-new-expr stream))
      ;; import() dynamic import
      ((eq type :T-IMPORT)
       (%js-parse-import-expr stream))
      ;; yield as expression (when used as identifier-like)
      ((eq type :T-YIELD)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (if (and (eq (js-peek-type rest) :T-OP) (string= (js-peek-value rest) "*"))
             (multiple-value-bind (tok2 rest2) (js-consume rest)
               (declare (ignore tok2))
               (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                 (values (%js-call '%js-yield-from expr) rest3)))
             (if (or (js-at-eof-p rest)
                     (member (js-peek-type rest)
                             '(:T-SEMI :T-COMMA :T-RBRACE :T-RPAREN :T-RBRACKET) :test #'eq))
                 (values (%js-call '%js-yield) rest)
                 (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                   (values (%js-call '%js-yield expr) rest2))))))
      ;; await as expression
      ((eq type :T-AWAIT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-await expr) rest2))))
      ;; Identifier — may begin a single-parameter arrow function: x => body
      ((eq type :T-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (if (eq (js-peek-type rest) :T-ARROW)
             (%js-finish-arrow-function (list (js-ident-sym val)) rest)
             (values (make-ast-var :name (js-ident-sym val)) rest))))
      ;; Contextual keywords used as identifiers (get, set, from, as, of, target, meta, using, static)
      ((member type '(:T-GET :T-SET :T-FROM :T-AS :T-OF
                      :T-TARGET :T-META :T-USING :T-STATIC :T-LET) :test #'eq)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name (js-ident-sym val)) rest)))
      ;; Private field identifier #name — as standalone (for #name in obj)
      ((eq type :T-PRIVATE-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-var :name (js-ident-sym (concatenate 'string "#" val))) rest)))
      (t
       (error "JS parse error: unexpected token ~S in expression" (js-peek stream))))))

