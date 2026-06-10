;;;; packages/javascript/src/parser-expr-primary.lisp — Primary Expression Parsers
;;;;
;;;; Concrete primary expression forms: literals, identifiers, parenthesized
;;;; expressions, arrow functions, async/class/import expressions, and the
;;;; main Pratt entry points (js-parse-expr, js-parse-assignment-expr).
;;;;
;;;; Depends on parser-expr.lisp for Pratt precedence table, operator lowering,
;;;; parameter parsers, array/object literals, function expression, new/member,
;;;; postfix, template literal, and unary parsers.
;;;;
;;;; Load order: after parser-expr.lisp, before parser-stmt.lisp.

(in-package :cl-cc/javascript)

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
      ;; null literal. The runtime null sentinel is +js-null+ (= :js-null); this
      ;; MUST match it, not the bare :null keyword — otherwise %js-to-string prints
      ;; "NULL" and %js-not-nullish treats `null' as non-nullish (null ?? x broke).
      ;; (+js-null+ itself can't be named here: this file compiles before runtime.)
      ((eq type :T-NULL)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :js-null) rest)))
      ;; undefined — likewise the +js-undefined+ sentinel (= :js-undefined)
      ((eq type :T-UNDEFINED)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :js-undefined) rest)))
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

;;; ─── Parenthesized Expression / Arrow Function ───────────────────────────────

(defun %js-call-callee-name (ast)
  "If AST is (ast-call (ast-var NAME) …), return NAME; else NIL."
  (when (and (ast-call-p ast) (ast-var-p (ast-call-func ast)))
    (ast-var-name (ast-call-func ast))))

(defun %js-expr-to-binding-pattern (expr)
  "Convert an arrow CoverGrammar parameter EXPR — already parsed as an expression
— into a destructuring binding pattern for %js-emit-destructure-bindings, or NIL
if EXPR is not a pattern.  A plain var becomes its symbol; an array literal
(%js-make-array …) becomes an :array-pattern (holes/spread/nesting preserved); an
object literal (%js-make-object k v …) becomes an :object-pattern.  This is how
([a,b])=>… and ({x,y})=>… recover their patterns, since the paren contents were
parsed as an array/object literal before the => was seen."
  (cond
    ((ast-var-p expr) (ast-var-name expr))
    ((eq (%js-call-callee-name expr) '%js-make-array)
     (list :array-pattern (gensym "ARR-DEST-")
           (mapcar
            (lambda (el)
              (cond
                ((and (ast-quote-p el) (eq (ast-quote-value el) :js-hole)) :hole)
                ((eq (%js-call-callee-name el) '%js-spread)
                 (list :rest (%js-expr-to-binding-pattern (first (ast-call-args el)))))
                (t (%js-expr-to-binding-pattern el))))
            (ast-call-args expr))))
    ((eq (%js-call-callee-name expr) '%js-make-object)
     (list :object-pattern (gensym "OBJ-DEST-")
           (loop for (k v) on (ast-call-args expr) by #'cddr
                 collect (list (and (ast-quote-p k) (ast-quote-value k))
                               (%js-expr-to-binding-pattern v)
                               nil))))
    (t nil)))

(defun %js-parse-paren-or-arrow (stream)
  "Parse ( ... ) — either a parenthesized expression or arrow function params.
Returns (values ast rest)."
  ;; We use a speculative approach: try to detect => after the closing )
  ;; by collecting the paren contents and peeking ahead.
  ;; Strategy: parse a comma-separated list of assignment-exprs or patterns,
  ;; then check for =>.
  (multiple-value-bind (tok rest) (js-consume stream) ; consume (
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RPAREN)
        ;; () => ... (arrow with no params) — must be followed by =>
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (if (eq (js-peek-type rest2) :T-ARROW)
              (%js-finish-arrow-function nil rest2)
              ;; () as expression is a syntax error in JS but we emit undefined for tolerance
              (values (make-ast-quote :value :js-undefined) rest2)))
        ;; Non-empty: collect items, recording BOTH an arrow-param interpretation
        ;; (params + optionals + rest-sym) and an expression fallback (exprs), then
        ;; commit once we can peek past the ) for =>.  A default param `x = e` is
        ;; ambiguous with a parenthesized assignment `(x = e)`, so we keep both.
        (let ((exprs nil)
              (params nil)
              (optionals nil)        ; (sym . default-ast) alist for `x = default'
              (param-patterns nil)   ; (gensym . binding-pattern) for [a,b]/{x,y} params
              (is-arrow-candidate t)
              (current rest)
              (rest-sym nil))
          ;; Collect items
          (loop
            (cond
              ;; ...rest param
              ((eq (js-peek-type current) :T-ELLIPSIS)
               (multiple-value-bind (tok2 rest2) (js-consume current)
                 (declare (ignore tok2))
                 (multiple-value-bind (name-tok rest3) (js-expect :T-IDENT rest2)
                   (let ((sym (js-ident-sym (js-tok-value name-tok))))
                     (setf rest-sym sym
                           current rest3))))
               (return)) ; rest param must be last
              (t
               ;; Try as assignment-expr
               (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
                 (setf current rest2)
                 ;; Default parameter: `var = default'.  js-parse-assignment-expr
                 ;; stops at the `=' here (it is not an assignment operator in this
                 ;; position), so consume it ourselves and parse the default.
                 (if (and (ast-var-p expr)
                          (eq (js-peek-type current) :T-OP)
                          (string= (js-peek-value current) "="))
                     (multiple-value-bind (eq-tok rest3) (js-consume current)
                       (declare (ignore eq-tok))
                       (multiple-value-bind (default-expr rest4)
                           (js-parse-assignment-expr rest3)
                         (push (ast-var-name expr) params)
                         (push (cons (ast-var-name expr) default-expr) optionals)
                         ;; non-arrow fallback: a parenthesized assignment expr
                         (push (make-ast-setq :var (ast-var-name expr) :value default-expr)
                               exprs)
                         (setf current rest4)))
                     (progn
                       (push expr exprs)
                       (cond
                         ((ast-var-p expr)
                          (push (ast-var-name expr) params))
                         ;; Array/object literal in param position: an arrow
                         ;; destructuring pattern.  Give it a gensym param and
                         ;; record the pattern for body-prologue unpacking.
                         (t
                          (let ((pat (%js-expr-to-binding-pattern expr)))
                            (if (and pat (listp pat))
                                (let ((g (second pat)))
                                  (push g params)
                                  (push (cons g pat) param-patterns))
                                ;; not a valid pattern -> not an arrow
                                (setf is-arrow-candidate nil))))))))))
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  (if (eq (js-peek-type rest2) :T-RPAREN)
                      (progn (setf current rest2) (return))
                      (setf current rest2)))
                (return)))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RPAREN current)
            (declare (ignore tok2))
            ;; Check for =>
            (if (and is-arrow-candidate (eq (js-peek-type rest2) :T-ARROW))
                (%js-finish-arrow-function (nreverse params) rest2
                                           :optionals (nreverse optionals)
                                           :rest-sym rest-sym
                                           :param-patterns (nreverse param-patterns))
                ;; Parenthesized expression — return last expression (or progn)
                (let ((all-exprs (nreverse exprs)))
                  (values (if (null (cdr all-exprs))
                              (car all-exprs)
                              (make-ast-progn :forms all-exprs))
                          rest2))))))))

(defun %js-finish-arrow-function (params stream &key optionals rest-sym param-patterns)
  "Consume => and parse arrow function body. PARAMS is the list of required-param
symbols; OPTIONALS is a (sym . default-ast) alist for defaulted params; REST-SYM
is the rest-parameter symbol (or nil); PARAM-PATTERNS is a (gensym . pattern)
alist for destructuring params like ([a,b])=>… (the gensym param is unpacked by a
body-prologue let, exactly as for named functions).  Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-expect :T-ARROW stream)
    (declare (ignore tok))
    (multiple-value-bind (required opts) (%js-split-params-by-defaults params optionals)
      ;; FINISH builds the lambda from a body-form list: defaults become
      ;; optional-params; the rest param is routed through %js-rest-binding, which
      ;; wraps the body to bind REST-SYM to a JS array of the trailing args.
      (flet ((finish (body-forms rest2)
               (multiple-value-bind (rest-param wrapped-body)
                   (%js-rest-binding rest-sym
                                     (%js-prepend-param-destructuring
                                      param-patterns body-forms))
                 (values (make-ast-lambda :params required
                                          :optional-params opts
                                          :rest-param rest-param
                                          :body wrapped-body)
                         rest2))))
        (if (eq (js-peek-type rest) :T-LBRACE)
            ;; Block body: => { stmts }. Wrap in (block nil ...) via %js-callable-body
            ;; so `return' (which lowers to return-from nil) works — a bare lambda
            ;; establishes no block, so without this the body fails to compile and the
            ;; enclosing form is silently dropped (same fix as regular functions).
            (multiple-value-bind (tok2 rest2) (js-consume rest)
              (declare (ignore tok2))
              (multiple-value-bind (body-forms rest3)
                  (js-parse-function-body rest2)
                (finish (%js-callable-body body-forms) rest3)))
            ;; Concise body: => expr. The lambda yields EXPR directly as its value;
            ;; no return-from/block is needed (and a bare lambda binds no block nil).
            (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
              (finish (list expr) rest2)))))))

;;; ─── Async Expression ────────────────────────────────────────────────────────

(defun %js-parse-async-expr (stream)
  "Parse async function or async arrow function.
Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'async'
    (declare (ignore tok))
    (let ((next-type (js-peek-type rest))
          (next-val  (js-peek-value rest)))
      (cond
        ;; async function [*] name? (params) { body }
        ((eq next-type :T-FUNCTION)
         (multiple-value-bind (tok2 rest2) (js-consume rest)
           (declare (ignore tok2))
           (js-parse-function-expr rest2 :async-p t)))
        ;; async (params) => body  or  async ident => body
        ((eq next-type :T-LPAREN)
         ;; async arrow function: async (params) =>
         (multiple-value-bind (params optionals rest-sym rest2)
             (js-parse-params rest)
           (declare (ignore optionals rest-sym))
           (if (eq (js-peek-type rest2) :T-ARROW)
               (%js-finish-async-arrow params rest2)
               ;; Not an arrow — async followed by ( is a call: async(...)
               (multiple-value-bind (args rest3) (js-parse-arguments rest)
                 (values (make-ast-call :func (make-ast-var :name (js-ident-sym "async"))
                                        :args args)
                         rest3)))))
        ;; async ident => body (single param arrow)
        ((eq next-type :T-IDENT)
         (let ((param-sym (js-ident-sym next-val)))
           (multiple-value-bind (tok2 rest2) (js-consume rest)
             (declare (ignore tok2))
             (if (eq (js-peek-type rest2) :T-ARROW)
                 (%js-finish-async-arrow (list param-sym) rest2)
                 ;; async used as property / identifier
                 (multiple-value-bind (ast rest3)
                     (js-parse-postfix (make-ast-var :name (js-ident-sym "async")) rest)
                   (values ast rest3))))))
        ;; async as standalone identifier
        (t
         (values (make-ast-var :name (js-ident-sym "async")) rest))))))

(defun %js-finish-async-arrow (params stream)
  "Parse => body for an async arrow function. Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-expect :T-ARROW stream)
    (declare (ignore tok))
    (let ((body-and-rest
           (if (eq (js-peek-type rest) :T-LBRACE)
               (multiple-value-bind (tok2 rest2) (js-consume rest)
                 (declare (ignore tok2))
                 (multiple-value-bind (body-forms rest3) (js-parse-function-body rest2)
                   (list (%js-callable-body body-forms) rest3)))
               (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                 (list (list expr) rest2)))))
      (values (%js-call '%js-make-async
                        (make-ast-lambda :params params :body (car body-and-rest)))
              (cadr body-and-rest)))))

;;; ─── Class Expression ────────────────────────────────────────────────────────

(defun %js-parse-class-expr (stream)
  "Parse a class EXPRESSION: class [name] [extends expr] { body }.
Delegates to js-parse-class-decl (parser-class.lisp) — the single source of
truth for class parsing — with :expression-p t. Returns (values ast rest).

Previously this file carried duplicate %js-parse-class-body/%js-parse-class-member
definitions that, under serial load, were shadowed by parser-class.lisp's
arity-2 %js-parse-class-body, so the old 1-arg call here crashed every class
expression. Routing through js-parse-class-decl removes the duplication."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'class'
    (declare (ignore tok))
    (multiple-value-bind (ast-list rest2) (js-parse-class-decl rest :expression-p t)
      (values (if (and (consp ast-list) (= (length ast-list) 1))
                  (first ast-list)
                  (make-ast-progn :forms ast-list))
              rest2))))

;;; ─── import() Dynamic Import ─────────────────────────────────────────────────

(defun %js-parse-import-expr (stream)
  "Parse import(specifier) or import.meta.
Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'import'
    (declare (ignore tok))
    (cond
      ;; import.meta
      ((and (eq (js-peek-type rest) :T-DOT)
            (eq (js-peek-type (cdr rest)) :T-META))
       (multiple-value-bind (dot-tok rest2) (js-consume rest)
         (declare (ignore dot-tok))
         (multiple-value-bind (meta-tok rest3) (js-consume rest2)
           (declare (ignore meta-tok))
           (values (%js-call '%js-import-meta) rest3))))
      ;; import(specifier)
      ((eq (js-peek-type rest) :T-LPAREN)
       (multiple-value-bind (tok2 rest2) (js-consume rest)
         (declare (ignore tok2))
         (multiple-value-bind (spec-ast rest3) (js-parse-assignment-expr rest2)
           (multiple-value-bind (tok3 rest4) (js-expect :T-RPAREN rest3)
             (declare (ignore tok3))
             (values (%js-call '%js-import spec-ast) rest4)))))
      (t
       (error "JS parse error: unexpected token after 'import': ~S" (js-peek rest))))))

;;; ─── Main Pratt Parser ───────────────────────────────────────────────────────

(defun js-parse-expr (stream &optional (min-prec 0))
  "Main Pratt expression parser. Returns (values ast rest).
Handles all infix operators at precedence >= MIN-PREC including
assignment (right-assoc), ternary, binary ops, and comma."
  (with-js-parse-depth
  (multiple-value-bind (lhs rest) (js-parse-unary stream)
    (loop
      (multiple-value-bind (prec right-assoc-p) (js-infix-prec rest)
        (when (<= prec min-prec)
          (return))
        (let ((op-type  (js-peek-type rest))
              (op-val   (js-peek-value rest)))
          (cond
            ;; Ternary: ? then : else
            ((eq op-type :T-QUESTION)
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (multiple-value-bind (then-ast rest3) (js-parse-assignment-expr rest2)
                 (multiple-value-bind (tok2 rest4) (js-expect :T-COLON rest3)
                   (declare (ignore tok2))
                   (multiple-value-bind (else-ast rest5) (js-parse-assignment-expr rest4)
                     ;; Coerce the condition to a JS boolean (%js-truthy), like the
                     ;; if/while/for statements do — otherwise "", null, undefined
                     ;; and NaN (all non-nil values) test as truthy in `c ? a : b'.
                     (setf lhs (make-ast-if :cond (%js-truthy-call lhs)
                                            :then then-ast :else else-ast)
                           rest rest5))))))
            ;; Comma operator
            ((eq op-type :T-COMMA)
             (when (> prec min-prec)
               (multiple-value-bind (tok rest2) (js-consume rest)
                 (declare (ignore tok))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 1)
                   (setf lhs (make-ast-progn :forms (list lhs rhs))
                         rest rest3)))))
            ;; Assignment operators (right-associative)
            ((and (eq op-type :T-OP)
                  (member op-val '("=" "+=" "-=" "*=" "/=" "%=" "**="
                                   "<<=" ">>=" ">>>=" "&=" "|=" "^="
                                   "&&=" "||=" "??=")
                          :test #'string=))
             ;; Only assign if prec > min-prec (for right-assoc, use >= on rhs)
             (when (> prec min-prec)
               (multiple-value-bind (tok rest2) (js-consume rest)
                 (declare (ignore tok))
                 (multiple-value-bind (rhs rest3)
                     (js-parse-expr rest2 (if right-assoc-p (1- prec) prec))
                   (setf lhs (%js-lower-assignment op-val lhs rhs)
                         rest rest3)))))
            ;; instanceof / in (keyword tokens, not :T-OP)
            ((or (eq op-type :T-INSTANCEOF) (eq op-type :T-IN))
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (let ((next-prec (if right-assoc-p (1- prec) prec)))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 next-prec)
                   (setf lhs (%js-lower-binary op-val lhs rhs)
                         rest rest3)))))
            ;; All other binary operators
            (t
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (let ((next-prec (if right-assoc-p (1- prec) prec)))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 next-prec)
                   (setf lhs (%js-lower-binary op-val lhs rhs)
                         rest rest3)))))))))
    (values lhs rest))))

(defun %js-lower-assignment (op-val lhs rhs)
  "Lower an assignment expression LHS op RHS to the appropriate AST."
  (cond
    ;; Simple variable assignment
    ((ast-var-p lhs)
     (let ((var-sym (ast-var-name lhs)))
       (cond
         ((string= op-val "=")
          (make-ast-setq :var var-sym :value rhs))
         ;; Logical assign
         ((member op-val '("&&=" "||=" "??=") :test #'string=)
          (%js-lower-logical-assign op-val var-sym rhs))
         ;; Compound assign
         (t
          (make-ast-setq :var var-sym
                         :value (%js-compound-rhs op-val lhs rhs))))))
    ;; Property assignment: obj.prop = val or obj[key] = val
    ((and (ast-call-p lhs)
          (ast-var-p (ast-call-func lhs))
          (eq (ast-var-name (ast-call-func lhs)) '%js-get-prop))
     (let ((obj (first  (ast-call-args lhs)))
           (key (second (ast-call-args lhs))))
       (cond
         ((string= op-val "=")
          (%js-call '%js-set-prop obj key rhs))
         (t
          ;; Compound prop assign: obj.k op= rhs → obj.k = (obj.k op rhs)
          (let ((obj-tmp (gensym "JS-OBJ-"))
                (key-tmp (gensym "JS-KEY-")))
            (make-ast-let
             :bindings (list (cons obj-tmp obj) (cons key-tmp key))
             :body (list (%js-call '%js-set-prop
                                   (make-ast-var :name obj-tmp)
                                   (make-ast-var :name key-tmp)
                                   (%js-compound-rhs op-val lhs rhs)))))))))
    ;; Private field assignment
    ((and (ast-call-p lhs)
          (ast-var-p (ast-call-func lhs))
          (eq (ast-var-name (ast-call-func lhs)) '%js-class-private-field-get))
     (let ((obj (first  (ast-call-args lhs)))
           (key (second (ast-call-args lhs))))
       (%js-call '%js-class-private-field-set obj key rhs)))
    ;; Destructuring assignment (array or object pattern) — lower to runtime helper
    (t
     (%js-call '%js-assign-pattern lhs rhs))))

(defun js-parse-assignment-expr (stream)
  "Like js-parse-expr but stops at comma (min-prec = 2).
Use for function arguments and array/object elements."
  (js-parse-expr stream 2))

;;; ─── Entry Point Helpers ─────────────────────────────────────────────────────

(defun js-parse-expression-from-tokens (tokens)
  "Parse a single expression from a token list. Returns (values ast remaining)."
  (js-parse-expr tokens 0))
