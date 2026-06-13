;;;; packages/javascript/src/parser-arrow.lisp — Arrow/Async/Paren/Class/Import Expression Parsers
;;;;
;;;; Helpers for complex primary expressions that require expression recursion:
;;;;   - Parenthesized expression / arrow function  (%js-parse-paren-or-arrow)
;;;;   - Arrow-function finishing                   (%js-finish-arrow-function)
;;;;   - Async expression / async arrow             (%js-parse-async-expr, %js-finish-async-arrow)
;;;;   - Class expression                           (%js-parse-class-expr)
;;;;   - Dynamic import()                           (%js-parse-import-expr)
;;;;
;;;; These are called by js-parse-primary (defined in parser-expr-primary.lisp).
;;;; Load order: after parser-expr-primary.lisp (CL resolves calls at runtime,
;;;; so the forward-reference from js-parse-primary to these helpers is fine).

(in-package :cl-cc/javascript)

;;; ─── Parenthesized Expression / Arrow Function ───────────────────────────────

(defun %js-call-callee-name (ast)
  "If AST is (ast-call (ast-var NAME) …), return NAME; else NIL."
  (when (and (ast-call-p ast) (ast-var-p (ast-call-func ast)))
    (ast-var-name (ast-call-func ast))))

(defun %js-spread-array-elements (expr)
  "If EXPR is the spread lowering of an array literal — (apply #'%js-make-array
(append (list e1) (%js-spread r) …)) — return the original element ASTs (a plain
element, or a (%js-spread …) marker for ...rest), else NIL.  Reverses
%js-spread-list-expr so [a,...r] arrow params can be recovered."
  (when (and (ast-apply-p expr) (eq (ast-apply-func expr) '%js-make-array))
    (let ((append-call (first (ast-apply-args expr))))
      (when (eq (%js-call-callee-name append-call) 'append)
        (mapcar (lambda (item)
                  (if (%js-spread-marker-p item)
                      item                              ; keep ...rest marker
                      (first (ast-call-args item))))    ; (list e) -> e
                (ast-call-args append-call))))))

(defun %js-object-fold-fields (expr)
  "Recover object-pattern fields from the spread-fold lowering of {…, ...rest}:
a left-nested chain of (%js-object-spread-set acc key val) / (%js-object-assign
acc src) over (%js-make-object).  Returns the fields in source order
((key local nil) … (:rest sym)), or :fail if EXPR is not such a chain."
  (let ((callee (%js-call-callee-name expr)))
    (cond
      ((eq callee '%js-make-object) nil)
      ((member callee '(%js-object-spread-set %js-object-assign))
       (let* ((args   (ast-call-args expr))
              (prefix (%js-object-fold-fields (first args))))
         (if (eq prefix :fail)
             :fail
             (append prefix
                     (list (if (eq callee '%js-object-spread-set)
                               (list (and (ast-quote-p (second args))
                                          (ast-quote-value (second args)))
                                     (%js-expr-to-binding-pattern (third args))
                                     nil)
                               (list :rest (%js-expr-to-binding-pattern (second args)))))))))
      (t :fail))))

(defun %js-expr-to-binding-pattern (expr)
  "Convert an arrow CoverGrammar parameter EXPR — already parsed as an expression
— into a destructuring binding pattern for %js-emit-destructure-bindings, or NIL
if EXPR is not a pattern.  A plain var becomes its symbol; an array literal
(%js-make-array …, or its (apply #'%js-make-array (append …)) spread form) becomes
an :array-pattern (holes/spread/nesting preserved); an object literal
(%js-make-object k v …, or its spread-fold chain) becomes an :object-pattern.
This is how ([a,b])=>…, ([a,...r])=>… and ({x,...rest})=>… recover their patterns,
since the paren contents were parsed as a literal before the => was seen."
  (cond
    ((ast-var-p expr) (ast-var-name expr))
    ;; array literal, with or without spread
    ((or (eq (%js-call-callee-name expr) '%js-make-array)
         (and (ast-apply-p expr) (eq (ast-apply-func expr) '%js-make-array)))
     (let ((els (if (ast-apply-p expr)
                    (%js-spread-array-elements expr)
                    (ast-call-args expr))))
       (list :array-pattern (gensym "ARR-DEST-")
             (mapcar
              (lambda (el)
                (cond
                  ((and (ast-quote-p el) (eq (ast-quote-value el) :js-hole)) :hole)
                  ((%js-spread-marker-p el)
                   (list :rest (%js-expr-to-binding-pattern (first (ast-call-args el)))))
                  (t (%js-expr-to-binding-pattern el))))
              els))))
    ;; object literal without spread
    ((eq (%js-call-callee-name expr) '%js-make-object)
     (list :object-pattern (gensym "OBJ-DEST-")
           (loop for (k v) on (ast-call-args expr) by #'cddr
                 collect (list (and (ast-quote-p k) (ast-quote-value k))
                               (%js-expr-to-binding-pattern v)
                               nil))))
    ;; object literal with spread (...rest) — recovered from the fold chain
    ((member (%js-call-callee-name expr) '(%js-object-spread-set %js-object-assign))
     (let ((fields (%js-object-fold-fields expr)))
       (unless (eq fields :fail)
         (list :object-pattern (gensym "OBJ-DEST-") fields))))
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

