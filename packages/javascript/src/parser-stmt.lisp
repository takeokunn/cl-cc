;;;; packages/javascript/src/parser-stmt.lisp — ES2026 JavaScript Parser: Statement Layer
;;;;
;;;; Parses a JavaScript token stream (list of plists) into the common AST.
;;;; Token format: (:type :T-XXX :value val)
;;;;
;;;; Loop lowering uses the block/tagbody/go pattern (same as PHP frontend).
;;;; All loop constructs are lowered at parse time rather than emitting
;;;; symbolic :while/:for calls, because the compiler resolves the pre-built
;;;; AST directly without a macro expansion pass for frontend ASTs.

(in-package :cl-cc/javascript)

;;; ─── Token Stream Helpers ────────────────────────────────────────────────────
;;; %js-consume-expected and js-skip-semis are in parser-stmt-binding.lisp.
;;; js-tok-type, js-tok-value, js-peek*, js-consume, js-expect, js-at-eof-p,
;;; js-ident-sym, and js-try-consume are defined in parser.lisp (loaded first).

;;; ─── Dynamic Variables for Loop/Switch Control Flow ─────────────────────────

(defvar *js-loop-continue-target* nil
  "Dynamically bound innermost loop continue target tag (gensym).")

(defvar *js-loop-break-target* nil
  "Dynamically bound innermost loop/switch break target tag (gensym).")

(defvar *js-break-targets* nil
  "Stack of break targets for nested loops/switch, innermost first.")

(defvar *js-continue-targets* nil
  "Stack of continue targets for nested loops, innermost first.")

(defvar *js-label-break-targets* (make-hash-table :test #'equal)
  "Maps JS label name strings to their break target tags (for labeled break).")

(defvar *js-label-continue-targets* (make-hash-table :test #'equal)
  "Maps JS label name strings to their continue target tags (for labeled continue).")

;;; *js-strict-mode* and *js-module-mode* are defined in parser.lisp (loaded first).

;;; ─── Statement Dispatcher Table ──────────────────────────────────────────────
;;;
;;; Each define-js-stmt-parser call registers a handler for one token type.
;;; The handler receives STREAM positioned AFTER the keyword token was consumed.
;;; js-parse-stmt consults this table first before falling back to complex cases.

(defvar *js-stmt-parsers* (make-hash-table)
  "Maps token type keywords to (lambda (after-keyword-stream)) statement parsers.")

(defmacro define-js-stmt-parser (token-type (stream) &body body)
  "Register a statement parser for TOKEN-TYPE in *js-stmt-parsers*.
  STREAM is positioned AFTER the keyword token; BODY must return (values ast rest)."
  `(setf (gethash ,token-type *js-stmt-parsers*)
         (lambda (,stream) ,@body)))

;;; Simple token-type → parser registrations (consume keyword, parse body)
(define-js-stmt-parser :T-VAR      (s) (js-parse-var-decl s :var))
(define-js-stmt-parser :T-LET      (s) (js-parse-var-decl s :let))
(define-js-stmt-parser :T-CONST    (s) (js-parse-var-decl s :const))
(define-js-stmt-parser :T-FUNCTION (s) (js-parse-function-decl s))
(define-js-stmt-parser :T-CLASS    (s)
  (multiple-value-bind (ast-list rest) (js-parse-class-decl s)
    (values (if (and (consp ast-list) (= (length ast-list) 1))
                (first ast-list)
                (make-ast-progn :forms ast-list))
            rest)))
(define-js-stmt-parser :T-IMPORT   (s) (js-parse-import-decl s))
(define-js-stmt-parser :T-EXPORT   (s) (js-parse-export-decl s))
(define-js-stmt-parser :T-IF       (s) (js-parse-if-stmt s))
(define-js-stmt-parser :T-WHILE    (s) (js-parse-while-stmt s))
(define-js-stmt-parser :T-DO       (s) (js-parse-do-while-stmt s))
(define-js-stmt-parser :T-FOR      (s) (js-parse-for-stmt s))
(define-js-stmt-parser :T-SWITCH   (s) (js-parse-switch-stmt s))
(define-js-stmt-parser :T-RETURN   (s) (js-parse-return-stmt s))
(define-js-stmt-parser :T-BREAK    (s) (js-parse-break-stmt s))
(define-js-stmt-parser :T-CONTINUE (s) (js-parse-continue-stmt s))
(define-js-stmt-parser :T-THROW    (s) (js-parse-throw-stmt s))
(define-js-stmt-parser :T-TRY      (s) (js-parse-try-stmt s))
(define-js-stmt-parser :T-DEBUGGER (s) (js-parse-debugger-stmt s))

;;; ─── Loop Lowering Helpers ───────────────────────────────────────────────────

(defun %js-make-tagbody (items)
  "Build an AST-TAGBODY from a flat list of tags (symbols) and form AST nodes.
  Symbols start new tag sections; AST nodes accumulate under the current tag."
  (let ((tags nil)
        (current-tag nil)
        (current-forms nil))
    (labels ((flush ()
               (when current-tag
                 (push (cons current-tag (nreverse current-forms)) tags))))
      (dolist (item items)
        (if (symbolp item)
            (progn
              (flush)
              (setf current-tag item current-forms nil))
            (progn
              (unless current-tag
                (setf current-tag (gensym "TAGBODY-")))
              (push item current-forms))))
      (flush)
      (make-ast-tagbody :tags (nreverse tags)))))

(defun %js-lower-while-loop (cond-expr body)
  "Lower while(cond){body} to block/tagbody/go AST.
  The loop tag serves as both the continue target and the back-edge."
  (let ((loop-tag (gensym "WHILE-"))
        (end-tag  (or *js-loop-break-target* (gensym "WHILE-END-"))))
    (make-ast-block :name nil
      :body (list (%js-make-tagbody
                   (append (list loop-tag
                                 (make-ast-if
                                  :cond cond-expr
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-return-from
                                         :name nil
                                         :value (make-ast-quote :value nil))))
                           body
                           (list (make-ast-go :tag loop-tag)
                                 end-tag)))))))

(defun %js-lower-while-with-tags (cond-expr body loop-tag end-tag)
  "Lower while with caller-supplied LOOP-TAG (continue) and END-TAG (break)."
  (make-ast-block :name nil
    :body (list (%js-make-tagbody
                 (append (list loop-tag
                               (make-ast-if
                                :cond cond-expr
                                :then (make-ast-quote :value nil)
                                :else (make-ast-return-from
                                       :name nil
                                       :value (make-ast-quote :value nil))))
                         body
                         (list (make-ast-go :tag loop-tag)
                               end-tag))))))

(defun %js-lower-do-while-with-tags (cond-expr body loop-tag end-tag)
  "Lower do{body}while(cond) with caller-supplied LOOP-TAG and END-TAG."
  (make-ast-block :name nil
    :body (list (%js-make-tagbody
                 (append (list loop-tag)
                         body
                         (list (make-ast-if
                                :cond cond-expr
                                :then (make-ast-go :tag loop-tag)
                                :else (make-ast-quote :value nil))
                               end-tag))))))

(defun %js-truthy-call (expr)
  "Wrap EXPR in a %js-truthy coercion call."
  (make-ast-call :func (make-ast-var :name '%js-truthy)
                 :args (list expr)))

;;; ─── Block Parsing ───────────────────────────────────────────────────────────

(defun js-parse-block (stream)
  "Parse { stmt... } and return (values ast-progn rest).
  The returned AST is an ast-progn wrapping all collected statements."
  (with-js-parse-depth
  (let ((current (%js-consume-expected :T-LBRACE stream))
        (stmts nil))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        (setf current rest)))
    (values (make-ast-progn :forms (%js-finish-let-bindings (nreverse stmts)))
            (%js-consume-expected :T-RBRACE current)))))

(defun %js-parse-stmt-body (stream)
  "Parse either a braced block or a single statement.
  Returns (values ast rest)."
  (if (eq (js-peek-type stream) :T-LBRACE)
      (js-parse-block stream)
      (js-parse-stmt stream)))

;;; ─── Variable / Binding Pattern Helpers ─────────────────────────────────────
;;; All binding helpers (%js-binding-sym, %js-parse-binding-pattern, etc.)
;;; and token helpers (%js-consume-expected, js-skip-semis) are in
;;; parser-stmt-binding.lisp (loaded before this file).

;;; ─── Variable Declaration Parsing ───────────────────────────────────────────

(defun js-parse-var-decl (stream kind)
  "Parse var/let/const declaration(s).
  KIND is :VAR :LET or :CONST.
  Handles: x, x = expr, {a,b}=obj, [a,b]=arr, and comma-separated lists.
  Returns (values ast rest)."
  (let ((current stream)
        (all-bindings nil))
    (loop
      ;; Parse one declarator
      (multiple-value-bind (binding rest) (%js-parse-binding-pattern current)
        (setf current rest)
        ;; A declarator with no initializer (`let x;') is undefined, NOT nil/false.
        ;; Binding it to nil made `typeof x' report "boolean" and `x === undefined'
        ;; false; the runtime undefined sentinel is :js-undefined.
        (let ((init-expr (make-ast-quote :value :js-undefined)))
          ;; Optional initializer: = expr
          (when (and current
                     (eq (js-peek-type current) :T-OP)
                     (equal (js-peek-value current) "="))
            (setf current (cdr current))
            ;; Use assignment-expr (NOT js-parse-expr) so the initializer stops at
            ;; the comma separating declarators — otherwise `const a = 1, b = 2`
            ;; parses `1, b = 2` as one comma-expression and drops the b binding.
            (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
              (setf init-expr expr
                    current rest2)))
          ;; Expand binding into bindings list
          (multiple-value-bind (bindings _extras)
              (%js-emit-destructure-bindings binding init-expr)
            (declare (ignore _extras))
            (setf all-bindings (append all-bindings bindings)))))
      ;; More declarators?
      (if (and current (eq (js-peek-type current) :T-COMMA))
          (setf current (cdr current))
          (return)))
    (setf current (js-skip-semis current))
    ;; A single empty-bodied let carrying all bindings. ast-let binds in PARALLEL,
    ;; but JS declarations are sequential (let*) — `let a = 1, b = a + 1' and
    ;; destructuring (a = tmp[0] must see tmp = init) both depend on it. The
    ;; block-finishing pass %js-finish-let-bindings expands a multi-binding
    ;; empty-bodied let into nested single-binding lets, giving that sequential
    ;; scoping (and scoping the bindings over the rest of the block).
    (values (make-ast-let :bindings all-bindings :body nil
                          :declarations (list kind))
            current)))

;;; ─── Function Declaration Parsing ───────────────────────────────────────────

(defun %js-parse-param-list (stream)
  "Parse (param, ...) parameter list.
  Returns (values params-list rest optionals rest-sym param-patterns), where
  OPTIONALS is an alist (sym . default-ast) for parameters with `= default',
  REST-SYM is the ...rest parameter symbol (or NIL), and PARAM-PATTERNS is an
  alist (param-gensym . binding-pattern) for destructuring parameters like
  function f([a,b]) / f({x,y}) — the caller prepends a destructuring let to the
  body that unpacks the gensym param into the pattern's names.  Callers binding
  only the first values are unaffected."
  (let ((current (%js-consume-expected :T-LPAREN stream))
        (params nil) (optionals nil) (rest-sym nil) (param-patterns nil))
    (loop
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RPAREN))
        (return))
      (cond
        ;; Rest parameter: ...name — the AST rest-param, not a positional one.
        ((eq (js-peek-type current) :T-ELLIPSIS)
         (setf current (cdr current))
         (multiple-value-bind (tok rest) (js-consume current)
           (setf rest-sym (%js-binding-sym (js-tok-value tok))
                 current rest))
         (return))                ; rest must be last
        (t
         (multiple-value-bind (binding rest) (%js-parse-binding-pattern current)
           (let ((sym (%js-binding-to-sym binding)))
             (push sym params)
             ;; A destructuring param (binding is a (:array-pattern|:object-pattern
             ;; gensym desc) list, not a bare symbol): record it so the caller can
             ;; unpack the gensym into the named locals at the top of the body.
             (when (listp binding)
               (push (cons sym binding) param-patterns))
             (setf current rest)
             ;; Default parameter value: = expr — recorded so the param becomes
             ;; an &optional with that default (was parsed and discarded).
             (when (and current
                        (eq (js-peek-type current) :T-OP)
                        (equal (js-peek-value current) "="))
               (setf current (cdr current))
               (multiple-value-bind (default-ast rest2) (js-parse-expr current)
                 (push (cons sym default-ast) optionals)
                 (setf current rest2)))))))
      (when (eq (js-peek-type current) :T-COMMA)
        (setf current (cdr current))))
    (values (nreverse params)
            (%js-consume-expected :T-RPAREN current)
            (nreverse optionals)
            rest-sym
            (nreverse param-patterns))))

(defun %js-prepend-param-destructuring (param-patterns body-stmts)
  "Prepend a destructuring let for each (param-gensym . binding-pattern) in
PARAM-PATTERNS to BODY-STMTS, so function f([a,b]){…} unpacks the gensym param
into a and b at the top of the body.  Each empty-bodied let is then nested over
the rest of the body via %js-finish-let-bindings — without that the unbodied let
fails to compile, the top-level handler-case drops the whole defun, and calls hit
'Undefined function'.  Re-running the pass is idempotent: already-bodied lets in
BODY-STMTS pass through unchanged."
  (if (null param-patterns)
      body-stmts
      (%js-finish-let-bindings
       (append
        (mapcar (lambda (pp)
                  (multiple-value-bind (bindings _extras)
                      (%js-emit-destructure-bindings (cdr pp)
                                                     (make-ast-var :name (car pp)))
                    (declare (ignore _extras))
                    (make-ast-let :bindings bindings :body nil
                                  :declarations (list :let))))
                param-patterns)
        body-stmts))))

(defun %js-split-params-by-defaults (params optionals)
  "Split PARAMS into (values required optional-entries). OPTIONALS is the
(sym . default-ast) alist. A parameter with a default — and every parameter after
it — becomes an optional entry (sym default-ast nil); JS gives a missing argument
`undefined', so a trailing parameter with no explicit default defaults to
undefined. This keeps required params as the positional prefix."
  (if (null optionals)
      (values params nil)
      (let ((first-opt (loop for p in params
                             when (assoc p optionals :test #'eq) return p)))
        (let ((required nil) (opts nil) (seen nil))
          (dolist (p params)
            (when (eq p first-opt) (setf seen t))
            (if seen
                (push (list p
                            (or (cdr (assoc p optionals :test #'eq))
                                (make-ast-quote :value cl-cc/javascript::+js-undefined+))
                            nil)
                      opts)
                (push p required)))
          (values (nreverse required) (nreverse opts))))))

(defun %js-rest-binding (rest-sym body-forms)
  "When REST-SYM is non-nil, return (values rest-param-sym wrapped-body): the AST
rest-param is a fresh gensym collecting the trailing args as a CL list, and the
body is wrapped in a let binding REST-SYM to that list converted to a JS array
(JS rest parameters are arrays). Otherwise (values nil BODY-FORMS)."
  (if rest-sym
      (let ((raw (gensym "JS-REST-")))
        (values raw
                (list (make-ast-let
                       :bindings (list (cons rest-sym
                                             (make-ast-call
                                              :func (make-ast-var :name 'cl-cc/javascript::%js-list-to-array)
                                              :args (list (make-ast-var :name raw)))))
                       :body body-forms))))
      (values nil body-forms)))

(defun js-parse-function-decl (stream &key async-p generator-p)
  "Parse function [*] name (params) { body }.
  Returns (values ast-defun rest).
  ASYNC-P and GENERATOR-P affect the :js-async / :js-generator declaration metadata."
  ;; Consume optional * for generators
  (let ((current stream))
    (when (and (eq (js-peek-type current) :T-OP)
               (equal (js-peek-value current) "*"))
      (setf generator-p t
            current (cdr current)))
    ;; Function name (optional for expressions, but required for declarations)
    (let ((fn-name nil))
      (when (eq (js-peek-type current) :T-IDENT)
        (multiple-value-bind (tok rest) (js-consume current)
          (setf fn-name (js-ident-sym (js-tok-value tok))
                current rest)))
      (multiple-value-bind (params rest optionals rest-sym param-patterns)
          (%js-parse-param-list current)
        (multiple-value-bind (body-ast rest2) (js-parse-block rest)
          (let ((body-stmts (%js-prepend-param-destructuring
                             param-patterns (ast-progn-forms body-ast)))
                (decls nil))
            (when async-p     (push :js-async     decls))
            (when generator-p (push :js-generator decls))
            (multiple-value-bind (required opts)
                (%js-split-params-by-defaults params optionals)
              (multiple-value-bind (rest-param body-forms)
                  (%js-rest-binding rest-sym (%js-callable-body body-stmts))
                ;; Generator/async function declarations: wrap body so calling the
                ;; defun returns the right runtime object.  Parameters captured by
                ;; closure so the zero-arg inner lambda sees them correctly.
                ;;   generator* f(n) { yield … }  →  body = [(%js-make-generator (lambda () …))]
                ;;   async function f(…) { … }     →  body = [(%js-async (lambda () …))]
                (let ((wrapped-body
                       (cond
                         (generator-p
                          (list (%js-call '%js-make-generator
                                         (make-ast-lambda :params nil
                                                          :body body-forms))))
                         (async-p
                          (list (%js-call '%js-async
                                         (make-ast-lambda :params nil
                                                          :body body-forms))))
                         (t body-forms))))
                  (values (make-ast-defun :name (or fn-name (gensym "JS-FN-"))
                                          :params required
                                          :optional-params opts
                                          :rest-param rest-param
                                          :declarations (nreverse decls)
                                          :body wrapped-body)
                          rest2))))))))))

;;; ─── If Statement ────────────────────────────────────────────────────────────

(defun %js-parse-if-tail (stream)
  "Parse the then-branch and optional else/else-if tail.
  Returns (values then-ast rest else-ast)."
  (multiple-value-bind (then-ast rest) (%js-parse-stmt-body stream)
    (let ((else-ast (make-ast-quote :value nil)))
      (when (and rest (eq (js-peek-type rest) :T-ELSE))
        (setf rest (cdr rest))
        (if (eq (js-peek-type rest) :T-IF)
            ;; else if — recurse
            (progn
              (setf rest (cdr rest))
              (let ((rest2 (%js-consume-expected :T-LPAREN rest)))
                (multiple-value-bind (cond-expr rest3) (js-parse-expr rest2)
                  (setf rest3 (%js-consume-expected :T-RPAREN rest3))
                  (multiple-value-bind (elseif-then rest4 elseif-else)
                      (%js-parse-if-tail rest3)
                    (setf else-ast (make-ast-if
                                    :cond (%js-truthy-call cond-expr)
                                    :then elseif-then
                                    :else elseif-else)
                          rest rest4)))))
            ;; plain else
            (multiple-value-bind (else-body rest2) (%js-parse-stmt-body rest)
              (setf else-ast else-body
                    rest rest2))))
      (values then-ast rest else-ast))))

(defun js-parse-if-stmt (stream)
  "Parse if (cond) stmt [else stmt]. Returns (values ast rest)."
  (let ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (multiple-value-bind (then-ast rest3 else-ast) (%js-parse-if-tail rest2)
        (values (make-ast-if :cond (%js-truthy-call cond-expr)
                             :then then-ast
                             :else else-ast)
                rest3)))))

;;; ─── While Statement ─────────────────────────────────────────────────────────

(defun js-parse-while-stmt (stream)
  "Parse while (cond) stmt. Returns (values ast rest)."
  (let ((rest (%js-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
      (setf rest2 (%js-consume-expected :T-RPAREN rest2))
      (let* ((loop-tag (gensym "WHILE-"))
             (end-tag  (gensym "WHILE-END-"))
             (*js-loop-continue-target* loop-tag)
             (*js-loop-break-target*    end-tag)
             (*js-break-targets*    (cons end-tag  *js-break-targets*))
             (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
        (multiple-value-bind (body-ast rest3) (%js-parse-stmt-body rest2)
          (let ((body-stmts (if (ast-progn-p body-ast)
                                (ast-progn-forms body-ast)
                                (list body-ast))))
            (values (%js-lower-while-with-tags
                     (%js-truthy-call cond-expr)
                     body-stmts
                     loop-tag end-tag)
                    rest3)))))))

;;; ─── Do-While Statement ──────────────────────────────────────────────────────

(defun js-parse-do-while-stmt (stream)
  "Parse do stmt while (cond);. Returns (values ast rest)."
  (let* ((loop-tag (gensym "DO-WHILE-"))
         (end-tag  (gensym "DO-WHILE-END-"))
         (*js-loop-continue-target* loop-tag)
         (*js-loop-break-target*    end-tag)
         (*js-break-targets*    (cons end-tag  *js-break-targets*))
         (*js-continue-targets* (cons loop-tag *js-continue-targets*)))
    (multiple-value-bind (body-ast rest) (%js-parse-stmt-body stream)
      (unless (eq (js-peek-type rest) :T-WHILE)
        (error "JS parse error: expected 'while' after do-body, got ~S" (js-peek rest)))
      (setf rest (cdr rest))           ; consume 'while'
      (setf rest (%js-consume-expected :T-LPAREN rest))
      (multiple-value-bind (cond-expr rest2) (js-parse-expr rest)
        (setf rest2 (%js-consume-expected :T-RPAREN rest2))
        (setf rest2 (js-skip-semis rest2))
        (let ((body-stmts (if (ast-progn-p body-ast)
                              (ast-progn-forms body-ast)
                              (list body-ast))))
          (values (%js-lower-do-while-with-tags
                   (%js-truthy-call cond-expr)
                   body-stmts
                   loop-tag end-tag)
                  rest2))))))

;;; ─── For Statement ───────────────────────────────────────────────────────────
;;; See parser-stmt-control.lisp for: for-loop lowering, switch, break/continue,
;;; return, throw, try/catch/finally, debugger, using, js-parse-stmt dispatcher,
;;; top-level list parsers, and public entry points parse-js-source/parse-js-module.
