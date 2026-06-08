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
;;; js-tok-type, js-tok-value, js-peek*, js-consume, js-expect, js-at-eof-p,
;;; js-ident-sym, and js-try-consume are defined in parser.lisp (loaded first).

(defun %js-consume-expected (type stream)
  "Like js-expect but returns only the rest stream (discards the token)."
  (nth-value 1 (js-expect type stream)))

(defun js-skip-semis (stream)
  "Skip zero or more semicolons in a loop (unlike js-skip-semi which skips one)."
  (loop while (and stream (eq (js-peek-type stream) :T-SEMI))
        do (setf stream (cdr stream)))
  stream)

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

(defun %js-binding-sym (name)
  "Intern a JS binding name (param, let/const/var, destructured name) as a symbol
in :cl-cc/javascript — using the SAME scheme as js-ident-sym so a binding and its
references resolve to the identical symbol.

Previously this prefixed names with `JS.', so a parameter or local bound the
symbol JS.X while the body referenced X (via js-ident-sym). Every `return param'
or `console.log(localVar)' then hit an unbound variable, the enclosing function
body failed to compile, the top-level handler-case silently dropped the defun,
and calls reported `Undefined function'. The package already namespaces JS
symbols, so the prefix was redundant as well as desynchronized."
  (intern (string-upcase (if (stringp name) name (symbol-name name)))
          :cl-cc/javascript))

(defun %js-parse-pattern-default (stream)
  "If STREAM begins with `= expr`, consume it and return (values default-ast rest);
otherwise return (values nil stream). Used for destructuring defaults like
[a = 1] and {a = 1}."
  (if (and stream
           (eq (js-peek-type stream) :T-OP)
           (equal (js-peek-value stream) "="))
      (multiple-value-bind (_tok rest) (js-consume stream)
        (declare (ignore _tok))
        (js-parse-assignment-expr rest))
      (values nil stream)))

(defun %js-default-access (access-expr default-ast)
  "Wrap ACCESS-EXPR so it yields DEFAULT-AST when the access is undefined,
matching JS destructuring-default semantics. Returns ACCESS-EXPR unchanged when
DEFAULT-AST is nil."
  (if default-ast
      (make-ast-if
       :cond (make-ast-call
              :func (make-ast-var :name '%js-strict-eq)
              :args (list access-expr (make-ast-quote :value :js-undefined)))
       :then default-ast
       :else access-expr)
      access-expr))

(defun %js-parse-binding-pattern (stream)
  "Parse a destructuring pattern or simple identifier.
  Returns (values sym/pattern rest).
  Handles: ident, {a,b,...}, [a,b,...] — simplified to %js-destructure-object/array calls."
  (let ((type (js-peek-type stream)))
    (cond
      ;; Simple identifier
      ((eq type :T-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (%js-binding-sym (js-tok-value tok)) rest)))
      ;; Object destructuring: {a, b: c, ...rest}
      ((eq type :T-LBRACE)
       (let ((current (cdr stream))
             (keys nil))
         (loop
           (setf current (js-skip-semis current))
           (when (or (js-at-eof-p current)
                     (eq (js-peek-type current) :T-RBRACE))
             (return))
           (cond
             ;; Rest element: ...rest
             ((eq (js-peek-type current) :T-ELLIPSIS)
              (setf current (cdr current))
              (multiple-value-bind (tok rest) (js-consume current)
                (push (list :rest (%js-binding-sym (js-tok-value tok))) keys)
                (setf current rest)))
             ;; key: binding or shorthand key
             (t
              (multiple-value-bind (key-tok rest) (js-consume current)
                (let* ((key-name (js-tok-value key-tok))
                       (local-sym (%js-binding-sym key-name)))
                  (if (and rest (eq (js-peek-type rest) :T-COLON))
                      (progn
                        (setf rest (cdr rest))
                        (multiple-value-bind (local-sym2 rest2)
                            (%js-parse-binding-pattern rest)
                          ;; key: pattern [= default]
                          (multiple-value-bind (dflt rest3) (%js-parse-pattern-default rest2)
                            (push (list key-name local-sym2 dflt) keys)
                            (setf current rest3))))
                      ;; shorthand {key [= default]}
                      (multiple-value-bind (dflt rest2) (%js-parse-pattern-default rest)
                        (push (list key-name local-sym dflt) keys)
                        (setf current rest2)))))))
           (when (eq (js-peek-type current) :T-COMMA)
             (setf current (cdr current))))
         (setf current (%js-consume-expected :T-RBRACE current))
         ;; Return a gensym for the binding; destructuring is emitted as a let
         (let ((tmp (gensym "OBJ-DEST-")))
           (values (list :object-pattern tmp (nreverse keys)) current))))
      ;; Array destructuring: [a, b, ...rest]
      ((eq type :T-LBRACKET)
       (let ((current (cdr stream))
             (elements nil))
         (loop
           (setf current (js-skip-semis current))
           (when (or (js-at-eof-p current)
                     (eq (js-peek-type current) :T-RBRACKET))
             (return))
           (cond
             ;; Elision (hole): ,
             ((eq (js-peek-type current) :T-COMMA)
              (push :hole elements))
             ;; Rest element: ...rest
             ((eq (js-peek-type current) :T-ELLIPSIS)
              (setf current (cdr current))
              (multiple-value-bind (tok rest) (js-consume current)
                (push (list :rest (%js-binding-sym (js-tok-value tok))) elements)
                (setf current rest)))
             (t
              (multiple-value-bind (sym rest) (%js-parse-binding-pattern current)
                ;; element [= default]
                (multiple-value-bind (dflt rest2) (%js-parse-pattern-default rest)
                  (push (if dflt (list :default sym dflt) sym) elements)
                  (setf current rest2)))))
           (when (eq (js-peek-type current) :T-COMMA)
             (setf current (cdr current))))
         (setf current (%js-consume-expected :T-RBRACKET current))
         (let ((tmp (gensym "ARR-DEST-")))
           (values (list :array-pattern tmp (nreverse elements)) current))))
      (t
       (error "JS parse error: expected binding pattern, got ~S" (js-peek stream))))))

(defun %js-binding-to-sym (binding-or-sym)
  "Extract the primary gensym from a binding pattern or plain symbol."
  (if (listp binding-or-sym)
      (second binding-or-sym)
      binding-or-sym))

(defun %js-emit-destructure-bindings (binding init-expr)
  "Emit let bindings for a destructuring BINDING initialized from INIT-EXPR.
  Returns (values bindings-alist extra-lets) where bindings-alist is
  ((sym . init-expr) ...) and extra-lets is a list of additional let wrappers."
  (if (not (listp binding))
      ;; Simple symbol
      (values (list (cons binding init-expr)) nil)
      (let ((kind (first binding))
            (tmp  (second binding))
            (desc (third binding)))
        (cond
          ((eq kind :object-pattern)
           ;; tmp = init-expr, then destructure fields
           (let ((bindings (list (cons tmp init-expr)))
                 (extras nil))
             (dolist (field desc)
               (if (eq (car field) :rest)
                   ;; rest property: emit %js-destructure-object call
                   (push (cons (second field)
                               (make-ast-call
                                :func (make-ast-var :name '%js-destructure-object)
                                :args (list (make-ast-var :name tmp)
                                            (make-ast-quote :value :rest))))
                         extras)
                   ;; named property, with optional default ((key local default))
                   (let ((key   (first field))
                         (local (second field))
                         (dflt  (third field)))
                     (push (cons (%js-binding-to-sym local)
                                 (%js-default-access
                                  (make-ast-call
                                   :func (make-ast-var :name '%js-get-prop)
                                   :args (list (make-ast-var :name tmp)
                                               (make-ast-quote :value key)))
                                  dflt))
                           extras))))
             (values (append bindings (nreverse extras)) nil)))
          ((eq kind :array-pattern)
           ;; tmp = init-expr, then destructure by index
           (let ((bindings (list (cons tmp init-expr)))
                 (idx 0))
             (dolist (elem desc)
               (cond
                 ((eq elem :hole)
                  (incf idx))
                 ((and (listp elem) (eq (car elem) :rest))
                  (push (cons (second elem)
                              (make-ast-call
                               :func (make-ast-var :name '%js-destructure-array)
                               :args (list (make-ast-var :name tmp)
                                           (make-ast-quote :value idx)
                                           (make-ast-quote :value :rest))))
                        bindings))
                 ;; element with default: (:default sym default-ast)
                 ((and (listp elem) (eq (car elem) :default))
                  (push (cons (%js-binding-to-sym (second elem))
                              (%js-default-access
                               (make-ast-call
                                :func (make-ast-var :name '%js-get-prop)
                                :args (list (make-ast-var :name tmp)
                                            (make-ast-quote :value idx)))
                               (third elem)))
                        bindings)
                  (incf idx))
                 (t
                  (push (cons (%js-binding-to-sym elem)
                              (make-ast-call
                               :func (make-ast-var :name '%js-get-prop)
                               :args (list (make-ast-var :name tmp)
                                           (make-ast-quote :value idx))))
                        bindings)
                  (incf idx))))
             (values (nreverse bindings) nil)))
          (t (values (list (cons binding init-expr)) nil))))))

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
        (let ((init-expr (make-ast-quote :value nil)))
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
    (values (make-ast-let :bindings all-bindings :body nil
                          :declarations (list kind))
            current)))

;;; ─── Function Declaration Parsing ───────────────────────────────────────────

(defun %js-parse-param-list (stream)
  "Parse (param, ...) parameter list.
  Returns (values params-list rest)."
  (let ((current (%js-consume-expected :T-LPAREN stream))
        (params nil))
    (loop
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RPAREN))
        (return))
      ;; Rest parameter: ...name
      (cond
        ((eq (js-peek-type current) :T-ELLIPSIS)
         (setf current (cdr current))
         (multiple-value-bind (tok rest) (js-consume current)
           (push (%js-binding-sym (js-tok-value tok)) params)
           (setf current rest)))
        (t
         (multiple-value-bind (binding rest) (%js-parse-binding-pattern current)
           (push (%js-binding-to-sym binding) params)
           (setf current rest)
           ;; Default parameter value: = expr (parsed but value is discarded at AST level)
           (when (and current
                      (eq (js-peek-type current) :T-OP)
                      (equal (js-peek-value current) "="))
             (setf current (cdr current))
             (multiple-value-bind (_default rest2) (js-parse-expr current)
               (declare (ignore _default))
               (setf current rest2))))))
      (when (eq (js-peek-type current) :T-COMMA)
        (setf current (cdr current))))
    (values (nreverse params)
            (%js-consume-expected :T-RPAREN current))))

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
      (multiple-value-bind (params rest) (%js-parse-param-list current)
        (multiple-value-bind (body-ast rest2) (js-parse-block rest)
          (let ((body-stmts (ast-progn-forms body-ast))
                (decls nil))
            (when async-p     (push :js-async     decls))
            (when generator-p (push :js-generator decls))
            (values (make-ast-defun :name (or fn-name (gensym "JS-FN-"))
                                    :params params
                                    :declarations (nreverse decls)
                                    :body (%js-callable-body body-stmts))
                    rest2)))))))

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
