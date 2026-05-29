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

(defun js-tok-type  (tok) (getf tok :type))
(defun js-tok-value (tok) (getf tok :value))

(defun js-peek       (stream) (car stream))
(defun js-peek-type  (stream) (when stream (js-tok-type  (car stream))))
(defun js-peek-value (stream) (when stream (js-tok-value (car stream))))

(defun js-consume (stream)
  "Return (values token rest)."
  (values (car stream) (cdr stream)))

(defun js-expect (type stream &optional value)
  "Consume one token of TYPE (optionally matching VALUE).
  Signals an error on mismatch."
  (if (and stream
           (eq (js-peek-type stream) type)
           (or (null value) (equal (js-peek-value stream) value)))
      (js-consume stream)
      (error "JS parse error: expected ~S~@[ ~S~] but got ~S"
             type value (js-peek stream))))

(defun %js-consume-expected (type stream)
  "Like js-expect but returns only the rest stream (discards the token)."
  (nth-value 1 (js-expect type stream)))

(defun js-at-eof-p (stream)
  "Return T when STREAM is exhausted or at :T-EOF."
  (or (null stream) (eq (js-peek-type stream) :T-EOF)))

(defun js-skip-semis (stream)
  "Skip zero or more semicolons (including ASI — automatic semicolon insertion)."
  (loop while (and stream (eq (js-peek-type stream) :T-SEMI))
        do (setf stream (cdr stream)))
  stream)

(defun js-ident-sym (name)
  "Intern a JS identifier NAME as a symbol in :cl-cc/javascript."
  (intern (string-upcase name) :cl-cc/javascript))

;;; ─── Dynamic Variables for Loop/Switch Control Flow ─────────────────────────

(defvar *js-loop-continue-target* nil
  "Dynamically bound innermost loop continue target tag (gensym).")

(defvar *js-loop-break-target* nil
  "Dynamically bound innermost loop/switch break target tag (gensym).")

(defvar *js-break-targets* nil
  "Stack of break targets for nested loops/switch, innermost first.")

(defvar *js-continue-targets* nil
  "Stack of continue targets for nested loops, innermost first.")

(defvar *js-strict-mode* nil
  "Dynamically bound: T when parsing in strict mode.")

(defvar *js-module-mode* nil
  "Dynamically bound: T when parsing a module (implies strict mode).")

;;; ─── Statement Dispatcher Table ──────────────────────────────────────────────

(defvar *js-stmt-parsers* (make-hash-table)
  "Maps token type keywords to statement parser functions.
  Each entry is a function (stream) -> (values ast rest).")

(defmacro define-js-stmt-parser (token-type (stream) &body body)
  "Register a statement parser for TOKEN-TYPE in *js-stmt-parsers*.
  The function receives STREAM positioned AFTER the keyword token was consumed."
  `(setf (gethash ,token-type *js-stmt-parsers*)
         (lambda (,stream) ,@body)))

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
    (values (make-ast-progn :forms (nreverse stmts))
            (%js-consume-expected :T-RBRACE current)))))

(defun %js-parse-stmt-body (stream)
  "Parse either a braced block or a single statement.
  Returns (values ast rest)."
  (if (eq (js-peek-type stream) :T-LBRACE)
      (js-parse-block stream)
      (js-parse-stmt stream)))

;;; ─── Variable / Binding Pattern Helpers ─────────────────────────────────────

(defun %js-binding-sym (name)
  "Intern JS binding name string as a symbol."
  (intern (concatenate 'string "JS." (string-upcase name))
          :cl-cc/javascript))

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
                          (push (list key-name local-sym2) keys)
                          (setf current rest2)))
                      (progn
                        (push (list key-name local-sym) keys)
                        (setf current rest)))))))
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
                (push sym elements)
                (setf current rest))))
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
                   ;; named property
                   (let ((key   (first field))
                         (local (second field)))
                     (push (cons (%js-binding-to-sym local)
                                 (make-ast-call
                                  :func (make-ast-var :name '%js-get-prop)
                                  :args (list (make-ast-var :name tmp)
                                              (make-ast-quote :value key))))
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
                                    :body body-stmts)
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
  BODY-FN-NAME is called by the caller to parse the body."
  (declare (ignore body-fn-name))
  (let ((var-sym  (%js-binding-to-sym binding))
        (iter-sym (gensym "FOR-ITER-")))
    (lambda (body-ast)
      (let ((body-stmts (if (ast-progn-p body-ast)
                            (ast-progn-forms body-ast)
                            (list body-ast))))
        (make-ast-let
         :bindings (list (cons iter-sym iter-expr))
         :body (list (%js-lower-while-with-tags
                      (%js-truthy-call (make-ast-var :name iter-sym))
                      (list (make-ast-let
                             :bindings (list (cons var-sym
                                                   (make-ast-call
                                                    :func (make-ast-var :name 'car)
                                                    :args (list (make-ast-var :name iter-sym)))))
                             :body (append body-stmts
                                           (list (make-ast-setq
                                                  :var iter-sym
                                                  :value (make-ast-call
                                                          :func (make-ast-var :name 'cdr)
                                                          :args (list (make-ast-var :name iter-sym))))))))
                      loop-tag end-tag)))))))

(defun js-parse-for-stmt (stream)
  "Handle for(init;cond;update){}, for(var x in obj){},
  for(var x of iter){}, for await(var x of asyncIter){}.
  Returns (values ast rest)."
  ;; Check for 'await' keyword (for-await-of)
  (let ((await-p nil)
        (current stream))
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
                                                :func (make-ast-var :name '%js-for-in)
                                                :args (list obj-expr))
                                               nil loop-tag end-tag)
                                              body-ast)))
                          (setf (ast-let-declarations lower) (list kind))
                          (values lower rest4))))))
                 ;; for (var x of iter) { } / for await (var x of iter) { }
                 ((eq iter-kw :T-OF)
                  (multiple-value-bind (iter-expr rest3) (js-parse-expr (cdr rest2))
                    (setf rest3 (%js-consume-expected :T-RPAREN rest3))
                    (let* ((of-fn (if await-p '%js-for-await-of '%js-for-of))
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
                  ;; rest2 is at the =/; of the init binding
                  ;; Parse optional init value
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

;;; ─── Break / Continue ────────────────────────────────────────────────────────

(defun js-parse-break-stmt (stream)
  "Parse break [label];. Returns (values ast rest)."
  ;; Check for labelled break (not yet supported — go to innermost break target)
  (let ((current stream)
        (target nil))
    ;; Optional label on same line
    (when (and current (eq (js-peek-type current) :T-IDENT))
      ;; Labelled break: skip label, use break target (simplified)
      (setf current (cdr current)))
    (setf current (js-skip-semis current))
    (setf target (or (first *js-break-targets*) *js-loop-break-target*))
    (unless target
      (error "JS parse error: break has no matching loop or switch"))
    (values (make-ast-go :tag target) current)))

(defun js-parse-continue-stmt (stream)
  "Parse continue [label];. Returns (values ast rest)."
  (let ((current stream)
        (target nil))
    ;; Optional label on same line
    (when (and current (eq (js-peek-type current) :T-IDENT))
      ;; Labelled continue: skip label, use continue target (simplified)
      (setf current (cdr current)))
    (setf current (js-skip-semis current))
    (setf target (or (first *js-continue-targets*) *js-loop-continue-target*))
    (unless target
      (error "JS parse error: continue has no matching loop"))
    (values (make-ast-go :tag target) current)))

;;; ─── Return Statement ────────────────────────────────────────────────────────

(defun js-parse-return-stmt (stream)
  "Parse return [expr];. Returns (values ast rest)."
  (if (or (js-at-eof-p stream)
          (eq (js-peek-type stream) :T-SEMI)
          (eq (js-peek-type stream) :T-RBRACE))
      (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
              (js-skip-semis stream))
      (multiple-value-bind (expr rest) (js-parse-expr stream)
        (values (make-ast-return-from :name nil :value expr)
                (js-skip-semis rest)))))

;;; ─── Throw Statement ─────────────────────────────────────────────────────────

(defun js-parse-throw-stmt (stream)
  "Parse throw expr; -> (values (%js-throw expr) rest)."
  (multiple-value-bind (expr rest) (js-parse-expr stream)
    (values (make-ast-call :func (make-ast-var :name '%js-throw)
                           :args (list expr))
            (js-skip-semis rest))))

;;; ─── Try / Catch / Finally ───────────────────────────────────────────────────

(defun js-parse-try-stmt (stream)
  "Parse try {} catch(e) {} finally {} .
  Lowers to ast-unwind-protect wrapping a %js-try-catch-finally call.
  Returns (values ast rest)."
  (multiple-value-bind (try-ast rest) (js-parse-block stream)
    (let ((try-body (ast-progn-forms try-ast))
          (catch-clauses nil)
          (finally-body nil)
          (finally-present nil)
          (current rest))
      ;; Parse zero or more catch clauses
      (loop while (and current (eq (js-peek-type current) :T-CATCH))
            do (setf current (cdr current))
               (let ((var-sym nil))
                 ;; Optional catch binding: catch (e)
                 (when (eq (js-peek-type current) :T-LPAREN)
                   (setf current (cdr current))
                   (when (eq (js-peek-type current) :T-IDENT)
                     (multiple-value-bind (tok rest2) (js-consume current)
                       (setf var-sym (%js-binding-sym (js-tok-value tok))
                             current rest2)))
                   (setf current (%js-consume-expected :T-RPAREN current)))
                 (multiple-value-bind (catch-ast rest2) (js-parse-block current)
                   (push (list var-sym (ast-progn-forms catch-ast)) catch-clauses)
                   (setf current rest2))))
      ;; Optional finally clause. Track PRESENCE separately: an empty `finally {}`
      ;; has a nil body but is still a valid finally clause.
      (when (and current (eq (js-peek-type current) :T-FINALLY))
        (setf current (cdr current)
              finally-present t)
        (multiple-value-bind (finally-ast rest2) (js-parse-block current)
          (setf finally-body (ast-progn-forms finally-ast)
                current rest2)))
      (unless (or catch-clauses finally-present)
        (error "JS parse error: try must have catch or finally"))
      ;; Lower to ast-unwind-protect with catch dispatch
      (let* ((err-sym (gensym "JS-ERR-"))
             (clauses (nreverse catch-clauses))
             ;; Build catch dispatch: chain of let/progn for each clause
             (catch-dispatch
              (if clauses
                  ;; For simplicity, use the first catch binding (JS has one catch)
                  (let* ((clause (first clauses))
                         (var    (first clause))
                         (body   (second clause)))
                    (if var
                        (make-ast-let :bindings (list (cons var (make-ast-var :name err-sym)))
                                      :body body)
                        (make-ast-progn :forms body)))
                  (make-ast-quote :value nil)))
             ;; Wrapped try body with catch
             (protected
              (make-ast-call
               :func (make-ast-var :name '%js-try-catch-finally)
               :args (list (make-ast-lambda :params nil :body try-body)
                           (make-ast-lambda :params (list err-sym)
                                            :body (list catch-dispatch))
                           (make-ast-lambda :params nil
                                            :body (or finally-body
                                                      (list (make-ast-quote :value nil))))))))
        (values protected current)))))

;;; ─── Debugger Statement ──────────────────────────────────────────────────────

(defun js-parse-debugger-stmt (stream)
  "Parse debugger; -> (values (%js-debugger) rest)."
  (values (make-ast-call :func (make-ast-var :name '%js-debugger)
                         :args nil)
          (js-skip-semis stream)))

;;; ─── Using Declaration (ES2025 Explicit Resource Management) ─────────────────

(defun js-parse-using-decl (stream)
  "Parse using x = expr (ES2025 explicit resource management).
  Lowers to ast-let + registration of disposable resource on scope exit.
  The disposal call is wrapped as a %js-using-register call at the binding site.
  Returns (values ast rest)."
  ;; 'using' is a contextual keyword: stream starts with the binding identifier
  (multiple-value-bind (name-tok rest) (js-expect :T-IDENT stream)
    (let ((var-sym (%js-binding-sym (js-tok-value name-tok))))
      (unless (and (eq (js-peek-type rest) :T-OP)
                   (equal (js-peek-value rest) "="))
        (error "JS parse error: expected '=' after identifier in using declaration, got ~S"
               (js-peek rest)))
      (let ((rest2 (cdr rest)))
        (multiple-value-bind (init-expr rest3) (js-parse-expr rest2)
          (values
           (make-ast-let
            :bindings (list (cons var-sym
                                  (make-ast-call
                                   :func (make-ast-var :name '%js-using-register)
                                   :args (list init-expr))))
            :declarations (list :js-using)
            :body nil)
           (js-skip-semis rest3)))))))

;;; ─── Main Statement Dispatcher ───────────────────────────────────────────────

(defun js-parse-stmt (stream)
  "Main statement dispatcher.
  Returns (values ast rest)."
  (setf stream (js-skip-semis stream))
  (when (js-at-eof-p stream)
    (return-from js-parse-stmt (values nil stream)))
  (let ((type  (js-peek-type  stream))
        (value (js-peek-value stream)))
    (cond
      ;; Braced block
      ((eq type :T-LBRACE)
       (js-parse-block stream))
      ;; var / let / const declarations
      ((eq type :T-VAR)
       (js-parse-var-decl (cdr stream) :var))
      ((eq type :T-LET)
       (js-parse-var-decl (cdr stream) :let))
      ((eq type :T-CONST)
       (js-parse-var-decl (cdr stream) :const))
      ;; function declaration (function* generator handled inside js-parse-function-decl)
      ((eq type :T-FUNCTION)
       (js-parse-function-decl (cdr stream)))
      ;; async function / async arrow
      ((and (eq type :T-ASYNC)
            (eq (js-peek-type (cdr stream)) :T-FUNCTION))
       (js-parse-function-decl (cddr stream) :async-p t))
      ;; class declaration
      ((eq type :T-CLASS)
       (multiple-value-bind (ast-list rest) (js-parse-class-decl (cdr stream))
         ;; js-parse-class-decl returns a LIST of nodes; wrap >1 in a progn so the
         ;; caller always receives a single statement node.
         (values (if (and (consp ast-list) (= (length ast-list) 1))
                     (first ast-list)
                     (make-ast-progn :forms ast-list))
                 rest)))
      ;; decorated class declaration: @dec class { ... }
      ((eq type :T-AT)
       (multiple-value-bind (decorators rest) (%js-parse-decorators stream)
         (if (eq (js-peek-type rest) :T-CLASS)
             (multiple-value-bind (ast-list rest2)
                 (js-parse-class-decl (cdr rest) :decorators decorators)
               (values (if (and (consp ast-list) (= (length ast-list) 1))
                           (first ast-list)
                           (make-ast-progn :forms ast-list))
                       rest2))
             (error "JS parse error: decorators must precede a class declaration"))))
      ;; import declaration ('import' keyword consumed; parser expects next token)
      ((eq type :T-IMPORT)
       (js-parse-import-decl (cdr stream)))
      ;; export declaration ('export' keyword consumed)
      ((eq type :T-EXPORT)
       (js-parse-export-decl (cdr stream)))
      ;; if
      ((eq type :T-IF)
       (js-parse-if-stmt (cdr stream)))
      ;; while
      ((eq type :T-WHILE)
       (js-parse-while-stmt (cdr stream)))
      ;; do-while
      ((eq type :T-DO)
       (js-parse-do-while-stmt (cdr stream)))
      ;; for
      ((eq type :T-FOR)
       (js-parse-for-stmt (cdr stream)))
      ;; switch
      ((eq type :T-SWITCH)
       (js-parse-switch-stmt (cdr stream)))
      ;; return
      ((eq type :T-RETURN)
       (js-parse-return-stmt (cdr stream)))
      ;; break
      ((eq type :T-BREAK)
       (js-parse-break-stmt (cdr stream)))
      ;; continue
      ((eq type :T-CONTINUE)
       (js-parse-continue-stmt (cdr stream)))
      ;; throw
      ((eq type :T-THROW)
       (js-parse-throw-stmt (cdr stream)))
      ;; try
      ((eq type :T-TRY)
       (js-parse-try-stmt (cdr stream)))
      ;; debugger
      ((eq type :T-DEBUGGER)
       (js-parse-debugger-stmt (cdr stream)))
      ;; using x = expr (ES2025, contextual)
      ((and (eq type :T-USING)
            (eq (js-peek-type (cdr stream)) :T-IDENT))
       (js-parse-using-decl (cdr stream)))
      ;; Labelled statement: ident : stmt
      ((and (eq type :T-IDENT)
            (eq (js-peek-type (cdr stream)) :T-COLON))
       (let* ((label-sym (js-ident-sym value))
              (rest (cddr stream)))           ; skip ident and colon
         (multiple-value-bind (stmt rest2) (js-parse-stmt rest)
           (values (make-ast-progn
                    :forms (list (%js-make-tagbody (list label-sym stmt))))
                   rest2))))
      ;; Expression statement (including assignments, calls, etc.)
      (t
       (multiple-value-bind (expr rest) (js-parse-expr stream)
         (values expr (js-skip-semis rest)))))))

;;; ─── Top-Level Statement List Parser ────────────────────────────────────────

(defun %js-parse-all-stmts (stream)
  "Parse all statements until EOF.
  Returns (values ast-list rest)."
  (let ((stmts nil)
        (current stream))
    (loop
      (setf current (js-skip-semis current))
      (when (js-at-eof-p current)
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        ;; Safety guard: a statement parser must consume at least one token.
        ;; If REST did not advance past CURRENT, signal rather than spin forever.
        (when (eq rest current)
          (error "JS parse error: no progress at ~S" (js-peek current)))
        (setf current rest)))
    (values (nreverse stmts) current)))

(defun js-parse-stmt-list (stream)
  "Parse statements until a closing } (which is consumed) or EOF.
Returns (values stmt-list rest-after-rbrace). Used for function, method, and
block bodies; STREAM is positioned just after the opening {. This is the real
statement parser that js-parse-function-body (parser-expr.lisp) dispatches to
via fboundp — previously absent, so bodies fell back to a token collector."
  (let ((stmts nil)
        (current stream))
    (loop
      (setf current (js-skip-semis current))
      (when (or (js-at-eof-p current)
                (eq (js-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest) (js-parse-stmt current)
        (when stmt (push stmt stmts))
        ;; Same no-progress guard as %js-parse-all-stmts: never spin forever.
        (when (eq rest current)
          (error "JS parse error: no progress in statement list at ~S"
                 (js-peek current)))
        (setf current rest)))
    (multiple-value-bind (_ rest) (js-expect :T-RBRACE current)
      (declare (ignore _))
      (values (nreverse stmts) rest))))

;;; ─── Public Entry Points ─────────────────────────────────────────────────────

(defun parse-js-source (source &key strict-mode module-p)
  "Parse JavaScript SOURCE string into a list of top-level AST nodes.
  STRICT-MODE: enable strict mode parsing.
  MODULE-P: treat as ES module (implies strict mode, enables import/export)."
  (let ((*js-strict-mode* (or strict-mode module-p))
        (*js-module-mode* module-p))
    (let ((tokens (tokenize-js-source source)))
      (multiple-value-bind (stmts _rest) (%js-parse-all-stmts tokens)
        (declare (ignore _rest))
        stmts))))

(defun parse-js-module (source)
  "Parse JavaScript SOURCE as an ES module.
  Implies strict mode and module semantics (import/export allowed)."
  (parse-js-source source :strict-mode t :module-p t))
