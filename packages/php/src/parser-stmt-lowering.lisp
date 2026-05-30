;;;; packages/php/src/parser-stmt-lowering.lisp — PHP Parser: Statement infrastructure and AST lowering

(in-package :cl-cc/php)

(defvar *php-stmt-parsers* (make-hash-table)
  "Maps PHP keyword values to statement parser functions.")

(defmacro define-php-stmt-parser (keyword (stream known-vars) &body body)
  "Register a statement parser for KEYWORD in *php-stmt-parsers*."
  `(setf (gethash ,keyword *php-stmt-parsers*)
          (lambda (,stream ,known-vars) ,@body)))

;;; %php-consume-expected and %php-keyword-p are defined in parser.lisp

(defun %php-include-keyword-value (value)
  "Normalize include/require identifiers or keywords to statement keywords."
  (let ((name (string-downcase (if (symbolp value) (symbol-name value) (princ-to-string value)))))
    (cond ((string= name "include") :include)
          ((string= name "require") :require)
          ((member name '("include_once" "include-once") :test #'string=) :include-once)
          ((member name '("require_once" "require-once") :test #'string=) :require-once)
          ((string= name "declare") :declare)
          ((string= name "goto") :goto)
          ((string= name "namespace") :namespace)
          ((string= name "use") :use)
          (t nil))))

(defun %php-parse-paren-expr (stream known-vars)
  "Parse a parenthesized expression."
  (let ((rest (%php-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
      (values expr (%php-consume-expected :T-RPAREN rest2) kv2))))

(defun %php-parse-expr-stmt (stream known-vars)
  "Parse an expression statement."
  (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
    (values expr (php-skip-semis rest) kv)))

(defvar *php-loop-continue-target* nil
  "Dynamically bound innermost loop continue target.")

(defvar *php-loop-break-target* nil
  "Dynamically bound innermost loop/switch break target.")

(defvar *php-break-targets* nil
  "Dynamically bound stack of loop/switch break targets, innermost first.")

(defvar *php-continue-targets* nil
  "Dynamically bound stack of loop continue targets, innermost first.")

(defun %php-break-target (level)
  "Return the break target for LEVEL, where 1 means the innermost loop/switch."
  (let ((index (max 0 (1- (or level 1)))))
    (if level
        (nth index *php-break-targets*)
        (or (first *php-break-targets*)
            *php-loop-break-target*))))

(defun %php-continue-target (level)
  "Return the continue target for LEVEL, where 1 means the innermost loop."
  (let ((index (max 0 (1- (or level 1)))))
    (if level
        (nth index *php-continue-targets*)
        (or (first *php-continue-targets*)
            *php-loop-continue-target*))))

(defun %php-parse-control-level (stream)
  "Parse an optional integer break/continue LEVEL and return LEVEL and STREAM."
  (if (eq (php-peek-type stream) :T-INT)
      (values (php-peek-value stream) (cdr stream))
      (values nil stream)))

(defun %php-skip-to-stmt-end (stream)
  "Skip tokens until a top-level semicolon or block close."
  (let ((current stream) (paren-depth 0) (brace-depth 0) (bracket-depth 0))
    (loop while current
          for type = (php-peek-type current)
          do (cond
               ((and (eq type :T-SEMI)
                     (zerop paren-depth) (zerop brace-depth) (zerop bracket-depth))
                (return (php-skip-semis current)))
               ((and (eq type :T-RBRACE)
                     (zerop paren-depth) (zerop brace-depth) (zerop bracket-depth))
                (return current))
               ((eq type :T-LPAREN) (incf paren-depth) (setf current (cdr current)))
               ((eq type :T-RPAREN) (decf paren-depth) (setf current (cdr current)))
               ((eq type :T-LBRACE) (incf brace-depth) (setf current (cdr current)))
               ((eq type :T-RBRACE) (decf brace-depth) (setf current (cdr current)))
               ((eq type :T-LBRACKET) (incf bracket-depth) (setf current (cdr current)))
               ((eq type :T-RBRACKET) (decf bracket-depth) (setf current (cdr current)))
               (t (setf current (cdr current)))))
    current))

(defun %php-make-tagbody (items &optional initial-tag)
  "Build an AST-TAGBODY from flat tag/form ITEMS.
Symbols and integers start new tag sections; AST nodes are accumulated under
the current tag, matching the core CL lowerer's ast-tagbody representation."
  (let ((tags nil)
        (current-tag initial-tag)
        (current-forms nil))
    (labels ((flush-current ()
               (when current-tag
                 (push (cons current-tag (nreverse current-forms)) tags))))
      (dolist (item items)
        (if (or (symbolp item) (integerp item))
            (progn
              (flush-current)
              (setf current-tag item
                    current-forms nil))
            (progn
              (unless current-tag
                (setf current-tag (gensym "TAGBODY-START-")))
              (push item current-forms))))
      (flush-current)
      (make-ast-tagbody :tags (nreverse tags)))))

(defun php-parse-block (stream known-vars)
  "Parse { stmt* }."
  (let ((current (%php-consume-expected :T-LBRACE stream))
        (stmts nil)
        (kv known-vars))
    (loop
      (setf current (php-skip-semis current))
      (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement current kv)
        (when stmt (push stmt stmts))
        (setf current rest2 kv kv2)))
    (values (nreverse stmts) (%php-consume-expected :T-RBRACE current) kv)))

(defun %php-parse-namespace-block-body (stream known-vars namespace-name)
  "Parse a braced namespace body and annotate each enclosed top-level form."
  (let ((*php-current-namespace* namespace-name)
        (*php-current-imports* nil)
        (*php-pending-top-level-forms* nil)
        (current (%php-consume-expected :T-LBRACE stream))
        (stmts nil)
        (kv known-vars))
    (loop
      (setf current (php-skip-semis current))
      (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement current kv)
        (cond
          (*php-pending-top-level-forms*
           (dolist (form (reverse *php-pending-top-level-forms*))
             (push form stmts))
           (setf *php-pending-top-level-forms* nil))
          (stmt
           (push (php-annotate-top-level-node stmt) stmts)))
        (setf current rest2 kv kv2)))
    (values (nreverse stmts) (%php-consume-expected :T-RBRACE current) kv)))

(defun %php-parse-statement-body (stream known-vars)
  "Parse either a braced block or one PHP statement. Return a statement list."
  (if (eq (php-peek-type stream) :T-LBRACE)
      (php-parse-block stream known-vars)
      (multiple-value-bind (stmt rest kv) (php-parse-statement stream known-vars)
        (values (if stmt (list stmt) nil) rest kv))))

(defun %php-type-keyword-token-p (stream)
  "Return true when STREAM starts with a keyword valid in PHP type position."
  (and stream
       (eq (php-peek-type stream) :T-KEYWORD)
       (member (php-peek-value stream) '(:array :null :true :false) :test #'eq)))

(defun %php-type-atom-token-p (stream)
  "Return true when STREAM starts with a PHP type atom."
  (and stream
       (or (eq (php-peek-type stream) :T-TYPE)
           (eq (php-peek-type stream) :T-IDENT)
           (%php-type-keyword-token-p stream))))

(defun %php-type-token-string (token)
  "Return TOKEN's PHP spelling for metadata storage."
  (let ((value (php-tok-value token)))
    (string-downcase
     (etypecase value
       (keyword (symbol-name value))
       (symbol (symbol-name value))
       (string value)))))

(defun php-parse-type-annotation (stream)
  "Parse a PHP type annotation from STREAM.
Returns (values type-string remaining-stream). Handles nullable, union, and
intersection type syntax as metadata only."
  (let ((current stream)
        (parts nil))
    (when (eq (php-peek-type current) :T-NULLABLE)
      (push "?" parts)
      (setf current (cdr current)))
    (unless (%php-type-atom-token-p current)
      (return-from php-parse-type-annotation (values nil stream)))
    (push (%php-type-token-string (php-peek current)) parts)
    (setf current (cdr current))
    (loop while (and current
                     (eq (php-peek-type current) :T-OP)
                     (member (php-peek-value current) '("|" "&") :test #'equal)
                     (%php-type-atom-token-p (cdr current)))
          do (push (php-peek-value current) parts)
             (setf current (cdr current))
             (push (%php-type-token-string (php-peek current)) parts)
             (setf current (cdr current)))
    (values (apply #'concatenate 'string (nreverse parts)) current)))

(defun %php-skip-type-annotation (stream)
  "Consume an optional type annotation from STREAM."
  (nth-value 1 (php-parse-type-annotation stream)))

(defun %php-parse-single-param (stream)
  "Parse one PHP parameter entry: attribute* visibility? type? $var.
Returns (values param-sym rest param-type param-attr-plist)."
  (multiple-value-bind (attributes rest-after-attributes) (%php-parse-attributes stream)
    ;; PHP 8.0 constructor property promotion: visibility/readonly modifiers may
    ;; precede the type, e.g. __construct(public int $x).
    (multiple-value-bind (promo-modifiers rest-after-mods)
        (%php-parse-visibility-modifiers rest-after-attributes)
      (multiple-value-bind (param-type rest-after-type)
          (php-parse-type-annotation rest-after-mods)
        (when (%php-reference-token-p rest-after-type)
          (error "PHP parse error: PHP parameter by reference (&$param) is not yet supported"))
        (multiple-value-bind (var-tok rest) (php-expect :T-VAR rest-after-type)
          (let* ((param (php-var-sym (php-tok-value var-tok)))
                 (attr-plist
                  (when (or attributes promo-modifiers)
                    (cons param
                          (append (when attributes
                                    (%php-attribute-metadata attributes :parameter))
                                  (when promo-modifiers
                                    (list :php-promote promo-modifiers)))))))
            (values param rest param-type attr-plist)))))))

(defun php-parse-param-list (stream)
  "Parse (attribute* type? $param, ...). Return params, rest, type metadata, and attribute metadata."
  (let ((current (%php-consume-expected :T-LPAREN stream))
        (params nil)
        (param-types nil)
        (param-attributes nil))
    (if (eq (php-peek-type current) :T-RPAREN)
        (values nil (cdr current) nil)
        (progn
          (loop
            (multiple-value-bind (param rest param-type attr-plist)
                (%php-parse-single-param current)
              (push param params)
              (when param-type
                (push (cons param param-type) param-types))
              (when attr-plist
                (push attr-plist param-attributes))
              (setf current rest))
            (when (and current (eq (php-peek-type current) :T-OP)
                       (equal "=" (php-peek-value current)))
              (setf current (cdr current))
              (loop while (and current
                               (not (eq (php-peek-type current) :T-COMMA))
                               (not (eq (php-peek-type current) :T-RPAREN)))
                    do (setf current (cdr current))))
            (if (eq (php-peek-type current) :T-COMMA)
                (setf current (cdr current))
                (return)))
          (values (nreverse params)
                  (%php-consume-expected :T-RPAREN current)
                  (nreverse param-types)
                  (nreverse param-attributes))))))

(defun php-parse-return-type (stream)
  "Parse an optional : type annotation after a function parameter list."
  (if (and stream (eq (php-peek-type stream) :T-COLON))
      (php-parse-type-annotation (cdr stream))
      (values nil stream)))

(defun %php-function-type-declarations (param-types return-type)
  "Build PHP type metadata plist for AST callable declarations."
  (append (when param-types (list :php-param-types param-types))
          (when return-type (list :php-return-type return-type))))

(defun %php-function-declarations (param-types return-type param-attributes attributes target-type)
  "Build PHP callable metadata including types and attributes."
  (append (%php-function-type-declarations param-types return-type)
          (when param-attributes (list :php-param-attributes param-attributes))
          (%php-attribute-metadata attributes target-type)))

(defun %php-parse-label-stmt (stream known-vars)
  "Parse `label:` into a tagbody label marker."
  (multiple-value-bind (label-tok rest) (php-expect :T-IDENT stream)
    (values (make-ast-tagbody :tags (list (cons (php-ident-sym (php-tok-value label-tok)) nil)))
            (%php-consume-expected :T-COLON rest)
            known-vars)))

(defun %php-parse-if-tail (stream known-vars)
  "Parse the then/body plus elseif/else tail after an if condition."
  (multiple-value-bind (then-stmts rest kv) (%php-parse-statement-body stream known-vars)
    (let ((else-ast (make-ast-quote :value nil)))
      (cond
        ((%php-keyword-p rest :elseif)
         (multiple-value-bind (elseif-cond rest2 kv2) (%php-parse-paren-expr (cdr rest) kv)
           (multiple-value-bind (elseif-body rest3 kv3 elseif-else) (%php-parse-if-tail rest2 kv2)
             (setf else-ast (make-ast-if :cond elseif-cond
                                          :then (make-ast-progn :forms elseif-body)
                                          :else elseif-else)
                   rest rest3 kv kv3))))
        ((%php-keyword-p rest :else)
         (multiple-value-bind (else-stmts rest2 kv2) (%php-parse-statement-body (cdr rest) kv)
           (setf else-ast (make-ast-progn :forms else-stmts)
                 rest rest2 kv kv2))))
      (values then-stmts rest kv else-ast))))

(defun %php-parse-switch-body (stream known-vars break-tag)
  "Parse switch case/default sections."
  (let ((current (%php-consume-expected :T-LBRACE stream))
        (cases nil)
        (default-body nil)
        (kv known-vars))
    (loop
      (setf current (php-skip-semis current))
      (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
        (return))
      (cond
        ((%php-keyword-p current :case)
         (multiple-value-bind (case-expr rest kv2) (php-parse-expr (cdr current) kv)
           (setf current (if (eq (php-peek-type rest) :T-COLON) (cdr rest) (php-skip-semis rest))
                 kv kv2)
           (let ((body nil))
             (loop
               (setf current (php-skip-semis current))
               (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE)
                         (%php-keyword-p current :case) (%php-keyword-p current :default))
                 (return))
               (let ((*php-loop-break-target* break-tag))
                 (multiple-value-bind (stmt rest2 kv3) (php-parse-statement current kv)
                   (when stmt (push stmt body))
                   (setf current rest2 kv kv3))))
             (push (cons case-expr (nreverse body)) cases))))
        ((%php-keyword-p current :default)
         (setf current (cdr current))
         (when (eq (php-peek-type current) :T-COLON) (setf current (cdr current)))
         (let ((body nil))
           (loop
             (setf current (php-skip-semis current))
             (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE)
                       (%php-keyword-p current :case))
               (return))
             (let ((*php-loop-break-target* break-tag))
               (multiple-value-bind (stmt rest2 kv2) (php-parse-statement current kv)
                 (when stmt (push stmt body))
                 (setf current rest2 kv kv2))))
           (setf default-body (nreverse body))))
        (t (setf current (%php-skip-to-stmt-end current)))))
    (values (nreverse cases) default-body (%php-consume-expected :T-RBRACE current) kv)))

(defun %php-lower-while-with-label (cond-expr body-stmts loop-tag)
  "Lower a PHP while loop using LOOP-TAG as the continue target."
  (make-ast-block :name nil
    :body (list
           (%php-make-tagbody
            (append (list loop-tag
                          (make-ast-if
                           :cond cond-expr
                           :then (make-ast-quote :value nil)
                           :else (make-ast-return-from :name nil
                                                      :value (make-ast-quote :value nil))))
                    body-stmts
                    (list (make-ast-go :tag loop-tag))
                    (when *php-loop-break-target*
                      (list *php-loop-break-target*)))))))

(defun %php-lower-do-while-with-label (cond-expr body-stmts loop-tag)
  "Lower a PHP do/while loop using LOOP-TAG as the continue target."
  (make-ast-block :name nil
    :body (list
           (%php-make-tagbody
            (append (list loop-tag)
                    body-stmts
                    (list (make-ast-if
                           :cond cond-expr
                           :then (make-ast-go :tag loop-tag)
                           :else (make-ast-quote :value nil)))
                    (when *php-loop-break-target*
                      (list *php-loop-break-target*)))))))

(defun php-lower-switch (switch-expr cases default-body break-tag)
  "Lower a PHP switch/case/default to a let/tagbody dispatch form."
  (let ((value-sym (gensym "SWITCH-VAL-"))
        (default-tag (when default-body (gensym "SWITCH-DEFAULT-"))))
    (let* ((case-labels (loop repeat (length cases) collect (gensym "SWITCH-CASE-")))
           (dispatch-forms
            (append
             (loop for case in cases
                   for case-val = (car case)
                   for label in case-labels
                   collect (make-ast-if
                            :cond (make-ast-call
                                   :func (make-ast-var :name 'equal)
                                   :args (list (make-ast-var :name value-sym) case-val))
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
                      :body (list (%php-make-tagbody
                                   (append dispatch-forms
                                           case-forms
                                           default-forms
                                           (list break-tag))))))))))

(defun %php-make-list-advance (list-sym)
  "Build an AST setq that advances LIST-SYM to (cdr LIST-SYM)."
  (make-ast-setq :var list-sym
                 :value (make-ast-call
                         :func (make-ast-var :name 'cdr)
                         :args (list (make-ast-var :name list-sym)))))

(defun %php-lower-foreach-with-label (arr-expr var-sym body-stmts loop-tag &optional key-sym)
  "Lower a PHP foreach loop using LOOP-TAG as the continue target."
  (let ((list-sym (gensym "FOREACH-LIST-")))
    (if key-sym
        (let ((pair-sym (gensym "FOREACH-PAIR-")))
          (make-ast-let
           :bindings (list (cons list-sym
                                 (make-ast-call :func (make-ast-var :name '%php-array-pairs)
                                                :args (list arr-expr)))
                           (cons pair-sym (make-ast-quote :value nil)))
           :body (list (%php-lower-while-with-label
                        (make-ast-var :name list-sym)
                        (list (make-ast-setq
                               :var pair-sym
                               :value (make-ast-call
                                       :func (make-ast-var :name 'car)
                                       :args (list (make-ast-var :name list-sym))))
                              (make-ast-let
                               :bindings (list (cons key-sym
                                                     (make-ast-call
                                                      :func (make-ast-var :name 'car)
                                                      :args (list (make-ast-var :name pair-sym))))
                                               (cons var-sym
                                                     (make-ast-call
                                                      :func (make-ast-var :name 'cdr)
                                                      :args (list (make-ast-var :name pair-sym)))))
                               :body (append body-stmts
                                             (list (%php-make-list-advance list-sym)))))
                        loop-tag))))
        (make-ast-let
         :bindings (list (cons list-sym arr-expr))
         :body (list (%php-lower-while-with-label
                      (make-ast-var :name list-sym)
                      (list (make-ast-let
                             :bindings (list (cons var-sym
                                                   (make-ast-call
                                                    :func (make-ast-var :name 'car)
                                                    :args (list (make-ast-var :name list-sym)))))
                             :body (append body-stmts
                                           (list (%php-make-list-advance list-sym)))))
                      loop-tag))))))

(defun %php-exception-object-cond (expr)
  "Build a predicate call testing whether EXPR is a PHP exception payload."
  (%php-call 'cl-cc/php::%php-exception-object-p expr))

(defun %php-exception-match-cond (expr class-name)
  "Build a predicate call testing whether EXPR matches PHP catch CLASS-NAME."
  (%php-call 'cl-cc/php::%php-exception-matches-p
             expr
             (make-ast-quote :value class-name)))

(defun %php-rethrow-exception (expr)
  "Build a VM throw that propagates an unmatched PHP exception payload."
  (make-ast-throw :tag (make-ast-quote :value 'php-exception)
                  :value expr))

(defun %php-catch-body (exception-sym var-sym body)
  "Return BODY with PHP catch variable VAR-SYM bound to EXCEPTION-SYM when present."
  (if var-sym
      (make-ast-let :bindings (list (cons var-sym
                                          (%php-call 'cl-cc/php::%php-exception-value
                                                     (make-ast-var :name exception-sym))))
                     :body body)
      (make-ast-progn :forms body)))

(defun %php-catch-dispatch (exception-sym clauses)
  "Build nested PHP catch dispatch for EXCEPTION-SYM and CATCH CLAUSES."
  (let ((exception-var (make-ast-var :name exception-sym)))
    (if clauses
        (destructuring-bind (class-name var-sym . body) (first clauses)
          (make-ast-if :cond (%php-exception-match-cond exception-var class-name)
                       :then (%php-catch-body exception-sym var-sym body)
                       :else (%php-catch-dispatch exception-sym (rest clauses))))
        (%php-rethrow-exception exception-var))))

(defun %php-lower-try-catches (try-body clauses)
  "Lower PHP try/catch using VM catch/throw plus PHP class-matching helpers."
  (let ((exception-sym (gensym "PHP-EXCEPTION-RESULT-")))
    (make-ast-let
     :bindings (list (cons exception-sym
                           (make-ast-catch :tag (make-ast-quote :value 'php-exception)
                                           :body try-body)))
     :body (list (make-ast-if
                   :cond (%php-exception-object-cond (make-ast-var :name exception-sym))
                   :then (%php-catch-dispatch exception-sym clauses)
                   :else (make-ast-var :name exception-sym))))))
