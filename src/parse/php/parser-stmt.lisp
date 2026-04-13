;;;; src/parse/php/parser-stmt.lisp — PHP Parser: Statement Layer
;;;;
;;;; Contains statement-level parsing, loop lowering, and the top-level entry
;;;; point. Expression-level parsing (php-parse-primary through php-parse-arglist)
;;;; is in parser.lisp (loads before).
;;;;
;;;; Load order: after parser.lisp.

(in-package :cl-cc)

;;; ─── Statement Parser ───────────────────────────────────────────────────────

(defun php-parse-block (stream known-vars)
  "Parse { stmt* }. Returns (values stmt-list remaining-stream known-vars)."
  (multiple-value-bind (tok rest) (php-expect :T-LBRACE stream)
    (declare (ignore tok))
    (let ((stmts nil)
          (current rest)
          (kv known-vars))
      (loop
        (setf current (php-skip-semis current))
        (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (stmt rest2 kv2) (php-parse-statement current kv)
          (when stmt (push stmt stmts))
          (setf current rest2 kv kv2)))
      (multiple-value-bind (tok2 rest2) (php-expect :T-RBRACE current)
        (declare (ignore tok2))
        (values (nreverse stmts) rest2 kv)))))

(defun php-parse-param-list (stream)
  "Parse (type? $param, ...) for function/method definitions."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2))
        (let ((params nil)
              (current rest))
          (loop
            ;; Skip optional type annotation
            (when (or (eq (php-peek-type current) :T-TYPE)
                      (eq (php-peek-type current) :T-IDENT)
                      (eq (php-peek-type current) :T-NULLABLE))
              (setf current (cdr current))
              ;; If nullable ?, skip type too
              (when (or (eq (php-peek-type current) :T-TYPE)
                        (eq (php-peek-type current) :T-IDENT))
                (setf current (cdr current))))
            ;; Expect $var
            (multiple-value-bind (var-tok rest2) (php-expect :T-VAR current)
              (push (php-var-sym (php-tok-value var-tok)) params)
              (setf current rest2))
            ;; Skip default value if present
            (when (and current (eq (php-peek-type current) :T-OP)
                       (equal "=" (php-peek-value current)))
              (setf current (cdr current))
              ;; consume default expression (simplified: consume until , or ))
              (loop while (and current
                               (not (eq (php-peek-type current) :T-COMMA))
                               (not (eq (php-peek-type current) :T-RPAREN)))
                    do (setf current (cdr current))))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse params) rest2))))))

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars)."
  (let ((type  (php-peek-type  stream))
        (value (php-peek-value stream)))
    (cond
      ;; echo expr;
      ((and (eq type :T-KEYWORD) (eq value :echo))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
           (let ((rest3 (php-skip-semis rest2)))
             (values (make-ast-print :expr expr) rest3 kv2)))))

      ;; return expr;
      ((and (eq type :T-KEYWORD) (eq value :return))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (if (eq (php-peek-type rest) :T-SEMI)
             (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
                     (php-skip-semis rest) known-vars)
             (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
               (values (make-ast-return-from :name nil :value expr)
                       (php-skip-semis rest2) kv2)))))

      ;; if ($cond) { } [else { }]
      ((and (eq type :T-KEYWORD) (eq value :if))
       (multiple-value-bind (tok rest) (php-consume stream) ; consume 'if'
         (declare (ignore tok))
         (multiple-value-bind (cond-tok rest2) (php-expect :T-LPAREN rest)
           (declare (ignore cond-tok))
           (multiple-value-bind (cond-expr rest3 kv3) (php-parse-expr rest2 known-vars)
             (multiple-value-bind (tok2 rest4) (php-expect :T-RPAREN rest3)
               (declare (ignore tok2))
               (multiple-value-bind (then-stmts rest5 kv5) (php-parse-block rest4 kv3)
                 (let ((else-ast (make-ast-quote :value nil)))
                   (when (and rest5 (eq (php-peek-type rest5) :T-KEYWORD)
                              (eq (php-peek-value rest5) :else))
                     (setf rest5 (cdr rest5))
                     (multiple-value-bind (else-stmts rest6 kv6) (php-parse-block rest5 kv5)
                       (setf else-ast (make-ast-progn :forms else-stmts)
                             rest5 rest6
                             kv5 kv6)))
                   (values (make-ast-if
                            :cond cond-expr
                            :then (make-ast-progn :forms then-stmts)
                            :else else-ast)
                           rest5 kv5))))))))

      ;; while ($cond) { }
      ((and (eq type :T-KEYWORD) (eq value :while))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (cond-expr rest3 kv3) (php-parse-expr rest2 known-vars)
             (multiple-value-bind (tok3 rest4) (php-expect :T-RPAREN rest3)
               (declare (ignore tok3))
               (multiple-value-bind (body-stmts rest5 kv5) (php-parse-block rest4 kv3)
                 (values (php-lower-while cond-expr body-stmts) rest5 kv5)))))))

      ;; for ($init; $cond; $incr) { }
      ((and (eq type :T-KEYWORD) (eq value :for))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (init rest3 kv3) (php-parse-expr rest2 known-vars)
             (multiple-value-bind (tok3 rest4) (php-expect :T-SEMI rest3)
               (declare (ignore tok3))
               (multiple-value-bind (cond-expr rest5 kv5) (php-parse-expr rest4 kv3)
                 (multiple-value-bind (tok4 rest6) (php-expect :T-SEMI rest5)
                   (declare (ignore tok4))
                   (multiple-value-bind (incr rest7 kv7) (php-parse-expr rest6 kv5)
                     (multiple-value-bind (tok5 rest8) (php-expect :T-RPAREN rest7)
                       (declare (ignore tok5))
                       (multiple-value-bind (body-stmts rest9 kv9) (php-parse-block rest8 kv7)
                         (values (make-ast-progn
                                  :forms (list init
                                               (php-lower-while
                                                cond-expr
                                                (append body-stmts (list incr)))))
                                 rest9 kv9)))))))))))

      ;; foreach ($arr as [$k =>] $v) { }
      ((and (eq type :T-KEYWORD) (eq value :foreach))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (arr-expr rest3 kv3) (php-parse-expr rest2 known-vars)
             ;; expect 'as'
             (let ((rest4 (if (and rest3 (eq (php-peek-type rest3) :T-KEYWORD)
                                   (eq (php-peek-value rest3) :as))
                              (cdr rest3)
                              (error "foreach: expected 'as'"))))
               (multiple-value-bind (var-tok rest5) (php-expect :T-VAR rest4)
                 (let ((var-sym (php-var-sym (php-tok-value var-tok)))
                       (rest6 rest5))
                   ;; Skip optional => $val (key => value form; we use the value var)
                   (when (and rest6 (eq (php-peek-type rest6) :T-OP)
                              (equal "=>" (php-peek-value rest6)))
                     (setf rest6 (cdr rest6))
                     (multiple-value-bind (val-tok rest7) (php-expect :T-VAR rest6)
                       (setf var-sym (php-var-sym (php-tok-value val-tok)))
                       (setf rest6 rest7)))
                   (multiple-value-bind (tok3 rest7) (php-expect :T-RPAREN rest6)
                     (declare (ignore tok3))
                     (multiple-value-bind (body-stmts rest8 kv8) (php-parse-block rest7 kv3)
                       (values (php-lower-foreach arr-expr var-sym body-stmts)
                               rest8 kv8))))))))))

      ;; function name($params) { }
      ((and (eq type :T-KEYWORD) (eq value :function))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
           (let ((fn-name (php-ident-sym (php-tok-value name-tok))))
             (multiple-value-bind (params rest3) (php-parse-param-list rest2)
               ;; Skip optional return type
               (let ((rest4 rest3))
                 (when (and rest4 (eq (php-peek-type rest4) :T-COLON))
                   (setf rest4 (cdr rest4))
                   (when (or (eq (php-peek-type rest4) :T-TYPE)
                             (eq (php-peek-type rest4) :T-IDENT)
                             (eq (php-peek-type rest4) :T-NULLABLE))
                     (setf rest4 (cdr rest4))
                     (when (or (eq (php-peek-type rest4) :T-TYPE)
                               (eq (php-peek-type rest4) :T-IDENT))
                       (setf rest4 (cdr rest4)))))
                 (let ((fn-kv (append params known-vars)))
                   (multiple-value-bind (body-stmts rest5 _) (php-parse-block rest4 fn-kv)
                     (declare (ignore _))
                     (values (make-ast-defun :name fn-name :params params :body body-stmts)
                             rest5 known-vars)))))))))

      ;; class Foo [extends Bar] [implements A, B] { }
      ((and (eq type :T-KEYWORD) (eq value :class))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
           (let ((class-name (php-ident-sym (php-tok-value name-tok)))
                 (supers nil)
                 (current rest2))
             ;; extends
             (when (and current (eq (php-peek-type current) :T-KEYWORD)
                        (eq (php-peek-value current) :extends))
               (setf current (cdr current))
               (multiple-value-bind (super-tok rest3) (php-expect :T-IDENT current)
                 (push (php-ident-sym (php-tok-value super-tok)) supers)
                 (setf current rest3)))
             ;; implements (skip interface list)
             (when (and current (eq (php-peek-type current) :T-KEYWORD)
                        (eq (php-peek-value current) :implements))
               (setf current (cdr current))
               (loop
                 (multiple-value-bind (tok2 rest3) (php-expect :T-IDENT current)
                   (declare (ignore tok2))
                   (setf current rest3))
                 (unless (and current (eq (php-peek-type current) :T-COMMA))
                   (return))
                 (setf current (cdr current))))
             ;; class body { ... }
             (multiple-value-bind (tok2 rest3) (php-expect :T-LBRACE current)
               (declare (ignore tok2))
               (let ((slots nil))
                 ;; Parse class body: collect property declarations and methods
                 (loop
                   (setf rest3 (php-skip-semis rest3))
                   (when (or (php-at-eof-p rest3) (eq (php-peek-type rest3) :T-RBRACE))
                     (return))
                   ;; Skip visibility modifiers
                   (loop while (and rest3
                                    (eq (php-peek-type rest3) :T-KEYWORD)
                                    (member (php-peek-value rest3)
                                            '(:public :private :protected :static
                                              :abstract :final :readonly)))
                         do (setf rest3 (cdr rest3)))
                   (cond
                     ;; Property: type? $var [= default];
                     ((eq (php-peek-type rest3) :T-VAR)
                      (multiple-value-bind (var-tok rest4) (php-consume rest3)
                        (push (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                              slots)
                        (setf rest3 rest4)
                        ;; Skip optional default
                        (when (and rest3 (eq (php-peek-type rest3) :T-OP)
                                   (equal "=" (php-peek-value rest3)))
                          (setf rest3 (cdr rest3))
                          (loop while (and rest3
                                           (not (eq (php-peek-type rest3) :T-SEMI)))
                                do (setf rest3 (cdr rest3))))
                        (setf rest3 (php-skip-semis rest3))))
                     ;; Typed property: type $var
                     ((or (eq (php-peek-type rest3) :T-TYPE)
                          (eq (php-peek-type rest3) :T-NULLABLE))
                      (setf rest3 (cdr rest3))
                      (when (or (eq (php-peek-type rest3) :T-TYPE)
                                (eq (php-peek-type rest3) :T-IDENT))
                        (setf rest3 (cdr rest3)))
                      (when (eq (php-peek-type rest3) :T-VAR)
                        (multiple-value-bind (var-tok rest4) (php-consume rest3)
                          (push (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                                slots)
                          (setf rest3 rest4)
                          (setf rest3 (php-skip-semis rest3)))))
                     ;; Method: function name(...) { }
                     ((and (eq (php-peek-type rest3) :T-KEYWORD)
                           (eq (php-peek-value rest3) :function))
                      (multiple-value-bind (method-ast rest4 _)
                          (php-parse-statement rest3 known-vars)
                        (declare (ignore _))
                        ;; Methods become slots with function values (simplified)
                        (when method-ast
                          (push (make-ast-slot-def :name (ast-defun-name method-ast)) slots))
                        (setf rest3 rest4)))
                     (t ; skip unknown tokens in class body
                      (setf rest3 (cdr rest3)))))
                 (multiple-value-bind (tok3 rest4) (php-expect :T-RBRACE rest3)
                   (declare (ignore tok3))
                   (values (make-ast-defclass :name class-name
                                              :superclasses (nreverse supers)
                                              :slots (nreverse slots))
                           rest4 known-vars))))))))

      ;; Expression statement (assignment, function call, etc.)
      (t
       (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
         (values expr (php-skip-semis rest) kv))))))

;;; ─── Top-Level Entry Point ──────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Post-process: wrap consecutive ast-let :body nil nodes into proper nested lets.
   (ast-let :bindings (...) :body nil) means 'declare var in scope for rest of stmts'.
   Iterative implementation to avoid stack overflow on large flat statement lists."
  (when (null stmts) (return-from php-finish-let-bindings nil))
  ;; Walk backwards through stmts, building up the nesting from the end.
  ;; We use a continuation-passing accumulator: 'tail' is the already-processed suffix.
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (setf tail
            (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
                (list (make-ast-let :bindings (ast-let-bindings stmt) :body tail))
                (cons stmt tail))))))

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
   Analogous to parse-all-forms for CL."
  (let* ((tokens  (tokenize-php-source source))
         (stream  tokens)
         (stmts   nil)
         (kv      nil))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (when stmt (push stmt stmts))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings (nreverse stmts))))
