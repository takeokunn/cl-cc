;;;; packages/php/src/parser-stmt.lisp — PHP Parser: Statement Layer

(in-package :cl-cc/php)

(defvar *php-stmt-parsers* (make-hash-table)
  "Maps PHP keyword values to statement parser functions.")

(defmacro define-php-stmt-parser (keyword (stream known-vars) &body body)
  "Register a statement parser for KEYWORD in *php-stmt-parsers*."
  `(setf (gethash ,keyword *php-stmt-parsers*)
         (lambda (,stream ,known-vars) ,@body)))

(defun %php-consume-expected (token stream)
  "Consume TOKEN from STREAM and return the remaining stream."
  (nth-value 1 (php-expect token stream)))

(defun %php-keyword-p (stream keyword)
  "Return true when STREAM starts with KEYWORD."
  (and stream (eq (php-peek-type stream) :T-KEYWORD)
       (eq (php-peek-value stream) keyword)))

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

(defvar *php-continue-targets* nil
  "Dynamically bound stack of loop continue targets, innermost first.")

(defun %php-continue-target (level)
  "Return the continue target for LEVEL, where 1 means the innermost loop."
  (let ((index (max 0 (1- (or level 1)))))
    (or (nth index *php-continue-targets*)
        *php-loop-continue-target*)))

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

(defun %php-parse-statement-body (stream known-vars)
  "Parse either a braced block or one PHP statement. Return a statement list."
  (if (eq (php-peek-type stream) :T-LBRACE)
      (php-parse-block stream known-vars)
      (multiple-value-bind (stmt rest kv) (php-parse-statement stream known-vars)
        (values (if stmt (list stmt) nil) rest kv))))

(defun %php-skip-type-annotation (stream)
  "Consume an optional type annotation from STREAM."
  (if (or (eq (php-peek-type stream) :T-TYPE)
          (eq (php-peek-type stream) :T-IDENT)
          (eq (php-peek-type stream) :T-NULLABLE))
      (let ((rest (cdr stream)))
        (if (or (eq (php-peek-type rest) :T-TYPE)
                (eq (php-peek-type rest) :T-IDENT))
            (cdr rest)
            rest))
      stream))

(defun php-parse-param-list (stream)
  "Parse (type? $param, ...)."
  (let ((current (%php-consume-expected :T-LPAREN stream))
        (params nil))
    (if (eq (php-peek-type current) :T-RPAREN)
        (values nil (cdr current))
        (progn
          (loop
            (setf current (%php-skip-type-annotation current))
            (multiple-value-bind (var-tok rest) (php-expect :T-VAR current)
              (push (php-var-sym (php-tok-value var-tok)) params)
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
          (values (nreverse params) (%php-consume-expected :T-RPAREN current))))))

(defun %php-skip-return-type (stream)
  "Consume an optional : type annotation after a function parameter list."
  (if (and stream (eq (php-peek-type stream) :T-COLON))
      (%php-skip-type-annotation (cdr stream))
      stream))

(defun %php-parse-label-stmt (stream known-vars)
  "Parse `label:` into a tagbody label marker."
  (multiple-value-bind (label-tok rest) (php-expect :T-IDENT stream)
    (values (make-ast-tagbody :tags (list (php-ident-sym (php-tok-value label-tok))))
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
    :body (list (make-ast-tagbody
                 :tags (list* loop-tag
                              (make-ast-if
                               :cond cond-expr
                               :then (make-ast-quote :value nil)
                               :else (make-ast-return-from :name nil
                                                          :value (make-ast-quote :value nil)))
                              (append body-stmts
                                      (list (make-ast-go :tag loop-tag))))))))

(defun %php-lower-do-while-with-label (cond-expr body-stmts loop-tag)
  "Lower a PHP do/while loop using LOOP-TAG as the continue target."
  (make-ast-block :name nil
    :body (list (make-ast-tagbody
                 :tags (append (list loop-tag)
                               body-stmts
                               (list (make-ast-if
                                      :cond cond-expr
                                      :then (make-ast-go :tag loop-tag)
                                      :else (make-ast-quote :value nil))))))))

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
                  collect (make-ast-tagbody
                           :tags (list* label (cdr case)))))
           (default-form
            (when default-body
              (make-ast-tagbody
               :tags (list* default-tag
                            (append default-body
                                    (list (make-ast-go :tag break-tag))))))))
      (make-ast-let
       :bindings (list (cons value-sym switch-expr))
       :body (list (make-ast-block :name nil
                     :body (list (make-ast-progn
                                  :forms (append dispatch-forms
                                                 case-forms
                                                 (when default-form (list default-form))
                                                 (list (make-ast-tagbody
                                                        :tags (list break-tag))))))))))))

(defun %php-lower-foreach-with-label (arr-expr var-sym body-stmts loop-tag)
  "Lower a PHP foreach loop using LOOP-TAG as the continue target."
  (let ((list-sym (gensym "FOREACH-LIST-")))
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
                                       (list (make-ast-setq
                                              :var list-sym
                                              :value (make-ast-call
                                                      :func (make-ast-var :name 'cdr)
                                                      :args (list (make-ast-var :name list-sym))))))))
                  loop-tag)))))

(defun %php-parse-classlike (stream known-vars &key enum-p)
  "Parse trait/interface/enum as ast-defclass-style declarations."
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT stream)
    (let ((current rest)
          (slots nil))
      (loop until (or (null current) (eq (php-peek-type current) :T-LBRACE))
            do (setf current (cdr current)))
      (setf current (%php-consume-expected :T-LBRACE current))
      (loop
        (setf current (php-skip-semis current))
        (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
          (return))
        (if (and enum-p (%php-keyword-p current :case))
            (progn
              (setf current (cdr current))
              (when (eq (php-peek-type current) :T-IDENT)
                (push (make-ast-slot-def :name (php-ident-sym (php-peek-value current))) slots)
                (setf current (cdr current)))
              (setf current (%php-skip-to-stmt-end current)))
            (setf current (%php-skip-to-stmt-end current))))
      (values (make-ast-defclass :name (php-ident-sym (php-tok-value name-tok))
                                 :superclasses nil
                                 :slots (nreverse slots))
              (%php-consume-expected :T-RBRACE current)
              known-vars))))

(define-php-stmt-parser :echo (rest known-vars)
  ;; Echo supports comma-separated expressions: echo 1, 2, 3;
  (let ((exprs nil) (current rest) (kv known-vars))
    (loop
      (multiple-value-bind (expr rest2 kv2) (php-parse-expr current kv)
        (push expr exprs)
        (setf current rest2 kv kv2))
      (unless (and current (eq (php-peek-type current) :T-COMMA))
        (return))
      (setf current (cdr current)))
    (values (make-ast-print :expr (if (cdr exprs)
                                      (make-ast-progn :forms (nreverse exprs))
                                      (car exprs)))
            (php-skip-semis current) kv)))


(define-php-stmt-parser :return (rest known-vars)
  (if (eq (php-peek-type rest) :T-SEMI)
      (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
              (php-skip-semis rest) known-vars)
      (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
        (values (make-ast-return-from :name nil :value expr) (php-skip-semis rest2) kv2))))

(define-php-stmt-parser :if (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (multiple-value-bind (then-stmts rest3 kv3 else-ast) (%php-parse-if-tail rest2 kv2)
      (values (make-ast-if :cond cond-expr
                           :then (make-ast-progn :forms then-stmts)
                           :else else-ast)
              rest3 kv3))))

(define-php-stmt-parser :while (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (let* ((*php-loop-continue-target* (gensym "WHILE-LOOP-"))
           (*php-loop-break-target* (gensym "WHILE-END-"))
           (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
      (multiple-value-bind (body-stmts rest3 kv3) (%php-parse-statement-body rest2 kv2)
        (values (%php-lower-while-with-label
                 cond-expr
                 (append body-stmts (list *php-loop-break-target*))
                 *php-loop-continue-target*)
                rest3 kv3)))))

(define-php-stmt-parser :for (rest known-vars)
  (let ((rest (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (init rest kv)
        (if (eq (php-peek-type rest) :T-SEMI)
            (values (make-ast-quote :value nil) rest known-vars)
            (php-parse-expr rest known-vars))
      (let ((rest (%php-consume-expected :T-SEMI rest)))
        (multiple-value-bind (cond-expr rest kv)
            (if (eq (php-peek-type rest) :T-SEMI)
                (values (make-ast-quote :value t) rest kv)
                (php-parse-expr rest kv))
          (let ((rest (%php-consume-expected :T-SEMI rest)))
            (multiple-value-bind (incr rest kv)
                (if (eq (php-peek-type rest) :T-RPAREN)
                    (values (make-ast-quote :value nil) rest kv)
                    (php-parse-expr rest kv))
              (let* ((rest (%php-consume-expected :T-RPAREN rest))
                     (*php-loop-continue-target* (gensym "FOR-LOOP-"))
                     (*php-loop-break-target* (gensym "FOR-END-"))
                     (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
                (multiple-value-bind (body-stmts rest _) (%php-parse-statement-body rest kv)
                  (declare (ignore _))
                  (values (make-ast-progn
                           :forms (list init (%php-lower-while-with-label
                                              cond-expr
                                              (append body-stmts
                                                      (list incr *php-loop-break-target*))
                                              *php-loop-continue-target*)))
                          rest kv))))))))))

(define-php-stmt-parser :foreach (rest known-vars)
  (let ((rest2 (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (arr-expr rest3 kv3) (php-parse-expr rest2 known-vars)
      (let ((rest4 (if (%php-keyword-p rest3 :as) (cdr rest3) (error "foreach: expected 'as'"))))
        (multiple-value-bind (var-tok rest5) (php-expect :T-VAR rest4)
          (let ((var-sym (php-var-sym (php-tok-value var-tok)))
                (rest6 rest5))
            (when (and rest6 (eq (php-peek-type rest6) :T-OP) (equal "=>" (php-peek-value rest6)))
              (multiple-value-bind (val-tok rest7) (php-expect :T-VAR (cdr rest6))
                (setf var-sym (php-var-sym (php-tok-value val-tok))
                      rest6 rest7)))
            (let* ((*php-loop-continue-target* (gensym "FOREACH-LOOP-"))
                   (*php-loop-break-target* (gensym "FOREACH-END-"))
                   (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
              (multiple-value-bind (body-stmts rest8 kv8)
                  (%php-parse-statement-body (%php-consume-expected :T-RPAREN rest6) kv3)
                (values (%php-lower-foreach-with-label
                         arr-expr var-sym
                         (append body-stmts (list *php-loop-break-target*))
                         *php-loop-continue-target*)
                        rest8 kv8)))))))))

(define-php-stmt-parser :do (rest known-vars)
  (let* ((*php-loop-continue-target* (gensym "DO-WHILE-LOOP-"))
         (*php-loop-break-target* (gensym "DO-WHILE-END-"))
         (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
    (multiple-value-bind (body-stmts rest2 kv2) (%php-parse-statement-body rest known-vars)
      (unless (%php-keyword-p rest2 :while)
        (error "PHP parse error: expected while after do body"))
      (multiple-value-bind (cond-expr rest3 kv3) (%php-parse-paren-expr (cdr rest2) kv2)
        (values (%php-lower-do-while-with-label
                 cond-expr
                 (append body-stmts (list *php-loop-break-target*))
                 *php-loop-continue-target*)
                (php-skip-semis rest3) kv3)))))

(define-php-stmt-parser :switch (rest known-vars)
  (multiple-value-bind (switch-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (let ((break-tag (gensym "SWITCH-END-")))
      (multiple-value-bind (cases default-body rest3 kv3) (%php-parse-switch-body rest2 kv2 break-tag)
        (values (php-lower-switch switch-expr cases default-body break-tag) rest3 kv3)))))

(define-php-stmt-parser :try (rest known-vars)
  (multiple-value-bind (try-body rest2 kv2) (php-parse-block rest known-vars)
    (let ((clauses nil) (finally-body nil) (current rest2) (kv kv2))
      (loop while (%php-keyword-p current :catch)
            do (setf current (%php-consume-expected :T-LPAREN (cdr current)))
               (let* ((type-tok (php-peek current))
                      (type-sym (if (member (php-tok-type type-tok) '(:T-IDENT :T-TYPE :T-KEYWORD))
                                    (php-ident-sym (php-tok-value type-tok))
                                    'error)))
                 (setf current (cdr current))
                 (let ((var-sym nil))
                   (when (eq (php-peek-type current) :T-VAR)
                     (setf var-sym (php-var-sym (php-peek-value current))
                           current (cdr current)))
                   (setf current (%php-consume-expected :T-RPAREN current))
                   (multiple-value-bind (catch-body rest3 kv3) (php-parse-block current kv)
                     (push (list* type-sym var-sym catch-body) clauses)
                     (setf current rest3 kv kv3)))))
      (when (%php-keyword-p current :finally)
        (multiple-value-bind (body rest3 kv3) (php-parse-block (cdr current) kv)
          (setf finally-body body current rest3 kv kv3)))
      (let ((protected (make-ast-handler-case :form (make-ast-progn :forms try-body)
                                              :clauses (nreverse clauses))))
        (values (if finally-body
                    (make-ast-unwind-protect :protected protected :cleanup finally-body)
                    protected)
                current kv)))))

(define-php-stmt-parser :continue (rest known-vars)
  (let ((level nil))
    (when (eq (php-peek-type rest) :T-INT)
      (setf level (php-peek-value rest)
            rest (cdr rest)))
    (let ((current (if (eq (php-peek-type rest) :T-SEMI) rest (%php-skip-to-stmt-end rest))))
      (values (make-ast-go :tag (or (%php-continue-target level) 'LOOP-HEAD-TAG))
              (php-skip-semis current) known-vars))))

(define-php-stmt-parser :break (rest known-vars)
  (let ((current (if (eq (php-peek-type rest) :T-SEMI) rest (%php-skip-to-stmt-end rest))))
    (values (if *php-loop-break-target*
                (make-ast-go :tag *php-loop-break-target*)
                (make-ast-return-from :name nil :value (make-ast-quote :value nil)))
            (php-skip-semis current) known-vars)))

(defun %php-parse-include-like (rest known-vars name)
  (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
    (values (make-ast-call :func (make-ast-var :name name) :args (list expr))
            (php-skip-semis rest2) kv2)))

(define-php-stmt-parser :include (rest known-vars) (%php-parse-include-like rest known-vars 'include))
(define-php-stmt-parser :require (rest known-vars) (%php-parse-include-like rest known-vars 'require))
(define-php-stmt-parser :include-once (rest known-vars) (%php-parse-include-like rest known-vars 'include-once))
(define-php-stmt-parser :require-once (rest known-vars) (%php-parse-include-like rest known-vars 'require-once))

(define-php-stmt-parser :declare (rest known-vars)
  (values (make-ast-progn :forms nil) (%php-skip-to-stmt-end rest) known-vars))

(define-php-stmt-parser :goto (rest known-vars)
  (multiple-value-bind (label-tok rest2) (php-expect :T-IDENT rest)
    (values (make-ast-go :tag (php-ident-sym (php-tok-value label-tok)))
            (php-skip-semis rest2) known-vars)))

(define-php-stmt-parser :namespace (rest known-vars)
  (values (make-ast-progn :forms nil)
          (if (eq (php-peek-type rest) :T-LBRACE)
              (nth-value 1 (php-parse-block rest known-vars))
              (%php-skip-to-stmt-end rest))
          known-vars))

(define-php-stmt-parser :use (rest known-vars)
  (values (make-ast-progn :forms nil) (%php-skip-to-stmt-end rest) known-vars))

(define-php-stmt-parser :trait (rest known-vars) (%php-parse-classlike rest known-vars))
(define-php-stmt-parser :interface (rest known-vars) (%php-parse-classlike rest known-vars))
(define-php-stmt-parser :enum (rest known-vars) (%php-parse-classlike rest known-vars :enum-p t))

(define-php-stmt-parser :function (rest known-vars)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((fn-name (php-ident-sym (php-tok-value name-tok))))
      (multiple-value-bind (params rest) (php-parse-param-list rest)
        (let ((rest (%php-skip-return-type rest)))
          (multiple-value-bind (body-stmts rest _) (php-parse-block rest (append params known-vars))
            (declare (ignore _))
            (values (make-ast-defun :name fn-name :params params :body body-stmts)
                    rest known-vars)))))))
;; ─── Keyword aliases (underscore → dash) ─────────────────────────────────
(setf (gethash :include-once *php-stmt-parsers*)
      (gethash :include *php-stmt-parsers*))
(setf (gethash :require-once *php-stmt-parsers*)
      (gethash :require *php-stmt-parsers*))
