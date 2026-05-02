;;;; parser-class.lisp — PHP class body parser, statement dispatcher, top-level entry point
(in-package :cl-cc/php)

;;; ─── Class Body Parser ───────────────────────────────────────────────────────

(defun %php-skip-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords (public, private, etc.)."
  (loop while (and stream
                   (eq (php-peek-type stream) :T-KEYWORD)
                   (member (php-peek-value stream)
                           '(:public :private :protected :static
                             :abstract :final :readonly)))
        do (setf stream (cdr stream)))
  stream)

(defun %php-parse-class-body-member (stream known-vars)
  "Parse one class body member: a property declaration or a method definition.
Returns (values slot-def-or-nil remaining-stream)."
  (let ((stream (php-skip-semis (%php-skip-visibility-modifiers stream))))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (multiple-value-bind (var-tok rest) (php-consume stream)
         (let ((slot (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))))
           ;; Skip optional default value
           (when (and rest (eq (php-peek-type rest) :T-OP)
                      (equal "=" (php-peek-value rest)))
             (setf rest (cdr rest))
             (loop while (and rest (not (eq (php-peek-type rest) :T-SEMI)))
                   do (setf rest (cdr rest))))
           (values slot (php-skip-semis rest)))))
      ;; type $var;  — typed property
      ((or (eq (php-peek-type stream) :T-TYPE)
           (eq (php-peek-type stream) :T-NULLABLE))
       (let ((rest (%php-skip-type-annotation stream)))
         (if (eq (php-peek-type rest) :T-VAR)
             (multiple-value-bind (var-tok rest2) (php-consume rest)
               (values (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                       (php-skip-semis rest2)))
             (values nil rest))))
      ;; function name(...) { }  — method
      ((and (eq (php-peek-type stream) :T-KEYWORD)
            (eq (php-peek-value stream) :function))
       (multiple-value-bind (method-ast rest _) (php-parse-statement stream known-vars)
         (declare (ignore _))
         (values (when method-ast
                   (make-ast-slot-def :name (ast-defun-name method-ast)))
                 rest)))
      ;; Unknown token in class body — skip it
      (t (values nil (cdr stream))))))

(defun %php-parse-class-superclasses (stream)
  "Consume optional 'extends ClassName' and 'implements A, B, ...' clauses.
Returns (values superclass-list remaining-stream)."
  (let ((supers nil) (current stream))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :extends))
      (setf current (cdr current))
      (multiple-value-bind (super-tok rest) (php-expect :T-IDENT current)
        (push (php-ident-sym (php-tok-value super-tok)) supers)
        (setf current rest)))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :implements))
      (setf current (cdr current))
      (loop
        (multiple-value-bind (tok rest) (php-expect :T-IDENT current)
          (declare (ignore tok))
          (setf current rest))
        (unless (and current (eq (php-peek-type current) :T-COMMA))
          (return))
        (setf current (cdr current))))
    (values (nreverse supers) current)))

(define-php-stmt-parser :class (rest known-vars)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((class-name (php-ident-sym (php-tok-value name-tok))))
      (multiple-value-bind (supers rest) (%php-parse-class-superclasses rest)
        (let ((current (%php-consume-expected :T-LBRACE rest))
              (slots nil))
          (loop
            (setf current (php-skip-semis current))
            (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
              (return))
            (multiple-value-bind (slot rest2) (%php-parse-class-body-member current known-vars)
              (when slot (push slot slots))
              (setf current rest2)))
          (values (make-ast-defclass :name class-name
                                     :superclasses supers
                                     :slots (nreverse slots))
                  (%php-consume-expected :T-RBRACE current)
                  known-vars))))))

;;; ─── Statement Dispatcher ────────────────────────────────────────────────────

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars).
Dispatches keyword statements through *php-stmt-parsers*; falls through
to %php-parse-expr-stmt for expression statements."
  (let ((handler (when (eq (php-peek-type stream) :T-KEYWORD)
                   (gethash (php-peek-value stream) *php-stmt-parsers*))))
    (if handler
        (multiple-value-bind (_ rest) (php-consume stream)
          (declare (ignore _))
          (funcall handler rest known-vars))
        (%php-parse-expr-stmt stream known-vars))))

;;; ─── Top-Level Entry Point ───────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Post-process: wrap consecutive ast-let :body nil nodes into proper nested lets.
Iterative CPS-style accumulator — walks backwards, building nesting from the end."
  (when (null stmts) (return-from php-finish-let-bindings nil))
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (setf tail
            (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
                (list (make-ast-let :bindings (ast-let-bindings stmt) :body tail))
                (cons stmt tail))))))

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
Analogous to parse-all-forms for CL."
  (let ((stream (tokenize-php-source source))
        (stmts nil) (kv nil))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (when stmt (push stmt stmts))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings (nreverse stmts))))
