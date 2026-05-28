;;;; parser-class.lisp — PHP class body parser, statement dispatcher, top-level entry point
(in-package :cl-cc/php)

;;; ─── Class Body Parser ───────────────────────────────────────────────────────

(defun %php-skip-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords (public, private, etc.)."
  (nth-value 1 (%php-parse-visibility-modifiers stream)))

(defun %php-parse-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords and return them as metadata."
  (let ((modifiers nil))
    (loop while (and stream
                     (eq (php-peek-type stream) :T-KEYWORD)
                     (member (php-peek-value stream)
                             '(:public :private :protected :static
                               :abstract :final :readonly)))
          do (push (php-peek-value stream) modifiers)
             (setf stream (cdr stream)))
    (values (nreverse modifiers) stream)))

(defun %php-slot-metadata (modifiers &key class-constant-p)
  "Build AST node metadata for PHP class members."
  (append (when modifiers (list :php-modifiers modifiers))
          (when class-constant-p (list :php-class-constant t))))

(defun %php-parse-class-constant (stream modifiers known-vars)
  "Parse const [TYPE] NAME = VALUE; as a class-scoped slot metadata node."
  (declare (ignore known-vars))
  (let ((current (cdr stream))
        (constant-type nil))
    (multiple-value-bind (parsed-type after-type) (php-parse-type-annotation current)
      (when (and parsed-type (eq (php-peek-type after-type) :T-IDENT))
        (setf constant-type parsed-type
              current after-type)))
    (multiple-value-bind (name-tok rest) (php-expect :T-IDENT current)
      (let ((initform nil)
            (current rest))
        (when (and current (eq (php-peek-type current) :T-OP)
                   (equal "=" (php-peek-value current)))
          (multiple-value-bind (expr rest2 _) (php-parse-expr (cdr current) nil)
            (declare (ignore _))
            (setf initform expr
                  current rest2)))
        (values (make-ast-slot-def :name (php-ident-sym (php-tok-value name-tok))
                                   :type constant-type
                                   :initform initform
                                   :allocation :class
                                   :imports (%php-slot-metadata modifiers
                                                               :class-constant-p t))
                (php-skip-semis current))))))

(defun %php-parse-property-slot (stream modifiers)
  "Parse an untyped or typed PHP property into an AST slot definition."
  (multiple-value-bind (property-type after-type) (php-parse-type-annotation stream)
    (let ((current (if (and property-type (eq (php-peek-type after-type) :T-VAR))
                       after-type
                       stream))
          (slot-type (when (and property-type (eq (php-peek-type after-type) :T-VAR))
                       property-type)))
      (multiple-value-bind (var-tok rest) (php-consume current)
        (let ((slot (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok))
                                       :type slot-type
                                       :allocation (if (member :static modifiers :test #'eq)
                                                       :class
                                                       :instance)
                                       :imports (%php-slot-metadata modifiers))))
          (when (and rest (eq (php-peek-type rest) :T-OP)
                     (equal "=" (php-peek-value rest)))
            (setf rest (%php-skip-expression-like (cdr rest) '(:T-SEMI) nil)))
          (values slot (php-skip-semis rest)))))))

(defun %php-skip-legacy-visibility-modifiers (stream)
  "Legacy modifier skipper retained for callers that only need the stream."
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
  (multiple-value-bind (modifiers stream) (%php-parse-visibility-modifiers stream)
  (let ((stream (php-skip-semis stream)))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (%php-parse-property-slot stream modifiers))
      ;; type $var;  — typed property, including nullable/union/intersection types
      ((or (%php-type-atom-token-p stream)
           (eq (php-peek-type stream) :T-NULLABLE))
       (multiple-value-bind (property-type rest) (php-parse-type-annotation stream)
         (if (and property-type (eq (php-peek-type rest) :T-VAR))
             (%php-parse-property-slot stream modifiers)
             (values nil rest))))
      ;; const [TYPE] NAME = value; — class constants as class-scoped metadata slots.
      ((and (eq (php-peek-type stream) :T-KEYWORD)
             (eq (php-peek-value stream) :const))
       (%php-parse-class-constant stream modifiers known-vars))
      ;; function name(...) { }  — method
      ((and (eq (php-peek-type stream) :T-KEYWORD)
            (eq (php-peek-value stream) :function))
       (multiple-value-bind (method-ast rest _) (php-parse-statement stream known-vars)
          (declare (ignore _))
          (values (when method-ast
                   ;; Store full ast-defun in slot-def initform to preserve method body
                   (make-ast-slot-def :name (ast-defun-name method-ast)
                                       :initform method-ast))
                  rest)))
      ;; Unknown token in class body — skip it
      (t (values nil (cdr stream)))))))

(defun %php-parse-class-superclasses (stream)
  "Consume optional 'extends ClassName' and 'implements A, B, ...' clauses.
Returns (values superclass-list remaining-stream)."
  (let ((supers nil) (current stream))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :extends))
      (setf current (cdr current))
      (multiple-value-bind (super-name rest) (php-parse-qualified-name current)
        (push (php-ident-sym super-name) supers)
        (setf current rest)))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :implements))
      (setf current (cdr current))
      (loop
        (multiple-value-bind (interface-name rest) (php-parse-qualified-name current)
          (declare (ignore interface-name))
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
                (let ((node (make-ast-let :bindings (ast-let-bindings stmt) :body tail)))
                  (setf (ast-namespace node) (ast-namespace stmt)
                        (ast-imports node) (copy-tree (ast-imports stmt)))
                  (list node))
                (cons stmt tail))))))

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
Analogous to parse-all-forms for CL."
  (let ((stream (tokenize-php-source source))
        (stmts nil) (kv nil)
        (*php-current-namespace* nil)
        (*php-current-imports* nil)
        (*php-pending-top-level-forms* nil))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (cond
          (*php-pending-top-level-forms*
           (dolist (form (reverse *php-pending-top-level-forms*))
             (push form stmts))
           (setf *php-pending-top-level-forms* nil))
          (stmt
           (push (php-annotate-top-level-node stmt) stmts)))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings (nreverse stmts))))
