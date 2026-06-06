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

(defun %php-slot-metadata (modifiers &key class-constant-p attributes target-type)
  "Build AST node metadata for PHP class members."
  (append (when modifiers (list :php-modifiers modifiers))
          (when class-constant-p (list :php-class-constant t))
          (%php-attribute-metadata attributes target-type)))

(defun %php-parse-class-constant (stream modifiers known-vars attributes)
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
                                                                :class-constant-p t
                                                                :attributes attributes
                                                                :target-type :constant))
                 (php-skip-semis current))))))

(defun %php-parse-trait-use-member (stream)
  "Parse use TraitName[, OtherTrait]; as class/enum metadata slots."
  (let ((current (cdr stream))
        (slots nil))
    (loop
      (multiple-value-bind (trait-name rest) (php-parse-qualified-name current)
        (let ((trait-sym (php-ident-sym (php-resolve-qualified-name trait-name :class))))
          (push (make-ast-slot-def :name trait-sym
                                   :allocation :class
                                   :imports (list :php-trait-use t))
                slots))
        (setf current rest))
      (unless (eq (php-peek-type current) :T-COMMA)
        (return))
      (setf current (cdr current)))
    (values (make-ast-slot-def :name (gensym "PHP-TRAIT-USE-")
                               :allocation :class
                               :imports (list :php-trait-uses (nreverse slots)))
            (php-skip-semis current))))

(defun %php-parse-property-slot (stream modifiers attributes)
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
                                        :imports (%php-slot-metadata modifiers
                                                                    :attributes attributes
                                                                    :target-type :property))))
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
  (multiple-value-bind (attributes stream) (%php-parse-attributes stream)
  (multiple-value-bind (modifiers stream) (%php-parse-visibility-modifiers stream)
  (let ((stream (php-skip-semis stream)))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (%php-parse-property-slot stream modifiers attributes))
      ;; type $var;  — typed property, including nullable/union/intersection types
      ((or (%php-type-atom-token-p stream)
           (eq (php-peek-type stream) :T-NULLABLE))
        (multiple-value-bind (property-type rest) (php-parse-type-annotation stream)
          (if (and property-type (eq (php-peek-type rest) :T-VAR))
              (%php-parse-property-slot stream modifiers attributes)
              (values nil rest))))
      ;; const [TYPE] NAME = value; — class constants as class-scoped metadata slots.
       ((and (eq (php-peek-type stream) :T-KEYWORD)
               (eq (php-peek-value stream) :const))
        (%php-parse-class-constant stream modifiers known-vars attributes))
       ;; use TraitName[, OtherTrait] [{ insteadof/as block }]; — trait use with
       ;; optional conflict-resolution block, inside class-like bodies.
       ;; Delegate to the full use-trait parser (defined in parser-trait.lisp)
       ;; that handles { insteadof/as } blocks.  The `use` keyword was already
       ;; consumed by the caller that found :use in the token stream, but here
       ;; we are still at the `use` token — strip it before dispatching.
       ((and (eq (php-peek-type stream) :T-KEYWORD)
             (eq (php-peek-value stream) :use))
        (multiple-value-bind (slot rest kv)
            (%php-parse-use-trait-stmt (cdr stream) known-vars)
          (declare (ignore kv))
          (values slot rest)))
      ;; function name(...) { }  — method
      ((and (eq (php-peek-type stream) :T-KEYWORD)
            (eq (php-peek-value stream) :function))
        (multiple-value-bind (method-ast rest _) (php-parse-statement stream known-vars)
           (declare (ignore _))
           (%php-attach-attributes-to-node method-ast attributes :method)
           (values (when method-ast
                    ;; Store full ast-defun in slot-def initform to preserve method body
                    (make-ast-slot-def :name (ast-defun-name method-ast)
                                        :initform method-ast
                                        :imports (%php-slot-metadata modifiers
                                                                    :attributes attributes
                                                                    :target-type :method)))
                   rest)))
      ;; Unknown class-body syntax must not be silently discarded. Silent skip
      ;; made unsupported PHP look successfully parsed while losing source.
      (t (error "PHP unsupported feature in class body near token ~S" (php-peek stream))))))))

(defun %php-parse-class-superclasses (stream)
  "Consume optional 'extends ClassName' and 'implements A, B, ...' clauses.
Returns (values superclass-list remaining-stream)."
  (let ((supers nil) (current stream))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :extends))
      (setf current (cdr current))
      (multiple-value-bind (super-name rest) (php-parse-qualified-name current)
        (push (php-ident-sym (php-resolve-qualified-name super-name :class)) supers)
        (setf current rest)))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :implements))
      (setf current (cdr current))
      (loop
        (multiple-value-bind (interface-name rest) (php-parse-qualified-name current)
          (push (php-ident-sym (php-resolve-qualified-name interface-name :class)) supers)
          (setf current rest))
        (unless (and current (eq (php-peek-type current) :T-COMMA))
          (return))
        (setf current (cdr current))))
    (values (nreverse supers) current)))

(define-php-stmt-parser :class (rest known-vars)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((class-name (php-ident-sym
                       (php-resolve-qualified-name (php-tok-value name-tok) :class))))
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
                                      :slots (nreverse slots)
                                      :php-kind :class)
                  (%php-consume-expected :T-RBRACE current)
                  known-vars))))))

;;; ─── Interface Statement Parser (overrides parser-stmt entry) ───────────────

(define-php-stmt-parser :interface (rest known-vars)
  "%php-parse-interface-decl handles the full interface syntax including
multiple-extends and abstract method signatures."
  (%php-parse-interface-decl rest known-vars))

;;; ─── Statement Dispatcher ────────────────────────────────────────────────────

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars).
Dispatches keyword statements through *php-stmt-parsers*; falls through
to %php-parse-expr-stmt for expression statements."
  (multiple-value-bind (attributes stream) (%php-parse-attributes stream)
  (cond
    ((eq (php-peek-type stream) :T-INLINE-HTML)
      (multiple-value-bind (tok rest) (php-consume stream)
        (values (%php-attach-attributes-to-node
                 (make-ast-print :expr (make-ast-quote :value (php-tok-value tok)))
                 attributes)
                rest known-vars)))
    (t
      (let ((handler (when (eq (php-peek-type stream) :T-KEYWORD)
                       (gethash (php-peek-value stream) *php-stmt-parsers*))))
        (if handler
            (multiple-value-bind (_ rest) (php-consume stream)
              (multiple-value-bind (stmt rest2 kv2) (funcall handler rest known-vars)
                (values (%php-attach-attributes-to-node
                         stmt attributes
                         (case (php-tok-value _)
                           (:class :class)
                           (:function :function)
                           ((:trait :interface :enum) :class)
                           (otherwise nil)))
                        rest2 kv2)))
            (multiple-value-bind (stmt rest2 kv2) (%php-parse-expr-stmt stream known-vars)
              (values (%php-attach-attributes-to-node stmt attributes)
                      rest2 kv2))))))))

;;; ─── Top-Level Entry Point ───────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Wrap each `ast-let' with an empty body around the statements that follow it.

PHP's first assignment to a variable lowers to (ast-let ((var . value)) :body nil)
— a declaration whose scope must cover every later statement in the same block.
Walking STMTS backwards, each empty-bodied let absorbs the already-nested tail as
its body, so `$x = 10; echo $x;' becomes (let (($x 10)) (echo $x)) rather than two
siblings where echo sees an unbound $x. Statements that are not empty-bodied lets
(including lets that already carry a body) pass through as siblings.

Previously a no-op stub, so every PHP variable was bound in a let with an empty
body and was invisible to the rest of the block — `$x = 1; echo $x;' printed
nothing. Applied at top level and in every braced block (see php-parse-block)."
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
          (progn
            (setf (ast-let-body stmt) tail)
            (setf tail (list stmt)))
          (push stmt tail)))))

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
