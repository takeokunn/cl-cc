;;;; parser-class.lisp — PHP class body parser, statement dispatcher, top-level entry point
(in-package :cl-cc/php)

;;; ─── Class Body Parser ───────────────────────────────────────────────────────

(defun %php-parse-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords and return them as metadata."
  (let ((modifiers nil))
    (loop while (and stream
                     (let ((type (php-peek-type stream))
                           (val  (php-peek-value stream)))
                       (or (and (eq type :T-KEYWORD)
                                (member val '(:public :private :protected :static
                                              :abstract :final :readonly)))
                           ;; `static` lexes as a T-TYPE because it is also the
                           ;; `static` return-type keyword.  In modifier position
                           ;; it is the static-member modifier — accept it here so
                           ;; static properties/methods are flagged :static rather
                           ;; than parsed with a bogus `static` type hint and left
                           ;; instance-allocated (which broke C::$prop and C::m()).
                           (and (eq type :T-TYPE) (eq val :STATIC)))))
          do (let ((type (php-peek-type stream))
                   (val  (php-peek-value stream)))
               (cond
                 ;; PHP 8.4 asymmetric property visibility:
                 ;; public private(set) Type $prop;
                 ((and (eq type :T-KEYWORD)
                       (member val '(:public :protected :private) :test #'eq)
                       (cdr stream)
                       (eq (php-peek-type (cdr stream)) :T-KEYWORD)
                       (member (php-peek-value (cdr stream)) '(:private :protected) :test #'eq)
                       (cddr stream)
                       (eq (php-peek-type (cddr stream)) :T-LPAREN)
                       (cdddr stream)
                       (eq (php-peek-type (cdddr stream)) :T-IDENT)
                       (string-equal (php-peek-value (cdddr stream)) "set")
                       (cddddr stream)
                       (eq (php-peek-type (cddddr stream)) :T-RPAREN))
                  (push val modifiers)
                  (push (cons :set-visibility (php-peek-value (cdr stream))) modifiers)
                  (setf stream (cdr (cddddr stream))))
                 (t
                  (push val modifiers)
                  (setf stream (cdr stream))))))
    (values (nreverse modifiers) stream)))

(defun %php-slot-metadata (modifiers &key class-constant-p attributes target-type)
  "Build AST node metadata for PHP class members."
  (let ((set-visibility (cdr (find :set-visibility modifiers
                                   :key (lambda (modifier)
                                          (when (consp modifier) (car modifier)))
                                   :test #'eq)))
        (plain-modifiers (remove-if #'consp modifiers)))
  (append (when plain-modifiers (list :php-modifiers plain-modifiers))
          (when set-visibility (list :php-set-visibility set-visibility))
          (when class-constant-p (list :php-class-constant t))
          (%php-attribute-metadata attributes target-type))))

(defun %php-parse-class-constant (stream modifiers known-vars attributes)
  "Parse const [TYPE] NAME = VALUE[, NAME = VALUE]*; as class-scoped slots."
  (declare (ignore known-vars))
  (let ((current (cdr stream))
        (constant-type nil)
        (slots nil))
    (multiple-value-bind (parsed-type after-type) (php-parse-type-annotation current)
      (when (and parsed-type (eq (php-peek-type after-type) :T-IDENT))
        (setf constant-type parsed-type
              current after-type)))
    (loop
      (multiple-value-bind (name-tok rest) (php-expect :T-IDENT current)
        (let ((initform nil))
          (setf current rest)
          (when (and current (eq (php-peek-type current) :T-OP)
                     (equal "=" (php-peek-value current)))
            (multiple-value-bind (expr rest2 _) (php-parse-expr (cdr current) nil)
              (declare (ignore _))
              (setf initform expr
                    current rest2)))
          (push (make-ast-slot-def :name (php-ident-sym (php-tok-value name-tok))
                                   :type constant-type
                                   :initform initform
                                   :allocation :class
                                   :imports (%php-slot-metadata modifiers
                                                               :class-constant-p t
                                                               :attributes attributes
                                                               :target-type :constant))
                slots)))
      (unless (and current (eq (php-peek-type current) :T-COMMA))
        (return))
      (setf current (cdr current)))
    (let ((ordered-slots (nreverse slots)))
      (values (first ordered-slots)
              (php-skip-semis current)
              (rest ordered-slots)))))

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
        ;; Use the SAME symbol convention as member access ($o->x): strip the
        ;; leading $ and intern via php-ident-sym (upcased). php-var-sym did not
        ;; upcase, so `public $x' declared slot |x| while $o->x looked up X, giving
        ;; "slot X is missing from the object".
        (let* ((raw  (php-tok-value var-tok))
               (bare (if (and (stringp raw) (plusp (length raw)) (char= (char raw 0) #\$))
                         (subseq raw 1) raw))
               ;; An untyped property with no initializer defaults to PHP null —
               ;; NOT the unbound-slot-marker. Without this, $o->x on `public $x;'
               ;; read as the marker, so is_null($o->x) was false and $o->x ?? d
               ;; never coalesced.
               (initform (make-ast-quote :value +php-null+)))
          ;; Default value: parse it into the slot initform (was discarded).
          (when (and rest (eq (php-peek-type rest) :T-OP)
                     (equal "=" (php-peek-value rest)))
            (multiple-value-bind (default-ast rest2) (php-parse-expr (cdr rest) nil)
              (setf initform default-ast
                    rest rest2)))
          (let ((slot (make-ast-slot-def :name (php-ident-sym bare)
                                         :type slot-type
                                         :initform initform
                                         :allocation (if (member :static modifiers :test #'eq)
                                                         :class
                                                         :instance)
                                         :imports (%php-slot-metadata modifiers
                                                                      :attributes attributes
                                                                      :target-type :property))))
            (values slot (php-skip-semis rest))))))))

(defun %php-promoted-param-assignments (param-attributes)
  "Build (ast-set-slot-value $this->prop $param) nodes for each promoted param.
PARAM-ATTRIBUTES is the :php-param-attributes plist value from an ast-defun."
  (loop for attr-plist in param-attributes
        for param = (car attr-plist)
        when (getf (cdr attr-plist) :php-promote)
          collect (make-ast-set-slot-value
                   :object (make-ast-var :name (php-var-sym "$this"))
                   :slot   (php-ident-sym (symbol-name param))
                   :value  (make-ast-var :name param))))

(defun %php-promoted-property-slots (param-attributes)
  "Build ast-slot-def nodes declaring each promoted param as a class property.
PHP 8.0 constructor promotion both declares AND initializes the property."
  (loop for attr-plist in param-attributes
        for param = (car attr-plist)
        for mods = (getf (cdr attr-plist) :php-promote)
        when mods
          collect (make-ast-slot-def
                   :name      (php-ident-sym (symbol-name param))
                   :initform  (make-ast-quote :value +php-null+)
                   :allocation (if (member :static mods :test #'eq) :class :instance)
                   :imports   (%php-slot-metadata mods :target-type :property))))

(defun %php-parse-class-body-member (stream known-vars &optional class-name)
  "Parse one class body member: a property declaration or a method definition.
Returns (values slot-def-or-nil remaining-stream extra-slots).
EXTRA-SLOTS is a list of additional slots to inject (used for constructor promotion)."
  (multiple-value-bind (attributes stream) (%php-parse-attributes stream)
  (multiple-value-bind (modifiers stream) (%php-parse-visibility-modifiers stream)
  (let ((stream (php-skip-semis stream)))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (multiple-value-bind (slots rest)
           (%php-parse-property-slot-with-hooks stream modifiers attributes class-name)
         (values (first slots) rest (rest slots))))
      ;; type $var;  — typed property, including nullable/union/intersection/DNF types
      ((or (%php-type-atom-token-p stream)
           (eq (php-peek-type stream) :T-LPAREN)
           (eq (php-peek-type stream) :T-NULLABLE))
        (multiple-value-bind (property-type rest) (php-parse-type-annotation stream)
          (if (and property-type (eq (php-peek-type rest) :T-VAR))
              (multiple-value-bind (slots rest2)
                  (%php-parse-property-slot-with-hooks stream modifiers attributes class-name)
                (values (first slots) rest2 (rest slots)))
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
           ;; Instance methods receive an implicit $this first parameter; the call
           ;; site ($o->m(args)) passes the receiver as the first argument. Static
           ;; methods have no receiver, so they are left as-is.
           (when (and method-ast
                      (ast-defun-p method-ast)
                      (not (member :static modifiers :test #'eq)))
             (setf (ast-defun-params method-ast)
                   (cons (php-var-sym "$this") (ast-defun-params method-ast))))
           ;; Constructor property promotion (PHP 8.0+): inject $this->prop = $param;
           ;; at the top of __construct for each param tagged :php-promote.
           (let (promoted-slots)
             (when (and method-ast
                        (ast-defun-p method-ast)
                        (eq (ast-defun-name method-ast) (php-ident-sym "__construct")))
               (let* ((attrs (getf (ast-defun-declarations method-ast) :php-param-attributes))
                      (promos (%php-promoted-param-assignments attrs)))
                 (when promos
                   (setf (ast-defun-body method-ast)
                         (append promos (ast-defun-body method-ast)))
                   (setf promoted-slots (%php-promoted-property-slots attrs)))))
             (values (when method-ast
                       ;; Store full ast-defun in slot-def initform to preserve method body.
                       ;; Static methods are class-allocated so their bound closure lives on
                       ;; the class object itself — C::method() then resolves via slot-value
                       ;; on the class (the same path class constants already use).  Instance
                       ;; methods stay :instance so each object carries its own closure.
                       (make-ast-slot-def :name (ast-defun-name method-ast)
                                          :initform method-ast
                                          :allocation (if (member :static modifiers :test #'eq)
                                                          :class
                                                          :instance)
                                          :imports (%php-slot-metadata modifiers
                                                                       :attributes attributes
                                                                       :target-type :method)))
                     rest
                     promoted-slots))))
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

(defun %php-readonly-class-slots (slots)
  "Mark instance property slots as readonly for PHP 8.2 readonly classes."
  (mapcar (lambda (slot)
            (if (and (ast-slot-def-p slot)
                     (eq (ast-slot-allocation slot) :instance)
                     (not (ast-defun-p (ast-slot-initform slot))))
                (let* ((copy (copy-structure slot))
                       (imports (ast-imports copy))
                       (base-imports (loop for (key value) on imports by #'cddr
                                           unless (member key '(:php-modifiers :readonly-p)
                                                          :test #'eq)
                                             append (list key value)))
                       (modifiers (copy-list (getf imports :php-modifiers))))
                  (unless (member :readonly modifiers :test #'eq)
                    (setf modifiers (append modifiers (list :readonly))))
                  (setf (ast-imports copy)
                        (append base-imports
                                (list :php-modifiers modifiers
                                      :readonly-p t)))
                  copy)
                slot))
          slots))

(defun %php-parse-class-decl (rest known-vars &key readonly-p)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((class-name (php-ident-sym
                       (php-resolve-qualified-name (php-tok-value name-tok) :class))))
      (multiple-value-bind (supers rest) (%php-parse-class-superclasses rest)
        (let ((current (%php-consume-expected :T-LBRACE rest))
              (slots nil)
              ;; Expose the class + parents to expression parsing so self:: /
              ;; static:: / parent:: inside method bodies resolve correctly.
              (*php-current-class* class-name)
              (*php-current-supers* supers))
          (loop
            (setf current (php-skip-semis current))
            (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
              (return))
            (multiple-value-bind (slot rest2 extra-slots) (%php-parse-class-body-member current known-vars class-name)
              (when slot (push slot slots))
              (dolist (es extra-slots) (push es slots))
              (setf current rest2)))
          (setf slots (nreverse slots))
          (when readonly-p
            (setf slots (%php-readonly-class-slots slots)))
          (%php-record-class-trait-uses class-name slots)
          (values (make-ast-defclass :name class-name
                                      :superclasses supers
                                      :slots slots
                                      :php-kind :class)
                  (%php-consume-expected :T-RBRACE current)
                  known-vars))))))

(define-php-stmt-parser :class (rest known-vars)
  (%php-parse-class-decl rest known-vars))

(define-php-stmt-parser :readonly (rest known-vars)
  (unless (and rest
               (eq (php-peek-type rest) :T-KEYWORD)
               (eq (php-peek-value rest) :class))
    (error "PHP readonly modifier is only supported before class declarations near token ~S"
           (php-peek rest)))
  (%php-parse-class-decl (cdr rest) known-vars :readonly-p t))

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
        ;; Inline HTML is emitted verbatim with NO trailing newline and must
        ;; participate in PHP output buffering.
        (values (%php-attach-attributes-to-node
                 (%php-call 'cl-cc/php::%php-output-write
                            (make-ast-quote :value (php-tok-value tok)))
                 attributes)
                rest known-vars)))
    (t
      (let ((handler (when (eq (php-peek-type stream) :T-KEYWORD)
                       (gethash (php-peek-value stream) *php-stmt-parsers*))))
        (if handler
            (multiple-value-bind (_ rest) (php-consume stream)
              (multiple-value-bind (stmt rest2 kv2) (funcall handler rest known-vars)
                (let ((target-type (case (php-tok-value _)
                                     ((:class :readonly) :class)
                                     (:const :constant)
                                     (:function :function)
                                     ((:trait :interface :enum) :class)
                                     (otherwise nil))))
                  (when (and attributes
                             (eq (php-tok-value _) :const)
                             (ast-progn-p stmt))
                    (dolist (form (ast-progn-forms stmt))
                      (%php-attach-attributes-to-node form attributes target-type)))
                  (values (%php-attach-attributes-to-node
                           stmt attributes target-type)
                          rest2 kv2))))
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

This pass is applied at top level and in every braced block (see
php-parse-block), keeping each first assignment visible to the later statements
that share the same PHP block scope."
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
        (*php-pending-top-level-forms* nil)
        (*php-by-ref-param-registry* (%php-seed-by-ref-param-registry))
        (*php-named-param-registry* (make-hash-table :test #'equal)))
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
    (php-finish-let-bindings
     (%php-lower-reference-assignments (nreverse stmts) nil))))
