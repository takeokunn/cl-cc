;;;; php84-features.lisp — PHP 8.0-8.4 feature implementations
;;;;
;;;; Covers:
;;;;   - PHP 8.4 Property Hooks (get/set, short and long forms)
;;;;   - PHP 8.4 Asymmetric Visibility (public private(set))
;;;;   - PHP 8.4 Array functions (array_find, array_find_key, array_any, array_all)
;;;;   - PHP 8.0 Named Arguments
;;;;   - PHP 8.1 First-Class Callables (strlen(...) syntax)
;;;;   - PHP 8.1 Fibers
;;;;   - PHP 8.1 Intersection Types
;;;;   - PHP 8.2 Readonly Classes
;;;;
;;;; Load order: after parser-class.lisp (all parser functions must be defined).

(in-package :cl-cc/php)

;;; ─── PHP 8.4 Property Hooks ─────────────────────────────────────────────────
;;;
;;; PHP 8.4 allows properties to have get/set hooks:
;;;
;;;   class User {
;;;     public string $name {
;;;       get { return $this->_name; }
;;;       set(string $v) { $this->_name = trim($v); }
;;;     }
;;;     public string $title {
;;;       get => $this->title;
;;;       set => $this->title = $value;
;;;     }
;;;     public private(set) string $id;
;;;   }
;;;
;;; Hooks lower to synthetic getter/setter methods prefixed __get_ / __set_.

(defun %php-parse-property-hook-short (stream known-vars)
  "Parse `=> expr` short hook body. Returns (values ast rest)."
  (multiple-value-bind (arrow-tok rest) (php-consume stream)
    (declare (ignore arrow-tok))
    (multiple-value-bind (expr rest2 _kv) (php-parse-expr rest known-vars)
      (declare (ignore _kv))
      (values expr rest2))))

(defun %php-parse-property-hook-long (stream known-vars)
  "Parse `{ stmts }` long hook body. Returns (values stmts rest)."
  (multiple-value-bind (body-stmts rest _kv) (php-parse-block stream known-vars)
    (declare (ignore _kv))
    (values body-stmts rest)))

(defun %php-parse-one-property-hook (stream known-vars)
  "Parse one hook clause: get or set(param). Returns (values kind param-sym ast rest)."
  (unless (and stream (eq (php-peek-type stream) :T-IDENT))
    (error "PHP 8.4 property hook error: expected 'get' or 'set', got ~S" (php-peek stream)))
  (multiple-value-bind (name-tok rest) (php-consume stream)
    (let* ((hook-kind (string-downcase (php-tok-value name-tok)))
           (param-sym nil)
           (current rest))
      (unless (member hook-kind '("get" "set") :test #'string=)
        (error "PHP 8.4 property hook error: unknown hook '~A', expected get or set" hook-kind))
      ;; set may have an optional typed parameter: set(Type $param)
      (when (and (string= hook-kind "set")
                 (eq (php-peek-type current) :T-LPAREN))
        (let ((inner (%php-consume-expected :T-LPAREN current)))
          (multiple-value-bind (_type after-type) (php-parse-type-annotation inner)
            (declare (ignore _type))
            (setf current after-type))
          (multiple-value-bind (var-tok after-var) (php-expect :T-VAR current)
            (setf param-sym (php-var-sym (php-tok-value var-tok))
                  current (%php-consume-expected :T-RPAREN after-var)))))
      ;; Body: => expr  OR  { stmts }
      (cond
        ((and (eq (php-peek-type current) :T-OP)
              (equal "=>" (php-peek-value current)))
         (multiple-value-bind (body-expr rest2)
             (%php-parse-property-hook-short current known-vars)
           (values (intern (string-upcase hook-kind) :keyword) param-sym body-expr rest2)))
        ((eq (php-peek-type current) :T-LBRACE)
         (multiple-value-bind (body-stmts rest2)
             (%php-parse-property-hook-long current known-vars)
           (values (intern (string-upcase hook-kind) :keyword) param-sym body-stmts rest2)))
        (t
         (error "PHP 8.4 property hook error: expected => or { after hook name, got ~S"
                (php-peek current)))))))

(defun %php-parse-property-hook-body (stream known-vars)
  "Parse { get {...} set(param) {...} } or { get => expr; set => expr; }.
Returns plist (:get getter-ast :set setter-ast :set-param set-param-sym) or nil."
  (unless (and stream (eq (php-peek-type stream) :T-LBRACE))
    (return-from %php-parse-property-hook-body (values nil stream)))
  (let ((current (%php-consume-expected :T-LBRACE stream))
        (getter-ast nil)
        (setter-ast nil)
        (setter-param nil))
    (loop
      (setf current (php-skip-semis current))
      (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
        (return))
      (multiple-value-bind (kind param-sym body rest2)
          (%php-parse-one-property-hook current known-vars)
        (case kind
          (:get (setf getter-ast body))
          (:set (setf setter-ast body setter-param param-sym))
          (t (error "PHP 8.4 property hook error: unexpected hook kind ~S" kind)))
        (setf current (php-skip-semis rest2))))
    (values (list :get getter-ast :set setter-ast :set-param setter-param)
            (%php-consume-expected :T-RBRACE current))))

(defun %php-lower-property-with-hooks (prop-name getter-ast setter-ast class-name
                                       &key (setter-param nil))
  "Generate getter __get_PropName and setter __set_PropName method ASTs.
Returns a list of ast-defun nodes."
  (declare (ignore class-name))
  (let* ((prop-str (symbol-name prop-name))
         (getter-name (php-ident-sym (format nil "__GET_~A" prop-str)))
         (setter-name (php-ident-sym (format nil "__SET_~A" prop-str)))
         (this-sym (php-var-sym "$this"))
         (value-sym (or setter-param (php-var-sym "$value")))
         (result nil))
    (when getter-ast
      (let ((body (if (listp getter-ast)
                      getter-ast
                      (list (make-ast-return-from :name nil :value getter-ast)))))
        (push (make-ast-defun :name getter-name
                              :params (list this-sym)
                              :declarations nil
                              :body (%php-callable-body body))
              result)))
    (when setter-ast
      (let ((body (if (listp setter-ast)
                      setter-ast
                      (list setter-ast))))
        (push (make-ast-defun :name setter-name
                              :params (list this-sym value-sym)
                              :declarations nil
                              :body (%php-callable-body body))
              result)))
    (nreverse result)))

(defun %php-parse-asymmetric-visibility (stream)
  "Parse public private(set) or public protected(set).
Returns (values outer-visibility inner-visibility rest-stream).
When no asymmetric visibility is found, inner-visibility is nil."
  (unless (and stream
               (eq (php-peek-type stream) :T-KEYWORD)
               (member (php-peek-value stream) '(:public :protected :private) :test #'eq))
    (return-from %php-parse-asymmetric-visibility
      (values nil nil stream)))
  (multiple-value-bind (outer-tok rest1) (php-consume stream)
    (let ((outer (php-tok-value outer-tok)))
      ;; Look for inner(set) pattern: keyword immediately followed by T-LPAREN
      (if (and rest1
               (eq (php-peek-type rest1) :T-KEYWORD)
               (member (php-peek-value rest1) '(:private :protected) :test #'eq)
               (cdr rest1)
               (eq (php-peek-type (cdr rest1)) :T-LPAREN))
          (multiple-value-bind (inner-tok rest2) (php-consume rest1)
            (let* ((inner (php-tok-value inner-tok))
                   (rest3 (%php-consume-expected :T-LPAREN rest2)))
              (unless (and rest3
                           (eq (php-peek-type rest3) :T-IDENT)
                           (string-equal (php-peek-value rest3) "set"))
                (error "PHP 8.4 asymmetric visibility: expected 'set' inside parentheses, got ~S"
                       (php-peek rest3)))
              (multiple-value-bind (_set-tok rest4) (php-consume rest3)
                (declare (ignore _set-tok))
                (values outer inner (%php-consume-expected :T-RPAREN rest4)))))
          ;; No inner visibility: just the outer keyword was consumed
          (values outer nil rest1)))))

;;; ─── Named Arguments (PHP 8.0) ──────────────────────────────────────────────
;;;
;;; Named arguments allow passing arguments by parameter name:
;;;
;;;   htmlspecialchars(string: $str, flags: ENT_QUOTES, double_encode: false)
;;;
;;; The parser detects IDENT : patterns and tags them as :named in the arg list.

(defun %php-named-arg-p (stream)
  "Return true when STREAM starts with IDENT : (named argument syntax).
Distinguishes from ternary ? : because the token type is :T-COLON not :T-NULLABLE."
  (and stream
       (member (php-peek-type stream) '(:T-IDENT :T-TYPE) :test #'eq)
       (cdr stream)
       (eq (php-peek-type (cdr stream)) :T-COLON)))

(defun %php-arg-descriptor-expression (descriptor)
  "Return the AST expression carried by a parsed PHP call argument DESCRIPTOR."
  (ecase (first descriptor)
    (:positional (second descriptor))
    (:named (third descriptor))
    (:spread (second descriptor))))

(defun %php-named-arg-pair-ast (pair)
  "Return an AST list representing one runtime named-argument PAIR."
  (make-ast-list
   :elements (list (make-ast-quote :value (car pair))
                   (cdr pair))))

(defun %php-parse-named-args (stream known-vars)
  "Parse mixed positional and named args up to T-RPAREN (exclusive).
Returns (values arg-list rest known-vars) where each arg is
(:positional expr) or (:named name expr).
The opening T-LPAREN must already have been consumed by the caller."
  (if (eq (php-peek-type stream) :T-RPAREN)
      (values nil stream known-vars)
      (let ((args nil)
            (current stream)
            (kv known-vars))
        (loop
          (cond
            ;; Spread operator: ...$args
            ((and (eq (php-peek-type current) :T-ELLIPSIS))
             (multiple-value-bind (_tok rest) (php-consume current)
               (declare (ignore _tok))
               (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest kv)
                 (push (list :spread expr) args)
                 (setf current rest2 kv kv2))))
            ;; Named argument: ident : expr
            ((%php-named-arg-p current)
             (let ((arg-name (php-tok-value (php-peek current))))
               (setf current (cddr current))        ; skip ident and colon
               (multiple-value-bind (expr rest2 kv2) (php-parse-expr current kv)
                 (push (list :named arg-name expr) args)
                 (setf current rest2 kv kv2))))
            ;; Positional argument
            (t
             (multiple-value-bind (expr rest2 kv2) (php-parse-expr current kv)
               (push (list :positional expr) args)
               (setf current rest2 kv kv2))))
          (if (and current (eq (php-peek-type current) :T-COMMA))
              (setf current (cdr current))
              (return)))
        (values (nreverse args) current kv))))

(defun %php-named-args-to-positional (arg-descriptors)
  "Lower named args to positional ast expressions for the call AST.
Named args appear as :positional after reordering since the call AST only
holds a flat list. Unknown name order is preserved in declaration order."
  (mapcar #'%php-arg-descriptor-expression arg-descriptors))

(defun %php-apply-named-args (func positional named-alist)
  "Build a PHP function call AST applying NAMED-ALIST overrides after POSITIONAL args.
Named args are appended as a list of (name . value) pairs in a runtime helper
call so the callee can do keyword-style dispatch."
  ;; For now we lower named args to a flat positional call with named args spliced
  ;; in the order they appear. A future pass could reorder by parameter name.
  (let* ((named-ast-pairs
           (mapcar #'%php-named-arg-pair-ast named-alist))
         (all-args (append positional named-ast-pairs)))
    (make-ast-call :func func :args all-args)))

(defun %php-parse-arglist-named (stream known-vars)
  "Parse (arg1, name: val, ...) supporting named arguments.
Assumes stream starts with T-LPAREN.
Returns (values arg-list rest known-vars) where each element is a plain AST
expression (named args are interleaved as positional for now)."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (multiple-value-bind (arg-descs rest2 kv2)
        (%php-parse-named-args rest known-vars)
      (multiple-value-bind (tok2 rest3) (php-expect :T-RPAREN rest2)
        (declare (ignore tok2))
        (values (%php-named-args-to-positional arg-descs) rest3 kv2)))))

;;; ─── First-Class Callables (PHP 8.1) ────────────────────────────────────────
;;;
;;; PHP 8.1 allows creating callable references with `...`:
;;;
;;;   $fn = strlen(...);
;;;   $closure = $obj->method(...);
;;;
;;; We detect T-ELLIPSIS as the sole argument in a call arglist and lower to
;;; a runtime callable reference.

(defun %php-first-class-callable-p (stream)
  "Return true when STREAM holds `( ... )` — first-class callable syntax."
  (and stream
       (eq (php-peek-type stream) :T-LPAREN)
       (cdr stream)
       (eq (php-peek-type (cdr stream)) :T-ELLIPSIS)
       (cddr stream)
       (eq (php-peek-type (cddr stream)) :T-RPAREN)))

(defun %php-callable-ref (func-name-ast)
  "Create a callable reference AST. Returns a lambda that calls FUNC-NAME-AST."
  ;; Lower strlen(...) to a callable trampoline that applies the PHP function.
  (let ((args-sym (gensym "CALLABLE-ARGS-")))
    (make-ast-lambda
     :params (list args-sym)
     :body (list (make-ast-call
                  :func (make-ast-var :name 'apply)
                  :args (list func-name-ast (make-ast-var :name args-sym)))))))

(defun %php-parse-first-class-callable (func-ast stream known-vars)
  "Parse `(...)` after FUNC-AST and lower to a callable reference.
STREAM must begin with T-LPAREN. Returns (values callable-ref rest known-vars)."
  ;; consume ( ... )
  (let* ((rest1 (cdr stream))          ; skip (
         (rest2 (cdr rest1))           ; skip ...
         (rest3 (cdr rest2)))          ; skip )
    (values (%php-callable-ref func-ast) rest3 known-vars)))

;;; ─── Fibers (PHP 8.1) ───────────────────────────────────────────────────────
;;;
;;; PHP 8.1 Fibers provide cooperative multitasking:
;;;
;;;   $fiber = new Fiber(function(): void {
;;;     $value = Fiber::suspend('fiber');
;;;     echo "Value: " . $value;
;;;   });
;;;   $value = $fiber->start();
;;;   $fiber->resume('test');
;;;
;;; We keep Fiber state in a struct, wrapped in a PHP object hash-table so
;;; ordinary `$fiber->start()` style method dispatch uses the existing runtime
;;; slot-value path.

(defstruct php-fiber
  "Runtime representation of a PHP 8.1 Fiber."
  callback
  (suspended-p nil)
  (started-p nil)
  (terminated-p nil)
  result
  send-value
  (resume-fn nil))

(defvar *current-fiber* nil
  "Dynamically bound to the currently executing PHP Fiber, or NIL.")

(defun %php-fiber-state (fiber)
  "Return the internal Fiber state struct stored in FIBER."
  (cond
    ((php-fiber-p fiber) fiber)
    ((and (hash-table-p fiber)
          (php-fiber-p (gethash "__fiber__" fiber)))
     (gethash "__fiber__" fiber))
    (t
     (error "PHP Fiber error: expected Fiber object"))))

(defun %php-fiber-method-symbol (name)
  "Return the hash-table key used by VM slot lookup for Fiber method NAME."
  (php-ident-sym name))

(defun %php-fiber-set-method (object name function)
  "Install Fiber method FUNCTION under PHP and VM lookup keys."
  (setf (gethash name object) function
        (gethash (%php-fiber-method-symbol name) object) function)
  object)

(defun %php-fiber-install-methods (object method-specs)
  "Install Fiber methods described by METHOD-SPECS on OBJECT."
  (dolist (spec method-specs object)
    (%php-fiber-set-method object (first spec) (symbol-function (second spec)))))

(defun %php-fiber-started-p (fiber)
  "Return true when FIBER has been started."
  (php-fiber-started-p (%php-fiber-state fiber)))

(defun %php-fiber-suspended-p (fiber)
  "Return true when FIBER is suspended."
  (php-fiber-suspended-p (%php-fiber-state fiber)))

(defun %php-fiber-start-method (self &rest args)
  (apply #'%php-fiber-start self args))

(defun %php-fiber-resume-method (self &optional (value nil))
  (%php-fiber-resume self value))

(defun %php-fiber-get-return-method (self)
  (%php-fiber-get-return self))

(defun %php-fiber-is-started-method (self)
  (%php-fiber-started-p self))

(defun %php-fiber-is-suspended-method (self)
  (%php-fiber-suspended-p self))

(defun %php-fiber-is-running-method (self)
  (%php-fiber-running-p self))

(defun %php-fiber-is-terminated-method (self)
  (%php-fiber-terminated-p self))

(defparameter +php-fiber-methods+
  '(("start"        %php-fiber-start-method)
    ("resume"       %php-fiber-resume-method)
    ("getReturn"    %php-fiber-get-return-method)
    ("isStarted"    %php-fiber-is-started-method)
    ("isSuspended"  %php-fiber-is-suspended-method)
    ("isRunning"    %php-fiber-is-running-method)
    ("isTerminated" %php-fiber-is-terminated-method))
  "PHP Fiber object method names and their Common Lisp implementations.")

(defun %php-fiber-make (callback)
  "Create a new PHP Fiber with CALLBACK as its body."
  (let ((object (make-hash-table :test 'equal))
        (state (make-php-fiber :callback callback)))
    (setf (gethash "__class__" object) "Fiber"
          (gethash "__fiber__" object) state)
    (%php-fiber-install-methods object +php-fiber-methods+)
    object))

(defun %php-fiber-start (fiber &rest args)
  "Start FIBER, passing ARGS to its callback.
Returns the first suspended value or the final return value."
  (setf fiber (%php-fiber-state fiber))
  (when (php-fiber-started-p fiber)
    (error "PHP Fiber error: cannot start a Fiber that has already been started"))
  (setf (php-fiber-started-p fiber) t)
  ;; Run the callback; any calls to %php-fiber-suspend will throw a continuation tag.
  (let* ((callback (%php-callable-function (php-fiber-callback fiber)))
         (result
           (catch '%php-fiber-suspend-tag
             (let ((*current-fiber* fiber))
               (if callback
                   (apply callback args)
                   (error "PHP Fiber error: callback is not callable"))))))
    (cond
      ((and (consp result) (eq (first result) '%php-fiber-suspend-payload))
       (setf (php-fiber-suspended-p fiber) t
             (php-fiber-result fiber) (second result))
       (php-fiber-result fiber))
      (t
       (setf (php-fiber-terminated-p fiber) t
             (php-fiber-result fiber) result)
       result))))

(defun %php-fiber-resume (fiber &optional (value nil))
  "Resume a suspended FIBER, passing VALUE as the return from Fiber::suspend().
Returns the next suspended value or the final return value."
  (setf fiber (%php-fiber-state fiber))
  (unless (php-fiber-suspended-p fiber)
    (error "PHP Fiber error: cannot resume a Fiber that is not suspended"))
  (setf (php-fiber-suspended-p fiber) nil
        (php-fiber-send-value fiber) value)
  (let ((resume-fn (php-fiber-resume-fn fiber)))
    (if resume-fn
        (let ((result (catch '%php-fiber-suspend-tag
                        (let ((*current-fiber* fiber))
                          (funcall resume-fn value)))))
          (cond
            ((and (consp result) (eq (first result) '%php-fiber-suspend-payload))
             (setf (php-fiber-suspended-p fiber) t
                   (php-fiber-result fiber) (second result))
             (php-fiber-result fiber))
            (t
             (setf (php-fiber-terminated-p fiber) t
                   (php-fiber-result fiber) result)
             result)))
        ;; No resume fn: fiber was started but suspended without CPS — return sent value
        (progn
          (setf (php-fiber-terminated-p fiber) t)
          +php-null+))))

(defun %php-fiber-suspend (value)
  "Suspend the current PHP Fiber, returning VALUE to the caller.
Must be called from within a running Fiber body."
  (unless *current-fiber*
    (error "PHP Fiber error: Fiber::suspend() called outside of a Fiber"))
  (throw '%php-fiber-suspend-tag (list '%php-fiber-suspend-payload value)))

(defun %php-fiber-get-return (fiber)
  "Return the final return value of a terminated FIBER."
  (setf fiber (%php-fiber-state fiber))
  (unless (php-fiber-terminated-p fiber)
    (error "PHP Fiber error: cannot get return value of a Fiber that has not terminated"))
  (php-fiber-result fiber))

(defun %php-fiber-running-p (fiber)
  "Return true when FIBER is currently executing."
  (setf fiber (%php-fiber-state fiber))
  (and (php-fiber-started-p fiber)
       (not (php-fiber-suspended-p fiber))
       (not (php-fiber-terminated-p fiber))))

(defun %php-fiber-terminated-p (fiber)
  "Return true when FIBER has finished execution."
  (php-fiber-terminated-p (%php-fiber-state fiber)))

;;; ─── PHP 8.4 Array Functions ────────────────────────────────────────────────
;;;
;;; array_find, array_find_key, array_any, array_all are defined in
;;; runtime-builtins-array.lisp (loaded before runtime-builtins-register.lisp).

;;; ─── PHP 8.2 Readonly Classes ───────────────────────────────────────────────
;;;
;;; In PHP 8.2 `readonly class Foo {}` implicitly marks every promoted
;;; constructor property as readonly. We lower this by post-processing the
;;; parsed slot list.

(defun %php-mark-prop-readonly (member)
  "Return MEMBER with readonly metadata when it is an instance property slot."
  (if (and (ast-slot-def-p member)
           (eq (ast-slot-allocation member) :instance)
           (not (ast-defun-p (ast-slot-initform member))))
      (let ((copy (copy-structure member)))
        (setf (ast-imports copy)
              (append (ast-imports copy)
                      (list :readonly-p t)))
        copy)
      member))

(defun %php-mark-all-props-readonly (class-members)
  "Mark every property slot in CLASS-MEMBERS as readonly.
Only :instance allocation slots are affected; methods and constants are skipped."
  (mapcar #'%php-mark-prop-readonly class-members))

;;; ─── PHP 8.1 Intersection Types ─────────────────────────────────────────────
;;;
;;; PHP 8.1 allows intersection types:
;;;
;;;   function f(Iterator&Countable $c): Iterator&Countable { ... }
;;;
;;; The existing php-parse-type-annotation already handles `&` in the type loop,
;;; but we add a dedicated helper that constructs a structured type descriptor.

(defun %php-parse-intersection-type (first-type stream)
  "After FIRST-TYPE, consume any `& TypeName` continuations and build an
intersection type descriptor.
Returns (values type-spec rest-stream).
When no & follows, returns FIRST-TYPE and STREAM unchanged."
  (if (and stream
           (eq (php-peek-type stream) :T-OP)
           (string= (php-peek-value stream) "&")
           (cdr stream)
           (%php-type-atom-token-p (cdr stream)))
      (let ((parts (list first-type))
            (current (cdr stream)))       ; skip &
        (loop
          (unless (%php-type-atom-token-p current) (return))
          (let ((segment (%php-type-token-string (php-peek current))))
            (push segment parts)
            (setf current (cdr current)))
          (if (and (eq (php-peek-type current) :T-OP)
                   (string= (php-peek-value current) "&")
                   (cdr current)
                   (%php-type-atom-token-p (cdr current)))
              (setf current (cdr current))  ; skip & and continue
              (return)))
        (values (list* :intersection (nreverse parts)) current))
      (values first-type stream)))

;;; ─── PHP 8.x Enum Completions ───────────────────────────────────────────────
;;;
;;; Additional enum helpers aligned with PHP 8.1 backed enum semantics.

(defun %php-enum-name (enum-case)
  "Return the symbolic name of a PHP enum case."
  (check-type enum-case hash-table)
  (gethash 'name enum-case +php-null+))

(defun %php-enum-p (value)
  "Return true when VALUE is a PHP enum case (backed or unit)."
  (%php-enum-case-p value))

;;; ─── PHP 8.0 Match Expression — exhaustiveness helper ───────────────────────

(defun %php-match-error ()
  "Signal an UnhandledMatchError equivalent for a non-exhaustive PHP match."
  (error "PHP UnhandledMatchError: Unhandled match case"))

;;; ─── PHP 8.x Nullsafe Chaining — already in parser-expr.lisp ───────────────
;;; See php-parse-postfix: :T-NULLSAFE-ARROW case.

;;; ─── PHP 8.x Readonly Properties — parser integration ───────────────────────
;;;
;;; A readonly property slot is identified by :readonly in the modifiers list.
;;; The existing %php-parse-visibility-modifiers already includes :readonly.
;;; This helper checks the modifier metadata at runtime.

(defun %php-slot-readonly-p (slot)
  "Return true when SLOT metadata contains the :readonly modifier."
  (and (ast-slot-def-p slot)
       (let ((imports (ast-imports slot)))
         (member :readonly (getf imports :php-modifiers) :test #'eq))))

(defun %php-hook-method-slot (method-defun modifiers)
  "Wrap generated property hook METHOD-DEFUN in a method slot."
  (make-ast-slot-def
   :name (ast-defun-name method-defun)
   :initform method-defun
   :imports (%php-slot-metadata modifiers
                                :attributes nil
                                :target-type :method)))

;;; ─── PHP 8.x Union Types — already handled ───────────────────────────────────
;;; The existing php-parse-type-annotation handles `A|B` union types as strings.
;;; %php-parse-intersection-type above extends this for `A&B` structured descriptors.

;;; ─── PHP 8.4 Property Hooks — class-body integration helper ─────────────────
;;;
;;; %php-parse-property-slot-with-hooks is called from the class body member
;;; parser when { appears after a property declaration. It returns both the
;;; original slot-def (for type/visibility metadata) and extra method slot-defs
;;; for the generated __get_* / __set_* accessors.

(defun %php-parse-property-slot-with-hooks (stream modifiers attributes class-name)
  "Parse a property that may be followed by { get/set hook(s) }.
Returns (values slot-def-list rest-stream).
When no hooks are present, the list contains only the original property slot."
  (multiple-value-bind (property-type after-type) (php-parse-type-annotation stream)
    (let ((current (if (and property-type (eq (php-peek-type after-type) :T-VAR))
                       after-type
                       stream))
          (slot-type (when (and property-type (eq (php-peek-type after-type) :T-VAR))
                       property-type)))
      (multiple-value-bind (var-tok rest) (php-consume current)
        (let* ((raw (php-tok-value var-tok))
               (bare (if (and (stringp raw) (plusp (length raw)) (char= (char raw 0) #\$))
                         (subseq raw 1)
                         raw))
               (prop-sym (php-ident-sym bare))
               (initform (make-ast-quote :value +php-null+))
               (base-slot (make-ast-slot-def
                           :name prop-sym
                           :type slot-type
                           :initform initform
                           :allocation (if (member :static modifiers :test #'eq) :class :instance)
                           :imports (%php-slot-metadata modifiers
                                                        :attributes attributes
                                                        :target-type :property))))
          ;; Check for optional default value
          (when (and rest (eq (php-peek-type rest) :T-OP)
                     (equal "=" (php-peek-value rest)))
            (multiple-value-bind (default-ast rest2) (php-parse-expr (cdr rest) nil)
              (setf (ast-slot-initform base-slot) default-ast
                    rest rest2)))
          ;; Check for property hooks { get ... set ... }
          (if (and rest (eq (php-peek-type rest) :T-LBRACE))
              (multiple-value-bind (hooks-plist rest2)
                  (%php-parse-property-hook-body rest nil)
                (if hooks-plist
                    (let* ((getter-ast (getf hooks-plist :get))
                           (setter-ast (getf hooks-plist :set))
                           (setter-param (getf hooks-plist :set-param))
                           (method-slots
                             (loop for method-defun in
                                   (%php-lower-property-with-hooks
                                    prop-sym getter-ast setter-ast class-name
                                    :setter-param setter-param)
                                   collect (%php-hook-method-slot method-defun modifiers))))
                      (values (cons base-slot method-slots)
                              (php-skip-semis rest2)))
                    (values (list base-slot) (php-skip-semis rest2))))
              (values (list base-slot) (php-skip-semis rest))))))))

;;; ─── Late registrations (after php84 functions are defined) ─────────────────

(eval-when (:load-toplevel :execute)
  ;; PHP 8.1 Fiber builtins.
  (when (fboundp '%php-register-builtin)
    (%php-register-builtin "fiber_create"     '%php-fiber-make)
    (%php-register-builtin "fiber_start"      '%php-fiber-start)
    (%php-register-builtin "fiber_resume"     '%php-fiber-resume)
    (%php-register-builtin "fiber_suspend"    '%php-fiber-suspend)
    (%php-register-builtin "fiber_get_return" '%php-fiber-get-return)))
