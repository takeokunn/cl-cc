;;;; packages/javascript/src/parser-class.lisp — ES2026 JavaScript class parser
;;;;
;;;; Parses ES2026 class declarations and expressions into the common CL-CC AST.
;;;;
;;;; Features covered:
;;;;   - constructor, instance methods, getters, setters
;;;;   - static methods and static fields
;;;;   - private fields and methods (#name)
;;;;   - public class fields (initialised and uninitialised)
;;;;   - static initialisation blocks (static { ... })
;;;;   - computed member names ([expr])
;;;;   - decorators (@decorator) on classes and members
;;;;   - async methods and generator methods
;;;;   - class expressions (class Foo { } assigned to a variable)
;;;;   - inheritance via `extends`
;;;;
;;;; AST lowering strategy (mirrors PHP parser-class.lisp):
;;;;   class C extends B { fields...; methods... }
;;;;   →  (ast-defclass :name C :superclasses (B) :slots ...)
;;;;       where each method slot has its ast-defun in :initform
;;;;       and public/private/static metadata is encoded in :imports

(in-package :cl-cc/javascript)

;;; ─── Token stream helpers (mirrors php-peek / php-consume conventions) ────────

(defun js-tok-type  (tok) (getf tok :type))
(defun js-tok-value (tok) (getf tok :value))

(defun js-peek       (stream) (car stream))
(defun js-peek-type  (stream) (when stream (js-tok-type  (car stream))))
(defun js-peek-value (stream) (when stream (js-tok-value (car stream))))

(defun js-consume (stream)
  "Return (values token rest)."
  (values (car stream) (cdr stream)))

(defun js-expect (type stream &optional value)
  "Consume a token of TYPE (optionally matching VALUE). Signals error on mismatch."
  (if (and stream
           (eq (js-peek-type stream) type)
           (or (null value) (equal (js-peek-value stream) value)))
      (js-consume stream)
      (error "JS parse error: expected ~S~@[ ~S~] but got ~S"
             type value (js-peek stream))))

(defun js-at-eof-p (stream)
  "Return T when STREAM is exhausted or at :T-EOF."
  (or (null stream) (eq (js-peek-type stream) :T-EOF)))

(defun js-skip-semis (stream)
  "Skip zero or more automatic semicolons / explicit semicolons."
  (loop while (and stream (eq (js-peek-type stream) :T-SEMI))
        do (setf stream (cdr stream)))
  stream)

;;; ─── Symbol helpers ──────────────────────────────────────────────────────────

(defun js-ident-sym (str)
  "Intern a JS identifier string as a CL symbol in :cl-cc/javascript."
  (intern (string-upcase (if (stringp str) str (symbol-name str)))
          :cl-cc/javascript))

(defun js-private-ident-sym (str)
  "Intern a JS private field name (without #) with a %JS-PRIV- prefix."
  (intern (concatenate 'string "%JS-PRIV-"
                       (string-upcase (if (stringp str) str (symbol-name str))))
          :cl-cc/javascript))

;;; ─── Decorator parser ────────────────────────────────────────────────────────

(defun %js-parse-decorator (stream)
  "Parse a single @expression decorator.
Returns (values decorator-ast rest) where DECORATOR-AST is an ast-call or
ast-var representing the decorator."
  ;; Consume the '@' token (already confirmed :T-AT by caller)
  (multiple-value-bind (_ rest) (js-consume stream)
    (declare (ignore _))
    ;; Decorator body: identifier optionally followed by member chain and/or call
    (multiple-value-bind (name-tok rest2) (js-expect :T-IDENT rest)
      (let ((sym (js-ident-sym (js-tok-value name-tok)))
            (current rest2))
        ;; Walk dotted member access: @foo.bar.baz
        (loop while (and current (eq (js-peek-type current) :T-DOT))
              do (setf current (cdr current))  ; consume dot
              (multiple-value-bind (mem-tok rest3) (js-expect :T-IDENT current)
                (setf sym (js-ident-sym
                           (concatenate 'string
                                        (symbol-name sym) "." (js-tok-value mem-tok)))
                      current rest3)))
        ;; Optional argument list: @decorator(args…)
        (if (and current (eq (js-peek-type current) :T-LPAREN))
            (let ((arg-rest (cdr current))  ; consume '('
                  (args nil))
              (loop until (or (js-at-eof-p arg-rest)
                              (eq (js-peek-type arg-rest) :T-RPAREN))
                    do (push (make-ast-var :name (js-ident-sym "_decorator-arg_"))
                             args)
                       ;; skip tokens until comma or closing paren
                       (loop while (and arg-rest
                                        (not (member (js-peek-type arg-rest)
                                                     '(:T-COMMA :T-RPAREN))))
                             do (setf arg-rest (cdr arg-rest)))
                       (when (and arg-rest (eq (js-peek-type arg-rest) :T-COMMA))
                         (setf arg-rest (cdr arg-rest))))
              (multiple-value-bind (_ rest4) (js-expect :T-RPAREN arg-rest)
                (declare (ignore _))
                (values (make-ast-call :func (make-ast-var :name sym)
                                       :args (nreverse args))
                        rest4)))
            (values (make-ast-var :name sym) current))))))

(defun %js-parse-decorators (stream)
  "Parse zero or more @decorator lines.
Returns (values decorators-list rest)."
  (let ((decorators nil)
        (current stream))
    (loop while (and current (eq (js-peek-type current) :T-AT))
          do (multiple-value-bind (dec rest) (%js-parse-decorator current)
               (push dec decorators)
               (setf current rest)))
    (values (nreverse decorators) current)))

;;; ─── Computed member name ────────────────────────────────────────────────────

(defun %js-parse-computed-name (stream)
  "Parse a computed property name [expr]. Consumes [ expr ].
Returns (values name-ast rest).  name-ast is wrapped in a list with
:computed-name metadata so lowering can distinguish it from a plain symbol."
  (let ((rest (cdr stream)))          ; consume '['
    ;; For now we represent the computed name as a special ast-call node
    ;; that downstream code recognises via its :imports metadata.
    ;; We skip tokens until the matching ].
    (let ((depth 1)
          (current rest)
          (expr-tokens nil))
      (loop while (and current (not (and (eq (js-peek-type current) :T-RBRACKET)
                                         (= depth 1))))
            do (cond
                 ((eq (js-peek-type current) :T-LBRACKET) (incf depth))
                 ((eq (js-peek-type current) :T-RBRACKET) (decf depth)))
               (push (car current) expr-tokens)
               (setf current (cdr current)))
      ;; consume ']'
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACKET current)
        (declare (ignore _))
        ;; Return a placeholder AST; real parsers would call js-parse-expr here
        (let ((name-node
               (make-ast-call
                :func (make-ast-var :name (js-ident-sym "%JS-COMPUTED-NAME"))
                :args (mapcar (lambda (tok)
                                (make-ast-quote :value (js-tok-value tok)))
                              (nreverse expr-tokens))
                :imports (list :js-computed-name t))))
          (values name-node rest2))))))

;;; ─── Member name parser ──────────────────────────────────────────────────────

(defun %js-parse-member-name (stream)
  "Parse a class member name. Returns (values name private-p rest orig-name).
NAME is a symbol for plain/private names or an AST node for computed names.
PRIVATE-P is T when the name was a #privateIdent. ORIG-NAME is the member name's
ORIGINAL-CASE string (js-ident-sym upcases the symbol, but JS property access is
case-sensitive and keys on the source string, so the class lowering stores
methods under ORIG-NAME); NIL for computed names."
  (cond
    ;; Computed name [expr]
    ((eq (js-peek-type stream) :T-LBRACKET)
     (multiple-value-bind (name-ast rest) (%js-parse-computed-name stream)
       (values name-ast nil rest nil)))
    ;; Private identifier #name
    ((eq (js-peek-type stream) :T-PRIVATE-IDENT)
     (multiple-value-bind (tok rest) (js-consume stream)
       (let ((v (js-tok-value tok)))
         (values (js-private-ident-sym v) t rest
                 (if (stringp v) v (symbol-name v))))))
    ;; Regular identifier — also accept keyword names used as method names
    ;; (e.g. class C { get() {} static() {} })
    (t
     (multiple-value-bind (tok rest) (js-consume stream)
       (let ((v (js-tok-value tok)))
         (values (js-ident-sym (if (stringp v) v (symbol-name v)))
                 nil rest
                 (if (stringp v) v (symbol-name v))))))))

;;; ─── Class body member kinds ─────────────────────────────────────────────────

;;; Internal helper: build a slot-def for a get/set accessor.
;;; KIND is :getter or :setter; STREAM points past the consumed 'get'/'set' token.
;;; Getters take no parameters; setters take one.
(defun %js-parse-accessor (kind static-p decorators stream)
  (multiple-value-bind (name private-p rest) (%js-parse-member-name stream)
    (multiple-value-bind (params body rest2) (%js-parse-method-params-body rest)
      (let* ((sym (if (symbolp name)
                      name
                      (gensym (if (eq kind :getter) "JS-GETTER-" "JS-SETTER-"))))
             (slot (make-ast-slot-def
                    :name sym
                    :initform (make-ast-defun :name sym
                                              :params (if (eq kind :getter) nil params)
                                              :body (list body))
                    :allocation (if static-p :class :instance)
                    :imports (%js-member-kind-metadata kind static-p private-p nil nil decorators))))
        (values slot (js-skip-semis rest2))))))

(defun %js-parse-method-params-body (stream)
  "Parse ( params ) { body } for a method. Returns (values params body rest).
Params is a list of symbols; body is an ast-progn of properly-parsed AST nodes.

Previously this function collected method body tokens verbatim and stored them
as (ast-progn (ast-quote raw-token-list)). That meant class method bodies were
never compiled — every method silently became a no-op. Now the body is parsed
via js-parse-stmt-list (the real statement parser) so methods execute normally."
  ;; Parameter list
  (let ((rest (nth-value 1 (js-expect :T-LPAREN stream)))
        (params nil))
    (loop until (or (js-at-eof-p rest) (eq (js-peek-type rest) :T-RPAREN))
          do (cond
               ;; Rest parameter ...name
               ((eq (js-peek-type rest) :T-ELLIPSIS)
                (setf rest (cdr rest))
                (multiple-value-bind (tok rest2) (js-expect :T-IDENT rest)
                  (push (list :rest (js-ident-sym (js-tok-value tok))) params)
                  (setf rest rest2)))
               ;; Normal identifier param (ignore defaults for now)
               ((member (js-peek-type rest) '(:T-IDENT :T-THIS))
                (multiple-value-bind (tok rest2) (js-consume rest)
                  (push (js-ident-sym (js-tok-value tok)) params)
                  (setf rest rest2)
                  ;; Skip default value = expr
                  (when (and rest (eq (js-peek-type rest) :T-OP)
                             (equal "=" (js-peek-value rest)))
                    (setf rest (cdr rest))
                    ;; Skip until comma or close-paren
                    (loop while (and rest
                                     (not (member (js-peek-type rest)
                                                  '(:T-COMMA :T-RPAREN))))
                          do (setf rest (cdr rest))))))
               (t
                ;; Destructuring params or other complex params — skip token
                (setf rest (cdr rest))))
          ;; consume comma separator
          (when (and rest (eq (js-peek-type rest) :T-COMMA))
            (setf rest (cdr rest))))
    (multiple-value-bind (_ rest2) (js-expect :T-RPAREN rest)
      (declare (ignore _))
      ;; Method body { stmts... }
      ;; js-parse-stmt-list consumes the opening { and closing } and returns
      ;; (values stmt-list rest-after-rbrace) — the real statement parser used
      ;; by js-parse-block and js-parse-function-body.
      (multiple-value-bind (_ rest3) (js-expect :T-LBRACE rest2)
        (declare (ignore _))
        (multiple-value-bind (body-stmts rest4) (js-parse-stmt-list rest3)
          (let ((body-ast (make-ast-progn :forms (%js-callable-body body-stmts))))
            (values (nreverse params) body-ast rest4)))))))

(defun %js-member-kind-metadata (kind static-p private-p async-p generator-p decorators
                                  &optional orig-name)
  "Build the :imports plist for a class member slot.
ORIG-NAME is the original-case method name string (case-sensitive key for prototype)."
  (append (list :js-member-kind kind)
          (when orig-name   (list :js-orig-name orig-name))
          (when static-p    (list :js-static    t))
          (when private-p   (list :js-private   t))
          (when async-p     (list :js-async     t))
          (when generator-p (list :js-generator t))
          (when decorators  (list :js-decorators decorators))))

;;; ─── Class body member parser ────────────────────────────────────────────────

(defun %js-parse-class-body-member (stream)
  "Parse one member from a class body.  Returns (values slot-def rest).
Handles: methods, getters, setters, fields, static blocks, decorators."
  ;; 1. Collect leading decorators
  (multiple-value-bind (decorators stream) (%js-parse-decorators stream)
    (let ((static-p nil)
          (async-p nil)
          (generator-p nil)
          (current stream))
      ;; 2. Consume optional 'static'
      (when (and current (eq (js-peek-type current) :T-STATIC))
        (setf static-p t
              current (cdr current)))
      ;; 3. Static initialisation block: static { ... }
      ;; Parse the body via js-parse-stmt-list (real statement parser) so
      ;; static initializer code actually executes, not a raw token dump.
      (when (and static-p current (eq (js-peek-type current) :T-LBRACE))
        (let ((rest (cdr current)))   ; consume '{'
          (multiple-value-bind (body-stmts rest2) (js-parse-stmt-list rest)
            (let* ((body-ast (make-ast-progn :forms body-stmts))
                   (slot (make-ast-slot-def
                          :name (gensym "JS-STATIC-INIT-")
                          :initform body-ast
                          :allocation :class
                          :imports (list :js-member-kind :static-block
                                         :js-decorators decorators))))
              (return-from %js-parse-class-body-member
                (values slot rest2))))))
      ;; 4. 'async' modifier
      (when (and current
                 (eq (js-peek-type current) :T-ASYNC)
                 ;; Only treat as modifier when NOT followed by '=' or ';' (field named async)
                 (not (and (cdr current)
                           (member (js-peek-type (cdr current))
                                   '(:T-OP :T-SEMI :T-RBRACE) :test #'eq)
                           (or (eq (js-peek-type (cdr current)) :T-SEMI)
                               (eq (js-peek-type (cdr current)) :T-RBRACE)
                               (and (eq (js-peek-type (cdr current)) :T-OP)
                                    (equal "=" (js-peek-value (cdr current))))))))
        (setf async-p t
              current (cdr current)))
      ;; 5. Generator star
      (when (and current (eq (js-peek-type current) :T-OP)
                 (equal "*" (js-peek-value current)))
        (setf generator-p t
              current (cdr current)))
      ;; 6. Getter: get NAME () { }
      (when (and current (eq (js-peek-type current) :T-GET)
                 (cdr current)
                 (not (member (js-peek-type (cdr current))
                              '(:T-LPAREN :T-SEMI :T-RBRACE) :test #'eq)))
        (return-from %js-parse-class-body-member
          (%js-parse-accessor :getter static-p decorators (cdr current))))
      ;; 7. Setter: set NAME (param) { }
      (when (and current (eq (js-peek-type current) :T-SET)
                 (cdr current)
                 (not (member (js-peek-type (cdr current))
                              '(:T-LPAREN :T-SEMI :T-RBRACE) :test #'eq)))
        (return-from %js-parse-class-body-member
          (%js-parse-accessor :setter static-p decorators (cdr current))))
      ;; 8. Normal member: parse name, then decide method vs field
      (multiple-value-bind (name private-p rest orig-name) (%js-parse-member-name current)
        ;; Method: name ( ...
        (if (and rest (eq (js-peek-type rest) :T-LPAREN))
            (multiple-value-bind (params body rest2) (%js-parse-method-params-body rest)
              (let* ((sym (if (symbolp name) name (gensym "JS-METHOD-")))
                     (defun-ast (make-ast-defun
                                 :name sym
                                 :params params
                                 :body (list body)))
                     (slot (make-ast-slot-def
                            :name sym
                            :initform defun-ast
                            :allocation (if static-p :class :instance)
                            ;; :js-name carries the ORIGINAL-CASE method name so the
                            ;; class lowering stores it under the key obj.m accesses.
                            :imports (%js-member-kind-metadata
                                      (if (equal (symbol-name sym) "CONSTRUCTOR")
                                          :constructor
                                          :method)
                                      static-p private-p async-p generator-p decorators
                                      orig-name))))
                (values slot (js-skip-semis rest2))))
            ;; Field declaration: name [= expr] ;
            (let ((initform nil)
                  (current rest))
              (when (and current (eq (js-peek-type current) :T-OP)
                         (equal "=" (js-peek-value current)))
                (setf current (cdr current))   ; consume '='
                ;; Collect field-initialiser tokens up to the top-level ';' or
                ;; class-closing '}'. The span is brace-aware: an initialiser
                ;; may itself contain (), [] and {} — e.g. an arrow function
                ;; `() => {}` or an object literal `{a: 1}` — so we track nesting
                ;; depth and only stop on a ';'/'}' seen at depth 0. A naive
                ;; brace-blind scan stops at the arrow body's '}', truncating the
                ;; initialiser and mis-reading that '}' as the end of the class.
                (let ((expr-toks nil)
                      (depth 0))
                  (loop while (and current
                                   (not (and (zerop depth)
                                             (member (js-peek-type current)
                                                     '(:T-SEMI :T-RBRACE)))))
                        do (case (js-peek-type current)
                             ((:T-LBRACE :T-LPAREN :T-LBRACKET) (incf depth))
                             ((:T-RBRACE :T-RPAREN :T-RBRACKET) (decf depth)))
                           (push (car current) expr-toks)
                           (setf current (cdr current)))
                  (setf initform
                        (make-ast-call
                         :func (make-ast-var :name (js-ident-sym "%JS-FIELD-INIT"))
                         :args (mapcar (lambda (tok)
                                         (make-ast-quote :value (js-tok-value tok)))
                                       (nreverse expr-toks))))))
              (let ((sym (if (symbolp name) name (gensym "JS-FIELD-"))))
                (values (make-ast-slot-def
                         :name sym
                         :initform initform
                         :allocation (if static-p :class :instance)
                         :imports (%js-member-kind-metadata
                                   :field static-p private-p nil nil decorators))
                        (js-skip-semis current)))))))))

;;; ─── %js-parse-class-body ────────────────────────────────────────────────────

(defun %js-parse-class-body (stream class-name)
  "Parse { member... } class body.
Returns (values member-list rest).
Each member is an ast-slot-def whose :imports plist carries:
  :js-member-kind  (:constructor :method :getter :setter :field :static-block)
  :js-static       t (when static)
  :js-private      t (when private #name)
  :js-async        t (when async method)
  :js-generator    t (when generator method)
  :js-decorators   list"
  (declare (ignore class-name))
  (multiple-value-bind (_ rest) (js-expect :T-LBRACE stream)
    (declare (ignore _))
    (let ((members nil)
          (current rest))
      (loop
        (setf current (js-skip-semis current))
        (when (or (js-at-eof-p current)
                  (eq (js-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (slot rest2) (%js-parse-class-body-member current)
          (when slot (push slot members))
          (setf current rest2)))
      (multiple-value-bind (_ rest2) (js-expect :T-RBRACE current)
        (declare (ignore _))
        (values (nreverse members) rest2)))))

;;; ─── %js-lower-class-to-ast ──────────────────────────────────────────────────

;;; Accessor shorthands — ast-slot-def uses (:conc-name ast-slot-) so slots
;;; are read as ast-slot-NAME, but :imports is inherited from ast-node which
;;; uses (:conc-name ast-) and is therefore read as ast-imports.

(defun %js-slot-method-p (slot)
  "True when SLOT is an instance method (initform is ast-defun/lambda, not a field)."
  (and slot
       (ast-slot-def-p slot)
       (or (ast-defun-p  (ast-slot-initform slot))
           (ast-lambda-p (ast-slot-initform slot)))
       (not (member :field (ast-imports slot)))))

(defun %js-slot-to-method-lambda (slot)
  "Convert a method slot's initform to an ast-lambda for %js-make-class."
  (let ((fn (ast-slot-initform slot)))
    (cond
      ((ast-lambda-p fn) fn)
      ((ast-defun-p fn)
       (make-ast-lambda :params (ast-defun-params fn)
                        :body   (ast-defun-body  fn)))
      (t nil))))

(defun %js-lower-class-to-ast (name-sym super-expr members decorators)
  "Lower a parsed class to a prototype-based JS class using %js-make-class.
Emits: (defvar ClassName (%js-make-class SuperOrNil CtorOrNil 'method1 fn1 ...))

NAME-SYM   — symbol or NIL for anonymous class expressions
SUPER-EXPR — AST node for the superclass or NIL
MEMBERS    — list of ast-slot-def nodes from %js-parse-class-body
DECORATORS — list of AST nodes for class-level decorators"
  (declare (ignore decorators))
  (let* ((effective-name (or name-sym (gensym "JS-CLASS-")))
         ;; Find constructor (slot whose orig-name is \"constructor\")
         (ctor-slot (find-if (lambda (s)
                               (and (ast-slot-def-p s)
                                    (let ((n (ast-slot-name s)))
                                      (and n (string-equal (symbol-name n) "CONSTRUCTOR")))))
                             members))
         ;; Instance methods (non-constructor, non-static, non-field)
         (method-slots (remove-if (lambda (s)
                                    (or (not (%js-slot-method-p s))
                                        (getf (ast-imports s) :js-static)
                                        (and (ast-slot-name s)
                                             (string-equal (symbol-name (ast-slot-name s))
                                                           "CONSTRUCTOR"))))
                                  members))
         ;; Static methods
         (static-slots (remove-if-not (lambda (s)
                                        (and (%js-slot-method-p s)
                                             (getf (ast-imports s) :js-static)))
                                      members))
         ;; Constructor lambda (nil if no explicit constructor)
         (ctor-lambda (if ctor-slot
                          (%js-slot-to-method-lambda ctor-slot)
                          (make-ast-quote :value nil)))
         ;; Method args: ("name1" fn1 "name2" fn2 ...)
         (method-args
          (loop for slot in method-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                for fn = (%js-slot-to-method-lambda slot)
                when fn
                  append (list (make-ast-quote :value orig-name) fn)))
         ;; %js-make-class call
         (make-class-call
          (make-ast-call
           :func (make-ast-var :name '%js-make-class)
           :args (list* (or super-expr (make-ast-quote :value nil))
                        ctor-lambda
                        method-args)))
         ;; defvar/defparameter node
         (defvar-node
          (make-ast-defvar
           :name effective-name
           :value make-class-call
           :kind 'defparameter))
         ;; Static method bindings (set-prop on the class object)
         (static-bindings
          (loop for slot in static-slots
                for orig-name = (or (getf (ast-imports slot) :js-orig-name)
                                    (let ((n (ast-slot-name slot)))
                                      (if n (string-downcase (symbol-name n)) "")))
                for fn = (%js-slot-to-method-lambda slot)
                when fn
                  collect (make-ast-call
                           :func (make-ast-var :name '%js-set-prop)
                           :args (list (make-ast-var :name effective-name)
                                       (make-ast-quote :value orig-name)
                                       fn)))))
    (list* defvar-node static-bindings)))

;;; ─── js-parse-class-decl (public entry point) ───────────────────────────────

(defun js-parse-class-decl (stream &key expression-p decorators)
  "Parse class [Name] [extends Super] { body }.

EXPRESSION-P — when T, the class keyword was consumed as part of an expression
               context; NAME is optional.
DECORATORS   — list of decorator AST nodes collected before the class keyword.

Returns (values ast-list rest) where AST-LIST is a list of AST nodes produced
by %js-lower-class-to-ast."
  ;; The 'class' keyword has already been consumed by the caller.
  (let ((current stream)
        (class-name nil)
        (super-expr nil))
    ;; Optional class name: present in declarations, optional in expressions
    (when (eq (js-peek-type current) :T-IDENT)
      (multiple-value-bind (tok rest) (js-consume current)
        (setf class-name (js-ident-sym (js-tok-value tok))
              current rest)))
    (when (and (not expression-p) (null class-name))
      (error "JS parse error: class declaration requires a name"))
    ;; Optional: extends SuperClass
    (when (and current (eq (js-peek-type current) :T-EXTENDS))
      (setf current (cdr current))   ; consume 'extends'
      ;; Parse super-class reference (simple identifier or member expression)
      (multiple-value-bind (tok rest) (js-expect :T-IDENT current)
        (setf super-expr (make-ast-var :name (js-ident-sym (js-tok-value tok)))
              current rest)
        ;; Walk optional .Member access on super expression
        (loop while (and current (eq (js-peek-type current) :T-DOT))
              do (setf current (cdr current))   ; consume '.'
              (multiple-value-bind (mem-tok rest2) (js-expect :T-IDENT current)
                (setf super-expr
                      (make-ast-call
                       :func (make-ast-var :name (js-ident-sym "%JS-GET-PROP"))
                       :args (list super-expr
                                   (make-ast-quote
                                    :value (js-tok-value mem-tok))))
                      current rest2)))))
    ;; Class body
    (multiple-value-bind (members rest) (%js-parse-class-body current class-name)
      (values (%js-lower-class-to-ast class-name super-expr members decorators)
              rest))))
