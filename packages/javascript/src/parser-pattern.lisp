;;;; packages/javascript/src/parser-pattern.lisp — ES2026 Destructuring Pattern Parser
;;;;
;;;; Parses binding patterns that appear in:
;;;;   - var/let/const declarations
;;;;   - function / arrow-function parameters
;;;;   - catch (e) clauses
;;;;
;;;; Pattern grammar (simplified):
;;;;
;;;;   BindingPattern     ::= ArrayPattern | ObjectPattern | BindingIdent
;;;;   ArrayPattern       ::= '[' ArrayElementList ']'
;;;;   ArrayElementList   ::= (Elision? BindingElement (',' Elision? BindingElement)* (',' Elision? BindingRestElement)?)?
;;;;   BindingElement     ::= BindingPattern ('=' AssignmentExpr)?
;;;;   BindingRestElement ::= '...' BindingPattern
;;;;   Elision            ::= ',' (empty slot)
;;;;
;;;;   ObjectPattern      ::= '{' PropertyList (',' BindingRestProperty)? '}'
;;;;   PropertyList       ::= BindingProperty (',' BindingProperty)*
;;;;   BindingProperty    ::= PropertyName ':' BindingElement | BindingIdent ('=' AssignmentExpr)?
;;;;   BindingRestProperty::= '...' BindingIdent
;;;;
;;;; AST lowering maps patterns to nested ast-let trees:
;;;;
;;;;   Array [a, b] = expr  →
;;;;     (let ((#:tmp expr))
;;;;       (let ((a (%js-get-prop #:tmp 0))
;;;;             (b (%js-get-prop #:tmp 1)))
;;;;         body))
;;;;
;;;;   Object {x: a, y: b} = expr  →
;;;;     (let ((#:tmp expr))
;;;;       (let ((a (%js-get-prop #:tmp "x"))
;;;;             (b (%js-get-prop #:tmp "y")))
;;;;         body))
;;;;
;;;; The BODY placeholder in the lowered forms is passed separately;
;;;; callers wrap the returned bindings-alist + extra-forms into their own
;;;; ast-let construction.
;;;;
;;;; Token stream convention: same as the rest of the JS parser.
;;;; Each token is a plist (:type :T-XXX :value val).
;;;; Functions return (values result remaining-stream).

(in-package :cl-cc/javascript)

;;; ─── Struct ─────────────────────────────────────────────────────────────────

(defstruct js-binding-pattern
  "Represents a destructuring binding pattern.

  kind       — :array, :object, or :ident
  elements   — for :array: list of (pattern-or-nil default-or-nil rest-p)
               pattern-or-nil is NIL for elision (hole) slots.
               rest-p is T for the ...rest element.
  properties — for :object: list of (key local-pattern default-or-nil rest-p)
               key is a string (property name).
               local-pattern is a js-binding-pattern (kind :ident or nested).
               rest-p is T for the ...rest property.
  name       — for :ident: the CL symbol naming the variable
  rest       — rest element for :array and :object patterns:
               a symbol (simple ...ident) or a nested js-binding-pattern"
  (kind nil)
  (elements nil)
  (properties nil)
  (name nil)
  (rest nil))

;;; ─── Token stream helpers (mirror of the PHP parser conventions) ─────────────

(defun %js-tok-type  (tok) (getf tok :type))
(defun %js-tok-value (tok) (getf tok :value))

(defun %js-peek       (stream) (car stream))
(defun %js-peek-type  (stream) (when stream (%js-tok-type  (car stream))))
(defun %js-peek-value (stream) (when stream (%js-tok-value (car stream))))

(defun %js-consume (stream)
  "Return (values head tail)."
  (values (car stream) (cdr stream)))

(defun %js-expect (type stream &optional value)
  "Consume a token of TYPE (optionally matching VALUE). Signal error on mismatch."
  (if (and stream
           (eq (%js-peek-type stream) type)
           (or (null value) (equal (%js-peek-value stream) value)))
      (%js-consume stream)
      (error "JS pattern parse error: expected ~S~@[ ~S~] but got ~S"
             type value (%js-peek stream))))

(defmacro %js-skip-token! (stream-var)
  "Advance STREAM-VAR past the current token, discarding it."
  `(setf ,stream-var (nth-value 1 (%js-consume ,stream-var))))

(defmacro %js-expect! (type stream-var &optional value)
  "Consume a required token of TYPE from STREAM-VAR; signal error on mismatch."
  `(setf ,stream-var (nth-value 1 (%js-expect ,type ,stream-var ,value))))

;;; ─── Identifier helper ───────────────────────────────────────────────────────

(defun %js-ident-sym (str)
  "Intern a JS identifier string as a CL symbol in the current package."
  (intern str :cl-cc/javascript))

;;; ─── Forward declaration (js-parse-binding-pattern calls the array/object
;;;     parsers which in turn may call js-parse-binding-pattern recursively) ───

(declaim (ftype (function (list) (values js-binding-pattern list)) js-parse-binding-pattern))

;;; ─── Array pattern ───────────────────────────────────────────────────────────

(defun %js-parse-array-element (stream)
  "Parse one array-pattern element and any default, from STREAM.
  Returns (values element-triple remaining-stream) where element-triple is
  (pattern-or-nil default-or-nil rest-p)."
  (multiple-value-bind (pat rest) (js-parse-binding-pattern stream)
    (setf stream rest)
    (let ((default nil))
      (when (and (eq (%js-peek-type stream) :T-OP)
                 (equal (%js-peek-value stream) "="))
        (%js-skip-token! stream)
        (multiple-value-bind (dflt rest3) (%js-parse-default-expr stream)
          (setf default dflt stream rest3)))
      (values (list pat default nil) stream))))

(defun js-parse-array-pattern (stream)
  "Parse an ArrayPattern starting just AFTER the opening '[' has been consumed.
  Returns (values js-binding-pattern remaining-stream).

  Elision (hole) syntax: a leading comma before a pattern slot leaves that
  index unbound.  Example: [, b] binds index 1 to b and skips index 0."
  (let ((elements nil)
        (rest-pattern nil)
        (done nil))
    (loop until done do
      (let ((type (%js-peek-type stream)))
        (cond
          ((eq type :T-RBRACKET)
           (%js-skip-token! stream)
           (setf done t))
          ((eq type :T-ELLIPSIS)
           (%js-skip-token! stream)
           (multiple-value-bind (pat rest2) (js-parse-binding-pattern stream)
             (setf rest-pattern pat stream rest2))
           (when (eq (%js-peek-type stream) :T-COMMA)
             (%js-skip-token! stream))
           (%js-expect! :T-RBRACKET stream)
           (setf done t))
          ((eq type :T-COMMA)
           (%js-skip-token! stream)
           (push (list nil nil nil) elements))
          (t
           (multiple-value-bind (elem rest) (%js-parse-array-element stream)
             (push elem elements)
             (setf stream rest))
           (if (eq (%js-peek-type stream) :T-COMMA)
               (%js-skip-token! stream)
               (progn (%js-expect! :T-RBRACKET stream) (setf done t)))))))
    (values (make-js-binding-pattern
             :kind :array
             :elements (nreverse elements)
             :rest rest-pattern)
            stream)))

;;; ─── Object pattern ──────────────────────────────────────────────────────────

(defun %js-parse-property-key-string (stream)
  "Parse an object property key (identifier or string literal).
  Returns (values key-string remaining-stream)."
  (let ((type (%js-peek-type stream)))
    (cond
      ;; Identifier used as key (most common: {x, y}, {x: localX})
      ((eq type :T-IDENT)
       (multiple-value-bind (tok rest) (%js-consume stream)
         (values (%js-tok-value tok) rest)))
      ;; String literal key: {"key": value}
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (%js-consume stream)
         (values (%js-tok-value tok) rest)))
      ;; Number literal key: {0: value}
      ((eq type :T-NUMBER)
       (multiple-value-bind (tok rest) (%js-consume stream)
         (values (princ-to-string (%js-tok-value tok)) rest)))
      (t
       (error "JS pattern parse error: expected property key but got ~S"
              (%js-peek stream))))))

(defun js-parse-object-pattern (stream)
  "Parse an ObjectPattern starting just AFTER the opening '{' has been consumed.
  Returns (values js-binding-pattern remaining-stream)."
  (let ((properties nil)
        (rest-pattern nil))
    (loop
      (cond
        ;; End of object pattern
        ((eq (%js-peek-type stream) :T-RBRACE)
         (%js-skip-token! stream)
         (return))
        ;; Rest property: ...ident
        ((eq (%js-peek-type stream) :T-ELLIPSIS)
         (%js-skip-token! stream)
         (multiple-value-bind (pat rest2) (js-parse-binding-pattern stream)
           (setf rest-pattern pat stream rest2))
         (when (eq (%js-peek-type stream) :T-COMMA) (%js-skip-token! stream))
         (%js-expect! :T-RBRACE stream)
         (return))
        ;; Named property shorthand or renamed binding
        (t
         (multiple-value-bind (key-str key-rest) (%js-parse-property-key-string stream)
           (setf stream key-rest)
           (let ((prop (if (eq (%js-peek-type stream) :T-COLON)
                           ;; key: pattern (rename or nested)
                           (progn
                             (%js-skip-token! stream)
                             (multiple-value-bind (local-pat rest2) (js-parse-binding-pattern stream)
                               (setf stream rest2)
                               (let ((default nil))
                                 (when (and (eq (%js-peek-type stream) :T-OP)
                                            (equal (%js-peek-value stream) "="))
                                   (%js-skip-token! stream)
                                   (multiple-value-bind (dflt rest3) (%js-parse-default-expr stream)
                                     (setf default dflt stream rest3)))
                                 (list key-str local-pat default nil))))
                           ;; Shorthand {key} or {key = default}
                           (let ((local-pat (make-js-binding-pattern :kind :ident :name (%js-ident-sym key-str)))
                                 (default nil))
                             (when (and (eq (%js-peek-type stream) :T-OP)
                                        (equal (%js-peek-value stream) "="))
                               (%js-skip-token! stream)
                               (multiple-value-bind (dflt rest2) (%js-parse-default-expr stream)
                                 (setf default dflt stream rest2)))
                             (list key-str local-pat default nil)))))
             (push prop properties)))
         ;; Consume trailing comma or closing brace
         (if (eq (%js-peek-type stream) :T-COMMA)
             (%js-skip-token! stream)
             (progn (%js-expect! :T-RBRACE stream) (return))))))
    (values (make-js-binding-pattern
             :kind :object
             :properties (nreverse properties)
             :rest rest-pattern)
            stream)))

;;; ─── Default expression parser ───────────────────────────────────────────────
;;;
;;; A minimal "stop before comma / close-bracket / close-brace" expression
;;; parser used for default values in patterns.  The JS expression parser
;;; (parser-expr.lisp, loaded later) may replace this with a full call once
;;; that file is present; for now we grab tokens until a delimiter or EOF.

(defun %js-parse-default-expr (stream)
  "Parse a default-value expression for a pattern element.
  Consumes tokens until it hits a non-nested ',' ']' or '}' (or EOF).
  Returns (values ast-node remaining-stream).

  The returned AST node is the minimal representation needed for lowering:
  a single-token constant becomes an ast-int or ast-quote; a multi-token
  expression is wrapped in an ast-call to preserve the raw token sequence
  as a quoted list (a stub until parser-expr.lisp provides full parsing)."
  (let ((depth 0)
        (toks nil))
    (loop
      (let ((type (%js-peek-type stream)))
        (cond
          ;; EOF — stop
          ((or (null type) (eq type :T-EOF))
           (return))
          ;; Opening brackets increase depth
          ((member type '(:T-LPAREN :T-LBRACKET :T-LBRACE) :test #'eq)
           (incf depth)
           (multiple-value-bind (tok rest) (%js-consume stream)
             (push tok toks)
             (setf stream rest)))
          ;; Closing brackets decrease depth; stop when depth reaches 0
          ((member type '(:T-RPAREN :T-RBRACKET :T-RBRACE) :test #'eq)
           (if (> depth 0)
               (progn
                 (decf depth)
                 (multiple-value-bind (tok rest) (%js-consume stream)
                   (push tok toks)
                   (setf stream rest)))
               (return)))
          ;; Comma at depth 0 — stop (it's the element separator)
          ((and (eq type :T-COMMA) (zerop depth))
           (return))
          ;; Any other token — consume
          (t
           (multiple-value-bind (tok rest) (%js-consume stream)
             (push tok toks)
             (setf stream rest))))))
    (let ((collected (nreverse toks)))
      (values (%js-toks-to-ast collected) stream))))

(defun %js-toks-to-ast (toks)
  "Convert a collected token list to a minimal AST node for use as a default value.
  Single numeric literal → ast-int.
  Single string literal  → ast-quote of string.
  :T-TRUE / :T-FALSE / :T-NULL / :T-UNDEFINED → ast-quote of boolean/nil.
  Single identifier      → ast-var.
  Everything else        → ast-call to %js-raw-default wrapping a quoted token list
                           (opaque placeholder; callers may substitute a proper expr)."
  (cond
    ((null toks)
     (make-ast-quote :value nil))
    ((= (length toks) 1)
     (let* ((tok  (first toks))
            (type (%js-tok-type tok))
            (val  (%js-tok-value tok)))
       (case type
         (:T-NUMBER    (make-ast-int :value val))
         (:T-BIGINT    (make-ast-int :value val))
         (:T-STRING    (make-ast-quote :value val))
         (:T-TRUE      (make-ast-quote :value t))
         (:T-FALSE     (make-ast-quote :value nil))
         (:T-NULL      (make-ast-quote :value nil))
         (:T-UNDEFINED (make-ast-quote :value nil))
         (:T-IDENT     (make-ast-var :name (%js-ident-sym val)))
         (otherwise    (make-ast-quote :value val)))))
    ;; Multi-token: emit an opaque marker for deferred expansion
    (t
     (make-ast-call
      :func (make-ast-var :name '%js-raw-default)
      :args (list (make-ast-quote :value toks))))))

;;; ─── Top-level pattern dispatcher ───────────────────────────────────────────

(defun js-parse-binding-pattern (stream)
  "Parse a BindingPattern from STREAM.  Dispatches on the leading token:
    '[' → ArrayPattern  '{' → ObjectPattern  identifier → ident pattern.
  Returns (values js-binding-pattern remaining-stream)."
  (case (%js-peek-type stream)
    (:T-LBRACKET
     (%js-skip-token! stream)
     (js-parse-array-pattern stream))
    (:T-LBRACE
     (%js-skip-token! stream)
     (js-parse-object-pattern stream))
    (:T-IDENT
     (multiple-value-bind (tok rest) (%js-consume stream)
       (values (make-js-binding-pattern :kind :ident :name (%js-ident-sym (%js-tok-value tok)))
               rest)))
    (t
     (error "JS pattern parse error: expected '[', '{', or identifier but got ~S"
            (%js-peek stream)))))


;;; AST lowering (%js-build-pattern-let, %js-lower-element, js-lower-binding-pattern)
;;; → see parser-pattern-lower.lisp
