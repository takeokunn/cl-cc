;;;; packages/javascript/src/parser.lisp - ES2026 JavaScript Parser Infrastructure
;;;;
;;;; Provides the token-stream helpers, context variables, name utilities,
;;;; forward declarations, and top-level entry points for the ES2026 parser.
;;;;
;;;; Token stream format: list of token plists (:type :T-XXX :value val).
;;;; Matches the same style as the PHP parser infrastructure.
;;;;
;;;; Entry points:
;;;;   parse-js-source source &key strict-mode module-p → list of AST nodes
;;;;   parse-js-module  source                          → list of AST nodes
;;;;
;;;; Recursive statement / expression parsers live in parser-stmt.lisp,
;;;; parser-expr.lisp, parser-class.lisp, parser-module.lisp, and
;;;; parser-pattern.lisp.

(in-package :cl-cc/javascript)

;;; ─── Token Accessors ────────────────────────────────────────────────────────

(defun js-tok-type (tok)
  "Return the :type field of a token plist."
  (getf tok :type))

(defun js-tok-value (tok)
  "Return the :value field of a token plist."
  (getf tok :value))

;;; ─── Stream Peek / Consume ──────────────────────────────────────────────────

(defun js-peek (stream)
  "Return the first token of STREAM without consuming it, or NIL if empty."
  (car stream))

(defun js-peek-type (stream)
  "Return the :type of the first token in STREAM, or NIL if STREAM is empty."
  (when stream (getf (car stream) :type)))

(defun js-peek-value (stream)
  "Return the :value of the first token in STREAM, or NIL if STREAM is empty."
  (when stream (getf (car stream) :value)))

(defun js-consume (stream)
  "Return (values first-token rest-of-stream). Does not error on empty stream."
  (values (car stream) (cdr stream)))

;;; ─── Expect / Try-consume ───────────────────────────────────────────────────

(defun js-expect (type stream &optional value)
  "Consume a token of TYPE (optionally matching VALUE) from STREAM.
Returns (values token rest-stream). Signals an error on mismatch."
  (if (and stream
           (eq (js-peek-type stream) type)
           (or (null value) (equal (js-peek-value stream) value)))
      (js-consume stream)
      (error "JS parse error: expected ~S~@[ ~S~] but got ~S"
             type value (js-peek stream))))

(defun js-at-eof-p (stream)
  "Return T when STREAM is exhausted or its first token is :T-EOF."
  (or (null stream) (eq (js-peek-type stream) :T-EOF)))

(defun js-try-consume (type stream &optional value)
  "Consume a token of TYPE (optionally matching VALUE) if present.
Returns (values token rest-stream) on match, or (values NIL stream) on mismatch."
  (if (and stream
           (eq (js-peek-type stream) type)
           (or (null value) (equal (js-peek-value stream) value)))
      (js-consume stream)
      (values nil stream)))

(defun js-skip-semi (stream)
  "Consume a semicolon token if present (simplified ASI).
Returns the remaining stream with or without the semicolon consumed."
  (if (and stream (eq (js-peek-type stream) :T-SEMI))
      (cdr stream)
      stream))

;;; ─── Parser Context Variables ───────────────────────────────────────────────

(defvar *js-in-function* nil
  "Non-NIL when parsing inside a function body.
Enables return statements and function-specific error messages.")

(defvar *js-in-generator* nil
  "Non-NIL when parsing inside a generator function (function*).
Enables yield expressions.")

(defvar *js-in-async* nil
  "Non-NIL when parsing inside an async function.
Enables await expressions.")

(defvar *js-in-class* nil
  "Non-NIL when parsing inside a class body.
Enables super, private fields, and class-specific validation.")

(defvar *js-strict-mode* nil
  "Non-NIL when the current parse context is strict-mode JavaScript.
Automatically set to T in module mode and when a 'use strict' directive is seen.")

(defvar *js-module-mode* nil
  "Non-NIL when parsing an ES module (enables import/export, implies strict mode).")

;;; ─── Recursion Depth Guard ───────────────────────────────────────────────────
;;; The expression and statement parsers are mutually recursive with no inherent
;;; bound, so pathologically nested input (e.g. "((((((…))))))" or "[[[[…]]]]")
;;; could exhaust the control stack (CWE-674 DoS) when parsing untrusted source.
;;; with-js-parse-depth bounds the recursion and errors gracefully instead.

(defvar *js-parse-depth* 0
  "Current expression/statement recursion depth during parsing.")

(defparameter *js-max-parse-depth* 2500
  "Maximum expression/statement nesting depth before a graceful parse error.")

(defmacro with-js-parse-depth (&body body)
  "Increment *js-parse-depth* for the dynamic extent of BODY (auto-restored on
unwind) and signal a parse error past *js-max-parse-depth* rather than
overflowing the control stack."
  `(let ((*js-parse-depth* (1+ *js-parse-depth*)))
     (when (> *js-parse-depth* *js-max-parse-depth*)
       (error "JS parse error: nesting too deep (limit ~D) — refusing to overflow the stack"
              *js-max-parse-depth*))
     ,@body))

;;; ─── Name Helpers ───────────────────────────────────────────────────────────

(defun js-ident-sym (str)
  "Convert a JS identifier string STR to an interned CL symbol in :cl-cc/javascript.
Accepts both strings and symbols.  Preserves CASE — JavaScript is case-sensitive,
so `a'/`A', `foo'/`Foo', and a lowercase instance var vs its Class name must be
distinct symbols.  Must stay in sync with %js-binding-sym (binding side) and the
parser-class.lisp copy."
  (intern (if (stringp str) str (symbol-name str))
          :cl-cc/javascript))

(defun js-private-sym (str)
  "Convert a #privateField name STR to an interned CL symbol in :cl-cc/javascript.
The resulting symbol name has a PRIVATE/ prefix so it is visually distinct."
  (intern (concatenate 'string "PRIVATE/"
                       (string-upcase (if (stringp str) str (symbol-name str))))
          :cl-cc/javascript))

;;; ─── Forward Reference Declarations ────────────────────────────────────────

(declaim (ftype (function (t) (values list t)) %js-parse-all-stmts))

;;; ─── Top-Level Entry Points ─────────────────────────────────────────────────
;;;
;;; The actual statement-parsing work is delegated to %js-parse-all-stmts,
;;; which is implemented in parser-stmt.lisp (loaded after this file).

(defun parse-js-source (source &key strict-mode module-p)
  "Parse a JavaScript SOURCE string and return a list of top-level AST nodes.
STRICT-MODE enables strict-mode validation.
MODULE-P treats the source as an ES module (implies strict mode)."
  (let ((*js-strict-mode* (or strict-mode module-p))
        (*js-module-mode* module-p))
    (let ((tokens (tokenize-js-source source)))
      (multiple-value-bind (stmts rest) (%js-parse-all-stmts tokens)
        (declare (ignore rest))
        stmts))))

(defun parse-js-module (source)
  "Parse a JavaScript ES module SOURCE string.
Equivalent to (parse-js-source source :strict-mode t :module-p t)."
  (parse-js-source source :strict-mode t :module-p t))

(defun %js-finish-let-bindings (stmts)
  "Nest each empty-bodied ast-let (from `let'/`const'/`var x = …') around the
statements that follow it in STMTS, so the binding scopes over the rest of the
block. Walks backwards; non-let statements and already-bodied lets pass through.
The JS analog of the PHP frontend's php-finish-let-bindings — without it
`let total = 0; … total …' leaves total unscoped and reads as undefined.

A multi-binding empty-bodied let (from `let a = 1, b = a + 1' or destructuring,
where a = tmp[0] must see the tmp = init binding) is expanded into NESTED
single-binding lets: ast-let binds in parallel, but JS declarations are sequential
(let*), so earlier bindings must scope over later initializers."
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
          (let ((nested tail)
                (decls (ast-let-declarations stmt)))
            ;; Right-to-left: the innermost let wraps TAIL; each earlier binding
            ;; wraps the let built so far, so it scopes over later initializers.
            (dolist (binding (reverse (ast-let-bindings stmt)))
              (setf nested (list (make-ast-let :bindings (list binding)
                                               :body nested
                                               :declarations decls))))
            (setf tail nested))
          (push stmt tail)))))

(defun %js-callable-body (body-stmts)
  "Wrap a JS function/method BODY-STMTS list in (block nil ...) so JS `return'
(which lowers to (return-from nil …)) exits the function. A bare ast-defun
establishes no block named NIL; without this the body fails to compile, the
top-level handler-case silently drops the defun, and calls hit `Undefined
function'. A NIL/empty body passes through unchanged."
  (if body-stmts
      (list (make-ast-block :name nil :body body-stmts))
      body-stmts))
