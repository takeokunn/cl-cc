;;;; packages/javascript/src/parser-expr.lisp — ES2026 JavaScript Expression Parser
;;;;
;;;; Pratt (top-down operator precedence) expression parser for JavaScript.
;;;;
;;;; Token stream is a list of token plists: (:type :T-XXX :value val).
;;;; All parsers return (values ast remaining-stream).
;;;;
;;;; Depends on lexer.lisp for token types and tokenize-js-source.
;;;; Load order: after lexer.lisp, before any statement parser.

(in-package :cl-cc/javascript)

;;; ─── Pratt Precedence Table ──────────────────────────────────────────────────
;;;
;;; Data table for operator string → (prec right-assoc-p).
;;; Type-keyed specials (comma, ternary, instanceof, in, member) remain in the
;;; dispatch function since they match on token type, not string value.

(defparameter *js-op-infix-prec*
  (let ((ht (make-hash-table :test #'equal)))
    ;; Assignment (right-associative, prec 2)
    (dolist (op '("=" "+=" "-=" "*=" "/=" "%=" "**="
                  "<<=" ">>=" ">>>=" "&=" "|=" "^="
                  "&&=" "||=" "??="))
      (setf (gethash op ht) '(2 t)))
    ;; Remaining binary ops (left-associative unless noted)
    (dolist (entry '(("??"  5 nil) ("||"  6 nil) ("&&"  7 nil)
                     ("|"   8 nil) ("^"   9 nil) ("&"  10 nil)
                     ("=="  11 nil) ("!="  11 nil) ("===" 11 nil) ("!==" 11 nil)
                     ("<"   12 nil) (">"   12 nil) ("<="  12 nil) (">="  12 nil)
                     ("<<"  13 nil) (">>"  13 nil) (">>>" 13 nil)
                     ("+"   14 nil) ("-"   14 nil)
                     ("*"   15 nil) ("/"   15 nil) ("%"   15 nil)
                     ("**"  16 t)   ("?."  19 nil)))
      (setf (gethash (first entry) ht) (rest entry)))
    ht)
  "Maps JS operator strings to (prec right-assoc-p). Used by js-infix-prec.")

(defun js-infix-prec (stream)
  "Return (values prec right-assoc-p) for current token as infix op, or (values 0 nil).

Precedence levels: 1=comma 2=assign 4=ternary 5=?? 6=|| 7=&& 8=| 9=^ 10=&
  11=equality 12=relational 13=shift 14=additive 15=multiplicative 16=** 19=member"
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ((eq type :T-COMMA)    (values 1 nil))
      ((eq type :T-QUESTION) (values 4 nil))
      ((or (eq type :T-INSTANCEOF) (eq type :T-IN)) (values 12 nil))
      ((or (eq type :T-DOT) (eq type :T-LBRACKET) (eq type :T-LPAREN)) (values 19 nil))
      ((eq type :T-OP)
       (let ((entry (gethash val *js-op-infix-prec*)))
         (if entry (values (first entry) (second entry)) (values 0 nil))))
      (t (values 0 nil)))))

;;; ─── Operator Lowering ───────────────────────────────────────────────────────

(defparameter *js-direct-binop-keywords*
  (let ((ht (make-hash-table :test #'equal)))
    ;; Values are the operator SYMBOLS the codegen op→constructor table
    ;; (*numeric-binop-ctor-specs*, keyed by symbol with :test #'eq) expects:
    ;; CL +,-,*,/,<,>,<=,>=. They were keywords (:+, :-, …) which never matched,
    ;; so `a + b' raised `Unknown binary operator :+', the enclosing form/function
    ;; body failed to compile, and was silently dropped — every JS program using
    ;; arithmetic broke. `%' and `**' have no direct VM constructor; they fall
    ;; through %js-lower-binary to the %js-binop runtime helper.
    ;; NOTE: `+' is intentionally NOT here — it is polymorphic in JS (numeric add
    ;; OR string concat) and routes through %js-add via *js-binop-runtime-helpers*.
    ;; `- * /' and comparisons are numeric-only and use the direct VM constructors.
    ;; `/' is NOT here — JS division must yield a float (5/2 => 2.5), but the VM
    ;; `/' returns the CL rational 5/2; it routes through %js-divide instead.
    ;; Comparisons (< > <= >=) are NOT here: lowering them to the VM's CL
    ;; comparison returns 1/0 (not a JS boolean) and ignores JS relational
    ;; semantics (string compare, NaN-always-false, ToNumber coercion). They
    ;; route through %js-lt/gt/le/ge via *js-binop-runtime-helpers* instead.
    (dolist (entry '(("-" . -) ("*" . *)))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Maps arithmetic operator strings to AST binop operator symbols.")

(defun js-lower-binop-keyword (op-str)
  "Map operator string to AST binop keyword, or NIL for runtime-dispatch ops."
  (gethash op-str *js-direct-binop-keywords*))

(defparameter *js-binop-runtime-helpers*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (entry '(("+"   . %js-add)  ("/"  . %js-divide)
                     ("%"   . %js-mod)  ("**" . %js-pow)
                     ("===" . %js-strict-eq) ("==" . %js-loose-eq)
                     ("<"   . %js-lt) (">"  . %js-gt)
                     ("<="  . %js-le) (">=" . %js-ge)
                     ("|"   . %js-bitwise-or) ("^"  . %js-bitwise-xor)
                     ("&"   . %js-bitwise-and)
                     ("<<"  . %js-shift-left)  (">>" . %js-shift-right)
                     (">>>" . %js-unsigned-shift-right)
                     ("instanceof" . %js-instanceof) ("in" . %js-in)))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Maps operator strings to their CPS runtime helper symbols.")

(defun %js-call (name &rest args)
  "Build an AST call to a JS runtime helper NAME with ARGS."
  (make-ast-call :func (make-ast-var :name name)
                 :args args))

(defparameter *js-coercion-call-helpers*
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (entry '(("Number" . %js-to-number)
                     ("String" . %js-to-string)
                     ("Boolean" . %js-truthy)
                     ("parseInt" . %js-parse-int)
                     ("parseFloat" . %js-parse-float)
                     ;; Standalone global builtins whose prelude binding is a value,
                     ;; not a callable function symbol — lower the direct call to the
                     ;; helper (the global binding still serves indirect/value use).
                     ("structuredClone" . %js-structured-clone)
                     ("queueMicrotask" . %js-queue-microtask)
                     ("setTimeout" . %js-set-timeout)
                     ("setInterval" . %js-set-interval)
                     ("clearTimeout" . %js-clear-timer)
                     ("clearInterval" . %js-clear-timer)
                     ;; Symbol(desc) constructs a symbol; Symbol.iterator (member
                     ;; access on the global) is unaffected.
                     ("Symbol" . %js-make-symbol)
                     ;; Other global functions whose prelude binding is a value
                     ;; (not a callable symbol), so a bare call needs lowering.
                     ;; (BigInt is intentionally NOT here: BigInt(x) builds a
                     ;; js-bigint struct that the arithmetic ops don't accept,
                     ;; unlike the 10n literal path — a separate bigint fix.)
                     ("encodeURIComponent" . %js-encode-uri-component)
                     ("decodeURIComponent" . %js-decode-uri-component)
                     ("encodeURI" . %js-encode-uri)
                     ("decodeURI" . %js-decode-uri)
                     ("btoa" . %js-btoa)
                     ("atob" . %js-atob)
                     ("isNaN" . %js-is-nan)
                     ("isFinite" . %js-is-finite)))
      (setf (gethash (js-ident-sym (car entry)) ht) (cdr entry)))
    ht)
  "Bare-call coercion builtins -> runtime helper symbols.  Number(x)/String(x)/
Boolean(x)/parseInt/parseFloat are called as functions, but the global holding
each is a value (not a callable function symbol the codegen can dispatch), so a
bare `Number(x)' raised `Undefined function: NUMBER'.  We lower the CALL directly
to the helper; `Number.isInteger' (member access) is unaffected.")

(defun %js-lower-coercion-call (helper args)
  "Lower a coercion builtin CALL to its HELPER. parseInt/parseFloat take the args
as-is (s, radix); Number/String/Boolean take one argument, with the JS empty-call
defaults Number()=0, String()=\"\", Boolean()=false."
  (cond
    ((member helper '(%js-parse-int %js-parse-float
                      %js-structured-clone %js-queue-microtask
                      %js-set-timeout %js-set-interval %js-clear-timer
                      %js-make-symbol
                      %js-encode-uri-component %js-decode-uri-component
                      %js-encode-uri %js-decode-uri %js-btoa %js-atob
                      %js-is-nan %js-is-finite))
     (apply #'%js-call helper args))
    ((null args)
     (case helper
       (%js-to-number (make-ast-int :value 0))
       (%js-to-string (make-ast-quote :value ""))
       (t             (make-ast-quote :value nil))))
    (t (%js-call helper (first args)))))

(defun %js-spread-marker-p (node)
  "True when NODE is a (%js-spread expr) marker produced for ...expr."
  (and (ast-call-p node)
       (let ((f (ast-call-func node)))
         (and (ast-var-p f) (eq (ast-var-name f) '%js-spread)))))

(defun %js-items-have-spread-p (items)
  "True when ITEMS contains a ...expr spread marker."
  (some #'%js-spread-marker-p items))

(defun %js-spread-list-expr (items)
  "Build a runtime list expression that flattens ITEMS: each (%js-spread expr)
marker contributes its values (%js-spread already returns a CL list) and each
regular item contributes a one-element list, appended together. Used to expand
spread in array literals and call arguments via apply."
  (make-ast-call
   :func (make-ast-var :name 'append)
   :args (mapcar (lambda (it)
                   (if (%js-spread-marker-p it)
                       it
                       (make-ast-call :func (make-ast-var :name 'list) :args (list it))))
                 items)))

(defun %js-lower-binary (op-str lhs rhs)
  "Lower a binary operator string + lhs + rhs to the appropriate AST.
   Dispatch is data-driven via *js-direct-binop-keywords* and *js-binop-runtime-helpers*."
  (cond
    ;; Direct AST binop (arithmetic, comparison) — O(1) table lookup
    ((gethash op-str *js-direct-binop-keywords*)
     (make-ast-binop :op (gethash op-str *js-direct-binop-keywords*) :lhs lhs :rhs rhs))
    ;; Logical short-circuit operators. JS && / || yield an OPERAND (not a
    ;; boolean) and short-circuit, so they lower to let+if like ?? does — NOT to
    ;; an ast-binop :and/:or (which codegen cannot emit; the enclosing function
    ;; then failed to compile and was silently dropped — "Undefined function").
    ((string= op-str "||") (%js-lower-logical-or  lhs rhs))
    ((string= op-str "&&") (%js-lower-logical-and lhs rhs))
    ((string= op-str "??") (%js-lower-nullish-coalesce lhs rhs))
    ;; Negated equality — wrap in NOT
    ((string= op-str "!==")
     (make-ast-call :func (make-ast-var :name 'not)
                    :args (list (%js-call '%js-strict-eq lhs rhs))))
    ((string= op-str "!=")
     (make-ast-call :func (make-ast-var :name 'not)
                    :args (list (%js-call '%js-loose-eq lhs rhs))))
    ;; Runtime helpers — O(1) table lookup
    ((gethash op-str *js-binop-runtime-helpers*)
     (%js-call (gethash op-str *js-binop-runtime-helpers*) lhs rhs))
    ;; Fallback: runtime dispatch
    (t (%js-call '%js-binop (make-ast-quote :value (intern op-str :keyword)) lhs rhs))))

(defun %js-lower-nullish-coalesce (lhs rhs)
  "Lower LHS ?? RHS without evaluating LHS twice."
  (let ((tmp (gensym "JS-NC-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%js-call '%js-not-nullish (make-ast-var :name tmp))
                  :then (make-ast-var :name tmp)
                  :else rhs)))))

(defun %js-lower-logical-and (lhs rhs)
  "Lower LHS && RHS: evaluate LHS once; if it is JS-truthy the result is RHS,
otherwise the result is LHS itself (so `0 && x' -> 0, `a && b' -> b)."
  (let ((tmp (gensym "JS-AND-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%js-call '%js-truthy (make-ast-var :name tmp))
                  :then rhs
                  :else (make-ast-var :name tmp))))))

(defun %js-lower-logical-or (lhs rhs)
  "Lower LHS || RHS: evaluate LHS once; if it is JS-truthy the result is LHS,
otherwise the result is RHS (so `3 || x' -> 3, `0 || b' -> b)."
  (let ((tmp (gensym "JS-OR-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%js-call '%js-truthy (make-ast-var :name tmp))
                  :then (make-ast-var :name tmp)
                  :else rhs)))))

(defun %js-logical-assign-short-circuit-p (op-str)
  "Return :truthy for &&=, :falsy for ||=, :non-null for ??="
  (cond ((string= op-str "&&=") :truthy)
        ((string= op-str "||=") :falsy)
        ((string= op-str "??=") :non-null)
        (t (error "JS parse error: unknown logical assign op ~S" op-str))))

(defun %js-lower-logical-assign (op-str lhs-sym rhs)
  "Lower &&= ||= ??= compound logical assignment on a variable.
   CPS-style: dispatches through %js-logical-assign-short-circuit-p to a uniform template."
  (let* ((lhs-var (make-ast-var :name lhs-sym))
         (kind    (%js-logical-assign-short-circuit-p op-str))
         (test    (ecase kind
                    (:truthy   (%js-call '%js-truthy lhs-var))
                    (:falsy    (%js-call '%js-truthy lhs-var))
                    (:non-null (%js-call '%js-not-nullish lhs-var))))
         (setq    (make-ast-setq :var lhs-sym :value rhs)))
    (ecase kind
      (:truthy   (make-ast-if :cond test :then setq    :else lhs-var))
      (:falsy    (make-ast-if :cond test :then lhs-var :else setq))
      (:non-null (make-ast-if :cond test :then lhs-var :else setq)))))

(defun %js-compound-rhs (op-str lhs-var rhs)
  "Compute the rhs value for compound assignment op like +=, -=, etc."
  (let ((plain (subseq op-str 0 (1- (length op-str))))) ; strip =
    (%js-lower-binary plain lhs-var rhs)))

;;; ─── Parameter / Argument Parsers ────────────────────────────────────────────

(defun js-parse-params (stream)
  "Parse (a, b = default, ...rest) parameter list.
Consumes opening LPAREN through closing RPAREN.
Returns (values param-syms optional-specs rest-sym new-stream) where
  param-syms    — list of all parameter symbols (positional + optional)
  optional-specs — list of (sym . default-expr) for params with defaults
  rest-sym      — symbol for rest parameter, or NIL
  new-stream    — stream after closing RPAREN."
  (multiple-value-bind (tok rest) (js-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (values nil nil nil rest2))
        (let ((params nil)
              (optionals nil)
              (rest-sym nil)
              (current rest))
          (loop
            ;; Rest parameter: ...name
            (when (eq (js-peek-type current) :T-ELLIPSIS)
              (multiple-value-bind (tok2 rest2) (js-consume current)
                (declare (ignore tok2))
                (multiple-value-bind (name-tok rest3) (js-expect :T-IDENT rest2)
                  (setf rest-sym (js-ident-sym (js-tok-value name-tok))
                        current rest3)
                  (return)))) ; rest param must be last
            ;; Normal or default parameter
            (multiple-value-bind (name-tok rest2) (js-expect :T-IDENT current)
              (let ((sym (js-ident-sym (js-tok-value name-tok))))
                (push sym params)
                (setf current rest2)
                ;; Default value?
                (when (and (eq (js-peek-type current) :T-OP)
                           (string= (js-peek-value current) "="))
                  (multiple-value-bind (eq-tok rest3) (js-consume current)
                    (declare (ignore eq-tok))
                    (multiple-value-bind (default-expr rest4)
                        (js-parse-assignment-expr rest3)
                      (push (cons sym default-expr) optionals)
                      (setf current rest4))))))
            ;; Continue on comma, stop otherwise
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  ;; Trailing comma before )
                  (if (eq (js-peek-type rest2) :T-RPAREN)
                      (progn (setf current rest2) (return))
                      (setf current rest2)))
                (return)))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse params)
                    (nreverse optionals)
                    rest-sym
                    rest2))))))

(defun js-parse-arguments (stream)
  "Parse argument list after LPAREN. Handles spread (...expr).
Returns (values arg-list rest)."
  (multiple-value-bind (tok rest) (js-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (values nil rest2))
        (let ((args nil)
              (current rest))
          (loop
            ;; Spread argument: ...expr
            (if (eq (js-peek-type current) :T-ELLIPSIS)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                    (push (%js-call '%js-spread expr) args)
                    (setf current rest3)))
                (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
                  (push expr args)
                  (setf current rest2)))
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  ;; Trailing comma
                  (if (eq (js-peek-type rest2) :T-RPAREN)
                      (progn (setf current rest2) (return))
                      (setf current rest2)))
                (return)))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse args) rest2))))))

;;; ─── Array / Object / Function-Expr Literals ───────────────────────────────
;;; js-parse-array-literal, %js-parse-object-property, js-parse-object-literal,
;;; js-parse-function-expr, and js-parse-function-body are in
;;; parser-expr-literal.lisp (loaded after this file).

;;; parser-expr-postfix.lisp handles:
;;;   js-parse-new-expr, js-parse-member-expr, %js-place-get-prop-p,
;;;   %js-lower-place-incdec, js-parse-postfix, template-literal parsers,
;;;   js-parse-unary, %js-template-parts-and-rest, %js-parse-tagged-template,
;;;   %js-parse-template-literal

