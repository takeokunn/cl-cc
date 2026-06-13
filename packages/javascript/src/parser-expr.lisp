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

;;; ─── Array / Object Literals ─────────────────────────────────────────────────

(defun js-parse-array-literal (stream)
  "Parse [...] array literal. Returns (values ast rest).
Handles elision (holes), spread elements, and assignment expressions."
  (multiple-value-bind (tok rest) (js-expect :T-LBRACKET stream)
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RBRACKET)
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (values (%js-call '%js-make-array) rest2))
        (let ((elements nil)
              (current rest))
          (loop
            (when (eq (js-peek-type current) :T-RBRACKET)
              (return))
            (cond
              ;; Elision hole: consecutive comma or leading comma
              ((eq (js-peek-type current) :T-COMMA)
               (push (make-ast-quote :value :js-hole) elements)
               (multiple-value-bind (tok2 rest2) (js-consume current)
                 (declare (ignore tok2))
                 (setf current rest2)))
              ;; Spread element: ...expr
              ((eq (js-peek-type current) :T-ELLIPSIS)
               (multiple-value-bind (tok2 rest2) (js-consume current)
                 (declare (ignore tok2))
                 (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                   (push (%js-call '%js-spread expr) elements)
                   (setf current rest3)
                   ;; Consume trailing comma if present
                   (when (eq (js-peek-type current) :T-COMMA)
                     (multiple-value-bind (tok3 rest4) (js-consume current)
                       (declare (ignore tok3))
                       (setf current rest4))))))
              ;; Regular element
              (t
               (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
                 (push expr elements)
                 (setf current rest2)
                 (cond
                   ((eq (js-peek-type current) :T-COMMA)
                    (multiple-value-bind (tok2 rest2b) (js-consume current)
                      (declare (ignore tok2))
                      (setf current rest2b)))
                   (t (return)))))))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RBRACKET current)
            (declare (ignore tok2))
            (let ((elems (nreverse elements)))
              (values (if (%js-items-have-spread-p elems)
                          ;; [...a, x] -> (apply #'%js-make-array (append a (list x)))
                          (make-ast-apply :func '%js-make-array
                                          :args (list (%js-spread-list-expr elems)))
                          (make-ast-call :func (make-ast-var :name '%js-make-array)
                                         :args elems))
                      rest2)))))))

(defun %js-parse-object-property (stream)
  "Parse one property in an object literal.
Returns (values key-expr value-expr method-p computed-p rest)."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Spread: ...expr
      ((eq type :T-ELLIPSIS)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
           (values :spread expr nil nil rest2))))
      ;; Computed property: [expr]: value
      ((eq type :T-LBRACKET)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (key-expr rest2) (js-parse-assignment-expr rest)
           (multiple-value-bind (tok2 rest3) (js-expect :T-RBRACKET rest2)
             (declare (ignore tok2))
             (cond
               ;; Method shorthand: [expr](...) { }
               ((eq (js-peek-type rest3) :T-LPAREN)
                (multiple-value-bind (fn-ast rest4)
                    (js-parse-function-expr rest3 :name nil)
                  (values key-expr fn-ast t t rest4)))
               (t
                (multiple-value-bind (tok3 rest4) (js-expect :T-COLON rest3)
                  (declare (ignore tok3))
                  (multiple-value-bind (val-expr rest5) (js-parse-assignment-expr rest4)
                    (values key-expr val-expr nil t rest5)))))))))
      ;; Generator method: * name(...) { }
      ((and (eq type :T-OP) (string= val "*"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (let ((key-str (js-peek-value rest)))
           (multiple-value-bind (key-tok rest2) (js-consume rest)
             (declare (ignore key-tok))
             (multiple-value-bind (fn-ast rest3)
                 (js-parse-function-expr rest2 :generator-p t :name (js-ident-sym key-str))
               (values (make-ast-quote :value key-str) fn-ast t nil rest3))))))
      ;; async method: async name(...) { }
      ((eq type :T-ASYNC)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (let ((next-type (js-peek-type rest))
               (next-val  (js-peek-value rest)))
           (cond
             ;; async * name — async generator
             ((and (eq next-type :T-OP) (string= next-val "*"))
              (multiple-value-bind (tok2 rest2) (js-consume rest)
                (declare (ignore tok2))
                (let ((key-str (js-peek-value rest2)))
                  (multiple-value-bind (key-tok rest3) (js-consume rest2)
                    (declare (ignore key-tok))
                    (multiple-value-bind (fn-ast rest4)
                        (js-parse-function-expr rest3 :async-p t :generator-p t
                                                      :name (js-ident-sym key-str))
                      (values (make-ast-quote :value key-str) fn-ast t nil rest4))))))
             ;; async name (not followed by = or , — it's a shorthand method)
             ((or (eq next-type :T-IDENT)
                  (eq next-type :T-STRING)
                  (eq next-type :T-NUMBER))
              (let ((key-str next-val))
                (multiple-value-bind (key-tok rest2) (js-consume rest)
                  (declare (ignore key-tok))
                  (multiple-value-bind (fn-ast rest3)
                      (js-parse-function-expr rest2 :async-p t :name (js-ident-sym key-str))
                    (values (make-ast-quote :value key-str) fn-ast t nil rest3)))))
             ;; async used as shorthand property name: { async }
             (t
              (let ((key-sym (js-ident-sym "async")))
                (cond
                  ((eq (js-peek-type rest) :T-COLON)
                   (multiple-value-bind (tok2 rest2) (js-consume rest)
                     (declare (ignore tok2))
                     (multiple-value-bind (val-expr rest3) (js-parse-assignment-expr rest2)
                       (values (make-ast-quote :value "async") val-expr nil nil rest3))))
                  (t
                   (values (make-ast-quote :value "async")
                           (make-ast-var :name key-sym)
                           nil nil rest)))))))))
      ;; get/set accessor: get name() { } / set name(x) { }
      ((and (or (eq type :T-GET) (eq type :T-SET))
            ;; Distinguish from shorthand {get} or {get, ...}
            (let ((next (js-peek-type (cdr stream))))
              (and (not (eq next :T-COMMA))
                   (not (eq next :T-RBRACE))
                   (not (eq next :T-COLON))
                   (not (eq next :T-EOF)))))
       (let ((accessor-kind (js-tok-value (car stream))))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (let ((key-str (js-peek-value rest)))
             (multiple-value-bind (key-tok rest2) (js-consume rest)
               (declare (ignore key-tok))
               (multiple-value-bind (fn-ast rest3)
                   (js-parse-function-expr rest2 :name (js-ident-sym key-str))
                 (let ((tagged-fn (%js-call '%js-accessor
                                            (make-ast-quote :value accessor-kind)
                                            fn-ast)))
                   (values (make-ast-quote :value key-str) tagged-fn t nil rest3))))))))
      ;; Identifier shorthand, method, or key: value
      (t
       ;; Consume the key
       (let ((key-str val))
         (multiple-value-bind (key-tok rest) (js-consume stream)
           (declare (ignore key-tok))
           (let ((next-type (js-peek-type rest)))
             (cond
               ;; Method shorthand: name(...) { }
               ((eq next-type :T-LPAREN)
                (multiple-value-bind (fn-ast rest2)
                    (js-parse-function-expr rest :name (js-ident-sym key-str))
                  (values (make-ast-quote :value key-str) fn-ast t nil rest2)))
               ;; Key : value
               ((eq next-type :T-COLON)
                (multiple-value-bind (tok2 rest2) (js-consume rest)
                  (declare (ignore tok2))
                  (multiple-value-bind (val-expr rest3) (js-parse-assignment-expr rest2)
                    (values (make-ast-quote :value key-str) val-expr nil nil rest3))))
               ;; Shorthand: { name } — same as { name: name }
               (t
                (let ((sym (js-ident-sym key-str)))
                  (values (make-ast-quote :value key-str)
                          (make-ast-var :name sym)
                          nil nil rest)))))))))))

(defun js-parse-object-literal (stream)
  "Parse {...} object literal. Returns (values ast rest).
Handles shorthand properties, method shorthands, computed keys,
spread elements, getters, and setters."
  (multiple-value-bind (tok rest) (js-expect :T-LBRACE stream)
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RBRACE)
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (values (%js-call '%js-make-object) rest2))
        (let ((entries nil)        ; ordered: (:spread expr) | (:pair key val)
              (has-spread nil)
              (current rest))
          (loop
            (when (eq (js-peek-type current) :T-RBRACE)
              (return))
            (multiple-value-bind (key val method-p computed-p rest2)
                (%js-parse-object-property current)
              (declare (ignore method-p computed-p))
              (if (eq key :spread)
                  (progn (push (list :spread val) entries) (setf has-spread t))
                  (push (list :pair key val) entries))
              (setf current rest2))
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  (setf current rest2))
                (return)))
          (setf entries (nreverse entries))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RBRACE current)
            (declare (ignore tok2))
            (values
             (if has-spread
                 ;; {...a, k:v, ...b}: merge semantics. Fold entries left-to-right,
                 ;; threading the object: a spread copies the source's own props
                 ;; (%js-object-assign), a pair sets one key (%js-object-spread-set,
                 ;; which returns the object). Later entries override earlier keys.
                 (let ((acc (%js-call '%js-make-object)))
                   (dolist (e entries acc)
                     (setf acc (if (eq (first e) :spread)
                                   (%js-call '%js-object-assign acc (second e))
                                   (%js-call '%js-object-spread-set acc
                                             (second e) (third e))))))
                 (make-ast-call
                  :func (make-ast-var :name '%js-make-object)
                  :args (loop for e in entries
                              append (list (second e) (third e)))))
             rest2))))))

;;; ─── Function Expression Parser ──────────────────────────────────────────────

(defun js-parse-function-expr (stream &key async-p generator-p name)
  "Parse function [*] [name] (params) { body } as expression.
ASYNC-P and GENERATOR-P are boolean flags from the calling context.
NAME is an optional symbol for the function name.
Returns (values ast rest)."
  (let ((current stream)
        (is-generator generator-p))
    ;; Consume optional * for generators
    (when (and (eq (js-peek-type current) :T-OP)
               (string= (js-peek-value current) "*"))
      (multiple-value-bind (tok rest) (js-consume current)
        (declare (ignore tok))
        (setf is-generator t
              current rest)))
    ;; Optional function name
    (when (and (null name) (eq (js-peek-type current) :T-IDENT))
      (multiple-value-bind (name-tok rest) (js-consume current)
        (setf name (js-ident-sym (js-tok-value name-tok))
              current rest)))
    ;; Parameter list
    (multiple-value-bind (params optionals rest-sym rest2)
        (js-parse-params current)
      ;; Body: { stmts... }
      (multiple-value-bind (tok3 rest3) (js-expect :T-LBRACE rest2)
        (declare (ignore tok3))
        (multiple-value-bind (body-forms rest4)
            (js-parse-function-body rest3)
          (multiple-value-bind (required opts)
              (%js-split-params-by-defaults params optionals)
          ;; Wrap in (block nil ...) so `return' (which lowers to return-from nil)
          ;; works — a function-expression body was previously used raw, so any
          ;; `return value' silently produced nothing.
          (multiple-value-bind (rest-param wrapped-body)
              (%js-rest-binding rest-sym (%js-callable-body body-forms))
          (let ((lambda-ast (make-ast-lambda :params required
                                             :optional-params opts
                                             :rest-param rest-param
                                             :body wrapped-body)))
            ;; Wrap async/generator expressions.
            ;; Generator expressions use %js-wrap-generator-body which returns a
            ;; CALLABLE that creates a fresh generator on each invocation — unlike
            ;; %js-make-generator which runs the body eagerly with no arguments.
            (let ((result (cond
                            ((and async-p is-generator)
                             (%js-call '%js-make-async-generator lambda-ast))
                            (async-p
                             (%js-call '%js-make-async lambda-ast))
                            (is-generator
                             (%js-call '%js-wrap-generator-body lambda-ast))
                            (t lambda-ast))))
              ;; Named function expression — the name is visible INSIDE the body
              ;; (for self-recursion) but not outside.  A plain (let ((f lambda)) f)
              ;; binds in PARALLEL, so `f` inside the lambda body resolves to the
              ;; enclosing scope (undefined → "Undefined function") rather than the
              ;; function itself.  Bind the name to nil first, then assign the
              ;; lambda: the name is now mutated AND captured by the lambda, so the
              ;; compiler boxes it and the closure reads the assigned value —
              ;; letrec semantics, so `function fac(n){...fac(n-1)...}` recurses.
              (values (if name
                          (make-ast-let
                           :bindings (list (cons name (make-ast-quote :value nil)))
                           :body (list (make-ast-setq :var name :value result)
                                       (make-ast-var :name name)))
                          result)
                      rest4))))))))))

(defun js-parse-function-body (stream)
  "Parse statements inside { } until matching }."
  (funcall #'js-parse-stmt-list stream))

;;; parser-expr-postfix.lisp handles:
;;;   js-parse-new-expr, js-parse-member-expr, %js-place-get-prop-p,
;;;   %js-lower-place-incdec, js-parse-postfix, template-literal parsers,
;;;   js-parse-unary, %js-template-parts-and-rest, %js-parse-tagged-template,
;;;   %js-parse-template-literal

