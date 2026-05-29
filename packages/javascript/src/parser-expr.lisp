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

(defun js-infix-prec (stream)
  "Return (values prec right-assoc-p) for current token as infix op, or (values 0 nil).

Precedence levels:
  1  comma
  2  assignment (right-assoc): = += -= etc.
  4  ternary ?:
  5  nullish ??
  6  logical-or ||
  7  logical-and &&
  8  bitwise-or |
  9  bitwise-xor ^
  10 bitwise-and &
  11 equality == != === !==
  12 relational < > <= >= instanceof in
  13 shift << >> >>>
  14 additive + -
  15 multiplicative * / %
  16 exponentiation ** (right-assoc)
  19 member . ?. [ ( (postfix)"
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Comma — lowest
      ((eq type :T-COMMA)
       (values 1 nil))
      ;; Assignment operators (right-associative)
      ((and (eq type :T-OP)
            (member val '("=" "+=" "-=" "*=" "/=" "%=" "**="
                          "<<=" ">>=" ">>>=" "&=" "|=" "^="
                          "&&=" "||=" "??=")
                    :test #'string=))
       (values 2 t))
      ;; Ternary ? (handled specially in js-parse-expr, but give it a prec)
      ((eq type :T-QUESTION)
       (values 4 nil))
      ;; Nullish coalescing ??
      ((and (eq type :T-OP) (string= val "??"))
       (values 5 nil))
      ;; Logical OR ||
      ((and (eq type :T-OP) (string= val "||"))
       (values 6 nil))
      ;; Logical AND &&
      ((and (eq type :T-OP) (string= val "&&"))
       (values 7 nil))
      ;; Bitwise OR |
      ((and (eq type :T-OP) (string= val "|"))
       (values 8 nil))
      ;; Bitwise XOR ^
      ((and (eq type :T-OP) (string= val "^"))
       (values 9 nil))
      ;; Bitwise AND &
      ((and (eq type :T-OP) (string= val "&"))
       (values 10 nil))
      ;; Equality
      ((and (eq type :T-OP)
            (member val '("==" "!=" "===" "!==") :test #'string=))
       (values 11 nil))
      ;; Relational / in / instanceof
      ((or (and (eq type :T-OP)
                (member val '("<" ">" "<=" ">=") :test #'string=))
           (eq type :T-INSTANCEOF)
           (eq type :T-IN))
       (values 12 nil))
      ;; Shift
      ((and (eq type :T-OP)
            (member val '("<<" ">>" ">>>") :test #'string=))
       (values 13 nil))
      ;; Additive
      ((and (eq type :T-OP) (member val '("+" "-") :test #'string=))
       (values 14 nil))
      ;; Multiplicative
      ((and (eq type :T-OP) (member val '("*" "/" "%") :test #'string=))
       (values 15 nil))
      ;; Exponentiation (right-associative)
      ((and (eq type :T-OP) (string= val "**"))
       (values 16 t))
      ;; Member access / call (postfix — handled in js-parse-postfix)
      ((or (eq type :T-DOT)
           (eq type :T-LBRACKET)
           (eq type :T-LPAREN)
           (and (eq type :T-OP) (string= val "?.")))
       (values 19 nil))
      ;; Not an infix operator
      (t
       (values 0 nil)))))

;;; ─── Operator Lowering ───────────────────────────────────────────────────────

(defun js-lower-binop-keyword (op-str)
  "Map operator string to AST binop keyword or nil for runtime-dispatch ops."
  (cond ((string= op-str "+")   :+)
        ((string= op-str "-")   :-)
        ((string= op-str "*")   :*)
        ((string= op-str "/")   :/)
        ((string= op-str "%")   :%)
        ((string= op-str "**")  :**)
        ((string= op-str "<")   :<)
        ((string= op-str ">")   :>)
        ((string= op-str "<=")  :<=)
        ((string= op-str ">=")  :>=)
        (t nil)))

(defun %js-call (name &rest args)
  "Build an AST call to a JS runtime helper NAME with ARGS."
  (make-ast-call :func (make-ast-var :name name)
                 :args args))

(defun %js-lower-binary (op-str lhs rhs)
  "Lower a binary operator string + lhs + rhs to the appropriate AST."
  (let ((kw (js-lower-binop-keyword op-str)))
    (cond
      ;; Direct ast-binop for arithmetic/comparison ops
      (kw
       (make-ast-binop :op kw :lhs lhs :rhs rhs))
      ;; Strict equality ===
      ((string= op-str "===")
       (%js-call '%js-strict-eq lhs rhs))
      ;; Loose equality ==
      ((string= op-str "==")
       (%js-call '%js-loose-eq lhs rhs))
      ;; Strict inequality !==
      ((string= op-str "!==")
       (make-ast-call :func (make-ast-var :name 'not)
                      :args (list (%js-call '%js-strict-eq lhs rhs))))
      ;; Loose inequality !=
      ((string= op-str "!=")
       (make-ast-call :func (make-ast-var :name 'not)
                      :args (list (%js-call '%js-loose-eq lhs rhs))))
      ;; Nullish coalescing ??
      ((string= op-str "??")
       (%js-lower-nullish-coalesce lhs rhs))
      ;; Logical OR
      ((string= op-str "||")
       (make-ast-binop :op :or :lhs lhs :rhs rhs))
      ;; Logical AND
      ((string= op-str "&&")
       (make-ast-binop :op :and :lhs lhs :rhs rhs))
      ;; Bitwise ops → runtime helpers
      ((string= op-str "|")
       (%js-call '%js-bitwise-or lhs rhs))
      ((string= op-str "^")
       (%js-call '%js-bitwise-xor lhs rhs))
      ((string= op-str "&")
       (%js-call '%js-bitwise-and lhs rhs))
      ((string= op-str "<<")
       (%js-call '%js-shift-left lhs rhs))
      ((string= op-str ">>")
       (%js-call '%js-shift-right lhs rhs))
      ((string= op-str ">>>")
       (%js-call '%js-unsigned-shift-right lhs rhs))
      ;; instanceof / in
      ((string= op-str "instanceof")
       (%js-call '%js-instanceof lhs rhs))
      ((string= op-str "in")
       (%js-call '%js-in lhs rhs))
      ;; Fallback: call a runtime dispatch helper
      (t
       (%js-call '%js-binop (make-ast-quote :value (intern op-str :keyword))
                 lhs rhs)))))

(defun %js-lower-nullish-coalesce (lhs rhs)
  "Lower LHS ?? RHS without evaluating LHS twice."
  (let ((tmp (gensym "JS-NC-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%js-call '%js-not-nullish (make-ast-var :name tmp))
                  :then (make-ast-var :name tmp)
                  :else rhs)))))

(defun %js-lower-logical-assign (op-str lhs-sym rhs)
  "Lower &&= ||= ??= compound logical assignment on a variable."
  (let ((lhs-var (make-ast-var :name lhs-sym)))
    (cond
      ((string= op-str "&&=")
       (make-ast-if
        :cond (%js-call '%js-truthy lhs-var)
        :then (make-ast-setq :var lhs-sym :value rhs)
        :else lhs-var))
      ((string= op-str "||=")
       (make-ast-if
        :cond (%js-call '%js-truthy lhs-var)
        :then lhs-var
        :else (make-ast-setq :var lhs-sym :value rhs)))
      ((string= op-str "??=")
       (make-ast-if
        :cond (%js-call '%js-not-nullish lhs-var)
        :then lhs-var
        :else (make-ast-setq :var lhs-sym :value rhs)))
      (t (error "JS parse error: unknown logical assign op ~S" op-str)))))

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
            (values (make-ast-call :func (make-ast-var :name '%js-make-array)
                                   :args (nreverse elements))
                    rest2))))))

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
        (let ((pairs nil)
              (current rest))
          (loop
            (when (eq (js-peek-type current) :T-RBRACE)
              (return))
            (multiple-value-bind (key val method-p computed-p rest2)
                (%js-parse-object-property current)
              (declare (ignore method-p computed-p))
              (cond
                ;; Spread entry
                ((eq key :spread)
                 (push (%js-call '%js-spread val) pairs))
                (t
                 (push key pairs)
                 (push val pairs)))
              (setf current rest2))
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  (setf current rest2))
                (return)))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RBRACE current)
            (declare (ignore tok2))
            (values (make-ast-call :func (make-ast-var :name '%js-make-object)
                                   :args (nreverse pairs))
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
      (declare (ignore optionals rest-sym))
      ;; Body: { stmts... }
      (multiple-value-bind (tok3 rest3) (js-expect :T-LBRACE rest2)
        (declare (ignore tok3))
        (multiple-value-bind (body-forms rest4)
            (js-parse-function-body rest3)
          (let ((lambda-ast (make-ast-lambda :params params
                                             :body body-forms)))
            ;; Wrap async/generator in metadata call if needed
            (let ((result (cond
                            ((and async-p is-generator)
                             (%js-call '%js-make-async-generator lambda-ast))
                            (async-p
                             (%js-call '%js-make-async lambda-ast))
                            (is-generator
                             (%js-call '%js-make-generator lambda-ast))
                            (t lambda-ast))))
              ;; Named function — wrap in let for self-reference
              (values (if name
                          (make-ast-let
                           :bindings (list (cons name result))
                           :body (list (make-ast-var :name name)))
                          result)
                      rest4))))))))

(defun js-parse-function-body (stream)
  "Parse statements inside { } until matching }. Forward declaration.
Returns (values body-form-list rest-after-rbrace)."
  ;; Forward reference — js-parse-stmt-list is defined in parser-stmt.lisp.
  ;; We call it via symbol-function to avoid a load-order compile-time dependency.
  (if (fboundp 'js-parse-stmt-list)
      (funcall #'js-parse-stmt-list stream)
      ;; Fallback: consume until matching }
      (%js-consume-until-rbrace stream)))

(defun %js-consume-until-rbrace (stream)
  "Consume tokens until matching } — fallback when statement parser is not loaded."
  (let ((forms nil)
        (depth 1)
        (current stream))
    (loop
      (when (js-at-eof-p current)
        (error "JS parse error: unexpected EOF inside function body"))
      (let ((type (js-peek-type current)))
        (cond
          ((eq type :T-LBRACE) (incf depth) (setf current (cdr current)))
          ((eq type :T-RBRACE)
           (decf depth)
           (if (zerop depth)
               (progn (setf current (cdr current)) (return))
               (setf current (cdr current))))
          (t
           (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
             (push expr forms)
             (setf current rest2))))))
    (values (nreverse forms) current)))

;;; ─── New Expression ──────────────────────────────────────────────────────────

(defun js-parse-new-expr (stream)
  "Parse new ClassName(args) or new.target. Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'new'
    (declare (ignore tok))
    ;; new.target
    (when (and (eq (js-peek-type rest) :T-DOT)
               (eq (js-peek-type (cdr rest)) :T-TARGET))
      (multiple-value-bind (dot-tok rest2) (js-consume rest)
        (declare (ignore dot-tok))
        (multiple-value-bind (target-tok rest3) (js-consume rest2)
          (declare (ignore target-tok))
          (return-from js-parse-new-expr
            (values (%js-call '%js-new-target) rest3)))))
    ;; new expr
    (multiple-value-bind (ctor-ast rest2) (js-parse-member-expr rest)
      ;; Optional argument list
      (if (eq (js-peek-type rest2) :T-LPAREN)
          (multiple-value-bind (args rest3) (js-parse-arguments rest2)
            (values (%js-call '%js-new ctor-ast
                              (make-ast-call :func (make-ast-var :name '%js-make-array)
                                             :args args))
                    rest3))
          (values (%js-call '%js-new ctor-ast
                            (%js-call '%js-make-array))
                  rest2)))))

(defun js-parse-member-expr (stream)
  "Parse a member expression for 'new' context (no call, only . and []).
Returns (values ast rest)."
  (multiple-value-bind (ast rest) (js-parse-primary stream)
    ;; Apply member accesses only (no calls — that would be CallExpression)
    (loop
      (let ((type (js-peek-type rest))
            (val  (js-peek-value rest)))
        (cond
          ;; obj.prop
          ((eq type :T-DOT)
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (prop-tok rest3) (js-consume rest2)
               (let* ((prop-str (js-tok-value prop-tok))
                      (key (make-ast-quote :value prop-str)))
                 (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                          :args (list ast key))
                       rest rest3)))))
          ;; obj[expr]
          ((eq type :T-LBRACKET)
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (idx-ast rest3) (js-parse-assignment-expr rest2)
               (multiple-value-bind (tok2 rest4) (js-expect :T-RBRACKET rest3)
                 (declare (ignore tok2))
                 (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                          :args (list ast idx-ast))
                       rest rest4)))))
          ;; optional chain ?.
          ((and (eq type :T-OP) (string= val "?."))
           (multiple-value-bind (tok rest2) (js-consume rest)
             (declare (ignore tok))
             (cond
               ;; ?.prop
               ((eq (js-peek-type rest2) :T-IDENT)
                (multiple-value-bind (prop-tok rest3) (js-consume rest2)
                  (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                    (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                             :args (list ast key))
                          rest rest3))))
               ;; ?.[expr]
               ((eq (js-peek-type rest2) :T-LBRACKET)
                (multiple-value-bind (tok2 rest3) (js-consume rest2)
                  (declare (ignore tok2))
                  (multiple-value-bind (idx-ast rest4) (js-parse-assignment-expr rest3)
                    (multiple-value-bind (tok3 rest5) (js-expect :T-RBRACKET rest4)
                      (declare (ignore tok3))
                      (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                               :args (list ast idx-ast))
                            rest rest5)))))
               (t
                (setf rest rest2)
                (return)))))
          (t (return)))))
    (values ast rest)))

;;; ─── Postfix ─────────────────────────────────────────────────────────────────

(defun js-parse-postfix (ast stream)
  "Apply postfix operations: ++ -- . [] () ?. to AST.
Returns (values ast rest). Loops until no more postfix ops."
  (loop
    (let ((type (js-peek-type stream))
          (val  (js-peek-value stream)))
      (cond
        ;; Postfix ++
        ((and (eq type :T-OP) (string= val "++"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           ;; Return current value, then increment (lower as: (let ((t ast)) (setq ast (+ t 1)) t))
           (when (ast-var-p ast)
             (let* ((var-sym (ast-var-name ast))
                    (tmp (gensym "JS-POSTFIX-")))
               (setf ast (make-ast-let
                          :bindings (list (cons tmp (make-ast-var :name var-sym)))
                          :body (list (make-ast-setq
                                       :var var-sym
                                       :value (make-ast-binop :op :+
                                                              :lhs (make-ast-var :name var-sym)
                                                              :rhs (make-ast-int :value 1)))
                                      (make-ast-var :name tmp)))
                     stream rest)))
           (unless (ast-var-p ast) ; non-variable — just wrap
             (setf ast (%js-call '%js-postfix-inc ast)
                   stream rest))))
        ;; Postfix --
        ((and (eq type :T-OP) (string= val "--"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (when (ast-var-p ast)
             (let* ((var-sym (ast-var-name ast))
                    (tmp (gensym "JS-POSTFIX-")))
               (setf ast (make-ast-let
                          :bindings (list (cons tmp (make-ast-var :name var-sym)))
                          :body (list (make-ast-setq
                                       :var var-sym
                                       :value (make-ast-binop :op :-
                                                              :lhs (make-ast-var :name var-sym)
                                                              :rhs (make-ast-int :value 1)))
                                      (make-ast-var :name tmp)))
                     stream rest)))
           (unless (ast-var-p ast)
             (setf ast (%js-call '%js-postfix-dec ast)
                   stream rest))))
        ;; Property access: obj.prop
        ((eq type :T-DOT)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           ;; private field #name
           (if (eq (js-peek-type rest) :T-PRIVATE-IDENT)
               (multiple-value-bind (prop-tok rest2) (js-consume rest)
                 (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                   (setf ast (make-ast-call :func (make-ast-var :name '%js-class-private-field-get)
                                            :args (list ast key))
                         stream rest2)))
               (multiple-value-bind (prop-tok rest2) (js-consume rest)
                 (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                   (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                            :args (list ast key))
                         stream rest2))))))
        ;; Computed member: obj[expr]
        ((eq type :T-LBRACKET)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (idx-ast rest2) (js-parse-assignment-expr rest)
             (multiple-value-bind (tok2 rest3) (js-expect :T-RBRACKET rest2)
               (declare (ignore tok2))
               (setf ast (make-ast-call :func (make-ast-var :name '%js-get-prop)
                                        :args (list ast idx-ast))
                     stream rest3)))))
        ;; Call: fn(args)
        ((eq type :T-LPAREN)
         (multiple-value-bind (args rest) (js-parse-arguments stream)
           (setf ast (make-ast-call :func ast :args args)
                 stream rest)))
        ;; Optional chain ?.
        ((and (eq type :T-OP) (string= val "?."))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (cond
             ;; ?.prop
             ((eq (js-peek-type rest) :T-IDENT)
              (multiple-value-bind (prop-tok rest2) (js-consume rest)
                (let ((key (make-ast-quote :value (js-tok-value prop-tok))))
                  (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                           :args (list ast key))
                        stream rest2))))
             ;; ?.[expr]
             ((eq (js-peek-type rest) :T-LBRACKET)
              (multiple-value-bind (tok2 rest2) (js-consume rest)
                (declare (ignore tok2))
                (multiple-value-bind (idx-ast rest3) (js-parse-assignment-expr rest2)
                  (multiple-value-bind (tok3 rest4) (js-expect :T-RBRACKET rest3)
                    (declare (ignore tok3))
                    (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-chain)
                                             :args (list ast idx-ast))
                          stream rest4)))))
             ;; ?.(args)
             ((eq (js-peek-type rest) :T-LPAREN)
              (multiple-value-bind (args rest2) (js-parse-arguments rest)
                (setf ast (make-ast-call :func (make-ast-var :name '%js-optional-call)
                                         :args (cons ast args))
                      stream rest2)))
             (t
              (setf stream rest)
              (return)))))
        ;; Tagged template literal: fn`...`
        ((eq type :T-TEMPLATE-START)
         (multiple-value-bind (template-ast rest) (%js-parse-template-literal stream)
           (setf ast (make-ast-call :func ast :args (list template-ast))
                 stream rest)))
        (t (return)))))
  (values ast stream))

;;; ─── Template Literal ────────────────────────────────────────────────────────

(defun %js-parse-template-literal (stream)
  "Parse a template literal starting at :T-TEMPLATE-START or :T-STRING.
Returns (values ast rest)."
  (let ((tok (js-peek stream)))
    (cond
      ;; Simple string template (no interpolation): lexer emitted :T-STRING
      ((eq (js-tok-type tok) :T-STRING)
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value str-tok)) rest)))
      ;; Template with parts: :T-TEMPLATE-PARTS (value = list of strings and (:template-expr tokens))
      ((eq (js-tok-type tok) :T-TEMPLATE-PARTS)
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (let ((parts (js-tok-value parts-tok))
               (segments nil))
           (dolist (part parts)
             (cond
               ((stringp part)
                (push (make-ast-quote :value part) segments))
               ((and (consp part) (eq (car part) :template-expr))
                ;; The inner tokens form an expression — parse it
                (let ((inner-tokens (cadr part)))
                  (multiple-value-bind (expr _rest)
                      (js-parse-assignment-expr inner-tokens)
                    (declare (ignore _rest))
                    (push (%js-call '%js-to-string expr) segments))))
               (t
                (push (make-ast-quote :value (format nil "~A" part)) segments))))
           ;; Build concat chain
           (let ((parts-list (nreverse segments)))
             (values (if (null (cdr parts-list))
                         (car parts-list)
                         (reduce (lambda (l r) (%js-call '%js-concat l r))
                                 parts-list))
                     rest)))))
      ;; :T-TEMPLATE-START — consumed by the template lexer inline
      ((eq (js-tok-type tok) :T-TEMPLATE-START)
       (multiple-value-bind (tok2 rest) (js-consume stream)
         (declare (ignore tok2))
         ;; After T-TEMPLATE-START, expect T-STRING or T-TEMPLATE-PARTS
         (%js-parse-template-literal rest)))
      (t
       (error "JS parse error: expected template literal, got ~S" tok)))))

;;; ─── Unary ───────────────────────────────────────────────────────────────────

(defun js-parse-unary (stream)
  "Parse prefix unary: ! ~ + - ++ -- typeof void delete await yield.
Returns (values ast rest)."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Prefix ++
      ((and (eq type :T-OP) (string= val "++"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (if (ast-var-p expr)
               (let ((var-sym (ast-var-name expr)))
                 (values (make-ast-setq :var var-sym
                                        :value (make-ast-binop :op :+
                                                               :lhs expr
                                                               :rhs (make-ast-int :value 1)))
                         rest2))
               (values (%js-call '%js-prefix-inc expr) rest2)))))
      ;; Prefix --
      ((and (eq type :T-OP) (string= val "--"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (if (ast-var-p expr)
               (let ((var-sym (ast-var-name expr)))
                 (values (make-ast-setq :var var-sym
                                        :value (make-ast-binop :op :-
                                                               :lhs expr
                                                               :rhs (make-ast-int :value 1)))
                         rest2))
               (values (%js-call '%js-prefix-dec expr) rest2)))))
      ;; Logical NOT !
      ((and (eq type :T-OP) (string= val "!"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (make-ast-call :func (make-ast-var :name 'not)
                                  :args (list (%js-call '%js-truthy expr)))
                   rest2))))
      ;; Bitwise NOT ~
      ((and (eq type :T-OP) (string= val "~"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-bitwise-not expr) rest2))))
      ;; Unary + (numeric coercion)
      ((and (eq type :T-OP) (string= val "+"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-unary-plus expr) rest2))))
      ;; Unary -
      ((and (eq type :T-OP) (string= val "-"))
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           ;; Constant folding for numeric literals
           (if (ast-int-p expr)
               (values (make-ast-int :value (- (ast-int-value expr))) rest2)
               (values (make-ast-call :func (make-ast-var :name '-)
                                      :args (list expr))
                       rest2)))))
      ;; typeof
      ((eq type :T-TYPEOF)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-typeof expr) rest2))))
      ;; void
      ((eq type :T-VOID)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (make-ast-progn :forms (list expr (make-ast-quote :value :undefined)))
                   rest2))))
      ;; delete
      ((eq type :T-DELETE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-delete expr) rest2))))
      ;; await
      ((eq type :T-AWAIT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-await expr) rest2))))
      ;; yield (prefix usage)
      ((eq type :T-YIELD)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         ;; yield* iterable
         (if (and (eq (js-peek-type rest) :T-OP) (string= (js-peek-value rest) "*"))
             (multiple-value-bind (tok2 rest2) (js-consume rest)
               (declare (ignore tok2))
               (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                 (values (%js-call '%js-yield-from expr) rest3)))
             ;; yield expr or bare yield
             (if (or (js-at-eof-p rest)
                     (member (js-peek-type rest)
                             '(:T-SEMI :T-COMMA :T-RBRACE :T-RPAREN :T-RBRACKET) :test #'eq))
                 (values (%js-call '%js-yield) rest)
                 (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                   (values (%js-call '%js-yield expr) rest2))))))
      ;; Fall through to postfix
      (t
       (multiple-value-bind (ast rest) (js-parse-primary stream)
         (js-parse-postfix ast rest))))))

;;; ─── Primary Expression ──────────────────────────────────────────────────────

(defun js-parse-primary (stream)
  "Parse a primary expression. Returns (values ast rest).
Handles: numbers, strings, booleans, null, undefined, this, super,
identifiers, parenthesized expressions, array literals [...],
object literals {...}, function expressions, async functions,
generator functions, class expressions, new expr, template literals,
yield, await, import()."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    (cond
      ;; Numeric literal (integer or float)
      ((eq type :T-NUMBER)
       (multiple-value-bind (tok rest) (js-consume stream)
         (let ((v (js-tok-value tok)))
           (if (integerp v)
               (values (make-ast-int :value v) rest)
               (values (make-ast-quote :value v) rest)))))
      ;; BigInt literal
      ((eq type :T-BIGINT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-int :value (js-tok-value tok)) rest)))
      ;; String literal
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value tok)) rest)))
      ;; Regex literal
      ((eq type :T-REGEX)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (%js-call '%js-make-regex
                           (make-ast-quote :value (js-tok-value tok)))
                 rest)))
      ;; Template literal
      ((or (eq type :T-TEMPLATE-START)
           (eq type :T-TEMPLATE-PARTS))
       (%js-parse-template-literal stream))
      ;; Boolean true
      ((eq type :T-TRUE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value t) rest)))
      ;; Boolean false
      ((eq type :T-FALSE)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value nil) rest)))
      ;; null literal
      ((eq type :T-NULL)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :null) rest)))
      ;; undefined
      ((eq type :T-UNDEFINED)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :undefined) rest)))
      ;; this
      ((eq type :T-THIS)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name '%js-this) rest)))
      ;; super
      ((eq type :T-SUPER)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name '%js-super) rest)))
      ;; Array literal [...]
      ((eq type :T-LBRACKET)
       (js-parse-array-literal stream))
      ;; Object literal {...}
      ((eq type :T-LBRACE)
       (js-parse-object-literal stream))
      ;; Parenthesized expression or arrow function params
      ((eq type :T-LPAREN)
       (%js-parse-paren-or-arrow stream))
      ;; function expression
      ((eq type :T-FUNCTION)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (js-parse-function-expr rest)))
      ;; async function / async arrow
      ((eq type :T-ASYNC)
       (%js-parse-async-expr stream))
      ;; class expression
      ((eq type :T-CLASS)
       (%js-parse-class-expr stream))
      ;; new expression (including new.target)
      ((eq type :T-NEW)
       (js-parse-new-expr stream))
      ;; import() dynamic import
      ((eq type :T-IMPORT)
       (%js-parse-import-expr stream))
      ;; yield as expression (when used as identifier-like)
      ((eq type :T-YIELD)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (if (and (eq (js-peek-type rest) :T-OP) (string= (js-peek-value rest) "*"))
             (multiple-value-bind (tok2 rest2) (js-consume rest)
               (declare (ignore tok2))
               (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                 (values (%js-call '%js-yield-from expr) rest3)))
             (if (or (js-at-eof-p rest)
                     (member (js-peek-type rest)
                             '(:T-SEMI :T-COMMA :T-RBRACE :T-RPAREN :T-RBRACKET) :test #'eq))
                 (values (%js-call '%js-yield) rest)
                 (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                   (values (%js-call '%js-yield expr) rest2))))))
      ;; await as expression
      ((eq type :T-AWAIT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2) (js-parse-unary rest)
           (values (%js-call '%js-await expr) rest2))))
      ;; Identifier — may begin a single-parameter arrow function: x => body
      ((eq type :T-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (if (eq (js-peek-type rest) :T-ARROW)
             (%js-finish-arrow-function (list (js-ident-sym val)) rest)
             (values (make-ast-var :name (js-ident-sym val)) rest))))
      ;; Contextual keywords used as identifiers (get, set, from, as, of, target, meta, using, static)
      ((member type '(:T-GET :T-SET :T-FROM :T-AS :T-OF
                      :T-TARGET :T-META :T-USING :T-STATIC :T-LET) :test #'eq)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-var :name (js-ident-sym val)) rest)))
      ;; Private field identifier #name — as standalone (for #name in obj)
      ((eq type :T-PRIVATE-IDENT)
       (multiple-value-bind (tok rest) (js-consume stream)
         (values (make-ast-var :name (js-ident-sym (concatenate 'string "#" val))) rest)))
      (t
       (error "JS parse error: unexpected token ~S in expression" (js-peek stream))))))

;;; ─── Parenthesized Expression / Arrow Function ───────────────────────────────

(defun %js-parse-paren-or-arrow (stream)
  "Parse ( ... ) — either a parenthesized expression or arrow function params.
Returns (values ast rest)."
  ;; We use a speculative approach: try to detect => after the closing )
  ;; by collecting the paren contents and peeking ahead.
  ;; Strategy: parse a comma-separated list of assignment-exprs or patterns,
  ;; then check for =>.
  (multiple-value-bind (tok rest) (js-consume stream) ; consume (
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-RPAREN)
        ;; () => ... (arrow with no params) — must be followed by =>
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (if (eq (js-peek-type rest2) :T-ARROW)
              (%js-finish-arrow-function nil rest2)
              ;; () as expression is a syntax error in JS but we emit nil for tolerance
              (values (make-ast-quote :value :undefined) rest2)))
        ;; Non-empty: collect expressions
        (let ((exprs nil)
              (params nil)
              (is-arrow-candidate t)
              (current rest)
              (rest-sym nil))
          ;; Collect items
          (loop
            (cond
              ;; ...rest param
              ((eq (js-peek-type current) :T-ELLIPSIS)
               (multiple-value-bind (tok2 rest2) (js-consume current)
                 (declare (ignore tok2))
                 (multiple-value-bind (name-tok rest3) (js-expect :T-IDENT rest2)
                   (let ((sym (js-ident-sym (js-tok-value name-tok))))
                     (setf rest-sym sym
                           current rest3))))
               (return)) ; rest param must be last
              (t
               ;; Try as assignment-expr
               (multiple-value-bind (expr rest2) (js-parse-assignment-expr current)
                 (push expr exprs)
                 ;; Can it be an arrow param?
                 (when (ast-var-p expr)
                   (push (ast-var-name expr) params))
                 (setf current rest2))))
            (if (eq (js-peek-type current) :T-COMMA)
                (multiple-value-bind (tok2 rest2) (js-consume current)
                  (declare (ignore tok2))
                  (if (eq (js-peek-type rest2) :T-RPAREN)
                      (progn (setf current rest2) (return))
                      (setf current rest2)))
                (return)))
          (multiple-value-bind (tok2 rest2) (js-expect :T-RPAREN current)
            (declare (ignore tok2))
            ;; Check for =>
            (if (and is-arrow-candidate (eq (js-peek-type rest2) :T-ARROW))
                (let ((arrow-params (nreverse params)))
                  (when rest-sym
                    (setf arrow-params (append arrow-params (list rest-sym))))
                  (%js-finish-arrow-function arrow-params rest2))
                ;; Parenthesized expression — return last expression (or progn)
                (let ((all-exprs (nreverse exprs)))
                  (values (if (null (cdr all-exprs))
                              (car all-exprs)
                              (make-ast-progn :forms all-exprs))
                          rest2))))))))

(defun %js-finish-arrow-function (params stream)
  "Consume => and parse arrow function body. PARAMS is list of symbols.
Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-expect :T-ARROW stream)
    (declare (ignore tok))
    (if (eq (js-peek-type rest) :T-LBRACE)
        ;; Block body: => { stmts }
        (multiple-value-bind (tok2 rest2) (js-consume rest)
          (declare (ignore tok2))
          (multiple-value-bind (body-forms rest3)
              (js-parse-function-body rest2)
            (values (make-ast-lambda :params params :body body-forms) rest3)))
        ;; Concise body: => expr
        (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
          (values (make-ast-lambda :params params
                                   :body (list (make-ast-return-from :name nil :value expr)))
                  rest2)))))

;;; ─── Async Expression ────────────────────────────────────────────────────────

(defun %js-parse-async-expr (stream)
  "Parse async function or async arrow function.
Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'async'
    (declare (ignore tok))
    (let ((next-type (js-peek-type rest))
          (next-val  (js-peek-value rest)))
      (cond
        ;; async function [*] name? (params) { body }
        ((eq next-type :T-FUNCTION)
         (multiple-value-bind (tok2 rest2) (js-consume rest)
           (declare (ignore tok2))
           (js-parse-function-expr rest2 :async-p t)))
        ;; async (params) => body  or  async ident => body
        ((eq next-type :T-LPAREN)
         ;; async arrow function: async (params) =>
         (multiple-value-bind (params optionals rest-sym rest2)
             (js-parse-params rest)
           (declare (ignore optionals rest-sym))
           (if (eq (js-peek-type rest2) :T-ARROW)
               (%js-finish-async-arrow params rest2)
               ;; Not an arrow — async followed by ( is a call: async(...)
               (multiple-value-bind (args rest3) (js-parse-arguments rest)
                 (values (make-ast-call :func (make-ast-var :name (js-ident-sym "async"))
                                        :args args)
                         rest3)))))
        ;; async ident => body (single param arrow)
        ((eq next-type :T-IDENT)
         (let ((param-sym (js-ident-sym next-val)))
           (multiple-value-bind (tok2 rest2) (js-consume rest)
             (declare (ignore tok2))
             (if (eq (js-peek-type rest2) :T-ARROW)
                 (%js-finish-async-arrow (list param-sym) rest2)
                 ;; async used as property / identifier
                 (multiple-value-bind (ast rest3)
                     (js-parse-postfix (make-ast-var :name (js-ident-sym "async")) rest)
                   (values ast rest3))))))
        ;; async as standalone identifier
        (t
         (values (make-ast-var :name (js-ident-sym "async")) rest))))))

(defun %js-finish-async-arrow (params stream)
  "Parse => body for an async arrow function. Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-expect :T-ARROW stream)
    (declare (ignore tok))
    (let ((body-and-rest
           (if (eq (js-peek-type rest) :T-LBRACE)
               (multiple-value-bind (tok2 rest2) (js-consume rest)
                 (declare (ignore tok2))
                 (multiple-value-bind (body-forms rest3) (js-parse-function-body rest2)
                   (list body-forms rest3)))
               (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                 (list (list (make-ast-return-from :name nil :value expr)) rest2)))))
      (values (%js-call '%js-make-async
                        (make-ast-lambda :params params :body (car body-and-rest)))
              (cadr body-and-rest)))))

;;; ─── Class Expression ────────────────────────────────────────────────────────

(defun %js-parse-class-expr (stream)
  "Parse a class EXPRESSION: class [name] [extends expr] { body }.
Delegates to js-parse-class-decl (parser-class.lisp) — the single source of
truth for class parsing — with :expression-p t. Returns (values ast rest).

Previously this file carried duplicate %js-parse-class-body/%js-parse-class-member
definitions that, under serial load, were shadowed by parser-class.lisp's
arity-2 %js-parse-class-body, so the old 1-arg call here crashed every class
expression. Routing through js-parse-class-decl removes the duplication."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'class'
    (declare (ignore tok))
    (multiple-value-bind (ast-list rest2) (js-parse-class-decl rest :expression-p t)
      (values (if (and (consp ast-list) (= (length ast-list) 1))
                  (first ast-list)
                  (make-ast-progn :forms ast-list))
              rest2))))

;;; ─── import() Dynamic Import ─────────────────────────────────────────────────

(defun %js-parse-import-expr (stream)
  "Parse import(specifier) or import.meta.
Returns (values ast rest)."
  (multiple-value-bind (tok rest) (js-consume stream) ; consume 'import'
    (declare (ignore tok))
    (cond
      ;; import.meta
      ((and (eq (js-peek-type rest) :T-DOT)
            (eq (js-peek-type (cdr rest)) :T-META))
       (multiple-value-bind (dot-tok rest2) (js-consume rest)
         (declare (ignore dot-tok))
         (multiple-value-bind (meta-tok rest3) (js-consume rest2)
           (declare (ignore meta-tok))
           (values (%js-call '%js-import-meta) rest3))))
      ;; import(specifier)
      ((eq (js-peek-type rest) :T-LPAREN)
       (multiple-value-bind (tok2 rest2) (js-consume rest)
         (declare (ignore tok2))
         (multiple-value-bind (spec-ast rest3) (js-parse-assignment-expr rest2)
           (multiple-value-bind (tok3 rest4) (js-expect :T-RPAREN rest3)
             (declare (ignore tok3))
             (values (%js-call '%js-import spec-ast) rest4)))))
      (t
       (error "JS parse error: unexpected token after 'import': ~S" (js-peek rest))))))

;;; ─── Main Pratt Parser ───────────────────────────────────────────────────────

(defun js-parse-expr (stream &optional (min-prec 0))
  "Main Pratt expression parser. Returns (values ast rest).
Handles all infix operators at precedence >= MIN-PREC including
assignment (right-assoc), ternary, binary ops, and comma."
  (multiple-value-bind (lhs rest) (js-parse-unary stream)
    (loop
      (multiple-value-bind (prec right-assoc-p) (js-infix-prec rest)
        (when (<= prec min-prec)
          (return))
        (let ((op-type  (js-peek-type rest))
              (op-val   (js-peek-value rest)))
          (cond
            ;; Ternary: ? then : else
            ((eq op-type :T-QUESTION)
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (multiple-value-bind (then-ast rest3) (js-parse-assignment-expr rest2)
                 (multiple-value-bind (tok2 rest4) (js-expect :T-COLON rest3)
                   (declare (ignore tok2))
                   (multiple-value-bind (else-ast rest5) (js-parse-assignment-expr rest4)
                     (setf lhs (make-ast-if :cond lhs :then then-ast :else else-ast)
                           rest rest5))))))
            ;; Comma operator
            ((eq op-type :T-COMMA)
             (when (> prec min-prec)
               (multiple-value-bind (tok rest2) (js-consume rest)
                 (declare (ignore tok))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 1)
                   (setf lhs (make-ast-progn :forms (list lhs rhs))
                         rest rest3)))))
            ;; Assignment operators (right-associative)
            ((and (eq op-type :T-OP)
                  (member op-val '("=" "+=" "-=" "*=" "/=" "%=" "**="
                                   "<<=" ">>=" ">>>=" "&=" "|=" "^="
                                   "&&=" "||=" "??=")
                          :test #'string=))
             ;; Only assign if prec > min-prec (for right-assoc, use >= on rhs)
             (when (> prec min-prec)
               (multiple-value-bind (tok rest2) (js-consume rest)
                 (declare (ignore tok))
                 (multiple-value-bind (rhs rest3)
                     (js-parse-expr rest2 (if right-assoc-p (1- prec) prec))
                   (setf lhs (%js-lower-assignment op-val lhs rhs)
                         rest rest3)))))
            ;; instanceof / in (keyword tokens, not :T-OP)
            ((or (eq op-type :T-INSTANCEOF) (eq op-type :T-IN))
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (let ((next-prec (if right-assoc-p (1- prec) prec)))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 next-prec)
                   (setf lhs (%js-lower-binary op-val lhs rhs)
                         rest rest3)))))
            ;; All other binary operators
            (t
             (multiple-value-bind (tok rest2) (js-consume rest)
               (declare (ignore tok))
               (let ((next-prec (if right-assoc-p (1- prec) prec)))
                 (multiple-value-bind (rhs rest3) (js-parse-expr rest2 next-prec)
                   (setf lhs (%js-lower-binary op-val lhs rhs)
                         rest rest3)))))))))
    (values lhs rest)))

(defun %js-lower-assignment (op-val lhs rhs)
  "Lower an assignment expression LHS op RHS to the appropriate AST."
  (cond
    ;; Simple variable assignment
    ((ast-var-p lhs)
     (let ((var-sym (ast-var-name lhs)))
       (cond
         ((string= op-val "=")
          (make-ast-setq :var var-sym :value rhs))
         ;; Logical assign
         ((member op-val '("&&=" "||=" "??=") :test #'string=)
          (%js-lower-logical-assign op-val var-sym rhs))
         ;; Compound assign
         (t
          (make-ast-setq :var var-sym
                         :value (%js-compound-rhs op-val lhs rhs))))))
    ;; Property assignment: obj.prop = val or obj[key] = val
    ((and (ast-call-p lhs)
          (ast-var-p (ast-call-func lhs))
          (eq (ast-var-name (ast-call-func lhs)) '%js-get-prop))
     (let ((obj (first  (ast-call-args lhs)))
           (key (second (ast-call-args lhs))))
       (cond
         ((string= op-val "=")
          (%js-call '%js-set-prop obj key rhs))
         (t
          ;; Compound prop assign: obj.k op= rhs → obj.k = (obj.k op rhs)
          (let ((obj-tmp (gensym "JS-OBJ-"))
                (key-tmp (gensym "JS-KEY-")))
            (make-ast-let
             :bindings (list (cons obj-tmp obj) (cons key-tmp key))
             :body (list (%js-call '%js-set-prop
                                   (make-ast-var :name obj-tmp)
                                   (make-ast-var :name key-tmp)
                                   (%js-compound-rhs op-val lhs rhs)))))))))
    ;; Private field assignment
    ((and (ast-call-p lhs)
          (ast-var-p (ast-call-func lhs))
          (eq (ast-var-name (ast-call-func lhs)) '%js-class-private-field-get))
     (let ((obj (first  (ast-call-args lhs)))
           (key (second (ast-call-args lhs))))
       (%js-call '%js-class-private-field-set obj key rhs)))
    ;; Destructuring assignment (array or object pattern) — lower to runtime helper
    (t
     (%js-call '%js-assign-pattern lhs rhs))))

(defun js-parse-assignment-expr (stream)
  "Like js-parse-expr but stops at comma (min-prec = 2).
Use for function arguments and array/object elements."
  (js-parse-expr stream 2))

;;; ─── Entry Point Helpers ─────────────────────────────────────────────────────

(defun js-parse-expression-from-tokens (tokens)
  "Parse a single expression from a token list. Returns (values ast remaining)."
  (js-parse-expr tokens 0))
