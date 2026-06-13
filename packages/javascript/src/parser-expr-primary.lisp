;;;; packages/javascript/src/parser-expr-primary.lisp — Primary Expression Parsers
;;;;
;;;; Concrete primary expression forms: literals, identifiers, parenthesized
;;;; expressions, arrow functions, async/class/import expressions, and the
;;;; main Pratt entry points (js-parse-expr, js-parse-assignment-expr).
;;;;
;;;; Depends on parser-expr.lisp for Pratt precedence table, operator lowering,
;;;; parameter parsers, array/object literals, function expression, new/member,
;;;; postfix, template literal, and unary parsers.
;;;;
;;;; Load order: after parser-expr.lisp, before parser-stmt.lisp.

(in-package :cl-cc/javascript)

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
      ;; null literal. The runtime null sentinel is +js-null+ (= :js-null); this
      ;; MUST match it, not the bare :null keyword — otherwise %js-to-string prints
      ;; "NULL" and %js-not-nullish treats `null' as non-nullish (null ?? x broke).
      ;; (+js-null+ itself can't be named here: this file compiles before runtime.)
      ((eq type :T-NULL)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :js-null) rest)))
      ;; undefined — likewise the +js-undefined+ sentinel (= :js-undefined)
      ((eq type :T-UNDEFINED)
       (multiple-value-bind (tok rest) (js-consume stream)
         (declare (ignore tok))
         (values (make-ast-quote :value :js-undefined) rest)))
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


;;; Arrow/paren/async/class/import expression helpers → see parser-arrow.lisp
;;; Main Pratt parser → see below (js-parse-expr, js-parse-assignment-expr)
;;; ─── Main Pratt Parser ───────────────────────────────────────────────────────

(defun js-parse-expr (stream &optional (min-prec 0))
  "Main Pratt expression parser. Returns (values ast rest).
Handles all infix operators at precedence >= MIN-PREC including
assignment (right-assoc), ternary, binary ops, and comma."
  (with-js-parse-depth
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
                     ;; Coerce the condition to a JS boolean (%js-truthy), like the
                     ;; if/while/for statements do — otherwise "", null, undefined
                     ;; and NaN (all non-nil values) test as truthy in `c ? a : b'.
                     (setf lhs (make-ast-if :cond (%js-truthy-call lhs)
                                            :then then-ast :else else-ast)
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
    (values lhs rest))))

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
