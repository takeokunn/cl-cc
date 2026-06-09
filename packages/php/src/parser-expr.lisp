;;;; frontend/php/parser-expr.lisp — PHP Expression Parser
;;;;
;;;; Extracted from parser.lisp.
;;;; Contains expression-level recursive descent parser functions:
;;;;   php-parse-primary  — literals, variables, identifiers, parens, new
;;;;   php-parse-new      — 'new ClassName(args)'
;;;;   php-parse-postfix  — method calls, property access, ++ / --
;;;;   php-parse-unary    — !, -, +, ~
;;;;   php-parse-binop    — left-associative binary operator parsing
;;;;   php-parse-power/mul/add/shift/concat/cmp/bitwise/and/or/coalesce — precedence chain
;;;;   php-parse-expr     — assignment detection + fallthrough to or-level
;;;;   php-parse-arglist  — (arg1, arg2, ...)
;;;;
;;;; Depends on parser.lisp for: php-tok-type, php-tok-value, php-peek,
;;;; php-peek-type, php-peek-value, php-consume, php-expect, php-var-sym,
;;;; php-ident-sym (all loaded before this file).
;;;;
;;;; Load order: after parser.lisp, before parser-expr-advanced.lisp.
(in-package :cl-cc/php)

;;; ─── Member-name helper ──────────────────────────────────────────────────────
;;; PHP permits reserved keywords (from, list, class, print, default, ...) and
;;; type words as method / property / constant names after -> ?-> and ::, e.g.
;;; Status::from(1) or $obj->print().  The plain T-IDENT expectation rejected
;;; them, leaving the enum from/tryFrom/cases lowering unreachable.  This helper
;;; accepts identifiers, keywords, and type words and returns the member's name
;;; as a string.

(defun %php-member-name (stream)
  "Consume a member name (after -> / ?-> / ::) accepting T-IDENT, T-KEYWORD, or
T-TYPE.  Returns (values name-string rest)."
  (let ((type (php-peek-type stream)))
    (if (member type '(:T-IDENT :T-KEYWORD :T-TYPE) :test #'eq)
        (multiple-value-bind (tok rest) (php-consume stream)
          (let ((v (php-tok-value tok)))
            (values (if (stringp v) v (string-downcase (symbol-name v))) rest)))
        (error "PHP parse error: expected member name but got ~S" (php-peek stream)))))

;;; ─── Expression Parser ──────────────────────────────────────────────────────

(defun php-parse-primary (stream known-vars)
  "Parse a primary expression (literal, variable, identifier, parenthesized)."
  (let ((type (php-peek-type stream)))
    (cond
      ((eq type :T-INT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-int :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-FLOAT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-quote :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (%php-string-token-ast (php-tok-value tok)) rest known-vars)))
      ((eq type :T-VAR)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-var :name (php-var-sym (php-tok-value tok))) rest known-vars)))
      ((eq type :T-KEYWORD)
       (let ((kw (php-peek-value stream)))
         (cond
            ((eq kw :null)
             (multiple-value-bind (tok rest) (php-consume stream)
               (declare (ignore tok))
              (values (make-ast-quote :value +php-null+) rest known-vars)))
           ((eq kw :true)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value t) rest known-vars)))
           ((eq kw :false)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value nil) rest known-vars)))
           ((eq kw :new)
            (php-parse-new stream known-vars))
           ((member kw '(:clone :fn :match :yield :throw :array :list :function))
            (%php-parse-keyword-expr stream kw known-vars))
           (t (error "PHP parse error: unexpected keyword ~S in expression" kw)))))
      ((eq type :T-LBRACKET)
       (%php-parse-array-expr stream known-vars))
      ((eq type :T-LPAREN)
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
           (multiple-value-bind (tok2 rest3) (php-expect :T-RPAREN rest2)
             (declare (ignore tok2))
             (values expr rest3 kv2)))))
       ((member type '(:T-IDENT :T-BACKSLASH) :test #'eq)
        ;; Could be a function call or identifier/constant.
        (multiple-value-bind (qualified-name rest) (php-parse-qualified-name stream)
          (let ((name (php-ident-sym (php-resolve-qualified-name qualified-name :const))))
            (if (eq (php-peek-type rest) :T-LPAREN)
                (multiple-value-bind (call rest2 kv2)
                    (%php-parse-function-call qualified-name
                                              (php-ident-sym
                                               (php-resolve-qualified-name qualified-name :function))
                                              rest known-vars)
                  (values call rest2 kv2))
                (values (make-ast-var :name name) rest known-vars)))))
      (t (error "PHP parse error: unexpected token ~S in expression" (php-peek stream))))))

(defun %php-parse-anonymous-class (stream known-vars)
  "Parse an anonymous class expression after `new class` (the `class` keyword is
already consumed): optional (ctor args), optional extends/implements, then a
class body. Returns (values ast rest kv) where ast is a progn that defines a
gensym-named class and makes an instance of it."
  (let ((current stream)
        (ctor-args nil)
        (kv known-vars)
        (anon-name (php-ident-sym (symbol-name (gensym "PHP-ANON-CLASS-")))))
    ;; Optional constructor argument list: new class (args) { ... }
    (when (eq (php-peek-type current) :T-LPAREN)
      (multiple-value-bind (args rest kv2) (php-parse-arglist current kv)
        (setf ctor-args args current rest kv kv2)))
    ;; Optional extends / implements
    (multiple-value-bind (supers rest) (%php-parse-class-superclasses current)
      (setf current rest)
      ;; Class body { members... }
      (let ((body (%php-consume-expected :T-LBRACE current))
            (slots nil))
        (loop
          (setf body (php-skip-semis body))
          (when (or (php-at-eof-p body) (eq (php-peek-type body) :T-RBRACE))
            (return))
          (multiple-value-bind (slot rest2) (%php-parse-class-body-member body known-vars)
            (when slot (push slot slots))
            (setf body rest2)))
        (setf current (%php-consume-expected :T-RBRACE body))
        (values
         (make-ast-progn
          :forms (list (make-ast-defclass :name anon-name
                                          :superclasses supers
                                          :slots (nreverse slots)
                                          :php-kind :class)
                       (make-ast-make-instance
                        :class (make-ast-var :name anon-name)
                        :initargs (loop for i from 0 for a in ctor-args
                                        collect (cons (intern (format nil "ARG~D" i) :keyword) a)))))
         current kv)))))

(defun php-parse-new (stream known-vars)
  "Parse 'new ClassName(args)' or 'new class [(args)] [extends/implements] { ... }'."
  (multiple-value-bind (tok rest) (php-consume stream) ; consume 'new'
    (declare (ignore tok))
    ;; Anonymous class: new class { ... }
    (when (%php-keyword-p rest :class)
      (return-from php-parse-new
        (%php-parse-anonymous-class (cdr rest) known-vars)))
    (multiple-value-bind (qualified-name rest2) (php-parse-qualified-name rest)
      (let ((class-name (php-ident-sym (php-resolve-qualified-name qualified-name :class))))
        (multiple-value-bind (args rest3 kv3) (php-parse-arglist rest2 known-vars)
          ;; new C(args): allocate the instance (properties default-init from their
          ;; initforms), then run __construct($this, args) via %php-construct, and
          ;; yield the instance. (Previously the args were passed as :ARGn CLOS
          ;; initargs — which the class rejected — and the constructor never ran.)
          (let ((inst-sym (gensym "PHP-INST-")))
            (values (make-ast-let
                     :bindings (list (cons inst-sym
                                           (make-ast-make-instance
                                            :class (make-ast-var :name class-name)
                                            :initargs nil)))
                     :body (list
                            ;; Run __construct($this, args) only if the class defines
                            ;; it. The call uses the normal method-dispatch (vm-call)
                            ;; path, with the instance as the implicit $this first arg.
                            (make-ast-if
                             :cond (make-ast-call
                                    :func (make-ast-var :name 'cl-cc/php::%php-has-method)
                                    :args (list (make-ast-var :name inst-sym)
                                                (make-ast-quote :value (php-ident-sym "__construct"))))
                             :then (make-ast-call
                                    :func (make-ast-slot-value
                                           :object (make-ast-var :name inst-sym)
                                           :slot (php-ident-sym "__construct"))
                                    :args (cons (make-ast-var :name inst-sym) args))
                             :else (make-ast-quote :value nil))
                            (make-ast-var :name inst-sym)))
                    rest3 kv3)))))))

;;; ─── Postfix ++/-- lowering ──────────────────────────────────────────────────
;;;
;;; Data-driven: OP is "++" or "--"; the arithmetic op is derived from the table.
;;; Yields the ORIGINAL value (captures it in a gensym) then mutates $var.

(defparameter *php-postfix-incdec-ops*
  '(("++" . +) ("--" . -))
  "Maps postfix operator string to the CL arithmetic symbol.")

(defun %php-lower-postfix-incdec (op obj)
  "Lower PHP postfix OP on OBJ: capture old value, adjust, return old value.
Non-variable targets are returned unchanged (mutation is not supported there)."
  (if (ast-var-p obj)
      (let* ((arith-op (cdr (assoc op *php-postfix-incdec-ops* :test #'equal)))
             (tmp      (gensym "PHP-POSTFIX-"))
             (var-sym  (ast-var-name obj)))
        (make-ast-let
         :bindings (list (cons tmp (make-ast-var :name var-sym)))
         :body     (list (make-ast-setq
                          :var   var-sym
                          :value (make-ast-binop :op  arith-op
                                                 :lhs (make-ast-var :name tmp)
                                                 :rhs (make-ast-int  :value 1)))
                         (make-ast-var :name tmp))))
      obj))

(defun php-parse-postfix (stream known-vars)
  "Parse postfix expressions: method calls, property access, array access."
  (multiple-value-bind (obj rest kv) (php-parse-primary stream known-vars)
    (loop
      (let ((type (php-peek-type rest)))
        (cond
          ;; -> method call or property
          ((eq type :T-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               (let ((prop (php-ident-sym name-str)))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       ;; Bind the receiver to a temp (evaluated once) and pass it
                       ;; as the method's implicit first argument ($this); the
                       ;; method declares $this as its first parameter.
                       (let ((recv (gensym "PHP-RECV-")))
                         (setf obj (make-ast-let
                                    :bindings (list (cons recv obj))
                                    :body (list (make-ast-call
                                                 :func (make-ast-slot-value
                                                        :object (make-ast-var :name recv)
                                                        :slot prop)
                                                 :args (cons (make-ast-var :name recv) args))))
                               rest rest4
                               kv kv4)))
                     (setf obj (make-ast-slot-value :object obj :slot prop)
                           rest rest3))))))
          ;; ?-> nullsafe
          ((eq type :T-NULLSAFE-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               (let* ((prop (php-ident-sym name-str))
                      (null-check (make-ast-binop :op '= :lhs obj
                                                  :rhs (make-ast-quote :value nil))))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (make-ast-if
                                  :cond null-check
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-call
                                         :func (make-ast-slot-value :object obj :slot prop)
                                         :args args))
                             rest rest4
                             kv kv4))
                      (setf obj (make-ast-if
                                 :cond null-check
                                 :then (make-ast-quote :value nil)
                                 :else (make-ast-slot-value :object obj :slot prop))
                            rest rest3))))))
          ;; :: static member/method access. Enum built-ins lower to PHP helpers.
          ((eq type :T-DOUBLE-COLON)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-str rest3) (%php-member-name rest2)
               (let ((member (php-ident-sym name-str)))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (cond
                                   ((string= (symbol-name member) "CASES")
                                    (%php-call 'cl-cc/php::%php-enum-cases obj))
                                   ((string= (symbol-name member) "FROM")
                                    (%php-call 'cl-cc/php::%php-enum-from obj (first args)))
                                   ((string= (symbol-name member) "TRYFROM")
                                    (%php-call 'cl-cc/php::%php-enum-try-from obj (first args)))
                                   (t
                                    (make-ast-call
                                     :func (make-ast-slot-value :object obj :slot member)
                                     :args args)))
                             rest rest4
                             kv kv4))
                     (setf obj (make-ast-slot-value :object obj :slot member)
                           rest rest3))))))
          ;; Postfix ++/-- — yield the ORIGINAL value, then adjust by ±1.
          ;; Only simple $var targets are mutated; complex lvalues are left unchanged.
          ((and (eq type :T-OP) (member (php-peek-value rest) '("++" "--") :test #'equal))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (setf obj (%php-lower-postfix-incdec (php-tok-value tok) obj)
                   rest rest2)))
          ;; Array access: $a[0] or $a[$i]
           ((eq type :T-LBRACKET)
            (multiple-value-bind (tok rest2) (php-consume rest)
              (declare (ignore tok))
              (if (eq (php-peek-type rest2) :T-RBRACKET)
                  ;; Empty subscript $a[] — array append target (assignment LHS).
                  (multiple-value-bind (tok2 rest3) (php-consume rest2)
                    (declare (ignore tok2))
                    (setf obj (%php-array-append-call obj)
                          rest rest3))
                  (multiple-value-bind (idx rest3 kv3) (php-parse-expr rest2 kv)
                    (multiple-value-bind (tok2 rest4) (php-expect :T-RBRACKET rest3)
                      (declare (ignore tok2))
                      (setf obj (%php-array-ref-call obj idx)
                            rest rest4
                            kv kv3))))))
           ;; Dynamic / variable call: $f(args), or a chained call such as
           ;; getCallback()(args). A named call foo(...) is already consumed in
           ;; php-parse-primary, so an LPAREN here always means "call the value of
           ;; OBJ". Lower to (call OBJ args); codegen calls the closure value (same
           ;; path JS uses for fn(args)).
           ((eq type :T-LPAREN)
            (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest kv)
              (setf obj (%php-call-with-spread obj args)
                    rest rest2
                    kv kv2)))
          (t (return)))))
    (values obj rest kv)))

;;; ─── Binary Op AST Builder (data-driven) ────────────────────────────────────
;;;
;;; Operators that lower to PHP runtime helpers are listed here; everything else
;;; falls through to a plain ast-binop.  Adding a new operator = one data line.

(defparameter *php-binary-op-helper-table*
  '(("."   . cl-cc/php::%php-concat)
    ("%"   . cl-cc/php::%php-modulo)
    ("<<"  . cl-cc/php::%php-shift-left)
    (">>"  . cl-cc/php::%php-shift-right)
    ("<=>" . cl-cc/php::%php-spaceship)
    ("&"   . cl-cc/php::%php-bitwise-and)
    ("^"   . cl-cc/php::%php-bitwise-xor)
    ("|"   . cl-cc/php::%php-bitwise-or)
    ;; Equality / identity. PHP == and === carry type-juggling semantics that no
    ;; single VM compare instruction expresses, so they lower to runtime helpers
    ;; rather than a plain ast-binop. Without these, (intern "==") produced an
    ;; unknown cl-cc/php::== op symbol that codegen could not emit, so EVERY
    ;; function/closure whose body used == / != / === / !== silently failed to
    ;; compile and was dropped (then "Undefined function" at the call site).
    ;; Relational ops (< > <= >=) work because (intern "<") returns the inherited
    ;; CL:< that codegen already handles.
    ("=="  . cl-cc/php::%php-eq-loose)
    ("===" . cl-cc/php::%php-eq-strict)
    ("!="  . cl-cc/php::%php-neq-loose)
    ("!==" . cl-cc/php::%php-neq-strict))
  "Alist mapping PHP binary operator strings to runtime helper symbols.")

(defun %php-binary-op-ast (op lhs rhs)
  "Lower PHP binary OP with LHS and RHS to the appropriate AST node."
  (cond
    ;; Short-circuit logical operators. PHP && / || evaluate the RHS only when the
    ;; LHS does not already decide the result, and they yield a real boolean — so
    ;; they lower to an ast-if over truthiness, NOT a binop or helper call (a helper
    ;; would eagerly evaluate both operands, breaking `$x && expensive()`). Each
    ;; branch is %php-truthy so the result is PHP true (t) / false (nil).
    ;; Without this, (intern "&&") produced an unknown cl-cc/php::&& op symbol that
    ;; codegen could not emit, so every expression using && / || failed to compile.
    ((equal op "&&")
     (make-ast-if :cond (%php-truthy-call lhs)
                  :then (%php-truthy-call rhs)
                  :else (make-ast-quote :value nil)))
    ((equal op "||")
     (make-ast-if :cond (%php-truthy-call lhs)
                  :then (make-ast-quote :value t)
                  :else (%php-truthy-call rhs)))
    (t
     (let ((helper (cdr (assoc op *php-binary-op-helper-table* :test #'equal))))
       (if helper
           (%php-call helper lhs rhs)
           (make-ast-binop :op (intern op) :lhs lhs :rhs rhs))))))

(defun php-parse-power (stream known-vars)
  "Parse PHP exponentiation. ** is right-associative and binds above unary."
  (multiple-value-bind (lhs rest kv) (php-parse-postfix stream known-vars)
    (if (and (eq (php-peek-type rest) :T-OP)
             (equal "**" (php-peek-value rest)))
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-unary rest2 kv)
            (values (%php-call 'expt lhs rhs) rest3 kv3)))
        (values lhs rest kv))))

(defun php-lower-prefix-incdec (op operand)
  "Lower PHP prefix ++/-- on OPERAND, yielding the NEW value (unlike postfix,
which yields the original). OP is \"++\" or \"--\". A simple $var is set in
place; array elements ($arr[i]) and object properties ($obj->p) lower through
the compound-assign place machinery (++ ≡ += 1, -- ≡ -= 1)."
  (let ((cop (if (equal op "++") "+=" "-=")))
    (cond
      ((ast-var-p operand)
       (make-ast-setq :var (ast-var-name operand)
                      :value (make-ast-binop :op (if (equal op "++") '+ '-)
                                             :lhs operand
                                             :rhs (make-ast-int :value 1))))
      ((%php-array-ref-call-p operand)
       (%php-lower-compound-assign cop operand (make-ast-int :value 1) :array))
      ((ast-slot-value-p operand)
       (%php-lower-compound-assign cop operand (make-ast-int :value 1) :property))
      (t
       (%php-unsupported
        (format nil "PHP prefix ~A is only supported on a $variable, array element, or property" op)
        operand)))))

(defun php-parse-unary (stream known-vars)
  "Parse unary expressions: prefix ++/--, !, -, +, ~."
  (cond
    ((%php-reference-token-p stream)
      ;; PHP reference operator (&expr): cl-cc uses value semantics, so a
      ;; reference lowers to the referenced value itself. This makes reference
      ;; assignment ($b = &$a), foreach (... as &$v), and by-ref arguments parse
      ;; and run with value semantics — true aliasing is not modelled.
      (multiple-value-bind (tok rest) (php-consume stream)
        (declare (ignore tok))
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values expr rest2 kv2))))
    ;; Prefix ++ / -- : increment/decrement the place, then yield the new value
    ((and (eq (php-peek-type stream) :T-OP)
          (member (php-peek-value stream) '("++" "--") :test #'equal))
      (multiple-value-bind (op-tok rest) (php-consume stream)
        (multiple-value-bind (operand rest2 kv2) (php-parse-postfix rest known-vars)
          (values (php-lower-prefix-incdec (php-tok-value op-tok) operand)
                  rest2 kv2))))
    ((and (eq (php-peek-type stream) :T-OP)
           (member (php-peek-value stream) '("!" "-" "+" "~") :test #'equal))
      (multiple-value-bind (tok rest) (php-consume stream)
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values (cond
                    ((equal "~" (php-tok-value tok))
                     (%php-call 'cl-cc/php::%php-bitwise-not expr))
                    ;; Logical NOT: yield PHP false (nil) when EXPR is truthy, else
                    ;; PHP true (t). Was lowered to a call on an undefined cl-cc/php::!
                    ;; function, so any expression with ! failed.
                    ((equal "!" (php-tok-value tok))
                     (make-ast-if :cond (%php-truthy-call expr)
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-quote :value t)))
                    (t
                     (make-ast-call :func (make-ast-var :name (intern (php-tok-value tok)))
                                    :args (list expr))))
                  rest2 kv2))))
     (t
      (php-parse-power stream known-vars))))

(defun php-parse-binop (stream known-vars ops next-parser)
  "Left-associative binary operator parsing."
  (multiple-value-bind (lhs rest kv) (funcall next-parser stream known-vars)
    (loop
      (if (and (eq (php-peek-type rest) :T-OP)
               (member (php-peek-value rest) ops :test #'equal))
          (multiple-value-bind (op-tok rest2) (php-consume rest)
            (multiple-value-bind (rhs rest3 kv3) (funcall next-parser rest2 kv)
              (setf lhs (%php-binary-op-ast (php-tok-value op-tok) lhs rhs)
                     rest rest3
                     kv kv3)))
          (return)))
    (values lhs rest kv)))

;;; ─── Binary Operator Precedence Chain (data-driven) ─────────────────────────
;;;
;;; Each entry: (function-name (op-strings...) next-level-function)
;;; Ordered from highest to lowest precedence.  Adding a new level = one data line.

(defmacro define-php-binop-levels (&body levels)
  "Generate left-associative binary-operator parser functions from a data table.
   Each entry: (name (ops...) next-parser)."
  `(progn
     ,@(mapcar (lambda (entry)
                 (destructuring-bind (fname ops next) entry
                   `(defun ,fname (stream known-vars)
                      (php-parse-binop stream known-vars ',ops #',next))))
               levels)))

(define-php-binop-levels
  (php-parse-mul        ("*" "/" "%")               php-parse-unary)
  (php-parse-add        ("+" "-")                   php-parse-mul)
  (php-parse-shift      ("<<" ">>")                 php-parse-add)
  (php-parse-concat     (".")                        php-parse-shift)
  (php-parse-relational ("<" ">" "<=" ">=" "<=>")   php-parse-concat)
  (php-parse-cmp        ("==" "===" "!=" "!==")     php-parse-relational)
  (php-parse-bit-and    ("&")                        php-parse-cmp)
  (php-parse-bit-xor    ("^")                        php-parse-bit-and)
  (php-parse-bit-or     ("|")                        php-parse-bit-xor)
  (php-parse-and        ("&&")                       php-parse-bit-or)
  (php-parse-or         ("||")                       php-parse-and))

(defun php-parse-coalesce (stream known-vars)
  "Parse right-associative null coalescing ?? without evaluating the left side twice."
  (multiple-value-bind (lhs rest kv) (php-parse-or stream known-vars)
    (if (and (eq (php-peek-type rest) :T-OP)
             (equal "??" (php-peek-value rest)))
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-coalesce rest2 kv)
            (values (%php-lower-null-coalesce lhs rhs) rest3 kv3)))
        (values lhs rest kv))))

(defun php-parse-ternary (stream known-vars)
  "Parse PHP ternary and Elvis operators."
  (multiple-value-bind (cond-expr rest kv) (php-parse-coalesce stream known-vars)
    (if (eq (php-peek-type rest) :T-NULLABLE)
        (multiple-value-bind (question-token rest2) (php-consume rest)
          (declare (ignore question-token))
          (if (eq (php-peek-type rest2) :T-COLON)
              (multiple-value-bind (colon-token rest3) (php-consume rest2)
                (declare (ignore colon-token))
                (multiple-value-bind (else-expr rest4 kv4) (php-parse-expr rest3 kv)
                  (values (%php-lower-elvis cond-expr else-expr) rest4 kv4)))
              (multiple-value-bind (then-expr rest3 kv3) (php-parse-expr rest2 kv)
                (multiple-value-bind (colon-token rest4) (php-expect :T-COLON rest3)
                  (declare (ignore colon-token))
                  (multiple-value-bind (else-expr rest5 kv5) (php-parse-expr rest4 kv3)
                    (values (make-ast-if :cond cond-expr :then then-expr :else else-expr)
                            rest5 kv5))))))
        (values cond-expr rest kv))))

(defun php-parse-expr (stream known-vars)
  "Parse an expression. Handles variable and PHP array-element assignment."
  (multiple-value-bind (lhs rest kv) (php-parse-ternary stream known-vars)
    (cond
      ((%php-assignment-op rest)
       (let ((op (%php-assignment-op rest))
             (rest2 (cdr rest)))
           (multiple-value-bind (val rest3 kv3) (php-parse-ternary rest2 kv)
            (cond
              ((ast-var-p lhs)
               (let* ((var-sym (ast-var-name lhs))
                      (already-known (member var-sym known-vars))
                      (new-kv (if already-known kv3 (cons var-sym kv3))))
                 (values
                  (if (equal op "=")
                      (if already-known
                          (make-ast-setq :var var-sym :value val)
                          (make-ast-let :bindings (list (cons var-sym val)) :body nil))
                      (%php-lower-compound-assign op lhs val :var))
                  rest3
                  new-kv)))
              ;; $a[] = v  — append. The array is a mutable hash-table held by
              ;; reference, so pushing onto it is the whole effect (no reassign).
              ((%php-array-append-call-p lhs)
               (let ((arr (first (ast-call-args lhs))))
                 (unless (equal op "=")
                   (error "PHP parse error: ~A not allowed on [] append target" op))
                 (values (%php-call 'cl-cc/php::%php-array-push arr val) rest3 kv3)))
              ((%php-array-ref-call-p lhs)
               (destructuring-bind (arr key) (ast-call-args lhs)
                 (values (if (equal op "=")
                             (%php-array-set-call arr key val)
                             (%php-lower-compound-assign op lhs val :array))
                         rest3 kv3)))
              ((ast-slot-value-p lhs)
               (values (if (equal op "=")
                           (make-ast-set-slot-value
                            :object (ast-slot-value-object lhs)
                            :slot (ast-slot-value-slot lhs)
                            :value val)
                           (%php-lower-compound-assign op lhs val :property))
                       rest3 kv3))
              ;; List/array destructuring assignment: [$a, $b] = expr (and the
              ;; legacy list($a, $b) = expr, which lowers to the same array LHS).
              ((and (equal op "=") (%php-array-literal-call-p lhs))
               (values (%php-lower-list-assign lhs val) rest3 kv3))
              (t
               (error "PHP parse error: unsupported assignment target ~S" lhs))))))
      ((%php-reference-token-p rest)
       ;; A `&` following a complete expression at this level is infix bitwise-AND
       ;; (the precedence chain normally consumes it; this is a defensive fallback).
       (multiple-value-bind (tok rest2) (php-consume rest)
         (declare (ignore tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-ternary rest2 kv)
           (values (%php-binary-op-ast "&" lhs rhs) rest3 kv3))))
      (t
       (values lhs rest kv)))))

(defun php-parse-arglist (stream known-vars)
  "Parse (arg1, arg2, ...). Assumes stream starts with T-LPAREN."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2 known-vars))
        (let ((args nil)
              (current rest)
              (kv known-vars))
          (loop
            (cond
              ;; First-class callable syntax: f(...)  ->  marker arg the caller
              ;; can turn into a callable reference. Detected as '...' then ')'.
              ((and (eq (php-peek-type current) :T-ELLIPSIS)
                    (eq (php-peek-type (cdr current)) :T-RPAREN))
               (push (make-ast-call :func (make-ast-var :name '%php-first-class-callable)
                                    :args nil)
                     args)
               (setf current (cdr current)))
              ;; Spread argument: ...expr  ->  (%php-spread expr)
              ((eq (php-peek-type current) :T-ELLIPSIS)
               (multiple-value-bind (_tok rest-after) (php-consume current)
                 (declare (ignore _tok))
                 (multiple-value-bind (arg rest2 kv2) (php-parse-expr rest-after kv)
                   (push (make-ast-call :func (make-ast-var :name '%php-spread)
                                        :args (list arg))
                         args)
                   (setf current rest2 kv kv2))))
              ;; Named argument: ident: expr  ->  (%php-named-arg "ident" expr)
              ((%php-named-arg-p current)
               (let* ((name-tok (php-peek current))
                      (name-str (let ((v (php-tok-value name-tok)))
                                  (if (stringp v) v (string-downcase (symbol-name v)))))
                      (after-colon (cddr current)))   ; skip ident and ':'
                 (multiple-value-bind (arg rest2 kv2) (php-parse-expr after-colon kv)
                   (push (make-ast-call :func (make-ast-var :name '%php-named-arg)
                                        :args (list (make-ast-quote :value name-str) arg))
                         args)
                   (setf current rest2 kv kv2))))
              ;; Ordinary positional argument
              (t
               (multiple-value-bind (arg rest2 kv2) (php-parse-expr current kv)
                 (push arg args)
                 (setf current rest2 kv kv2))))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse args) rest2 kv))))))

;;; Extended expression handlers (%php-parse-keyword-expr, array helpers,
;;; match/yield lowering, compound-assign lowering, etc.) are defined in
;;; parser-expr-advanced.lisp, which is loaded immediately after this file.
