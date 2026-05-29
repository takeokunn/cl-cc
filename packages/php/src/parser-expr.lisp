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
;;;; Load order: after parser.lisp, before parser-stmt.lisp.
(in-package :cl-cc/php)

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

(defun php-parse-new (stream known-vars)
  "Parse 'new ClassName(args)'."
  (multiple-value-bind (tok rest) (php-consume stream) ; consume 'new'
    (declare (ignore tok))
    (multiple-value-bind (qualified-name rest2) (php-parse-qualified-name rest)
      (let ((class-name (php-ident-sym (php-resolve-qualified-name qualified-name :class))))
        (multiple-value-bind (args rest3 kv3) (php-parse-arglist rest2 known-vars)
          (values (make-ast-make-instance
                   :class (make-ast-var :name class-name)
                   :initargs (loop for i from 0 for a in args
                                   collect (cons (intern (format nil "ARG~D" i) :keyword) a)))
                  rest3 kv3))))))

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
             (multiple-value-bind (name-tok rest3) (php-expect :T-IDENT rest2)
               (let ((prop (php-ident-sym (php-tok-value name-tok))))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (make-ast-call
                                  :func (make-ast-slot-value :object obj :slot prop)
                                  :args args)
                             rest rest4
                             kv kv4))
                     (setf obj (make-ast-slot-value :object obj :slot prop)
                           rest rest3))))))
          ;; ?-> nullsafe
          ((eq type :T-NULLSAFE-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-tok rest3) (php-expect :T-IDENT rest2)
               (let* ((prop (php-ident-sym (php-tok-value name-tok)))
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
             (multiple-value-bind (name-tok rest3) (php-expect :T-IDENT rest2)
               (let ((member (php-ident-sym (php-tok-value name-tok))))
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
          ;; ++ postfix — produce ast-setq incrementing by 1
          ((and (eq type :T-OP) (equal "++" (php-peek-value rest)))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (setf obj (make-ast-setq :var (ast-var-name obj)
                                      :value (make-ast-binop :op '+
                                              :lhs obj :rhs (make-ast-int :value 1)))
                   rest rest2)))
          ;; -- postfix — produce ast-setq decrementing by 1
          ((and (eq type :T-OP) (equal "--" (php-peek-value rest)))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (setf obj (make-ast-setq :var (ast-var-name obj)
                                      :value (make-ast-binop :op '-
                                              :lhs obj :rhs (make-ast-int :value 1)))
                   rest rest2)))
          ;; Array access: $a[0] or $a[$i]
           ((eq type :T-LBRACKET)
            (multiple-value-bind (tok rest2) (php-consume rest)
              (declare (ignore tok))
              (multiple-value-bind (idx rest3 kv3) (php-parse-expr rest2 kv)
                (multiple-value-bind (tok2 rest4) (php-expect :T-RBRACKET rest3)
                  (declare (ignore tok2))
                  (setf obj (%php-array-ref-call obj idx)
                        rest rest4
                        kv kv3)))))
          (t (return)))))
    (values obj rest kv)))

(defun %php-binary-op-ast (op lhs rhs)
  "Lower PHP binary OP with LHS and RHS to the appropriate AST node."
  (cond ((equal op ".")
         (%php-call 'cl-cc/php::%php-concat lhs rhs))
        ((equal op "%")
         (%php-call 'cl-cc/php::%php-modulo lhs rhs))
        ((equal op "<<")
         (%php-call 'cl-cc/php::%php-shift-left lhs rhs))
        ((equal op ">>")
         (%php-call 'cl-cc/php::%php-shift-right lhs rhs))
        ((equal op "<=>")
         (%php-call 'cl-cc/php::%php-spaceship lhs rhs))
        ((equal op "&")
         (%php-call 'cl-cc/php::%php-bitwise-and lhs rhs))
        ((equal op "^")
         (%php-call 'cl-cc/php::%php-bitwise-xor lhs rhs))
        ((equal op "|")
         (%php-call 'cl-cc/php::%php-bitwise-or lhs rhs))
        (t
         (make-ast-binop :op (intern op) :lhs lhs :rhs rhs))))

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

(defun php-parse-unary (stream known-vars)
  "Parse unary expressions: !, -, +, ~."
  (cond
    ((%php-reference-token-p stream)
      (multiple-value-bind (tok rest) (php-consume stream)
        (declare (ignore tok))
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values (%php-unsupported "PHP reference operator (&) is not yet supported" expr)
                  rest2 kv2))))
    ((and (eq (php-peek-type stream) :T-OP)
           (member (php-peek-value stream) '("!" "-" "+" "~") :test #'equal))
      (multiple-value-bind (tok rest) (php-consume stream)
        (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
          (values (if (equal "~" (php-tok-value tok))
                      (%php-call 'cl-cc/php::%php-bitwise-not expr)
                      (make-ast-call :func (make-ast-var :name (intern (php-tok-value tok)))
                                     :args (list expr)))
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

(defun php-parse-mul (stream known-vars)
  (php-parse-binop stream known-vars '("*" "/" "%") #'php-parse-unary))

(defun php-parse-add (stream known-vars)
  (php-parse-binop stream known-vars '("+" "-") #'php-parse-mul))

(defun php-parse-shift (stream known-vars)
  (php-parse-binop stream known-vars '("<<" ">>") #'php-parse-add))

(defun php-parse-concat (stream known-vars)
  (php-parse-binop stream known-vars '(".") #'php-parse-shift))

(defun php-parse-relational (stream known-vars)
  (php-parse-binop stream known-vars '("<" ">" "<=" ">=" "<=>")
                   #'php-parse-concat))

(defun php-parse-cmp (stream known-vars)
  (php-parse-binop stream known-vars '("==" "===" "!=" "!==")
                   #'php-parse-relational))

(defun php-parse-bit-and (stream known-vars)
  (php-parse-binop stream known-vars '("&") #'php-parse-cmp))

(defun php-parse-bit-xor (stream known-vars)
  (php-parse-binop stream known-vars '("^") #'php-parse-bit-and))

(defun php-parse-bit-or (stream known-vars)
  (php-parse-binop stream known-vars '("|") #'php-parse-bit-xor))

(defun php-parse-and (stream known-vars)
  (php-parse-binop stream known-vars '("&&") #'php-parse-bit-or))

(defun php-parse-or (stream known-vars)
  (php-parse-binop stream known-vars '("||") #'php-parse-and))

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
              (t
               (error "PHP parse error: unsupported assignment target ~S" lhs))))))
      ((%php-reference-token-p rest)
       (multiple-value-bind (tok rest2) (php-consume rest)
         (declare (ignore tok))
          (multiple-value-bind (rhs rest3 kv3) (php-parse-ternary rest2 kv)
           (declare (ignore rhs))
           (values (%php-unsupported "PHP bitwise AND operator (&) is not yet supported" lhs)
                   rest3 kv3))))
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
            (multiple-value-bind (arg rest2 kv2) (php-parse-expr current kv)
              (push arg args)
              (setf current rest2 kv kv2))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse args) rest2 kv))))))

;;; ─── Extended Expression Handlers ──────────────────────────────────────

(defun %php-parse-keyword-expr (stream kw known-vars)
  "Dispatch keyword-led expression to the appropriate handler."
  (multiple-value-bind (tok rest) (php-consume stream)
    (declare (ignore tok))
    (case kw
      (:clone
       (let ((fn (make-ast-var :name 'clone)))
         (if (and (eq (php-peek-type rest) :T-LPAREN)
                  (eq (php-peek-type (cdr rest)) :T-ELLIPSIS))
             (values (make-ast-quote :value nil)
                     (cdddr rest) known-vars)
             (values (make-ast-call :func fn
                                     :args (list (nth-value 0 (php-parse-unary rest known-vars))))
                     (nth-value 1 (php-parse-unary rest known-vars))
                     known-vars))))
      (:fn
       (%php-parse-arrow-function rest known-vars))
      (:match
       (%php-parse-match-expression rest known-vars))
      (:yield
        (%php-parse-yield-expression rest known-vars))
      (:throw
       (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
          (values (make-ast-throw :tag (make-ast-quote :value 'php-exception)
                                   :value (%php-exception-payload-call expr))
                  rest2 kv2)))
      (:array
        (if (eq (php-peek-type rest) :T-LPAREN)
            (%php-parse-array-expr rest known-vars :open :T-LPAREN :close :T-RPAREN)
            (values (make-ast-quote :value nil) rest known-vars)))
      (:list
       (if (eq (php-peek-type rest) :T-LPAREN)
           (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
             (values (make-ast-call :func (make-ast-var :name '%php-list-bind) :args args)
                     rest2 kv2))
           (values (make-ast-quote :value nil) rest known-vars)))
      (:function
       (%php-parse-anonymous-function rest known-vars)))))

(defun %php-null-quote ()
  "Return the AST representation of PHP null."
  (make-ast-quote :value +php-null+))

(defun %php-call (name &rest args)
  "Build an AST call to NAME with ARGS."
  (make-ast-call :func (make-ast-var :name name) :args args))

(defun %php-truthy-call (expr)
  "Build a PHP truthiness helper call for conditional contexts."
  (%php-call 'cl-cc/php::%php-truthy expr))

(defun %php-exception-class-from-expression (expr)
  "Return the PHP class symbol visible in a throw expression, when known."
  (when (and (ast-make-instance-p expr)
             (ast-var-p (ast-make-instance-class expr)))
    (ast-var-name (ast-make-instance-class expr))))

(defun %php-exception-payload-call (expr)
  "Wrap thrown PHP EXPR with class metadata for VM catch/throw lowering."
  (%php-call 'cl-cc/php::%php-make-exception
             (make-ast-quote :value (%php-exception-class-from-expression expr))
             expr))

(defun %php-string-token-ast (value)
  "Lower a lexer string VALUE to AST, preserving simple interpolation."
  (if (and (consp value) (eq (first value) :php-interpolated-string))
      (let ((args (mapcar (lambda (segment)
                            (ecase (first segment)
                              (:string (make-ast-quote :value (second segment)))
                              (:var (make-ast-var :name (php-var-sym (second segment))))))
                          (second value))))
        (if args
            (make-ast-call :func (make-ast-var :name 'cl-cc/php::%php-concat)
                           :args args)
            (make-ast-quote :value "")))
      (make-ast-quote :value value)))

(defun %php-builtin-helper-symbol (qualified-name)
  "Return the PHP runtime helper symbol for simple builtin QUALIFIED-NAME."
  (let ((lower (string-downcase qualified-name)))
    (unless (find (code-char 92) lower)
      (cond ((string= lower "count") 'cl-cc/php::%php-count)
            ((string= lower "strlen") 'cl-cc/php::%php-strlen)
            ((string= lower "strtolower") 'cl-cc/php::%php-strtolower)
            ((string= lower "strtoupper") 'cl-cc/php::%php-strtoupper)
            ((string= lower "isset") 'cl-cc/php::%php-isset)
            ((string= lower "array_key_exists") 'cl-cc/php::%php-array-key-exists)
            (t nil)))))

(defun %php-simple-function-spelling (qualified-name)
  "Return QUALIFIED-NAME as a simple global function name, or NIL."
  (let ((name (%php-strip-leading-namespace-separator qualified-name)))
    (unless (find (code-char 92) name)
      name)))

(defun %php-global-builtin-function-p (qualified-name fallback-name)
  "Return true when QUALIFIED-NAME can use PHP global builtin helper lowering."
  (let ((simple-name (%php-simple-function-spelling qualified-name)))
    (and simple-name
         (or (%php-name-absolute-p qualified-name)
             (not (and *php-current-namespace*
                       (not (string= *php-current-namespace* "")))))
         (string= (string-upcase simple-name)
                  (symbol-name fallback-name)))))

(defun %php-parse-function-call (qualified-name fallback-name stream known-vars)
  "Parse a PHP function call and lower known builtins to runtime helpers."
  (multiple-value-bind (args rest kv) (php-parse-arglist stream known-vars)
    (values (make-ast-call
             :func (make-ast-var
                    :name (or (and (%php-global-builtin-function-p
                                    qualified-name fallback-name)
                                   (%php-builtin-helper-symbol
                                    (%php-simple-function-spelling qualified-name)))
                              fallback-name))
             :args args)
            rest kv)))

(defun %php-not-null-cond (expr)
  "Build (not (eq EXPR null)) as an AST expression."
  (%php-call 'not (%php-call 'eq expr (%php-null-quote))))

(defun %php-lower-null-coalesce (lhs rhs)
  "Lower LHS ?? RHS without evaluating LHS twice."
  (let ((tmp (gensym "PHP-COALESCE-")))
    (make-ast-let
     :bindings (list (cons tmp lhs))
     :body (list (make-ast-if
                  :cond (%php-not-null-cond (make-ast-var :name tmp))
                  :then (make-ast-var :name tmp)
                  :else rhs)))))

(defun %php-lower-elvis (condition else-expr)
  "Lower CONDITION ?: ELSE-EXPR without evaluating CONDITION twice."
  (let ((tmp (gensym "PHP-ELVIS-")))
    (make-ast-let
     :bindings (list (cons tmp condition))
     :body (list (make-ast-if
                  :cond (make-ast-var :name tmp)
                  :then (make-ast-var :name tmp)
                  :else else-expr)))))

(defun %php-unsupported (message &optional expr)
  "Build a PHP unsupported-form marker call."
  (make-ast-call :func (make-ast-var :name '%php-unsupported)
                 :args (append (list (make-ast-quote :value message))
                               (when expr (list expr)))))

(defun %php-capture-wrapper (captures value)
  "Wrap VALUE in explicit let-bindings that snapshot CAPTURES by value."
  (make-ast-let
   :bindings (mapcar (lambda (name) (cons name (make-ast-var :name name))) captures)
   :body (list value)))

(defun %php-find-free-vars (body)
  "Return a list of free variable symbols referenced in BODY AST."
  (let ((free nil))
    (labels ((walk (node)
               (when (typep node 'cl-cc/ast:ast-var)
                 (pushnew (cl-cc/ast:ast-var-name node) free :test #'eq))
               (when (typep node 'cl-cc/ast:ast-node)
                 (dolist (child (cl-cc/ast:ast-children node))
                   (when child (walk child))))))
      (walk body)
      free)))

(defun %php-arrow-captures (body params known-vars)
  "Return PHP variables captured by an arrow function body."
  (set-difference (intersection (%php-find-free-vars body) known-vars :test #'eq)
                  params :test #'eq))

(defun %php-parse-arrow-function (stream known-vars)
  "Parse fn(params) => expr and lower it to captured ast-lambda."
  (multiple-value-bind (params rest param-types) (php-parse-param-list stream)
    (declare (ignore param-types))
    (multiple-value-bind (return-type rest2) (php-parse-return-type rest)
      (declare (ignore return-type))
      (unless (and (eq (php-peek-type rest2) :T-OP)
                   (equal "=>" (php-peek-value rest2)))
        (error "PHP parse error: expected => after arrow function parameters"))
      (multiple-value-bind (arrow-token rest3) (php-consume rest2)
        (declare (ignore arrow-token))
        (multiple-value-bind (body rest4 kv4) (php-parse-expr rest3 (append params known-vars))
          (let* ((captures (%php-arrow-captures body params known-vars))
                 (lambda (make-ast-lambda :params params :body (list body))))
            (values (%php-capture-wrapper captures lambda) rest4 kv4)))))))

(defun %php-parse-closure-use-list (stream)
  "Parse optional PHP closure use($x, $y) and return captures/rest."
  (if (and (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :use))
      (let ((current (%php-consume-expected :T-LPAREN (cdr stream)))
            (captures nil))
        (unless (eq (php-peek-type current) :T-RPAREN)
          (loop
            (when (and (eq (php-peek-type current) :T-OP)
                       (equal "&" (php-peek-value current)))
              (error "PHP parse error: PHP closure use-by-reference (&) is not yet supported"))
            (multiple-value-bind (var-token rest) (php-expect :T-VAR current)
              (push (php-var-sym (php-tok-value var-token)) captures)
              (setf current rest))
            (if (eq (php-peek-type current) :T-COMMA)
                (setf current (cdr current))
                (return))))
        (values (nreverse captures) (%php-consume-expected :T-RPAREN current)))
      (values nil stream)))

(defun %php-parse-anonymous-function (stream known-vars)
  "Parse function(params) use(vars) { body } as an ast-lambda with explicit captures."
  (multiple-value-bind (params rest param-types) (php-parse-param-list stream)
    (declare (ignore param-types))
    (multiple-value-bind (captures rest2) (%php-parse-closure-use-list rest)
      (multiple-value-bind (return-type rest3) (php-parse-return-type rest2)
        (declare (ignore return-type))
        (multiple-value-bind (body-stmts rest4 kv4)
            (php-parse-block rest3 (append params captures known-vars))
          (values (%php-capture-wrapper
                   captures
                   (make-ast-lambda :params params :body body-stmts))
                   rest4 kv4))))))

(defun %php-parse-yield-unsupported (stream known-vars)
  "Parse yield/yield from as an unsupported marker with the yielded expression attached."
  (if (and (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :from))
      (multiple-value-bind (from-token rest) (php-consume stream)
        (declare (ignore from-token))
        (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
          (values (%php-unsupported "PHP yield from is not yet supported" expr)
                  rest2 kv2)))
      (if (member (php-peek-type stream) '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF) :test #'eq)
          (values (%php-unsupported "PHP yield is not yet supported") stream known-vars)
           (multiple-value-bind (expr rest2 kv2) (php-parse-expr stream known-vars)
             (values (%php-unsupported "PHP yield is not yet supported" expr)
                     rest2 kv2)))))

(defun %php-parse-yield-expression (stream known-vars)
  "Parse yield/yield from into PHP runtime helper calls.

This is still a lightweight yield representation rather than a full coroutine
engine, but it prevents yield syntax from being silently rejected and preserves
the yielded value for later generator lowering passes."
  (cond
    ((and (eq (php-peek-type stream) :T-KEYWORD)
          (eq (php-peek-value stream) :from))
     (multiple-value-bind (from-token rest) (php-consume stream)
       (declare (ignore from-token))
       (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
         (values (%php-call 'cl-cc/php::%php-yield-from expr) rest2 kv2))))
    ((member (php-peek-type stream) '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF) :test #'eq)
     (values (%php-call 'cl-cc/php::%php-yield) stream known-vars))
    (t
     (multiple-value-bind (expr rest2 kv2) (php-parse-expr stream known-vars)
       (values (%php-call 'cl-cc/php::%php-yield expr) rest2 kv2)))))

(defun %php-match-error-call ()
  "Return the fallback call used when no PHP match arm applies."
  (%php-call '%php-match-error))

(defun %php-build-match-condition (subject-sym tests)
  "Build an OR of strict-equality tests for one match arm."
  (let ((comparisons
          (mapcar (lambda (test)
                    (%php-call 'cl-cc/php::%php-eq-strict
                               (make-ast-var :name subject-sym)
                               test))
                  tests)))
    (reduce (lambda (lhs rhs)
              (make-ast-binop :op 'or :lhs lhs :rhs rhs))
            (cdr comparisons)
            :initial-value (car comparisons))))

(defun %php-lower-match (subject arms default-expr)
  "Lower PHP match SUBJECT and ARMS to a subject let plus nested ast-if chain."
  (let ((subject-sym (gensym "PHP-MATCH-SUBJECT-")))
    (labels ((chain (remaining)
               (if remaining
                   (destructuring-bind (tests . value) (car remaining)
                     (make-ast-if
                      :cond (%php-build-match-condition subject-sym tests)
                      :then value
                      :else (chain (cdr remaining))))
                   (or default-expr (%php-match-error-call)))))
      (make-ast-let
       :bindings (list (cons subject-sym subject))
       :body (list (chain arms))))))

(defun %php-parse-match-arm-tests (stream known-vars)
  "Parse one PHP match arm condition list or default marker."
  (if (and (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :default))
      (values :default (cdr stream) known-vars)
      (let ((tests nil)
            (current stream)
            (kv known-vars))
        (loop
          (multiple-value-bind (test rest kv2) (php-parse-expr current kv)
            (push test tests)
            (setf current rest kv kv2))
          (cond
            ((%php-double-arrow-p current) (return))
            ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current)))
            (t (error "PHP parse error: expected comma or => in match arm, got ~S"
                      (php-peek current)))))
        (values (nreverse tests) current kv))))

(defun %php-parse-match-expression (stream known-vars)
  "Parse match(expr) { arms } and lower to let plus nested ifs."
  (let ((current (%php-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (subject rest kv) (php-parse-expr current known-vars)
      (setf current (%php-consume-expected :T-RPAREN rest))
      (setf current (%php-consume-expected :T-LBRACE current))
      (let ((arms nil)
            (default-expr nil)
            (kv-current kv))
        (loop
          (when (eq (php-peek-type current) :T-RBRACE)
            (return))
          (multiple-value-bind (tests rest2 kv2) (%php-parse-match-arm-tests current kv-current)
            (setf current rest2 kv-current kv2)
            (unless (%php-double-arrow-p current)
              (error "PHP parse error: expected => in match arm, got ~S" (php-peek current)))
            (multiple-value-bind (arrow-token rest3) (php-consume current)
              (declare (ignore arrow-token))
              (multiple-value-bind (value rest4 kv4) (php-parse-expr rest3 kv-current)
                (if (eq tests :default)
                    (setf default-expr value)
                    (push (cons tests value) arms))
                (setf current rest4 kv-current kv4))))
          (cond
            ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current)))
            ((eq (php-peek-type current) :T-RBRACE))
            (t (error "PHP parse error: expected comma or } after match arm, got ~S"
                      (php-peek current)))))
        (values (%php-lower-match subject (nreverse arms) default-expr)
                (%php-consume-expected :T-RBRACE current)
                kv-current)))))

(defun %php-helper-var (name)
  "Return an AST variable for a PHP runtime helper NAME."
  (make-ast-var :name name))

(defun %php-compound-value (op lhs rhs)
  "Return the read-modify-write value for PHP compound assignment OP."
  (cond ((equal op "+=") (make-ast-binop :op '+ :lhs lhs :rhs rhs))
        ((equal op "-=") (make-ast-binop :op '- :lhs lhs :rhs rhs))
        ((equal op "*=") (make-ast-binop :op '* :lhs lhs :rhs rhs))
        ((equal op "/=") (make-ast-binop :op '/ :lhs lhs :rhs rhs))
        ((equal op ".=") (%php-call 'cl-cc/php::%php-concat lhs rhs))
        ((equal op "%=") (%php-call 'cl-cc/php::%php-modulo lhs rhs))
        ((equal op "**=") (%php-call 'expt lhs rhs))
        ((equal op "&=") (%php-call 'cl-cc/php::%php-bitwise-and lhs rhs))
        ((equal op "|=") (%php-call 'cl-cc/php::%php-bitwise-or lhs rhs))
        ((equal op "^=") (%php-call 'cl-cc/php::%php-bitwise-xor lhs rhs))
        ((equal op "<<=") (%php-call 'cl-cc/php::%php-shift-left lhs rhs))
        ((equal op ">>=") (%php-call 'cl-cc/php::%php-shift-right lhs rhs))
        (t (error "PHP parse error: unsupported compound assignment operator ~S" op))))

(defun %php-nullish-cond (value)
  "Return a condition that is true when VALUE is Lisp NIL or PHP null."
  (make-ast-binop :op 'or
                  :lhs (%php-call 'null value)
                  :rhs (%php-call 'cl-cc/php::%php-null-p value)))

(defun %php-lower-compound-assign (op lhs-expr rhs-expr target-kind)
  "Build the lowered AST for PHP compound assignment OP on LHS-EXPR."
  (ecase target-kind
    (:var
     (let* ((var-sym (ast-var-name lhs-expr))
            (tmp (gensym "PHP-COMPOUND-LHS-"))
            (tmp-var (make-ast-var :name tmp)))
       (make-ast-let
        :bindings (list (cons tmp lhs-expr))
        :body (list (if (equal op "??=")
                        (make-ast-if
                         :cond (%php-nullish-cond tmp-var)
                         :then (make-ast-setq :var var-sym :value rhs-expr)
                         :else tmp-var)
                        (make-ast-setq
                         :var var-sym
                         :value (%php-compound-value op tmp-var rhs-expr)))))))
    (:array
     (destructuring-bind (array key) (ast-call-args lhs-expr)
       (let* ((array-sym (gensym "PHP-COMPOUND-ARRAY-"))
              (key-sym (gensym "PHP-COMPOUND-KEY-"))
              (array-var (make-ast-var :name array-sym))
              (key-var (make-ast-var :name key-sym))
              (current (%php-array-ref-call array-var key-var)))
         (make-ast-let
          :bindings (list (cons array-sym array)
                          (cons key-sym key))
          :body (list (if (equal op "??=")
                          (let* ((current-sym (gensym "PHP-COMPOUND-CURRENT-"))
                                 (current-var (make-ast-var :name current-sym)))
                            (make-ast-let
                             :bindings (list (cons current-sym current))
                             :body (list (make-ast-if
                                          :cond (%php-nullish-cond current-var)
                                          :then (%php-array-set-call array-var key-var rhs-expr)
                                          :else current-var))))
                          (%php-array-set-call
                           array-var key-var
                           (%php-compound-value op current rhs-expr))))))))
    (:property
     (let* ((object-sym (gensym "PHP-COMPOUND-OBJECT-"))
            (object-var (make-ast-var :name object-sym))
            (slot (ast-slot-value-slot lhs-expr))
            (current (make-ast-slot-value :object object-var :slot slot)))
       (make-ast-let
        :bindings (list (cons object-sym (ast-slot-value-object lhs-expr)))
        :body (list (if (equal op "??=")
                        (let* ((current-sym (gensym "PHP-COMPOUND-CURRENT-"))
                               (current-var (make-ast-var :name current-sym)))
                          (make-ast-let
                           :bindings (list (cons current-sym current))
                           :body (list (make-ast-if
                                        :cond (%php-nullish-cond current-var)
                                        :then (make-ast-set-slot-value
                                               :object object-var :slot slot :value rhs-expr)
                                        :else current-var))))
                        (make-ast-set-slot-value
                         :object object-var
                         :slot slot
                         :value (%php-compound-value op current rhs-expr)))))))))

(defun %php-double-arrow-p (stream)
  "Return true when STREAM begins with PHP double-arrow '=>'."
  (or (eq (php-peek-type stream) :T-DOUBLE-ARROW)
      (and (eq (php-peek-type stream) :T-OP)
           (equal "=>" (php-peek-value stream)))))

(defun %php-array-ref-call (array key)
  "Lower ARRAY[KEY] to the PHP ordered-array reference helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-ref)
                 :args (list array key)))

(defun %php-array-ref-call-p (node)
  "Return true when NODE is a %php-array-ref helper call."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-array-ref)))
       (= (length (ast-call-args node)) 2)))

(defun %php-array-set-call (array key value)
  "Lower ARRAY[KEY] = VALUE to the PHP ordered-array mutation helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-set)
                  :args (list array key value)))

(defun %php-array-unset-call (array key)
  "Lower unset(ARRAY[KEY]) to the PHP ordered-array deletion helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-unset)
                 :args (list array key)))

(defun %php-array-entry (key-present-p key value)
  "Build a runtime entry descriptor for %php-array."
  (make-ast-list :elements (list (make-ast-quote :value key-present-p)
                                 key
                                 value)))

(defun %php-array-call (entries)
  "Build the %php-array constructor call for ENTRIES."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array)
                 :args entries))

(defun %php-parse-array-expr (stream known-vars &key (open :T-LBRACKET) (close :T-RBRACKET))
  "Parse PHP short array [..] or legacy array(..) syntax."
  (multiple-value-bind (tok rest) (php-expect open stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) close)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values (%php-array-call nil) rest2 known-vars))
        (let ((entries nil)
              (current rest)
              (kv known-vars))
          (loop
            (multiple-value-bind (first-expr rest2 kv2) (php-parse-expr current kv)
              (if (%php-double-arrow-p rest2)
                  (multiple-value-bind (arrow-tok rest3) (php-consume rest2)
                    (declare (ignore arrow-tok))
                    (multiple-value-bind (value-expr rest4 kv4) (php-parse-expr rest3 kv2)
                      (push (%php-array-entry t first-expr value-expr) entries)
                      (setf current rest4 kv kv4)))
                  (progn
                    (push (%php-array-entry nil (make-ast-quote :value nil) first-expr) entries)
                    (setf current rest2 kv kv2))))
            (cond
              ((eq (php-peek-type current) :T-COMMA)
               (setf current (cdr current))
               (when (eq (php-peek-type current) close)
                 (return)))
              ((eq (php-peek-type current) close)
               (return))
              (t
               (error "PHP parse error: expected comma or ~S in array literal, got ~S"
                      close (php-peek current)))))
          (multiple-value-bind (tok2 rest2) (php-expect close current)
            (declare (ignore tok2))
            (values (%php-array-call (nreverse entries)) rest2 kv))))))

(defun %php-skip-expression-like (stream stop-types &optional stop-op-values)
  "Skip expression tokens until boundary."
  (let ((current stream) (depth 0))
    (loop
      (when (or (null current) (eq (php-peek-type current) :T-EOF)) (return current))
      (let ((type (php-peek-type current)))
        (when (and (zerop depth) (member type stop-types)) (return current))
        (case type
          ((:T-LPAREN :T-LBRACKET :T-LBRACE) (incf depth))
          ((:T-RPAREN :T-RBRACKET :T-RBRACE) (when (plusp depth) (decf depth)))))
      (setf current (cdr current)))))
