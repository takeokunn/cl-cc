;;;; frontend/php/parser-expr.lisp — PHP Expression Parser
;;;;
;;;; Extracted from parser.lisp.
;;;; Contains expression-level recursive descent parser functions:
;;;;   php-parse-primary  — literals, variables, identifiers, parens, new
;;;;   php-parse-new      — 'new ClassName(args)'
;;;;   php-parse-postfix  — method calls, property access, ++ / --
;;;;   php-parse-unary    — !, -, +
;;;;   php-parse-binop    — left-associative binary operator parsing
;;;;   php-parse-mul/add/cmp/and/or — precedence chain
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
         (values (make-ast-quote :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-VAR)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-var :name (php-var-sym (php-tok-value tok))) rest known-vars)))
      ((eq type :T-KEYWORD)
       (let ((kw (php-peek-value stream)))
         (cond
           ((eq kw :null)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value nil) rest known-vars)))
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
      ((eq type :T-IDENT)
       ;; Could be a function call or identifier
       (multiple-value-bind (tok rest) (php-consume stream)
         (let ((name (php-ident-sym (php-tok-value tok))))
           (if (eq (php-peek-type rest) :T-LPAREN)
               ;; Function call
               (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
                 (values (make-ast-call :func (make-ast-var :name name) :args args)
                         rest2 kv2))
               (values (make-ast-var :name name) rest known-vars)))))
      (t (error "PHP parse error: unexpected token ~S in expression" (php-peek stream))))))

(defun php-parse-new (stream known-vars)
  "Parse 'new ClassName(args)'."
  (multiple-value-bind (tok rest) (php-consume stream) ; consume 'new'
    (declare (ignore tok))
    (multiple-value-bind (class-tok rest2) (php-expect :T-IDENT rest)
      (let ((class-name (php-ident-sym (php-tok-value class-tok))))
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
                 (setf obj (make-ast-call :func (make-ast-var :name 'aref)
                                           :args (list obj idx))
                       rest rest4
                       kv kv3)))))
          (t (return)))))
    (values obj rest kv)))

(defun php-parse-unary (stream known-vars)
  "Parse unary expressions: !, -, +."
  (if (and (eq (php-peek-type stream) :T-OP)
           (member (php-peek-value stream) '("!" "-" "+") :test #'equal))
      (multiple-value-bind (tok rest) (php-consume stream)
        (multiple-value-bind (expr rest2 kv2) (php-parse-postfix rest known-vars)
          (values (make-ast-call :func (make-ast-var :name (intern (php-tok-value tok)))
                                 :args (list expr))
                  rest2 kv2)))
      (php-parse-postfix stream known-vars)))

(defun php-parse-binop (stream known-vars ops next-parser)
  "Left-associative binary operator parsing."
  (multiple-value-bind (lhs rest kv) (funcall next-parser stream known-vars)
    (loop
      (if (and (eq (php-peek-type rest) :T-OP)
               (member (php-peek-value rest) ops :test #'equal))
          (multiple-value-bind (op-tok rest2) (php-consume rest)
            (multiple-value-bind (rhs rest3 kv3) (funcall next-parser rest2 kv)
              (setf lhs (make-ast-binop
                         :op (intern (php-tok-value op-tok))
                         :lhs lhs
                         :rhs rhs)
                    rest rest3
                    kv kv3)))
          (return)))
    (values lhs rest kv)))

(defun php-parse-mul (stream known-vars)
  (php-parse-binop stream known-vars '("*" "/") #'php-parse-unary))

(defun php-parse-add (stream known-vars)
  (php-parse-binop stream known-vars '("+" "-" ".") #'php-parse-mul))

(defun php-parse-cmp (stream known-vars)
  (php-parse-binop stream known-vars '("==" "===" "!=" "!==" "<" ">" "<=" ">=")
                   #'php-parse-add))

(defun php-parse-coalesce (stream known-vars)
  "Parse null coalescing ?? and ternary ?: operators."
  (multiple-value-bind (lhs rest kv) (php-parse-cmp stream known-vars)
    (loop
      (let ((type (php-peek-type rest)))
        (cond
          ((and (eq type :T-OP) (equal "??" (php-peek-value rest)))
           (multiple-value-bind (op-tok rest2) (php-consume rest)
             (declare (ignore op-tok))
             (multiple-value-bind (rhs rest3 kv3) (php-parse-cmp rest2 kv)
               (setf lhs (make-ast-if
                          :cond (make-ast-binop :op '= :lhs lhs
                                                 :rhs (make-ast-quote :value nil))
                          :then rhs
                          :else lhs)
                     rest rest3
                     kv kv3))))
          (t (return)))))
    (values lhs rest kv)))

(defun php-parse-and (stream known-vars)
  (php-parse-binop stream known-vars '("&&") #'php-parse-coalesce))

(defun php-parse-or (stream known-vars)
  (php-parse-binop stream known-vars '("||") #'php-parse-and))

(defun php-parse-expr (stream known-vars)
  "Parse an expression. Handles assignment $var = expr."
  (if (and (eq (php-peek-type stream) :T-VAR)
           (let ((rest (cdr stream)))
             (and rest (eq (php-peek-type rest) :T-OP)
                  (equal "=" (php-peek-value rest)))))
      ;; Assignment: $var = expr
      (multiple-value-bind (var-tok rest) (php-consume stream)
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (val rest3 kv3) (php-parse-or rest2 known-vars)
            (let* ((var-sym (php-var-sym (php-tok-value var-tok)))
                   (already-known (member var-sym known-vars))
                   (new-kv (if already-known kv3 (cons var-sym kv3))))
              (values
               (if already-known
                   (make-ast-setq :var var-sym :value val)
                   (make-ast-let :bindings (list (cons var-sym val)) :body nil))
               rest3
               new-kv)))))
      (php-parse-or stream known-vars)))

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
       (values (make-ast-call :func (make-ast-var :name 'fn) :args nil)
               (%php-skip-expression-like rest '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF))
               known-vars))
      (:match
       (values (make-ast-call :func (make-ast-var :name 'match) :args nil)
               (%php-skip-expression-like rest '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF))
               known-vars))
      (:yield
       (values (make-ast-call :func (make-ast-var :name 'yield) :args nil)
               (%php-skip-expression-like rest '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF))
               known-vars))
      (:throw
       (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
         (values (make-ast-throw :tag (make-ast-quote :value 'php-exception)
                                  :value expr)
                 rest2 kv2)))
      (:array
       (if (eq (php-peek-type rest) :T-LPAREN)
           (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
             (values (make-ast-call :func (make-ast-var :name 'make-array) :args args)
                     rest2 kv2))
           (values (make-ast-quote :value nil) rest known-vars)))
      (:list
       (if (eq (php-peek-type rest) :T-LPAREN)
           (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
             (values (make-ast-call :func (make-ast-var :name '%php-list-bind) :args args)
                     rest2 kv2))
           (values (make-ast-quote :value nil) rest known-vars)))
      (:function
       (values (make-ast-call :func (make-ast-var :name 'function) :args nil)
               (%php-skip-expression-like rest '(:T-SEMI :T-COMMA :T-RPAREN :T-RBRACE :T-EOF))
               known-vars)))))

(defun %php-parse-array-expr (stream known-vars)
  "Parse short array syntax [expr, ...]."
  (multiple-value-bind (tok rest) (php-consume stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RBRACKET)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values (make-ast-call :func (make-ast-var :name 'make-array) :args nil)
                  rest2 known-vars))
        (let ((items nil) (current rest) (kv known-vars))
          (loop
            (multiple-value-bind (val rest2 kv2) (php-parse-expr current kv)
              (push val items) (setf current rest2 kv kv2))
            (when (and current (eq (php-peek-type current) :T-COMMA))
              (setf current (cdr current)))
            (when (eq (php-peek-type current) :T-RBRACKET) (return)))
          (multiple-value-bind (tok2 rest2) (php-consume current)
            (declare (ignore tok2))
            (values (make-ast-call :func (make-ast-var :name 'make-array)
                                    :args (nreverse items))
                    rest2 kv))))))

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
