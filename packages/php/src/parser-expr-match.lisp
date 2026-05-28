;;;; frontend/php/parser-expr-match.lisp — PHP Match Expression Parser
;;;;
;;;; Contains match expression parsing and lowering.
;;;; Loaded after parser-stmt.lisp because match lowering uses
;;;; %php-consume-expected and other statement-level helpers.
(in-package :cl-cc/php)

(defun %php-match-error-call ()
  "Return the fallback call used when no PHP match arm applies."
  (%php-call '%php-match-error))

(defun %php-build-match-condition (subject-sym tests)
  "Build an OR of strict-equality tests for one match arm."
  (let ((comparisons
          (mapcar (lambda (test)
                    (%php-call 'equal (make-ast-var :name subject-sym) test))
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
