;;;; packages/php/src/parser-stmt-decls.lisp — PHP Parser: declaration structures and registered statement parsers

(in-package :cl-cc/php)

;;; ─── Catch type parsing ──────────────────────────────────────────────────

(defun %php-catch-type-token-p (token)
  "Return true when TOKEN can appear as a PHP catch class name."
  (and token
       (member (php-tok-type token) '(:T-IDENT :T-TYPE :T-KEYWORD) :test #'eq)))

(defun %php-parse-catch-type-list (stream)
  "Parse a PHP catch type or union type list, returning (values type stream)."
  (let ((types nil)
        (current stream))
    (unless (or (eq (php-peek-type current) :T-BACKSLASH)
                (%php-catch-type-token-p (php-peek current)))
      (error "PHP parse error: expected catch type, got ~S" (php-peek current)))
    (loop
      (multiple-value-bind (catch-name after-name) (php-parse-qualified-name current)
        (push (php-ident-sym (php-resolve-qualified-name catch-name :class)) types)
        (setf current after-name))
      (if (and (eq (php-peek-type current) :T-OP)
               (equal (php-peek-value current) "|"))
          (progn
            (setf current (cdr current))
            (unless (or (eq (php-peek-type current) :T-BACKSLASH)
                        (%php-catch-type-token-p (php-peek current)))
              (error "PHP parse error: expected catch type after |, got ~S" (php-peek current))))
          (return)))
    (let ((ordered (nreverse types)))
      (values (if (rest ordered) ordered (first ordered)) current))))

;;; ─── Enum support helpers ────────────────────────────────────────────────

(defun %php-enum-backing-type (type)
  "Return normalized PHP enum backing TYPE metadata."
  (cond ((null type) nil)
        ((string= type "int") :int)
        ((string= type "string") :string)
        (t (error "PHP parse error: enum backing type must be int or string, got ~S" type))))

(defun %php-parse-enum-backing-type (stream)
  "Parse optional : int|string enum backing type."
  (if (eq (php-peek-type stream) :T-COLON)
      (multiple-value-bind (type rest) (php-parse-type-annotation (cdr stream))
        (values (%php-enum-backing-type type) rest))
      (values nil stream)))

(defun %php-enum-case-initform (class-name case-name value)
  "Build an AST initform creating one PHP enum case singleton."
  (%php-call 'cl-cc/php::%php-enum-make-case
             (make-ast-quote :value class-name)
             (make-ast-quote :value case-name)
             (or value (make-ast-quote :value +php-null+))))

(defun %php-parse-enum-case (stream known-vars class-name enum-type &optional attributes)
  "Parse case NAME [= VALUE]; in an enum body."
  (declare (ignore enum-type))
  (let ((current (cdr stream)))
    (multiple-value-bind (name-tok rest) (php-expect :T-IDENT current)
      (let* ((case-name (php-ident-sym (php-tok-value name-tok)))
             (value nil)
             (current rest))
        (when (%php-assignment-op-p current)
          (multiple-value-bind (expr rest2 kv2) (php-parse-expr (cdr current) known-vars)
            (declare (ignore kv2))
            (setf value expr
                  current rest2)))
        (values (make-ast-slot-def :name case-name
                                   :type enum-type
                                   :initform (%php-enum-case-initform class-name case-name value)
                                    :allocation :class
                                    :imports (list :php-enum-case t
                                                   :php-enum-value value
                                                   :php-attributes (mapcar (lambda (attribute)
                                                                             (setf (php-attribute-target-type attribute) :constant)
                                                                             attribute)
                                                                           attributes)))
                (php-skip-semis current)
                (list :name case-name :value value))))))

;;; ─── Class-like parsing (trait/interface/enum) ───────────────────────────

(defun %php-parse-classlike (stream known-vars &key enum-p kind)
  "Parse trait/interface/enum as ast-defclass-style declarations."
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT stream)
    (let* ((class-name (php-ident-sym
                        (php-resolve-qualified-name
                         (php-tok-value name-tok) :class)))
           (current rest)
           (slots nil)
           (enum-cases nil)
           (enum-type nil)
           (supers nil))
      (when enum-p
        (multiple-value-bind (parsed-type after-type)
            (%php-parse-enum-backing-type current)
          (setf enum-type parsed-type
                current after-type)))
      (multiple-value-bind (parsed-supers after-supers)
          (%php-parse-class-superclasses current)
        (setf supers parsed-supers
              current after-supers))
      (setf current (%php-consume-expected :T-LBRACE current))
      (loop
        (setf current (php-skip-semis current))
        (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (attributes after-attributes) (%php-parse-attributes current)
          (setf current after-attributes)
        (if (and enum-p (%php-keyword-p current :case))
            (multiple-value-bind (slot rest2 case-meta)
                (%php-parse-enum-case current known-vars class-name enum-type attributes)
              (push slot slots)
              (push case-meta enum-cases)
              (setf current rest2))
            (multiple-value-bind (slot rest2) (%php-parse-class-body-member current known-vars)
              (when slot
                ;; Enum methods are stored CLASS-allocated (on the enum class
                ;; object) so $case->method() resolves through the case's
                ;; __class__ link; they remain instance methods — $this (= the
                ;; case) is still prepended because they are not `static'.
                (when (and enum-p (ast-slot-def-p slot)
                           (ast-defun-p (ast-slot-initform slot)))
                  (setf (ast-slot-allocation slot) :class))
                (push slot slots))
              (setf current rest2)))))
      (let* ((slots-rev (nreverse slots))
             (defclass (make-ast-defclass :name class-name
                                          :superclasses supers
                                          :slots slots-rev
                                          :php-kind (or kind (and enum-p :enum))
                                          :php-enum-type enum-type
                                          :php-enum-cases (nreverse enum-cases))))
        ;; Register trait methods for compile-time application (mirrors parser-trait.lisp).
        (when (eq kind :trait)
          (setf (gethash (string-upcase (symbol-name class-name)) *php-trait-registry*)
                slots-rev))
        (values
         ;; For enums, follow the class def with a call that links each case
         ;; singleton to the class (__class__) so method dispatch works.
         (if enum-p
             (make-ast-progn
              :forms (list defclass
                           (%php-call 'cl-cc/php::%php-enum-finalize
                                      (make-ast-var :name class-name))))
             defclass)
         (%php-consume-expected :T-RBRACE current)
         known-vars)))))

;;; ─── Use/import parsing helpers ──────────────────────────────────────────

(defun %php-use-kind (stream)
  "Return use kind and stream after optional function/const qualifier."
  (cond ((%php-keyword-p stream :function) (values :function (cdr stream)))
        ((%php-keyword-p stream :const) (values :const (cdr stream)))
        (t (values :class stream))))

(defun %php-optional-use-kind (stream)
  "Return optional group-use kind qualifier and stream after it."
  (cond ((%php-keyword-p stream :function) (values :function (cdr stream)))
        ((%php-keyword-p stream :const) (values :const (cdr stream)))
        (t (values nil stream))))

(defun %php-parse-use-alias (stream)
  "Parse optional `as Alias` and return alias/rest."
  (if (%php-keyword-p stream :as)
      (multiple-value-bind (alias-name rest) (php-parse-qualified-name (cdr stream))
        (values alias-name rest))
      (values nil stream)))

(defun %php-make-import (kind name alias)
  "Build a PHP import descriptor plist."
  (list :type kind :name name :alias alias))

(defun %php-join-qualified-prefix (prefix name)
  "Join import PREFIX and NAME with a PHP namespace separator."
  (cond ((or (null prefix) (string= prefix "")) name)
        ((or (null name) (string= name "")) prefix)
        ((char= (char prefix (1- (length prefix))) #\\) (concatenate 'string prefix name))
        (t (format nil "~A\\~A" prefix name))))

(defun %php-parse-group-use-items (stream prefix default-kind)
  "Parse `{A, B as C}` group-use items after PREFIX."
  (let ((current (%php-consume-expected :T-LBRACE stream))
        (imports nil))
    (loop
      (multiple-value-bind (kind rest) (%php-optional-use-kind current)
        (multiple-value-bind (name rest2) (php-parse-qualified-name rest)
          (multiple-value-bind (alias rest3) (%php-parse-use-alias rest2)
            (push (%php-make-import (or kind default-kind)
                                    (%php-join-qualified-prefix prefix name)
                                    alias)
                  imports)
            (setf current rest3))))
      (cond ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current))
             (when (eq (php-peek-type current) :T-RBRACE) (return)))
            (t (return))))
    (values (nreverse imports) (%php-consume-expected :T-RBRACE current))))

(defun %php-parse-use-imports (stream kind)
  "Parse one PHP use statement body and return import descriptors/rest."
  (let ((current stream)
        (imports nil))
    (loop
      (multiple-value-bind (name rest) (php-parse-qualified-name current)
        (cond
          ((and (eq (php-peek-type rest) :T-BACKSLASH)
                (eq (php-peek-type (cdr rest)) :T-LBRACE))
           (multiple-value-bind (group-imports rest2)
               (%php-parse-group-use-items (cdr rest) name kind)
             (setf imports (append imports group-imports)
                   current rest2)))
          ((eq (php-peek-type rest) :T-LBRACE)
           (multiple-value-bind (group-imports rest2)
               (%php-parse-group-use-items rest name kind)
             (setf imports (append imports group-imports)
                   current rest2)))
          (t
           (multiple-value-bind (alias rest2) (%php-parse-use-alias rest)
             (setf imports (append imports (list (%php-make-import kind name alias))))
             (setf current rest2)))))
      (cond ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current)))
            (t (return))))
    (values imports
            (%php-consume-expected :T-SEMI current))))

;;; ─── Registered statement parsers ───────────────────────────────────────

(define-php-stmt-parser :echo (rest known-vars)
  ;; Echo supports comma-separated expressions: echo 1, 2, 3;
  ;; Each value is converted with PHP's string semantics (true -> "1", false/null
  ;; -> "", arrays -> "Array", numbers/strings as-is) via %php-concat, which calls
  ;; %php-stringify per argument. Without this echo went straight to ast-print's
  ;; generic VM printer, so a boolean (e.g. the result of == / a comparison)
  ;; printed as "T"/"NIL" instead of PHP's "1"/"".
  (let ((exprs nil) (current rest) (kv known-vars))
    (loop
      (multiple-value-bind (expr rest2 kv2) (php-parse-expr current kv)
        (push expr exprs)
        (setf current rest2 kv kv2))
      (unless (and current (eq (php-peek-type current) :T-COMMA))
        (return))
      (setf current (cdr current)))
    ;; echo emits its arguments verbatim with NO trailing newline.  ast-print
    ;; lowers to vm-print ("~A~%"), which appended a spurious newline after
    ;; every echo; route through princ (vm-princ, no newline/no escaping)
    ;; instead.  The builtin registry keys on symbol-name ("PRINC"), so this
    ;; reuses the proven CL princ -> vm-princ codegen path.
    (values (%php-call 'princ
                       (apply #'%php-call 'cl-cc/php::%php-concat (nreverse exprs)))
            (php-skip-semis current) kv)))

(define-php-stmt-parser :return (rest known-vars)
  (if (eq (php-peek-type rest) :T-SEMI)
      (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
              (php-skip-semis rest) known-vars)
      (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
        (values (make-ast-return-from :name nil :value expr) (php-skip-semis rest2) kv2))))

(define-php-stmt-parser :if (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (multiple-value-bind (then-stmts rest3 kv3 else-ast) (%php-parse-if-tail rest2 kv2)
      (values (make-ast-if :cond (%php-truthy-call cond-expr)
                           :then (make-ast-progn :forms then-stmts)
                           :else else-ast)
              rest3 kv3))))

(define-php-stmt-parser :while (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (let* ((*php-loop-continue-target* (gensym "WHILE-LOOP-"))
           (*php-loop-break-target* (gensym "WHILE-END-"))
           (*php-break-targets* (cons *php-loop-break-target* *php-break-targets*))
           (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
      (multiple-value-bind (body-stmts rest3 kv3) (%php-parse-statement-body rest2 kv2)
        (values (%php-lower-while-with-label
                 cond-expr
                  body-stmts
                 *php-loop-continue-target*)
                rest3 kv3)))))

(define-php-stmt-parser :for (rest known-vars)
  (let ((rest (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (init rest kv)
        (if (eq (php-peek-type rest) :T-SEMI)
            (values (make-ast-quote :value nil) rest known-vars)
            (php-parse-expr rest known-vars))
      (let ((rest (%php-consume-expected :T-SEMI rest)))
        (multiple-value-bind (cond-expr rest kv)
            (if (eq (php-peek-type rest) :T-SEMI)
                (values (make-ast-quote :value t) rest kv)
                (php-parse-expr rest kv))
          (let ((rest (%php-consume-expected :T-SEMI rest)))
            (multiple-value-bind (incr rest kv)
                (if (eq (php-peek-type rest) :T-RPAREN)
                    (values (make-ast-quote :value nil) rest kv)
                    (php-parse-expr rest kv))
              (let* ((rest (%php-consume-expected :T-RPAREN rest))
                      (*php-loop-continue-target* (gensym "FOR-LOOP-"))
                      (*php-loop-break-target* (gensym "FOR-END-"))
                      (*php-break-targets* (cons *php-loop-break-target* *php-break-targets*))
                      (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
                (multiple-value-bind (body-stmts rest _) (%php-parse-statement-body rest kv)
                  (declare (ignore _))
                  ;; The init (e.g. $i = 1) lowers to an empty-bodied ast-let when
                  ;; it introduces a new variable; nest the loop inside that let
                  ;; (via php-finish-let-bindings) so the loop var is visible to
                  ;; the condition, body and increment. Otherwise it stays a progn.
                  (values (make-ast-progn
                           :forms (php-finish-let-bindings
                                   (list init (%php-lower-for-with-labels
                                               cond-expr
                                               body-stmts
                                               incr
                                               *php-loop-continue-target*))))
                          rest kv))))))))))

(defun %php-lower-foreach-by-ref (arr-expr var-sym body-stmts loop-tag &optional key-sym)
  "Lower 'foreach ($arr as &$val)' — BODY-FN receives a ref box; writes back."
  (let ((box-sym (gensym "FOREACH-BOX-"))
        (key-param (gensym "FOREACH-KEY-")))
    (%php-call 'cl-cc/php::%php-foreach-by-ref
               arr-expr
               (make-ast-lambda
                :params (list box-sym key-param)
                :body (list (make-ast-let
                             :bindings (append
                                        (when key-sym (list (cons key-sym (make-ast-var :name key-param))))
                                        (list (cons var-sym (make-ast-var :name box-sym))))
                             :body (list (%php-lower-while-with-label
                                          (make-ast-quote :value t)
                                          body-stmts
                                          loop-tag))))))))

(define-php-stmt-parser :foreach (rest known-vars)
  (let ((rest2 (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (arr-expr rest3 kv3) (php-parse-expr rest2 known-vars)
      (let ((rest4 (if (%php-keyword-p rest3 :as) (cdr rest3) (error "foreach: expected 'as'"))))
        ;; Detect by-reference on the value position: foreach ($arr as &$val)
        (let* ((by-ref-p (%php-reference-token-p rest4))
               (rest4b (if by-ref-p (cdr rest4) rest4)))
          (multiple-value-bind (var-tok rest5) (php-expect :T-VAR rest4b)
            (let ((key-sym nil)
                  (var-sym (php-var-sym (php-tok-value var-tok)))
                  (val-by-ref-p by-ref-p)
                  (rest6 rest5))
              ;; foreach ($arr as $key => &$val) or ($arr as $key => $val)
              (when (and rest6 (eq (php-peek-type rest6) :T-OP) (equal "=>" (php-peek-value rest6)))
                (let* ((after-arrow (cdr rest6))
                       (val-ref-p (%php-reference-token-p after-arrow))
                       (after-ref (if val-ref-p (cdr after-arrow) after-arrow)))
                  (multiple-value-bind (val-tok rest7) (php-expect :T-VAR after-ref)
                    (setf key-sym var-sym
                          var-sym (php-var-sym (php-tok-value val-tok))
                          val-by-ref-p val-ref-p
                          rest6 rest7))))
              (let* ((*php-loop-continue-target* (gensym "FOREACH-LOOP-"))
                     (*php-loop-break-target* (gensym "FOREACH-END-"))
                     (*php-break-targets* (cons *php-loop-break-target* *php-break-targets*))
                     (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
                (multiple-value-bind (body-stmts rest8 kv8)
                    (%php-parse-statement-body (%php-consume-expected :T-RPAREN rest6) kv3)
                  (values (if val-by-ref-p
                              (%php-lower-foreach-by-ref
                               arr-expr var-sym body-stmts
                               *php-loop-continue-target* key-sym)
                              (%php-lower-foreach-with-label
                               arr-expr var-sym body-stmts
                               *php-loop-continue-target* key-sym))
                          rest8 kv8))))))))))

(define-php-stmt-parser :unset (rest known-vars)
  (let ((current (%php-consume-expected :T-LPAREN rest))
        (forms nil)
        (kv known-vars))
    (unless (eq (php-peek-type current) :T-RPAREN)
      (loop
        (multiple-value-bind (target rest2 kv2) (php-parse-expr current kv)
          (cond
            ((%php-array-ref-call-p target)
             (destructuring-bind (array key) (ast-call-args target)
               (push (%php-array-unset-call array key) forms)))
            ((ast-var-p target)
             (push (make-ast-setq :var (ast-var-name target)
                                  :value (make-ast-quote :value +php-null+))
                   forms)
             (setf kv (remove (ast-var-name target) kv2 :test #'eq)))
            (t
             (error "PHP parse error: unsupported unset target ~S" target)))
          (setf current rest2)
          (cond
            ((eq (php-peek-type current) :T-COMMA)
             (setf current (cdr current)))
            ((eq (php-peek-type current) :T-RPAREN)
             (return))
            (t
             (error "PHP parse error: expected comma or ) in unset, got ~S"
                    (php-peek current)))))))
    (setf current (%php-consume-expected :T-RPAREN current))
    (values (if (rest forms)
                (make-ast-progn :forms (nreverse forms))
                (or (first forms) (make-ast-quote :value +php-null+)))
            (php-skip-semis current)
            kv)))

(define-php-stmt-parser :do (rest known-vars)
  (let* ((*php-loop-continue-target* (gensym "DO-WHILE-LOOP-"))
         (*php-loop-break-target* (gensym "DO-WHILE-END-"))
         (*php-break-targets* (cons *php-loop-break-target* *php-break-targets*))
         (*php-continue-targets* (cons *php-loop-continue-target* *php-continue-targets*)))
    (multiple-value-bind (body-stmts rest2 kv2) (%php-parse-statement-body rest known-vars)
      (unless (%php-keyword-p rest2 :while)
        (error "PHP parse error: expected while after do body"))
      (multiple-value-bind (cond-expr rest3 kv3) (%php-parse-paren-expr (cdr rest2) kv2)
        (values (%php-lower-do-while-with-label
                 cond-expr
                  body-stmts
                 *php-loop-continue-target*)
                (php-skip-semis rest3) kv3)))))

(define-php-stmt-parser :switch (rest known-vars)
  (multiple-value-bind (switch-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (let* ((break-tag (gensym "SWITCH-END-"))
           (*php-loop-break-target* break-tag)
           (*php-break-targets* (cons break-tag *php-break-targets*)))
      (multiple-value-bind (cases default-body rest3 kv3) (%php-parse-switch-body rest2 kv2 break-tag)
        (values (php-lower-switch switch-expr cases default-body break-tag) rest3 kv3)))))

(define-php-stmt-parser :try (rest known-vars)
  (multiple-value-bind (try-body rest2 kv2) (php-parse-block rest known-vars)
    (let ((clauses nil) (finally-body nil) (current rest2) (kv kv2))
      (loop while (%php-keyword-p current :catch)
            do (setf current (%php-consume-expected :T-LPAREN (cdr current)))
               (multiple-value-bind (type-sym after-types)
                   (%php-parse-catch-type-list current)
                 (setf current after-types)
                 (let ((var-sym nil))
                   (when (eq (php-peek-type current) :T-VAR)
                     (setf var-sym (php-var-sym (php-peek-value current))
                           current (cdr current)))
                   (setf current (%php-consume-expected :T-RPAREN current))
                   (multiple-value-bind (catch-body rest3 kv3) (php-parse-block current kv)
                     (push (list* type-sym var-sym catch-body) clauses)
                     (setf current rest3 kv kv3)))))
      (when (%php-keyword-p current :finally)
        (multiple-value-bind (body rest3 kv3) (php-parse-block (cdr current) kv)
          (setf finally-body body current rest3 kv kv3)))
      (let ((protected (%php-lower-try-catches try-body (nreverse clauses))))
        (values (make-ast-unwind-protect :protected protected :cleanup finally-body)
                current kv)))))

(define-php-stmt-parser :continue (rest known-vars)
  (multiple-value-bind (level rest2) (%php-parse-control-level rest)
    (let* ((target (%php-continue-target level))
           (current (if (eq (php-peek-type rest2) :T-SEMI) rest2 (%php-skip-to-stmt-end rest2))))
      (unless target
        (error "PHP parse error: continue~@[ ~D~] has no matching loop" level))
      (values (make-ast-go :tag target)
              (php-skip-semis current) known-vars))))

(define-php-stmt-parser :break (rest known-vars)
  (multiple-value-bind (level rest2) (%php-parse-control-level rest)
    (let* ((target (%php-break-target level))
           (current (if (eq (php-peek-type rest2) :T-SEMI) rest2 (%php-skip-to-stmt-end rest2))))
      (unless target
        (error "PHP parse error: break~@[ ~D~] has no matching loop or switch" level))
      (values (make-ast-go :tag target)
              (php-skip-semis current) known-vars))))

(define-php-stmt-parser :throw (rest known-vars)
  (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
    (values (make-ast-throw :tag (make-ast-quote :value 'php-exception)
                            :value (%php-exception-payload-call expr))
            (php-skip-semis rest2) kv2)))

(defun %php-parse-include-like (rest known-vars name)
  (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
    (values (make-ast-call :func (make-ast-var :name name) :args (list expr))
            (php-skip-semis rest2) kv2)))

(define-php-stmt-parser :include (rest known-vars) (%php-parse-include-like rest known-vars 'include))
(define-php-stmt-parser :require (rest known-vars) (%php-parse-include-like rest known-vars 'require))
(define-php-stmt-parser :include-once (rest known-vars) (%php-parse-include-like rest known-vars 'include-once))
(define-php-stmt-parser :require-once (rest known-vars) (%php-parse-include-like rest known-vars 'require-once))

(define-php-stmt-parser :declare (rest known-vars)
  (values (make-ast-progn :forms nil) (%php-skip-to-stmt-end rest) known-vars))

(define-php-stmt-parser :goto (rest known-vars)
  (multiple-value-bind (label-tok rest2) (php-expect :T-IDENT rest)
    (values (make-ast-go :tag (php-ident-sym (php-tok-value label-tok)))
            (php-skip-semis rest2) known-vars)))

(define-php-stmt-parser :namespace (rest known-vars)
  (multiple-value-bind (namespace-name after-name)
      (if (eq (php-peek-type rest) :T-LBRACE)
          (values nil rest)
          (php-parse-qualified-name rest))
    (cond
      ((eq (php-peek-type after-name) :T-LBRACE)
       (multiple-value-bind (forms rest2 kv2)
           (%php-parse-namespace-block-body after-name known-vars namespace-name)
         (setf *php-pending-top-level-forms* forms)
         (values nil rest2 kv2)))
      ((eq (php-peek-type after-name) :T-SEMI)
       (setf *php-current-namespace* namespace-name
             *php-current-imports* nil)
       (values nil (php-skip-semis after-name) known-vars))
      (t (error "PHP parse error: expected { or ; after namespace declaration, got ~S"
                (php-peek after-name))))))

(define-php-stmt-parser :use (rest known-vars)
  (multiple-value-bind (kind use-rest) (%php-use-kind rest)
    (multiple-value-bind (imports rest2) (%php-parse-use-imports use-rest kind)
      (setf *php-current-imports* (append *php-current-imports* imports))
      (values nil rest2 known-vars))))

(define-php-stmt-parser :trait (rest known-vars) (%php-parse-classlike rest known-vars :kind :trait))
(define-php-stmt-parser :interface (rest known-vars) (%php-parse-classlike rest known-vars :kind :interface))
(define-php-stmt-parser :enum (rest known-vars) (%php-parse-classlike rest known-vars :enum-p t))

(define-php-stmt-parser :function (rest known-vars)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((fn-name (php-ident-sym
                    (php-resolve-qualified-name (php-tok-value name-tok) :function))))
      (multiple-value-bind (params rest param-types param-attributes by-ref-indices
                            param-defaults variadic-param)
          (php-parse-param-list rest)
        (multiple-value-bind (return-type rest) (php-parse-return-type rest)
          ;; Register by-reference parameter info for call-site lowering
          (when by-ref-indices
            (setf (gethash (symbol-name fn-name) *php-by-ref-param-registry*)
                  by-ref-indices))
          ;; Parameters with `= default` become &optional params so a call that
          ;; omits them binds the default instead of leaving them unset.
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
            ;; Abstract / interface method signature: `function f(...): T;` with no
            ;; body. Produce a body-less ast-defun (a signature) instead of
            ;; requiring a brace block.
            (if (eq (php-peek-type rest) :T-SEMI)
                (values (make-ast-defun :name fn-name
                                         :params required
                                         :optional-params optionals
                                         :declarations (%php-function-declarations
                                                        param-types return-type param-attributes nil :function)
                                         :body nil)
                        (php-skip-semis rest) known-vars)
                (multiple-value-bind (body-stmts rest _)
                    (php-parse-block rest (append params
                                                  (when variadic-param (list variadic-param))
                                                  known-vars))
                  (declare (ignore _))
                  (multiple-value-bind (rest-param wrapped-body)
                      (%php-variadic-rest-binding variadic-param (%php-callable-body body-stmts))
                    (values (make-ast-defun :name fn-name
                                             :params required
                                             :optional-params optionals
                                             :rest-param rest-param
                                             :declarations (%php-function-declarations
                                                            param-types return-type param-attributes nil :function)
                                             :body wrapped-body)
                            rest known-vars))))))))))
