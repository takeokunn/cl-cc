;;;; frontend/php/parser-expr-advanced.lisp — PHP Extended Expression Handlers
;;;;
;;;; Extracted from parser-expr.lisp at the "Extended Expression Handlers" boundary.
;;;; Contains helper/lowering functions invoked by the recursive-descent chain:
;;;;   %php-parse-keyword-expr  — clone/fn/match/yield/throw/array/list/function dispatch
;;;;   %php-null-quote, %php-call, %php-truthy-call  — tiny AST builders
;;;;   %php-string-token-ast    — interpolated-string lowering
;;;;   %php-parse-function-call / builtin resolution
;;;;   %php-lower-null-coalesce, %php-lower-elvis  — short-circuit lowering
;;;;   %php-parse-arrow-function, %php-parse-anonymous-function
;;;;   %php-parse-yield-expression
;;;;   %php-parse-match-expression / arm parsing / lowering
;;;;   %php-compound-value, %php-lower-compound-assign  — compound-assign lowering
;;;;   %php-array-* helpers, %php-parse-array-expr
;;;;   %php-skip-expression-like
;;;;
;;;; Depends on parser-expr.lisp for: php-parse-primary, php-parse-postfix,
;;;; php-parse-expr, php-parse-arglist (all loaded before this file).
;;;;
;;;; Load order: after parser-expr.lisp, before parser-stmt-lowering.lisp.
(in-package :cl-cc/php)

;;; ─── Tiny AST Builders ───────────────────────────────────────────────────────

(defun %php-null-quote ()
  "Return the AST representation of PHP null."
  (make-ast-quote :value +php-null+))

(defun %php-call (name &rest args)
  "Build an AST call to NAME with ARGS."
  (make-ast-call :func (make-ast-var :name name) :args args))

(defun %php-truthy-call (expr)
  "Build a PHP truthiness helper call for conditional contexts."
  (%php-call 'cl-cc/php::%php-truthy expr))

(defun %php-shadow-ref-vars (ref-vars bound-vars)
  "Remove BOUND-VARS from REF-VARS for recursive body rewriting."
  (set-difference ref-vars (mapcar #'%php-param-name bound-vars) :test #'eq))

(defun %php-rewrite-ref-vars (node-or-list ref-vars)
  "Rewrite reads/writes of REF-VARS so PHP by-reference locals use ref boxes."
  (labels ((walk-list (forms refs)
             (mapcar (lambda (form) (walk form refs)) forms))
           (walk-bindings (bindings refs)
             (mapcar (lambda (binding)
                       (cons (car binding) (walk (cdr binding) refs)))
                     bindings))
           (walk-initargs (initargs refs)
             (loop for (key . value) in initargs
                   collect (cons key (walk value refs))))
           (call (name &rest args)
             (apply #'%php-call name args))
           (ref-var-p (sym refs)
             (member sym refs :test #'eq))
           (walk (node refs)
             (cond
               ((null node) nil)
               ((listp node) (walk-list node refs))
               ((ast-var-p node)
                (let ((name (ast-var-name node)))
                  (if (ref-var-p name refs)
                      (call 'cl-cc/php::%php-deref node)
                      node)))
               ((ast-setq-p node)
                (let ((var (ast-setq-var node))
                      (value (walk (ast-setq-value node) refs)))
                  (if (ref-var-p var refs)
                      (call 'cl-cc/php::%php-ref-set! (make-ast-var :name var) value)
                      (make-ast-setq :var var :value value))))
               ((ast-binop-p node)
                (make-ast-binop :op (ast-binop-op node)
                                :lhs (walk (ast-binop-lhs node) refs)
                                :rhs (walk (ast-binop-rhs node) refs)))
               ((ast-if-p node)
                (make-ast-if :cond (walk (ast-if-cond node) refs)
                             :then (walk (ast-if-then node) refs)
                             :else (walk (ast-if-else node) refs)))
               ((ast-progn-p node)
                (make-ast-progn :forms (walk-list (ast-progn-forms node) refs)))
               ((ast-print-p node)
                (make-ast-print :expr (walk (ast-print-expr node) refs)))
               ((ast-let-p node)
                (let* ((bindings (ast-let-bindings node))
                       (ref-bindings (remove-if-not (lambda (binding)
                                                      (ref-var-p (car binding) refs))
                                                    bindings))
                       (plain-bindings (remove-if (lambda (binding)
                                                    (ref-var-p (car binding) refs))
                                                  bindings))
                       (body-refs (%php-shadow-ref-vars refs (mapcar #'car plain-bindings)))
                       (ref-sets (mapcar (lambda (binding)
                                           (call 'cl-cc/php::%php-ref-set!
                                                 (make-ast-var :name (car binding))
                                                 (walk (cdr binding) refs)))
                                         ref-bindings)))
                  (if ref-bindings
                      (make-ast-progn
                       :forms (append
                               ref-sets
                               (if plain-bindings
                                   (list (make-ast-let
                                          :bindings (walk-bindings plain-bindings refs)
                                          :declarations (ast-let-declarations node)
                                          :body (walk-list (ast-let-body node) body-refs)))
                                   (walk-list (ast-let-body node) refs))))
                      (make-ast-let :bindings (walk-bindings bindings refs)
                                    :declarations (ast-let-declarations node)
                                    :body (walk-list (ast-let-body node) body-refs)))))
               ((ast-lambda-p node)
                (let ((body-refs (%php-shadow-ref-vars
                                  refs
                                  (append (ast-lambda-params node)
                                          (ast-lambda-optional-params node)
                                          (ast-lambda-key-params node)
                                          (when (ast-lambda-rest-param node)
                                            (list (ast-lambda-rest-param node)))))))
                  (make-ast-lambda :params (ast-lambda-params node)
                                   :optional-params (ast-lambda-optional-params node)
                                   :rest-param (ast-lambda-rest-param node)
                                   :key-params (ast-lambda-key-params node)
                                   :declarations (ast-lambda-declarations node)
                                   :body (walk-list (ast-lambda-body node) body-refs)
                                   :env (ast-lambda-env node))))
               ((ast-defun-p node)
                (let ((body-refs (%php-shadow-ref-vars
                                  refs
                                  (append (ast-defun-params node)
                                          (ast-defun-optional-params node)
                                          (ast-defun-key-params node)
                                          (when (ast-defun-rest-param node)
                                            (list (ast-defun-rest-param node)))))))
                  (make-ast-defun :name (ast-defun-name node)
                                  :params (ast-defun-params node)
                                  :optional-params (ast-defun-optional-params node)
                                  :rest-param (ast-defun-rest-param node)
                                  :key-params (ast-defun-key-params node)
                                  :declarations (ast-defun-declarations node)
                                  :documentation (ast-defun-documentation node)
                                  :body (walk-list (ast-defun-body node) body-refs))))
               ((ast-call-p node)
                (make-ast-call :func (walk (ast-call-func node) refs)
                               :args (walk-list (ast-call-args node) refs)))
               ((ast-apply-p node)
                (make-ast-apply :func (walk (ast-apply-func node) refs)
                                :args (walk-list (ast-apply-args node) refs)))
               ((ast-block-p node)
                (make-ast-block :name (ast-block-name node)
                                :body (walk-list (ast-block-body node) refs)))
               ((ast-return-from-p node)
                (make-ast-return-from :name (ast-return-from-name node)
                                      :value (walk (ast-return-from-value node) refs)))
               ((ast-tagbody-p node)
                (make-ast-tagbody
                 :tags (mapcar (lambda (tag)
                                  (if (typep tag 'cl-cc/ast:ast-node)
                                      (walk tag refs)
                                      tag))
                                (ast-tagbody-tags node))))
               ((ast-values-p node)
                (make-ast-values :forms (walk-list (ast-values-forms node) refs)))
               ((ast-multiple-value-call-p node)
                (make-ast-multiple-value-call :func (walk (ast-mv-call-func node) refs)
                                              :args (walk-list (ast-mv-call-args node) refs)))
               ((ast-multiple-value-prog1-p node)
                (make-ast-multiple-value-prog1 :first (walk (ast-mv-prog1-first node) refs)
                                               :forms (walk-list (ast-mv-prog1-forms node) refs)))
               ((ast-multiple-value-bind-p node)
                (let ((body-refs (%php-shadow-ref-vars refs (ast-mvb-vars node))))
                  (make-ast-multiple-value-bind :vars (ast-mvb-vars node)
                                                :values-form (walk (ast-mvb-values-form node) refs)
                                                :body (walk-list (ast-mvb-body node) body-refs))))
               ((ast-catch-p node)
                (make-ast-catch :tag (walk (ast-catch-tag node) refs)
                                :body (walk-list (ast-catch-body node) refs)))
               ((ast-throw-p node)
                (make-ast-throw :tag (walk (ast-throw-tag node) refs)
                                :value (walk (ast-throw-value node) refs)))
               ((ast-unwind-protect-p node)
                (make-ast-unwind-protect :protected (walk (ast-unwind-protected node) refs)
                                         :cleanup (walk-list (ast-unwind-cleanup node) refs)))
               ((ast-handler-case-p node)
                (make-ast-handler-case :form (walk (ast-handler-case-form node) refs)
                                       :clauses (mapcar (lambda (clause)
                                                          (destructuring-bind (types var body) clause
                                                            (list types var
                                                                  (walk-list body
                                                                             (%php-shadow-ref-vars refs
                                                                                                    (when var (list var)))))))
                                                        (ast-handler-case-clauses node))))
               ((ast-list-p node)
                (make-ast-list :elements (walk-list (ast-list-elements node) refs)))
               ((ast-the-p node)
                (make-ast-the :type (ast-the-type node)
                              :value (walk (ast-the-value node) refs)))
               ((ast-make-instance-p node)
                (make-ast-make-instance :class (walk (ast-make-instance-class node) refs)
                                        :initargs (walk-initargs (ast-make-instance-initargs node) refs)))
               ((ast-slot-value-p node)
                (make-ast-slot-value :object (walk (ast-slot-value-object node) refs)
                                     :slot (ast-slot-value-slot node)))
               ((ast-set-slot-value-p node)
                (make-ast-set-slot-value :object (walk (ast-set-slot-value-object node) refs)
                                         :slot (ast-set-slot-value-slot node)
                                         :value (walk (ast-set-slot-value-value node) refs)))
               ((ast-set-gethash-p node)
                (make-ast-set-gethash :key (walk (ast-set-gethash-key node) refs)
                                      :table (walk (ast-set-gethash-table node) refs)
                                      :value (walk (ast-set-gethash-value node) refs)))
               (t node))))
    (walk node-or-list ref-vars)))

(defun %php-reference-marker (expr)
  "Mark a parsed &EXPR reference operand until assignment lowering sees it."
  (%php-call '%php-reference-marker expr))

(defun %php-reference-marker-p (node)
  "Return true when NODE is the internal &EXPR marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-reference-marker)))))

(defun %php-reference-marker-expr (node)
  "Return the referenced expression stored in a reference marker."
  (first (ast-call-args node)))

(defun %php-reference-assignment-marker (dest source)
  "Mark $DEST = &$SOURCE until statement-sequence lowering can create aliases."
  (%php-call '%php-reference-assignment-marker
             (make-ast-var :name dest)
             (make-ast-var :name source)))

(defun %php-reference-assignment-marker-p (node)
  "Return true when NODE is the internal reference-assignment marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-reference-assignment-marker)))))

(defun %php-reference-assignment-dest (node)
  "Return the destination symbol for a reference-assignment marker."
  (ast-var-name (first (ast-call-args node))))

(defun %php-reference-assignment-source (node)
  "Return the source symbol for a reference-assignment marker."
  (ast-var-name (second (ast-call-args node))))

(defun %php-variable-init-expression-marker (var value result)
  "Mark a PHP expression that first initializes VAR before yielding RESULT."
  (%php-call '%php-variable-init-expression-marker
             (make-ast-var :name var)
             value
             result))

(defun %php-variable-init-expression-marker-p (node)
  "Return true when NODE is the internal first-use variable init marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) '%php-variable-init-expression-marker)))))

(defun %php-variable-init-expression-var (node)
  "Return the variable symbol stored in a first-use init marker."
  (ast-var-name (first (ast-call-args node))))

(defun %php-variable-init-expression-value (node)
  "Return the initialization value stored in a first-use init marker."
  (second (ast-call-args node)))

(defun %php-variable-init-expression-result (node)
  "Return the expression result stored in a first-use init marker."
  (third (ast-call-args node)))

(defun %php-hoist-variable-init-expressions (node)
  "Rewrite first-use variable init markers and return hoisted VAR/VALUE pairs.

Markers can appear inside a larger expression, for example `echo (++$x).$x`.
The hoisted empty-body lets are emitted before the current statement so
php-finish-let-bindings scopes them over the rewritten statement and the rest of
the PHP block."
  (let ((bindings nil))
    (labels ((walk-list (forms)
               (mapcar #'walk forms))
             (walk-bindings (pairs)
               (mapcar (lambda (pair)
                         (cons (car pair) (walk (cdr pair))))
                       pairs))
             (walk-initargs (initargs)
               (loop for (key . value) in initargs
                     collect (cons key (walk value))))
             (walk (form)
               (cond
                 ((null form) nil)
                 ((listp form) (walk-list form))
                 ((%php-variable-init-expression-marker-p form)
                  (let ((value (walk (%php-variable-init-expression-value form))))
                    (push (cons (%php-variable-init-expression-var form) value)
                          bindings))
                  (walk (%php-variable-init-expression-result form)))
                 ((ast-binop-p form)
                  (make-ast-binop :op (ast-binop-op form)
                                  :lhs (walk (ast-binop-lhs form))
                                  :rhs (walk (ast-binop-rhs form))))
                 ((ast-if-p form)
                  (make-ast-if :cond (walk (ast-if-cond form))
                               :then (walk (ast-if-then form))
                               :else (walk (ast-if-else form))))
                 ((ast-progn-p form)
                  (make-ast-progn :forms (walk-list (ast-progn-forms form))))
                 ((ast-print-p form)
                  (make-ast-print :expr (walk (ast-print-expr form))))
                 ((ast-let-p form)
                  (make-ast-let :bindings (walk-bindings (ast-let-bindings form))
                                :declarations (ast-let-declarations form)
                                :body (walk-list (ast-let-body form))))
                 ((ast-setq-p form)
                  (make-ast-setq :var (ast-setq-var form)
                                 :value (walk (ast-setq-value form))))
                 ((ast-call-p form)
                  (make-ast-call :func (walk (ast-call-func form))
                                 :args (walk-list (ast-call-args form))))
                 ((ast-apply-p form)
                  (make-ast-apply :func (walk (ast-apply-func form))
                                  :args (walk-list (ast-apply-args form))))
                 ((ast-block-p form)
                  (make-ast-block :name (ast-block-name form)
                                  :body (walk-list (ast-block-body form))))
                 ((ast-return-from-p form)
                  (make-ast-return-from :name (ast-return-from-name form)
                                        :value (walk (ast-return-from-value form))))
                 ((ast-values-p form)
                  (make-ast-values :forms (walk-list (ast-values-forms form))))
                 ((ast-multiple-value-call-p form)
                  (make-ast-multiple-value-call :func (walk (ast-mv-call-func form))
                                                :args (walk-list (ast-mv-call-args form))))
                 ((ast-multiple-value-prog1-p form)
                  (make-ast-multiple-value-prog1 :first (walk (ast-mv-prog1-first form))
                                                 :forms (walk-list (ast-mv-prog1-forms form))))
                 ((ast-multiple-value-bind-p form)
                  (make-ast-multiple-value-bind :vars (ast-mvb-vars form)
                                                :values-form (walk (ast-mvb-values-form form))
                                                :body (walk-list (ast-mvb-body form))))
                 ((ast-catch-p form)
                  (make-ast-catch :tag (walk (ast-catch-tag form))
                                  :body (walk-list (ast-catch-body form))))
                 ((ast-throw-p form)
                  (make-ast-throw :tag (walk (ast-throw-tag form))
                                  :value (walk (ast-throw-value form))))
                 ((ast-unwind-protect-p form)
                  (make-ast-unwind-protect :protected (walk (ast-unwind-protected form))
                                           :cleanup (walk-list (ast-unwind-cleanup form))))
                 ((ast-handler-case-p form)
                  (make-ast-handler-case
                   :form (walk (ast-handler-case-form form))
                   :clauses (mapcar (lambda (clause)
                                      (destructuring-bind (types var body) clause
                                        (list types var (walk-list body))))
                                    (ast-handler-case-clauses form))))
                 ((ast-list-p form)
                  (make-ast-list :elements (walk-list (ast-list-elements form))))
                 ((ast-the-p form)
                  (make-ast-the :type (ast-the-type form)
                                :value (walk (ast-the-value form))))
                 ((ast-make-instance-p form)
                  (make-ast-make-instance :class (walk (ast-make-instance-class form))
                                          :initargs (walk-initargs
                                                     (ast-make-instance-initargs form))))
                 ((ast-slot-value-p form)
                  (make-ast-slot-value :object (walk (ast-slot-value-object form))
                                       :slot (ast-slot-value-slot form)))
                 ((ast-set-slot-value-p form)
                  (make-ast-set-slot-value :object (walk (ast-set-slot-value-object form))
                                           :slot (ast-set-slot-value-slot form)
                                           :value (walk (ast-set-slot-value-value form))))
                 ((ast-set-gethash-p form)
                  (make-ast-set-gethash :key (walk (ast-set-gethash-key form))
                                        :table (walk (ast-set-gethash-table form))
                                        :value (walk (ast-set-gethash-value form))))
                 (t form))))
      (values (walk node) (nreverse bindings)))))

(defun %php-bind-or-set (var value bound-vars)
  "Bind VAR to VALUE when new in this sequence, otherwise assign it."
  (if (member var bound-vars :test #'eq)
      (make-ast-setq :var var :value value)
      (make-ast-let :bindings (list (cons var value)) :body nil)))

(defun %php-form-bound-vars (form)
  "Return variables newly introduced by FORM at the current statement level."
  (if (and (ast-let-p form) (null (ast-let-body form)))
      (mapcar #'car (ast-let-bindings form))
      nil))

(defun %php-lower-reference-assignments (forms known-vars)
  "Lower $b = &$a aliases over a statement sequence using PHP ref boxes."
  (let ((out nil)
        (bound-vars (copy-list known-vars))
        (ref-vars nil))
    (dolist (form forms)
      (if (%php-reference-assignment-marker-p form)
          (let* ((dest (%php-reference-assignment-dest form))
                 (source (%php-reference-assignment-source form)))
            (unless (member source ref-vars :test #'eq)
              (push (%php-bind-or-set
                     source
                     (%php-call 'cl-cc/php::%php-make-ref
                                (if (member source bound-vars :test #'eq)
                                    (make-ast-var :name source)
                                    (%php-null-quote)))
                     bound-vars)
                    out)
              (pushnew source bound-vars :test #'eq)
              (pushnew source ref-vars :test #'eq))
            (push (%php-bind-or-set dest (make-ast-var :name source) bound-vars) out)
            (pushnew dest bound-vars :test #'eq)
            (pushnew dest ref-vars :test #'eq))
          (multiple-value-bind (hoisted-form init-bindings)
              (%php-hoist-variable-init-expressions form)
            (dolist (binding init-bindings)
              (push (%php-bind-or-set (car binding) (cdr binding) bound-vars) out)
              (pushnew (car binding) bound-vars :test #'eq))
            (let ((lowered (if ref-vars
                               (%php-rewrite-ref-vars hoisted-form ref-vars)
                               hoisted-form)))
            (dolist (var (%php-form-bound-vars hoisted-form))
              (pushnew var bound-vars :test #'eq))
              (push lowered out)))))
    (nreverse out)))

;;; ─── String / Exception Helpers ─────────────────────────────────────────────

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

(defun %php-parse-interp-expr (text)
  "Parse a complex-interpolation expression TEXT (e.g. $a[\"k\"], $o->x, $o->m())
into an AST node. The text is tokenized as a standalone PHP expression and parsed
with php-parse-expr; %php-concat stringifies the resulting value."
  (let ((stream (tokenize-php-source (concatenate 'string "<?php " text))))
    (values (php-parse-expr stream nil))))

(defun %php-string-token-ast (value)
  "Lower a lexer string VALUE to AST, preserving simple interpolation."
  (if (and (consp value) (eq (first value) :php-interpolated-string))
      (let ((args (mapcar (lambda (segment)
                            (ecase (first segment)
                              (:string (make-ast-quote :value (second segment)))
                              (:var (make-ast-var :name (php-var-sym (second segment))))
                              ;; {$expr} complex interpolation — array/prop access etc.
                              (:expr (%php-parse-interp-expr (second segment)))))
                          (second value))))
        (if args
            (make-ast-call :func (make-ast-var :name 'cl-cc/php::%php-concat)
                           :args args)
            (make-ast-quote :value "")))
      (make-ast-quote :value value)))

;;; ─── Function Call / Builtin Resolution ─────────────────────────────────────

(defun %php-builtin-helper-symbol (qualified-name)
  "Return the PHP runtime helper symbol for simple builtin QUALIFIED-NAME, or NIL.

Consults the central builtin registry (populated by %php-register-builtin) so
every registered builtin — array_*, str*, is_*, math, type predicates, … —
lowers to its cl-cc/php::%php- helper. Previously a hand-coded cond covered only
count/strlen/strtolower/strtoupper/isset/array_key_exists, so the other ~80
registered builtins fell back to an unbridged user symbol and hit `Undefined
function' at runtime (e.g. array_push)."
  (let ((lower (string-downcase qualified-name)))
    (unless (find (code-char 92) lower)
      (%php-lookup-builtin-symbol lower))))

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

(defun %php-fcc-marker-p (node)
  "True when NODE is the first-class-callable marker emitted for f(...)."
  (and (ast-call-p node)
       (let ((f (ast-call-func node)))
         (and (ast-var-p f) (eq (ast-var-name f) '%php-first-class-callable)))))

(defun %php-build-fcc-closure (fn-sym)
  "Build the PHP 8.1 first-class-callable closure for FN-SYM — fn(...$a) =>
FN-SYM(...$a) — a variadic closure that forwards all of its arguments."
  (let* ((args-sym (gensym "PHP-FCC-ARGS-"))
         (body (make-ast-apply
                :func fn-sym
                :args (list (make-ast-call
                             :func (make-ast-var :name 'cl-cc/php::%php-array-values-list)
                             :args (list (make-ast-var :name args-sym)))))))
    (multiple-value-bind (rest-param wrapped-body)
        (%php-variadic-rest-binding args-sym (list body))
      (make-ast-lambda :params nil :optional-params nil
                       :rest-param rest-param :body wrapped-body))))

(defun %php-preg-match-out-param-call-p (qualified-name args)
  "True when this is preg_match / preg_match_all with a $matches variable arg —
the call whose third argument is a by-reference out-parameter."
  (and (>= (length args) 3)
       (typep (third args) 'cl-cc/ast:ast-var)
       (let ((name (string-downcase (%php-strip-leading-namespace-separator qualified-name))))
         (or (string= name "preg_match") (string= name "preg_match_all")))))

(defun %php-lower-preg-match-out-param (qualified-name args)
  "Lower preg_match($p,$s,$m) / preg_match_all($p,$s,$m) so $matches is populated
in the caller's variable: assign $m = the matches array (a helper returns it BY
VALUE — the VM copies host structs across the bridge, so a ref box cannot
propagate), then yield the match count.  Pattern/subject and optional
flags/offset are bound to temps so each is evaluated once and the two helper
calls never share arg AST nodes."
  (let* ((name (string-downcase (%php-strip-leading-namespace-separator qualified-name)))
         (all-p (string= name "preg_match_all"))
         (matches-sym (if all-p 'cl-cc/php::%php-preg-match-all-matches
                          'cl-cc/php::%php-preg-match-matches))
         (count-sym (if all-p 'cl-cc/php::%php-preg-match-all '%php-preg-match))
         (matches-var (cl-cc/ast:ast-var-name (third args)))
         (pat-tmp (gensym "PHP-PREG-PAT-"))
         (subj-tmp (gensym "PHP-PREG-SUBJ-"))
         (flags-p (>= (length args) 4))
         (offset-p (>= (length args) 5))
         (flags-tmp (and flags-p (gensym "PHP-PREG-FLAGS-")))
         (offset-tmp (and offset-p (gensym "PHP-PREG-OFFSET-")))
         (count-tmp (gensym "PHP-PREG-COUNT-"))
         (bindings (append (list (cons pat-tmp (first args))
                                 (cons subj-tmp (second args)))
                           (when flags-p (list (cons flags-tmp (fourth args))))
                           (when offset-p (list (cons offset-tmp (fifth args))))))
         (count-args (cond
                       (offset-p (list (make-ast-var :name pat-tmp)
                                       (make-ast-var :name subj-tmp)
                                       (%php-null-quote)
                                       (if flags-p (make-ast-var :name flags-tmp) (make-ast-int :value 0))
                                       (make-ast-var :name offset-tmp)))
                       (flags-p (list (make-ast-var :name pat-tmp)
                                      (make-ast-var :name subj-tmp)
                                      (%php-null-quote)
                                      (make-ast-var :name flags-tmp)))
                       (t (list (make-ast-var :name pat-tmp)
                                (make-ast-var :name subj-tmp)))))
         (matches-args (cond
                         (offset-p (list (make-ast-var :name pat-tmp)
                                         (make-ast-var :name subj-tmp)
                                         (if flags-p (make-ast-var :name flags-tmp) (make-ast-int :value 0))
                                         (make-ast-var :name offset-tmp)))
                         (flags-p (list (make-ast-var :name pat-tmp)
                                        (make-ast-var :name subj-tmp)
                                        (make-ast-var :name flags-tmp)))
                         (t (list (make-ast-var :name pat-tmp)
                                  (make-ast-var :name subj-tmp))))))
    (make-ast-let
     :bindings bindings
     :body (list
            (make-ast-let
             :bindings (list (cons count-tmp
                                   (make-ast-call :func (make-ast-var :name count-sym)
                                                  :args count-args)))
             :body (list
                    (make-ast-setq
                     :var matches-var
                     :value (make-ast-call :func (make-ast-var :name matches-sym)
                                           :args matches-args))
                    (make-ast-var :name count-tmp)))))))

(defun %php-sscanf-out-param-call-p (qualified-name args)
  "True when this is sscanf($str,$fmt,$a,...) with variable out-params."
  (and (>= (length args) 3)
       (every (lambda (arg) (typep arg 'cl-cc/ast:ast-var)) (cddr args))
       (string= (string-downcase
                 (%php-strip-leading-namespace-separator qualified-name))
                "sscanf")))

(defun %php-scanf-out-param-call-p (qualified-name args)
  "True when this is scanf($fmt,$a,...) with variable out-params."
  (and (>= (length args) 2)
       (every (lambda (arg) (typep arg 'cl-cc/ast:ast-var)) (cdr args))
       (string= (string-downcase
                 (%php-strip-leading-namespace-separator qualified-name))
                "scanf")))

(defun %php-lower-scanf-like-out-param (values-helper helper-args out-args temp-prefix)
  "Lower scanf-family out-params into one parse, assignments, then count."
  (let ((values-tmp (gensym temp-prefix))
        (out-vars (mapcar #'cl-cc/ast:ast-var-name out-args)))
    (make-ast-let
     :bindings (list (cons values-tmp
                           (make-ast-call
                            :func (make-ast-var :name values-helper)
                            :args helper-args)))
     :body (append
            (loop for var in out-vars
                  for idx from 0
                  collect (make-ast-setq
                           :var var
                           :value (make-ast-call
                                   :func (make-ast-var :name 'cl-cc/php::%php-array-ref)
                                        :args (list (make-ast-var :name values-tmp)
                                                    (make-ast-quote :value idx)))))
            (list (make-ast-call
                   :func (make-ast-var :name 'cl-cc/php::%php-count)
                   :args (list (make-ast-var :name values-tmp))))))))

(defun %php-lower-sscanf-out-param (args)
  "Lower sscanf($str,$fmt,$a,...) into caller out-param assignments."
  (%php-lower-scanf-like-out-param 'cl-cc/php::%php-sscanf-values
                                   (list (first args) (second args))
                                   (cddr args)
                                   "PHP-SSCANF-VALUES-"))

(defun %php-lower-scanf-out-param (args)
  "Lower scanf($fmt,$a,...) into caller out-param assignments."
  (%php-lower-scanf-like-out-param 'cl-cc/php::%php-scanf-values
                                   (list (first args))
                                   (cdr args)
                                   "PHP-SCANF-VALUES-"))

(defun %php-by-ref-indices-for-name (name)
  "Return by-reference parameter indices registered for NAME."
  (when name
    (let ((key (princ-to-string name)))
      (or (gethash key *php-by-ref-param-registry*)
          (gethash (string-upcase key) *php-by-ref-param-registry*)
          (gethash (string-downcase key) *php-by-ref-param-registry*)))))

(defun %php-by-ref-indices-for-call (fn-sym)
  "Return by-reference parameter indices registered for the function FN-SYM."
  (%php-by-ref-indices-for-name (symbol-name fn-sym)))

(defun %php-by-ref-indices-for-function (qualified-name fn-sym fallback-name)
  "Return by-reference indices for a function before or after builtin lowering."
  (or (%php-by-ref-indices-for-call fn-sym)
      (%php-by-ref-indices-for-call fallback-name)
      (%php-by-ref-indices-for-name
       (%php-simple-function-spelling qualified-name))))

(defun %php-lower-by-ref-call (func-node args by-ref-indices)
  "Lower a call with by-reference parameters.

Variable actuals are boxed before the call and written back after the callee
returns. Non-variable actuals still receive a temporary ref box so the callee
can run, matching PHP's local mutation behavior for the callee body."
  (let ((box-bindings nil)
        (call-args nil)
        (writebacks nil))
    (loop for arg in args
          for idx from 0
          do (if (member idx by-ref-indices :test #'=)
                 (let ((box-sym (gensym "PHP-BY-REF-ARG-")))
                   (push (cons box-sym
                               (%php-call 'cl-cc/php::%php-make-ref arg))
                         box-bindings)
                   (push (make-ast-var :name box-sym) call-args)
                   (when (ast-var-p arg)
                     (push (make-ast-setq
                            :var (ast-var-name arg)
                            :value (%php-call 'cl-cc/php::%php-deref
                                              (make-ast-var :name box-sym)))
                           writebacks)))
                 (push arg call-args)))
    (let ((call-node (make-ast-call :func func-node
                                    :args (nreverse call-args))))
      (if box-bindings
          (let ((result-sym (gensym "PHP-BY-REF-RESULT-")))
            (make-ast-let
             :bindings (nreverse box-bindings)
             :body (list
                    (make-ast-let
                     :bindings (list (cons result-sym call-node))
                     :body (append (nreverse writebacks)
                                   (list (make-ast-var :name result-sym)))))))
          call-node))))

(defun %php-emit-lowered-function-call (fn-sym args by-ref-indices)
  "Emit a lowered PHP function call from ARGS."
  (let ((func (make-ast-var :name fn-sym)))
    (%php-emit-lowered-call func
                            (if (%php-args-have-named-p args)
                                (%php-reorder-named-args-for-call fn-sym args)
                                (%php-static-call-lowering-result args))
                            :by-ref-indices by-ref-indices)))

(defun %php-compact-static-names-from-node (node)
  "Return static compact() variable names described by NODE.
The second value is true when NODE was fully understood."
  (cond
    ((and (ast-quote-p node)
          (stringp (ast-quote-value node)))
     (values (list (ast-quote-value node)) t))
    ((%php-array-constructor-call-p node)
     (let ((names nil)
           (supported-p t))
       (dolist (entry (ast-call-args node))
         (let* ((elements (and (ast-list-p entry) (ast-list-elements entry)))
                (value (and (= (length elements) 3) (third elements))))
           (if value
               (multiple-value-bind (entry-names entry-supported-p)
                   (%php-compact-static-names-from-node value)
                 (if entry-supported-p
                     (setf names (append names entry-names))
                     (setf supported-p nil)))
               (setf supported-p nil))))
       (values names supported-p)))
    (t
     (values nil nil))))

(defun %php-compact-static-names (args)
  "Return all static variable names requested by a compact() call."
  (let ((names nil)
        (supported-p t))
    (dolist (arg args)
      (multiple-value-bind (arg-names arg-supported-p)
          (%php-compact-static-names-from-node arg)
        (if arg-supported-p
            (setf names (append names arg-names))
            (setf supported-p nil))))
    (values names supported-p)))

(defun %php-lower-static-compact-call (args known-vars)
  "Lower compact('x', ['y']) into an array literal capturing visible variables."
  (multiple-value-bind (names supported-p) (%php-compact-static-names args)
    (when supported-p
      (%php-array-call
       (loop for name in names
             for var-sym = (php-var-sym name)
             when (member var-sym known-vars :test #'eq)
               collect (%php-array-entry t
                                         (make-ast-quote :value name)
                                         (make-ast-var :name var-sym)))))))

(defun %php-lower-isset-call (args fn-sym)
  "Lower isset($x) so the variable operand is not evaluated before boundp."
  (when (= (length args) 1)
    (let ((arg (first args)))
      (when (ast-var-p arg)
        (make-ast-call :func (make-ast-var :name fn-sym)
                       :args (list (make-ast-quote :value (ast-var-name arg))))))))

(defun %php-lower-empty-call (args empty-fn-sym known-vars)
  "Lower empty($x) without evaluating an obviously unbound variable operand."
  (when (= (length args) 1)
    (let ((arg (first args)))
      (when (ast-var-p arg)
        (if (member (ast-var-name arg) known-vars :test #'eq)
            (make-ast-call :func (make-ast-var :name empty-fn-sym)
                           :args (list arg))
            (make-ast-quote :value t))))))

(defun %php-valid-extract-name-p (name)
  "Return true when NAME can become a PHP variable name."
  (and (stringp name)
       (plusp (length name))
       (let ((first (char name 0)))
         (or (alpha-char-p first) (char= first #\_)))
       (loop for ch across name
             always (or (alphanumericp ch) (char= ch #\_)))))

(defun %php-static-extract-bindings-from-node (node)
  "Return variable bindings for extract() when NODE is a static array literal."
  (when (%php-array-constructor-call-p node)
    (let ((bindings nil)
          (supported-p t))
      (dolist (entry (ast-call-args node))
        (let* ((elements (and (ast-list-p entry) (ast-list-elements entry)))
               (key-present-p (and (= (length elements) 3)
                                   (ast-quote-p (first elements))
                                   (ast-quote-value (first elements))))
               (key-node (and key-present-p (second elements)))
               (value-node (and (= (length elements) 3) (third elements))))
          (cond
            ((not (= (length elements) 3))
             (setf supported-p nil))
            ((not key-present-p)
             nil)
            ((and (ast-quote-p key-node)
                  (%php-valid-extract-name-p (ast-quote-value key-node)))
             (let* ((var-sym (php-var-sym (ast-quote-value key-node)))
                    (cell (assoc var-sym bindings :test #'eq)))
               (if cell
                   (setf (cdr cell) value-node)
                   (setf bindings (append bindings (list (cons var-sym value-node)))))))
            (t
             nil))))
      (when supported-p bindings))))

(defun %php-lower-static-extract-call (args)
  "Lower extract(['x' => 1]) into first-class PHP variable bindings."
  (when (= (length args) 1)
    (let ((bindings (%php-static-extract-bindings-from-node (first args))))
      (when bindings
        (make-ast-let :bindings bindings :body nil)))))

(defun %php-static-extract-bound-vars (args)
  "Return the variables introduced by a statically lowered extract() call."
  (let ((bindings (and (= (length args) 1)
                       (%php-static-extract-bindings-from-node (first args)))))
    (mapcar #'car bindings)))

(defun %php-global-builtin-function-name (qualified-name fallback-name)
  "Return the PHP builtin spelling for QUALIFIED-NAME when it is global."
  (when (%php-global-builtin-function-p qualified-name fallback-name)
    (%php-simple-function-spelling qualified-name)))

(defun %php-syntax-like-builtin-kind (builtin-name)
  "Classify syntax-like builtin names that need special lowering."
  (cond
    ((null builtin-name) nil)
    ((string= builtin-name "isset") :isset)
    ((string= builtin-name "empty") :empty)
    ((string= builtin-name "compact") :compact)
    ((string= builtin-name "extract") :extract)
    (t nil)))

(defun %php-parse-function-call (qualified-name fallback-name stream known-vars)
  "Parse a PHP function call and lower known builtins to runtime helpers."
  (multiple-value-bind (args rest kv) (php-parse-arglist stream known-vars)
    (let* ((builtin-name (%php-global-builtin-function-name qualified-name
                                                            fallback-name))
           (builtin-kind (%php-syntax-like-builtin-kind builtin-name))
           (fn-sym (or (and builtin-name
                            (%php-builtin-helper-symbol builtin-name))
                       fallback-name))
           (by-ref-indices
             (%php-by-ref-indices-for-function qualified-name fn-sym fallback-name)))
      (values (cond
                ;; preg_match($p,$s,$m): $matches is a by-reference out-param.
                ((%php-preg-match-out-param-call-p qualified-name args)
                 (%php-lower-preg-match-out-param qualified-name args))
                ;; sscanf($s,$fmt,$a,...): parse once, assign out-params, return count.
                ((%php-sscanf-out-param-call-p qualified-name args)
                 (%php-lower-sscanf-out-param args))
                ;; scanf($fmt,$a,...): read stdin once, assign out-params, return count.
                ((%php-scanf-out-param-call-p qualified-name args)
                 (%php-lower-scanf-out-param args))
                ;; isset($x) is PHP syntax: the variable must not be evaluated
                ;; before the runtime helper checks whether it is bound.
                ((eq builtin-kind :isset)
                 (or (%php-lower-isset-call args fn-sym)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; empty($x) is also syntax-like in PHP: an undefined variable
                ;; is empty and must not be evaluated before that decision.
                ((eq builtin-kind :empty)
                 (or (%php-lower-empty-call args fn-sym kv)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; compact('x', ['y']): requires caller variables, so static
                ;; name lists are lowered before the ordinary builtin path.
                ((eq builtin-kind :compact)
                 (or (%php-lower-static-compact-call args kv)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; extract(['x' => 1]) mutates the caller's variable table. For
                ;; static array literals, lower it into ordinary PHP bindings.
                ((eq builtin-kind :extract)
                 (or (%php-lower-static-extract-call args)
                     (make-ast-call :func (make-ast-var :name fn-sym)
                                    :args args)))
                ;; f(...): first-class callable — a forwarding closure, not a call.
                ((and (= (length args) 1) (%php-fcc-marker-p (first args)))
                 (%php-build-fcc-closure fn-sym))
                (t (%php-emit-lowered-function-call fn-sym
                                                   args
                                                   by-ref-indices)))
              rest
              (if (eq builtin-kind :extract)
                  (append (%php-static-extract-bound-vars args) kv)
                  kv)))))

;;; ─── Null-coalesce / Elvis Lowering ─────────────────────────────────────────

(defun %php-not-null-cond (expr)
  "Build (not (%php-null-p EXPR)) as an AST expression."
  (%php-call 'not (%php-call 'cl-cc/php::%php-null-p expr)))

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

;;; ─── Capture / Closure Helpers ───────────────────────────────────────────────

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
  (multiple-value-bind (params rest param-types _param-attrs by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs))
    (multiple-value-bind (return-type rest2) (php-parse-return-type rest)
      (declare (ignore return-type))
      (unless (and (eq (php-peek-type rest2) :T-OP)
                   (equal "=>" (php-peek-value rest2)))
        (error "PHP parse error: expected => after arrow function parameters"))
      (multiple-value-bind (arrow-token rest3) (php-consume rest2)
        (declare (ignore arrow-token))
        (multiple-value-bind (body rest4 kv4)
            (php-parse-expr rest3 (append params
                                          (when variadic-param (list variadic-param))
                                          known-vars))
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
            (let* ((by-ref-params (mapcar (lambda (idx) (nth idx params)) by-ref-indices))
                   (callable-body (if by-ref-params
                                      (%php-rewrite-ref-vars (list body) by-ref-params)
                                      (list body))))
              (multiple-value-bind (rest-param wrapped-body)
                  (%php-variadic-rest-binding variadic-param callable-body)
                (let* ((captures (%php-arrow-captures body params known-vars))
                       (lambda (make-ast-lambda
                                :params required
                                :optional-params optionals
                                :rest-param rest-param
                                :declarations (when by-ref-indices
                                                (list (list :php-by-ref-indices
                                                            by-ref-indices)))
                                :body wrapped-body)))
                  (values (%php-capture-wrapper captures lambda) rest4 kv4))))))))))

(defun %php-parse-closure-use-list (stream)
  "Parse optional PHP closure use($x, &$y) and return (captures by-ref-set rest).
Captures is a list of variable symbols; by-ref-set is a hash-table of the by-ref ones."
  (if (and (eq (php-peek-type stream) :T-KEYWORD)
           (eq (php-peek-value stream) :use))
      (let ((current (%php-consume-expected :T-LPAREN (cdr stream)))
            (captures nil)
            (by-ref (make-hash-table)))
        (unless (eq (php-peek-type current) :T-RPAREN)
          (loop
            ;; Detect &$var — by-reference capture
            (let* ((is-ref (%php-reference-token-p current))
                   (after-amp (if is-ref (cdr current) current)))
              (multiple-value-bind (var-token rest) (php-expect :T-VAR after-amp)
                (let ((var-sym (php-var-sym (php-tok-value var-token))))
                  (push var-sym captures)
                  (when is-ref
                    (setf (gethash var-sym by-ref) t))
                  (setf current rest))))
            (if (eq (php-peek-type current) :T-COMMA)
                (setf current (cdr current))
                (return))))
        (values (nreverse captures) by-ref (%php-consume-expected :T-RPAREN current)))
      (values nil (make-hash-table) stream)))

(defun %php-parse-anonymous-function (stream known-vars)
  "Parse function(params) use($x, &$y) { body } as an ast-lambda with captures.
By-reference captures (&$var) are wrapped in ref boxes so mutations propagate."
  (let* ((returns-by-ref (%php-reference-token-p stream))
         (stream (if returns-by-ref (cdr stream) stream)))
  (multiple-value-bind (params rest param-types _param-attrs by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs))
    (multiple-value-bind (captures by-ref rest2) (%php-parse-closure-use-list rest)
      (multiple-value-bind (return-type rest3) (php-parse-return-type rest2)
        (declare (ignore return-type))
        (multiple-value-bind (body-stmts rest4 kv4)
            (php-parse-block rest3 (append params captures
                                           (when variadic-param (list variadic-param))
                                           known-vars))
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
            (let* ((ref-captures (remove-if-not (lambda (sym) (gethash sym by-ref))
                                                captures))
                   (by-ref-params (mapcar (lambda (idx) (nth idx params)) by-ref-indices))
                   (ref-vars (remove-duplicates (append ref-captures by-ref-params)
                                                :test #'eq))
                   (callable-body (%php-callable-body body-stmts))
                   (callable-body (if ref-vars
                                      (%php-rewrite-ref-vars callable-body ref-vars)
                                      callable-body)))
              (multiple-value-bind (rest-param wrapped-body)
                  (%php-variadic-rest-binding variadic-param callable-body)
                (let* ((ref-bindings
                        (when (> (hash-table-count by-ref) 0)
                          (remove nil
                                  (mapcar (lambda (sym)
                                            (when (gethash sym by-ref)
                                              (cons sym
                                                    (%php-call 'cl-cc/php::%php-make-ref
                                                               (make-ast-var :name sym)))))
                                          captures))))
                       (lambda-ast
                        (make-ast-lambda
                         :params required
                         :optional-params optionals
                         :rest-param rest-param
                         :declarations (append (when by-ref-indices
                                                 (list (list :php-by-ref-indices
                                                             by-ref-indices)))
                                               (when returns-by-ref
                                                 (list (list :php-returns-by-ref t))))
                         :body wrapped-body))
                       (wrapped (if ref-bindings
                                    (make-ast-let :bindings ref-bindings
                                                  :body (list lambda-ast))
                                    lambda-ast)))
              (values (%php-capture-wrapper captures wrapped) rest4 kv4)))))))))))

;;; ─── Yield Handlers ─────────────────────────────────────────────────────────

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

;;; ─── Match Expression Lowering ───────────────────────────────────────────────

(defun %php-match-error-call ()
  "Return the fallback call used when no PHP match arm applies."
  (%php-call '%php-match-error))

(defun %php-build-match-condition (subject-sym tests)
  "Build the condition for one match arm: true when SUBJECT === any of TESTS.
Lowered as a short-circuit nested if rather than (ast-binop :op 'or ...) — codegen
has no emitter for an :or binop, so a multi-condition arm (1, 2 => …) silently
failed to compile and the whole match produced nothing."
  (labels ((eq-test (test)
             (%php-call 'cl-cc/php::%php-eq-strict
                        (make-ast-var :name subject-sym) test))
           (build (ts)
             ;; Single (or final) test: the bare strict-equality call, which is
             ;; already a boolean. Earlier tests chain via if so the arm matches
             ;; when SUBJECT === any test, without an :or binop.
             (if (cdr ts)
                 (make-ast-if :cond (eq-test (car ts))
                              :then (make-ast-quote :value t)
                              :else (build (cdr ts)))
                 (eq-test (car ts)))))
    (build tests)))

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

;;; ─── Compound Assignment Lowering ────────────────────────────────────────────
;;;
;;; Data table maps PHP compound-assignment operator strings to runtime helper
;;; symbols.  Lowering logic stays in %PHP-COMPOUND-VALUE.

(defparameter *php-compound-op-table*
  '(("+="  . cl-cc/php::%php-add)
    ("-="  . cl-cc/php::%php-sub)
    ("*="  . cl-cc/php::%php-mul)
    ("/="  . cl-cc/php::%php-div)
    (".="  . cl-cc/php::%php-concat)
    ("%="  . cl-cc/php::%php-modulo)
    ("**=" . expt)
    ("&="  . cl-cc/php::%php-bitwise-and)
    ("|="  . cl-cc/php::%php-bitwise-or)
    ("^="  . cl-cc/php::%php-bitwise-xor)
    ("<<=" . cl-cc/php::%php-shift-left)
    (">>=" . cl-cc/php::%php-shift-right))
  "Alist mapping PHP compound-assignment operator strings to runtime helper symbols.")

(defun %php-compound-value (op lhs rhs)
  "Return the read-modify-write value for PHP compound assignment OP."
  (let ((entry (assoc op *php-compound-op-table* :test #'equal)))
    (if entry
        (%php-call (cdr entry) lhs rhs)
        (error "PHP parse error: unsupported compound assignment operator ~S" op))))

(defun %php-nullish-cond (value)
  "Return a condition that is true when VALUE is PHP null — the inverse of
%php-not-null-cond.  Was (ast-binop :op 'or (null value) (%php-null-p value)):
codegen has no :or-binop emitter, so EVERY ??= (the only caller) failed to
compile and the whole program was dropped; and `(null value)' wrongly treated
PHP false as nullish.  A single `(eq value null)' fixes both."
  (%php-call 'eq value (%php-null-quote)))

(defun %php-compound-undefined-current (op)
  "AST for the value an UNDEFINED variable takes as the left operand of `$x OP= r'.
PHP treats the missing var as null, which coerces to 0 for arithmetic/bitwise
operators and to '' for the `.' (concat) operator."
  (cond
    ((equal op ".=") (make-ast-quote :value ""))
    (t (make-ast-int :value 0))))

(defun %php-lower-compound-assign (op lhs-expr rhs-expr target-kind &optional (var-known t))
  "Build the lowered AST for PHP compound assignment OP on LHS-EXPR.  VAR-KNOWN
applies to the :VAR kind: when NIL the variable does not yet exist, so it is
INTRODUCED (PHP treats the undefined left operand as null) rather than read +
setq'd — without this `$a += 3' (no prior $a) read an unbound $a and dropped the
whole program."
  (ecase target-kind
    (:var
     (let ((var-sym (ast-var-name lhs-expr)))
       (if var-known
           (let* ((tmp (gensym "PHP-COMPOUND-LHS-"))
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
                               :value (%php-compound-value op tmp-var rhs-expr))))))
           ;; Undefined var: introduce it. ??= on undefined (null) yields RHS; any
           ;; other op applies to the coerced-null left operand (0 or '').
           (make-ast-let
            :bindings (list (cons var-sym
                                  (if (equal op "??=")
                                      rhs-expr
                                      (%php-compound-value
                                       op (%php-compound-undefined-current op) rhs-expr))))
            :body nil))))
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

;;; ─── Array / Collection Helpers ─────────────────────────────────────────────

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

(defun %php-array-literal-call-p (node)
  "Return true when NODE is a %php-array constructor call (an array literal),
i.e. a valid destructuring-assignment target like [$a, $b] or list($a, $b)."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-array)))))

(defun %php-list-constructor-p (node)
  "True when NODE is a [..] / list(..) constructor (a %php-array call) — i.e. a
nested destructuring target."
  (and (ast-call-p node)
       (let ((f (ast-call-func node)))
         (and (ast-var-p f) (eq (ast-var-name f) 'cl-cc/php::%php-array)))))

(defun %php-collect-list-bindings (lhs val bindings)
  "Accumulate destructuring bindings for LHS (a %php-array constructor) pulling
from the VAL expression.  Honours `key => $var' entries (access by key) and
recurses into nested [..] / list(..) targets.  Returns the bindings list, with
later entries pushed on the front (caller nreverses)."
  (let ((idx 0))
    (dolist (entry (ast-call-args lhs) bindings)
      ;; Each entry is (make-ast-list :elements (key-present-p key value)).
      (let* ((elts (and (ast-list-p entry) (ast-list-elements entry)))
             (key-present (and elts (ast-quote-p (first elts)) (ast-quote-value (first elts))))
             (key (and elts (second elts)))
             (target (third elts))
             (access (if key-present
                         (%php-array-ref-call val key)
                         (%php-array-ref-call val (make-ast-int :value idx)))))
        (cond
          ((null target) nil)                     ; [ , $b] hole — skip
          ((ast-var-p target)
           (push (cons (ast-var-name target) access) bindings))
          ((%php-list-constructor-p target)        ; nested [$a,$b] / list(..)
           (setf bindings (%php-collect-list-bindings target access bindings)))))
      (incf idx))
    bindings))

(defun %php-lower-list-assign (lhs val)
  "Lower [$a, $b, ...] = VAL destructuring assignment. LHS is a %php-array
constructor call whose entry values are the assignment targets. Each target
binds to (%php-array-ref VAL <index-or-key>) in a single body-less let
(consistent with how plain $x = v introduces a binding). NIL holes ([, $b]) are
skipped; `key => $var' uses the key; nested [..] targets recurse. Re-references
VAL per element, which is correct for the common variable RHS."
  (make-ast-let
   :bindings (nreverse (%php-collect-list-bindings lhs val nil))
   :body nil))

(defun %php-array-set-call (array key value)
  "Lower ARRAY[KEY] = VALUE to the PHP ordered-array mutation helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-set)
                  :args (list array key value)))

(defun %php-array-append-call (array)
  "Lower ARRAY[] (empty subscript) to an append-target marker. This is only a
valid assignment LHS; the assignment parser turns `$a[] = v' into a push. Using []
to read is a PHP fatal error, represented here by the %php-array-append-target
marker."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-append-target)
                 :args (list array)))

(defun %php-array-append-call-p (node)
  "Return true when NODE is an ARRAY[] append-target marker."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-array-append-target)))))

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
  "Parse PHP array literals written as [..] or array(..)."
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
            ;; Spread element: [...$b] — splice another iterable's entries in.
            (if (eq (php-peek-type current) :T-ELLIPSIS)
                (multiple-value-bind (_tok rest-after) (php-consume current)
                  (declare (ignore _tok))
                  (multiple-value-bind (spread-expr rest2 kv2) (php-parse-expr rest-after kv)
                    (push (%php-array-entry nil (make-ast-quote :value nil)
                                            (make-ast-call :func (make-ast-var :name '%php-spread)
                                                           :args (list spread-expr)))
                          entries)
                    (setf current rest2 kv kv2)))
            (multiple-value-bind (first-expr rest2 kv2) (php-parse-expr current kv)
              (if (%php-double-arrow-p rest2)
                  (multiple-value-bind (arrow-tok rest3) (php-consume rest2)
                    (declare (ignore arrow-tok))
                    (multiple-value-bind (value-expr rest4 kv4) (php-parse-expr rest3 kv2)
                      (push (%php-array-entry t first-expr value-expr) entries)
                      (setf current rest4 kv kv4)))
                  (progn
                    (push (%php-array-entry nil (make-ast-quote :value nil) first-expr) entries)
                    (setf current rest2 kv kv2)))))
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

;;; ─── Skip Helper ─────────────────────────────────────────────────────────────

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

;;; ─── Keyword Expression Dispatcher ──────────────────────────────────────────
;;;
;;; This function is called from parser-expr.lisp (php-parse-primary) for
;;; keyword-led expressions.  It lives here because all its targets (%php-parse-
;;; arrow-function, %php-parse-match-expression, etc.) are defined in this file.

(defun %php-clone-expression-ast (expr &optional overrides)
  "Lower a PHP clone expression, optionally applying PHP 8.5 clone-with overrides."
  (let ((clone-sym (gensym "PHP-CLONE-")))
    (make-ast-let
     :bindings (list (cons clone-sym
                           (make-ast-call
                            :func (make-ast-var :name 'cl-cc/php::%php-clone)
                            :args (list expr))))
     :body (append
            (list
             (make-ast-if
              :cond (make-ast-call
                     :func (make-ast-var :name 'cl-cc/php::%php-has-method)
                     :args (list (make-ast-var :name clone-sym)
                                 (make-ast-quote :value (php-ident-sym "__clone"))))
              :then (make-ast-call
                     :func (make-ast-slot-value
                            :object (make-ast-var :name clone-sym)
                            :slot (php-ident-sym "__clone"))
                     :args (list (make-ast-var :name clone-sym)))
              :else (make-ast-quote :value nil)))
            (when overrides
              (list
               (make-ast-call
                :func (make-ast-var :name 'cl-cc/php::%php-clone-with)
                :args (list (make-ast-var :name clone-sym) overrides))))
            (list (make-ast-var :name clone-sym))))))

(defun %php-parse-keyword-expr (stream kw known-vars)
  "Dispatch keyword-led expression to the appropriate handler."
  (multiple-value-bind (tok rest) (php-consume stream)
    (declare (ignore tok))
    (case kw
      (:clone
       (if (and (eq (php-peek-type rest) :T-LPAREN)
                (eq (php-peek-type (cdr rest)) :T-ELLIPSIS))
           (values (make-ast-quote :value nil)
                   (cdddr rest) known-vars)
           (if (eq (php-peek-type rest) :T-LPAREN)
               (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
                 (unless (= (length args) 2)
                   (error "PHP parse error: clone-with expects object and override array"))
                 (values (%php-clone-expression-ast (first args) (second args)) rest2 kv2))
               (multiple-value-bind (expr rest2 kv2) (php-parse-unary rest known-vars)
                 (values (%php-clone-expression-ast expr) rest2 kv2)))))
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
       ;; list($a, $b) = expr is destructuring assignment. Parse it to the SAME
       ;; array-literal node that the short form [$a, $b] produces, so it flows
       ;; through the existing assignment-target path (%php-array-literal-call-p ->
       ;; %php-lower-list-assign). The old %php-list-bind call node was not a
       ;; recognized assignment target -> "unsupported assignment target".
       (if (eq (php-peek-type rest) :T-LPAREN)
           (%php-parse-array-expr rest known-vars :open :T-LPAREN :close :T-RPAREN)
           (values (make-ast-quote :value nil) rest known-vars)))
      (:function
       (%php-parse-anonymous-function rest known-vars)))))
