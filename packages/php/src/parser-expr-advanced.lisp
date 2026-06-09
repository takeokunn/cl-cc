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

(defun %php-spread-call-p (node)
  "Return true when NODE is a (...expr) spread-argument marker emitted by
php-parse-arglist."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-spread)))))

(defun %php-args-have-spread-p (args)
  "Return true when ARGS contains a spread marker."
  (some #'%php-spread-call-p args))

(defun %php-spread-arglist-expr (args)
  "Build a runtime list-of-arguments expression for a call with spreads: each
positional arg A becomes (list A) and each spread (...expr) becomes its array's
values list; the pieces are appended into one flat argument list for apply."
  (make-ast-call
   :func (make-ast-var :name 'append)
   :args (mapcar (lambda (a)
                   (if (%php-spread-call-p a)
                       (make-ast-call
                        :func (make-ast-var :name 'cl-cc/php::%php-array-values-list)
                        :args (list (first (ast-call-args a))))
                       (make-ast-call :func (make-ast-var :name 'list) :args (list a))))
                 args)))

(defun %php-call-with-spread (func-node args)
  "Build a PHP call of FUNC-NODE with ARGS, expanding any (...expr) spread args
via apply. FUNC-NODE is a value expression (e.g. a closure variable). When no
spread is present this is an ordinary ast-call."
  (if (%php-args-have-spread-p args)
      (make-ast-apply :func func-node :args (list (%php-spread-arglist-expr args)))
      (make-ast-call :func func-node :args args)))

(defun %php-helper-var (name)
  "Return an AST variable for a PHP runtime helper NAME."
  (make-ast-var :name name))

(defun %php-unsupported (message &optional expr)
  "Build a PHP unsupported-form marker call."
  (make-ast-call :func (make-ast-var :name '%php-unsupported)
                 :args (append (list (make-ast-quote :value message))
                               (when expr (list expr)))))

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

(defun %php-parse-function-call (qualified-name fallback-name stream known-vars)
  "Parse a PHP function call and lower known builtins to runtime helpers."
  (multiple-value-bind (args rest kv) (php-parse-arglist stream known-vars)
    (let ((fn-sym (or (and (%php-global-builtin-function-p
                            qualified-name fallback-name)
                           (%php-builtin-helper-symbol
                            (%php-simple-function-spelling qualified-name)))
                      fallback-name)))
      (values (if (%php-args-have-spread-p args)
                  ;; f(...$args): pass the function NAME symbol to apply so it
                  ;; resolves as a function, spreading the runtime argument list.
                  (make-ast-apply :func fn-sym
                                  :args (list (%php-spread-arglist-expr args)))
                  (make-ast-call :func (make-ast-var :name fn-sym) :args args))
              rest kv))))

;;; ─── Null-coalesce / Elvis Lowering ─────────────────────────────────────────

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
  (multiple-value-bind (params rest param-types _param-attrs _by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs _by-ref-indices))
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
            (multiple-value-bind (rest-param wrapped-body)
                (%php-variadic-rest-binding variadic-param (list body))
              (let* ((captures (%php-arrow-captures body params known-vars))
                     (lambda (make-ast-lambda :params required
                                              :optional-params optionals
                                              :rest-param rest-param
                                              :body wrapped-body)))
                (values (%php-capture-wrapper captures lambda) rest4 kv4)))))))))

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
  (multiple-value-bind (params rest param-types _param-attrs _by-ref-indices
                        param-defaults variadic-param)
      (php-parse-param-list stream)
    (declare (ignore param-types _param-attrs _by-ref-indices))
    (multiple-value-bind (captures by-ref rest2) (%php-parse-closure-use-list rest)
      (multiple-value-bind (return-type rest3) (php-parse-return-type rest2)
        (declare (ignore return-type))
        (multiple-value-bind (body-stmts rest4 kv4)
            (php-parse-block rest3 (append params captures
                                           (when variadic-param (list variadic-param))
                                           known-vars))
          ;; For by-ref captures, wrap them in ref boxes so mutations propagate.
          (multiple-value-bind (required optionals)
              (%php-split-params-by-defaults params param-defaults)
          (multiple-value-bind (rest-param wrapped-body)
              (%php-variadic-rest-binding variadic-param (%php-callable-body body-stmts))
          (let* ((ref-bindings
                  (when (> (hash-table-count by-ref) 0)
                    (remove nil
                            (mapcar (lambda (sym)
                                      (when (gethash sym by-ref)
                                        (cons sym (%php-call 'cl-cc/php::%php-make-ref
                                                             (make-ast-var :name sym)))))
                                    captures))))
                 (lambda-ast (make-ast-lambda :params required
                                              :optional-params optionals
                                              :rest-param rest-param
                                              :body wrapped-body))
                 (wrapped (if ref-bindings
                              (make-ast-let :bindings ref-bindings :body (list lambda-ast))
                              lambda-ast)))
            (values (%php-capture-wrapper captures wrapped) rest4 kv4)))))))))

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
;;; Data table maps PHP compound-assignment operator string to a 2-arg builder fn.
;;; Entries that lower to ast-binop use inline lambdas; entries that call PHP
;;; runtime helpers use %php-call.  The fallback signals an error.

(defparameter *php-compound-op-table*
  (list (cons "+="  (lambda (l r) (make-ast-binop :op '+ :lhs l :rhs r)))
        (cons "-="  (lambda (l r) (make-ast-binop :op '- :lhs l :rhs r)))
        (cons "*="  (lambda (l r) (make-ast-binop :op '* :lhs l :rhs r)))
        (cons "/="  (lambda (l r) (make-ast-binop :op '/ :lhs l :rhs r)))
        (cons ".="  (lambda (l r) (%php-call 'cl-cc/php::%php-concat l r)))
        (cons "%="  (lambda (l r) (%php-call 'cl-cc/php::%php-modulo l r)))
        (cons "**=" (lambda (l r) (%php-call 'expt l r)))
        (cons "&="  (lambda (l r) (%php-call 'cl-cc/php::%php-bitwise-and l r)))
        (cons "|="  (lambda (l r) (%php-call 'cl-cc/php::%php-bitwise-or l r)))
        (cons "^="  (lambda (l r) (%php-call 'cl-cc/php::%php-bitwise-xor l r)))
        (cons "<<=" (lambda (l r) (%php-call 'cl-cc/php::%php-shift-left l r)))
        (cons ">>=" (lambda (l r) (%php-call 'cl-cc/php::%php-shift-right l r))))
  "Alist mapping PHP compound-assignment operator strings to 2-arg value builders.")

(defun %php-compound-value (op lhs rhs)
  "Return the read-modify-write value for PHP compound assignment OP."
  (let ((entry (assoc op *php-compound-op-table* :test #'equal)))
    (if entry
        (funcall (cdr entry) lhs rhs)
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

(defun %php-lower-list-assign (lhs val)
  "Lower [$a, $b, ...] = VAL destructuring assignment. LHS is a %php-array
constructor call whose entry values are the assignment targets. Each target
binds to (%php-array-ref VAL index) in a single body-less let (consistent with
how plain $x = v introduces a binding). NIL holes ([, $b]) are skipped.
Re-references VAL per element, which is correct for the common variable RHS."
  (let ((bindings nil)
        (idx 0))
    (dolist (entry (ast-call-args lhs))
      ;; Each entry is (make-ast-list :elements (key-present-p key value)).
      (let* ((elts (and (ast-list-p entry) (ast-list-elements entry)))
             (target (third elts)))
        (when (and target (ast-var-p target))
          (push (cons (ast-var-name target)
                      (%php-array-ref-call val (make-ast-int :value idx)))
                bindings)))
      (incf idx))
    (make-ast-let :bindings (nreverse bindings) :body nil)))

(defun %php-array-set-call (array key value)
  "Lower ARRAY[KEY] = VALUE to the PHP ordered-array mutation helper."
  (make-ast-call :func (%php-helper-var 'cl-cc/php::%php-array-set)
                  :args (list array key value)))

(defun %php-array-append-call (array)
  "Lower ARRAY[] (empty subscript) to an append-target marker. This is only a
valid assignment LHS; the assignment parser turns `$a[] = v' into a push. Using []
to read is a PHP fatal error, modelled by the %php-array-append-target stub."
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
