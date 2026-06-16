(in-package :cl-cc/php)

(declaim (ftype function %php-call))

(defun %php-assignment-op (stream)
  "Return the PHP assignment operator string at STREAM, or NIL."
  (when (eq (php-peek-type stream) :T-OP)
    (let ((op (php-peek-value stream)))
      (when (member op '("=" "+=" "-=" "*=" "/=" ".=" "%=" "**="
                         "&=" "|=" "^=" "<<=" ">>=" "??=")
                    :test #'equal)
        op))))

(defun %php-assignment-op-p (stream)
  "Return true when STREAM begins with a PHP assignment operator."
  (not (null (%php-assignment-op stream))))

(defun %php-reference-token-p (stream)
  "Return true when STREAM begins with PHP's single ampersand token."
  (and (eq (php-peek-type stream) :T-OP)
       (equal "&" (php-peek-value stream))))

(defstruct (%php-call-lowering-result (:conc-name %php-call-lowering-result-))
  static-args
  runtime-arglist-expr
  runtime-p)

(defun %php-call-lowering-result-args (lowering-result &optional receiver-arg)
  "Return lowered call arguments for LOWERING-RESULT.

The first value is T when the call must stay dynamic and evaluate a runtime
arglist expression. The second value is the runtime arglist expression or the
final static argument list. RECEIVER-ARG, when non-NIL, is prepended for method
calls."
  (if (%php-call-lowering-result-runtime-p lowering-result)
      (let ((runtime-arglist-expr
              (%php-call-lowering-result-runtime-arglist-expr lowering-result)))
        (values t (if receiver-arg
                      (%php-call 'append
                                 (%php-call 'list receiver-arg)
                                 runtime-arglist-expr)
                      runtime-arglist-expr)))
      (let ((static-args (%php-call-lowering-result-static-args lowering-result)))
        (values nil (if receiver-arg
                        (cons receiver-arg static-args)
                        static-args)))))

(defun %php-static-call-lowering-result (static-args)
  "Create a lowering result that already has a static argument list."
  (make-%php-call-lowering-result :static-args static-args))

(defun %php-runtime-call-lowering-result (runtime-arglist-expr)
  "Create a lowering result that must be evaluated at runtime."
  (make-%php-call-lowering-result
   :runtime-arglist-expr runtime-arglist-expr
   :runtime-p t))

(defun %php-emit-lowered-call (func lowering-result &key receiver-arg by-ref-indices)
  "Emit an AST call form from LOWERING-RESULT.

When the lowering result stays dynamic we preserve the runtime arglist and use
`ast-apply'. Otherwise we emit a normal call unless the lowered argument list
still contains a spread or the call needs by-reference lowering."
  (multiple-value-bind (runtime-p call-args)
      (%php-call-lowering-result-args lowering-result receiver-arg)
    (cond
      (runtime-p
       (make-ast-apply :func func :args (list call-args)))
      ((%php-args-have-spread-p call-args)
       (make-ast-apply :func func
                       :args (list (%php-spread-arglist-expr call-args))))
      (by-ref-indices
       (%php-lower-by-ref-call func call-args by-ref-indices))
      (t
       (make-ast-call :func func :args call-args)))))

(defun %php-yield-call-p (node)
  "T when NODE is a lowered PHP yield / yield-from call (see %php-parse-yield-
expression, which lowers `yield' to a %php-yield / %php-yield-from call)."
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (member (ast-var-name func)
                      '(cl-cc/php::%php-yield cl-cc/php::%php-yield-from)
                      :test #'eq)))))

(defun %php-body-contains-yield-p (node)
  "T when NODE (an AST node or a list of nodes) contains a yield call in the
CURRENT function scope. Does not descend into nested ast-lambda / ast-defun
bodies: a nested closure's yield makes that closure its own generator, not the
enclosing function."
  (cond
    ((null node) nil)
    ((listp node) (some #'%php-body-contains-yield-p node))
    ((%php-yield-call-p node) t)
    ((or (typep node 'ast-lambda) (typep node 'ast-defun)) nil)
    (t (some #'%php-body-contains-yield-p (ast-children node)))))

(defun %php-callable-body (body-stmts)
  "Wrap a PHP function/method/closure BODY-STMTS list in (block nil ...).

PHP `return' lowers to (return-from nil ...), but a bare ast-defun/ast-lambda
establishes no block named NIL. Without this wrapper the body fails to compile,
the top-level handler-case silently drops the whole defun, and every call then
hits `Undefined function'. Returns a one-element body list; a NIL/empty body
(abstract method signature) is returned unchanged so it stays body-less.

When the body contains a `yield' (lowered to %php-yield calls), the callable is a
PHP generator: calling it must RETURN a generator object rather than run the body
eagerly. We wrap the (block nil ...) in a zero-arg lambda handed to
%php-make-generator, which runs the body under *current-generator* and collects
yielded values. PHP `return' (return-from nil) then becomes the generator's
return value."
  (if body-stmts
      (let ((block (make-ast-block :name nil :body body-stmts)))
        (if (%php-body-contains-yield-p body-stmts)
            ;; Generator. Thread the generator object explicitly as a VM value
            ;; rather than via a host thunk: %php-generator-enter creates it and
            ;; pushes it on the VM-global active-generator stack, the body's
            ;; %php-yield calls (bridged to host) read the top of that stack via
            ;; *vm-current-state*, and %php-generator-exit pops + finalizes it.
            ;; This keeps every host call value-in/value-out — no host->VM
            ;; callback — and the let returns the generator object.
            (let ((gen-sym (gensym "PHP-GEN-")))
              (list (make-ast-let
                     :bindings (list (cons gen-sym
                                           (make-ast-call
                                            :func (make-ast-var :name 'cl-cc/php::%php-generator-enter)
                                            :args nil)))
                     :body (list (make-ast-call
                                  :func (make-ast-var :name 'cl-cc/php::%php-generator-exit)
                                  :args (list (make-ast-var :name gen-sym) block))
                                 (make-ast-var :name gen-sym)))))
            (list block)))
      body-stmts))
