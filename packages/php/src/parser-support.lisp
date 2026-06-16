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

(defun %php-helper-var (name)
  (intern (string-upcase name) (find-package '#:cl-cc/php)))

(defun %php-unsupported (message)
  (error "Unsupported PHP parse form: ~A" message))

(defvar *php-by-ref-param-registry* (make-hash-table :test #'equal))

(defparameter *php-named-param-registry* (make-hash-table :test #'equal))

(defun %php-param-name (param)
  (etypecase param
    (symbol (symbol-name param))
    (string param)))

(defun %php-param-string-name (param)
  (string-downcase (%php-param-name param)))

(defun %php-spread-call-p (node)
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-spread)))))

(defun %php-args-have-spread-p (args)
  (some #'%php-spread-call-p args))

(defun %php-spread-arglist-expr (args)
  (make-ast-call
   :func (make-ast-var :name 'append)
   :args (mapcar (lambda (a)
                   (if (%php-spread-call-p a)
                       (make-ast-call
                        :func (make-ast-var :name 'cl-cc/php::%php-array-values-list)
                        :args (list (first (ast-call-args a))))
                       (make-ast-call :func (make-ast-var :name 'list)
                                      :args (list a))))
                 args)))

(defun %php-callable-by-ref-indices (node)
  (cond
    ((ast-lambda-p node)
     (loop for decl in (ast-lambda-declarations node)
           when (and (consp decl)
                     (eq (first decl) :php-by-ref-indices))
             return (second decl)))
    ((and (ast-let-p node)
          (consp (ast-let-body node))
          (null (cdr (ast-let-body node))))
     (%php-callable-by-ref-indices (first (ast-let-body node))))
    (t nil)))

(defun %php-strip-callable-by-ref-metadata (node)
  (cond
    ((ast-lambda-p node)
     (make-ast-lambda
      :params (ast-lambda-params node)
      :optional-params (ast-lambda-optional-params node)
      :rest-param (ast-lambda-rest-param node)
      :key-params (ast-lambda-key-params node)
      :declarations (remove-if (lambda (decl)
                                 (and (consp decl)
                                      (eq (first decl) :php-by-ref-indices)))
                               (ast-lambda-declarations node))
      :body (ast-lambda-body node)
      :env (ast-lambda-env node)))
    ((and (ast-let-p node)
          (consp (ast-let-body node))
          (null (cdr (ast-let-body node))))
     (make-ast-let
      :bindings (ast-let-bindings node)
      :declarations (ast-let-declarations node)
      :body (list (%php-strip-callable-by-ref-metadata (first (ast-let-body node))))))
    (t node)))

(defun %php-call-with-spread (func-node args)
  (let ((by-ref-indices (%php-callable-by-ref-indices func-node)))
    (cond
      ((%php-args-have-spread-p args)
       (make-ast-apply :func func-node :args (list (%php-spread-arglist-expr args))))
      (by-ref-indices
       (%php-lower-by-ref-call
        (%php-strip-callable-by-ref-metadata func-node)
        args
        by-ref-indices))
      (t
       (make-ast-call :func func-node :args args)))))

(defun %php-register-named-callable-params (fn-sym params param-defaults variadic-param)
  (setf (gethash (symbol-name fn-sym) *php-named-param-registry*)
        (list :params params
              :defaults param-defaults
              :variadic variadic-param)))

(defun %php-named-arg-call-p (node)
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-named-arg)))))

(defun %php-args-have-named-p (args)
  (some #'%php-named-arg-call-p args))

(defun %php-named-arg-name (node)
  (let ((name-node (first (ast-call-args node))))
    (when (ast-quote-p name-node)
      (ast-quote-value name-node))))

(defun %php-named-arg-value (node)
  (second (ast-call-args node)))

(defun %php-param-index-by-name (name params)
  (position name params
            :test (lambda (needle param)
                    (let ((param-name (%php-param-string-name param)))
                      (or (string= needle param-name)
                          (string-equal needle param-name))))))

(defun %php-default-for-param (param defaults)
  (cdr (assoc (%php-param-name param) defaults :test #'eq)))

(defun %php-array-constructor-call-p (node)
  (and (ast-call-p node)
       (let ((func (ast-call-func node)))
         (and (ast-var-p func)
              (eq (ast-var-name func) 'cl-cc/php::%php-array)))))

(defun %php-array-auto-entry-p (entry)
  (let ((elements (and (ast-list-p entry) (ast-list-elements entry))))
    (and (= (length elements) 3)
         (ast-quote-p (first elements))
         (not (ast-quote-value (first elements)))
         (not (%php-spread-call-p (third elements))))))

(defun %php-spread-static-width (arg)
  (unless (%php-spread-call-p arg)
    (return-from %php-spread-static-width (values nil nil)))
  (let ((expr (first (ast-call-args arg))))
    (if (and (%php-array-constructor-call-p expr)
             (every #'%php-array-auto-entry-p (ast-call-args expr)))
        (values (length (ast-call-args expr)) t)
        (values nil nil))))

(defun %php-runtime-call-arg-descriptor (arg)
  (cond
    ((%php-named-arg-call-p arg)
     (%php-call 'list
                (make-ast-quote :value :named)
                (make-ast-quote :value (%php-named-arg-name arg))
                (%php-named-arg-value arg)))
    ((%php-spread-call-p arg)
     (%php-call 'list
                (make-ast-quote :value :spread)
                (first (ast-call-args arg))))
    (t
     (%php-call 'list
                (make-ast-quote :value :pos)
                arg))))

(defun %php-dynamic-named-spread-arglist-expr (fn-sym params defaults args)
  (%php-call 'cl-cc/php::%php-call-args-with-named-spread
             (make-ast-quote :value fn-sym)
             (make-ast-quote :value (mapcar #'%php-param-string-name params))
             (make-ast-quote :value (mapcar (lambda (param)
                                              (not (null (%php-default-for-param
                                                          param defaults))))
                                            params))
             (%php-call 'list
                        (mapcar (lambda (param)
                                  (or (%php-default-for-param param defaults)
                                      (make-ast-quote :value nil)))
                                params))
             (mapcar #'%php-runtime-call-arg-descriptor args)))

(defstruct (%php-named-call-state (:conc-name %php-named-call-state-))
  fn-sym params defaults slots filled positional-index last-index seen-named
  extra-args)

(defun %php-make-named-call-state (fn-sym metadata)
  (let ((params (getf metadata :params))
        (defaults (getf metadata :defaults)))
    (when params
      (make-%php-named-call-state
       :fn-sym fn-sym
       :params params
       :defaults defaults
       :slots (make-array (length params) :initial-element nil)
       :filled (make-array (length params) :initial-element nil)
       :positional-index 0
       :last-index -1
       :seen-named nil
       :extra-args nil))))

(defun %php-named-call-state-dynamic-arglist-expr (state args)
  (%php-dynamic-named-spread-arglist-expr
   (%php-named-call-state-fn-sym state)
   (%php-named-call-state-params state)
   (%php-named-call-state-defaults state)
   args))

(defun %php-add-named-call-arg (state arg)
  (setf (%php-named-call-state-seen-named state) t)
  (let* ((name (%php-named-arg-name arg))
         (params (%php-named-call-state-params state))
         (index (and name (%php-param-index-by-name name params))))
    (unless index
      (error "Unknown named argument ~A for PHP function ~A"
             name (%php-named-call-state-fn-sym state)))
    (when (aref (%php-named-call-state-filled state) index)
      (error "Duplicate named argument ~A for PHP function ~A"
             name (%php-named-call-state-fn-sym state)))
    (setf (aref (%php-named-call-state-slots state) index) (%php-named-arg-value arg)
          (aref (%php-named-call-state-filled state) index) t
          (%php-named-call-state-last-index state)
          (max (%php-named-call-state-last-index state) index)))
  state)

(defun %php-add-spread-call-arg (state arg args)
  (when (%php-named-call-state-seen-named state)
    (error "PHP spread argument after named argument in call to ~A"
           (%php-named-call-state-fn-sym state)))
  (multiple-value-bind (width known-width-p) (%php-spread-static-width arg)
    (unless known-width-p
      (return-from %php-add-spread-call-arg
        (values nil (%php-named-call-state-dynamic-arglist-expr state args))))
    (cond
      ((zerop width) nil)
      ((< (%php-named-call-state-positional-index state)
          (length (%php-named-call-state-params state)))
       (dotimes (offset width)
         (let ((index (+ (%php-named-call-state-positional-index state) offset)))
           (when (and (< index (length (%php-named-call-state-params state)))
                      (aref (%php-named-call-state-filled state) index))
             (error "Duplicate PHP argument for parameter ~A in call to ~A"
                    (%php-param-string-name (nth index (%php-named-call-state-params state)))
                    (%php-named-call-state-fn-sym state)))))
       (setf (aref (%php-named-call-state-slots state)
                   (%php-named-call-state-positional-index state))
             arg
             (aref (%php-named-call-state-filled state)
                   (%php-named-call-state-positional-index state))
             t
             (%php-named-call-state-last-index state)
             (max (%php-named-call-state-last-index state)
                  (min (1- (length (%php-named-call-state-params state)))
                       (+ (%php-named-call-state-positional-index state)
                          width -1))))
       (loop for index from (1+ (%php-named-call-state-positional-index state))
             below (min (length (%php-named-call-state-params state))
                        (+ (%php-named-call-state-positional-index state) width))
             do (setf (aref (%php-named-call-state-filled state) index)
                      :php-spread-covered))
       (incf (%php-named-call-state-positional-index state) width))
      (t
       (push arg (%php-named-call-state-extra-args state)))))
  (values state nil))

(defun %php-add-positional-call-arg (state arg)
  (if (< (%php-named-call-state-positional-index state)
         (length (%php-named-call-state-params state)))
      (progn
        (setf (aref (%php-named-call-state-slots state)
                    (%php-named-call-state-positional-index state))
              arg
              (aref (%php-named-call-state-filled state)
                    (%php-named-call-state-positional-index state))
              t
              (%php-named-call-state-last-index state)
              (max (%php-named-call-state-last-index state)
                   (%php-named-call-state-positional-index state)))
        (incf (%php-named-call-state-positional-index state)))
      (push arg (%php-named-call-state-extra-args state)))
  state)

(defun %php-finalize-named-call-state (state)
  (append
   (loop for i from 0 to (%php-named-call-state-last-index state)
         for param = (nth i (%php-named-call-state-params state))
         unless (eq (aref (%php-named-call-state-filled state) i) :php-spread-covered)
           collect (cond
                     ((aref (%php-named-call-state-filled state) i)
                      (aref (%php-named-call-state-slots state) i))
                     ((%php-default-for-param param (%php-named-call-state-defaults state)))
                     (t (error "Missing required PHP argument ~A in call to ~A"
                               (%php-param-string-name param)
                               (%php-named-call-state-fn-sym state)))))
   (nreverse (%php-named-call-state-extra-args state))))

(defun %php-reorder-named-args-for-call (fn-sym args)
  (let ((state (%php-make-named-call-state
                fn-sym
                (gethash (symbol-name fn-sym) *php-named-param-registry*))))
    (unless state
      (return-from %php-reorder-named-args-for-call
        (%php-static-call-lowering-result args)))
    (dolist (arg args)
      (multiple-value-bind (continue-p arglist-expr)
          (cond
            ((%php-named-arg-call-p arg)
             (values (%php-add-named-call-arg state arg) nil))
            ((%php-spread-call-p arg)
             (%php-add-spread-call-arg state arg args))
            (t
             (when (%php-named-call-state-seen-named state)
               (error "PHP positional argument after named argument in call to ~A"
                      fn-sym))
             (values (%php-add-positional-call-arg state arg) nil)))
        (unless continue-p
          (return-from %php-reorder-named-args-for-call
            (%php-runtime-call-lowering-result arglist-expr)))))
    (%php-static-call-lowering-result (%php-finalize-named-call-state state))))

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
