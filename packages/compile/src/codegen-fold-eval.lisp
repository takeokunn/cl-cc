;;;; packages/compile/src/codegen-fold-eval.lisp — Compile-time Partial Evaluator Engine
;;;
;;; Contains: %compile-time-eval-binop, %compile-time-eval-call,
;;;           %evaluate-ast-sequence, %evaluate-ast.
;;;
;;; Data tables and helpers (%evaluate-ast-then, %fold-ast-binop,
;;;   *compile-time-eval-fns*, etc.) are in codegen-fold.lisp.
;;;
;;; Load order: after codegen-fold.lisp, before codegen-fold-optimize.lisp.

(in-package :cl-cc/compile)

;;; ─── Data: foldable function sets ───────────────────────────────────────────

(defparameter *compile-time-multi-arg-fns*
  '(+ - * = < <= > >=)
  "N-ary CL functions foldable at compile time via symbol-function.")

(defparameter *compile-time-unary-pred-fns*
  '(not zerop plusp minusp oddp evenp numberp integerp consp null symbolp stringp functionp)
  "Unary predicate fns foldable at compile time via symbol-function.")

(defparameter *compile-time-binop-table*
  `((+ . ,(lambda (lhs rhs) (+ lhs rhs)))
    (- . ,(lambda (lhs rhs) (- lhs rhs)))
    (* . ,(lambda (lhs rhs) (* lhs rhs)))
    (/ . ,(lambda (lhs rhs)
            (unless (zerop rhs) (let ((q (/ lhs rhs))) (when (integerp q) q)))))
    (1+ . ,(lambda (lhs rhs) (when (and (integerp lhs) (null rhs)) (1+ lhs))))
    (1- . ,(lambda (lhs rhs) (when (and (integerp lhs) (null rhs)) (1- lhs)))))
  "Compile-time constant folding for binary/unary-as-binary operators.")

;;; ─── Logic ──────────────────────────────────────────────────────────────────

(defun %compile-time-eval-binop (op lhs rhs)
  "Evaluate OP on compile-time constants LHS and RHS; return nil if not foldable."
  (let ((entry (assoc op *compile-time-binop-table* :test #'eq)))
    (when entry (funcall (cdr entry) lhs rhs))))

(defun %compile-time-pair-bindings (params args)
  "Pair PARAMS and ARGS into an alist."
  (labels ((scan (ps as)
             (if (and (consp ps) (consp as))
                 (cons (cons (car ps) (car as))
                       (scan (cdr ps) (cdr as)))
                 nil)))
    (scan params args)))

(defun %compile-time-eval-known-call (name args)
  "Evaluate NAME applied to ARGS at compile time; returns (values result t) or (values nil nil)."
  (cond
    ((eq name '/)
     (let ((v (ignore-errors (apply #'/ args))))
       (if v (values v t) (values nil nil))))
    ((member name *compile-time-multi-arg-fns* :test #'eq)
     (values (apply (symbol-function name) args) t))
    ((member name *compile-time-unary-pred-fns* :test #'eq)
     (values (funcall (symbol-function name) (car args)) t))
    (t (values nil nil))))

(defun %compile-time-lookup-defun (name)
  "Return the ast-defun for NAME when it is inlinable (no optional/rest/key params), else NIL."
  (multiple-value-bind (node found-p)
      (%compile-time-lookup name *compile-time-function-env*)
    (when (and found-p
               (typep node 'ast-defun)
               (null (ast-defun-optional-params node))
               (null (ast-defun-rest-param node))
               (null (ast-defun-key-params node)))
      node)))

(defun %compile-time-eval-var-call (func-name args depth)
  "Try compile-time evaluation of a call to named function FUNC-NAME."
  (cond
    ((and (= (length args) 1)
          (string= (symbol-name func-name) "STRING-LENGTH")
          (stringp (car args)))
     (values (length (car args)) t))
    (t
     (multiple-value-bind (known-value known-ok)
         (%compile-time-eval-known-call func-name args)
       (if known-ok
           (values known-value t)
           (let ((defun-node (and (>= depth 0) (%compile-time-lookup-defun func-name))))
             (if defun-node
                 (%evaluate-ast-sequence
                  (ast-defun-body defun-node)
                  (%compile-time-pair-bindings (ast-defun-params defun-node) args)
                  *compile-time-function-env*
                  (1- depth))
                 (values nil nil))))))))

(defun %compile-time-eval-call (func args depth)
  (typecase func
    (ast-var    (%compile-time-eval-var-call (ast-var-name func) args depth))
    (ast-lambda (%evaluate-ast-sequence
                 (ast-lambda-body func)
                 (%compile-time-pair-bindings (ast-lambda-params func) args)
                 *compile-time-function-env*
                 (1- depth)))
    (t (values nil nil))))

(defun %evaluate-ast-sequence (forms value-env function-env depth)
  "Evaluate FORMS left-to-right under VALUE-ENV and FUNCTION-ENV.
Returns (values last-value t) on success, (values nil nil) on first failure."
  (if (null forms)
      (values nil t)
      (let ((*compile-time-value-env* value-env)
            (*compile-time-function-env* function-env))
        (labels ((scan (remaining last-value)
                   (if (consp remaining)
                       (multiple-value-bind (value ok)
                           (%evaluate-ast (car remaining) depth)
                         (if ok
                             (scan (cdr remaining) value)
                             (values nil nil)))
                       (values last-value t))))
          (scan forms nil)))))

(defun %compile-time-eval-let-bindings (bindings depth)
  (labels ((scan (remaining)
             (if (consp remaining)
                 (let ((binding (car remaining)))
                   (multiple-value-bind (value ok)
                       (%evaluate-ast (cdr binding) (1- depth))
                     (if ok
                         (multiple-value-bind (rest-bindings rest-ok)
                             (scan (cdr remaining))
                           (if rest-ok
                               (values (cons (cons (car binding) value)
                                             rest-bindings)
                                       t)
                               (values nil nil)))
                         (values nil nil))))
                 (values nil t))))
    (scan bindings)))

(defun %compile-time-append-env (bindings env)
  (if (consp bindings)
      (cons (car bindings)
            (%compile-time-append-env (cdr bindings) env))
      env))

(defun %compile-time-eval-args (args depth)
  (labels ((scan (remaining)
             (if (consp remaining)
                 (multiple-value-bind (value ok)
                     (%evaluate-ast (car remaining) (1- depth))
                   (if ok
                       (multiple-value-bind (rest-values rest-ok)
                           (scan (cdr remaining))
                         (if rest-ok
                             (values (cons value rest-values) t)
                             (values nil nil)))
                       (values nil nil)))
                 (values nil t))))
    (scan args)))

(defun %evaluate-ast (node depth)
  (when (minusp depth)
    (return-from %evaluate-ast (values nil nil)))
  (typecase node
    (ast-int
     (values (ast-int-value node) t))
    (ast-quote
     (values (ast-quote-value node) t))
    (ast-var
     (multiple-value-bind (value found-p)
         (%compile-time-lookup (ast-var-name node) *compile-time-value-env*)
       (if found-p (values value t) (values nil nil))))
    (ast-binop
     (%evaluate-ast-then
      (ast-binop-lhs node) depth
      (lambda (lhs)
        (%evaluate-ast-then
         (ast-binop-rhs node) depth
         (lambda (rhs)
           (let ((value (%compile-time-eval-binop
                         (ast-binop-op node) lhs rhs)))
             (if (and value (integerp value))
                 (values value t)
                 (values nil nil))))))))
    (ast-if
     (%evaluate-ast-then
      (ast-if-cond node) depth
      (lambda (cond-value)
        (%evaluate-ast
         (if (opt-falsep cond-value) (ast-if-else node) (ast-if-then node))
         (1- depth)))))
    (ast-progn
     (%evaluate-ast-sequence
      (ast-progn-forms node)
      *compile-time-value-env*
      *compile-time-function-env*
      (1- depth)))
    (ast-block
     (let* ((tag (gensym "BLOCK-"))
            (*compile-time-block-env*
              (cons (cons (ast-block-name node) tag)
                    *compile-time-block-env*))
            (raw (multiple-value-list
                  (catch tag
                    (%evaluate-ast-sequence
                     (ast-block-body node)
                     *compile-time-value-env*
                     *compile-time-function-env*
                     (1- depth))))))
       ;; sequence exit → 2 values (last-value ok); throw exit → 1 value
       (if (= (length raw) 2)
           (values (car raw) (cadr raw))
           (values (car raw) t))))
    (ast-return-from
     (multiple-value-bind (tag found-p)
         (%compile-time-lookup (ast-return-from-name node)
                               *compile-time-block-env*)
       (if found-p
           (%evaluate-ast-then
            (ast-return-from-value node) depth
            (lambda (value) (throw tag value)))
           (values nil nil))))
    (ast-let
     (multiple-value-bind (bindings ok)
         (%compile-time-eval-let-bindings (ast-let-bindings node) depth)
       (if ok
           (%evaluate-ast-sequence
            (ast-let-body node)
            (%compile-time-append-env bindings *compile-time-value-env*)
            *compile-time-function-env*
            (1- depth))
           (values nil nil))))
    (ast-the
     (%evaluate-ast (ast-the-value node) (1- depth)))
    (ast-call
     (multiple-value-bind (args ok)
         (%compile-time-eval-args (ast-call-args node) depth)
       (if ok
           (%compile-time-eval-call (ast-call-func node) args depth)
           (values nil nil))))
    (t (values nil nil))))
