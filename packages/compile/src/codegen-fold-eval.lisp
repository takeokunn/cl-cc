;;;; packages/compile/src/codegen-fold-eval.lisp — Compile-time Partial Evaluator Engine
;;;
;;; Contains: %compile-time-eval-binop, %compile-time-eval-call,
;;;           %evaluate-ast-sequence, %evaluate-ast.
;;;
;;; Data tables and helpers (%evaluate-ast-then, %fold-ast-binop,
;;;   *compile-time-eval-fns*, *compile-time-simple-binops*, etc.)
;;;   are in codegen-fold.lisp.
;;;
;;; Load order: after codegen-fold.lisp, before codegen-fold-optimize.lisp.

(in-package :cl-cc/compile)

(defun %compile-time-eval-binop (op lhs rhs)
  "Evaluate OP on compile-time constant LHS and RHS; return nil if not foldable."
  (if (eq op '+)
      (+ lhs rhs)
      (if (eq op '-)
          (- lhs rhs)
          (if (eq op '*)
              (* lhs rhs)
              (if (eq op '/)
                  (if (zerop rhs)
                      nil
                      (let ((q (/ lhs rhs)))
                        (if (integerp q) q nil)))
                  (if (eq op '1+)
                      (if (and (integerp lhs) (null rhs)) (1+ lhs) nil)
                      (if (eq op '1-)
                          (if (and (integerp lhs) (null rhs)) (1- lhs) nil)
                          nil)))))))

(defun %compile-time-pair-bindings (params args)
  "Pair PARAMS and ARGS into an alist without using MAPCAR during selfhost."
  (labels ((scan (remaining-params remaining-args)
             (if (and (consp remaining-params) (consp remaining-args))
                 (cons (cons (car remaining-params) (car remaining-args))
                       (scan (cdr remaining-params) (cdr remaining-args)))
                 nil)))
    (scan params args)))

(defun %compile-time-eval-known-call (name args)
  (if (eq name '+)
      (values (apply #'+ args) t)
      (if (eq name '-)
      (values (apply #'- args) t)
      (if (eq name '*)
      (values (apply #'* args) t)
      (if (eq name '/)
      (let ((v (ignore-errors (apply #'/ args))))
        (if v (values v t) (values nil nil)))
      (if (eq name '=)
      (values (if (apply #'= args) t nil) t)
      (if (eq name '<)
      (values (if (apply #'< args) t nil) t)
      (if (eq name '<=)
      (values (if (apply #'<= args) t nil) t)
      (if (eq name '>)
      (values (if (apply #'> args) t nil) t)
      (if (eq name '>=)
      (values (if (apply #'>= args) t nil) t)
      (if (eq name 'not)
      (values (not (car args)) t)
      (if (eq name 'zerop)
      (values (zerop (car args)) t)
      (if (eq name 'plusp)
      (values (plusp (car args)) t)
      (if (eq name 'minusp)
      (values (minusp (car args)) t)
      (if (eq name 'oddp)
      (values (oddp (car args)) t)
      (if (eq name 'evenp)
      (values (evenp (car args)) t)
      (if (eq name 'numberp)
      (values (numberp (car args)) t)
      (if (eq name 'integerp)
      (values (integerp (car args)) t)
      (if (eq name 'consp)
      (values (consp (car args)) t)
      (if (eq name 'null)
      (values (null (car args)) t)
      (if (eq name 'symbolp)
      (values (symbolp (car args)) t)
      (if (eq name 'stringp)
      (values (stringp (car args)) t)
      (if (eq name 'functionp)
      (values (functionp (car args)) t)
      (values nil nil))))))))))))))))))))))))

(defun %compile-time-eval-call (func args depth)
  (if (and (typep func 'ast-var)
           (= (length args) 1)
           (string= (symbol-name (ast-var-name func)) "STRING-LENGTH")
           (stringp (car args)))
      (values (length (car args)) t)
      (if (typep func 'ast-var)
          (multiple-value-bind (known-value known-ok)
              (%compile-time-eval-known-call (ast-var-name func) args)
            (if known-ok
                (values known-value t)
                (if (<= 0 depth)
                    (multiple-value-bind (defun-node found-p)
                        (%compile-time-lookup (ast-var-name func)
                                              *compile-time-function-env*)
                      (if (and found-p
                               (typep defun-node 'ast-defun)
                               (null (ast-defun-optional-params defun-node))
                               (null (ast-defun-rest-param defun-node))
                               (null (ast-defun-key-params defun-node)))
                          (%evaluate-ast-sequence
                           (ast-defun-body defun-node)
                           (%compile-time-pair-bindings (ast-defun-params defun-node) args)
                           *compile-time-function-env*
                           (1- depth))
                          nil))
                    nil)))
          (if (typep func 'ast-lambda)
              (%evaluate-ast-sequence
               (ast-lambda-body func)
               (%compile-time-pair-bindings (ast-lambda-params func) args)
               *compile-time-function-env*
               (1- depth))
              nil))))

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
  (if (minusp depth)
      (values nil nil)
      (if (typep node 'ast-int)
          (values (ast-int-value node) t)
          (if (typep node 'ast-quote)
              (values (ast-quote-value node) t)
              (if (typep node 'ast-var)
                  (multiple-value-bind (value found-p)
                      (%compile-time-lookup (ast-var-name node)
                                            *compile-time-value-env*)
                    (if found-p (values value t) (values nil nil)))
                  (if (typep node 'ast-binop)
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
                                  (values nil nil)))))))
                      (if (typep node 'ast-if)
                          (%evaluate-ast-then
                           (ast-if-cond node) depth
                           (lambda (cond-value)
                             (%evaluate-ast
                              (if (opt-falsep cond-value)
                                  (ast-if-else node)
                                  (ast-if-then node))
                              (1- depth))))
                          (if (typep node 'ast-progn)
                              (%evaluate-ast-sequence
                               (ast-progn-forms node)
                               *compile-time-value-env*
                               *compile-time-function-env*
                               (1- depth))
                              (if (typep node 'ast-block)
                                  (let ((tag (gensym "BLOCK-")))
                                    (let ((*compile-time-block-env*
                                            (cons (cons (ast-block-name node) tag)
                                                  *compile-time-block-env*)))
                                      (catch tag
                                        (%evaluate-ast-sequence
                                         (ast-block-body node)
                                         *compile-time-value-env*
                                         *compile-time-function-env*
                                         (1- depth)))))
                                  (if (typep node 'ast-return-from)
                                      (multiple-value-bind (tag found-p)
                                          (%compile-time-lookup
                                           (ast-return-from-name node)
                                           *compile-time-block-env*)
                                        (if found-p
                                            (%evaluate-ast-then
                                             (ast-return-from-value node) depth
                                             (lambda (value) (throw tag value)))
                                            (values nil nil)))
                                      (if (typep node 'ast-let)
                                          (multiple-value-bind (bindings ok)
                                              (%compile-time-eval-let-bindings
                                               (ast-let-bindings node) depth)
                                            (if ok
                                                (%evaluate-ast-sequence
                                                 (ast-let-body node)
                                                 (%compile-time-append-env
                                                  bindings
                                                  *compile-time-value-env*)
                                                 *compile-time-function-env*
                                                 (1- depth))
                                                (values nil nil)))
                                          (if (typep node 'ast-the)
                                              (%evaluate-ast (ast-the-value node)
                                                             (1- depth))
                                              (if (typep node 'ast-call)
                                                  (multiple-value-bind (args ok)
                                                      (%compile-time-eval-args
                                                       (ast-call-args node) depth)
                                                    (if ok
                                                        (%compile-time-eval-call
                                                         (ast-call-func node)
                                                         args
                                                         depth)
                                                        (values nil nil)))
                                                  (values nil nil))))))))))))))
