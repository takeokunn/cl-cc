;;;; packages/engine/compile/src/codegen-fold.lisp — Compile-time constant folding and partial evaluation
;;;
;;; Contains:
;;;   - %fold-ast-binop          constant-fold a binop node with already-folded operands
;;;   - *compile-time-eval-fns*  data table: symbol → evaluator lambda
;;;   - %evaluate-ast            partial evaluator (walks the AST at compile time)
;;;
;;; %loc macro and optimize-ast fold pass are in codegen-fold-optimize.lisp.
;;; This file is a pure data+logic layer: no VM instructions are emitted here.
;;; Load order: after codegen-core, before codegen-fold-optimize.

(in-package :cl-cc/compile)

;;; ── CPS evaluation helper ────────────────────────────────────────────────────
;;;
;;; %with-eval encodes the compile-time short-circuit: evaluate NODE at DEPTH-1,
;;; bind VAR to the result, continue into BODY — or silently return (values nil nil).
;;; This is the continuation-passing style of "evaluate or abandon".

(defmacro %with-eval ((var node depth) &body body)
  (let ((ok-sym (gensym "OK")))
    `(multiple-value-bind (,var ,ok-sym) (%evaluate-ast ,node (1- ,depth))
       (if ,ok-sym
           (progn ,@body)
           (values nil nil)))))

;;; ── Binop constant-fold helpers ──────────────────────────────────────────────

(defun %ast-constant-number-value (node)
  "Return NODE's integer value when NODE is a constant AST integer."
  (typecase node
    (ast-int (ast-int-value node))
    (ast-quote (let ((value (ast-quote-value node)))
                 (when (integerp value)
                   value)))
    (t nil)))

(defun %clone-source (node &rest make-args)
  "Apply MAKE-ARGS to a constructor that accepts :source-file/:source-line/:source-column,
filling those fields from NODE."
  (apply (first make-args)
         :source-file   (ast-source-file   node)
         :source-line   (ast-source-line   node)
         :source-column (ast-source-column node)
         (rest make-args)))

(defun %same-ast-binop (node lhs rhs)
  (%clone-source node #'make-ast-binop :op (ast-binop-op node) :lhs lhs :rhs rhs))

(defun %fold-ast-binop (node lhs rhs)
  (let ((lv (%ast-constant-number-value lhs))
        (rv (%ast-constant-number-value rhs)))
    (if (and lv rv)
        (let ((value (case (ast-binop-op node)
                       (+ (+ lv rv))
                       (- (- lv rv))
                       (* (* lv rv))
                       (/ (when (not (zerop rv))
                            (let ((quot (/ lv rv)))
                              (when (integerp quot) quot))))
                       (otherwise nil))))
          (if (integerp value)
              (%clone-source node #'make-ast-int :value value)
              (%same-ast-binop node lhs rhs)))
        (%same-ast-binop node lhs rhs))))

;;; ── Compile-time evaluation state ────────────────────────────────────────────

(defvar *compile-time-value-env* nil
  "Alist of compile-time constant bindings available to optimize-ast.")

(defvar *compile-time-function-env* nil
  "Alist of compile-time known function definitions available to optimize-ast.")

(defvar *compile-time-block-env* nil
  "Alist of compile-time block tags available to optimize-ast.")

(defvar *compile-time-eval-depth-limit* 64
  "Maximum recursion depth for compile-time partial evaluation.")

;;; ── Data table: compile-time evaluable built-ins ─────────────────────────────

(defparameter *compile-time-eval-fns*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (sym fn) (setf (gethash sym ht) fn)))
      ;; Variadic arithmetic
      (reg '+ (lambda (args) (values (apply #'+ args) t)))
      (reg '- (lambda (args) (values (apply #'- args) t)))
      (reg '* (lambda (args) (values (apply #'* args) t)))
      (reg '/ (lambda (args)
                (let ((v (ignore-errors (apply #'/ args))))
                  (when v (values v t)))))
      ;; Variadic comparisons
      (reg '= (lambda (args) (values (if (apply #'= args) t nil) t)))
      (reg '< (lambda (args) (values (if (apply #'< args) t nil) t)))
      (reg '<= (lambda (args) (values (if (apply #'<= args) t nil) t)))
      (reg '> (lambda (args) (values (if (apply #'> args) t nil) t)))
      (reg '>= (lambda (args) (values (if (apply #'>= args) t nil) t)))
      ;; Unary predicates
      (dolist (pair '((not . not) (zerop . zerop) (plusp . plusp) (minusp . minusp)
                      (oddp . oddp) (evenp . evenp) (numberp . numberp)
                      (integerp . integerp) (consp . consp) (null . null)
                      (symbolp . symbolp) (stringp . stringp) (functionp . functionp)))
        (let ((fn (symbol-function (cdr pair))))
          (reg (car pair) (lambda (args) (values (funcall fn (first args)) t))))))
    ht)
  "Hash table mapping compile-time evaluable symbols to (args) -> (values result t) lambdas.")

;;; ── Compile-time evaluation helpers ─────────────────────────────────────────

(defun %compile-time-falsep (value)
  (opt-falsep value))

(defun %ast-constant-node-p (node)
  (or (typep node 'ast-int)
      (typep node 'ast-quote)))

(defun %ast->compile-time-value (node)
  (typecase node
    (ast-int (ast-int-value node))
    (ast-quote (ast-quote-value node))
    (t nil)))

(defun %compile-time-value->ast (value node)
  (if (integerp value)
      (%clone-source node #'make-ast-int   :value value)
      (%clone-source node #'make-ast-quote :value value)))

(defun %compile-time-lookup (name env)
  "Look up NAME in ENV alist; return (values value t) when found, (values nil nil) otherwise.
Block environments use the same structure, so %compile-time-lookup-block is an alias."
  (let ((entry (assoc name env)))
    (when entry
      (values (cdr entry) t))))

;;; Block environments have the same alist structure as value environments.
(defun %compile-time-lookup-block (name env)
  "Look up block NAME in ENV using the same alist protocol as values."
  (%compile-time-lookup name env))

(defparameter *compile-time-simple-binops*
  '((+ . +) (- . -) (* . *))
  "Alist of binary operators foldable as (op lhs rhs) with no guards required.
Does not include /  (needs zerop guard + integerp check)
or 1+/1- (unary, require rhs = nil).")

(defun %compile-time-eval-binop (op lhs rhs)
  "Evaluate OP on compile-time constant LHS and RHS; return nil if not foldable."
  (let ((simple (assoc op *compile-time-simple-binops*)))
    (cond
      ;; Table-driven: pure binary application, no guards
      (simple (funcall (symbol-function (cdr simple)) lhs rhs))
      ;; Division: only fold when result is an exact integer
      ((eq op '/) (let ((q (and (not (zerop rhs)) (/ lhs rhs))))
                    (when (integerp q) q)))
      ;; Unary arithmetic exposed through binary signature (rhs = nil)
      ((eq op '1+) (when (and (integerp lhs) (null rhs)) (1+ lhs)))
      ((eq op '1-) (when (and (integerp lhs) (null rhs)) (1- lhs))))))

(defun %compile-time-eval-call (func args depth)
  (cond
    ((and (typep func 'ast-var)
           (= (length args) 1)
           (string= (symbol-name (ast-var-name func)) "STRING-LENGTH")
           (stringp (first args)))
      (values (length (first args)) t))
    ((and (typep func 'ast-var)
          (gethash (ast-var-name func) *compile-time-eval-fns*))
     (let ((fn (gethash (ast-var-name func) *compile-time-eval-fns*)))
       (funcall fn args)))
    ((and (typep func 'ast-var)
          (<= 0 depth)
          (multiple-value-bind (entry found-p)
              (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
            (declare (ignore entry))
            found-p))
     (multiple-value-bind (defun-node found-p)
         (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
       (when (and found-p (typep defun-node 'ast-defun)
                  (null (ast-defun-optional-params defun-node))
                  (null (ast-defun-rest-param defun-node))
                  (null (ast-defun-key-params defun-node)))
         (let ((param-bindings (mapcar #'cons (ast-defun-params defun-node) args)))
           (%evaluate-ast-sequence (ast-defun-body defun-node)
                                   param-bindings
                                   *compile-time-function-env*
                                   (1- depth))))))
    ((typep func 'ast-lambda)
     (let ((param-bindings (mapcar #'cons (ast-lambda-params func) args)))
       (%evaluate-ast-sequence (ast-lambda-body func)
                               param-bindings
                               *compile-time-function-env*
                               (1- depth))))
    (t nil)))

(defun %evaluate-ast-sequence (forms value-env function-env depth)
  "Evaluate FORMS left-to-right under VALUE-ENV and FUNCTION-ENV.
Returns (values last-value t) on success, (values nil nil) on first failure.
Uses loop's unless-return to avoid mid-body return-from."
  (when (null forms)
    (return-from %evaluate-ast-sequence (values nil t)))
  (let ((*compile-time-value-env* value-env)
        (*compile-time-function-env* function-env))
    (let ((last-value nil))
      (dolist (form forms (values last-value t))
        (multiple-value-bind (value ok)
            (%evaluate-ast form depth)
          (unless ok
            (return-from %evaluate-ast-sequence (values nil nil)))
          (setf last-value value))))))

(defun %evaluate-ast (node depth)
  (when (minusp depth)
    (return-from %evaluate-ast (values nil nil)))
  (typecase node
    (ast-int (values (ast-int-value node) t))
    (ast-quote (values (ast-quote-value node) t))
    (ast-var
     (multiple-value-bind (value found-p)
         (%compile-time-lookup (ast-var-name node) *compile-time-value-env*)
       (if found-p
           (values value t)
           (values nil nil))))
    (ast-binop
     (%with-eval (lhs (ast-binop-lhs node) depth)
       (%with-eval (rhs (ast-binop-rhs node) depth)
         (let ((value (%compile-time-eval-binop (ast-binop-op node) lhs rhs)))
           (if (and value (integerp value))
               (values value t)
               (values nil nil))))))
    (ast-if
     (%with-eval (cond-value (ast-if-cond node) depth)
       (%evaluate-ast (if (%compile-time-falsep cond-value)
                          (ast-if-else node)
                          (ast-if-then node))
                      (1- depth))))
    (ast-progn
     (%evaluate-ast-sequence (ast-progn-forms node)
                              *compile-time-value-env*
                              *compile-time-function-env*
                              (1- depth)))
    (ast-block
     (let ((tag (gensym "BLOCK-")))
       (let ((*compile-time-block-env* (acons (ast-block-name node)
                                              tag
                                              *compile-time-block-env*)))
         (catch tag
           (%evaluate-ast-sequence (ast-block-body node)
                                   *compile-time-value-env*
                                   *compile-time-function-env*
                                   (1- depth))))))
    (ast-return-from
     (multiple-value-bind (tag found-p)
         (%compile-time-lookup-block (ast-return-from-name node) *compile-time-block-env*)
       (if found-p
           (%with-eval (value (ast-return-from-value node) depth)
             (throw tag value))
           (values nil nil))))
    (ast-let
     (let ((bindings nil))
       (dolist (binding (ast-let-bindings node))
         (multiple-value-bind (value ok)
             (%evaluate-ast (cdr binding) (1- depth))
           (unless ok (return-from %evaluate-ast (values nil nil)))
           (push (cons (car binding) value) bindings)))
       (%evaluate-ast-sequence (ast-let-body node)
                               (append (nreverse bindings) *compile-time-value-env*)
                               *compile-time-function-env*
                               (1- depth))))
    (ast-the
     (%evaluate-ast (ast-the-value node) (1- depth)))
    (ast-call
     (let* ((func (ast-call-func node))
            (args nil))
       (dolist (arg (ast-call-args node))
         (multiple-value-bind (value ok)
             (%evaluate-ast arg (1- depth))
           (unless ok (return-from %evaluate-ast (values nil nil)))
           (push value args)))
       (%compile-time-eval-call func (nreverse args) depth)))
    (t (values nil nil))))

;;; %loc macro and optimize-ast fold pass are in codegen-fold-optimize.lisp
;;; (loaded immediately after this file).
