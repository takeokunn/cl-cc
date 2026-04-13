;;;; src/compile/codegen-fold.lisp — Compile-time constant folding and partial evaluation
;;;
;;; Contains:
;;;   - %fold-ast-binop          constant-fold a binop node with already-folded operands
;;;   - *compile-time-eval-fns*  data table: symbol → evaluator lambda
;;;   - %evaluate-ast            partial evaluator (walks the AST at compile time)
;;;   - %loc                     macro: inherit source location from an AST node
;;;   - optimize-ast             top-level fold pass called from compile-toplevel-forms
;;;
;;; This file is a pure data+logic layer: no VM instructions are emitted here.
;;; Load order: after codegen-core, before codegen.

(in-package :cl-cc)

;;; ── Binop constant-fold helpers ──────────────────────────────────────────────

(defun %ast-constant-number-value (node)
  "Return NODE's integer value when NODE is a constant AST integer."
  (typecase node
    (ast-int (ast-int-value node))
    (ast-quote (let ((value (ast-quote-value node)))
                 (when (integerp value)
                   value)))
    (t nil)))

(defun %same-ast-binop (node lhs rhs)
  (make-ast-binop :op (ast-binop-op node)
                  :lhs lhs
                  :rhs rhs
                  :source-file (ast-source-file node)
                  :source-line (ast-source-line node)
                  :source-column (ast-source-column node)))

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
              (make-ast-int :value value
                            :source-file (ast-source-file node)
                            :source-line (ast-source-line node)
                            :source-column (ast-source-column node))
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
  (cond
    ((integerp value)
     (make-ast-int :value value
                   :source-file (ast-source-file node)
                   :source-line (ast-source-line node)
                   :source-column (ast-source-column node)))
    (t
     (make-ast-quote :value value
                     :source-file (ast-source-file node)
                     :source-line (ast-source-line node)
                     :source-column (ast-source-column node)))))

(defun %compile-time-lookup (name env)
  "Look up NAME in ENV alist; return (values value t) when found, (values nil nil) otherwise.
Block environments use the same structure, so %compile-time-lookup-block is an alias."
  (let ((entry (assoc name env)))
    (when entry
      (values (cdr entry) t))))

;;; Block environments have the same alist structure — share the implementation.
(setf (symbol-function '%compile-time-lookup-block)
      #'%compile-time-lookup)

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
    (loop for form in forms
          for (value ok) = (multiple-value-list (%evaluate-ast form depth))
          unless ok return (values nil nil)
          finally (return (values value t)))))

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
     (multiple-value-bind (lhs lhs-ok) (%evaluate-ast (ast-binop-lhs node) (1- depth))
       (multiple-value-bind (rhs rhs-ok) (%evaluate-ast (ast-binop-rhs node) (1- depth))
         (if (and lhs-ok rhs-ok)
             (let ((value (%compile-time-eval-binop (ast-binop-op node) lhs rhs)))
               (if (and value (integerp value))
                   (values value t)
                   (values nil nil)))
             (values nil nil)))))
    (ast-if
     (multiple-value-bind (cond cond-ok) (%evaluate-ast (ast-if-cond node) (1- depth))
       (if cond-ok
           (%evaluate-ast (if (%compile-time-falsep cond)
                              (ast-if-else node)
                              (ast-if-then node))
                          (1- depth))
           (values nil nil))))
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
           (multiple-value-bind (value ok)
               (%evaluate-ast (ast-return-from-value node) (1- depth))
             (if ok
                 (throw tag value)
                 (values nil nil)))
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
     (let ((func (ast-call-func node)))
       (multiple-value-bind (args ok)
           (loop for arg in (ast-call-args node)
                 collect (multiple-value-list (%evaluate-ast arg (1- depth))) into results
                 finally (return (values (mapcar #'first results)
                                         (every #'second results))))
         (if ok
             (%compile-time-eval-call func args depth)
             (values nil nil)))))
    (t (values nil nil))))

;;; ── Source-location macro ────────────────────────────────────────────────────

;;; Usage: (%loc node (make-ast-foo :field val ...))
;;; Expands to: (make-ast-foo :field val ... :source-file F :source-line L :source-column C)
(defmacro %loc (node &rest make-form)
  "Append source-location keyword args from NODE to MAKE-FORM constructor."
  (let ((n (gensym "node")))
    `(let ((,n ,node))
       (,(first make-form) ,@(rest make-form)
        :source-file   (ast-source-file   ,n)
        :source-line   (ast-source-line   ,n)
        :source-column (ast-source-column ,n)))))

;;; ── AST constant fold pass ───────────────────────────────────────────────────

(defun optimize-ast (node)
  "Fold small pure constant expressions before VM lowering."
  (typecase node
    (ast-binop
     (%fold-ast-binop node
                      (optimize-ast (ast-binop-lhs node))
                      (optimize-ast (ast-binop-rhs node))))
    (ast-call
     (let* ((func      (optimize-ast (ast-call-func node)))
            (args      (mapcar #'optimize-ast (ast-call-args node)))
            (call-node (%loc node make-ast-call :func func :args args)))
       (multiple-value-bind (value ok)
           (let ((*compile-time-value-env*    *compile-time-value-env*)
                 (*compile-time-function-env* *compile-time-function-env*))
             (%evaluate-ast call-node *compile-time-eval-depth-limit*))
         (if ok (%compile-time-value->ast value node) call-node))))
    (ast-progn
     (%loc node make-ast-progn
       :forms (mapcar #'optimize-ast (ast-progn-forms node))))
    (ast-let
     (%loc node make-ast-let
       :bindings     (mapcar (lambda (b) (cons (car b) (optimize-ast (cdr b))))
                             (ast-let-bindings node))
       :declarations (ast-let-declarations node)
       :body         (mapcar #'optimize-ast (ast-let-body node))))
    (ast-if
     (%loc node make-ast-if
       :cond (optimize-ast (ast-if-cond node))
       :then (optimize-ast (ast-if-then node))
       :else (optimize-ast (ast-if-else node))))
    (ast-lambda
     (%loc node make-ast-lambda
       :params          (ast-lambda-params node)
       :optional-params (ast-lambda-optional-params node)
       :rest-param      (ast-lambda-rest-param node)
       :key-params      (ast-lambda-key-params node)
       :declarations    (ast-lambda-declarations node)
       :body            (mapcar #'optimize-ast (ast-lambda-body node))
       :env             (ast-lambda-env node)))
    (ast-defun
     (%loc node make-ast-defun
       :name            (ast-defun-name node)
       :params          (ast-defun-params node)
       :optional-params (ast-defun-optional-params node)
       :rest-param      (ast-defun-rest-param node)
       :key-params      (ast-defun-key-params node)
       :declarations    (ast-defun-declarations node)
       :body            (mapcar #'optimize-ast (ast-defun-body node))))
    (ast-defvar
     (%loc node make-ast-defvar
       :name  (ast-defvar-name node)
       :value (optimize-ast (ast-defvar-value node))
       :kind  (ast-defvar-kind node)))
    (ast-block
     (%loc node make-ast-block
       :name (ast-block-name node)
       :body (mapcar #'optimize-ast (ast-block-body node))))
    (ast-return-from
     (%loc node make-ast-return-from
       :name  (ast-return-from-name node)
       :value (optimize-ast (ast-return-from-value node))))
    (ast-setq
     (%loc node make-ast-setq
       :var   (ast-setq-var node)
       :value (optimize-ast (ast-setq-value node))))
    (ast-the
     (%loc node make-ast-the
       :type  (ast-the-type node)
       :value (optimize-ast (ast-the-value node))))
    (t node)))
