;;;; packages/compile/src/codegen-fold.lisp — Constant-Fold Data and Helpers
;;;
;;; Contains:
;;;   - %evaluate-ast-then        CPS continuation helper
;;;   - %fold-ast-binop           constant-fold a binop node with already-folded operands
;;;   - *compile-time-eval-fns*   data table: symbol → evaluator lambda
;;;   - *compile-time-value-env*  compile-time binding state vars
;;;   - %ast-constant-node-p, %ast->compile-time-value, %compile-time-lookup, etc.
;;;
;;; %loc macro and optimize-ast pass are in codegen-fold-optimize.lisp.
;;;
;;; Load order: after codegen-core, before codegen-fold-eval.

(in-package :cl-cc/compile)

;;; ── CPS evaluation helper ────────────────────────────────────────────────────
;;;
;;; %evaluate-ast-then encodes the compile-time short-circuit: evaluate NODE at
;;; DEPTH-1, then pass the value to CONTINUATION — or silently return
;;; (values nil nil). This keeps the compile-time partial evaluator in explicit
;;; continuation-passing style without requiring a macro layer.

(defun %evaluate-ast-then (node depth continuation)
  (multiple-value-bind (value ok)
      (%evaluate-ast node (1- depth))
    (if ok
        (funcall continuation value)
        (values nil nil))))

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

;;; Data-driven constant folding for binary operations.
;;; Each entry maps an operator symbol to a (lambda (lv rv) → (values result foldable-p)) function.
;;; Adding new foldable operators requires one data entry, not a new case branch.

(defparameter *fold-binop-specs*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (sym fn) (setf (gethash sym ht) fn)))
      (reg '+ (lambda (lv rv) (values (+ lv rv) t)))
      (reg '- (lambda (lv rv) (values (- lv rv) t)))
      (reg '* (lambda (lv rv) (values (* lv rv) t)))
      (reg '/ (lambda (lv rv)
                (when (not (zerop rv))
                  (let ((q (/ lv rv)))
                    (when (integerp q) (values q t))))))
      (reg 'mod (lambda (lv rv)
                  (when (not (zerop rv)) (values (mod lv rv) t))))
      (reg 'rem (lambda (lv rv)
                  (when (not (zerop rv)) (values (rem lv rv) t)))))
    ht)
  "Data table for constant-foldable binops: op → (lambda (lv rv) → (values result foldable-p)).")

(defun %fold-ast-binop (node lhs rhs)
  (let ((lv (%ast-constant-number-value lhs))
        (rv (%ast-constant-number-value rhs)))
    (if (and lv rv)
        (let* ((folder (gethash (ast-binop-op node) *fold-binop-specs*))
               (value (and folder (multiple-value-bind (v ok) (funcall folder lv rv)
                                    (and ok v)))))
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
  "Look up NAME in ENV alist; return (values value t) when found, (values nil nil) otherwise."
  (let ((entry (assoc name env)))
    (when entry
      (values (cdr entry) t))))

(defparameter *compile-time-simple-binops*
  '((+ . +) (- . -) (* . *))
  "Alist of binary operators foldable as (op lhs rhs) with no guards required.
Does not include /  (needs zerop guard + integerp check)
or 1+/1- (unary, require rhs = nil).")

;;; Evaluation engine (%compile-time-eval-binop, %compile-time-eval-call,
;;; %evaluate-ast-sequence, %evaluate-ast) → see codegen-fold-eval.lisp.
