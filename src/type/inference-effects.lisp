;;;; src/type/inference-effects.lisp - Effect Inference (Phase 5)
;;;
;;; Extracted from inference.lisp.
;;; infer-effects, infer-with-effects, check-body-effects,
;;; effect signature registry, and built-in effect initialisation.

(in-package :cl-cc/type)

;;; Pure-AST type table (data-driven)

(defparameter *pure-ast-effect-types*
  '(cl-cc:ast-int cl-cc:ast-quote cl-cc:ast-var
    cl-cc:ast-binop cl-cc:ast-lambda
    cl-cc:ast-defun cl-cc:ast-defvar)
  "AST node types whose evaluation produces no side effects.
   Any node whose type appears here is pure and returns +pure-effect-row+.")

;;; Effect Inference

(defun infer-effects (ast env)
  "Infer the effect row produced by evaluating AST in environment ENV.
   Returns a type-effect-row. Pure expressions return +pure-effect-row+."
  (flet ((union-list (forms)
           (reduce #'effect-row-union
                   (mapcar (lambda (f) (infer-effects f env)) forms)
                   :initial-value +pure-effect-row+)))
    (cond
      ;; Data-driven pure check: any type in *pure-ast-effect-types* → no effects
      ((some (lambda (type) (typep ast type)) *pure-ast-effect-types*)
       +pure-effect-row+)

      ((typep ast 'cl-cc:ast-print) +io-effect-row+)

      ((typep ast 'cl-cc:ast-setq)
       (make-type-effect-row :effects (list (make-type-effect :name 'state))
                             :row-var nil))

      ((typep ast 'cl-cc:ast-if)
       (effect-row-union (infer-effects (cl-cc:ast-if-cond ast) env)
                         (effect-row-union
                          (infer-effects (cl-cc:ast-if-then ast) env)
                          (infer-effects (cl-cc:ast-if-else ast) env))))

      ((typep ast 'cl-cc:ast-let)
       (effect-row-union
        (union-list (mapcar #'cdr (cl-cc:ast-let-bindings ast)))
        (union-list (cl-cc:ast-let-body ast))))

      ((typep ast 'cl-cc:ast-progn)  (union-list (cl-cc:ast-progn-forms ast)))
      ((typep ast 'cl-cc:ast-block)  (union-list (cl-cc:ast-block-body  ast)))

      ((typep ast 'cl-cc:ast-call)
       (let* ((func         (cl-cc:ast-call-func ast))
              (base-effects (if (typep func 'cl-cc:ast-var)
                                (lookup-effect-signature (cl-cc:ast-var-name func))
                                +pure-effect-row+)))
         (effect-row-union base-effects
                           (union-list (cl-cc:ast-call-args ast)))))

      (t (make-type-effect-row :effects nil
                               :row-var (make-type-variable "eff"))))))

(defun infer-with-effects (ast env)
  "Infer type, substitution, AND effect row for AST.
   Returns (values type substitution effect-row)."
  (multiple-value-bind (type subst) (infer ast env)
    (let ((effects (infer-effects ast env)))
      (values type subst effects))))

(defun check-body-effects (asts declared-effects env)
  "Check that the union of effects in ASTS is a subset of DECLARED-EFFECTS.
   Signals type-inference-error if the body has undeclared effects."
  (let ((actual-effects
         (reduce #'effect-row-union
                 (mapcar (lambda (a) (infer-effects a env)) asts)
                 :initial-value +pure-effect-row+)))
    (unless (effect-row-subset-p actual-effects declared-effects)
      (error 'type-inference-error
             :message (format nil "Function has undeclared effects: ~A (declared: ~A)"
                               (type-to-string actual-effects)
                               (type-to-string declared-effects))))))

;;; Effect Signature Table

(defvar *effect-signature-table* (make-hash-table :test #'eq)
  "Maps operation names (symbols) to their effect rows (type-effect-row).
   Operations not in this table are treated as pure ({}).")

(defun register-effect-signature (op-name effect-row)
  "Register that OP-NAME has the given EFFECT-ROW."
  (setf (gethash op-name *effect-signature-table*) effect-row))

(defun lookup-effect-signature (op-name)
  "Return the effect-row for OP-NAME, or +pure-effect-row+ if not registered."
  (gethash op-name *effect-signature-table* +pure-effect-row+))

;; Initialize built-in effect signatures
(let ((io-row +io-effect-row+)
      (error-row (make-type-effect-row
                  :effects (list (make-type-effect :name 'error))
                  :row-var nil))
      (state-row (make-type-effect-row
                  :effects (list (make-type-effect :name 'state))
                  :row-var nil)))
  ;; IO effects
  (dolist (op '(print format read read-line read-char write-char
                vm-print vm-format-inst vm-read-char vm-read-line
                vm-write-string vm-fresh-line))
    (register-effect-signature op io-row))
  ;; Error effects
  (dolist (op '(error signal warn vm-signal-error))
    (register-effect-signature op error-row))
  ;; State effects (global mutation)
  (dolist (op '(setq vm-setq setf vm-set-global))
    (register-effect-signature op state-row)))

;;; Exports

(export '(;; Phase 5 effect type inference
          *pure-ast-effect-types*
          infer-effects
          infer-with-effects
          check-body-effects
          register-effect-signature
          lookup-effect-signature
          *effect-signature-table*))
