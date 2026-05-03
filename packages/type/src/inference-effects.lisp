;;;; packages/type/src/inference-effects.lisp - Effect Inference (Phase 5)
;;;
;;; Extracted from inference.lisp.
;;; infer-effects, infer-with-effects, check-body-effects,
;;; effect signature registry, and built-in effect initialisation.

(in-package :cl-cc/type)

;;; Pure-AST type table (data-driven)

(defparameter *pure-ast-effect-types*
  '(cl-cc/ast:ast-int cl-cc/ast:ast-quote cl-cc/ast:ast-var
    cl-cc/ast:ast-binop cl-cc/ast:ast-lambda
    cl-cc/ast:ast-defun cl-cc/ast:ast-defvar)
  "AST node types whose evaluation produces no side effects.
   Any node whose type appears here is pure and returns +pure-effect-row+.")

;;; Constant Effect Table — maps AST *type symbols* to their canonical effect row.
;;; This is the data-driven version of the cond form in infer-effects.
;;; Keys are the class-name symbols of AST node types (e.g., 'cl-cc/ast:ast-int).

(defvar *constant-effect-table* (make-hash-table :test #'eq)
  "Maps AST node type symbols to their constant effect rows.
   Used for data-driven effect lookup in infer-effects.")

;;; Effect Inference

(defun %infer-effects-union (forms env)
  "Return the union of effect rows inferred for each form in FORMS."
  (reduce #'effect-row-union
          (mapcar (lambda (f) (infer-effects f env)) forms)
          :initial-value +pure-effect-row+))

(defun infer-effects (ast env)
  "Infer the effect row produced by evaluating AST in environment ENV.
   Returns a type-effect-row. Pure expressions return +pure-effect-row+."
  ;; Data-driven pure check: extensible via *pure-ast-effect-types*
  (when (some (lambda (type) (typep ast type)) *pure-ast-effect-types*)
    (return-from infer-effects +pure-effect-row+))
  (typecase ast
    (cl-cc/ast:ast-print +io-effect-row+)
    (cl-cc/ast:ast-setq
     (make-type-effect-row :effects (list (make-type-effect-op :name 'state :args nil))
                           :row-var nil))
    (cl-cc/ast:ast-if
     (effect-row-union (infer-effects (cl-cc/ast:ast-if-cond ast) env)
                       (effect-row-union
                        (infer-effects (cl-cc/ast:ast-if-then ast) env)
                        (infer-effects (cl-cc/ast:ast-if-else ast) env))))
    (cl-cc/ast:ast-let
     (effect-row-union
      (%infer-effects-union (mapcar #'cdr (cl-cc/ast:ast-let-bindings ast)) env)
      (%infer-effects-union (cl-cc/ast:ast-let-body ast) env)))
    (cl-cc/ast:ast-progn (%infer-effects-union (cl-cc/ast:ast-progn-forms ast) env))
    (cl-cc/ast:ast-block (%infer-effects-union (cl-cc/ast:ast-block-body  ast) env))
    (cl-cc/ast:ast-call
     (let* ((func         (cl-cc/ast:ast-call-func ast))
            (base-effects (if (typep func 'cl-cc/ast:ast-var)
                              (lookup-effect-signature (cl-cc/ast:ast-var-name func))
                              +pure-effect-row+)))
       (effect-row-union base-effects
                         (%infer-effects-union (cl-cc/ast:ast-call-args ast) env))))
    (t (make-type-effect-row :effects nil
                             :row-var (fresh-type-var "eff")))))

(defun infer-with-effects (ast env)
  "Infer type, substitution, AND effect row for AST.
   Returns (values type substitution effect-row)."
  (multiple-value-bind (type subst) (infer ast env)
    (let ((effects (infer-effects ast env)))
      (values type subst effects))))

(defun check-body-effects (asts declared-effects env)
  "Check that the union of effects in ASTS is a subset of DECLARED-EFFECTS.
   Signals type-inference-error if the body has undeclared effects."
  (let ((actual-effects (%infer-effects-union asts env)))
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
                  :effects (list (make-type-effect-op :name 'error :args nil))
                  :row-var nil))
      (state-row (make-type-effect-row
                  :effects (list (make-type-effect-op :name 'state :args nil))
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
    (register-effect-signature op state-row))
  ;; Populate *constant-effect-table* with the per-AST-type defaults.
  (dolist (node-type *pure-ast-effect-types*)
    (setf (gethash node-type *constant-effect-table*) +pure-effect-row+))
  (setf (gethash 'cl-cc/ast:ast-print *constant-effect-table*) io-row)
  (setf (gethash 'cl-cc/ast:ast-setq  *constant-effect-table*) state-row))
