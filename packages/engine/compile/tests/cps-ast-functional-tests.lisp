;;;; tests/unit/compile/cps-ast-functional-tests.lisp
;;;; Unit tests for src/compile/cps-ast-functional.lisp
;;;;
;;;; Covers: cps-transform-ast for functional/multi-value forms:
;;;;   ast-multiple-value-bind, ast-multiple-value-call,
;;;;   ast-multiple-value-prog1, ast-defvar, ast-handler-case,
;;;;   ast-make-instance, ast-slot-value, ast-set-slot-value,
;;;;   ast-defclass, ast-set-gethash.
;;;;
;;;; Structural inspection — tests verify the shape of produced S-expressions
;;;; rather than evaluating them (which would require a full runtime).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %cps-k (node)
  "CPS-transform NODE with a fixed continuation symbol K."
  (cl-cc/compile::cps-transform-ast node 'k))

(defun %form-contains-p (form sym)
  "Return T if SYM appears anywhere in the s-expression FORM."
  (cond ((eq form sym) t)
        ((consp form) (or (%form-contains-p (car form) sym)
                          (%form-contains-p (cdr form) sym)))
        (t nil)))

;;; ─── ast-defvar ──────────────────────────────────────────────────────────────

(deftest cps-defvar-with-value-contains-defvar-and-funcall-k
  "ast-defvar with a value threads through the value then calls defvar."
  (let* ((node (cl-cc/ast::make-ast-defvar
                :name '*x* :kind 'defvar
                :value (cl-cc/ast::make-ast-int :value 0)))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'defvar))
    (assert-true (%form-contains-p result 'funcall))))

(deftest cps-defvar-without-value-immediate-defvar
  "ast-defvar without a value emits (defvar name) + (funcall k name) directly."
  (let* ((node (cl-cc/ast::make-ast-defvar :name '*y* :kind 'defvar :value nil))
         (result (%cps-k node)))
    ;; Top-level progn: (progn (defvar *y*) (funcall k *y*))
    (assert-eq 'progn (car result))
    (assert-true (%form-contains-p result 'defvar))
    (assert-true (%form-contains-p result 'funcall))))

;;; ─── ast-handler-case ────────────────────────────────────────────────────────

(deftest cps-handler-case-produces-host-handler-case
  "ast-handler-case emits a host handler-case form."
  (let* ((body-node (cl-cc/ast::make-ast-int :value 0))
         (node (cl-cc/ast::make-ast-handler-case
                :form (cl-cc/ast::make-ast-int :value 1)
                :clauses (list (list 'error 'e body-node))))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'handler-case))))

;;; ─── ast-make-instance ───────────────────────────────────────────────────────

(deftest cps-make-instance-contains-make-instance-call
  "ast-make-instance CPS output contains a host (make-instance ...) call."
  (let* ((node (cl-cc/ast::make-ast-make-instance
                :class (cl-cc/ast::make-ast-quote :value 'dog)
                :initargs nil))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'make-instance))))

(deftest cps-make-instance-threads-initargs
  "ast-make-instance with initargs contains nested lambdas for each arg."
  (let* ((node (cl-cc/ast::make-ast-make-instance
                :class (cl-cc/ast::make-ast-quote :value 'point)
                :initargs (list :x (cl-cc/ast::make-ast-int :value 1)
                                :y (cl-cc/ast::make-ast-int :value 2))))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'make-instance))
    ;; Should have nested lambdas for x and y args
    (assert-true (%form-contains-p result 'lambda))))

;;; ─── ast-slot-value ──────────────────────────────────────────────────────────

(deftest cps-slot-value-contains-host-slot-value
  "ast-slot-value CPS output contains (slot-value obj 'slot)."
  (let* ((node (cl-cc/ast::make-ast-slot-value
                :object (cl-cc/ast::make-ast-var :name 'obj)
                :slot 'x))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'slot-value))))

;;; ─── ast-set-slot-value ──────────────────────────────────────────────────────

(deftest cps-set-slot-value-contains-setf-slot-value
  "ast-set-slot-value CPS output contains (setf (slot-value ...) ...)."
  (let* ((node (cl-cc/ast::make-ast-set-slot-value
                :object (cl-cc/ast::make-ast-var :name 'obj)
                :slot 'x
                :value (cl-cc/ast::make-ast-int :value 42)))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'slot-value))
    (assert-true (%form-contains-p result 'setf))))

;;; ─── ast-defclass ────────────────────────────────────────────────────────────

(deftest cps-defclass-produces-progn-defclass-funcall-k
  "ast-defclass emits (progn (defclass ...) (funcall k 'name))."
  (let* ((node (cl-cc/ast::make-ast-defclass
                :name 'animal :superclasses nil :slots nil))
         (result (%cps-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defclass (caadr result))
    (let ((last-form (car (last result))))
      (assert-eq 'funcall (car last-form))
      (assert-eq 'k (second last-form)))))

(deftest cps-defclass-with-superclasses
  "ast-defclass with superclasses includes them in the defclass form."
  (let* ((node (cl-cc/ast::make-ast-defclass
                :name 'dog :superclasses '(animal) :slots nil))
         (result (%cps-k node)))
    (let ((defclass-form (cadr result)))
      (assert-equal '(animal) (third defclass-form)))))

;;; ─── ast-defgeneric ──────────────────────────────────────────────────────────

(deftest cps-defgeneric-with-multiple-params
  "ast-defgeneric with multiple params passes them to host defgeneric."
  (let* ((node (cl-cc/ast::make-ast-defgeneric :name 'draw :params '(shape context)))
         (result (%cps-k node)))
    (let ((defgeneric-form (cadr result)))
      (assert-eq 'defgeneric (car defgeneric-form))
      (assert-equal '(shape context) (third defgeneric-form)))))

;;; ─── ast-defmethod ───────────────────────────────────────────────────────────

(deftest cps-defmethod-produces-progn-defmethod-funcall-k
  "ast-defmethod emits (progn (defmethod ...) (funcall k 'name))."
  (let* ((node (cl-cc/ast::make-ast-defmethod
                :name 'area
                :params '(shape)
                :specializers '(circle)
                :body (list (cl-cc/ast::make-ast-int :value 0))))
         (result (%cps-k node)))
    (assert-eq 'progn (car result))
    (assert-eq 'defmethod (caadr result))
    (assert-true (%form-contains-p result 'funcall))))

;;; ─── ast-set-gethash ─────────────────────────────────────────────────────────

(deftest cps-set-gethash-contains-setf-gethash
  "ast-set-gethash CPS output contains (setf (gethash ...) ...)."
  (let* ((node (cl-cc/ast::make-ast-set-gethash
                :key (cl-cc/ast::make-ast-quote :value :x)
                :table (cl-cc/ast::make-ast-var :name 'ht)
                :value (cl-cc/ast::make-ast-int :value 99)))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'gethash))
    (assert-true (%form-contains-p result 'setf))))

;;; ─── ast-multiple-value-bind ─────────────────────────────────────────────────

(deftest cps-mvb-with-ast-values-produces-multiple-value-bind
  "ast-mvb with an ast-values producer emits a host multiple-value-bind."
  (let* ((values-node (cl-cc/ast::make-ast-values
                        :forms (list (cl-cc/ast::make-ast-int :value 1)
                                     (cl-cc/ast::make-ast-int :value 2))))
         (node (cl-cc/ast::make-ast-multiple-value-bind
                :vars '(a b)
                :values-form values-node
                :body (list (cl-cc/ast::make-ast-var :name 'a))))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'multiple-value-bind))))

(deftest cps-mvb-without-ast-values-binds-primary
  "ast-mvb with a non-ast-values producer binds primary value; rest default to NIL."
  (let* ((call-node (cl-cc/ast::make-ast-call :func 'floor :args (list (cl-cc/ast::make-ast-int :value 7))))
         (node (cl-cc/ast::make-ast-multiple-value-bind
                :vars '(q r)
                :values-form call-node
                :body (list (cl-cc/ast::make-ast-var :name 'q))))
         (result (%cps-k node)))
    ;; Should contain a let binding r to nil
    (assert-true (%form-contains-p result 'let))
    (assert-true (%form-contains-p result 'nil))))

;;; ─── ast-multiple-value-call ─────────────────────────────────────────────────

(deftest cps-mvc-contains-apply
  "ast-multiple-value-call CPS output spreads args via APPLY."
  (let* ((node (cl-cc/ast::make-ast-multiple-value-call
                :func (cl-cc/ast::make-ast-var :name 'f)
                :args (list (cl-cc/ast::make-ast-int :value 1))))
         (result (%cps-k node)))
    (assert-true (%form-contains-p result 'apply))))

(deftest cps-mvc-collects-into-list
  "ast-multiple-value-call uses a local list to collect args before apply."
  (let* ((node (cl-cc/ast::make-ast-multiple-value-call
                :func (cl-cc/ast::make-ast-var :name 'list)
                :args (list (cl-cc/ast::make-ast-int :value 1)
                            (cl-cc/ast::make-ast-int :value 2))))
         (result (%cps-k node)))
    ;; Should have push + nreverse pattern
    (assert-true (or (%form-contains-p result 'push)
                     (%form-contains-p result 'nreverse)))))

;;; ─── ast-multiple-value-prog1 ────────────────────────────────────────────────

(deftest cps-mvprog1-delivers-first-form-result
  "ast-multiple-value-prog1 CPS output binds the first form result and delivers it last."
  (let* ((node (cl-cc::make-ast-multiple-value-prog1
                :first (cl-cc/ast::make-ast-int :value 42)
                :forms (list (cl-cc/ast::make-ast-call :func 'print
                                                    :args (list (cl-cc/ast::make-ast-int :value 0))))))
         (result (%cps-k node)))
    ;; The result register is bound first, side effects follow, then funcall k result
    (assert-true (%form-contains-p result 'funcall))
    (assert-true (%form-contains-p result 'lambda))))
