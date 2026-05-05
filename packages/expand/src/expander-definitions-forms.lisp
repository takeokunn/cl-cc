(in-package :cl-cc/expand)

;;; Core definition forms: function/class/flow/type-definition expanders.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*documentation-table*)
    (defvar *documentation-table* (make-hash-table :test #'equal))))

;; defun — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for defun (form)
  (let* ((fn-name   (second form))
         (raw-body  (cdddr form))
         (docstring (%extract-docstring raw-body))
         (body      (%strip-docstring raw-body)))
    (when (and docstring (symbolp fn-name))
      (setf (gethash (list fn-name 'function) *documentation-table*) docstring))
    (if (and (>= (length form) 4)
             (symbolp fn-name)
             (listp (third form))
             (lambda-list-has-typed-p (third form)))
        (expand-typed-defun-or-lambda 'defun fn-name (third form)
                                      (if docstring (cons docstring body) body))
        (list* 'defun fn-name
               (expand-lambda-list-defaults (third form))
               (append (when docstring (list docstring))
                       (mapcar #'compiler-macroexpand-all body))))))

;; lambda — typed params get check-type assertions; untyped: expand defaults + body
(define-expander-for lambda (form)
  (let ((body (%strip-docstring (cddr form))))
    (if (and (>= (length form) 3)
             (listp (second form))
             (lambda-list-has-typed-p (second form)))
        (expand-typed-defun-or-lambda 'lambda nil (second form) body)
        (list* 'lambda
               (expand-lambda-list-defaults (second form))
               (mapcar #'compiler-macroexpand-all body)))))

;; defclass — register accessor mappings; expand :initform values in slot specs
(define-expander-for defclass (form)
  (let ((class-name (second form))
        (slot-specs (fourth form))
        (class-options (nthcdr 4 form)))
    (register-defclass-accessors class-name slot-specs)
    (append (list 'defclass class-name
                  (mapcar #'compiler-macroexpand-all (third form))
                  (when (listp slot-specs)
                    (mapcar #'expand-defclass-slot-spec slot-specs)))
            class-options)))

;; progn — process forms sequentially, eagerly registering any defmacro siblings
(define-expander-for progn (form)
  (expand-progn-with-eager-defmacro (cdr form)))

;; deftype — register type alias at expand time; returns (quote name)
(define-expander-for deftype (form)
  (let ((name (second form)))
    (cond
      ((and (>= (length form) 4) (symbolp name) (listp (third form))
            (every #'symbolp (third form)))
       (let* ((lambda-list (third form))
              (body (cdddr form))
              (expansion (if (= (length body) 1) (first body) `(progn ,@body))))
          (if (null lambda-list)
              (let ((result (our-eval expansion)))
                (cl-cc/type:register-type-alias name result))
              (let ((expander (lambda (&rest args)
                                (funcall *macro-eval-fn*
                                         (cons `(lambda ,lambda-list ,@body) args)))))
                (cl-cc/type:register-type-alias name expander)))))
      ((and (= (length form) 3) (symbolp name))
       (cl-cc/type:register-type-alias name (third form))))
    `(quote ,name)))
