(in-package :cl-cc)

;;; CL-CC Macro System
;;; A complete macro system implementation with:
;;; - Full destructuring-bind for lambda lists
;;; - Environment classes for lexical scoping
;;; - Macro expansion (single and full)
;;; - Built-in macros for bootstrap

(defclass macro-env ()
  ((macros :initform (make-hash-table :test 'eq) :reader macro-env-table))
  (:documentation "Global environment for macro definitions."))

;;; Macro Environment and Registration - also available at compile-time

(defvar *macro-environment* (make-instance 'macro-env)
  "Global macro environment for macro definitions.")

(defun register-macro (name expander)
  "Register NAME as a macro with EXPANDER function in the global environment."
  (setf (gethash name (macro-env-table *macro-environment*)) expander))

(defun lookup-macro (name &optional env)
  "Look up macro NAME in the global macro environment."
  (declare (ignore env))
  (gethash name (macro-env-table *macro-environment*)))

;;; Macro Expansion

(defun our-macroexpand-1 (form &optional env)
  "Perform a single macro expansion on FORM.
   Returns (VALUES expanded-form expanded-p)."
  (if (and (consp form) (symbolp (car form)))
      (let ((macro-fn (lookup-macro (car form) env)))
        (if macro-fn
            (let ((expanded (funcall macro-fn form env)))
              (if (equal expanded form)
                  (values form nil)
                  (values expanded t)))
            (values form nil)))
      (values form nil)))

(defun %our-macroexpand-all-recursive (form env)
  "Recursively expand FORM after a single expansion step."
  (multiple-value-bind (expanded expanded-p)
      (our-macroexpand-1 form env)
    (if expanded-p
        (%our-macroexpand-all-recursive expanded env)
        (typecase form
          (cons
           (mapcar (lambda (x) (%our-macroexpand-all-recursive x env)) form))
          (t form)))))

(defun our-macroexpand (form &optional env)
  "Fully expand FORM by repeatedly applying macroexpand-1.
   Returns (VALUES expanded-form expanded-p)."
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expanded step-expanded-p)
          (our-macroexpand-1 form env)
        (unless step-expanded-p
          (return (values form expanded-p)))
        (setf form expanded
              expanded-p t)))))

(defun %expand-quasiquote (template)
  "Transform a quasiquote template into list/cons/append calls.
Handles (cl-cc::unquote x) and (cl-cc::unquote-splicing x) within template."
  (cond
    ;; (cl-cc::unquote x) at top level => just x
    ((and (consp template) (eq (car template) 'cl-cc::unquote))
     (second template))
    ;; A list => process each element for unquote/splicing
    ((consp template)
     (let ((parts nil))
       (dolist (elem template)
         (cond
           ((and (consp elem) (eq (car elem) 'cl-cc::unquote))
            (push (list 'list (second elem)) parts))
           ((and (consp elem) (eq (car elem) 'cl-cc::unquote-splicing))
            (push (second elem) parts))
           (t
            (push (list 'list (%expand-quasiquote elem)) parts))))
       (let ((reversed (nreverse parts)))
         (if (= (length reversed) 1)
             (first reversed)
             (cons 'append reversed)))))
    ;; Atom => quote it
    (t (list 'quote template))))

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (cond
    ;; Quasiquote — expand before further processing
    ((and (consp form) (eq (car form) 'cl-cc::backquote))
      (our-macroexpand-all (%expand-quasiquote (second form)) env))
    ;; Quote — never recurse into quoted data
    ((and (consp form) (eq (car form) 'quote))
      form)
    ;; General: try macro expansion first, then recurse
    (t (%our-macroexpand-all-recursive form env))))

;;; Macro Definition Macro

(defmacro our-defmacro (name lambda-list &body body)
  "Define NAME as a macro with LAMBDA-LIST and BODY.
   The macro expander function receives (FORM ENV) as arguments."
  (let* ((form-var (gensym "FORM"))
         (env-var (gensym "ENV"))
         (info (parse-lambda-list lambda-list))
         (env-sym (lambda-list-info-environment info)))
    `(register-macro ',name
                     (lambda (,form-var ,env-var)
                       ,@(if env-sym
                             `((let ((,env-sym ,env-var))
                                 (let* ,(generate-lambda-bindings lambda-list form-var)
                                   ,@body)))
                             `((declare (ignore ,env-var))
                               (let* ,(generate-lambda-bindings lambda-list form-var)
                                 ,@body)))))))
