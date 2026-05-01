(in-package :cl-cc/expand)

;; Defined in expander-data.lisp, but declare it here too so compile-file sees
;; the intended dynamic binding even when this file is compiled first.
(defvar *macro-eval-fn*)

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

(defvar *macroexpand-step-cache*
  (make-hash-table :test #'eq :weakness :key)
  "Weak cache mapping ENV → (equal-hash-table FORM → (EXPANDED-P . VALUE)).")

(defvar *macroexpand-all-cache*
  (make-hash-table :test #'eq :weakness :key)
  "Weak cache mapping ENV → (equal-hash-table FORM → recursively EXPANDED-FORM).")

(defvar *compiler-macro-table*)

(defvar *macroexpansion-cache-lock*
  #+(and sb-thread (not cl-cc-self-hosting)) (sb-thread:make-mutex :name "macroexpansion-cache")
  #-(and sb-thread (not cl-cc-self-hosting)) nil)

(defmacro %with-macroexpansion-cache-lock (&body body)
  #+(and sb-thread (not cl-cc-self-hosting)) `(sb-thread:with-mutex (*macroexpansion-cache-lock*) ,@body)
  #-(and sb-thread (not cl-cc-self-hosting)) `(progn ,@body))

(defun %macroexpansion-cache-table (root env)
  (or (gethash env root)
      (setf (gethash env root)
            (make-hash-table :test #'equal))))

(defun %macroexpansion-cache-lookup (form env &optional (root *macroexpand-step-cache*))
  (%with-macroexpansion-cache-lock
    (gethash form (%macroexpansion-cache-table root env))))

(defun %macroexpansion-cache-store (form env expanded &optional (root *macroexpand-step-cache*))
  (%with-macroexpansion-cache-lock
    (setf (gethash form (%macroexpansion-cache-table root env)) expanded)))

(defun %reset-macroexpansion-caches ()
  "Drop cached macroexpansions after macro environment changes."
  (%with-macroexpansion-cache-lock
    (setf *macroexpand-step-cache* (make-hash-table :test #'eq :weakness :key)
          *macroexpand-all-cache* (make-hash-table :test #'eq :weakness :key))))

(defun %contains-uninterned-symbol-p (node)
  "Return T when NODE or any subtree contains an uninterned symbol.
Gensym-hygienic expansions are never cached."
  (typecase node
    (symbol (null (symbol-package node)))
    (cons   (or (%contains-uninterned-symbol-p (car node))
                (%contains-uninterned-symbol-p (cdr node))))
    (t nil)))

(defun %cacheable-macroexpansion-p (form)
  "Return T when FORM is safe to reuse from the macroexpansion cache."
  (not (%contains-uninterned-symbol-p form)))

(defun register-macro (name expander)
  "Register NAME as a macro with EXPANDER in the global environment.
EXPANDER may be either a host function or a descriptor consumed by
`invoke-registered-expander'."
  (setf (gethash name (macro-env-table *macro-environment*)) expander)
  (%reset-macroexpansion-caches)
  name)

(defun register-compiler-macro (name expander)
  "Register NAME as a compiler macro expander in the global environment.
EXPANDER may be either a host function or a descriptor consumed by
`invoke-registered-expander'."
  (setf (gethash name *compiler-macro-table*) expander)
  (%reset-macroexpansion-caches)
  name)

(defparameter *expander-descriptor-kinds*
  '(:macro-expander :compiler-macro-expander :register-macro-expander)
  "Valid :kind values for data-backed macro expander descriptors.")

(defun %expander-descriptor-p (object)
  "Return T when OBJECT is a data-backed macro expander descriptor."
  (and (listp object)
       (not (null (member (getf object :kind)
                          *expander-descriptor-kinds*
                          :test #'eq)))))

(defun %maybe-postprocess-expansion (result descriptor env)
  "Apply post-expansion processing to RESULT if the descriptor requests it."
  (case (getf descriptor :post-expand)
    (:our-macroexpand-all (our-macroexpand-all result env))
    (otherwise result)))

(defun %invoke-expander-descriptor (descriptor form env)
  "Evaluate DESCRIPTOR against FORM and ENV through `*macro-eval-fn*'."
  (case (getf descriptor :kind)
    (:macro-expander
     (let* ((lambda-list (getf descriptor :lambda-list))
            (body        (getf descriptor :body))
            (form-var    (gensym "FORM"))
            (eval-form   `(let ((,form-var ',form))
                            (let* ,(generate-lambda-bindings lambda-list form-var)
                              ,@body))))
       (%maybe-postprocess-expansion (funcall *macro-eval-fn* eval-form) descriptor env)))
    (:compiler-macro-expander
     (let* ((lambda-list (getf descriptor :lambda-list))
            (body        (getf descriptor :body))
            (form-var    (gensym "FORM"))
            (eval-form   `(let ((,form-var ',form))
                            (let* ,(destructure-lambda-list lambda-list `(cdr ,form-var))
                              ,@body))))
       (funcall *macro-eval-fn* eval-form)))
    (:register-macro-expander
     (let* ((parameters (getf descriptor :parameters))
            (body       (getf descriptor :body))
            (form-var   (first parameters))
            (env-var    (second parameters))
            (bindings   (append (when form-var `((,form-var ',form)))
                                (when env-var  `((,env-var  ',env)))))
            (eval-form  `(let ,bindings ,@body)))
       (%maybe-postprocess-expansion (funcall *macro-eval-fn* eval-form) descriptor env)))
    (otherwise
     (error "Unknown expander descriptor kind: ~S" descriptor))))

(defun invoke-registered-expander (expander form env)
  "Invoke EXPANDER on FORM and ENV.
Supports both host functions and descriptor-backed expanders."
  (cond
    ((functionp expander) (funcall expander form env))
    ((%expander-descriptor-p expander) (%invoke-expander-descriptor expander form env))
    (t (error "Unsupported expander representation: ~S" expander))))

(defun lookup-macro (name)
  "Look up macro NAME in the global macro environment."
  (gethash name (macro-env-table *macro-environment*)))

(defun lookup-compiler-macro (name)
  "Look up compiler macro NAME in the global compiler-macro environment."
  (gethash name *compiler-macro-table*))

;;; Macro Expansion

(defun %step-cache-and-return (form env result expanded-p)
  "Store (expanded-p . result) in the step cache for FORM/ENV and return (values result expanded-p)."
  (when (and (%cacheable-macroexpansion-p form)
             (or (not expanded-p) (%cacheable-macroexpansion-p result)))
    (%macroexpansion-cache-store form env (cons expanded-p result) *macroexpand-step-cache*))
  (values result expanded-p))

(defun our-macroexpand-1 (form &optional env)
  "Perform a single macro expansion on FORM.
   Returns (VALUES expanded-form expanded-p)."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-step-cache*)
      (when hitp
        (return-from our-macroexpand-1 (values (cdr cached) (car cached))))))
  (if (and (consp form) (symbolp (car form)))
      (let ((macro-fn (lookup-macro (car form))))
        (if macro-fn
            (let ((expanded (invoke-registered-expander macro-fn form env)))
              (if (equal expanded form)
                  (%step-cache-and-return form env form nil)
                  (%step-cache-and-return form env expanded t)))
            (%step-cache-and-return form env form nil)))
      (%step-cache-and-return form env form nil)))

(defun %cache-all-result (form env result)
  "Store RESULT in the all-cache for FORM/ENV when both are safe to cache, then return RESULT."
  (when (and (%cacheable-macroexpansion-p form)
             (%cacheable-macroexpansion-p result))
    (%macroexpansion-cache-store form env result *macroexpand-all-cache*))
  result)

(defun %our-macroexpand-all-recursive (form env)
  "Recursively expand FORM after a single expansion step."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-all-cache*)
      (when hitp (return-from %our-macroexpand-all-recursive cached))))
  (multiple-value-bind (expanded expanded-p)
      (our-macroexpand-1 form env)
    (if expanded-p
        (%cache-all-result form env (%our-macroexpand-all-recursive expanded env))
        (typecase form
          (cons (%cache-all-result form env
                                   (mapcar (lambda (x) (%our-macroexpand-all-recursive x env)) form)))
          (t    (%cache-all-result form env form))))))

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

(defun %qq-head-p (form name)
  "Return T when FORM is a list headed by a symbol named NAME."
  (and (consp form) (symbolp (car form)) (string= (symbol-name (car form)) name)))

(defun %expand-qq-element (elem)
  "Expand a single element of a quasiquote list template."
  (cond
    ((%qq-head-p elem "UNQUOTE")          (list 'list (second elem)))
    ((%qq-head-p elem "UNQUOTE-SPLICING") (second elem))
    (t                                    (list 'list (%expand-quasiquote elem)))))

(defun %expand-quasiquote (template)
  "Transform a quasiquote template into list/cons/append calls.
Handles (unquote x) and (unquote-splicing x) within template.
These symbols live in :cl-cc/bootstrap so both parse and expand share them."
  (cond
    ((%qq-head-p template "UNQUOTE")
     (second template))
    ((consp template)
     (let ((parts (mapcar #'%expand-qq-element template)))
       (if (= (length parts) 1) (first parts) (cons 'append parts))))
    (t
     (list 'quote template))))

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (cond
    ((%qq-head-p form "BACKQUOTE")
     (our-macroexpand-all (%expand-quasiquote (second form)) env))
    ((and (consp form) (eq (car form) 'quote))
     form)
    (t (%our-macroexpand-all-recursive form env))))

;;; Macro Definition Macro

(defmacro our-defmacro (name lambda-list &body body)
  "Define NAME as a macro with LAMBDA-LIST and BODY.
   The macro expander function receives (FORM ENV) as arguments."
  (let* ((form-var (gensym "FORM"))
         (env-var  (gensym "ENV"))
         (info     (parse-lambda-list lambda-list))
         (env-sym  (lambda-list-info-environment info))
         (bindings (generate-lambda-bindings lambda-list form-var))
         (expander-body
           (if env-sym
               `((let ((,env-sym ,env-var))
                   (let* ,bindings ,@body)))
               `((declare (ignore ,env-var))
                 (let* ,bindings ,@body)))))
    `(register-macro ',name (lambda (,form-var ,env-var) ,@expander-body))))

;;; Wire expand functions into VM hooks for runtime macroexpand support
(defun %vm-install-macroexpand-hooks-if-available ()
  (when cl-cc/bootstrap::*vm-macroexpand-hook-installer*
    (funcall cl-cc/bootstrap::*vm-macroexpand-hook-installer* #'our-macroexpand-1 #'our-macroexpand)))

#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  (%vm-install-macroexpand-hooks-if-available))
