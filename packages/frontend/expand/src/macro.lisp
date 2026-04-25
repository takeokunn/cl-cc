(in-package :cl-cc/expand)

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
  #+cl-cc-self-hosting nil
  #-(or sb-thread cl-cc-self-hosting) nil)

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

(defun %contains-uninterned-symbol-p (form)
  "Return T when FORM contains any uninterned symbol.
We avoid caching such expansions so hygiene-related gensyms stay fresh across
independent macroexpansions." 
  (labels ((walk (node)
             (typecase node
               (symbol (null (symbol-package node)))
               (cons (or (walk (car node))
                         (walk (cdr node))))
               (t nil))))
    (walk form)))

(defun %cacheable-macroexpansion-p (form)
  "Return T when FORM is safe to reuse from the macroexpansion cache."
  (not (%contains-uninterned-symbol-p form)))

(defun register-macro (name expander)
  "Register NAME as a macro with EXPANDER function in the global environment."
  (setf (gethash name (macro-env-table *macro-environment*)) expander)
  (%reset-macroexpansion-caches)
  name)

(defun register-compiler-macro (name expander)
  "Register NAME as a compiler macro expander in the global environment."
  (setf (gethash name *compiler-macro-table*) expander)
  (%reset-macroexpansion-caches)
  name)

(defun lookup-macro (name &optional env)
  "Look up macro NAME in the global macro environment."
  (declare (ignore env))
  (gethash name (macro-env-table *macro-environment*)))

(defun lookup-compiler-macro (name &optional env)
  "Look up compiler macro NAME in the global compiler-macro environment."
  (declare (ignore env))
  (gethash name *compiler-macro-table*))

;;; Macro Expansion

(defun our-macroexpand-1 (form &optional env)
  "Perform a single macro expansion on FORM.
   Returns (VALUES expanded-form expanded-p)."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-step-cache*)
      (when hitp
        (return-from our-macroexpand-1 (values (cdr cached) (car cached))))))
  (if (and (consp form) (symbolp (car form)))
      (let ((macro-fn (lookup-macro (car form) env)))
        (if macro-fn
            (let ((expanded (funcall macro-fn form env)))
              (if (equal expanded form)
                   (progn (when (%cacheable-macroexpansion-p form)
                           (%macroexpansion-cache-store form env (cons nil form) *macroexpand-step-cache*))
                          (values form nil))
                   (progn (when (and (%cacheable-macroexpansion-p form)
                                     (%cacheable-macroexpansion-p expanded))
                           (%macroexpansion-cache-store form env (cons t expanded) *macroexpand-step-cache*))
                          (values expanded t))))
             (progn (when (%cacheable-macroexpansion-p form)
                      (%macroexpansion-cache-store form env (cons nil form) *macroexpand-step-cache*))
                    (values form nil))))
      (progn (when (%cacheable-macroexpansion-p form)
                (%macroexpansion-cache-store form env (cons nil form) *macroexpand-step-cache*))
              (values form nil))))

(defun %our-macroexpand-all-recursive (form env)
  "Recursively expand FORM after a single expansion step."
  (when (%cacheable-macroexpansion-p form)
    (multiple-value-bind (cached hitp)
        (%macroexpansion-cache-lookup form env *macroexpand-all-cache*)
      (when hitp (return-from %our-macroexpand-all-recursive cached))))
  (multiple-value-bind (expanded expanded-p)
      (our-macroexpand-1 form env)
    (if expanded-p
        (let ((result (%our-macroexpand-all-recursive expanded env)))
          (when (and (%cacheable-macroexpansion-p form)
                     (%cacheable-macroexpansion-p result))
             (%macroexpansion-cache-store form env result *macroexpand-all-cache*))
          result)
        (typecase form
          (cons
            (let ((result (mapcar (lambda (x) (%our-macroexpand-all-recursive x env)) form)))
              (when (and (%cacheable-macroexpansion-p form)
                         (%cacheable-macroexpansion-p result))
                 (%macroexpansion-cache-store form env result *macroexpand-all-cache*))
              result))
           (t
            (when (%cacheable-macroexpansion-p form)
               (%macroexpansion-cache-store form env form *macroexpand-all-cache*))
             form)))))

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
Handles (unquote x) and (unquote-splicing x) within template.
These symbols live in :cl-cc/bootstrap so both parse and expand share them."
  (labels ((qq-head-p (form name)
             (and (consp form)
                  (symbolp (car form))
                  (string= (symbol-name (car form)) name))))
  (cond
    ;; (unquote x) at top level => just x
    ((qq-head-p template "UNQUOTE")
     (second template))
    ;; A list => process each element for unquote/splicing
    ((consp template)
     (let ((parts nil))
       (dolist (elem template)
          (cond
            ((qq-head-p elem "UNQUOTE")
             (push (list 'list (second elem)) parts))
            ((qq-head-p elem "UNQUOTE-SPLICING")
             (push (second elem) parts))
            (t
             (push (list 'list (%expand-quasiquote elem)) parts))))
        (let ((reversed (nreverse parts)))
          (if (= (length reversed) 1)
              (first reversed)
              (cons 'append reversed)))))
    ;; Atom => quote it
    (t (list 'quote template)))))

(defun our-macroexpand-all (form &optional env)
  "Recursively expand all macros in FORM, including in subforms."
  (cond
    ;; Quasiquote — expand before further processing
    ((and (consp form)
          (symbolp (car form))
          (string= (symbol-name (car form)) "BACKQUOTE"))
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
         (env-sym (lambda-list-info-environment info))
         (bindings (generate-lambda-bindings lambda-list form-var))
         (expander-body
           (if env-sym
               (list (list 'let (list (list env-sym env-var))
                           (cons 'let* (cons bindings body))))
               (list (list 'declare (list 'ignore env-var))
                     (cons 'let* (cons bindings body))))))
    (list 'register-macro
          (list 'quote name)
          (cons 'lambda
                (cons (list form-var env-var)
                      expander-body)))))

;;; Wire expand functions into VM hooks for runtime macroexpand support
(defun %vm-install-macroexpand-hooks-if-available ()
  (let* ((pkg (find-package :cl-cc/vm))
         (sym (and pkg (find-symbol "VM-INSTALL-MACROEXPAND-HOOKS" pkg))))
    (when (and sym (fboundp sym))
      (funcall (symbol-function sym) #'our-macroexpand-1 #'our-macroexpand))))

#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  (%vm-install-macroexpand-hooks-if-available))
