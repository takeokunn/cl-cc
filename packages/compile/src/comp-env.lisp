;;;; compile/comp-env.lisp --- CLTL2-style compilation environment helpers
(in-package :cl-cc/compile)

;;; FR-989: expose a small, explicit compilation-environment object for macro
;;; expanders and compiler passes that need lexical variable/function metadata.
;;; This is intentionally conservative: it records the information cl-cc already
;;; tracks and returns NIL for implementation details it cannot prove.

(defstruct (compilation-environment
            (:constructor make-compilation-environment
                (&key parent variables functions declarations))
            (:copier %copy-compilation-environment))
  parent
  variables
  functions
  declarations)

(defvar *declaration-handlers* (make-hash-table :test #'eq)
  "Handlers registered by DEFINE-DECLARATION.")

(defun copy-compilation-environment (environment)
  "Return a shallow copy of ENVIRONMENT."
  (let ((copy (%copy-compilation-environment
               (or environment (make-compilation-environment)))))
    (setf (compilation-environment-variables copy)
          (copy-list (compilation-environment-variables copy))
          (compilation-environment-functions copy)
          (copy-list (compilation-environment-functions copy))
          (compilation-environment-declarations copy)
          (copy-list (compilation-environment-declarations copy)))
    copy))

(defun %comp-env (environment)
  (or environment (make-compilation-environment)))

(defun %comp-env-assoc (name accessor environment &key (test #'eq))
  (loop for env = environment then (compilation-environment-parent env)
        while env
        for entry = (assoc name (funcall accessor env) :test test)
        when entry return entry))

(defun %declaration-name (spec)
  (if (consp spec) (car spec) spec))

(defun %push-declaration (name spec alist)
  (let ((entry (assoc name alist :test #'eq)))
    (if entry
        (progn (push spec (cdr entry)) alist)
        (acons name (list spec) alist))))

(defun %variable-declaration-targets (spec)
  (when (consp spec)
    (case (car spec)
      (type (cddr spec))
      ((special ignorable ignore dynamic-extent) (cdr spec))
      (otherwise
       (when (cl-cc/type:looks-like-type-specifier-p (car spec))
         (cdr spec))))))

(defun %function-declaration-targets (spec)
  (when (and (consp spec)
             (member (car spec) '(ftype inline notinline) :test #'eq))
    (if (eq (car spec) 'ftype)
        (cddr spec)
        (cdr spec))))

(defun %collect-declarations (declare-forms)
  (let ((variable-decls nil)
        (function-decls nil)
        (general-decls nil))
    (dolist (decl-form declare-forms)
      (when (and (consp decl-form) (eq (car decl-form) 'declare))
        (dolist (spec (cdr decl-form))
          (setf general-decls
                (%push-declaration (%declaration-name spec) spec general-decls))
          (dolist (name (%variable-declaration-targets spec))
            (when (symbolp name)
              (setf variable-decls (%push-declaration name spec variable-decls))))
          (dolist (name (%function-declaration-targets spec))
            (setf function-decls (%push-declaration name spec function-decls))))))
    (values variable-decls function-decls general-decls)))

(defun %make-info (kind local-p declarations &rest extra)
  (list* :kind kind :local-p local-p :declarations declarations extra))

(defun augment-environment (environment &key variable symbol-macro function macro declare)
  "Return a new environment augmented with lexical VARIABLE/FUNCTION metadata.

VARIABLE, SYMBOL-MACRO, FUNCTION, and MACRO are lists of names.  DECLARE is a
list of `(declare ...)` forms whose relevant clauses are attached to names and
also retained for DECLARATION-INFORMATION."
  (multiple-value-bind (variable-decls function-decls general-decls)
      (%collect-declarations declare)
    (let ((env (make-compilation-environment :parent environment
                                             :declarations general-decls)))
      (dolist (name variable)
        (push (cons name (%make-info :lexical t
                                     (cdr (assoc name variable-decls))))
              (compilation-environment-variables env)))
      (dolist (binding symbol-macro)
        (let ((name (if (consp binding) (car binding) binding))
              (expansion (and (consp binding) (second binding))))
          (push (cons name (%make-info :symbol-macro t
                                       (cdr (assoc name variable-decls))
                                       :expansion expansion))
                (compilation-environment-variables env))))
      (dolist (entry variable-decls)
        (when (and (some (lambda (spec)
                           (and (consp spec) (eq (car spec) 'special)))
                         (cdr entry))
                   (not (assoc (car entry)
                               (compilation-environment-variables env))))
          (push (cons (car entry) (%make-info :special nil (cdr entry)))
                (compilation-environment-variables env))))
      (dolist (name function)
        (push (cons name (%make-info :function t
                                     (cdr (assoc name function-decls :test #'equal))))
              (compilation-environment-functions env)))
      (dolist (name macro)
        (push (cons name (%make-info :macro t
                                     (cdr (assoc name function-decls :test #'equal))))
              (compilation-environment-functions env)))
      env)))

(defun variable-information (symbol &optional environment)
  "Return KIND, LOCAL-P, and DECLARATIONS for SYMBOL in ENVIRONMENT."
  (let* ((env (%comp-env environment))
         (entry (%comp-env-assoc symbol #'compilation-environment-variables env))
         (info (cdr entry)))
    (cond
      (info
       (values (getf info :kind)
               (getf info :local-p)
               (getf info :declarations)))
      ((or (keywordp symbol) (member symbol '(nil t) :test #'eq))
       (values :constant nil nil))
      ((member symbol *builtin-special-variables* :test #'eq)
       (values :special nil nil))
      (t
       (values nil nil nil)))))

(defun function-information (name &optional environment)
  "Return KIND, LOCAL-P, and DECLARATIONS for function NAME in ENVIRONMENT."
  (let* ((env (%comp-env environment))
         (entry (%comp-env-assoc name #'compilation-environment-functions env
                                 :test #'equal))
         (info (cdr entry)))
    (cond
      (info
       (values (getf info :kind)
               (getf info :local-p)
               (getf info :declarations)))
      ((and (symbolp name) (macro-function name))
       (values :macro nil nil))
      ((fboundp name)
       (values :function nil nil))
      (t
       (values nil nil nil)))))

(defun declaration-information (declaration-name &optional environment)
  "Return declaration metadata for DECLARATION-NAME in ENVIRONMENT."
  (let* ((env (%comp-env environment))
         (entry (%comp-env-assoc declaration-name
                                 #'compilation-environment-declarations env)))
    (or (cdr entry)
        (let ((handler (gethash declaration-name *declaration-handlers*)))
          (and handler (funcall handler declaration-name env))))))

(defun define-declaration (declaration-name handler)
  "Register HANDLER for DECLARATION-NAME and return DECLARATION-NAME."
  (setf (gethash declaration-name *declaration-handlers*) handler)
  declaration-name)

(defun parse-macro (name lambda-list body &optional environment)
  "Return a macro-expander function for NAME, LAMBDA-LIST, and BODY."
  (declare (ignore environment))
  (let ((macro-name (or name (gensym "MACRO"))))
    (eval `(macrolet ((,macro-name ,lambda-list ,@body))
             (macro-function ',macro-name)))))

(defun compilation-environment-from-context (ctx &optional parent)
  "Create a compilation environment snapshot from compiler context CTX."
  (let ((env (make-compilation-environment :parent parent)))
    (dolist (binding (ctx-env ctx))
      (when (symbolp (car binding))
        (push (cons (car binding) (%make-info :lexical t nil))
              (compilation-environment-variables env))))
    (maphash (lambda (name present-p)
               (declare (ignore present-p))
               (push (cons name (%make-info :special nil nil))
                     (compilation-environment-variables env)))
             (ctx-global-variables ctx))
    (maphash (lambda (name label)
               (declare (ignore label))
               (push (cons name (%make-info :function nil nil))
                     (compilation-environment-functions env)))
             (ctx-global-functions ctx))
    env))
